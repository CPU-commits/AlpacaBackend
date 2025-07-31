package follow_repository

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlFollowRepository struct {
	db *sql.DB
}

func (sqlFR sqlFollowRepository) Delete(criteria *Criteria) error {
	where := sqlFR.criteriaToWhere(criteria)

	_, err := models.Follows(where...).DeleteAll(context.Background(), sqlFR.db)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlFR sqlFollowRepository) Insert(follow model.Follow) error {
	sqlFollow := models.Follow{
		IDUser:    follow.IDUser,
		IDProfile: null.NewInt64(follow.IDProfile, follow.IDProfile != 0),
		IDStudio:  null.NewInt64(follow.IDStudio, follow.IDStudio != 0),
	}

	err := sqlFollow.Insert(context.Background(), sqlFR.db, boil.Infer())
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlFR sqlFollowRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlFR.criteriaToWhere(criteria)

	count, err := models.Follows(where...).Count(context.Background(), sqlFR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return count, nil
}

func (sqlFR sqlFollowRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.IDProfile != 0 {
		where = append(where, models.FollowWhere.IDProfile.EQ(null.Int64From(criteria.IDProfile)))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.FollowWhere.IDStudio.EQ(null.Int64From(criteria.IDStudio)))
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			where = append(where, models.FollowWhere.CreatedAt.LTE(criteria.CreatedAt.LTE))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			where = append(where, models.FollowWhere.CreatedAt.LT(criteria.CreatedAt.LT))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			where = append(where, models.FollowWhere.CreatedAt.GTE(criteria.CreatedAt.GTE))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			where = append(where, models.FollowWhere.CreatedAt.GT(criteria.CreatedAt.GT))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			where = append(where, models.FollowWhere.CreatedAt.EQ(criteria.CreatedAt.EQ))
		}
	}

	return where
}

func (sqlFR sqlFollowRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlFR.criteriaToWhere(criteria)

	exists, err := models.Follows(where...).Exists(context.Background(), sqlFR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlFR sqlFollowRepository) CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error) {
	where := sqlFR.criteriaToWhere(criteria)

	type SqlFollowResult struct {
		Day     time.Time `boil:"day"`
		Follows int64     `boil:"follows"`
	}
	var follows []SqlFollowResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Follows),
			Select("DATE(created_at) as day"),
			Select("COUNT(*) as follows"),
			GroupBy("day"),
		}, where...)...,
	).Bind(context.Background(), sqlFR.db, &follows)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(follows, func(follow SqlFollowResult) CountGroupByDayResult {
		return CountGroupByDayResult{
			Follows: follow.Follows,
			Day:     follow.Day,
		}
	}), nil
}

func NewSqlFollowRepository() FollowRepository {
	return sqlFollowRepository{
		db: db.DB,
	}
}

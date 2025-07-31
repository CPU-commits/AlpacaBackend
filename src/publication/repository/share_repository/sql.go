package share_repository

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"

	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlShareRepository struct {
	db *sql.DB
}

func (sqlShareRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.IDPost != 0 {
		mod = append(mod, models.ShareWhere.IDPost.EQ(criteria.IDPost))
	}
	if criteria.IDUser != 0 {
		mod = append(mod, models.ShareWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			mod = append(mod, models.ShareWhere.CreatedAt.LTE(criteria.CreatedAt.LTE))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			mod = append(mod, models.ShareWhere.CreatedAt.LT(criteria.CreatedAt.LT))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			mod = append(mod, models.ShareWhere.CreatedAt.GTE(criteria.CreatedAt.GTE))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			mod = append(mod, models.ShareWhere.CreatedAt.GT(criteria.CreatedAt.GT))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			mod = append(mod, models.ShareWhere.CreatedAt.EQ(criteria.CreatedAt.EQ))
		}
	}

	return mod
}

func (sqlLR sqlShareRepository) CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error) {
	where := sqlLR.criteriaToWhere(criteria)

	type SqlShareResult struct {
		Day    time.Time `boil:"day"`
		Shares int64     `boil:"shares"`
	}
	var shares []SqlShareResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Shares),
			Select("DATE(created_at) as day"),
			Select("COUNT(*) as shares"),
			GroupBy("day"),
		}, where...)...,
	).Bind(context.Background(), sqlLR.db, &shares)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(shares, func(view SqlShareResult) CountGroupByDayResult {
		return CountGroupByDayResult{
			Shares: view.Shares,
			Day:    view.Day,
		}
	}), nil
}

func (sqlLR sqlShareRepository) Delete(criteria *Criteria) error {
	where := sqlLR.criteriaToWhere(criteria)

	_, err := models.Shares(where...).DeleteAll(context.Background(), sqlLR.db)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlLR sqlShareRepository) Insert(share model.Share) error {
	sqlShare := models.Share{
		IDUser: share.IDUser,
		IDPost: share.IDPost,
	}

	if err := sqlShare.Insert(context.Background(), sqlLR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlLR sqlShareRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlLR.criteriaToWhere(criteria)

	exists, err := models.Shares(where...).Exists(context.Background(), sqlLR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlLR sqlShareRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlLR.criteriaToWhere(criteria)

	count, err := models.Shares(where...).Count(context.Background(), sqlLR.db)
	if err != nil {
		return 0, err
	}
	return count, nil
}

func NewSqlShareRepository(db *sql.DB) ShareRepository {
	return sqlShareRepository{
		db: db,
	}
}

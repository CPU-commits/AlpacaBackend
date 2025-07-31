package like_repository

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

type sqlLikeRepository struct {
	db *sql.DB
}

func (sqlLikeRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.IDPost != 0 {
		mod = append(mod, models.LikeWhere.IDPost.EQ(criteria.IDPost))
	}
	if criteria.IDProfile != 0 {
		mod = append(mod, models.LikeWhere.IDProfile.EQ(criteria.IDProfile))
	}
	if criteria.IDUser != 0 {
		mod = append(mod, models.LikeWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			mod = append(mod, models.LikeWhere.CreatedAt.LTE(criteria.CreatedAt.LTE))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			mod = append(mod, models.LikeWhere.CreatedAt.LT(criteria.CreatedAt.LT))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			mod = append(mod, models.LikeWhere.CreatedAt.GTE(criteria.CreatedAt.GTE))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			mod = append(mod, models.LikeWhere.CreatedAt.GT(criteria.CreatedAt.GT))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			mod = append(mod, models.LikeWhere.CreatedAt.EQ(criteria.CreatedAt.EQ))
		}
	}

	return mod
}

func (sqlLR sqlLikeRepository) CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error) {
	where := sqlLR.criteriaToWhere(criteria)

	type SqlLikeResult struct {
		Day   time.Time `boil:"day"`
		Likes int64     `boil:"likes"`
	}
	var likes []SqlLikeResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Likes),
			Select("DATE(created_at) as day"),
			Select("COUNT(*) as likes"),
			GroupBy("day"),
		}, where...)...,
	).Bind(context.Background(), sqlLR.db, &likes)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(likes, func(view SqlLikeResult) CountGroupByDayResult {
		return CountGroupByDayResult{
			Likes: view.Likes,
			Day:   view.Day,
		}
	}), nil
}

func (sqlLR sqlLikeRepository) Delete(criteria *Criteria) error {
	where := sqlLR.criteriaToWhere(criteria)

	_, err := models.Likes(where...).DeleteAll(context.Background(), sqlLR.db)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlLR sqlLikeRepository) Insert(like model.Like) error {
	sqlLike := models.Like{
		IDUser:    like.IDUser,
		IDProfile: like.IDProfile,
		IDPost:    like.IDPost,
	}

	if err := sqlLike.Insert(context.Background(), sqlLR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlLR sqlLikeRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlLR.criteriaToWhere(criteria)

	exists, err := models.Likes(where...).Exists(context.Background(), sqlLR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlLR sqlLikeRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlLR.criteriaToWhere(criteria)

	count, err := models.Likes(where...).Count(context.Background(), sqlLR.db)
	if err != nil {
		return 0, err
	}
	return count, nil
}

func NewSqlLikeRepository(db *sql.DB) LikeRepository {
	return sqlLikeRepository{
		db: db,
	}
}

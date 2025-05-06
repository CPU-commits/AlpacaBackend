package like_repository

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
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

	return mod
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

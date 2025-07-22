package follow_repository

import (
	"context"
	"database/sql"
	"fmt"

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

func NewSqlFollowRepository() FollowRepository {
	return sqlFollowRepository{
		db: db.DB,
	}
}

package follow_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlFollowRepository struct {
	db *sql.DB
}

type SqlFollowRepository = sqlFollowRepository

func (sqlFR *sqlFollowRepository) criteriaToWhere(criteria *FollowCriteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.IDProfile != 0 {
		mod = append(mod, Where("id_profile = ?", criteria.IDProfile))
	}
	if criteria.IDUser != 0 {
		mod = append(mod, Where("id_user = ?", criteria.IDUser))
	}

	return mod
}

func (sqlFR *sqlFollowRepository) CountProfileFollowers(criteria *FollowCriteria) (int64, error) {
	where := sqlFR.criteriaToWhere(criteria)

	count, err := models.Follows(where...).Count(context.Background(), sqlFR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return 0, nil
		}
		return 0, utils.ErrRepositoryFailed
	}
	return count, nil
}

func NewSqlFollowRepository(db *sql.DB) sqlFollowRepository {
	return sqlFollowRepository{
		db: db,
	}
}

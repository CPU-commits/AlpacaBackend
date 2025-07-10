package access_repository

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlAccessRepository struct {
	db *sql.DB
}

func (sqlAccessRepository) criteriaToWhere(criteria *AccessCriteria) []QueryMod {
	if criteria == nil {
		return nil
	}
	where := []QueryMod{}
	if criteria.IsRevoked != nil {
		where = append(where, Where("is_revoked = ?", *criteria.IsRevoked))
	}
	if criteria.ID_NE != 0 {
		where = append(where, Where("id != ?", criteria.ID_NE))
	}
	if criteria.Token != "" {
		where = append(where, Where("token = ?", criteria.Token))
	}

	return where
}

func (sqlAR sqlAccessRepository) Delete(criteria *AccessCriteria) error {
	where := sqlAR.criteriaToWhere(criteria)

	_, err := models.Accesses(
		where...,
	).DeleteAll(context.Background(), sqlAR.db)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlAR sqlAccessRepository) Exists(criteria *AccessCriteria) (int64, error) {
	where := sqlAR.criteriaToWhere(criteria)

	access, err := models.Accesses(append(
		[]QueryMod{Select("id")},
		where...,
	)...).One(context.Background(), sqlAR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return 0, nil
		}
		return 0, utils.ErrRepositoryFailed
	}

	return access.ID, nil
}

func (sqlAR sqlAccessRepository) InsertOne(access model.Access) (id int64, err error) {
	accessSQL := models.Access{
		Token:     access.Token,
		IDSession: access.IDAccess,
		CreatedAt: time.Now(),
	}
	err = accessSQL.Insert(context.Background(), sqlAR.db, boil.Infer())
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return accessSQL.ID, nil
}

func (sqlAR sqlAccessRepository) Update(
	criteria *AccessCriteria,
	data AccessUpdateData,
) error {
	where := sqlAR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.IsRevoked != nil {
		cols["is_revoked"] = data.IsRevoked
	}

	_, err := models.Accesses(
		where...,
	).UpdateAll(context.Background(), sqlAR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func NewSQLAccessRepository(db *sql.DB) AccessRepository {
	return sqlAccessRepository{
		db: db,
	}
}

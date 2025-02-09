package auth_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlAuthRepository struct {
	db *sql.DB
}

func (sqlAuthRepository) sqlAuthToAuth(auth *models.Auth) *model.Auth {
	return &model.Auth{
		ID:       auth.ID,
		Password: auth.Password,
		IDUser:   auth.IDUser,
	}
}

func (sqlAR sqlAuthRepository) FindOneByUsername(username string) (*model.Auth, error) {
	auth, err := models.Auths(
		InnerJoin("users u on u.id = auths.id_user"),
		Where("u.email = ?", username),
	).One(context.Background(), sqlAR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}

	return sqlAR.sqlAuthToAuth(auth), nil
}

func NewSQLAuthRepository(db *sql.DB) AuthRepository {
	return sqlAuthRepository{
		db: db,
	}
}

package tokenpassword_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/sqlboiler/v4/boil"
)

type sqlTokenPasswordRepository struct {
	db *sql.DB
}

func (sqlAR sqlTokenPasswordRepository) InsertOne(tokenpassword model.TokenPassword) (id int64, err error) {
	tokenpasswordSQL := models.TokenPassword{
		Token:  tokenpassword.Token,
		IDUser: tokenpassword.IDUser,
	}
	err = tokenpasswordSQL.Insert(context.Background(), sqlAR.db, boil.Infer())
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}
	return tokenpasswordSQL.ID, nil
}

func NewSQLTokenPasswordRepository(db *sql.DB) TokenPasswordRepository {
	return sqlTokenPasswordRepository{
		db: db,
	}
}

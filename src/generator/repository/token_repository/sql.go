package token_repository

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlTokenRepository struct {
	db *sql.DB
}

func (sqlTokenRepository) sqlTokenToToken(token models.Token) *model.Token {
	return &model.Token{
		ID:        token.ID,
		IDUser:    token.IDUser,
		Token:     token.Token,
		IsActive:  token.IsActive,
		ExpiresAt: token.ExpiresAt,
		CreatedAt: token.CreatedAt,
	}
}

func (sqlTokenRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	var mod []QueryMod
	if criteria == nil {
		return mod
	}
	if criteria.Token != "" {
		mod = append(mod, Where("token = ?", criteria.Token))
	}
	if criteria.IsActive != nil {
		mod = append(mod, Where("is_active = ?", criteria.IsActive))
	}

	return mod
}

func (sqlTR sqlTokenRepository) InsertOne(token model.Token, duration time.Duration) (*model.Token, error) {
	expiresAt := time.Now().Add(duration * time.Minute)
	sqlToken := models.Token{
		IDUser:    token.IDUser,
		Token:     token.Token,
		ExpiresAt: expiresAt,
	}
	err := sqlToken.Insert(context.Background(), sqlTR.db, boil.Infer())
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return sqlTR.sqlTokenToToken(sqlToken), nil
}

func (sqlTR sqlTokenRepository) VerifyToken(token model.Token) (*model.Token, error) {
	where := sqlTR.criteriaToWhere(&Criteria{
		Token:    token.Token,
		IsActive: utils.Bool(true),
	})
	ctx := context.Background()

	tx, err := sqlTR.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	sqlToken, err := models.Tokens(where...).One(ctx, tx)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}
	if sqlToken.IsActive {
		sqlToken.IsActive = false
		if _, err := sqlToken.Update(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return nil, utils.ErrRepositoryFailed
	}

	return nil, nil
}

func (sqlTR sqlTokenRepository) DeactiveToken(token model.RedisToken) error {
	where := sqlTR.criteriaToWhere(&Criteria{
		Token:    token.Token,
		IsActive: utils.Bool(true),
	})

	ctx := context.Background()

	tx, err := sqlTR.db.BeginTx(ctx, nil)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	tokenSql, err := models.Tokens(where...).One(ctx, tx)
	if err != nil {
		tx.Rollback()
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return utils.ErrRepositoryFailed
	}

	tokenSql.IsActive = false

	if _, err := tokenSql.Update(ctx, tx, boil.Infer()); err != nil {
		tx.Rollback()

		return utils.ErrRepositoryFailed
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}

	return nil
}

func NewSqlTokenRepository(db *sql.DB) TokenRepository {
	return sqlTokenRepository{
		db: db,
	}
}

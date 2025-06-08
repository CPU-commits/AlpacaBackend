package code_repository

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlCodeRepository struct {
	db *sql.DB
}

func (sqlCodeRepository) sqlCodeToCode(code models.Code) *model.Code {
	return &model.Code{
		ID:        code.ID,
		IDUser:    code.IDUser,
		IsActive:  code.IsActive,
		Type:      code.Type,
		ExpiresAt: code.ExpiresAt,
		CreatedAt: code.CreatedAt,
	}
}

func (sqlCodeRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	var mod []QueryMod
	if criteria == nil {
		return mod
	}
	if criteria.Code != "" {
		mod = append(mod, Where("code = ?", criteria.Code))
	}
	if criteria.IsActive != nil {
		mod = append(mod, Where("is_active = ?", criteria.IsActive))
	}

	return mod
}

func (sqlCR sqlCodeRepository) InsertOne(code model.Code, duration int64) (*model.Code, error) {
	expiresAt := time.Now().Add(time.Duration(duration) * time.Minute)
	sqlCode := models.Code{
		IDUser:        code.IDUser,
		Code:          code.Code,
		Type:          code.Type,
		UsesRemaining: code.UsesRemaining,
		ExpiresAt:     expiresAt,
	}
	err := sqlCode.Insert(context.Background(), sqlCR.db, boil.Infer())
	if err != nil {
		return nil, err
	}
	return sqlCR.sqlCodeToCode(sqlCode), nil
}

func (sqlCR sqlCodeRepository) VerifyCode(code model.Code) (*model.Code, error) {

	where := sqlCR.criteriaToWhere(&Criteria{
		Code:     code.Code,
		IsActive: utils.Bool(true),
	})
	ctx := context.Background()
	tx, err := sqlCR.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	sqlCode, err := models.Codes(where...).One(ctx, tx)
	if err != nil {
		tx.Rollback()

		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}

	if sqlCode.IsActive {
		sqlCode.IsActive = false
		if _, err := sqlCode.Update(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}

		return sqlCR.sqlCodeToCode(*sqlCode), nil
	}

	return nil, nil
}

// func (sqlCR sqlCodeRepository) DeleteExpiredCodes() error {

// 	return nil
// }

func NewSqlCodeRepository(db *sql.DB) CodeRepository {
	return sqlCodeRepository{
		db: db,
	}
}

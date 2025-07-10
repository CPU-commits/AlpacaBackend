package session_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlSessionRepository struct {
	db *sql.DB
}

func (sqlSessionRepository) criteriaToWhere(criteria *SessionCriteria) []QueryMod {
	if criteria == nil {
		return nil
	}
	where := []QueryMod{}
	if criteria.Token != "" {
		where = append(where, Where("token = ?", criteria.Token))
	}
	if criteria.ExpiredAtGt != nil {
		where = append(where, Where("expires_at > ?", criteria.ExpiredAtGt))
	}
	if criteria.ExpiredAtLt != nil {
		where = append(where, Where("expires_at < ?", criteria.ExpiredAtGt))
	}

	return where
}

func (sqlSR sqlSessionRepository) Delete(criteria *SessionCriteria) error {
	where := sqlSR.criteriaToWhere(criteria)

	_, err := models.Sessions(
		where...,
	).DeleteAll(context.Background(), sqlSR.db)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlSR sqlSessionRepository) InsertOne(session model.Session) (id int64, err error) {
	sessionSQL := models.Session{
		Token:     session.Token,
		IDAuth:    session.IDAuth,
		ExpiresAt: session.ExpiresAt,
	}
	if session.Device != "" {
		sessionSQL.Device = null.StringFrom(session.Device)
	}
	if session.Browser != "" {
		sessionSQL.Browser = null.StringFrom(session.Browser)
	}
	if session.IP != "" {
		sessionSQL.IP = null.StringFrom(session.IP)
	}
	if session.Location != "" {
		sessionSQL.Location = null.StringFrom(session.Location)
	}
	err = sessionSQL.Insert(context.Background(), sqlSR.db, boil.Infer())

	return sessionSQL.ID, err
}

func (sqlSR sqlSessionRepository) Exists(criteria *SessionCriteria) (int64, error) {
	where := sqlSR.criteriaToWhere(criteria)

	session, err := models.Sessions(append(
		[]QueryMod{Select("id")},
		where...,
	)...).One(context.Background(), sqlSR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return 0, nil
		}

		return 0, err
	}

	return session.ID, nil
}

func (sqlSR sqlSessionRepository) Update(
	criteria *SessionCriteria,
	data SessionUpdateData,
) error {
	where := sqlSR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.Token != "" {
		cols["token"] = data.Token
	}
	if data.ExpiresAt != nil {
		cols["expires_at"] = *data.ExpiresAt
	}

	_, err := models.Sessions(
		where...,
	).UpdateAll(context.Background(), sqlSR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	return nil
}

func NewSQLSessionRepository(db *sql.DB) SessionRepository {
	return sqlSessionRepository{
		db: db,
	}
}

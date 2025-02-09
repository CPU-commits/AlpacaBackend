package session_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
)

type SessionCriteria struct {
	ExpiredAtGt *time.Time
	ExpiredAtLt *time.Time
	Token       string
}

type SessionUpdateData struct {
	Token     string
	ExpiresAt *time.Time
}

type SessionRepository interface {
	InsertOne(session model.Session) (id int64, err error)
	Exists(criteria *SessionCriteria) (id int64, err error)
	Update(criteria *SessionCriteria, data SessionUpdateData) error
	Delete(criteria *SessionCriteria) error
}

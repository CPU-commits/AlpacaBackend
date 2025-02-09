package model

import "time"

type Access struct {
	ID        int64
	Token     string
	ExpiresAt time.Time
	CreatedAt time.Time
	IsRevoked bool
	IDAccess  int64
}

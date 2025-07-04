package model

import "time"

type Code struct {
	ID            int64     `json:"id"`
	IDUser        int64     `json:"idUser"`
	Code          string    `json:"code"`
	IsActive      bool      `json:"isActive"`
	UsesRemaining int64     `json:"usesRemaining"`
	Type          string    `json:"type"`
	ExpiresAt     time.Time `json:"expiresAt"`
	CreatedAt     time.Time `json:"createdAt"`
}

var CodeTypeList = []string{
	"recoveryEmail",
	"recoveryPassword",
}

package model

import "time"

type Token struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"idUser"`
	Token     string    `json:"token"`
	IsUsed    bool      `json:"isUsed"`
	ExpiresAt time.Time `json:"expiresAt"`
	CreatedAt time.Time `json:"createdAt"`
}

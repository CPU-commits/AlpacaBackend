package model

import "time"

type Token struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"idUser"`
	Token     string    `json:"token"`
	IsActive  bool      `json:"isActive"`
	ExpiresAt time.Time `json:"expiresAt"`
	CreatedAt time.Time `json:"createdAt"`
}

type RedisToken struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"idUser"`
	Token     string    `json:"token"`
	ExpiresAt time.Time `json:"expiresAt"`
	CreatedAt time.Time `json:"createdAt"`
}

var TokenType = []string{
	"email",
	"password",
}

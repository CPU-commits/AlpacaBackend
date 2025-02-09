package model

type UsernameType string

type Auth struct {
	ID       int64
	Password string
	IDUser   int64
}

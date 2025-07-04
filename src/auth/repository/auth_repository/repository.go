package auth_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type DataUpdate struct {
	Password *string
}

type AuthRepository interface {
	FindOneByUsername(username string) (*model.Auth, error)
	FindOneByUserId(userId int64) (*model.Auth, error)
	UpdatePassword(userId int64, data DataUpdate) error
}

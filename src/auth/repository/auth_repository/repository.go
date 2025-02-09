package auth_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type AuthRepository interface {
	FindOneByUsername(username string) (*model.Auth, error)
}

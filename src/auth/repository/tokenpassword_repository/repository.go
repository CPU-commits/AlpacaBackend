package tokenpassword_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type TokenPasswordRepository interface {
	InsertOne(tower model.TokenPassword) (id int64, err error)
}

package token_repository

import (
	"time"

	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
)

type Criteria struct {
	Token    string
	IsActive *bool
}

type TokenRepository interface {
	InsertOne(token model.Token, duration time.Duration) (*model.Token, error) // Crea un token
	VerifyToken(token model.Token) (*model.Token, error)
	DeactiveToken(token model.RedisToken) error
}

type TokenGenerator interface {
	NewSessionToken(expiredAt time.Time, idUser int64) (string, error)
	NewAccessToken(expiredAt time.Time, user userModel.User) (string, error)
	NewFirstTimeToken(IDUser int64) (string, error)
	NewRecoveryToken(expiredAt time.Time, user userModel.User) (string, error)
}

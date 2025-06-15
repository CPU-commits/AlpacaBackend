package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/tokenpassword_repository"
	tokenRepository "github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/token_repository"
)

var tokenPasswordServiceInstance *TokenPasswordService

type TokenPasswordService struct {
	tokenPasswordRepository tokenpassword_repository.TokenPasswordRepository
	tokenGenerator          tokenRepository.TokenGenerator
}

func (tokenPasswordService *TokenPasswordService) NewTokenPassword(
	IDuser int64,
) (string, error) {
	firstToken, err := tokenPasswordService.tokenGenerator.NewFirstTimeToken(IDuser)
	if err != nil {
		return "", err
	}
	newTokenPassword := model.TokenPassword{
		IDUser: IDuser,
		Token:  firstToken,
	}
	_, err = tokenPasswordService.tokenPasswordRepository.InsertOne(newTokenPassword)
	if err != nil {
		return "", err
	}
	return firstToken, nil
}

func NewTokenPasswordService(
	tokenpasswordRepository tokenpassword_repository.TokenPasswordRepository,
	tokenGenerator tokenRepository.TokenGenerator,
) *TokenPasswordService {
	if tokenPasswordServiceInstance == nil {
		tokenPasswordServiceInstance = &TokenPasswordService{
			tokenPasswordRepository: tokenpasswordRepository,
			tokenGenerator:          tokenGenerator,
		}
	}
	return tokenPasswordServiceInstance
}

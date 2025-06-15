package services

import (
	"time"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/token_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type TokenService struct {
	tokenRepository token_repository.TokenRepository
	tokenGenerator  token_repository.TokenGenerator
	userService     authService.UserService
}

func NewTokenService(
	tokenRepository token_repository.TokenRepository,
	tokenGenerator token_repository.TokenGenerator,
	userService authService.UserService,
) *TokenService {
	return &TokenService{
		tokenRepository: tokenRepository,
		tokenGenerator:  tokenGenerator,
		userService:     userService,
	}
}

// Token generado despues de confirmar el codigo
func (tokenService *TokenService) CreateRecoveryToken(expiresAt time.Time, user authModel.User) (*model.Token, error) {
	if _, err := tokenService.userService.GetUserById(user.ID); err != nil {
		return nil, err
	}

	token, err := tokenService.tokenGenerator.NewRecoveryToken(expiresAt, user)
	if err != nil {
		return nil, err
	}

	modelToken := model.Token{
		IDUser: user.ID,
		Token:  token,
	}
	newToken, err := tokenService.tokenRepository.InsertOne(modelToken, time.Duration(5))
	if err != nil {
		return nil, err
	}
	return newToken, nil
}

func (tokenService *TokenService) IsTokenValid(token dto.TokenDTO) error {
	tokenModel := token.ToModel()

	resultToken, err := tokenService.tokenRepository.VerifyToken(*tokenModel)
	if err != nil {
		return ErrTokenNotValid
	}
	if err := utils.VerifyNotExpiredAt(resultToken.ExpiresAt, "local", ErrTokenNotValid); err != nil {
		return err
	}
	return nil

}

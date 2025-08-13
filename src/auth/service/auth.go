package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	generatorModel "github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"golang.org/x/crypto/bcrypt"
)

var authService *AuthService

type AuthService struct {
	authRepository auth_repository.AuthRepository
	userRepository user_repository.UserRepository
	bus            bus.Bus
}

func (authService *AuthService) CheckIfEmailOrUsernameExists(email, username string) error {
	existsEmailOrUsername, err := authService.userRepository.Exists(&user_repository.Criteria{
		Or: []user_repository.Criteria{
			{
				Email: repository.CriteriaString{
					EQ: utils.String(email),
				},
			},
			{
				Username: repository.CriteriaString{
					EQ: utils.String(username),
				},
			},
		},
	})
	if err != nil {
		return err
	}
	if existsEmailOrUsername {
		return ErrExistsEmailOrUsername
	}

	return nil
}

func (authService *AuthService) Register(
	registerDto *dto.RegisterDto,
) error {
	user, err := registerDto.ToModel()
	if err != nil {
		return err
	}
	err = authService.CheckIfEmailOrUsernameExists(registerDto.Email, registerDto.Username)
	if err != nil {
		return err
	}

	_, err = authService.userRepository.InsertOne(user, registerDto.Password)
	return err
}

func (authService *AuthService) Login(authDto dto.AuthDto) (*model.User, int64, error) {
	auth, err := authService.authRepository.FindOneByUsername(authDto.Username)
	if err != nil {
		return nil, 0, utils.ErrRepositoryFailed
	}
	if auth == nil {
		return nil, 0, ErrUserLoginNotFound
	}
	if err := bcrypt.CompareHashAndPassword(
		[]byte(auth.Password),
		[]byte(authDto.Password),
	); err != nil {
		return nil, 0, ErrInvalidCredentials
	}

	// User
	user, err := authService.userRepository.FindOneByEmail(authDto.Username)
	if err != nil {
		return nil, 0, err
	}
	if user == nil {
		return nil, 0, ErrUserLoginNotFound
	}

	return user, auth.ID, nil
}

func (authService *AuthService) UpdatePassword(userId int64, data dto.UpdateAuthPasswordDTO) error {
	var token generatorModel.RedisToken
	err := authService.bus.Request(
		bus.Event{
			Name:    GET_TOKEN_PASSWORD_UPDATE,
			Payload: utils.Payload(userId),
		},
		&token)

	if err != nil {
		return err
	}
	if token.Token == "" {
		return ErrNotValidToken
	}

	auth, err := authService.authRepository.FindOneByUserId(userId)
	if err != nil {
		return err
	}
	if auth == nil {
		return ErrUserLoginNotFound
	}

	if err := bcrypt.CompareHashAndPassword(
		[]byte(auth.Password),
		[]byte(data.NewPassword),
	); err == nil {
		return ErrInvalidCredentials
	}

	if err := authService.authRepository.UpdatePassword(userId, auth_repository.DataUpdate{
		Password: &data.NewPassword,
	}); err != nil {
		return err
	}

	go authService.bus.Publish(bus.Event{
		Name:    UPDATE_TOKEN_STATUS,
		Payload: utils.Payload(token),
	})

	return nil
}

func NewAuthService(
	authRepository auth_repository.AuthRepository,
	userRepository user_repository.UserRepository,
	bus bus.Bus,
) *AuthService {
	if authService == nil {
		authService = &AuthService{
			authRepository: authRepository,
			userRepository: userRepository,
			bus:            bus,
		}
	}
	return authService
}

package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"golang.org/x/crypto/bcrypt"
)

type authService struct {
	authRepository auth_repository.AuthRepository
	userRepository user_repository.UserRepository
}

func (authService *authService) Register(
	registerDto *dto.RegisterDto,
) error {
	user, err := registerDto.ToModel()
	if err != nil {
		return err
	}
	existsEmailOrUsername, err := authService.userRepository.Exists(&user_repository.Criteria{
		Or: []user_repository.Criteria{
			{
				Email: registerDto.Email,
			},
			{
				Username: registerDto.Username,
			},
		},
	})
	if existsEmailOrUsername {
		return ErrExistsEmailOrUsername
	}

	_, err = authService.userRepository.InsertOne(user, registerDto.Password)
	return err
}

func (authService *authService) Login(authDto dto.AuthDto) (*model.User, int64, error) {
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

func NewAuthService(
	authRepository auth_repository.AuthRepository,
	userRepository user_repository.UserRepository,
) *authService {
	return &authService{
		authRepository: authRepository,
		userRepository: userRepository,
	}
}

package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type UserService struct {
	userRepository user_repository.UserRepository
}

func (userService *UserService) GetUserIDFromUsername(username string) (int64, error) {
	opts := user_repository.NewFindOneOptions().Select(user_repository.SelectOpts{
		ID: utils.Bool(true),
	})

	user, err := userService.userRepository.FindOne(
		&user_repository.Criteria{
			Username: username,
		},
		opts,
	)
	if err != nil {
		return 0, err
	}
	if user == nil {
		return 0, ErrUsernameNotExists
	}

	return user.ID, nil
}

func (userService *UserService) GetUserById(idUser int64) (*model.User, error) {
	user, err := userService.userRepository.FindOneByID(idUser)
	if err != nil {
		return nil, nil
	}
	if user == nil {
		return nil, ErrUserNotFound
	}

	return user, nil
}

func NewUserService(
	userRepository user_repository.UserRepository,
) *UserService {
	return &UserService{
		userRepository: userRepository,
	}
}

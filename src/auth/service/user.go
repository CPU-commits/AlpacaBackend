package service

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	generatorModel "github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var userService *UserService

type UserService struct {
	userRepository user_repository.UserRepository
	roleRepository role_repository.RoleRepository
	bus            bus.Bus
}

func (userService *UserService) UserIsTattooArtist(
	idUser int64,
) (bool, error) {
	existsUser, err := userService.userRepository.Exists(&user_repository.Criteria{
		ID: idUser,
	})
	if err != nil {
		return false, err
	}
	if !existsUser {
		return false, ErrUserNotFound
	}
	existsArtist, err := userService.roleRepository.Exists(&role_repository.Criteria{
		IDUser: idUser,
		Role:   model.TATTOO_ARTIST_ROLE,
	})
	if err != nil {
		return false, err
	}

	return existsArtist, nil
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

func (userService *UserService) UserUpdate(idUser int64, data dto.UserUpdateData) error {

	if _, err := userService.GetUserById(idUser); err != nil {
		return err
	}

	var dataUpdate user_repository.UserUpdateData

	if data.Name != "" {
		dataUpdate.Name = &data.Name
	}
	if data.Phone != "" {
		dataUpdate.Phone = &data.Phone
	}

	return userService.userRepository.UpdateOne(idUser, dataUpdate)
}

func (userService *UserService) UpdateEmail(data dto.UpdateAuthEmailDTO, idUser int64) error {

	var token generatorModel.RedisToken
	err := userService.bus.Request(
		bus.Event{
			Name:    GET_TOKEN_EMAIL_UPDATE,
			Payload: utils.Payload(idUser),
		},
		&token)

	if err != nil {
		return err
	}
	if token.Token == "" {
		return ErrNotValidToken
	}
	if data.NewEmail == "" {
		return ErrInvalidParams
	}

	emailExist, err := userService.userRepository.Exists(&user_repository.Criteria{
		Email: data.NewEmail,
	})
	if err != nil {
		return err
	}
	if emailExist {
		return ErrExistsEmail
	}

	if err := userService.userRepository.UpdateOne(idUser, user_repository.UserUpdateData{
		Email: &data.NewEmail,
	}); err != nil {
		return err
	}

	go userService.bus.Publish(bus.Event{
		Name:    UPDATE_TOKEN_STATUS,
		Payload: utils.Payload(token),
	})

	return nil

}

func (userService *UserService) IsOwner(userId int64, params dto.QueryIsOwner) error {
	if params.ID == 0 && params.UserName == "" {
		return ErrInvalidParams

	}

	user, err := userService.userRepository.FindOne(&user_repository.Criteria{
		Or: []user_repository.Criteria{
			{
				Username: params.UserName,
			},
			{
				ID: params.ID,
			},
		},
	}, nil)
	if err != nil {
		return err
	}
	fmt.Printf("user: %v\n", user)
	if user == nil {
		return ErrUserNotFound
	}
	if user.ID != userId {
		return ErrUserNotFound
	}

	return nil
}

func NewUserService(
	userRepository user_repository.UserRepository,
	roleRepository role_repository.RoleRepository,
	bus bus.Bus,
) *UserService {
	if userService == nil {
		userService = &UserService{
			userRepository: userRepository,
			roleRepository: roleRepository,
			bus:            bus,
		}
	}
	return userService
}

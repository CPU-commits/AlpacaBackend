package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
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
			Username: repository.CriteriaString{
				EQ: utils.String(username),
			},
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

func (userService *UserService) ThrowIfUserNotExists(idUser int64) error {
	exists, err := userService.userRepository.Exists(&user_repository.Criteria{
		ID: idUser,
	})
	if err != nil {
		return err
	}
	if !exists {
		return ErrUserNotFound
	}

	return nil
}

func (userService *UserService) GetEmailUserById(idUser int64) (string, error) {
	opts := user_repository.NewFindOneOptions().Select(user_repository.SelectOpts{
		Email: utils.Bool(true),
	})

	user, err := userService.userRepository.FindOne(
		&user_repository.Criteria{
			ID: idUser,
		},
		opts,
	)
	if err != nil {
		return "", nil
	}
	if user == nil {
		return "", ErrUserNotFound
	}

	return user.Email, nil
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

func (userService *UserService) SearchUsers(
	q string,
	filterUsers []int64,
	roles ...model.Role,
) ([]model.User, error) {
	opts := user_repository.NewFindOptions().
		Select(user_repository.SelectOpts{
			ID:       utils.Bool(true),
			Username: utils.Bool(true),
			Name:     utils.Bool(true),
			Email:    utils.Bool(true),
		}).
		Limit(5)

	return userService.userRepository.Find(
		&user_repository.Criteria{
			ID_NIN: filterUsers,
			Roles:  roles,
			Or: []user_repository.Criteria{
				{
					Email: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
				{
					Username: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
				{
					Name: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
			},
		},
		opts,
	)
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
		Email: repository.CriteriaString{
			EQ: utils.String(data.NewEmail),
		},
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
				Username: repository.CriteriaString{
					EQ: utils.String(params.UserName),
				},
			},
			{
				ID: params.ID,
			},
		},
	}, nil)
	if err != nil {
		return err
	}
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

package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type AdminStudioService struct {
	peopleStudioRepository people_studio_repository.PeopleStudioRepository
	studioRepository       studio_repository.StudioRepository
	userService            service.UserService
}

var adminStudioService *AdminStudioService

func (adminStudioService *AdminStudioService) userIsAdminInStudio(
	idUser,
	idStudio int64,
) (bool, error) {
	isAdmin, err := adminStudioService.peopleStudioRepository.Exists(&people_studio_repository.Criteria{
		IDUser:   idUser,
		IDStudio: idStudio,
		Roles:    []model.StudioRole{model.ADMIN_ROLE},
	})
	if err != nil {
		return false, err
	}

	return isAdmin, nil
}

func (adminStudioService *AdminStudioService) ThrowIfStudioIsNotActive(
	idStudio int64,
) error {
	isActive, err := adminStudioService.studioRepository.Exists(&studio_repository.Criteria{
		ID:       idStudio,
		IsActive: utils.Bool(true),
	})
	if err != nil {
		return err
	}
	if !isActive {
		return ErrStudioIsNotActive
	}

	return nil
}

func (adminStudioService *AdminStudioService) GetRolesInStudio(
	idUser,
	idStudio int64,
) ([]model.StudioRole, error) {
	isOwner, err := adminStudioService.studioRepository.Exists(&studio_repository.Criteria{
		IDOwner: idUser,
		ID:      idStudio,
	})
	if err != nil {
		return nil, err
	}
	if isOwner {
		return []model.StudioRole{model.OWNER_ROLE}, nil
	}

	person, err := adminStudioService.peopleStudioRepository.FindOne(
		&people_studio_repository.Criteria{
			IDUser:   idUser,
			IDStudio: idStudio,
		},
	)
	if err != nil {
		return nil, err
	}
	if person == nil {
		return nil, ErrUserNotInStudio
	}

	return person.Roles, nil
}

func (adminStudioService *AdminStudioService) ThrowAccessInStudioIfIsNotOwner(
	idUser,
	idStudio int64,
) error {
	studio, err := adminStudioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		studio_repository.NewFindOneOptions().
			Select(studio_repository.SelectOpts{
				IsActive: utils.Bool(true),
				ID:       *utils.Bool(true),
			}),
	)
	if err != nil {
		return err
	}
	if studio == nil {
		return ErrNoExistStudio
	}
	if !studio.IsActive {
		return ErrStudioIsNotActive
	}
	isOwner, err := adminStudioService.studioRepository.Exists(&studio_repository.Criteria{
		IDOwner: idUser,
		ID:      idStudio,
	})
	if err != nil {
		return err
	}
	if !isOwner {
		return ErrNoHasPermission
	}

	return nil
}

func (adminStudioService *AdminStudioService) ThrowAccessInStudio(
	idUser,
	idStudio int64,
	permissions ...model.StudioPermission,
) error {
	studio, err := adminStudioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		studio_repository.NewFindOneOptions().
			Select(studio_repository.SelectOpts{
				IsActive: utils.Bool(true),
				ID:       *utils.Bool(true),
			}),
	)
	if err != nil {
		return err
	}
	if studio == nil {
		return ErrNoExistStudio
	}
	if !studio.IsActive {
		return ErrStudioIsNotActive
	}

	isOwner, err := adminStudioService.studioRepository.Exists(&studio_repository.Criteria{
		IDOwner: idUser,
		ID:      idStudio,
	})
	if err != nil {
		return err
	}
	if isOwner {
		return nil
	}

	isAdmin, err := adminStudioService.userIsAdminInStudio(idUser, idStudio)
	if err != nil {
		return err
	}
	if !isAdmin {
		return ErrUserIsNotAdmin
	}
	if permissions != nil {
		hasPermissions, err := adminStudioService.peopleStudioRepository.Exists(&people_studio_repository.Criteria{
			IDUser:      idUser,
			IDStudio:    idStudio,
			Permissions: permissions,
		})
		if err != nil {
			return err
		}
		if !hasPermissions {
			return ErrNoHasPermission
		}
	}

	return nil
}

func (adminStudioService *AdminStudioService) getStudioOwnerToPeople(
	idStudio int64,
) (*model.StudioPerson, error) {
	opts := studio_repository.NewFindOneOptions().Select(
		studio_repository.SelectOpts{
			IDOwner: true,
			ID:      true,
		},
	)

	studio, err := adminStudioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		opts,
	)
	if err != nil {
		return nil, err
	}
	if studio == nil {
		return nil, ErrNoExistStudio
	}
	// Get user
	user, err := adminStudioService.userService.GetUserById(studio.Owner.ID)
	if err != nil {
		return nil, err
	}

	return &model.StudioPerson{
		ID:          0,
		IDUser:      user.ID,
		Roles:       []model.StudioRole{model.OWNER_ROLE},
		Permissions: model.ALL_PERMISSIONS,
		User:        user,
	}, nil
}

func (adminStudioService *AdminStudioService) GetStudioPeople(
	idUser,
	idStudio int64,
	roles ...model.StudioRole,
) ([]model.StudioPerson, error) {
	if err := adminStudioService.ThrowAccessInStudio(
		idUser,
		idStudio,
		model.SHOW_PEOPLE_PERMISSION,
	); err != nil {
		return nil, err
	}
	opts := people_studio_repository.NewFindOptions().
		Include(people_studio_repository.Include{
			User: &user_repository.SelectOpts{
				ID:       utils.Bool(true),
				Username: utils.Bool(true),
				Name:     utils.Bool(true),
				Email:    utils.Bool(true),
			},
		})

	people, err := adminStudioService.peopleStudioRepository.Find(
		&people_studio_repository.Criteria{
			IDStudio: idStudio,
			Roles:    roles,
		},
		opts,
	)
	if err != nil {
		return nil, err
	}

	if utils.Includes(roles, model.OWNER_ROLE) || roles == nil {
		owner, err := adminStudioService.getStudioOwnerToPeople(idStudio)
		if err != nil {
			return nil, err
		}

		return append([]model.StudioPerson{*owner}, people...), nil
	}

	return people, nil
}

func (adminStudioService *AdminStudioService) GetPermissionsInStudio(
	idUser,
	idStudio int64,
) ([]model.StudioPermission, bool, error) {
	isOwner, err := adminStudioService.studioRepository.Exists(&studio_repository.Criteria{
		IDOwner: idUser,
		ID:      idStudio,
	})
	if err != nil {
		return nil, false, err
	}
	if isOwner {
		return model.ALL_PERMISSIONS, true, nil
	}

	admin, err := adminStudioService.peopleStudioRepository.FindOne(
		&people_studio_repository.Criteria{
			IDStudio: idStudio,
			IDUser:   idUser,
		},
	)
	if admin == nil {
		return nil, false, ErrUserIsNotAdmin
	}

	return admin.Permissions, false, nil
}

func (adminStudioService *AdminStudioService) JoinPerson(
	idUser,
	idStudio,
	idAdmin int64,
	roles []model.StudioRole,
) error {
	if utils.Includes(roles, model.OWNER_ROLE) {
		return ErrCantCreateOwner
	}

	if err := adminStudioService.ThrowAccessInStudio(
		idAdmin,
		idStudio,
		model.JOIN_PEOPLE_PERMISSION,
	); err != nil {
		return err
	}
	// Exists user
	if err := adminStudioService.userService.ThrowIfUserNotExists(
		idUser,
	); err != nil {
		return err
	}
	// Join
	joined, err := adminStudioService.peopleStudioRepository.Exists(
		&people_studio_repository.Criteria{
			IDStudio: idStudio,
			IDUser:   idUser,
		},
	)
	if err != nil {
		return err
	}
	if joined {
		return nil
	}

	return adminStudioService.peopleStudioRepository.InsertOne(model.StudioPerson{
		IDStudio: idStudio,
		IDUser:   idUser,
		Roles:    roles,
	})
}

func (adminStudioService *AdminStudioService) ChangeRoles(
	idUser,
	idStudio,
	idAdmin int64,
	roles []model.StudioRole,
) error {
	if utils.Includes(roles, model.OWNER_ROLE) {
		return ErrCantCreateOwner
	}

	if err := adminStudioService.ThrowAccessInStudio(
		idAdmin,
		idStudio,
		model.GIVE_ROLES_PERMISSION,
	); err != nil {
		return err
	}
	// Join
	joined, err := adminStudioService.peopleStudioRepository.Exists(
		&people_studio_repository.Criteria{
			IDStudio: idStudio,
			IDUser:   idUser,
		},
	)
	if err != nil {
		return err
	}
	if !joined {
		return ErrUserNotInStudio
	}

	return adminStudioService.peopleStudioRepository.Update(
		&people_studio_repository.Criteria{
			IDUser:   idUser,
			IDStudio: idStudio,
		},
		people_studio_repository.UpdateData{
			Roles: roles,
		},
	)
}

func (adminStudioService *AdminStudioService) SetPermission(
	idUser,
	idStudio,
	idAdmin int64,
	permissionDto *dto.PermissionDTO,
) error {
	permission := model.StudioPermission(permissionDto.Permission)

	if err := adminStudioService.ThrowAccessInStudio(
		idAdmin,
		idStudio,
		model.GIVE_ROLES_PERMISSION,
		permission,
	); err != nil {
		return err
	}
	userIsAdmin, err := adminStudioService.userIsAdminInStudio(
		idUser,
		idStudio,
	)
	if err != nil {
		return err
	}
	if !userIsAdmin {
		return ErrTheUserIsNotAdmin
	}
	allPermissionsToSet := []model.StudioPermission{permission}

	if permissionDto.Enabled {
		p, _ := utils.Find(model.AllPermissionsTree, func(p model.Permission) (bool, error) {
			return p.Permission == permission, nil
		})

		allPermissionsToSet = append(allPermissionsToSet, p.DependsOn...)
	} else {
		ps := utils.FilterNoError(model.AllPermissionsTree, func(p model.Permission) bool {
			return utils.Includes(p.DependsOn, permission)
		})

		allPermissionsToSet = append(allPermissionsToSet, utils.MapNoError(ps, func(p model.Permission) model.StudioPermission {
			return p.Permission
		})...)
	}

	return utils.ConcurrentForEach(
		allPermissionsToSet,
		func(p model.StudioPermission, setError func(err error)) {
			err := adminStudioService.peopleStudioRepository.Update(
				&people_studio_repository.Criteria{
					IDStudio: idStudio,
					IDUser:   idUser,
				},
				people_studio_repository.UpdateData{
					Permission: &people_studio_repository.UpdatePermission{
						Permission: p,
						Enabled:    permissionDto.Enabled,
					},
				},
			)
			if err != nil {
				setError(err)
			}
		},
		nil,
	)
}

func (adminStudioService *AdminStudioService) RemovePerson(
	idUser,
	idStudio,
	idAdmin int64,
) error {
	if err := adminStudioService.ThrowAccessInStudio(
		idAdmin,
		idStudio,
		model.DELETE_PEOPLE_PERMISSION,
	); err != nil {
		return err
	}

	return adminStudioService.peopleStudioRepository.Delete(
		&people_studio_repository.Criteria{
			IDStudio: idStudio,
			IDUser:   idUser,
		},
	)
}

func NewPeopleStudioService(
	peopleStudioRepository people_studio_repository.PeopleStudioRepository,
	studioRepository studio_repository.StudioRepository,
	userService service.UserService,
) *AdminStudioService {
	if adminStudioService == nil {
		adminStudioService = &AdminStudioService{
			peopleStudioRepository: peopleStudioRepository,
			studioRepository:       studioRepository,
			userService:            userService,
		}
	}

	return adminStudioService
}

package service

import (
	"fmt"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var profileService *ProfileService

type ProfileService struct {
	profileRepository       profile_repository.ProfileRepository
	followRepository        follow_repository.FollowRepository
	userService             authService.UserService
	imageStore              store.ImageStore
	fileService             file_service.FileService
	publicationRDRepository publication_repository.RedisPublicationRepository
}

func (profileService *ProfileService) GetAvatarFromIDUser(idUser int64) (string, error) {
	profile, err := profileService.profileRepository.FindOne(
		&profile_repository.Criteria{
			IDUser: idUser,
		},
		profile_repository.NewFindOneOptions().
			Load(profile_repository.LoadOpts{
				Avatar: true,
			}).
			Select(profile_repository.SelectOpts{
				Avatar: utils.Bool(true),
			}),
	)
	if err != nil {
		return "", err
	}
	if profile == nil {
		return "", ErrUserNoHasProfile
	}
	if profile.Avatar == nil {
		return "", nil
	}

	return profile.Avatar.Key, nil
}

func (profileService *ProfileService) GetProfile(username string) (*model.Profile, error) {
	idProfile, err := profileService.GetProfileIdFromUsername(username)
	if err != nil {
		return nil, err
	}

	opts := profile_repository.NewFindOneOptions().
		Load(profile_repository.LoadOpts{
			Avatar: true,
			User: &user_repository.SelectOpts{
				Name:     utils.Bool(true),
				ID:       utils.Bool(true),
				Email:    utils.Bool(true),
				Phone:    utils.Bool(true),
				Location: utils.Bool(true),
				Username: utils.Bool(true),
			},
			Roles:     true,
			UserMedia: true,
		})

	profile, err := profileService.profileRepository.FindOne(
		&profile_repository.Criteria{
			ID: idProfile,
		},
		opts,
	)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return nil, err
	}
	return profile, nil
}

func (profileService *ProfileService) SearchProfile(q string) ([]model.Profile, error) {
	users, err := profileService.userService.SearchUsers(q, nil)
	if err != nil {
		return nil, err
	}
	if users == nil {
		return nil, nil
	}

	opts := profile_repository.NewFindOptions().
		Load(profile_repository.LoadOpts{
			Avatar: true,
			User: &user_repository.SelectOpts{
				Name:     utils.Bool(true),
				ID:       utils.Bool(true),
				IDUser:   utils.Bool(true),
				Username: utils.Bool(true),
			},
			Roles: true,
		})

	profiles, err := profileService.profileRepository.Find(
		&profile_repository.Criteria{
			IDUser_IN: utils.MapNoError(users, func(user authModel.User) int64 {
				return user.ID
			}),
		},
		opts,
	)
	if err != nil {
		return nil, err
	}
	return profiles, nil
}

func (profileService *ProfileService) UpdateProfile(
	updateProfileDto *dto.UpdateProfileDto,
	idUser int64,
) error {
	idProfile, err := profileService.GetProfileIDFromIDUser(idUser)
	if err != nil {
		return err
	}

	return profileService.profileRepository.UpdateOne(
		&profile_repository.Criteria{
			ID: idProfile,
		},
		profile_repository.UpdateData{
			Description: updateProfileDto.Description,
		},
	)
}

func (profileService *ProfileService) ChangeAvatar(
	imageDto store.ImageDto,
	idUser int64,
) (string, error) {
	err := profileService.fileService.CheckImageMimeType(imageDto)

	if err != nil {
		return "", err
	}

	opts := profile_repository.NewFindOneOptions().
		Load(profile_repository.LoadOpts{
			Avatar: true,
		}).
		Select(profile_repository.SelectOpts{
			Avatar: utils.Bool(true),
		})

	profile, err := profileService.profileRepository.FindOne(
		&profile_repository.Criteria{
			IDUser: idUser,
		},
		opts,
	)
	if err != nil {
		return "", err
	}
	if profile.Avatar != nil {
		if err := profileService.imageStore.Delete(profile.Avatar.Key); err != nil {
			return "", err
		}
	}

	image, err := profileService.imageStore.Upload(imageDto, fmt.Sprintf("avatars/%d", idUser))
	if err != nil {
		return "", err
	}

	if err := profileService.profileRepository.UpdateOne(
		&profile_repository.Criteria{
			IDUser: idUser,
		},
		profile_repository.UpdateData{
			Avatar: image,
		},
	); err != nil {
		return "", err
	}

	return image.Key, nil
}

func (profileService *ProfileService) GetProfileIdFromUsername(username string) (int64, error) {
	idUser, err := profileService.userService.GetUserIDFromUsername(username)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return 0, err
	}

	return profileService.GetProfileIDFromIDUser(idUser)
}

func (profileService *ProfileService) GetProfileIDFromIDUser(idUser int64) (int64, error) {
	opts := profile_repository.NewFindOneOptions().Select(profile_repository.SelectOpts{
		ID: utils.Bool(true),
	})

	profile, err := profileService.profileRepository.FindOne(
		&profile_repository.Criteria{
			IDUser: idUser,
		},
		opts,
	)
	if err != nil {
		return 0, err
	}
	if profile == nil {
		return 0, ErrUserNoHasProfile
	}

	return profile.ID, nil
}

func (profileService *ProfileService) GetFollows(idProfile int64) (int64, error) {
	return profileService.followRepository.Count(&follow_repository.Criteria{
		IDProfile: idProfile,
	})
}

// GetAllUserView
func (profileService *ProfileService) GetAllUserView(identifier string) ([]int64, error) {

	userViews, err := profileService.publicationRDRepository.GetAllUserView(identifier)
	if err != nil {
		return []int64{}, err
	}
	return userViews, err
}

func NewProfileService(
	profileRepository profile_repository.ProfileRepository,
	userService authService.UserService,
	imageStore store.ImageStore,
	fileService file_service.FileService,
	followRepository follow_repository.FollowRepository,
	publicationRDRepository publication_repository.RedisPublicationRepository,

) *ProfileService {
	if profileService == nil {
		profileService = &ProfileService{
			profileRepository:       profileRepository,
			userService:             userService,
			imageStore:              imageStore,
			fileService:             fileService,
			followRepository:        followRepository,
			publicationRDRepository: publicationRDRepository,
		}
	}
	return profileService
}

package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
)

type FollowService struct {
	followRepository follow_repository.FollowRepository
	profileService   service.ProfileService
}

var followService *FollowService

func (followService *FollowService) GetMyFollow(
	params GetFollowParam,
	idUser int64,
) (bool, error) {
	var idProfile int64

	if params.Username != "" {
		var err error

		idProfile, err = followService.profileService.GetProfileIdFromUsername(
			params.Username,
		)
		if err != nil {
			return false, err
		}
	}

	return followService.followRepository.Exists(
		&follow_repository.Criteria{
			IDStudio:  params.IDStudio,
			IDProfile: idProfile,
		},
	)
}

func (followService *FollowService) ToggleFollow(
	followDto *dto.FollowDto,
	idUser int64,
	follow bool,
) error {
	followModel, err := followDto.ToModel(idUser)
	if err != nil {
		return err
	}

	var idProfile int64

	if followDto.Username != "" {
		var err error

		idProfile, err = followService.profileService.GetProfileIdFromUsername(
			followDto.Username,
		)
		if err != nil {
			return err
		}
		myIdProfile, err := followService.profileService.GetProfileIDFromIDUser(idUser)
		if err != nil {
			return err
		}
		if myIdProfile == idProfile {
			return ErrCantFollowYou
		}
		followModel.IDProfile = idProfile
	}

	criteria := &follow_repository.Criteria{
		IDStudio:  followDto.IDStudio,
		IDProfile: idProfile,
	}
	hasFollow, err := followService.followRepository.Exists(
		criteria,
	)
	if err != nil {
		return err
	}
	if hasFollow && follow || !hasFollow && !follow {
		return nil
	}
	if follow {
		return followService.followRepository.Insert(
			*followModel,
		)
	}

	return followService.followRepository.Delete(criteria)
}

func NewFollowService(
	followRepository follow_repository.FollowRepository,
	profileService service.ProfileService,
) *FollowService {
	if followService == nil {
		followService = &FollowService{
			followRepository: followRepository,
			profileService:   profileService,
		}
	}

	return followService
}

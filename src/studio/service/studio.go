package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
)

type StudioService struct {
	studioRepository studio_repository.StudioRepository
	authService      service.AuthService
	fileService      fileService.FileService
}

var studioService *StudioService

func (studioService *StudioService) GetPermissions() []model.StudioPermission {
	return model.ALL_PERMISSIONS
}

func (studioService *StudioService) GetStudio(
	idStudio int64,
) (*model.Studio, error) {
	opts := studio_repository.NewFindOneOptions().
		Include(studio_repository.Include{
			AvatarImage: true,
			BannerImage: true,
		})

	return studioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		opts,
	)
}

func (studioService *StudioService) GetMyStudios(
	idUser int64,
) ([]model.Studio, error) {
	opts := studio_repository.NewFindOptions().
		Include(studio_repository.Include{
			AvatarImage: true,
			BannerImage: true,
		}).
		Select(studio_repository.SelectOpts{
			Name:        true,
			Username:    true,
			Description: true,
			ID:          true,
		})

	return studioService.studioRepository.Find(
		&studio_repository.Criteria{
			IDOwner: idUser,
		},
		opts,
	)
}

func (studioService *StudioService) checkIfExistsEmailOrUsername(
	email,
	username string,
) error {
	if err := studioService.authService.CheckIfEmailOrUsernameExists(
		email,
		username,
	); err != nil {
		return err
	}

	exists, err := studioService.studioRepository.Exists(&studio_repository.Criteria{
		OR: []studio_repository.Criteria{
			{Email: email},
			{Username: username},
		},
	})
	if err != nil {
		return err
	}
	if exists {
		return ErrExistsEmailOrUsername
	}
	return nil
}

func (studioService *StudioService) CreateStudio(
	studio *dto.StudioDTO,
	idUser int64,
) error {
	count, err := studioService.studioRepository.Count(&studio_repository.Criteria{
		IDOwner: idUser,
	})
	if err != nil {
		return err
	}
	if count >= 2 {
		return ErrMaxStudios
	}
	if err := studioService.checkIfExistsEmailOrUsername(
		studio.Email,
		studio.Username,
	); err != nil {
		return err
	}
	// Upload images
	var avatarImage *fileModel.Image
	if studio.AvatarImage != nil {
		images, err := studioService.fileService.UploadImages([]store.ImageDto{*studio.AvatarImage}, "studios")
		if err != nil {
			return err
		}
		avatarImage = &images[0]
	}

	var bannerImage *fileModel.Image
	if studio.BannerImage != nil {
		images, err := studioService.fileService.UploadImages([]store.ImageDto{*studio.BannerImage}, "studios")
		if err != nil {
			return err
		}
		bannerImage = &images[0]
	}

	return studioService.studioRepository.InsertOne(
		studio.ToModel(avatarImage, bannerImage, idUser),
	)
}

func NewStudioService(
	studioRepository studio_repository.StudioRepository,
	authService service.AuthService,
	fileService fileService.FileService,
) *StudioService {
	if studioService == nil {
		studioService = &StudioService{
			studioRepository: studioRepository,
			authService:      authService,
			fileService:      fileService,
		}
	}

	return studioService
}

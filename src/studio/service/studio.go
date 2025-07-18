package service

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type StudioService struct {
	studioRepository   studio_repository.StudioRepository
	adminStudioService AdminStudioService
	authService        service.AuthService
	imageStore         store.ImageStore
	fileService        fileService.FileService
	uidGenerator       uid.UIDGenerator
}

var studioService *StudioService

func (*StudioService) toShortLinksMedia(media []model.Media) []model.Media {
	return utils.MapNoError(media, func(media model.Media) model.Media {
		return model.Media{
			ID:   media.ID,
			Type: media.Type,
			Link: fmt.Sprintf("%s/s/%s", settingsData.BACKEND_URL, media.ShortCode),
		}
	})
}

func (studioService *StudioService) GetPermissions() []model.Permission {
	return model.AllPermissionsTree
}

func (studioService *StudioService) GetStudio(
	idStudio int64,
) (*model.Studio, error) {
	opts := studio_repository.NewFindOneOptions().
		Include(studio_repository.Include{
			AvatarImage: true,
			BannerImage: true,
			Media:       true,
		})

	studio, err := studioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		opts,
	)
	if err != nil {
		return nil, err
	}
	studio.Media = studioService.toShortLinksMedia(studio.Media)

	return studio, nil
}

func (studioService *StudioService) GetStudioUsername(
	idStudio int64,
) (string, error) {
	opts := studio_repository.NewFindOneOptions().
		Select(studio_repository.SelectOpts{
			ID:       true,
			Username: true,
		})

	studio, err := studioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		opts,
	)
	if err != nil {
		return "", err
	}

	return studio.Username, nil
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

func (studioService *StudioService) UpdateStudio(
	studio *dto.UpdateStudioDTO,
	idUser,
	idStudio int64,
) error {
	if err := studioService.adminStudioService.ThrowAccessInStudio(
		idUser,
		idStudio,
		model.UPDATE_STUDIO_PERMISSION,
	); err != nil {
		return err
	}
	studioData, err := studioService.studioRepository.FindOne(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		studio_repository.NewFindOneOptions().
			Select(studio_repository.SelectOpts{
				IDAvatar: utils.Bool(true),
				IDBanner: utils.Bool(true),
				ID:       true,
			}).
			Include(studio_repository.Include{
				AvatarImage: true,
				BannerImage: true,
			}),
	)
	if err != nil {
		return err
	}
	// Upload images
	var avatarImage *fileModel.Image
	if studio.AvatarImage != nil {
		images, err := studioService.fileService.UploadImages([]store.ImageDto{*studio.AvatarImage}, "studios")
		if err != nil {
			return err
		}
		if studioData.Avatar != nil {
			err = studioService.imageStore.Delete(studioData.Avatar.Key)
			if err != nil {
				return err
			}
		}
		avatarImage = &images[0]
	}

	var bannerImage *fileModel.Image
	if studio.BannerImage != nil {
		images, err := studioService.fileService.UploadImages([]store.ImageDto{*studio.BannerImage}, "studios")
		if err != nil {
			return err
		}
		if studioData.Banner != nil {
			err = studioService.imageStore.Delete(studioData.Banner.Key)
			if err != nil {
				return err
			}
		}
		bannerImage = &images[0]
	}
	media, err := studio.ToMedia(studioService.uidGenerator, idStudio)
	if err != nil {
		return err
	}

	return studioService.studioRepository.Update(
		&studio_repository.Criteria{
			ID: idStudio,
		},
		studio_repository.UpdateData{
			Banner:      bannerImage,
			Avatar:      avatarImage,
			Name:        studio.Name,
			Description: studio.Description,
			FullAddress: studio.Address,
			Email:       studio.Email,
			Phone:       studio.Phone,
			RemoveMedia: studio.RemoveMedia,
			AddMedia:    media,
		},
	)
}

func NewStudioService(
	studioRepository studio_repository.StudioRepository,
	authService service.AuthService,
	fileService fileService.FileService,
	adminStudioService AdminStudioService,
	imageStore store.ImageStore,
	uidGenerator uid.UIDGenerator,
) *StudioService {
	if studioService == nil {
		studioService = &StudioService{
			studioRepository:   studioRepository,
			authService:        authService,
			fileService:        fileService,
			imageStore:         imageStore,
			adminStudioService: adminStudioService,
			uidGenerator:       uidGenerator,
		}
	}

	return studioService
}

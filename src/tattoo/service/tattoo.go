package service

import (
	"fmt"

	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var tattooService *TattooService

type TattooService struct {
	imageStore       store.ImageStore
	profileService   service.ProfileService
	tattooRepository tattoo_repository.TattooRepository
	fileService      file_service.FileService
}

func (tattooService *TattooService) updateViews(idTattoos []int64) {
	tattooService.tattooRepository.UpdateViews(idTattoos)
}

type TattoosMetadata struct {
	Limit int
	Total int
}

func (tattooService *TattooService) GetTattoos(username string, page int) ([]model.Tattoo, *TattoosMetadata, error) {
	idProfile, err := tattooService.profileService.GetProfileIdFromUsername(
		username,
	)
	if err != nil {
		return nil, nil, err
	}
	limit := 10

	opts := tattoo_repository.NewFindOptions().
		Limit(limit).
		Skip(limit * page).
		Include(tattoo_repository.Include{
			Image:         true,
			Categories:    true,
			ProfileAvatar: true,
			ProfileUser:   true,
		}).
		Sort(tattoo_repository.Sort{
			CreatedAt: "DESC",
		})

	tattoos, err := tattooService.tattooRepository.Find(
		&tattoo_repository.Criteria{
			IDProfile: idProfile,
		},
		opts,
	)
	if err != nil {
		return nil, nil, err
	}
	// Counts
	count, err := tattooService.tattooRepository.Count(nil)
	if err != nil {
		return nil, nil, err
	}
	go tattooService.updateViews(utils.MapNoError(tattoos, func(tattoo model.Tattoo) int64 {
		return tattoo.ID
	}))

	return tattoos, &TattoosMetadata{
		Limit: limit,
		Total: int(count),
	}, nil
}

func (tattooService *TattooService) GetLatestTattoos(username string) ([]model.Tattoo, error) {
	idProfile, err := tattooService.profileService.GetProfileIdFromUsername(
		username,
	)
	if err != nil {
		return nil, err
	}

	opts := tattoo_repository.NewFindOptions().
		Limit(5).
		Include(tattoo_repository.Include{
			Image: true,
		}).
		Sort(tattoo_repository.Sort{
			CreatedAt: "DESC",
		})
	return tattooService.tattooRepository.Find(
		&tattoo_repository.Criteria{
			IDProfile: idProfile,
		},
		opts,
	)
}

func (tattooService *TattooService) PublishTattoos(
	tattoosDto []dto.TattooDto,
	idUser int64,
) ([]model.Tattoo, error) {
	idProfile, err := tattooService.profileService.GetProfileIDFromIDUser(idUser)
	if err != nil {
		return nil, err
	}
	if err := utils.ForEach(tattoosDto, func(tattooDto dto.TattooDto) error {
		return nil
	}); err != nil {
		return nil, err
	}

	tattoos, err := utils.ConcurrentMap(tattoosDto, func(tattooDto dto.TattooDto) (model.Tattoo, error) {
		err := tattooService.fileService.CheckImageMimeType(tattooDto.Image)
		if err != nil {
			return model.Tattoo{}, err
		}

		image, err := tattooService.imageStore.Upload(tattooDto.Image, fmt.Sprintf("tattoos/%d", idUser))
		if err != nil {
			return model.Tattoo{}, err
		}
		tattoo := tattooDto.ToModel(*image)

		tattoo.Categories = utils.ExtractWords[string](tattoo.Description, "#")

		return tattoo, nil
	}, nil)
	if err != nil {
		return nil, err
	}

	modelTatto, err := tattooService.tattooRepository.Insert(tattoos, idProfile)
	if err != nil {

		return nil, err
	}
	return modelTatto, nil
}

func NewTattooService(
	imageStore store.ImageStore,
	profileService service.ProfileService,
	tattooRepository tattoo_repository.TattooRepository,
	fileService file_service.FileService,

) *TattooService {
	if tattooService == nil {
		tattooService = &TattooService{
			imageStore:       imageStore,
			profileService:   profileService,
			tattooRepository: tattooRepository,
			fileService:      fileService,
		}
	}

	return tattooService
}

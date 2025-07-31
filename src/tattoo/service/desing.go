package service

import (
	"fmt"

	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/design_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var designService *DesignService

type DesignsMetadata struct {
	Limit int
	Total int
}

type DesignService struct {
	imageStore       store.ImageStore
	profileService   service.ProfileService
	designRepository design_repository.DesignRepository
	fileService      file_service.FileService
}

func (designService *DesignService) PublishDesigns(designsDto []dto.DesignDto, userId int64) ([]model.Design, error) {
	profileId, err := designService.profileService.GetProfileIDFromIDUser(userId)
	if err != nil {
		return nil, err
	}
	designs, err := utils.ConcurrentMap(designsDto, func(designDto dto.DesignDto) (model.Design, error) {
		err := designService.fileService.CheckImageMimeType(designDto.Image)
		if err != nil {
			return model.Design{}, err
		}

		image, err := designService.imageStore.Upload(designDto.Image, fmt.Sprintf("designs/%d", userId))
		if err != nil {
			return model.Design{}, err
		}
		design := designDto.ToModel(*image)
		design.Categories = utils.ExtractWords[string](design.Description, "#")
		return design, nil
	}, nil)
	if err != nil {
		return nil, err
	}
	modelDesigns, err := designService.designRepository.Insert(designs, profileId)
	if err != nil {
		return nil, err
	}
	return modelDesigns, nil
}

func (designService *DesignService) GetDesigns(username string, params dto.DesignFindDto) ([]model.Design, *DesignsMetadata, error) {
	profileId, err := designService.profileService.GetProfileIdFromUsername(username)
	if err != nil {
		return nil, nil, err
	}

	limit := 20
	skip := 0
	if params.SortCreatedAt == "" {
		params.SortCreatedAt = "DESC"
	}
	if !params.Paginated {
		limit = 0
	} else {
		skip = limit * params.Page
	}
	opts := design_repository.NewFindOptions().
		Limit(limit).
		Skip(skip).
		Include(design_repository.Include{
			Image:       true,
			ProfileUser: true,
			Categories:  true,
		}).
		Sort(design_repository.Sort{
			CreatedAt: params.SortCreatedAt,
			Price:     params.SortPrice,
		})
	desings, err := designService.designRepository.Find(&design_repository.Criteria{
		IDProfile: profileId,
		Category:  params.Category,
	}, opts)
	fmt.Printf("desings: %v\n", desings)
	if err != nil {
		return nil, nil, err
	}

	count, err := designService.designRepository.Count(&design_repository.Criteria{IDProfile: profileId})
	if err != nil {
		return nil, nil, err
	}

	return desings, &DesignsMetadata{
		Limit: limit,
		Total: int(count),
	}, nil
}

func (designService *DesignService) GetLatestDesigns(username string) ([]model.Design, error) {
	profileId, err := designService.profileService.GetProfileIdFromUsername(username)
	if err != nil {
		return nil, err
	}
	opts := design_repository.NewFindOptions().Limit(5).Include(design_repository.Include{
		Image: true,
	}).Sort(design_repository.Sort{
		CreatedAt: "DESC",
	})

	return designService.designRepository.Find(&design_repository.Criteria{
		IDProfile: profileId,
	}, opts)
}

func (designService *DesignService) UpdateDesign(profileId int64, data dto.DataUpdate) error {
	if data.Description == "" && data.Price == 0 {
		return ErrNotParams
	}

	return designService.designRepository.Update(&design_repository.Criteria{
		ID:        data.ID,
		IDProfile: profileId,
	}, design_repository.UpdateData{
		Description: &data.Description,
		Price:       &data.Price,
	})
}

func (designService *DesignService) DeleteDesign(idUser int64, designId int64) error {
	idProfile, err := designService.profileService.GetProfileIDFromIDUser(idUser)
	if err != nil {
		return err
	}

	return designService.designRepository.Delete(&design_repository.Criteria{
		ID:        designId,
		IDProfile: idProfile,
	})
}

func (designService *DesignService) GetDesignCategories(username string) ([]string, error) {
	profileId, err := designService.profileService.GetProfileIdFromUsername(username)
	if err != nil {
		return nil, err
	}

	return designService.designRepository.GetCategories(&design_repository.Criteria{
		IDProfile: profileId,
	})
}

func NewDesignService(
	imageStore store.ImageStore,
	profileService service.ProfileService,
	designRepository design_repository.DesignRepository,
	fileService file_service.FileService,
) *DesignService {
	if designService == nil {
		designService = &DesignService{
			imageStore:       imageStore,
			profileService:   profileService,
			designRepository: designRepository,
			fileService:      fileService,
		}
	}
	return designService
}

package service

import (
	"fmt"

	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	modelPublication "github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	view_service "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

var tattooService *TattooService

type TattooService struct {
	imageStore       store.ImageStore
	profileService   service.ProfileService
	tattooRepository tattoo_repository.TattooRepository
	fileService      file_service.FileService
	embedding        embedding.Embedding
	bus              bus.Bus
	viewService      view_service.ViewService
}

func (tattooService *TattooService) updateViews(idTattoos []int64) {
	tattooService.tattooRepository.UpdateViews(idTattoos)
}

type TattoosMetadata struct {
	Limit int
	Total int
}

func (tattooService *TattooService) GetUrlImageTattoo(
	idTattoo int64,
) (string, error) {
	opts := tattoo_repository.NewFindOneOptions().Include(tattoo_repository.Include{
		Image: true,
	})

	tattoo, err := tattooService.tattooRepository.FindOne(
		&tattoo_repository.Criteria{
			ID: idTattoo,
		},
		opts,
	)
	if err != nil {
		return "", err
	}

	return tattoo.Image.Key, nil
}

func (tattooService *TattooService) SearchByImage(
	params SearchByImageParams,
	page int,
) ([]model.Tattoo, *TattoosMetadata, error) {
	limit := 10

	paramsSimilarity := tattoo_repository.SimilarityParams{}
	if params.Image != nil {
		base64Image, err := utils.ReaderImageToBase64(params.Image)
		if err != nil {
			return nil, nil, err
		}
		paramsSimilarity.ImageBase64 = base64Image
	} else if params.IsLikeTattooID != 0 {
		paramsSimilarity.IDTattoo = params.IsLikeTattooID
	}
	opts := tattoo_repository.NewSimilarityOptions().
		Limit(limit).
		Skip(limit * page).
		Include(tattoo_repository.Include{
			Image:         true,
			Categories:    true,
			ProfileAvatar: true,
			ProfileUser:   true,
		})

	tattoos, total, err := tattooService.tattooRepository.TattooSimilarity(
		paramsSimilarity,
		opts,
	)
	if err != nil {
		return nil, nil, err
	}

	return tattoos, &TattoosMetadata{
		Limit: limit,
		Total: int(total),
	}, nil
}

func (tattooService *TattooService) GetTattoos(params GetTattoosParams, page int, identifie, ip string) ([]model.Tattoo, *TattoosMetadata, error) {
	criteria := &tattoo_repository.Criteria{}

	if params.Username != "" {
		idProfile, err := tattooService.profileService.GetProfileIdFromUsername(
			params.Username,
		)
		if err != nil {
			return nil, nil, err
		}
		criteria.IDProfile = idProfile
	}
	if params.IDStudio != 0 {
		criteria.IDStudio = params.IDStudio
	}
	if params.IDPublication != 0 {
		criteria.IDPublication = params.IDPublication
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
		criteria,
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
	if identifie != "" || ip != "" {
		go utils.ConcurrentForEach(tattoos, func(tattoo model.Tattoo, setError func(err error)) {
			tattooService.viewService.AddPermanentViewIfTemporalViewNotExists(identifie, view_service.ToView{
				IDTattoo: tattoo.ID,
			}, utils.String(ip))
		}, &utils.OptionsConcurrentForEach{
			MaxConcurrency: 10,
		})
	}
	tattoosWithViews := utils.MapNoError(tattoos, func(tattoo model.Tattoo) model.Tattoo {
		views, err := tattooService.viewService.CountAllViews(view_service.ToView{
			IDTattoo: tattoo.ID,
		})
		if err != nil {
			return model.Tattoo{}
		}
		tattoo.Views = int(views)
		return tattoo
	})
	return tattoosWithViews, &TattoosMetadata{
		Limit: limit,
		Total: int(count),
	}, nil
}

func (tattooService *TattooService) GetLatestTattoos(params GetTattoosParams) ([]model.Tattoo, error) {
	criteria := &tattoo_repository.Criteria{}

	if params.Username != "" {
		idProfile, err := tattooService.profileService.GetProfileIdFromUsername(
			params.Username,
		)
		if err != nil {
			return nil, err
		}
		criteria.IDProfile = idProfile
	}
	if params.IDStudio != 0 {
		criteria.IDStudio = params.IDStudio
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
		criteria,
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
	// if err := utils.ForEach(tattoosDto, func(tattooDto dto.TattooDto) error {
	// 	return nil
	// }); err != nil {
	// 	return nil, err
	// }

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

		tattoo.Categories = utils.ExtractHashtags(tattoo.Description)

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

type DataUpdateRating struct {
	Tattoos       []model.Tattoo
	TSPublication modelPublication.TSPublication
}

func (tattooService *TattooService) UpdateRating(publication modelPublication.TSPublication, tattoos []model.Tattoo) {
	data := DataUpdateRating{
		Tattoos:       tattoos,
		TSPublication: publication,
	}
	go tattooService.bus.Publish(bus.Event{
		Name:    UPDATE_TATTOOS_RATINGS,
		Payload: utils.ToPayload(data),
	})
}
func (tattooService *TattooService) DeleteTattoos(tattoos []model.Tattoo) error {

	go tattooService.bus.Publish(bus.Event{
		Name:    DELETE_TATTOO,
		Payload: utils.Payload(tattoos),
	})
	return nil
}

func NewTattooService(
	imageStore store.ImageStore,
	profileService service.ProfileService,
	tattooRepository tattoo_repository.TattooRepository,
	fileService file_service.FileService,
	embedding embedding.Embedding,
	bus bus.Bus,
	viewService view_service.ViewService,

) *TattooService {
	if tattooService == nil {
		tattooService = &TattooService{
			imageStore:       imageStore,
			profileService:   profileService,
			tattooRepository: tattooRepository,
			fileService:      fileService,
			embedding:        embedding,
			bus:              bus,
			viewService:      viewService,
		}
	}

	return tattooService
}

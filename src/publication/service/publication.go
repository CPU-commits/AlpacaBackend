package service

import (
	"fmt"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/llm"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/llm/openaiprovider"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/like_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	studioModel "github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var publicationService *PublicationService

type PublicationService struct {
	tattooService         service.TattooService
	profileService        userService.ProfileService
	imageStore            store.ImageStore
	publicationRepository publication_repository.PublicationRepository
	likeRepository        like_repository.LikeRepository
	tattooRepository      tattoo_repository.TattooRepository
	fileService           file_service.FileService
	bus                   bus.Bus
	userRepository        user_repository.UserRepository
	adminStudioService    studioService.AdminStudioService
}

type PublicationsMetadata struct {
	Limit int
	Total int
}

func (publicationService *PublicationService) GetPublications(
	params PublicationsParams,
	page int,
) ([]model.Publication, *PublicationsMetadata, error) {
	criteria := &publication_repository.Criteria{}

	if params.Username != "" {
		idProfile, err := publicationService.profileService.GetProfileIdFromUsername(
			params.Username,
		)
		if err != nil {
			return nil, nil, err
		}
		criteria.IDProfile = idProfile
		criteria.IDStudio = &repository.CriteriaNull[*int64]{
			EQ: nil,
		}
	} else if params.IDStudio != 0 {
		criteria.IDStudio = &repository.CriteriaNull[*int64]{
			EQ: &params.IDStudio,
		}
	}

	limit := 10
	opts := publication_repository.NewFindOptions().
		Include(publication_repository.Include{
			Tattoos:       true,
			TattoosImage:  true,
			Images:        true,
			Categories:    true,
			Profile:       true,
			ProfileAvatar: true,
			ProfileUser:   true,
		}).
		Limit(limit).
		Skip(page * limit).
		Select(publication_repository.SelectOpts{
			User: &user_repository.SelectOpts{
				ID:       utils.Bool(true),
				Username: utils.Bool(true),
				Name:     utils.Bool(true),
			},
		}).
		Sort(publication_repository.Sort{
			CreatedAt: "DESC",
		})

	publications, err := publicationService.publicationRepository.Find(
		criteria,
		opts,
	)
	if err != nil {
		return nil, nil, err
	}
	// Count
	count, err := publicationService.publicationRepository.Count(
		criteria,
	)

	return publications, &PublicationsMetadata{
		Limit: limit,
		Total: int(count),
	}, nil
}

func (publicationService *PublicationService) GetPublication(
	idPublication int64,
) (*model.Publication, error) {
	opts := publication_repository.NewFindOneOptions().
		Include(publication_repository.Include{
			Tattoos:       true,
			TattoosImage:  true,
			Categories:    true,
			Images:        true,
			Profile:       true,
			ProfileAvatar: true,
			ProfileUser:   true,
		})

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPublication,
		},
		opts,
	)
	if err != nil {
		return nil, err
	}
	if publication == nil {
		return nil, ErrPublicationNotExists
	}

	return publication, nil
}

func (publicationService *PublicationService) Search(
	q string,
	categories []string,
	page int,
) ([]model.Publication, *PublicationsMetadata, error) {
	limit := 10
	opts := publication_repository.NewSearchOptions().
		Include(publication_repository.Include{
			Tattoos:       true,
			TattoosImage:  true,
			Images:        true,
			Categories:    true,
			Profile:       true,
			ProfileAvatar: true,
			ProfileUser:   true,
		}).
		Limit(limit).
		Skip(page * limit).
		Select(publication_repository.SelectOpts{
			User: &user_repository.SelectOpts{
				ID:       utils.Bool(true),
				Username: utils.Bool(true),
				Name:     utils.Bool(true),
			},
		})

	publications, founded, err := publicationService.publicationRepository.Search(
		q,
		&publication_repository.Criteria{
			Categories: categories,
		},
		opts,
	)

	return publications, &PublicationsMetadata{
		Total: int(founded),
		Limit: limit,
	}, err
}

func (publicationService *PublicationService) GetMyLike(
	idPost int64,
	idUser int64,
) (bool, error) {
	opts := publication_repository.NewFindOneOptions().
		Select(publication_repository.SelectOpts{
			IDProfile: utils.Bool(true),
			ID:        utils.Bool(true),
		})

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPost,
		},
		opts,
	)
	if err != nil {
		return false, err
	}
	if publication == nil {
		return false, ErrPublicationNotExists
	}

	return publicationService.likeRepository.Exists(
		&like_repository.Criteria{
			IDUser: idUser,
			IDPost: idPost,
		},
	)
}

func (publicationService *PublicationService) HandleLike(
	idPost int64,
	idUser int64,
) (bool, error) {
	opts := publication_repository.NewFindOneOptions().
		Select(publication_repository.SelectOpts{
			IDProfile: utils.Bool(true),
			ID:        utils.Bool(true),
		})

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPost,
		},
		opts,
	)
	if err != nil {
		return false, err
	}
	if publication == nil {
		return false, ErrPublicationNotExists
	}
	// Exists like
	existsLike, err := publicationService.likeRepository.Exists(
		&like_repository.Criteria{
			IDProfile: publication.IDProfile,
			IDUser:    idUser,
			IDPost:    idPost,
		},
	)
	if err != nil {
		return false, err
	}
	if existsLike {

		err = publicationService.likeRepository.Delete(&like_repository.Criteria{
			IDProfile: publication.IDProfile,
			IDUser:    idUser,
			IDPost:    idPost,
		})
		if err != nil {

			return false, err
		}
		if err := publicationService.publicationRepository.UpdateOne(
			&publication_repository.Criteria{
				ID: publication.ID,
			},
			publication_repository.UpdateData{
				SumLikes: -1,
			},
		); err != nil {
			return false, err
		}
		err = publicationService.interactionEvent(publication.ID)
		if err != nil {
			return false, err
		}
		return false, nil
	} else {
		err = publicationService.likeRepository.Insert(model.Like{
			IDProfile: publication.IDProfile,
			IDUser:    idUser,
			IDPost:    idPost,
		})
		if err != nil {
			return false, err
		}
		if err := publicationService.publicationRepository.UpdateOne(
			&publication_repository.Criteria{
				ID: publication.ID,
			},
			publication_repository.UpdateData{
				SumLikes: 1,
			},
		); err != nil {
			return false, err
		}
		err = publicationService.interactionEvent(publication.ID)
		if err != nil {
			return false, err
		}
		return true, nil
	}
}

func (publicationService *PublicationService) predictIfPublicationImagesAreTattoos(
	publication *model.Publication,
) {
	imageTattoos, err := utils.ConcurrentFilter(publication.Images, func(image fileModel.Image) (bool, error) {
		imageUrl, err := publicationService.imageStore.GetURL(image.Key)
		if err != nil {
			return false, err
		}
		prediction, _, err := openaiprovider.Predict[TattooPredict](
			llm.PredictSchema{
				Name:        "tattoo_schema",
				Description: "determine if is tattoo",
			},
			AssistanteMessageTattoo,
			llm.Message{
				ImageURL: imageUrl,
			},
		)
		if err != nil {
			return false, err
		}

		return prediction.IsTattoo, nil
	})
	if err != nil {
		return
	}
	idImages := utils.MapNoError(imageTattoos, func(image fileModel.Image) int64 {
		return image.ID
	})
	tattoos := utils.MapNoError(imageTattoos, func(image fileModel.Image) tattooModel.Tattoo {
		return tattooModel.Tattoo{
			Image:         image,
			Description:   publication.Content,
			IDPublication: publication.ID,
			Categories:    publication.Categories,
		}
	})

	newTattoos, err := publicationService.tattooRepository.ConvertImageInTattoo(
		idImages,
		tattoos,
		publication.IDProfile,
	)
	if err != nil {
		return
	}

	for _, newTattoo := range newTattoos {
		tattoo := newTattoo
		go publicationService.bus.Publish(bus.Event{
			Name:    NEW_TATTOO,
			Payload: utils.Payload(tattoo),
		})
	}

}

func (publicationService *PublicationService) throwAccessToPublish(
	idUser,
	idStudio int64,
	userRoles []authModel.Role,
) error {
	if idStudio == 0 && utils.Includes(userRoles, authModel.TATTOO_ARTIST_ROLE) {
		return ErrUnauthorizedPublishPublication
	}

	return publicationService.adminStudioService.ThrowAccessInStudio(
		idUser,
		idStudio,
		studioModel.PUBLISH_PERMISSION,
	)
}

func (publicationService *PublicationService) Publish(
	publicationDto *dto.PublicationDto,
	idUser int64,
	userRoles []authModel.Role,
) (*model.Publication, error) {
	if err := publicationService.throwAccessToPublish(
		idUser,
		publicationDto.IDStudio,
		userRoles,
	); err != nil {
		return nil, err
	}

	idProfile, err := publicationService.profileService.GetProfileIDFromIDUser(
		idUser,
	)
	if err != nil {
		return nil, err
	}
	publication, imagesDto := publicationDto.ToModel()
	if len(imagesDto) > 5 {
		return nil, ErrTooManyImages
	}

	images, err := publicationService.fileService.UploadImages(imagesDto, fmt.Sprintf("publications/%d", idUser))
	if err != nil {
		return nil, err
	}
	publication.Images = images

	categories := utils.ExtractWords[string](publication.Content, "#")
	dirtyMentions := utils.ExtractWords[string](publication.Content, "@")

	publication.Categories = categories

	mentions, err := utils.Map(dirtyMentions, func(mention string) (id int64, err error) {
		userBool, err := publicationService.userRepository.Exists(&user_repository.Criteria{
			Username: repository.CriteriaString{
				EQ: utils.String(mention),
			},
		})
		if err != nil {
			return 0, err
		}
		if !userBool {
			return 0, err
		}
		idProfileUser, err := publicationService.profileService.GetProfileIdFromUsername(mention)
		if err != nil {
			return 0, err
		}
		return idProfileUser, nil
	})

	if err != nil {
		return nil, err
	}
	// Elimina los 0
	publication.Mentions = utils.FilterNoError(mentions, func(x int64) bool {
		return x != 0
	})
	insertedPublication, err := publicationService.publicationRepository.Insert(*publication, idProfile)
	if err != nil {
		return nil, err
	}
	if _, err := publicationService.tattooService.PublishTattoos(
		publicationDto.ToTattoos(insertedPublication.ID),
		idUser,
	); err != nil {
		return nil, err
	}

	publication, err = publicationService.GetPublication(insertedPublication.ID)
	if err != nil {
		return nil, err
	}
	go publicationService.bus.Publish(bus.Event{
		Name:    NEW_PUBLICATION,
		Payload: utils.Payload(publication),
	})
	go publicationService.predictIfPublicationImagesAreTattoos(
		publication,
	)

	return publication, nil
}

func (publicationService *PublicationService) DeletePublication(
	idPublication int64,
	idUser int64,
) error {
	opts := publication_repository.NewFindOneOptions().
		Include(publication_repository.Include{
			ProfileUser: true,
			Tattoos:     true,
			Profile:     true,
		})

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPublication,
		},
		opts,
	)
	if err != nil {
		return err
	}
	if publication == nil {
		return ErrPublicationNotExists
	}
	if publication.Profile.User.ID != idUser && publication.IDStudio == 0 {
		return ErrPublicationNotAccess
	} else if publication.IDStudio != 0 && publication.Profile.User.ID != idUser {
		if err := publicationService.adminStudioService.ThrowAccessInStudio(
			idUser,
			publication.IDStudio,
			studioModel.EDIT_PUBLICATIONS_PERMISSION,
		); err != nil {
			return ErrPublicationNotAccess
		}
	} else {
		if err := publicationService.adminStudioService.ThrowAccessInStudio(
			idUser,
			publication.IDStudio,
		); err != nil {
			return ErrPublicationNotAccess
		}
	}
	// Update
	images, err := publicationService.publicationRepository.FindImages(
		idPublication,
	)
	if err != nil {
		return err
	}
	idTattoos := utils.MapNoError(publication.Tattoos, func(tattoo tattooModel.Tattoo) int64 {
		return tattoo.ID
	})
	if err := publicationService.tattooRepository.Update(
		&tattoo_repository.Criteria{
			IDs: idTattoos,
		},
		tattoo_repository.UpdateData{
			UnsetIDPublication: true,
		},
	); err != nil {
		return err
	}
	// Delete images
	for _, image := range images {
		if err := publicationService.imageStore.Delete(image.Key); err != nil {
			return err
		}
	}
	err = publicationService.publicationRepository.Delete(
		&publication_repository.Criteria{
			ID: idPublication,
		},
	)
	if err != nil {
		return err
	}
	go publicationService.bus.Publish(bus.Event{
		Name:    DELETE_PUBLICATION,
		Payload: utils.Payload(publication),
	})
	return nil
}

func (publicationService *PublicationService) AddView(idPublication int64, identifier string) error {

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPublication,
		},
		nil,
	)
	if err != nil {
		return err
	}
	if publication == nil {
		return ErrPublicationNotExists
	}

	err = publicationService.publicationRepository.UpdateOne(&publication_repository.Criteria{
		ID: idPublication,
	}, publication_repository.UpdateData{
		SumViews: 1,
	})
	if err != nil {
		return err
	}
	err = publicationService.interactionEvent(idPublication)
	if err != nil {
		return err
	}

	data := model.TemporalViewPublication{
		IDPublication: idPublication,
		Identifier:    identifier,
	}

	go publicationService.bus.Publish(bus.Event{
		Name:    ADD_TEMPORAL_VIEW,
		Payload: utils.Payload(data),
	})
	return nil
}

func (publicationService *PublicationService) interactionEvent(idPost int64) error {

	publication, err := publicationService.GetPublication(idPost)
	if err != nil {
		return err
	}
	go publicationService.bus.Publish(bus.Event{
		Name:    PUBLICATION_INTERACTION,
		Payload: utils.Payload(publication),
	})

	return nil
}

func (publicationService *PublicationService) UpdateRatings() {
	go publicationService.bus.Publish(bus.Event{
		Name: PUBLICATION_UPDATE_RATING,
	})
}

func NewPublicationService(
	tattooService service.TattooService,
	profileService userService.ProfileService,
	imageStore store.ImageStore,
	publicationRepository publication_repository.PublicationRepository,
	likeRepository like_repository.LikeRepository,
	tattooRepository tattoo_repository.TattooRepository,
	userRepository user_repository.UserRepository,
	fileService file_service.FileService,
	adminStudioService studioService.AdminStudioService,
	bus bus.Bus,
) *PublicationService {
	if publicationService == nil {
		publicationService = &PublicationService{
			tattooService:         tattooService,
			profileService:        profileService,
			imageStore:            imageStore,
			publicationRepository: publicationRepository,
			likeRepository:        likeRepository,
			tattooRepository:      tattooRepository,
			userRepository:        userRepository,
			bus:                   bus,
			fileService:           fileService,
			adminStudioService:    adminStudioService,
		}
	}

	return publicationService
}

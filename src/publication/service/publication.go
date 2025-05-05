package service

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/like_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var publicationService *PublicationService

type PublicationService struct {
	tattooService         service.TattooService
	categoryService       service.CategoryService
	profileService        userService.ProfileService
	imageStore            store.ImageStore
	publicationRepository publication_repository.PublicationRepository
	likeRepository        like_repository.LikeRepository
	tattooRepository      tattoo_repository.TattooRepository
	fileService           file_service.FileService
	bus                   bus.Bus
}

type PublicationsMetadata struct {
	Limit int
	Total int
}

func (publicationService *PublicationService) GetPublications(
	username string,
	page int,
) ([]model.Publication, *PublicationsMetadata, error) {
	idProfile, err := publicationService.profileService.GetProfileIdFromUsername(
		username,
	)
	if err != nil {
		return nil, nil, err
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
		&publication_repository.Criteria{
			IDProfile: idProfile,
		},
		opts,
	)
	if err != nil {
		return nil, nil, err
	}
	// Count
	count, err := publicationService.publicationRepository.Count(
		&publication_repository.Criteria{
			IDProfile: idProfile,
		},
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

		return true, nil
	}
}

func (publicationService *PublicationService) Publish(
	publicationDto *dto.PublicationDto,
	idUser int64,
) (*model.Publication, error) {
	idProfile, err := publicationService.profileService.GetProfileIDFromIDUser(
		idUser,
	)
	if err != nil {
		return nil, err
	}
	if err := publicationService.categoryService.ExistsCategories(
		publicationDto.IDCategories,
	); err != nil {
		return nil, err
	}

	publication, imagesDto := publicationDto.ToModel()
	images, err := utils.ConcurrentMap(imagesDto, func(imageDto store.ImageDto) (fileModel.Image, error) {
		err := publicationService.fileService.CheckImageMimeType(imageDto)
		if err != nil {
			return fileModel.Image{}, err
		}
		image, err := publicationService.imageStore.Upload(imageDto, fmt.Sprintf("publications/%d", idUser))
		if err != nil {
			return fileModel.Image{}, err
		}

		return *image, nil
	}, nil)
	if err != nil {
		return nil, err
	}
	publication.Images = images
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

	return publication, nil
}

func (publicationService *PublicationService) DeletePublication(
	idPublication int64,
	idUser int64,
) error {
	opts := publication_repository.NewFindOneOptions().
		Select(publication_repository.SelectOpts{
			IDProfile: utils.Bool(true),
			User: &user_repository.SelectOpts{
				ID: utils.Bool(true),
			},
		}).
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
	if publication.Profile.User.ID != idUser {
		return ErrPublicationNotAccess
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

	return publicationService.publicationRepository.Delete(
		&publication_repository.Criteria{
			ID: idPublication,
		},
	)
}

func NewPublicationService(
	tattooService service.TattooService,
	categoryService service.CategoryService,
	profileService userService.ProfileService,
	imageStore store.ImageStore,
	publicationRepository publication_repository.PublicationRepository,
	likeRepository like_repository.LikeRepository,
	tattooRepository tattoo_repository.TattooRepository,
	busS bus.Bus,
) *PublicationService {
	if publicationService == nil {
		publicationService = &PublicationService{
			tattooService:         tattooService,
			categoryService:       categoryService,
			profileService:        profileService,
			imageStore:            imageStore,
			publicationRepository: publicationRepository,
			likeRepository:        likeRepository,
			tattooRepository:      tattooRepository,
			bus:                   busS,
		}
	}

	return publicationService
}

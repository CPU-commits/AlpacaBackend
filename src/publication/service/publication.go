package service

import (
	"fmt"
	"time"

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
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/share_repository"
	studioModel "github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	viewService "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
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
	viewService           viewService.ViewService
	shareRepository       share_repository.ShareRepository
}

type PublicationsMetadata struct {
	Limit int
	Total int
}

func (publicationService *PublicationService) GetPublicationMetrics(
	idPublication,
	idUser int64,
	fromDate time.Time,
	toDate time.Time,
) (*Metrics, error) {
	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPublication,
		},
		publication_repository.NewFindOneOptions().
			Select(publication_repository.SelectOpts{
				ID:        utils.Bool(true),
				IDStudio:  utils.Bool(true),
				IDProfile: utils.Bool(true),
			}),
	)
	if err != nil {
		return nil, err
	}
	if publication.IDStudio != 0 {
		if err := publicationService.adminStudioService.ThrowAccessInStudio(
			idUser,
			publication.IDStudio,
			studioModel.SHOW_METRICS_PERMISSION,
		); err != nil {
			return nil, err
		}
	} else {
		idProfile, err := publicationService.profileService.GetProfileIDFromIDUser(idUser)
		if err != nil {
			return nil, err
		}
		if idProfile != publication.IDProfile {
			return nil, ErrPublicationNotAccess
		}
	}

	var viewStats []viewService.StatView
	var viewCount int64
	var likesCount int64
	var likesStats []Stat
	var sharesCount int64
	var sharesStats []Stat

	err = utils.Concurrency(3, 3, func(index int, setError func(err error)) {
		var err error
		switch index {
		case 0:
			viewStats, viewCount, err = publicationService.viewService.StatsViews(
				viewService.ToView{
					IDPost: idPublication,
				},
				fromDate,
				toDate,
			)
		case 1:
			likesStatsC, err := publicationService.likeRepository.CountGroupByDay(
				&like_repository.Criteria{
					IDPost: idPublication,
					CreatedAt: &repository.CriteriaTime{
						GTE: fromDate,
						LTE: toDate,
					},
				},
			)
			if err != nil {
				setError(err)
				return
			}

			likesStats = utils.IterateDates(fromDate.Truncate(24*time.Hour), toDate.Truncate(24*time.Hour), func(d time.Time) Stat {
				like := utils.FindNoError(likesStatsC, func(like like_repository.CountGroupByDayResult) bool {
					return like.Day.Equal(d)
				})
				if like != nil {
					likesCount += like.Likes
					return Stat{
						Count: like.Likes,
						Day:   like.Day,
					}
				}

				return Stat{
					Day: d,
				}
			})
		case 2:
			sharesStatsC, err := publicationService.shareRepository.CountGroupByDay(
				&share_repository.Criteria{
					IDPost: idPublication,
					CreatedAt: &repository.CriteriaTime{
						GTE: fromDate,
						LTE: toDate,
					},
				},
			)
			if err != nil {
				setError(err)
				return
			}

			sharesStats = utils.IterateDates(fromDate.Truncate(24*time.Hour), toDate.Truncate(24*time.Hour), func(d time.Time) Stat {
				share := utils.FindNoError(sharesStatsC, func(share share_repository.CountGroupByDayResult) bool {
					return share.Day.Equal(d)
				})
				if share != nil {
					sharesCount += share.Shares
					return Stat{
						Count: share.Shares,
						Day:   share.Day,
					}
				}

				return Stat{
					Day: d,
				}
			})
		}
		if err != nil {
			setError(err)
			return
		}
	})
	if err != nil {
		return nil, err
	}

	metrics := &Metrics{
		Views: Metric{
			Count: viewCount,
			Stats: utils.MapNoError(viewStats, func(stat viewService.StatView) Stat {
				return Stat{
					Count: stat.Views,
					Day:   stat.Day,
				}
			}),
		},
		Likes: Metric{
			Count: likesCount,
			Stats: likesStats,
		},
		Shares: Metric{
			Count: sharesCount,
			Stats: sharesStats,
		},
	}

	return metrics, nil
}

func (publicationService *PublicationService) ListPublications(
	page int,
) ([]model.Publication, *PublicationsMetadata, error) {
	limit := 100
	opts := publication_repository.NewFindOptions().
		Limit(limit).
		Skip(page * limit).
		Include(publication_repository.Include{
			Images:       true,
			Tattoos:      true,
			TattoosImage: true,
		}).
		Select(publication_repository.SelectOpts{
			ID:        utils.Bool(true),
			CreatedAt: utils.Bool(true),
		})

	publications, err := publicationService.publicationRepository.Find(
		nil,
		opts,
	)
	if err != nil {
		return nil, nil, err
	}
	// Count
	count, err := publicationService.publicationRepository.Count(
		nil,
	)

	return publications, &PublicationsMetadata{
		Limit: limit,
		Total: int(count),
	}, nil
}

func (publicationService *PublicationService) GetPublications(
	params PublicationsParams,
	page int,
) ([]model.Publication, *PublicationsMetadata, error) {
	criteria := &publication_repository.Criteria{}
	if params.Q != "" {
		criteria.Content = repository.CriteriaString{
			IContains: &params.Q,
		}
	}
	if !params.FromDate.IsZero() {
		criteria.CreatedAt = &repository.CriteriaTime{
			GTE: params.FromDate,
		}
	}
	if !params.ToDate.IsZero() {
		if criteria.CreatedAt == nil {
			criteria.CreatedAt = &repository.CriteriaTime{}
		}

		criteria.CreatedAt.LTE = params.ToDate
	}

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
	color string,
	areas []tattooModel.TattooArea,
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
			TattooCriteria: &tattoo_repository.Criteria{
				Color: color,
				Areas: areas,
			},
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

func (publicationService *PublicationService) Share(
	idPost int64,
	idUser int64,
) error {
	opts := publication_repository.NewFindOneOptions().
		Select(publication_repository.SelectOpts{
			ID: utils.Bool(true),
		})

	publication, err := publicationService.publicationRepository.FindOne(
		&publication_repository.Criteria{
			ID: idPost,
		},
		opts,
	)
	if err != nil {
		return err
	}
	if publication == nil {
		return ErrPublicationNotExists
	}
	// Exists share
	existsShare, err := publicationService.shareRepository.Exists(
		&share_repository.Criteria{
			IDUser: idUser,
			IDPost: idPost,
		},
	)
	if err != nil {
		return err
	}
	if !existsShare {
		err = publicationService.shareRepository.Insert(model.Share{
			IDUser: idUser,
			IDPost: idPost,
		})
		if err != nil {
			return err
		}
		if err := publicationService.publicationRepository.UpdateOne(
			&publication_repository.Criteria{
				ID: publication.ID,
			},
			publication_repository.UpdateData{
				SumShares: 1,
			},
		); err != nil {
			return err
		}
		err = publicationService.interactionEvent(publication.ID)
		if err != nil {
			return err
		}
		return nil
	}

	return nil
}

func (publicationService *PublicationService) HandleLike(
	idPost int64,
	idUser int64,
) (bool, error) {
	opts := publication_repository.NewFindOneOptions().
		Select(publication_repository.SelectOpts{
			IDProfile: utils.Bool(true),
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
	tattoos, _, err := publicationService.tattooService.GetTattoos(service.GetTattoosParams{
		IDPublication: publication.ID,
	}, 0)
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
		if err := publicationService.tattooRepository.UpdateLikes(utils.MapNoError(tattoos, func(tattoo tattooModel.Tattoo) int64 {
			return tattoo.ID
		}), "reduce"); err != nil {
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
		if err := publicationService.tattooRepository.UpdateLikes(utils.MapNoError(tattoos, func(tattoo tattooModel.Tattoo) int64 {
			return tattoo.ID
		}), "increase"); err != nil {
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
	idStudio int64,
) {
	type toMap struct {
		image      fileModel.Image
		prediction *TattooPredict
	}

	imageTattoosPredictions, err := utils.ConcurrentMap(publication.Images, func(image fileModel.Image) (toMap, error) {
		imageUrl, err := publicationService.imageStore.GetURL(image.Key)
		if err != nil {
			return toMap{
				image: image,
			}, err
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
			return toMap{
				image: image,
			}, err
		}

		return toMap{
			image:      image,
			prediction: &prediction,
		}, nil
	}, nil)
	imageTattoos := utils.FilterNoError(imageTattoosPredictions, func(mapped toMap) bool {
		return mapped.prediction != nil && mapped.prediction.IsTattoo
	})

	if err != nil {
		return
	}
	idImages := utils.MapNoError(imageTattoos, func(mapped toMap) int64 {
		return mapped.image.ID
	})
	var idStudioPtr *int64
	if idStudio != 0 {
		idStudioPtr = &idStudio
	}

	tattoos := utils.MapNoError(imageTattoos, func(mapped toMap) tattooModel.Tattoo {
		return tattooModel.Tattoo{
			Image:         mapped.image,
			Description:   publication.Content,
			IDPublication: publication.ID,
			Categories:    publication.Categories,
			IDStudio:      idStudioPtr,
			Areas: utils.MapNoError(mapped.prediction.Area, func(area string) tattooModel.TattooArea {
				return tattooModel.TattooArea(area)
			}),
			LLMDescription: mapped.prediction.Description,
			Color:          mapped.prediction.Color,
			Mentions:       publication.Mentions,
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
		return nil
	} else if idStudio == 0 && !utils.Includes(userRoles, authModel.TATTOO_ARTIST_ROLE) {
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

	categories := utils.ExtractHashtags(publication.Content)
	dirtyMentions := utils.ExtractMentions(publication.Content)

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
		publicationDto.IDStudio,
	)

	return publication, nil
}

func (publicationService *PublicationService) DeletePublication(
	idPublication int64,
	idUser int64,
) error {
	opts := publication_repository.NewFindOneOptions().
		Include(publication_repository.Include{
			ProfileUser:  true,
			Tattoos:      true,
			TattoosImage: true,
			Profile:      true,
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
	} else if publication.IDStudio != 0 {
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
	err = utils.ConcurrentForEach(publication.Tattoos, func(tattoo tattooModel.Tattoo, setError func(err error)) {
		if err := publicationService.imageStore.Delete(tattoo.Image.Key); err != nil {
			setError(err)
			return
		}
		if err := publicationService.tattooRepository.Delete(&tattoo_repository.Criteria{
			ID: tattoo.ID,
		}); err != nil {
			setError(err)
			return
		}
	}, &utils.OptionsConcurrentForEach{
		MaxConcurrency: 5,
	})
	if err != nil {
		return err
	}
	// if err := publicationService.tattooRepository.Update(
	// 	&tattoo_repository.Criteria{
	// 		IDs: idTattoos,
	// 	},
	// 	tattoo_repository.UpdateData{
	// 		UnsetIDPublication: true,
	// 	},
	// ); err != nil {
	// 	return err
	// }

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

func (publicationService *PublicationService) AddView(idPublication int64, identifier dto.ViewIdentifier, ip string) error {

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

	if err := publicationService.viewService.AddPermanentView(
		0,
		viewService.ToView{
			IDPost: idPublication,
		},
		utils.String(ip),
	); err != nil {
		return err
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
		Identifier:    identifier.Identifier,
	}

	isViewExists, err := publicationService.TemporalViewExists(data)
	if err != nil {
		return err
	}
	if isViewExists {
		return ErrTemporalViewExists
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

func (publicationService *PublicationService) TemporalViewExists(data model.TemporalViewPublication) (bool, error) {
	var isTemporalView bool
	err := publicationService.bus.Request(bus.Event{
		Name:    VERIFY_TEMPORAL_VIEW,
		Payload: utils.Payload(data),
	}, &isTemporalView)
	return isTemporalView, err
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
	viewService viewService.ViewService,
	shareRepository share_repository.ShareRepository,
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
			viewService:           viewService,
			shareRepository:       shareRepository,
		}
	}

	return publicationService
}

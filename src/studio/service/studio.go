package service

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid"
	shorterModel "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	viewServices "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

type StudioService struct {
	studioRepository   studio_repository.StudioRepository
	adminStudioService AdminStudioService
	authService        service.AuthService
	imageStore         store.ImageStore
	fileService        fileService.FileService
	uidGenerator       uid.UIDGenerator
	viewService        viewServices.ViewService
	followService      userService.FollowService
}

var studioService *StudioService

func (*StudioService) toShortLinksMedia(media []shorterModel.Media) []shorterModel.Media {
	return utils.MapNoError(media, func(media shorterModel.Media) shorterModel.Media {
		return shorterModel.Media{
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
	identifier,
	ip string,
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
	go studioService.viewService.AddPermanentViewIfTemporalViewNotExists(
		identifier,
		viewServices.ToView{
			IDStudio: idStudio,
		},
		utils.String(ip),
	)

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

func (studioService *StudioService) GetStudioMetrics(
	idStudio,
	idUser int64,
	params MetricsParams,
) (*Metrics, error) {
	if err := studioService.adminStudioService.ThrowAccessInStudio(
		idUser,
		idStudio,
		model.SHOW_METRICS_PERMISSION,
	); err != nil {
		return nil, err
	}

	statsLocation, countLocation, err := studioService.viewService.StatsViewsByLocation(
		viewServices.ToView{
			IDStudio: idStudio,
		},
		params.From,
		params.To,
		utils.GetTagsGeo(utils.SANTIAGO)...,
	)
	if err != nil {
		return nil, err
	}
	stats, countViews, err := studioService.viewService.StatsViews(
		viewServices.ToView{
			IDStudio: idStudio,
		},
		params.From,
		params.To,
	)
	if err != nil {
		return nil, err
	}
	statsComparative, countViewsComparative, err := studioService.viewService.StatsViews(
		viewServices.ToView{
			IDStudio: idStudio,
		},
		params.FromComparative,
		params.ToComparative,
	)
	if err != nil {
		return nil, err
	}
	statsFollows, countFollows, err := studioService.followService.StatsFollows(
		0,
		idStudio,
		params.From,
		params.To,
	)
	if err != nil {
		return nil, err
	}
	statsFollowsComparative, countFollowsComparative, err := studioService.followService.StatsFollows(
		0,
		idStudio,
		params.FromComparative,
		params.ToComparative,
	)
	if err != nil {
		return nil, err
	}

	var metrics []LocationMetric
	for k, v := range statsLocation {
		metrics = append(metrics, LocationMetric{
			Location: k,
			Value:    v,
		})
	}

	return &Metrics{
		Views: MetricsViews{
			ByLocation: MetricsLocation{
				Locations: metrics,
				Count:     countLocation,
			},
			Timeline: MetricsTimeline{
				Stats: stats,
				Count: countViews,
			},
			TimelineComparative: MetricsTimeline{
				Stats: statsComparative,
				Count: countViewsComparative,
			},
		},
		Follows: MetricsFollows{
			Timeline: MetricsFollowsTimeline{
				Stats: statsFollows,
				Count: countFollows,
			},
			TimelineComparative: MetricsFollowsTimeline{
				Stats: statsFollowsComparative,
				Count: countFollowsComparative,
			},
		},
	}, nil
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

func (studioService *StudioService) SearchStudios(
	q string,
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
		}).
		Limit(5)

	return studioService.studioRepository.Find(
		&studio_repository.Criteria{
			IsActive: utils.Bool(true),
			OR: []studio_repository.Criteria{
				{
					Name: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
				{
					Email: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
				{
					Username: repository.CriteriaString{
						IContains: utils.String(q),
					},
				},
			},
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
			{Email: repository.CriteriaString{
				EQ: utils.String(email),
			}},
			{Username: repository.CriteriaString{
				EQ: utils.String(username),
			}},
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
			RemoveMedia: studio_repository.RemoveMedia{
				IDs:      studio.RemoveMedia,
				IDStudio: idStudio,
			},
			AddMedia: media,
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
	viewService viewServices.ViewService,
	followService userService.FollowService,
) *StudioService {
	if studioService == nil {
		studioService = &StudioService{
			studioRepository:   studioRepository,
			authService:        authService,
			fileService:        fileService,
			imageStore:         imageStore,
			adminStudioService: adminStudioService,
			uidGenerator:       uidGenerator,
			viewService:        viewService,
			followService:      followService,
		}
	}

	return studioService
}

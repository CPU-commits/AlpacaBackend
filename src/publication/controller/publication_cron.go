package controller

import (
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
)

type CronPublicationController struct {
	bus                bus.Bus
	publicationService service.PublicationService
}

func NewCronPublication(
	bus bus.Bus,
) *CronPublicationController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	profileService := *userServices.NewProfileService(
		profileRepository,
		*userService,
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		userServices.SinglentonFollowService(),
	)

	adminStudioService := studioService.NewPeopleStudioService(
		adminStudioRepository,
		studioRepository,
		*userService,
		peopleHistoriesRepository,
	)

	return &CronPublicationController{
		bus: bus,
		publicationService: *service.NewPublicationService(
			*tattooService.NewTattooService(
				imageStore,
				profileService,
				tattooRepository,
				*fileService,
				embeddingapi.NewAPIEmbedding(),
				bus,
				*viewService,
			),
			profileService,
			imageStore,
			publicationRepository,
			likeRepository,
			tattooRepository,
			userRepository,
			*fileService,
			*adminStudioService,
			*viewService,
			shareRepository,
			bus,
		),
	}
}

func (cron *CronPublicationController) UpdateRatings() {
	cron.publicationService.UpdateRatings()
}

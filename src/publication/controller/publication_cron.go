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

type cronPublicationController struct {
	bus                bus.Bus
	publicationService service.PublicationService
}

func NewCronPublication(
	bus bus.Bus,
) *cronPublicationController {
	profileService := *userServices.NewProfileService(
		profileRepository,
		*authService.NewUserService(
			userRepository,
			roleRepository,
			uidGenerator,
			bus,
		),
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		userServices.SinglentonFollowService(),
	)
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	adminStudioService := studioService.NewPeopleStudioService(
		adminStudioRepository,
		studioRepository,
		*userService,
	)

	return &cronPublicationController{
		bus: bus,
		publicationService: *service.NewPublicationService(
			*tattooService.NewTattooService(
				imageStore,
				profileService,
				tattooRepository,
				*fileService,
				embeddingapi.NewAPIEmbedding(),
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

func (cron *cronPublicationController) UpdateRatings() {
	cron.publicationService.UpdateRatings()
}

package controller

import (
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
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
			bus,
		),
		imageStore,
		*fileService,
		&followRepository,
		publicationRDRepository,
	)
	return &cronPublicationController{
		bus: bus,
		publicationService: *service.NewPublicationService(
			*tattooService.NewTattooService(
				imageStore,
				profileService,
				tattooRepository,
				*fileService,
			),
			profileService,
			imageStore,
			publicationRepository,
			likeRepository,
			tattooRepository,
			userRepository,
			*fileService,
			bus,
		),
	}
}

func (cron *cronPublicationController) UpdateRatings() {
	cron.publicationService.UpdateRatings()
}

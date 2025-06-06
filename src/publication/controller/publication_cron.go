package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
)

type cronPublicationController struct {
	bus                bus.Bus
	publicationService service.PublicationService
}

func NewCronPublication(
	bus bus.Bus,
) *cronPublicationController {

	return &cronPublicationController{
		bus: bus,
		publicationService: *service.NewPublicationService(
			*tattooService.NewTattooService(
				imageStore,
				*profileService,
				tattooRepository,
				*fileService,
			),
			*profileService,
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

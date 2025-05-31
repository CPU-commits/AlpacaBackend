package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
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
			*tattooService,
			*profileService,
			imageStore,
			publicationRepository,
			likeRepository,
			tattooRepository,
			userRepository,
			bus,
		),
	}
}

func (cron *cronPublicationController) UpdateRatings() {
	cron.publicationService.UpdateRatings()
}

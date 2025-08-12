package cron

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
)

var publicationCron *PublicationCron

type PublicationCron struct {
	bus                bus.Bus
	publicationService service.PublicationService
}

func NewCronPublication(
	bus bus.Bus,
	publicationService service.PublicationService,

) *PublicationCron {
	if publicationCron == nil {
		publicationCron = &PublicationCron{
			bus:                bus,
			publicationService: publicationService,
		}
	}
	return publicationCron
}

func (cron *PublicationCron) UpdateRatings() {
	cron.publicationService.UpdateRatings()
}

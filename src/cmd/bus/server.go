package bus

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	tattooCon "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/controller"
)

func Init(logger logger.Logger) {
	queueBus := queue.New(logger)
	// Controllers
	publicationController := controller.NewPublicationQueueController()
	tattooController := tattooCon.NewTattooQueueController()

	queueBus.Subscribe(
		NEW_PUBLICATION,
		publicationController.IndexPublication,
	)
	queueBus.Subscribe(
		PUBLICATION_INTERACTION,
		publicationController.InteractionEvent,
	)
	queueBus.Subscribe(
		PUBLICATION_UPDATE_RATING,
		publicationController.UpdateRatings,
	)
	queueBus.Subscribe(
		ADD_TEMPORAL_VIEW,
		publicationController.AddTemporalView,
	)
	queueBus.Subscribe(
		DELETE_PUBLICATION,
		publicationController.DeletePublication,
	)
	queueBus.Subscribe(
		NEW_TATTOO,
		tattooController.IndexTattoo,
	)
}

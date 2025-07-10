package bus

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	generatorCon "github.com/CPU-commits/Template_Go-EventDriven/src/generator/controller"
	notificationsCon "github.com/CPU-commits/Template_Go-EventDriven/src/notifications/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	tattooCon "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/controller"
)

func Init(logger logger.Logger) {
	queueBus := queue.New(logger)
	// Controllers
	publicationController := controller.NewPublicationQueueController(queueBus)
	tattooController := tattooCon.NewTattooQueueController()
	tokenController := generatorCon.NewTokenQueueController()
	emailController := notificationsCon.NewEmailQueueController(queueBus)

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
	queueBus.Subscribe(
		NEW_TOKEN_EMAIL_UPDATE,
		tokenController.AddTokenEmail,
	)

	queueBus.Subscribe(
		NEW_TOKEN_PASSWORD_UPDATE,
		tokenController.AddTokenPassword,
	)
	queueBus.SubscribeAndRespond(
		GET_TOKEN_EMAIL_UPDATE,
		tokenController.GetTokenEmail,
	)

	queueBus.SubscribeAndRespond(
		GET_TOKEN_PASSWORD_UPDATE,
		tokenController.GetTokenPassword,
	)
	queueBus.Subscribe(
		UPDATE_TOKEN_STATUS,
		tokenController.UpdateTokenStatus,
	)
	queueBus.Subscribe(
		SEND_EMAIL_PASSWORD_RESET,
		emailController.SendPasswordResetCodeBus,
	)
	queueBus.Subscribe(
		SEND_EMAIL_RESET,
		emailController.SendEmailResetCode,
	)

}

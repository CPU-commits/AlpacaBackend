package bus

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
)

func Init(logger logger.Logger) {
	queueBus := queue.New(logger)

	// Init controllers
	dogController := controller.NewQueueDogController(queueBus)
	// Init subscribers
	queueBus.Subscribe(
		INSERT_DOG,
		dogController.NotifyDogIsInserted,
	)
}

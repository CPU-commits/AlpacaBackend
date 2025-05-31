package cron

import (
	"log"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	"github.com/robfig/cron/v3"
)

func Init(logger logger.Logger) {
	c := cron.New(cron.WithSeconds())
	bus := queue.New(logger)

	cronPublication := controller.NewCronPublication(bus)

	_, err := c.AddFunc("@every 30m", cronPublication.UpdateRatings)
	if err != nil {
		log.Fatalf("Failed to schedule cronjob: %v", err)
	}
	c.Start()

	log.Println("Cronjob started.")

	select {}
}

package cron

import (
	"log"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"

	"github.com/go-redsync/redsync/v4"
	goredis "github.com/go-redsync/redsync/v4/redis/goredis/v9"
	"github.com/robfig/cron/v3"
)

func Init(logger logger.Logger) {
	c := cron.New(cron.WithSeconds())
	bus := queue.New(logger)

	cronPublication := controller.NewCronPublication(bus)

	pool := goredis.NewPool(db.RClient)
	rs := redsync.New(pool)
	_, err := c.AddFunc("@every 30m", func() {
		mutex := rs.NewMutex("cron-update-ratings",
			redsync.WithExpiry(40*time.Minute),
			redsync.WithTries(1),
		)

		if err := mutex.Lock(); err != nil {
			log.Fatalf("Failed to schedule cronjob: %v", err)
		}

		cronPublication.UpdateRatings()

		if ok, err := mutex.Unlock(); !ok || err != nil {
			log.Fatalf("Failed to schedule cronjob: %v", err)

		} else {
			log.Fatalf("success cron-update-ratings")

		}
	})
	if err != nil {
		log.Fatalf("Failed to schedule cronjob: %v", err)
	}
	c.Start()

	log.Println("Cronjob started.")

	select {}
}

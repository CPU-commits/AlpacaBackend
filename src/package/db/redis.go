package db

import (
	"github.com/redis/go-redis/v9"
)

var RClient *redis.Client

func init() {
	RClient = redis.NewClient(&redis.Options{
		Addr:          settingsData.REDIS_CONNECTION,
		Password:      settingsData.REDIS_PASS,
		DB:            0,
		UnstableResp3: true,
	})

}

package temporal_view_repository

import (
	"context"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/redis/go-redis/v9"
)

type RdTemportalViewRepository struct {
	rd *redis.Client
}

func (redisTVR RdTemportalViewRepository) AddView(keyView string, identifier string) error {

	key := fmt.Sprintf("%s:temporal_view:%s", keyView, identifier)
	timeExpired := time.Hour * 4

	_, err := redisTVR.rd.Incr(context.Background(), key).Result()
	if err != nil {
		return err
	}
	if _, err := redisTVR.rd.Expire(context.Background(), key, timeExpired).Result(); err != nil {
		return err
	}

	return nil
}

func (redisTVR RdTemportalViewRepository) ExistsView(keyView string, identifier string) (bool, error) {
	key := fmt.Sprintf("%s:temporal_view:%s", keyView, identifier)

	result, err := redisTVR.rd.Exists(context.Background(), key).Result()
	if err != nil {
		return false, err
	}

	return result != 0, nil
}

func NewRdTemportalViewRepository() TemporalViewRepository {
	return RdTemportalViewRepository{
		rd: db.RClient,
	}
}

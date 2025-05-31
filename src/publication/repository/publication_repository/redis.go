package publication_repository

import (
	"context"
	"encoding/json"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/redis/go-redis/v9"
)

type rdPublicationRepository struct {
	rd *redis.Client
}

func NewRdPublicationRepository() rdPublicationRepository {
	return rdPublicationRepository{
		rd: db.RClient,
	}
}

type RedisPublication struct {
	IDPublication int64     `json:"id_publication"`
	IDProfile     int64     `json:"id_profile"`
	Likes         int       `json:"likes"`
	Views         int       `json:"views"`
	Shares        int64     `json:"shares"`
	CreatedAt     time.Time `json:"created_at"`
}

func (rdPublicationRepository *rdPublicationRepository) Delete(redisPublication *RedisPublication) error {

	_, err := rdPublicationRepository.rd.JSONDel(context.Background(), fmt.Sprintf("publication:%d", redisPublication.IDPublication), "$").Result()
	if err != nil {
		return err
	}

	return nil
}

func (rdPublicationRepository *rdPublicationRepository) GetAllPublications() ([]RedisPublication, error) {
	var cursor uint64
	var keys []string

	for {
		var scanKeys []string
		var err error
		scanKeys, cursor, err = rdPublicationRepository.rd.Scan(context.Background(), cursor, "publication:*", 100).Result()
		if err != nil {
			return nil, fmt.Errorf("error en SCAN: %w", err)
		}

		keys = append(keys, scanKeys...)

		if cursor == 0 {
			break
		}
	}
	if len(keys) == 0 {
		return nil, nil
	}

	values, err := rdPublicationRepository.rd.JSONMGet(context.Background(), "$", keys...).Result()
	if err != nil {
		return nil, fmt.Errorf("error en JSONMGET: %w", err)
	}

	var publications []RedisPublication
	for _, val := range values {
		if val == nil {
			continue
		}

		var pub []RedisPublication
		if err := json.Unmarshal([]byte(val.(string)), &pub); err != nil {
			fmt.Println("Error parseando JSON:", err)
			continue
		}
		publications = append(publications, pub...)
	}

	return publications, nil
}

func (rdPublicationRepository *rdPublicationRepository) AddInteraction(publication *model.Publication) error {
	post := RedisPublication{
		IDPublication: publication.ID,
		IDProfile:     publication.IDProfile,
		Likes:         publication.Likes,
		Views:         publication.Views,
		Shares:        0,
		CreatedAt:     publication.CreatedAt,
	}
	_, err := rdPublicationRepository.rd.JSONSet(context.Background(), fmt.Sprintf("publication:%d", publication.ID), "$", post).Result()
	if err != nil {
		return err
	}

	return nil
}

func init() {
	client := db.RClient

	_, err := client.FTInfo(context.Background(), "idx:publications").Result()

	if err != nil {
		_, err := client.FTCreate(
			context.Background(),
			"idx:publications",
			&redis.FTCreateOptions{
				OnJSON: true,
				Prefix: []interface{}{"publication:"},
			},
			&redis.FieldSchema{
				FieldName: "$.id_publication",
				As:        "id_publication",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.id_profile",
				As:        "id_profile",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.likes",
				As:        "likes",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.views",
				As:        "views",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.shares",
				As:        "shares",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.created_at",
				As:        "created_at",
				FieldType: redis.SearchFieldTypeText,
			},
		).Result()
		if err != nil {
			panic(err)
		}
	}

}

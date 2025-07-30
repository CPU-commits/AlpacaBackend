package publication_repository

import (
	"context"
	"encoding/json"
	"fmt"
	"strconv"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/redis/go-redis/v9"
)

type RdPublicationRepository struct {
	rd *redis.Client
}

func NewRdPublicationRepository() RedisPublicationRepository {
	return &RdPublicationRepository{
		rd: db.RClient,
	}
}

func (rdPublicationRepository *RdPublicationRepository) AddView(idPost int64, identifier string) error {

	key := fmt.Sprintf("user_view:%s", identifier)
	timeExpired := time.Hour * 4

	_, err := rdPublicationRepository.rd.SAdd(context.Background(), key, idPost).Result()
	if err != nil {
		panic(err)
	}

	_, err = rdPublicationRepository.rd.Expire(context.Background(), key, timeExpired).Result()
	if err != nil {
		panic(err)
	}

	return nil
}

// UserViews = Los post visto por el usuario - Son temporales
func (rdPublicationRepository *RdPublicationRepository) GetAllUserView(identifier string) ([]int64, error) {
	members, err := rdPublicationRepository.rd.SMembers(context.TODO(), fmt.Sprintf("user_view:%s", identifier)).Result()
	if err != nil {
		return []int64{}, nil
	}
	var userViews []int64
	for _, member := range members {
		id, err := strconv.ParseInt(member, 10, 64)
		if err != nil {
			fmt.Println("Error convirtiendo a int64:", err)
			continue
		}
		userViews = append(userViews, id)
	}

	return userViews, nil
}

func (rdPublicationRepository *RdPublicationRepository) DeleteUserView(idUser int64) error {
	_, err := rdPublicationRepository.rd.Del(context.Background(), fmt.Sprintf("user_view:%d", idUser)).Result()
	if err != nil {
		return err
	}
	return nil
}
func (rdPublicationRepository *RdPublicationRepository) DeleteRedisPublications(redisPublication *model.RedisPublication) error {

	_, err := rdPublicationRepository.rd.JSONDel(context.Background(), fmt.Sprintf("publication:%d", redisPublication.IDPublication), "$").Result()
	if err != nil {
		return err
	}

	return nil
}
func (rdPublicationRepository *RdPublicationRepository) GetAllPublications() ([]model.RedisPublication, error) {
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

	var publications []model.RedisPublication
	for _, val := range values {
		if val == nil {
			continue
		}

		var pub []model.RedisPublication
		if err := json.Unmarshal([]byte(val.(string)), &pub); err != nil {
			fmt.Println("Error parseando JSON:", err)
			continue
		}
		publications = append(publications, pub...)
	}

	return publications, nil
}
func (rdPublicationRepository *RdPublicationRepository) AddInteraction(publication *model.Publication) error {
	post := model.RedisPublication{
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

package token_repository

import (
	"context"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/redis/go-redis/v9"
)

type RdTokenRepository struct {
	rd *redis.Client
}

func NewRdTokenRepository() RdTokenRepository {
	return RdTokenRepository{
		rd: db.RClient,
	}
}

func (rdTokenRepository *RdTokenRepository) AddToken(token *model.Token, tokenType string) error {

	tokenRedis := model.RedisToken{
		ID:        token.ID,
		IDUser:    token.IDUser,
		Token:     token.Token,
		ExpiresAt: token.ExpiresAt,
		CreatedAt: token.CreatedAt,
	}

	timeExpires := time.Minute * 5

	key := fmt.Sprintf("token:%s:update:%d", tokenType, token.IDUser)

	if _, err := rdTokenRepository.rd.JSONSet(context.Background(), key, "$", tokenRedis).Result(); err != nil {
		panic(err)
	}

	if _, err := rdTokenRepository.rd.Expire(context.Background(), key, timeExpires).Result(); err != nil {
		panic(err)
	}

	return nil
}

// Recibe el token de email
func (rdTokenRepository *RdTokenRepository) GetToken(token *model.Token, tokenType string) (string, error) {

	result, err := rdTokenRepository.rd.JSONGet(context.Background(), fmt.Sprintf("token:%s:update:%d", tokenType, token.IDUser), ".").Result()
	if err != nil {
		panic(err)
	}
	return result, nil
}

func init() {
	client := db.RClient

	_, err := client.FTInfo(context.Background(), "idx:token:email:update").Result()

	if err != nil {
		_, err := client.FTCreate(
			context.Background(),
			"idx:token:email:update",
			&redis.FTCreateOptions{
				OnJSON: true,
				Prefix: []interface{}{"token:email:update:"},
			},
			&redis.FieldSchema{
				FieldName: "$.id_token",
				As:        "id_token",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.token",
				As:        "token",
				FieldType: redis.SearchFieldTypeText,
			},
			&redis.FieldSchema{
				FieldName: "$.expires_at",
				As:        "expires_at",
				FieldType: redis.SearchFieldTypeText,
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
	_, err = client.FTInfo(context.Background(), "idx:token:password:update").Result()
	if err != nil {
		_, err := client.FTCreate(
			context.Background(),
			"idx:token:password:update",
			&redis.FTCreateOptions{
				OnJSON: true,
				Prefix: []interface{}{"token:password:update:"},
			},
			&redis.FieldSchema{
				FieldName: "$.id_token",
				As:        "id_token",
				FieldType: redis.SearchFieldTypeNumeric,
			},
			&redis.FieldSchema{
				FieldName: "$.token",
				As:        "token",
				FieldType: redis.SearchFieldTypeText,
			},
			&redis.FieldSchema{
				FieldName: "$.expires_at",
				As:        "expires_at",
				FieldType: redis.SearchFieldTypeText,
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

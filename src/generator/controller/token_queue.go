package controller

import (
	"encoding/json"
	"strconv"

	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/token_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

type QueueTokenController struct {
	tokenRepository token_repository.TokenRepository
}

func (*QueueTokenController) AddTokenEmail(c bus.Context) error {
	var token model.Token
	if err := c.BindData(&token); err != nil {
		return c.Kill(err.Error())
	}
	return tokenRDRepository.AddToken(&token, model.TokenType[0])
}

func (*QueueTokenController) AddTokenPassword(c bus.Context) error {
	var token model.Token
	if err := c.BindData(&token); err != nil {
		return c.Kill(err.Error())
	}
	return tokenRDRepository.AddToken(&token, model.TokenType[1])
}

func (*QueueTokenController) GetTokenEmail(c bus.Context) (*bus.BusResponse, error) {
	idUserStr := string(c.Data)
	idInt64, err := strconv.ParseInt(idUserStr, 10, 64)
	if err != nil {
		return nil, err
	}

	token, err := tokenRDRepository.GetToken(&model.Token{
		IDUser: int64(idInt64),
	}, model.TokenType[0])
	if err != nil {
		return nil, err
	}
	var redisToken model.RedisToken
	if token == "" {
		redisToken.Token = token
		return &bus.BusResponse{
			Success: true,
			Data:    redisToken,
		}, nil
	}

	if err := json.Unmarshal([]byte(token), &redisToken); err != nil {

		return nil, err
	}
	return &bus.BusResponse{
		Success: true,
		Data:    redisToken,
	}, nil
}

func (*QueueTokenController) GetTokenPassword(c bus.Context) (*bus.BusResponse, error) {
	idUserStr := string(c.Data)
	idInt64, err := strconv.ParseInt(idUserStr, 10, 64)
	if err != nil {
		return nil, err
	}

	token, err := tokenRDRepository.GetToken(&model.Token{
		IDUser: int64(idInt64),
	}, model.TokenType[1])
	if err != nil {
		return nil, err
	}
	var redisToken model.RedisToken
	if token == "" {
		redisToken.Token = token
		return &bus.BusResponse{
			Success: true,
			Data:    redisToken,
		}, nil
	}

	if err := json.Unmarshal([]byte(token), &redisToken); err != nil {

		return nil, err
	}
	return &bus.BusResponse{
		Success: true,
		Data:    redisToken,
	}, nil
}

func (queueController *QueueTokenController) UpdateTokenStatus(c bus.Context) error {
	var token model.RedisToken
	if err := c.BindData(&token); err != nil {
		return c.Kill(err.Error())
	}

	return queueController.tokenRepository.DeactiveToken(token)
}

func NewTokenQueueController() *QueueTokenController {
	return &QueueTokenController{
		tokenRepository: sqlTokenRepository,
	}
}

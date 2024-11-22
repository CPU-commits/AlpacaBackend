package utils

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/service"
)

type errRes struct {
	StatusCode  int
	MessageId   string
	TypeDetails string
}

var errorsService map[error]errRes = make(map[error]errRes)

func GetErrRes(err error) errRes {
	errResOk, exists := errorsService[err]
	if !exists {
		return errRes{
			StatusCode: http.StatusInternalServerError,
			MessageId:  "server.internal_error",
		}
	}

	return errResOk
}

func init() {
	errorsService[service.ErrDogNotExists] = errRes{
		StatusCode: http.StatusNotFound,
		MessageId:  "dog.not_found",
	}
	errorsService[service.ErrOwnerDogHasOneDog] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "dog.has_owner",
	}
}

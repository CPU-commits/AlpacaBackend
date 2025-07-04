package services

import "errors"

var (
	ErrCodeNotValid     = errors.New("err: code not valid")
	ErrCodeTypeNotValid = errors.New("err: code type not valid")
	ErrTokenNotValid    = errors.New("err: token not valid")
)

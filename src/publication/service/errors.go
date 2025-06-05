package service

import "errors"

var (
	ErrPublicationNotExists = errors.New("err: publication not exists")
	ErrPublicationNotAccess = errors.New("err: no access no publication")
	ErrTooManyImages        = errors.New("err: too many images")
)

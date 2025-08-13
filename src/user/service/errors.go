package service

import "errors"

var (
	ErrUserNoHasProfile    = errors.New("err: user no has profile")
	ErrInvalidMimeType     = errors.New("err: invalid mime type")
	ErrUnauthorizedProfile = errors.New("err: unauthorized profile")
	ErrProfileNotExists    = errors.New("err: profile not exists")
)

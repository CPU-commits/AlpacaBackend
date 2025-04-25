package service

import "errors"

var (
	ErrUserNoHasProfile = errors.New("err: user no has profile")
	ErrInvalidMimeType  = errors.New("err: invalid mime type")
)

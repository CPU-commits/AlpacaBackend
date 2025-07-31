package service

import "errors"

var (
	ErrNoLink         = errors.New("err: no link")
	ErrNoAccessToLink = errors.New("err: no access to link")
)

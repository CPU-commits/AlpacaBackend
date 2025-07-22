package service

import "errors"

var (
	ErrCategoriesNotExists = errors.New("err: categories not exists")
	ErrNotParams           = errors.New("err: not params, description or price")
)

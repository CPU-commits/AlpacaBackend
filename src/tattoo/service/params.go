package service

import "io"

type SearchByImageParams struct {
	Image          io.Reader
	IsLikeTattooID int64
}

type GetTattoosParams struct {
	IDPublication int64
	Username      string
	IDStudio      int64
}

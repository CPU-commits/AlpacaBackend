package service

import "io"

type SearchByImageParams struct {
	Image          io.Reader
	IsLikeTattooID int64
}

type GetTattoosParams struct {
	Username string
	IDStudio int64
}

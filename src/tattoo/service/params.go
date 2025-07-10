package service

import "io"

type SearchByImageParams struct {
	Image          io.Reader
	IsLikeTattooID int64
}

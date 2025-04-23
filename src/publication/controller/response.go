package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
)

type GetPublicationsResponse struct {
	Publications []model.Publication `json:"publications"`
}

type GetLikeResponse struct {
	Islike bool `json:"is_like"`
}

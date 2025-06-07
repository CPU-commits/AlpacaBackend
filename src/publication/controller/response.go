package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
)

type GetPublicationResponse struct {
	Publication model.Publication `json:"publication"`
}

type GetPublicationsResponse struct {
	Publications []model.Publication `json:"publications"`
}

type GetLikeResponse struct {
	Isliked bool `json:"isLiked"`
}

type PublicationDtoResponse struct {
	Content string `form:"content" binding:"required,max=500"`
	// Images       []PublicationImageDto `form:"images" binding:"dive"`
	IDCategories []int64 `form:"idCategories"`
}

package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Publication struct {
	ID           int64                  `json:"id"`
	Content      string                 `json:"content"`
	Likes        int                    `json:"likes"`
	IDProfile    int64                  `json:"idProfile"`
	Images       []model.Image          `json:"images,omitempty"`
	Tattoos      []tattooModel.Tattoo   `json:"tattoos,omitempty"`
	IDCategories []int64                `json:"idCategories,omitempty"`
	Categories   []tattooModel.Category `json:"categories,omitempty"`
	CreatedAt    time.Time              `json:"createdAt"`
	Profile      *userModel.Profile     `json:"profile,omitempty"`
}

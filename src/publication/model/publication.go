package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Publication struct {
	ID         int64                `json:"id"`
	Content    string               `json:"content"`
	Likes      int                  `json:"likes"`
	Views      int                  `json:"Views"`
	IDProfile  int64                `json:"idProfile"`
	Images     []model.Image        `json:"images,omitempty"`
	Tattoos    []tattooModel.Tattoo `json:"tattoos,omitempty"`
	Categories []string             `json:"categories,omitempty"`
	Mentions   []int64              `json:"mentions,omitempty"`
	CreatedAt  time.Time            `json:"createdAt"`
	Profile    *userModel.Profile   `json:"profile,omitempty"`
}

type TemporalViewPublication struct {
	IDPublication int64  `json:"id_publication"`
	Identifier    string `json:"identifier"`
}

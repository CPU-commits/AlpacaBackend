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
	Views      int                  `json:"views"`
	IDProfile  int64                `json:"idProfile"`
	Images     []model.Image        `json:"images,omitempty"`
	Tattoos    []tattooModel.Tattoo `json:"tattoos,omitempty"`
	Categories []string             `json:"categories,omitempty"`
	Shares     int64                `json:"shares,omitempty"`
	Mentions   []int64              `json:"mentions,omitempty"`
	CreatedAt  time.Time            `json:"createdAt"`
	Profile    *userModel.Profile   `json:"profile,omitempty"`
	IDStudio   int64                `json:"idStudio,omitempty"`
}

type TemporalViewPublication struct {
	IDPublication int64  `json:"id_publication"`
	Identifier    string `json:"identifier"`
}
type RedisPublication struct {
	IDPublication int64     `json:"id_publication"`
	IDProfile     int64     `json:"id_profile"`
	Likes         int       `json:"likes"`
	Views         int       `json:"views"`
	Shares        int64     `json:"shares"`
	CreatedAt     time.Time `json:"created_at"`
}

type TSPublication struct {
	ID         string   `json:"id"`
	IDProfile  int64    `json:"id_profile"`
	Content    string   `json:"content"`
	Likes      int32    `json:"likes"`
	Views      int32    `json:"views"`
	Categories []string `json:"categories"`
	Mentions   []int64  `json:"mentions"`
	CreatedAt  int64    `json:"created_at"`
	Rating     float64  `json:"rating"`
	Image1     string   `json:"image_1,omitempty"`
	Image2     string   `json:"image_2,omitempty"`
	Image3     string   `json:"image_3,omitempty"`
	Image4     string   `json:"image_4,omitempty"`
	Image5     string   `json:"image_5,omitempty"`
}

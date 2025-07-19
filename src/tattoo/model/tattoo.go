package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Coord struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
}

type Tattoo struct {
	ID            int64              `json:"id"`
	Likes         int                `json:"likes"`
	Image         model.Image        `json:"image"`
	Coord         *Coord             `json:"coord,omitempty"`
	Description   string             `json:"description,omitempty"`
	IDPublication int64              `json:"idPublication,omitempty"`
	Categories    []string           `json:"categories,omitempty"`
	CreatedAt     time.Time          `json:"createdAt"`
	Views         int                `json:"views"`
	Profile       *userModel.Profile `json:"profile,omitempty"`
	IDStudio      *int64             `json:"idStudio,omitempty"`
}

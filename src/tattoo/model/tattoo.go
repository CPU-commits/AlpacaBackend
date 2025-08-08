package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

// Area
type TattooArea string

const (
	ARM        TattooArea = "arm"
	LEG        TattooArea = "leg"
	BACK       TattooArea = "back"
	CHEST      TattooArea = "chest"
	ABDOMEN    TattooArea = "abdomen"
	NECK       TattooArea = "neck"
	HEAD       TattooArea = "head"
	HAND       TattooArea = "hand"
	FOOT       TattooArea = "foot"
	HIP        TattooArea = "hip"
	OTHER_AREA TattooArea = "other"
)

type Coord struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
}

type Tattoo struct {
	ID             int64              `json:"id"`
	Likes          int                `json:"likes"`
	Areas          []TattooArea       `json:"-"`
	Image          model.Image        `json:"image"`
	Coord          *Coord             `json:"coord,omitempty"`
	Description    string             `json:"description,omitempty"`
	LLMDescription string             `json:"llmDescription,omitempty"`
	Color          string             `json:"color,omitempty"`
	Mentions       []int64            `json:"mentions,omitempty"`
	IDPublication  int64              `json:"idPublication,omitempty"`
	Categories     []string           `json:"categories,omitempty"`
	CreatedAt      time.Time          `json:"createdAt"`
	Views          int                `json:"views"`
	Profile        *userModel.Profile `json:"profile,omitempty"`
	IDStudio       *int64             `json:"idStudio,omitempty"`
}

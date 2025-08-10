package model

import (
	"encoding/json"
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
	Areas          []TattooArea       `json:"areas"`
	Image          model.Image        `json:"image"`
	Coord          *Coord             `json:"coord,omitempty"`
	Description    string             `json:"description,omitempty"`
	LLMDescription string             `json:"llmDescription,omitempty"`
	Color          string             `json:"color,omitempty"`
	Mentions       []int64            `json:"mentions,omitempty"`
	IDPublication  int64              `json:"idPublication,omitempty"`
	Categories     []string           `json:"categories,omitempty"`
	Views          int                `json:"views"`
	Profile        *userModel.Profile `json:"profile,omitempty"`
	IDStudio       *int64             `json:"idStudio,omitempty"`
	CreatedAt      time.Time          `json:"createdAt"`
}

type TSTattoo struct {
	ID                     string   `json:"id"`
	Likes                  int32    `json:"likes"`
	IDImage                int64    `json:"id_image"`
	IDProfile              int64    `json:"id_profile"`
	Areas                  []string `json:"areas"`
	Image                  string   `json:"image,omitempty"`
	Description            string   `json:"description"`
	PublicationDescription string   `json:"publication_description"`
	Color                  string   `json:"color"`
	IDPublication          string   `json:"id_publication"`
	Categories             []string `json:"categories,omitempty"`
	Views                  int32    `json:"views"`
	Mentions               []int64  `json:"mentions"`
	CreatedAt              int64    `json:"created_at"`
	Rating                 float64  `json:"rating"`
}

func (t *Tattoo) AreasToStringSlice() []string {
	result := make([]string, len(t.Areas))
	for i, area := range t.Areas {
		s := string(area)
		if len(s) > 0 && s[0] == '[' {
			var inner []string
			if err := json.Unmarshal([]byte(s), &inner); err == nil && len(inner) > 0 {
				s = inner[0]
			}
		}
		result[i] = s
	}
	return result
}

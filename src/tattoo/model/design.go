package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Design struct {
	ID          int64       `json:"id"`
	IDProfile   int64       `json:"idProfile"`
	Image       model.Image `json:"image"`
	Coord       *Coord      `json:"coord,omitempty"`
	Description string      `json:"description,omitempty"`
	Price       int64       `json:"price" binding:"omitempty"`
	Categories  []string    `json:"categories,omitempty"`
	IsDeleted   bool        `json:"isDeleted"`
	CreatedAt   time.Time   `json:"createdAt"`

	Profile *userModel.Profile `json:"profile,omitempty"`
}

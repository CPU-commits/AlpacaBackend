package model

import (
	"time"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
)

type Profile struct {
	ID          int64           `json:"id"`
	Description string          `json:"description"`
	IDUser      int64           `json:"idUser"`
	Likes       int             `json:"likes"`
	CreatedAt   time.Time       `json:"createdAt"`
	Avatar      *model.Image    `json:"avatar,omitempty"`
	User        *authModel.User `json:"user,omitempty"`
}

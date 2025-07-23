package model

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
)

type Role string

// Roles
const (
	USER_ROLE          Role = "user"
	TATTOO_ARTIST_ROLE Role = "tattooArtist"
	ADMIN_ROLE         Role = "admin"
)

type User struct {
	ID        int64         `json:"id,omitempty"`
	Location  string        `json:"location,omitempty"`
	Email     string        `json:"email,omitempty"`
	Name      string        `json:"name"`
	Phone     string        `json:"phone,omitempty"`
	Username  string        `json:"username"`
	Roles     []Role        `json:"roles,omitempty"`
	Media     []model.Media `json:"media,omitempty"`
	CreatedAt time.Time     `json:"created_at,omitempty"`
}

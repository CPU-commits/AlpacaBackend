package model

import (
	"time"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	shorterModel "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
)

type Studio struct {
	ID          int64                `json:"id"`
	Name        string               `json:"name"`
	IDAvatar    int64                `json:"-"`
	IDBanner    int64                `json:"-"`
	Username    string               `json:"username"`
	Avatar      *model.Image         `json:"avatar,omitempty"`
	Banner      *model.Image         `json:"banner,omitempty"`
	Description string               `json:"description,omitempty"`
	FullAddress string               `json:"fullAddress,omitempty"`
	Email       string               `json:"email,omitempty"`
	Phone       string               `json:"phone,omitempty"`
	Owner       *authModel.User      `json:"owner,omitempty"`
	IsActive    bool                 `json:"isActive"`
	IsLimited   bool                 `json:"isLimited"`
	CreatedAt   time.Time            `json:"createdAt,omitempty"`
	Admins      []authModel.User     `json:"admins,omitempty"`
	Media       []shorterModel.Media `json:"media,omitempty"`
}

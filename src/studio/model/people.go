package model

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type StudioRole string

const (
	OWNER_ROLE         StudioRole = "owner"
	ADMIN_ROLE         StudioRole = "admin"
	TATTOO_ARTIST_ROLE StudioRole = "tattoo_artist"
)

var ALL_ROLES = []StudioRole{
	OWNER_ROLE,
	ADMIN_ROLE,
	TATTOO_ARTIST_ROLE,
}

type StudioPerson struct {
	ID          int64              `json:"id"`
	IDUser      int64              `json:"idUser"`
	IDStudio    int64              `json:"idStudio"`
	Permissions []StudioPermission `json:"permissions"`
	Roles       []StudioRole       `json:"roles"`
	User        *model.User        `json:"user,omitempty"`
}

func IsRole(role string) bool {
	return utils.Includes(ALL_ROLES, StudioRole(role))
}

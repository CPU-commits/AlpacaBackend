package model

import "github.com/CPU-commits/Template_Go-EventDriven/src/utils"

type TypeMedia string

const (
	WHATSAPP_MEDIA  TypeMedia = "whatsapp"
	WEB_MEDIA       TypeMedia = "web"
	X_MEDIA         TypeMedia = "x"
	FACEBOOK_MEDIA  TypeMedia = "facebook"
	INSTAGRAM_MEDIA TypeMedia = "instagram"
)

var ALL_MEDIAS = []TypeMedia{
	WEB_MEDIA,
	WHATSAPP_MEDIA,
	X_MEDIA,
	FACEBOOK_MEDIA,
	INSTAGRAM_MEDIA,
}

type Media struct {
	ID        int64     `json:"id"`
	Link      string    `json:"link"`
	Type      TypeMedia `json:"typeMedia"`
	ShortCode string    `json:"-"`
	IDStudio  int64     `json:"-"`
}

func IsMediaType(media string) bool {
	return utils.Includes(ALL_MEDIAS, TypeMedia(media))
}

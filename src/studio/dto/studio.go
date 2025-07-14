package dto

import (
	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
)

type StudioDTO struct {
	Name        string          `form:"name" binding:"required,max=100"`
	Username    string          `form:"username" binding:"required,max=100,username"`
	Description string          `form:"description" binding:"max=500"`
	Email       string          `form:"email" binding:"required,email"`
	Phone       string          `form:"phone" binding:"max=20"`
	Address     string          `form:"address" binding:"required,max=150"`
	AvatarImage *store.ImageDto `form:"-"`
	BannerImage *store.ImageDto `form:"-"`
}

func (s StudioDTO) ToModel(avatar, banner *fileModel.Image, idOwner int64) model.Studio {
	var image *fileModel.Image
	if avatar != nil {
		image = avatar
	}
	var bannerImage *fileModel.Image
	if banner != nil {
		bannerImage = banner
	}

	return model.Studio{
		Name:        s.Name,
		Username:    s.Username,
		Description: s.Description,
		FullAddress: s.Address,
		Email:       s.Email,
		Phone:       s.Phone,
		Owner: &authModel.User{
			ID: idOwner,
		},
		Avatar: image,
		Banner: bannerImage,
	}
}

package dto

import (
	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/go-playground/validator/v10"
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

type UpdateMediaDTO struct {
	Link string `form:"link" binding:"required,http_url,max=150"`
	Type string `form:"type" binding:"required,mediaType"`
}

var IsMediaType validator.Func = func(fl validator.FieldLevel) bool {
	media, ok := fl.Field().Interface().(string)
	if ok {
		return model.IsMediaType(media)
	}
	return false
}

type UpdateStudioDTO struct {
	Name        string           `form:"name" binding:"omitempty,max=100"`
	Description *string          `form:"description" binding:"omitempty,max=500"`
	Email       string           `form:"email" binding:"omitempty,email"`
	Phone       *string          `form:"phone" binding:"omitempty,max=20"`
	Address     string           `form:"address" binding:"omitempty,max=150"`
	AvatarImage *store.ImageDto  `form:"-"`
	BannerImage *store.ImageDto  `form:"-"`
	AddMedia    []UpdateMediaDTO `form:"addMedia" binding:"omitempty,dive"`
	RemoveMedia []int64          `form:"removeMedia"`
}

func (s *UpdateStudioDTO) ToMedia(generator uid.UIDGenerator, idStudio int64) ([]model.Media, error) {
	return utils.Map(s.AddMedia, func(media UpdateMediaDTO) (model.Media, error) {
		code, err := generator.Generate()
		if err != nil {
			return model.Media{}, err
		}

		return model.Media{
			ShortCode: code,
			Link:      media.Link,
			Type:      model.TypeMedia(media.Type),
			IDStudio:  idStudio,
		}, nil
	})
}

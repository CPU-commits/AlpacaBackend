package dto

import (
	"regexp"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/go-playground/validator/v10"
)

type UserCreatedEvent struct {
	IDUser int64  `json:"id_user"`
	Name   string `json:"name"`
	Email  string `json:"email"`
}

type UserUpdateData struct {
	Name        string               `json:"name,omitempty" binding:"omitempty,max=100"`
	Phone       string               `json:"phone,omitempty" binding:"omitempty,max=20,phone"`
	Location    string               `json:"location,omitempty" binding:"omitempty,max=200"`
	AddMedia    []dto.UpdateMediaDTO `json:"addMedia" binding:"omitempty,dive"`
	RemoveMedia []int64              `json:"removeMedia"`
}

func (s *UserUpdateData) ToMedia(generator uid.UIDGenerator, idProfile int64) ([]model.Media, error) {
	return utils.Map(s.AddMedia, func(media dto.UpdateMediaDTO) (model.Media, error) {
		code, err := generator.Generate()
		if err != nil {
			return model.Media{}, err
		}

		return model.Media{
			ShortCode: code,
			Link:      media.Link,
			Type:      model.TypeMedia(media.Type),
			IDUser:    idProfile,
		}, nil
	})
}

type UpdateAuthEmailDTO struct {
	NewEmail string `json:"newEmail" binding:"required,email" validate:"required"`
}

type QueryIsOwner struct {
	ID       int64  `form:"id,omitempty"`
	UserName string `form:"userName,omitempty" `
}

var rePhoneE164 = regexp.MustCompile(`^\+?[1-9]\d{1,14}$`)

var IsPhone validator.Func = func(fl validator.FieldLevel) bool {
	s, ok := fl.Field().Interface().(string)
	if !ok {
		return false
	}
	s = strings.TrimSpace(s)
	if s == "" { // si quieres permitir vacío, quita esta línea y usa `omitempty`
		return false
	}
	return rePhoneE164.MatchString(s) // true = válido, false = rechaza
}

package dto

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/go-playground/validator/v10"
)

type PersonDTO struct {
	Roles []string `json:"roles" binding:"required,dive,studioRole"`
}

var IsRole validator.Func = func(fl validator.FieldLevel) bool {
	role, ok := fl.Field().Interface().(string)
	if ok {
		return model.IsRole(role)
	}
	return false
}

package dto

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/go-playground/validator/v10"
)

type PermissionDTO struct {
	Permission string `json:"permission" binding:"required,permission"`
	Enabled    bool   `json:"enabled"`
}

var IsPermission validator.Func = func(fl validator.FieldLevel) bool {
	permission, ok := fl.Field().Interface().(string)
	if ok {
		return model.IsPermission(permission)
	}
	return false
}

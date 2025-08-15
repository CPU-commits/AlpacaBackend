package dto

import (
	"regexp"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/go-playground/validator/v10"
)

type RegisterDto struct {
	Name     string `json:"name" binding:"required,max=100" validate:"required"`
	Username string `json:"username" binding:"required,max=100,username" validate:"required"`
	Email    string `json:"email" binding:"required,email" validate:"required"`
	Password string `json:"password" binding:"required,min=6" validate:"required"`
}

func (registerDto *RegisterDto) ToModel() (*model.User, error) {
	return &model.User{
		Name:     registerDto.Name,
		Email:    registerDto.Email,
		Username: registerDto.Username,
		Roles:    []model.Role{model.USER_ROLE},
	}, nil
}

var IsUsername validator.Func = func(fl validator.FieldLevel) bool {
	username, ok := fl.Field().Interface().(string)
	if ok {
		pattern := `^[a-z0-9._]+$`
		matched, err := regexp.MatchString(pattern, username)
		if err != nil || !matched {
			return false
		}
		return regexp.MustCompile(`[a-z]`).MatchString(username)
	}
	return false
}

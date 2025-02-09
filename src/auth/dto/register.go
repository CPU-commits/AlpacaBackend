package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type RegisterDto struct {
	Name     string `json:"name" binding:"required,max=100" validate:"required"`
	Username string `json:"username" binding:"required,max=100" validate:"required"`
	Email    string `json:"email" binding:"required,email" validate:"required"`
	Password string `json:"password" binding:"required,min=6" validate:"required"`
	Role     string `json:"role" binding:"required"`
}

func (registerDto *RegisterDto) ToModel() (*model.User, error) {
	if registerDto.Role != string(model.TATTOO_ARTIST_ROLE) && registerDto.Role != string(model.USER_ROLE) {
		return nil, ErrUnableToRegisterRole
	}

	return &model.User{
		Name:     registerDto.Name,
		Email:    registerDto.Email,
		Username: registerDto.Username,
		Roles:    []model.Role{model.Role(registerDto.Role)},
	}, nil
}

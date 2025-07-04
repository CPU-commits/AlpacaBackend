package dto

type AuthDto struct {
	Username string `json:"username" binding:"required" validate:"required"`
	Password string `json:"password" binding:"required" validate:"required"`
}

type UpdateAuthPasswordDTO struct {
	NewPassword string `json:"newPassword" binding:"required,min=6"`
}

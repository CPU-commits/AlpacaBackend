package dto

type UpdateProfileDto struct {
	Description string `form:"description" binding:"omitempty,max=500"`
}

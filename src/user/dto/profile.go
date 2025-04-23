package dto

type UpdateProfileDto struct {
	Description string `form:"description" binding:"omitempty,max=500"`
}
type QuerySearchProfiles struct {
	CategoriesProfile string `json:"categories_profile" `
}

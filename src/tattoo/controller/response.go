package controller

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type GetTattoosResponse struct {
	Tattoos []model.Tattoo `json:"tattoos"`
}

type GetDesignResponse struct {
	Designs []model.Design `json:"designs"`
}

type GetDesignsResponse struct {
	Design model.Design `json:"design"`
}

type GetCategoriesResponse struct {
	Categories []string `json:"categories"`
}

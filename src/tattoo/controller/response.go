package controller

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type GetTattoosResponse struct {
	Tattoos []model.Tattoo `json:"tattoos"`
}

type GetDesignsResponse struct {
	Designs []model.Design `json:"designs"`
}

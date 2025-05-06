package controller

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type GetTattoosResponse struct {
	Tattoos []model.Tattoo `json:"tattoos"`
}

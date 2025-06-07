package controller

import "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"

type GetProfileResponse struct {
	Profile model.Profile `json:"profile"`
}

type UpdateProfileResponse struct {
	Key string
}

type GetUserViews struct {
	Views []int64 `json:"views"`
}

package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"

type CodeDTO struct {
	Code   string `json:"code" binding:"required"`
	IDUser int64  `json:"idUser" binding:"omitempty"`
}

type CodeParams struct {
	Code string `uri:"code"`
	Type string `form:"type"`
}

type NewCodeDTO struct {
	IDUser        int64  `json:"idUser" binding:"omitempty"`
	UsesRemaining int64  `json:"usesRemaining" binding:"required,min=1"`
	Type          string `json:"type" binding:"required"`
	Duration      int64  `json:"duration" binding:"required,min=1"`
}

func (code *NewCodeDTO) ToModel() *model.Code {
	return &model.Code{
		IDUser:        code.IDUser,
		UsesRemaining: code.UsesRemaining,
		Type:          code.Type,
	}
}

func (code *CodeDTO) ToModel() *model.Code {
	return &model.Code{
		Code:   code.Code,
		IDUser: code.IDUser,
	}
}

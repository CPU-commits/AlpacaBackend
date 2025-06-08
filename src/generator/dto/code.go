package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"

type CodeDTO struct {
	Code string `json:"code"`
}

type NewCodeDTO struct {
	IDUser        int64  `json:"idUser"`
	UsesRemaining int64  `json:"usesRemaining"`
	Type          string `json:"type"`
	Duration      int64  `json:"duration"`
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
		Code: code.Code,
	}
}

package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"

type TokenDTO struct {
	Token string `json:"token" binding:"required"`
}

func (token *TokenDTO) ToModel() *model.Token {
	return &model.Token{
		Token: token.Token,
	}
}

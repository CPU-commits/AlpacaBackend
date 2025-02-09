package dto

import (
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
)

type TattoosDto struct {
	TattooDto []TattooDto `form:"tattoos" binding:"required" validate:"required"`
}

type CoordDto struct {
	X float64
	Y float64
}

type TattooDto struct {
	IDCategories  []int64 `form:"idCategories"`
	Description   string  `form:"description" binding:"omitempty,max=250"`
	Image         store.ImageDto
	Coord         *CoordDto
	IDPublication int64
}

func (tattooDto *TattooDto) ToModel(image fileModel.Image) model.Tattoo {
	tattoo := model.Tattoo{
		Likes:         0,
		Image:         image,
		IDCategories:  tattooDto.IDCategories,
		Description:   tattooDto.Description,
		IDPublication: tattooDto.IDPublication,
	}
	if tattooDto.Coord != nil {
		tattoo.Coord = &model.Coord{
			X: tattooDto.Coord.X,
			Y: tattooDto.Coord.Y,
		}
	}

	return tattoo
}

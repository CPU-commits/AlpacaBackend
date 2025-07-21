package dto

import (
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
)

type DesignsDto struct {
	DesignDto []DesignDto `form:"design" binding:"required" validate:"required"`
}

type DesignFindDto struct {
	Username string `uri:"username"`

	Page int `form:"page,default=0" binding:"min=0"`
}
type DesignDto struct {
	Description string `form:"description" binding:"omitempty, max=500"`
	Price       int64  `form:"price" binding:"omitempty"`
	Image       store.ImageDto
	Coord       *CoordDto
}

func (designDto *DesignDto) ToModel(image fileModel.Image) model.Design {
	design := model.Design{
		Description: designDto.Description,
		Image:       image,
		Price:       designDto.Price,
	}
	if designDto.Coord != nil {
		design.Coord = &model.Coord{
			X: design.Coord.X,
			Y: design.Coord.Y,
		}
	}
	return design
}

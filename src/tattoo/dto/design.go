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

	Page          int    `form:"page,default=0" binding:"min=0"`
	Category      string `form:"category"`
	SortCreatedAt string `form:"sortCreatedAt"`
	SortPrice     string `form:"sortPrice"`
	Paginated     bool   `form:"paginated"`
}
type DesignDto struct {
	Description string `form:"description" binding:"omitempty, max=500"`
	Price       int64  `form:"price" binding:"omitempty"`
	Image       store.ImageDto
	Coord       *CoordDto
}

type DataUpdate struct {
	ID          int64  `uri:"id" binding:"required"`
	Description string `json:"description" binding:"omitempty"`
	Price       int64  `json:"price" bindin:"omitempty"`
}

type DesignParam struct {
	ID int64 `uri:"id" binding:"required"`
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

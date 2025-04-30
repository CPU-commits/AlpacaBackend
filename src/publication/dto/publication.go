package dto

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type PublicationImageDto struct {
	IsTattoo *bool `form:"isTattoo" binding:"required"`
	Coord    *dto.CoordDto
	Image    store.ImageDto
}

type PublicationDto struct {
	Content      string                `form:"content" binding:"required,max=500"`
	Images       []PublicationImageDto `form:"images" binding:"dive"`
	IDCategories []int64               `form:"idCategories"`
}

type TestPublicationDto struct {
	Content      string  `form:"content" binding:"required,max=500"`
	IDCategories []int64 `form:"idCategories"`
}

func (publicationDto *TestPublicationDto) PruebaToModel() *model.Publication {
	return &model.Publication{
		Content:      publicationDto.Content,
		IDCategories: publicationDto.IDCategories,
	}
}

func (publicationDto *PublicationDto) ToTattoos(idPublication int64) []dto.TattooDto {
	return utils.MapNoError(utils.FilterNoError(publicationDto.Images, func(image PublicationImageDto) bool {
		return *image.IsTattoo
	}), func(image PublicationImageDto) dto.TattooDto {
		return dto.TattooDto{
			IDCategories:  publicationDto.IDCategories,
			Description:   publicationDto.Content,
			Image:         image.Image,
			Coord:         image.Coord,
			IDPublication: idPublication,
		}
	})
}

func (publicationDto *PublicationDto) ToModel() (*model.Publication, []store.ImageDto) {
	images := utils.MapNoError(utils.FilterNoError(publicationDto.Images, func(image PublicationImageDto) bool {
		return !*image.IsTattoo
	}), func(image PublicationImageDto) store.ImageDto {
		return image.Image
	})

	return &model.Publication{
		Content:      publicationDto.Content,
		IDCategories: publicationDto.IDCategories,
	}, images
}

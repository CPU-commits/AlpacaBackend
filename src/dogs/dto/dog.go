package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/dogs/model"

type DogDTO struct {
	Name      string `json:"name" binding:"required"`
	OwnerName string `json:"owner" binding:"required"`
}

func (dogDto *DogDTO) ToModel() model.Dog {
	return model.Dog{
		Name:      dogDto.Name,
		OwnerName: dogDto.OwnerName,
	}
}

package controller

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

type queueDogController struct {
	dogService *service.DogService
}

func (dogController *queueDogController) InsertDogQueue(c bus.Context) error {
	var dogDto dto.DogDTO
	if err := c.BindData(&dogDto); err != nil {
		return c.Kill(err.Error())
	}

	return dogController.dogService.InsertDog(dogDto)
}

func (*queueDogController) NotifyDogIsInserted(c bus.Context) error {
	var dog model.Dog

	if err := c.BindData(&dog); err != nil {
		return c.Kill(err.Error())
	}

	fmt.Printf("dog: %v\n", dog)
	fmt.Printf("\"se ha insertado un perro y enviado una notificación!\": %v\n", "se ha insertado un perro y enviado una notificación!")

	return nil
}

func NewQueueDogController(bus bus.Bus) *queueDogController {
	return &queueDogController{
		dogService: service.NewDogService(
			dogRepositoryMemory,
			bus,
		),
	}
}

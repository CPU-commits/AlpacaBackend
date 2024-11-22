package service

import (
	"encoding/json"

	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

var dogServiceInstance *DogService

type DogService struct {
	dogRepository repository.DogRepository
	bus           bus.Bus
}

func (dogService *DogService) GetDogById(idDog int) (*model.Dog, error) {
	dog, err := dogService.dogRepository.FindOne(&repository.CriteriaDog{
		ID: idDog,
	})
	if err != nil {
		return nil, err
	}
	if dog == nil {
		return nil, ErrDogNotExists
	}
	return dog, nil
}

func (dogService *DogService) InsertDog(dogDto dto.DogDTO) error {
	dog := dogDto.ToModel()

	exists, err := dogService.dogRepository.Exists(&repository.CriteriaDog{
		OwnerName: dog.OwnerName,
	})
	if err != nil {
		return err
	}
	if exists {
		return ErrOwnerDogHasOneDog
	}

	_, err = dogService.dogRepository.Insert(dog)

	go func() {
		data, _ := json.Marshal(dog)

		dogService.bus.Publish(bus.Event{
			Name:    INSERT_DOG_EVENT,
			Payload: data,
		})
	}()
	return err
}

func NewDogService(
	dogRepository repository.DogRepository,
	bus bus.Bus,
) *DogService {
	if dogServiceInstance == nil {
		dogServiceInstance = &DogService{
			dogRepository: dogRepository,
			bus:           bus,
		}
	}

	return dogServiceInstance
}

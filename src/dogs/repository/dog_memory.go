package repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var dogs []model.Dog
var correlative = 1

type dogRepositoryMemory struct{}

func (*dogRepositoryMemory) applyCriteria(criteria *CriteriaDog) []model.Dog {
	var localDogs []model.Dog
	copy(dogs, localDogs)

	if criteria == nil {
		return dogs
	}
	if criteria.ID != 0 {
		localDogs, _ = utils.Filter(dogs, func(dog model.Dog) (bool, error) {
			return dog.ID == criteria.ID, nil
		})
	}
	if criteria.OwnerName != "" {
		localDogs, _ = utils.Filter(dogs, func(dog model.Dog) (bool, error) {
			return dog.OwnerName == criteria.OwnerName, nil
		})
	}
	return localDogs
}

func (dogRepositoryMemory dogRepositoryMemory) FindOne(criteria *CriteriaDog) (*model.Dog, error) {
	dogs := dogRepositoryMemory.applyCriteria(criteria)
	if len(dogs) > 0 {
		return &dogs[0], nil
	}
	return nil, nil
}

func (dogRepositoryMemory dogRepositoryMemory) Exists(criteria *CriteriaDog) (bool, error) {
	dogs := dogRepositoryMemory.applyCriteria(criteria)
	return len(dogs) > 0, nil
}

func (dogRepositoryMemory) Insert(dog model.Dog) (model.Dog, error) {
	dog.ID = correlative
	correlative++

	dogs = append(dogs, dog)
	return dog, nil
}

func NewDogRepositoryMemory() DogRepository {
	return dogRepositoryMemory{}
}

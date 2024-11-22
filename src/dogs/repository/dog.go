package repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/dogs/model"

// Criteria
type CriteriaDog struct {
	ID        int
	OwnerName string
}

type DogRepository interface {
	FindOne(criteria *CriteriaDog) (*model.Dog, error)
	Exists(criteria *CriteriaDog) (bool, error)
	Insert(dog model.Dog) (model.Dog, error)
}

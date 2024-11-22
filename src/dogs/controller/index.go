package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/repository"
)

// Repositories
var (
	dogRepositoryMemory = repository.NewDogRepositoryMemory()
)

package category_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type Criteria struct {
	ID    int64
	State *bool
	And   []Criteria
}

type CategoryRepository interface {
	Find(criteria *Criteria) ([]model.Category, error)
	Exists(criteria *Criteria) (bool, error)
}

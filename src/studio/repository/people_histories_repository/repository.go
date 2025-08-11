package people_histories_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
)

type Criteria struct {
	IDUser         int64
	IDStudio       int64
	RemoveAtIsNull *bool
	OR             []Criteria
	CreatedAt      *repository.CriteriaTime
	RemovedAt      *repository.CriteriaTime
}

type UpdateData struct {
	RemovedAt time.Time
}

type Sort struct {
	CreatedAt repository.OrderMod
}

type findOptions struct {
	limit *int64
	sort  *Sort
}

func NewFindOptions() *findOptions {
	return &findOptions{}
}

func (f *findOptions) Sort(sort Sort) *findOptions {
	f.sort = &sort

	return f
}

func (f *findOptions) Limit(limit int64) *findOptions {
	f.limit = &limit

	return f
}

type PeopleHistoriesRepository interface {
	Insert(history model.PeopleHistory) error
	Update(criteria *Criteria, data UpdateData) error
	Find(criteria *Criteria, opts *findOptions) ([]model.PeopleHistory, error)
}

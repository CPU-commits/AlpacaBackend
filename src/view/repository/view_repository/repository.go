package view_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/model"
)

type Criteria struct {
	IDPost    int64
	IDProfile int64
	IDStudio  int64
	IDTattoo  int64
	IDLink    int64
	CreatedAt *repository.CriteriaTime
}

type CountGroupByDayResult struct {
	Day   time.Time
	Views int64
}

type CountGroupByDayResultAndLocation struct {
	Views   int64
	City    string
	Region  string
	Country string
}

type ViewRepository interface {
	Insert(view model.View) error
	CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error)
	CountGroupByDayAndLocation(criteria *Criteria) ([]CountGroupByDayResultAndLocation, error)
	Count(criteria *Criteria) (int64, error)
}

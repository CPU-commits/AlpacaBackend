package share_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
)

type Criteria struct {
	IDPost    int64
	IDUser    int64
	CreatedAt *repository.CriteriaTime
}

type CountGroupByDayResult struct {
	Day    time.Time
	Shares int64
}

type ShareRepository interface {
	Insert(share model.Share) error
	Exists(criteria *Criteria) (bool, error)
	Delete(criteria *Criteria) error
	Count(criteria *Criteria) (int64, error)
	CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error)
}

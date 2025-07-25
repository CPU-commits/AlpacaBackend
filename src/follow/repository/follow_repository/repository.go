package follow_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/follow/model"

type Criteria struct {
	IDStudio  int64
	IDProfile int64
}

type FollowRepository interface {
	Exists(criteria *Criteria) (bool, error)
	Delete(criteria *Criteria) error
	Insert(follow model.Follow) error
	Count(criteria *Criteria) (int64, error)
}

package review_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"

type Criteria struct {
	IDUser        int64
	IDAppointment int64
}

type ReviewRepository interface {
	InsertOne(review model.Review) error
	FindOne(criteria *Criteria) (*model.Review, error)
	Exists(criteria *Criteria) (bool, error)
}

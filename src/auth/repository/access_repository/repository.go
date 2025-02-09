package access_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type AccessCriteria struct {
	IsRevoked *bool
	Token     string
	// ID Not equal
	ID_NE int64
}

type AccessUpdateData struct {
	IsRevoked *bool
}

type AccessRepository interface {
	InsertOne(access model.Access) (id int64, err error)
	Update(criteria *AccessCriteria, data AccessUpdateData) error
	Exists(criteria *AccessCriteria) (id int64, err error)
	Delete(criteria *AccessCriteria) error
}

package link_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"

type Criteria struct {
	ShortCode string
}

type LinkRepository interface {
	FindOne(criteria *Criteria) (*model.Link, error)
}

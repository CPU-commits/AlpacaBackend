package user_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
)

type Criteria struct {
	ID       int64
	Username string
	Email    string
	Or       []Criteria
}

type SelectOpts struct {
	ID       *bool
	Username *bool
	Name     *bool
}

type FindOneOptions struct {
	selectOpts *SelectOpts
}

func NewFindOneOptions() *FindOneOptions {
	return &FindOneOptions{}
}

func (opts *FindOneOptions) Select(selectOpts SelectOpts) *FindOneOptions {
	opts.selectOpts = &selectOpts

	return opts
}

type UserRepository interface {
	FindOneByEmail(email string) (*model.User, error)
	FindOneByID(id int64) (*model.User, error)
	FindOne(criteria *Criteria, opts *FindOneOptions) (*model.User, error)
	Exists(criteria *Criteria) (bool, error)
	InsertOne(user *model.User, password string) (*model.User, error)
}

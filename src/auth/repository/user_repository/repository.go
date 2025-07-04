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
	Email    *bool
	Phone    *bool
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

type UserUpdateData struct {
	Email *string
	Name  *string
	Phone *string
}

type UserRepository interface {
	FindOneByEmail(email string) (*model.User, error)
	FindOneByID(id int64) (*model.User, error)
	FindOne(criteria *Criteria, opts *FindOneOptions) (*model.User, error)
	Exists(criteria *Criteria) (bool, error)
	InsertOne(user *model.User, password string) (*model.User, error)
	UpdateOne(userId int64, data UserUpdateData) error
}

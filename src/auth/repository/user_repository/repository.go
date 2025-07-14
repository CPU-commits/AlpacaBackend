package user_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
)

type Criteria struct {
	ID       int64
	ID_NIN   []int64
	Username repository.CriteriaString
	Name     repository.CriteriaString
	Email    repository.CriteriaString
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

type FindOptions struct {
	selectOpts *SelectOpts
	limit      *int64
	skip       *int64
}

func NewFindOptions() *FindOptions {
	return &FindOptions{}
}

func (opts *FindOptions) Select(selectOpts SelectOpts) *FindOptions {
	opts.selectOpts = &selectOpts

	return opts
}

func (opts *FindOptions) Limit(limit int64) *FindOptions {
	opts.limit = &limit

	return opts
}

func (opts *FindOptions) Skip(skip int64) *FindOptions {
	opts.skip = &skip

	return opts
}

type UserRepository interface {
	FindOneByEmail(email string) (*model.User, error)
	FindOneByID(id int64) (*model.User, error)
	FindOne(criteria *Criteria, opts *FindOneOptions) (*model.User, error)
	Find(criteria *Criteria, opts *FindOptions) ([]model.User, error)
	Exists(criteria *Criteria) (bool, error)
	InsertOne(user *model.User, password string) (*model.User, error)
	UpdateOne(userId int64, data UserUpdateData) error
}

package profile_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Criteria struct {
	ID        int64
	IDUser_IN []int64
	IDUser    int64
	OR        []Criteria
	Username  *repository.CriteriaString
}

type SelectOpts struct {
	ID     *bool
	IDUser *bool
	Avatar *bool
	Limit  *int
	OffSet *int
}

type LoadOpts struct {
	Avatar    bool
	User      *user_repository.SelectOpts
	Roles     bool
	UserMedia bool
}

type FindOneOptions struct {
	SelectOpts *SelectOpts
	load       *LoadOpts
}

type FindOptions struct {
	SelectOpts *SelectOpts
	load       *LoadOpts
}

func NewFindOptions() *FindOptions {
	return &FindOptions{}
}

func (findOptions *FindOptions) Select(selectOpts SelectOpts) *FindOptions {
	findOptions.SelectOpts = &selectOpts

	return findOptions
}

func (opts *FindOptions) Load(load LoadOpts) *FindOptions {
	opts.load = &load

	return opts
}

func NewFindOneOptions() *FindOneOptions {
	return &FindOneOptions{}
}

func (findOneOptions *FindOneOptions) Select(selectOpts SelectOpts) *FindOneOptions {
	findOneOptions.SelectOpts = &selectOpts

	return findOneOptions
}

func (opts *FindOneOptions) Load(load LoadOpts) *FindOneOptions {
	opts.load = &load

	return opts
}

type UpdateData struct {
	Avatar      *fileModel.Image
	Description *string
}

type ProfileRepository interface {
	FindOne(criteria *Criteria, opts *FindOneOptions) (*model.Profile, error)
	UpdateOne(criteria *Criteria, data UpdateData) error
	Find(criteria *Criteria, opts *FindOptions) ([]model.Profile, error)
}

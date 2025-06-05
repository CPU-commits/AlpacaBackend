package profile_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

type Criteria struct {
	ID     int64
	IDUser int64
}

type SelectOpts struct {
	ID     *bool
	IDUser *bool
	Avatar *bool
	Limit  *int
	OffSet *int
}

type LoadOpts struct {
	Avatar bool
	User   *user_repository.SelectOpts
	Roles  bool
}

type FindOneOptions struct {
	SelectOpts *SelectOpts
	load       *LoadOpts
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
	Find(opts *FindOneOptions) (*[]model.Profile, error)
}

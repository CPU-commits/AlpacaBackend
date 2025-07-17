package studio_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"

type Criteria struct {
	ID       int64
	IDOwner  int64
	Email    string
	Username string
	OR       []Criteria
}

type Include struct {
	AvatarImage bool
	BannerImage bool
}

type SelectOpts struct {
	ID          bool
	IDOwner     bool
	Name        bool
	Username    bool
	Description bool
	Address     *bool
}

type findOptions struct {
	include    *Include
	selectOpts *SelectOpts
}

func NewFindOptions() *findOptions {
	return &findOptions{}
}

func (f *findOptions) Select(selectOpts SelectOpts) *findOptions {
	f.selectOpts = &selectOpts

	return f
}

func (f *findOptions) Include(include Include) *findOptions {
	f.include = &include

	return f
}

type findOneOptions struct {
	include    *Include
	selectOpts *SelectOpts
}

func NewFindOneOptions() *findOneOptions {
	return &findOneOptions{}
}

func (f *findOneOptions) Include(include Include) *findOneOptions {
	f.include = &include

	return f
}

func (f *findOneOptions) Select(selectOpts SelectOpts) *findOneOptions {
	f.selectOpts = &selectOpts

	return f
}

type StudioRepository interface {
	Count(criteria *Criteria) (int64, error)
	Exists(criteria *Criteria) (bool, error)
	Find(criteria *Criteria, opts *findOptions) ([]model.Studio, error)
	FindOne(criteria *Criteria, opts *findOneOptions) (*model.Studio, error)
	InsertOne(studio model.Studio) error
}

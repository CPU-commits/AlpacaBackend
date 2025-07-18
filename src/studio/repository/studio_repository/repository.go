package studio_repository

import (
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
)

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
	Media       bool
}

type SelectOpts struct {
	ID          bool
	IDOwner     bool
	IDAvatar    *bool
	IDBanner    *bool
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

type UpdateData struct {
	Name        string
	Description *string
	FullAddress string
	Email       string
	Phone       *string
	Avatar      *fileModel.Image
	Banner      *fileModel.Image
	AddMedia    []model.Media
	RemoveMedia []int64
}

type StudioRepository interface {
	Count(criteria *Criteria) (int64, error)
	Exists(criteria *Criteria) (bool, error)
	Find(criteria *Criteria, opts *findOptions) ([]model.Studio, error)
	FindOne(criteria *Criteria, opts *findOneOptions) (*model.Studio, error)
	InsertOne(studio model.Studio) error
	Update(criteria *Criteria, data UpdateData) error
}

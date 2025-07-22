package studio_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	shorterModel "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
)

type Criteria struct {
	ID       int64
	IDOwner  int64
	Email    repository.CriteriaString
	Username repository.CriteriaString
	Name     repository.CriteriaString
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
	limit      *int64
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

func (f *findOptions) Limit(limit int64) *findOptions {
	f.limit = &limit

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

type RemoveMedia struct {
	IDStudio int64
	IDs      []int64
}

type UpdateData struct {
	Name        string
	Description *string
	FullAddress string
	Email       string
	Phone       *string
	Avatar      *fileModel.Image
	Banner      *fileModel.Image
	AddMedia    []shorterModel.Media
	RemoveMedia RemoveMedia
}

type StudioRepository interface {
	Count(criteria *Criteria) (int64, error)
	Exists(criteria *Criteria) (bool, error)
	Find(criteria *Criteria, opts *findOptions) ([]model.Studio, error)
	FindOne(criteria *Criteria, opts *findOneOptions) (*model.Studio, error)
	InsertOne(studio model.Studio) error
	Update(criteria *Criteria, data UpdateData) error
}

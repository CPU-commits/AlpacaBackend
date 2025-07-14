package publication_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
)

type Criteria struct {
	ID         int64
	IDProfile  int64
	Categories []string
	IDStudio   *repository.CriteriaNull[*int64]
}

type SelectOpts struct {
	User      *user_repository.SelectOpts
	ID        *bool
	IDProfile *bool
}

type Include struct {
	Tattoos       bool
	TattoosImage  bool
	Images        bool
	Categories    bool
	Profile       bool
	ProfileAvatar bool
	ProfileUser   bool
}

type FindOneOptions struct {
	include    *Include
	selectOpts *SelectOpts
}

type Sort struct {
	CreatedAt string
}

func (opts *FindOneOptions) Include(include Include) *FindOneOptions {
	opts.include = &include

	return opts
}

func (opts *FindOneOptions) Select(selectOpts SelectOpts) *FindOneOptions {
	opts.selectOpts = &selectOpts

	return opts
}

func NewFindOneOptions() *FindOneOptions {
	return &FindOneOptions{}
}

type FindOptions struct {
	include    *Include
	limit      *int
	skip       *int
	selectOpts *SelectOpts
	sort       *Sort
}

func (opts *FindOptions) Include(include Include) *FindOptions {
	opts.include = &include

	return opts
}

func (opts *FindOptions) Select(selectOpts SelectOpts) *FindOptions {
	opts.selectOpts = &selectOpts

	return opts
}

func (opts *FindOptions) Limit(limit int) *FindOptions {
	opts.limit = &limit

	return opts
}

func (opts *FindOptions) Skip(skip int) *FindOptions {
	opts.skip = &skip

	return opts
}

func (opts *FindOptions) Sort(sort Sort) *FindOptions {
	opts.sort = &sort

	return opts
}

func NewFindOptions() *FindOptions {
	return &FindOptions{}
}

type UpdateData struct {
	SumLikes int
	SumViews int
}

type SearchOptions struct {
	limit      *int
	skip       *int
	include    *Include
	selectOpts *SelectOpts
}

func (opts *SearchOptions) Limit(limit int) *SearchOptions {
	opts.limit = &limit

	return opts
}

func (opts *SearchOptions) Skip(skip int) *SearchOptions {
	opts.skip = &skip

	return opts
}

func (opts *SearchOptions) Select(selectOpts SelectOpts) *SearchOptions {
	opts.selectOpts = &selectOpts

	return opts
}

func (opts *SearchOptions) Include(include Include) *SearchOptions {
	opts.include = &include

	return opts
}

func NewSearchOptions() *SearchOptions {
	return &SearchOptions{}
}

type PublicationRepository interface {
	Insert(publication model.Publication, idProfile int64) (*model.Publication, error)
	FindOne(criteria *Criteria, opts *FindOneOptions) (*model.Publication, error)
	Find(criteria *Criteria, opts *FindOptions) ([]model.Publication, error)
	Exists(criteria *Criteria) (bool, error)
	Count(criteria *Criteria) (int64, error)
	UpdateOne(criteria *Criteria, data UpdateData) error
	Delete(criteria *Criteria) error
	FindImages(idPublication int64) ([]fileModel.Image, error)
	Search(q string, criteria *Criteria, opts *SearchOptions) ([]model.Publication, int64, error)
}

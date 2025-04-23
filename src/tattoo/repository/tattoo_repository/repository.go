package tattoo_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type Criteria struct {
	IDs       []int64
	IDProfile int64
}

type Include struct {
	Image         bool
	Categories    bool
	ProfileAvatar bool
	ProfileUser   bool
}

type Sort struct {
	CreatedAt string
}

type FindOpts struct {
	skip    *int
	limit   *int
	include *Include
	sort    *Sort
}

func NewFindOptions() *FindOpts {
	return &FindOpts{}
}

func (opts *FindOpts) Skip(skip int) *FindOpts {
	opts.skip = &skip

	return opts
}

func (opts *FindOpts) Limit(limit int) *FindOpts {
	opts.limit = &limit

	return opts
}

func (opts *FindOpts) Include(include Include) *FindOpts {
	opts.include = &include

	return opts
}

func (opts *FindOpts) Sort(sort Sort) *FindOpts {
	opts.sort = &sort

	return opts
}

type UpdateData struct {
	UnsetIDPublication bool
}

type TattooRepository interface {
	Insert(tattoos []model.Tattoo, idProfile int64) ([]model.Tattoo, error)
	Find(criteria *Criteria, opts *FindOpts) ([]model.Tattoo, error)
	Count(criteria *Criteria) (int64, error)
	UpdateViews(ids []int64) error
	Update(criteria *Criteria, data UpdateData) error
}

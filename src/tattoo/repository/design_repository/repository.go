package design_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"

type Criteria struct {
	ID        int64
	IDs       []int64
	IDProfile int64
	Category  string
	IsDeleted *bool
}

type Include struct {
	Image         bool
	Categories    bool
	ProfileAvatar bool
	ProfileUser   bool
}

type Sort struct {
	CreatedAt string
	Price     string
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
	Description *string
	Categories  *[]string
	Price       *int64
	IsDeleted   *bool
}

type SimilarityParams struct {
	Embedding []float64
	IDDesign  int64
}

type SimilarityOptions struct {
	skip    *int
	limit   *int
	include *Include
}

func NewSimilarityOptions() *SimilarityOptions {
	return &SimilarityOptions{}
}

func (opts *SimilarityOptions) Skip(skip int) *SimilarityOptions {
	opts.skip = &skip
	return opts
}

func (opts *SimilarityOptions) Limit(limit int) *SimilarityOptions {
	opts.limit = &limit
	return opts
}

func (opts *SimilarityOptions) Include(include Include) *SimilarityOptions {
	opts.include = &include
	return opts
}

type FindOneOpts struct {
	include *Include
}

func NewFindOneOptions() *FindOneOpts {
	return &FindOneOpts{}
}

func (opts *FindOneOpts) Include(include Include) *FindOneOpts {
	opts.include = &include
	return opts
}

type DesignRepository interface {
	Insert(designs []model.Design, idProfile int64) ([]model.Design, error)
	Find(criteria *Criteria, opts *FindOpts) ([]model.Design, error)
	FindOne(criteria *Criteria, opts *FindOneOpts) (*model.Design, error)
	Count(criteria *Criteria) (int64, error)
	Update(criteria *Criteria, data UpdateData) error
	Delete(criteria *Criteria) error
	GetCategories(criteria *Criteria) ([]string, error)
}

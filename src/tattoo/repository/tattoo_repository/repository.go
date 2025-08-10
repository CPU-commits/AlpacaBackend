package tattoo_repository

import (
	modelPublication "github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
)

type Criteria struct {
	ID            int64
	IDs           []int64
	IDProfile     int64
	IDStudio      int64
	IDPublication int64
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

type SimilarityParams struct {
	Embedding   []float64
	ImageBase64 string
	IDTattoo    int64
}

type SimilarityOptions struct {
	skip    *int
	limit   *int
	include *Include
}

func (opts *SimilarityOptions) Limit(limit int) *SimilarityOptions {
	opts.limit = &limit

	return opts
}

func (opts *SimilarityOptions) Skip(skip int) *SimilarityOptions {
	opts.skip = &skip

	return opts
}

func (opts *SimilarityOptions) Include(include Include) *SimilarityOptions {
	opts.include = &include

	return opts
}

func NewSimilarityOptions() *SimilarityOptions {
	return &SimilarityOptions{}
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

type TattooRepository interface {
	Insert(tattoos []model.Tattoo, idProfile int64) ([]model.Tattoo, error)
	Find(criteria *Criteria, opts *FindOpts) ([]model.Tattoo, error)
	FindOne(criteria *Criteria, opts *FindOneOpts) (*model.Tattoo, error)
	Count(criteria *Criteria) (int64, error)
	ConvertImageInTattoo(idImages []int64, tattoos []model.Tattoo, idProfile int64) ([]model.Tattoo, error)
	UpdateViews(ids []int64) error
	Update(criteria *Criteria, data UpdateData) error
	TattooSimilarity(params SimilarityParams, opts *SimilarityOptions) ([]model.Tattoo, int64, error)
	Delete(criteria *Criteria) error
}

type TattooTSRepository interface {
	Search(params SimilarityParams, opts *SimilarityOptions) ([]int64, int64, error)
	IndexTattoo(tattoo *model.Tattoo) error
	UpdateRatingTattoo(tattoo *model.Tattoo, publication *modelPublication.TSPublication) error
	DeleteTattoo(tattoo *model.Tattoo) error
}

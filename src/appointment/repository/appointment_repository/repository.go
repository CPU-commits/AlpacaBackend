package appointment_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

type Criteria struct {
	IDUser         int64
	IDTattooArtist int64
}

type Sort struct {
	CreatedAt string
}

type findOptions struct {
	sort  *Sort
	limit *int64
	load  *LoadOpts
	skip  *int64
}

type LoadOpts struct {
	TattooArtist *user_repository.SelectOpts
	User         *user_repository.SelectOpts
	Profile      *profile_repository.SelectOpts
	Images       bool
}

func (f *findOptions) Skip(skip int64) *findOptions {
	f.skip = &skip

	return f
}

func (f *findOptions) Limit(limit int64) *findOptions {
	f.limit = &limit

	return f
}

func (f *findOptions) Sort(sort Sort) *findOptions {
	f.sort = &sort

	return f
}

func (f *findOptions) Load(load LoadOpts) *findOptions {
	f.load = &load

	return f
}

func NewFindOptions() *findOptions {
	return &findOptions{}
}

type AppointmentRepository interface {
	Find(criteria *Criteria, opts *findOptions) ([]model.Appointment, error)
	Count(criteria *Criteria) (int64, error)
	Insert(appointment *model.Appointment) (*model.Appointment, error)
}

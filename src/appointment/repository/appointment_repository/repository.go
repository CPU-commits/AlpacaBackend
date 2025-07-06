package appointment_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

type Criteria struct {
	ID             int64
	IDNE           int64
	Status         model.AppointmentStatus
	IDUser         int64
	IDTattooArtist int64
	FinishedAt     *repository.CriteriaTime
	ScheduledAtGTE time.Time
	Or             []Criteria
}

type Sort struct {
	CreatedAt string
}

type SelectOpts struct {
	IDUser         *bool
	IDTattooArtist *bool
	IDCalendar     *bool
}

type findOptions struct {
	sort  *Sort
	limit *int64
	load  *LoadOpts
	skip  *int64
}

type LoadOpts struct {
	TattooArtist  *user_repository.SelectOpts
	User          *user_repository.SelectOpts
	Profile       *profile_repository.SelectOpts
	ProfileAvatar bool
	Images        bool
	Review        bool
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

type findOneOptions struct {
	selectOpts *SelectOpts
}

func (f *findOneOptions) Select(selectOpts SelectOpts) *findOneOptions {
	f.selectOpts = &selectOpts

	return f
}

func NewFindOneOptions() *findOneOptions {
	return &findOneOptions{}
}

type UpdateData struct {
	Status          model.AppointmentStatus
	ScheduledAt     time.Time
	Duration        float64
	FinishedAt      time.Time
	UnsetDuration   bool
	UnsetFinishedAt bool
	IDCalendar      string
}

type AppointmentRepository interface {
	Find(criteria *Criteria, opts *findOptions) ([]model.Appointment, error)
	FindOne(criteria *Criteria, opts *findOneOptions) (*model.Appointment, error)
	Count(criteria *Criteria) (int64, error)
	Exists(criteria *Criteria) (bool, error)
	Update(criteria *Criteria, data *UpdateData) error
	Insert(appointment *model.Appointment) (*model.Appointment, error)
}

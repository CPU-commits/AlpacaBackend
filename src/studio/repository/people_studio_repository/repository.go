package people_studio_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
)

type Criteria struct {
	IDStudio    int64
	IDUser      int64
	Permissions []model.StudioPermission
	Roles       []model.StudioRole
}

type Include struct {
	User *user_repository.SelectOpts
}

type findOpts struct {
	include *Include
}

func (f *findOpts) Include(include Include) *findOpts {
	f.include = &include

	return f
}

func NewFindOptions() *findOpts {
	return &findOpts{}
}

type UpdatePermission struct {
	Permission model.StudioPermission
	Enabled    bool
}

type UpdateData struct {
	Roles      []model.StudioRole
	Permission *UpdatePermission
}

type PeopleStudioRepository interface {
	Exists(criteria *Criteria) (bool, error)
	FindOne(criteria *Criteria) (*model.StudioPerson, error)
	Find(criteria *Criteria, opts *findOpts) ([]model.StudioPerson, error)
	InsertOne(admin model.StudioPerson) error
	Update(criteria *Criteria, data UpdateData) error
	Delete(criteria *Criteria) error
}

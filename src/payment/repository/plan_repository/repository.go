package plan_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"

type Criteria struct {
	ID         int64
	IsActive   *bool
	Identifier string
	Code       model.CodePlan
	ForStudios *bool
}

type PlanRepository interface {
	Find(criteria *Criteria) ([]model.Plan, error)
	FindOne(criteria *Criteria) (*model.Plan, error)
}

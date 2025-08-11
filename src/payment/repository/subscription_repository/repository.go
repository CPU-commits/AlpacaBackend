package subscription_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
)

type Criteria struct {
	ID         int64
	IDUser     int64
	Identifier string
	IDPlan     int64
	IDStudio   int64
	StatusNE   model.SubscriptionStatus
}

type SelectOpts struct {
	ID         *bool
	Status     *bool
	CanceledAt *bool
}

type LoadOpts struct {
	Plan bool
}

type findOneOptions struct {
	selectOpts *SelectOpts
	loadOpts   *LoadOpts
}

func (f *findOneOptions) Select(selectOpts SelectOpts) *findOneOptions {
	f.selectOpts = &selectOpts

	return f
}

func (f *findOneOptions) Load(loadOpts LoadOpts) *findOneOptions {
	f.loadOpts = &loadOpts

	return f
}

func NewFindOneOptions() *findOneOptions {
	return &findOneOptions{}
}

type UpdateData struct {
	CardBrand       string
	BillingAnchor   int
	CardLastFour    string
	EndDate         time.Time
	Status          model.SubscriptionStatus
	CanceledAt      *time.Time
	NextBillingDate time.Time
}

type SubscriptionRepository interface {
	FindLast(criteria *Criteria, opts *findOneOptions) (*model.Subscription, error)
	Exists(criteria *Criteria) (bool, error)
	FindOne(criteria *Criteria, opts *findOneOptions) (*model.Subscription, error)
	InsertOne(subscription model.Subscription) (int64, error)
	Update(criteria *Criteria, data UpdateData) error
}

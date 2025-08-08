package payment_repository

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
)

type Criteria struct {
	Identifier     string
	IDUser         int64
	IDSubscription int64
}

type UpdateData struct {
	Status   model.PaymentStatus
	Price    float64
	Currency string
}

type Order struct {
	UpdatedAt repository.OrderMod
}

type findOptions struct {
	limit *int64
	skip  *int64
	order *Order
}

func (f *findOptions) Order(order Order) *findOptions {
	f.order = &order

	return f
}

func (f *findOptions) Skip(skip int64) *findOptions {
	f.skip = &skip

	return f
}

func (f *findOptions) Limit(limit int64) *findOptions {
	f.limit = &limit

	return f
}

func NewFindOptions() *findOptions {
	return &findOptions{}
}

type PaymentRepository interface {
	InsertOne(payment model.Payment) error
	Update(criteria *Criteria, data UpdateData) error
	Exists(criteria *Criteria) (bool, error)
	Find(criteria *Criteria, opts *findOptions) ([]model.Payment, error)
	Count(criteria *Criteria) (int64, error)
}

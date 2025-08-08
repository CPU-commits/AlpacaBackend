package model

import "time"

type PaymentStatus string

const (
	PAYMENT_PENDING  PaymentStatus = "pending"
	PAYMENT_FAILED   PaymentStatus = "refused"
	PAYMENT_PAID     PaymentStatus = "paid"
	PAYMENT_REFUNDED PaymentStatus = "refunded"
)

type Payment struct {
	ID             int64         `json:"id"`
	IDUser         int64         `json:"-"`
	IDStudio       int64         `json:"-"`
	IDSubscription int64         `json:"-"`
	Status         PaymentStatus `json:"status"`
	Text           string        `json:"text"`
	Price          float64       `json:"price"`
	PriceFormated  string        `json:"priceFormated"`
	Currency       string        `json:"currency"`
	Identifier     string        `json:"-"`
	CreatedAt      time.Time     `json:"createdAt"`
	UpdatedAt      time.Time     `json:"updatedAt"`
}

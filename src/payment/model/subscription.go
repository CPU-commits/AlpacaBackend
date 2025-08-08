package model

import "time"

type SubscriptionStatus string

const (
	SUBSCRIPTION_TRIAL    SubscriptionStatus = "trialing"
	SUBSCRIPTION_ACTIVE   SubscriptionStatus = "active"
	SUBSCRIPTION_PAUSED   SubscriptionStatus = "paused"
	SUBSCRIPTION_PAST_DUE SubscriptionStatus = "past_due"
	SUBSCRIPTION_CANCELED SubscriptionStatus = "canceled"
)

type Subscription struct {
	ID              int64              `json:"id"`
	IDUser          int64              `json:"-"`
	IDPlan          int64              `json:"-"`
	IDStudio        int64              `json:"-"`
	StartDate       time.Time          `json:"-"`
	Plan            *Plan              `json:"plan,omitempty"`
	CardBrand       string             `json:"cardBrand,omitempty"`
	BillingAnchor   int                `json:"billingAnchor"`
	CardLastFour    string             `json:"cardLastFour,omitempty"`
	EndDate         time.Time          `json:"endDate"`
	Status          SubscriptionStatus `json:"status"`
	CanceledAt      *time.Time         `json:"cancelledAt,omitempty"`
	NextBillingDate time.Time          `json:"nextBillingDate"`
	Identifier      string             `json:"-"`
	UpdateMethodUrl string             `json:"updateMethodUrl,omitempty"`
	CreatedAt       time.Time          `json:"createdAt"`
}

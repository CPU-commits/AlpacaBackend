package model

import "time"

type PlanFeature struct {
	Code  string `json:"-"`
	Label string `json:"label"`
	Value any    `json:"value"`
}

type CodePlan string

const (
	SubscriptionTattooer CodePlan = "subscriptionTattooer"
	SubscriptionStudio   CodePlan = "subscriptionStudio"
)

type PricingModel string

const (
	PRICING_MODEL_STANDARD PricingModel = "standard"
	PRICING_MODEL_VOLUME   PricingModel = "volume"
)

type Plan struct {
	ID           int64         `json:"id"`
	Name         string        `json:"name"`
	PricingModel PricingModel  `json:"pricingModel"`
	Description  string        `json:"description,omitempty"`
	Price        float64       `json:"price"`
	Code         CodePlan      `json:"-"`
	ForStudios   bool          `json:"forStudios"`
	Currency     string        `json:"currency"`
	Features     []PlanFeature `json:"features"`
	BillingCycle string        `json:"billingCycle"`
	TrialDays    int           `json:"trailDays"`
	IsActive     bool          `json:"isActive"`
	Identifier   string        `json:"-"`
	BannerUrl    string        `json:"bannerUrl,omitempty"`
	VolumeItem   string        `json:"volumeItem,omitempty"`
	CreatedAt    time.Time     `json:"createdAt"`
	UpdatedAt    time.Time     `json:"updatedAt"`
}

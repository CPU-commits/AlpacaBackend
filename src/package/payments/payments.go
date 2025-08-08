package payments

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
)

type BacksUrls struct {
	Success string
	Failure string
	Pending string
}

type BaseUser struct {
	Name  string
	Email string
}

type PaymentConfig struct {
	Identifier string
	BacksUrls  BacksUrls
	User       BaseUser
	CustomData map[string]string
	ExpiresAt  *time.Time
	SkipTrial  bool
}

type Payment struct {
	Identifier           string
	TargetIdentifier     string
	TargetIsSubscription bool
	Currency             string
	Price                float64
	Status               model.PaymentStatus
	TotalFormated        string
}

type SubscriptionUrls struct {
	UpdatePaymentMethod string
}

type Subscription struct {
	PlanIdentifier string
	Identifier     string
	Status         model.SubscriptionStatus
	BillingAnchor  int
	CanceledAt     *time.Time
	EndsAt         time.Time
	CardBrand      string
	CardLastFour   string
	Urls           SubscriptionUrls
	CreatedAt      time.Time
	RenewsAt       time.Time
}

type Payments interface {
	CancelSubscription(identifier string) error
	RequestPayment(config PaymentConfig) (string, error)
	FetchPayment(identifier string) (*Payment, error)
	FetchSubscription(identifier string) (*Subscription, error)
}

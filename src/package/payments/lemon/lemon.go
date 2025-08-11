package lemon

import (
	"context"
	"fmt"
	"strconv"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/payments"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/NdoleStudio/lemonsqueezy-go"
	"github.com/go-resty/resty/v2"
)

var settingsData = settings.GetSettings()

type M map[string]any

type lemonSqueezePayments struct {
	client *lemonsqueezy.Client
	http   *resty.Client
}

func (l lemonSqueezePayments) SetUsageInSubscription(identifier string, quantity int) error {
	payload := M{"data": M{
		"type": "usage-records",
		"attributes": M{
			"quantity": quantity,
			"action":   "set",
		},
		"relationships": M{
			"subscription-item": M{
				"data": M{
					"type": "subscription-items",
					"id":   identifier,
				},
			},
		},
	}}

	res, err := l.http.R().
		SetHeader("Content-Type", "application/vnd.api+json").
		SetHeader("Accept", "application/vnd.api+json").
		SetHeader("Authorization", fmt.Sprintf("Bearer %s", settingsData.LEMONSQUEEZY_TOKEN)).
		SetBody(payload).
		Post("https://api.lemonsqueezy.com/v1/usage-records")

	if err != nil {
		return err
	}
	if res.IsError() {
		return fmt.Errorf("HTTP: %s, Res: %s", res.Status(), res.String())
	}

	return nil
}

func (l lemonSqueezePayments) CancelSubscription(identifier string) error {
	_, _, err := l.client.Subscriptions.Cancel(context.Background(), identifier)
	return err
}

func (l lemonSqueezePayments) FetchPayment(id string) (*payments.Payment, error) {
	order, _, err := l.client.Orders.Get(context.Background(), id)
	if err != nil {
		return nil, err
	}
	var status model.PaymentStatus = model.PAYMENT_PENDING
	orderStatus := order.Data.Attributes.Status

	if orderStatus == "pending" {
		status = model.PAYMENT_PENDING
	} else if orderStatus == "failed" || orderStatus == "fraudulent" {
		status = model.PAYMENT_FAILED
	} else if orderStatus == "paid" {
		status = model.PAYMENT_PAID
	} else if order.Data.Attributes.Refunded {
		status = model.PAYMENT_REFUNDED
	}
	targetIdentifier := strconv.Itoa(order.Data.Attributes.FirstOrderItem.VariantID)

	variant, res, err := l.client.Variants.Get(context.Background(), order.Data.Attributes.FirstOrderItem.VariantID)
	if res.HTTPResponse.StatusCode != 404 && err != nil {
		return nil, err
	}

	return &payments.Payment{
		Identifier:           id,
		TargetIdentifier:     targetIdentifier,
		Currency:             order.Data.Attributes.Currency,
		Price:                float64(order.Data.Attributes.Total) / 100,
		Status:               status,
		TotalFormated:        order.Data.Attributes.TotalFormatted,
		TargetIsSubscription: variant.Data.Attributes.IsSubscription,
	}, nil
}

func (l lemonSqueezePayments) FetchSubscription(id string) (*payments.Subscription, error) {
	subscription, _, err := l.client.Subscriptions.Get(context.Background(), id)
	if err != nil {
		return nil, err
	}
	var status model.SubscriptionStatus
	subStatus := subscription.Data.Attributes.Status

	if subscription.Data.Attributes.Cancelled || subStatus == "cancelled" || subStatus == "expired" || subStatus == "unpaid" {
		status = model.SUBSCRIPTION_CANCELED
	} else if subStatus == "past_due" {
		status = model.SUBSCRIPTION_PAST_DUE
	} else if subStatus == "on_trial" {
		status = model.SUBSCRIPTION_TRIAL
	} else if subStatus == "active" {
		status = model.SUBSCRIPTION_ACTIVE
	} else if subStatus == "paused" {
		status = model.SUBSCRIPTION_PAUSED
	}
	var endsAt time.Time
	if subscription.Data.Attributes.TrialEndsAt != nil {
		endsAt = *subscription.Data.Attributes.TrialEndsAt
	} else if subStatus == "active" {
		now := time.Now()
		endsAt = time.Date(
			now.Year(),
			now.Month(),
			subscription.Data.Attributes.BillingAnchor,
			0,
			0,
			0,
			0,
			time.UTC,
		)
	} else if subStatus == "unpaid" || subStatus == "past_due" {
		endsAt = time.Date(
			subscription.Data.Attributes.RenewsAt.Year(),
			subscription.Data.Attributes.RenewsAt.Month(),
			subscription.Data.Attributes.BillingAnchor,
			0,
			0,
			0,
			0,
			time.UTC,
		)
	}
	var identifierItemsSubscription string
	if subscription.Data.Attributes.FirstSubscriptionItem != nil {
		identifierItemsSubscription = strconv.Itoa(subscription.Data.Attributes.FirstSubscriptionItem.ID)
	}

	return &payments.Subscription{
		Identifier:     id,
		PlanIdentifier: strconv.Itoa(subscription.Data.Attributes.VariantID),
		Status:         status,
		BillingAnchor:  subscription.Data.Attributes.BillingAnchor,
		EndsAt:         endsAt,
		CardBrand:      subscription.Data.Attributes.CardBrand,
		CardLastFour:   subscription.Data.Attributes.CardLastFour,
		Urls: payments.SubscriptionUrls{
			UpdatePaymentMethod: subscription.Data.Attributes.Urls.UpdatePaymentMethod,
		},
		CreatedAt:                   subscription.Data.Attributes.CreatedAt,
		RenewsAt:                    subscription.Data.Attributes.RenewsAt,
		CanceledAt:                  subscription.Data.Attributes.EndsAt,
		IdentifierItemsSubscription: identifierItemsSubscription,
	}, nil
}

func (l lemonSqueezePayments) RequestPayment(config payments.PaymentConfig) (string, error) {
	var expiresAt *string
	if config.ExpiresAt != nil {
		expiresAtStr := config.ExpiresAt.Format(time.RFC3339)

		expiresAt = &expiresAtStr
	}
	var response *lemonsqueezy.CheckoutAPIResponse
	attributes := M{
		"checkout_data": M{
			"email":  config.User.Email,
			"name":   config.User.Name,
			"custom": config.CustomData,
		},
		"checkout_options": M{
			"embed":      true,
			"skip_trial": config.SkipTrial,
		},
		"test_mode": true,
		"product_options": M{
			"redirect_url":     config.BacksUrls.Success,
			"receipt_link_url": config.BacksUrls.Success,
		},
	}
	if expiresAt != nil {
		attributes["expires_at"] = *expiresAt
	}

	payload := M{"data": M{
		"type":       "checkouts",
		"attributes": attributes,
		"relationships": M{
			"store": M{
				"data": M{
					"type": "stores",
					"id":   settingsData.LEMONSQUEEZY_STORE,
				},
			},
			"variant": M{
				"data": M{
					"type": "variants",
					"id":   config.Identifier,
				},
			},
		},
	}}

	res, err := l.http.R().
		SetHeader("Content-Type", "application/vnd.api+json").
		SetHeader("Accept", "application/vnd.api+json").
		SetHeader("Authorization", fmt.Sprintf("Bearer %s", settingsData.LEMONSQUEEZY_TOKEN)).
		SetBody(payload).
		SetResult(&response).
		Post("https://api.lemonsqueezy.com/v1/checkouts")

	if err != nil {
		return "", err
	}
	if res.IsError() {
		return "", fmt.Errorf("HTTP: %s, Res: %s", res.Status(), res.String())
	}

	/*
		res, _, err := l.client.Checkouts.Create(
			context.Background(),
			208461,
			id,
			&lemonsqueezy.CheckoutCreateAttributes{
				CheckoutData: lemonsqueezy.CheckoutCreateData{
					Email: config.User.Email,
					Name:  config.User.Name,
					Custom: map[string]any{
						"user_id": strconv.Itoa(int(config.User.ID)),
					},
				},
				CheckoutOptions: lemonsqueezy.CheckoutCreateOptions{
					Embed: utils.Bool(true),
				},
				TestMode: utils.Bool(true),
				ProductOptions: lemonsqueezy.CheckoutCreateProductOptions{
					RedirectURL:    config.BacksUrls.Success,
					ReceiptLinkURL: config.BacksUrls.Success,
				},
				ExpiresAt: expiresAt,
			},
		)
		if err != nil {
			return "", err
		}
	*/

	return response.Data.Attributes.URL, nil
}

func NewLemonSqueezePayments() payments.Payments {
	client := lemonsqueezy.New(lemonsqueezy.WithAPIKey(settingsData.LEMONSQUEEZY_TOKEN))

	return lemonSqueezePayments{
		client: client,
		http:   resty.New(),
	}
}

package controller

import "time"

type LSMeta struct {
	EventName  string         `json:"event_name"`
	CustomData map[string]any `json:"custom_data"`
}

type LSData struct {
	Type string `json:"type"`
	ID   string `json:"id"`
}

type LemonSqueezeWebhookEvent struct {
	Meta LSMeta         `json:"meta"`
	Data map[string]any `json:"data"`
}

type RemoveBenefitsPayload struct {
	IDUser   int64  `json:"idUser"`
	IDStudio int64  `json:"idStudio"`
	Code     string `json:"codePlan"`
}

type WatchVolumeSubscriptionPayload struct {
	IDSubscription int64     `json:"idSubscription" binding:"required"`
	IDPlan         int64     `json:"idPlan" binding:"required"`
	BillingDate    time.Time `json:"billingDate" binding:"required"`
}

package dto

import "time"

type CreatedActionData struct {
	CreatedAt time.Time `json:"createdAt"`
}

type PaymentEventDto struct {
	Event             string             `json:"event" binding:"required"`
	IDUser            int64              `json:"idUser"`
	Action            string             `json:"action" binding:"required"`
	IDStudio          int64              `json:"idStudio"`
	Identifier        string             `json:"identifier" binding:"required"`
	CreatedActionData *CreatedActionData `json:"createdActionData"`
}

package dto

type PaymentEventDto struct {
	Event      string `json:"event" binding:"required"`
	IDUser     int64  `json:"idUser"`
	IDStudio   int64  `json:"idStudio"`
	Identifier string `json:"identifier" binding:"required"`
}

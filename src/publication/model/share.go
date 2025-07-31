package model

import "time"

type Share struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"idUser"`
	IDPost    int64     `json:"idPost"`
	CreatedAt time.Time `json:"createdAt"`
}

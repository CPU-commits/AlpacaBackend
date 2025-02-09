package model

import "time"

type Like struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"idUser"`
	IDProfile int64     `json:"idProfile"`
	IDPost    int64     `json:"idPost"`
	CreatedAt time.Time `json:"createdAt"`
}

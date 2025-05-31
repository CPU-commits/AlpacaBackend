package model

import "time"

type Follow struct {
	ID        int64     `json:"id"`
	IDUser    int64     `json:"id_user"`
	IDProfile int64     `json:"id_profile"`
	CreatedAt time.Time `json:"created_at"`
}

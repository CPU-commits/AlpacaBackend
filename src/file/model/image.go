package model

import "time"

type Image struct {
	ID        int64     `json:"id"`
	Key       string    `json:"key"`
	Name      string    `json:"name"`
	MimeType  string    `json:"mime"`
	CreatedAt time.Time `json:"createdAt"`
}

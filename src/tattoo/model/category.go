package model

import "time"

type Category struct {
	ID          int64     `json:"id"`
	Name        string    `json:"name"`
	State       bool      `json:"state"`
	Description string    `json:"description"`
	Slug        string    `json:"slug"`
	CreatedAt   time.Time `json:"createdAt"`
}

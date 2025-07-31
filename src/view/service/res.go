package service

import "time"

type StatView struct {
	Day   time.Time `json:"day"`
	Views int64     `json:"views"`
}

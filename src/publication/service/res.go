package service

import "time"

type Stat struct {
	Day   time.Time `json:"day"`
	Count int64     `json:"count"`
}

type Metric struct {
	Count int64  `json:"count"`
	Stats []Stat `json:"stats"`
}

type Metrics struct {
	Views  Metric `json:"views"`
	Shares Metric `json:"shares"`
	Likes  Metric `json:"likes"`
}

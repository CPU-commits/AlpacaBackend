package model

import "time"

type View struct {
	ID        int64
	IDPost    int64
	IDProfile int64
	IDUser    int64
	IDStudio  int64
	IDLink    int64
	Country   string
	Continent string
	City      string
	Region    string
	TimeZone  string
	CreatedAt time.Time
}

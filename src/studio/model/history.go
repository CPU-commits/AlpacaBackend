package model

import "time"

type PeopleHistory struct {
	ID        int64
	IDUser    int64
	IDStudio  int64
	RemovedAt *time.Time
	CreatedAt time.Time
}

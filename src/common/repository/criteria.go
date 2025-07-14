package repository

import "time"

type CriteriaTime struct {
	EQ  time.Time
	GTE time.Time
	LTE time.Time
	LT  time.Time
	GT  time.Time
}

type CriteriaNull[T any] struct {
	EQ T
}

type CriteriaString struct {
	EQ        *string
	IContains *string
}

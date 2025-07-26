package service

import "time"

type PublicationsParams struct {
	IDStudio int64
	Username string
	Q        string
	FromDate time.Time
	ToDate   time.Time
}

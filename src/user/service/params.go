package service

import "time"

type MetricsParams struct {
	From            time.Time
	To              time.Time
	FromComparative time.Time
	ToComparative   time.Time
	Geo             string
}

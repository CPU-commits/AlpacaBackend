package service

import "time"

type AppointmentParams struct {
	Page            int
	FromDate        time.Time
	ToDate          time.Time
	Paginated       bool
	Statuses        []string
	AllAppointments bool
	IDStudio        int64
}

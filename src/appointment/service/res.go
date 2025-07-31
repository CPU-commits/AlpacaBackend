package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"

type AppointmentMetric struct {
	Count  int64                   `json:"count"`
	Status model.AppointmentStatus `json:"status"`
}

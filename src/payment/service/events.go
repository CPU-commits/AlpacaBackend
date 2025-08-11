package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

const (
	REMOVE_BENEFITS_USER_FROM_PLAN bus.EventName = "payment.remove_benefits"
	WATCH_VOLUME_PLAN              bus.EventName = "payment.watch_volume"
)

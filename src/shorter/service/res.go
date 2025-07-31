package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"

type MetricByTime struct {
	Stats []service.StatView `json:"stats"`
	Count int64              `json:"count"`
}

type Metrics struct {
	ByTime            MetricByTime `json:"byTime"`
	ByTimeComparative MetricByTime `json:"byTimeComparative"`
}

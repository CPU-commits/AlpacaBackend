package service

import (
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

type Metadata struct {
	Limit int
	Total int
}

type LocationMetric struct {
	Location string `json:"location"`
	Value    int64  `json:"value"`
}

type MetricsLocation struct {
	Locations []LocationMetric `json:"locations"`
	Count     int64            `json:"count"`
}

type MetricsTimeline struct {
	Stats []service.StatView `json:"stats"`
	Count int64              `json:"count"`
}

type MetricsViews struct {
	ByLocation          MetricsLocation `json:"byLocation"`
	Timeline            MetricsTimeline `json:"byTime"`
	TimelineComparative MetricsTimeline `json:"byTimeComparative"`
}

type MetricsFollowsTimeline struct {
	Stats []userService.StatFollow `json:"stats"`
	Count int64                    `json:"count"`
}

type MetricsFollows struct {
	Timeline            MetricsFollowsTimeline `json:"byTime"`
	TimelineComparative MetricsFollowsTimeline `json:"byTimeComparative"`
}

type Metrics struct {
	Views   MetricsViews   `json:"views"`
	Follows MetricsFollows `json:"follows"`
}

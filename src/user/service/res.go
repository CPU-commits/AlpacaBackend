package service

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

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

type StatFollow struct {
	Follows int64     `json:"follows"`
	Day     time.Time `json:"day"`
}

type MetricsViews struct {
	ByLocation          MetricsLocation `json:"byLocation"`
	Timeline            MetricsTimeline `json:"byTime"`
	TimelineComparative MetricsTimeline `json:"byTimeComparative"`
}

type MetricsFollowsTimeline struct {
	Stats []StatFollow `json:"stats"`
	Count int64        `json:"count"`
}

type MetricsFollows struct {
	Timeline            MetricsFollowsTimeline `json:"byTime"`
	TimelineComparative MetricsFollowsTimeline `json:"byTimeComparative"`
}

type Metrics struct {
	Views   MetricsViews   `json:"views"`
	Follows MetricsFollows `json:"follows"`
}

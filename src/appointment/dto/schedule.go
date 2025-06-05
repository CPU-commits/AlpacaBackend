package dto

import (
	"time"
)

type ScheduleAppointmentDto struct {
	ScheduledAt string `json:"scheduledAt" binding:"required"`
	FinishedAt  string `json:"finishedAt"`
}

func (sa *ScheduleAppointmentDto) ToTimes() (time.Time, time.Time, error) {
	scheduledAt, err := time.Parse(time.RFC3339, sa.ScheduledAt)
	if err != nil {
		return time.Time{}, time.Time{}, err
	}
	var finishedAt time.Time
	if sa.FinishedAt != "" {
		finishedAt, err = time.Parse(time.RFC3339, sa.FinishedAt)
		if err != nil {
			return time.Time{}, time.Time{}, err
		}
	}

	return scheduledAt, finishedAt, nil
}

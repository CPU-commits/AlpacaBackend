package calendar

import "time"

type EventID string

type Event struct {
	ID                 EventID
	Start              time.Time
	End                time.Time
	TimeZone           string
	Summary            string
	Location           string
	ParticipantsEmails []string
}

type ICalendar interface {
	Send(event Event) error
	GenerateEventID() EventID
	Update(event Event) error
	Delete(eventId EventID) error
}

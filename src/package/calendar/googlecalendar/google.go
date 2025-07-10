package googlecalendar

import (
	"encoding/base32"
	"fmt"
	"strings"
	"time"

	icalendar "github.com/CPU-commits/Template_Go-EventDriven/src/package/calendar"
	"github.com/google/uuid"
	"google.golang.org/api/calendar/v3"
)

type googleCalendar struct{}

func (googleCalendar) GenerateEventID() icalendar.EventID {
	u := uuid.New()

	encoder := base32.HexEncoding.WithPadding(base32.NoPadding)
	encoded := encoder.EncodeToString(u[:])

	return icalendar.EventID(strings.ToLower(encoded))
}

func (googleCalendar) makeEvent(event icalendar.Event) *calendar.Event {
	var attendees []*calendar.EventAttendee
	for _, email := range event.ParticipantsEmails {
		attendees = append(attendees, &calendar.EventAttendee{
			Email: email,
		})
	}

	return &calendar.Event{
		Summary:  event.Summary,
		Location: event.Location,
		Start: &calendar.EventDateTime{
			DateTime: event.Start.Format(time.RFC3339),
			TimeZone: event.TimeZone,
		},
		End: &calendar.EventDateTime{
			DateTime: event.End.Format(time.RFC3339),
			TimeZone: event.TimeZone,
		},
		Attendees: attendees,
	}
}

func (gc googleCalendar) Send(event icalendar.Event) error {
	calendarEvent := gc.makeEvent(event)
	calendarEvent.Id = string(event.ID)

	call := srv.Events.Insert("primary", calendarEvent)
	call = call.SendUpdates("all")

	x, err := call.Do()
	fmt.Printf("x.Id: %v\n", x.Id)
	return err
}

func (gc googleCalendar) Update(event icalendar.Event) error {
	calendarEvent := gc.makeEvent(event)

	call := srv.Events.Patch("primary", string(event.ID), calendarEvent)
	call = call.SendUpdates("all")

	_, err := call.Do()
	return err
}

func (gc googleCalendar) Delete(eventId icalendar.EventID) error {
	return srv.Events.Delete("primary", string(eventId)).SendUpdates("all").Do()
}

func NewGoogleCalendar() icalendar.ICalendar {
	return googleCalendar{}
}

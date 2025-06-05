package service

import "errors"

var (
	ErrUserIsNotTattooArtists           = errors.New("err: user is not a tattoo artist")
	ErrUserHasNoAccessToAppointment     = errors.New("err: no appointment access")
	ErrStatusIsNotCreated               = errors.New("err: appointment status is not created")
	ErrScheduleDateMustBeAfterNow       = errors.New("err: schedule date must be after now")
	ErrScheduleDateMusteBeAfterFinished = errors.New("err: schedule date must be after finished")
	ErrScheduleIsBussy                  = errors.New("err: schedule is bussy")
	ErrAppointmentIsFinished            = errors.New("err: appointment is finished")
)

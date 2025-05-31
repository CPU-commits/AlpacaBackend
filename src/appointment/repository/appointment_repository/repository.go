package appointment_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"

type AppointmentRepository interface {
	Insert(appointment *model.Appointment) (*model.Appointment, error)
}

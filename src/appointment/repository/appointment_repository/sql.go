package appointment_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/null/v8"
	"github.com/volatiletech/sqlboiler/v4/boil"
)

type appointmentRepositorySql struct {
	db *sql.DB
}

func (appointmentRepositorySql) sqlAppointmentToModel(
	sqlAppointment *models.Appointment,
) *model.Appointment {
	appointment := &model.Appointment{}

	return appointment
}

func (sqlAR appointmentRepositorySql) Insert(
	appointment *model.Appointment,
) (*model.Appointment, error) {
	tx, err := sqlAR.db.BeginTx(context.Background(), nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	sqlAppointment := models.Appointment{
		IDUser:         appointment.IDUser,
		IDTattooArtist: appointment.IDTattooArtist,
		Status:         string(appointment.Status),
		Area:           null.NewString(string(appointment.Area), appointment.Area != ""),
		Color:          null.NewString(string(appointment.Color), appointment.Color != ""),
		Description:    appointment.Description,
		HasIdea:        *appointment.HasIdea,
		Height:         null.Float64From(float64(appointment.Height)),
		Width:          null.Float64From(float64(appointment.Width)),
		Phone:          null.StringFrom(appointment.Phone),
	}
	if err := sqlAppointment.Insert(context.Background(), tx, boil.Infer()); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}
	// Upload images
	for _, image := range appointment.Images {
		sqlImage := models.Image{
			Key:      image.Key,
			Name:     image.Name,
			MimeType: image.MimeType,
		}

		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
		sqlImageAppointment := models.AppointmentImage{
			IDAppointment: sqlAppointment.ID,
			IDImage:       sqlImage.ID,
		}
		if err := sqlImageAppointment.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}

	return sqlAR.sqlAppointmentToModel(&sqlAppointment), nil
}

func NewSqlAppointmentRepository() AppointmentRepository {
	return appointmentRepositorySql{
		db: db.DB,
	}
}

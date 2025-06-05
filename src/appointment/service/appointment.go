package service

import (
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/appointment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type AppointmentService struct {
	fileService           fileService.FileService
	appointmentRepository appointment_repository.AppointmentRepository
	userService           service.UserService
}

var appointmentService *AppointmentService

func (appointmentService *AppointmentService) hasAccessToAppointment(
	idAppointment,
	idUser int64,
	isTattooArtist bool,
) error {
	criteria := appointment_repository.Criteria{
		ID: idAppointment,
	}
	if isTattooArtist {
		criteria.IDTattooArtist = idUser
	} else {
		criteria.IDUser = idUser
	}

	hasAccess, err := appointmentService.appointmentRepository.Exists(&criteria)
	if err != nil {
		return err
	}
	if !hasAccess {
		return ErrUserHasNoAccessToAppointment
	}

	return nil
}

func (appointmentService *AppointmentService) GetAppointments(
	idUser int64,
	isArtist bool,
	params AppointmentParams,
) ([]model.Appointment, int64, error) {
	criteria := &appointment_repository.Criteria{}
	load := appointment_repository.LoadOpts{
		Images: true,
		Profile: &profile_repository.SelectOpts{
			ID:     utils.Bool(true),
			Avatar: utils.Bool(true),
			IDUser: utils.Bool(true),
		},
		ProfileAvatar: true,
	}
	if isArtist {
		criteria.IDTattooArtist = idUser
		load.User = &user_repository.SelectOpts{
			Name:     utils.Bool(true),
			Username: utils.Bool(true),
			ID:       utils.Bool(true),
			Email:    utils.Bool(true),
		}
	} else {
		criteria.IDUser = idUser
		load.TattooArtist = &user_repository.SelectOpts{
			Name:     utils.Bool(true),
			Username: utils.Bool(true),
			ID:       utils.Bool(true),
		}
	}

	opts := appointment_repository.NewFindOptions().
		Limit(10).
		Skip(int64(params.Page) * 10).
		Sort(appointment_repository.Sort{
			CreatedAt: "DESC",
		}).
		Load(load)

	appointments, err := appointmentService.appointmentRepository.Find(
		criteria,
		opts,
	)
	if err != nil {
		return nil, 0, err
	}
	// Total
	total, err := appointmentService.appointmentRepository.Count(criteria)
	if err != nil {
		return nil, 0, err
	}

	return appointments, total, nil
}

func (appointmentService *AppointmentService) CancelAppointment(
	idAppointment,
	idTattooArtist int64,
) error {
	if err := appointmentService.hasAccessToAppointment(
		idAppointment,
		idTattooArtist,
		true,
	); err != nil {
		return err
	}
	appointmentIsFinished, err := appointmentService.appointmentRepository.Exists(
		&appointment_repository.Criteria{
			ID:            idAppointment,
			FinishedAtLTE: time.Now(),
		},
	)
	if err != nil {
		return err
	}
	if appointmentIsFinished {
		return ErrAppointmentIsFinished
	}

	return appointmentService.appointmentRepository.Update(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		&appointment_repository.UpdateData{
			Status: model.STATUS_CANCELED,
		},
	)
}

func (appointmentService *AppointmentService) ScheduleAppointment(
	idAppointment,
	idTattooArtist int64,
	scheduleAppointment dto.ScheduleAppointmentDto,
) error {
	scheduledAt, finishedAt, err := scheduleAppointment.ToTimes()
	if err != nil {
		return err
	}
	if time.Now().After(scheduledAt) {
		return ErrScheduleDateMustBeAfterNow
	}
	if !finishedAt.IsZero() && scheduledAt.After(finishedAt) {
		return ErrScheduleDateMusteBeAfterFinished
	}

	if err := appointmentService.hasAccessToAppointment(
		idAppointment,
		idTattooArtist,
		true,
	); err != nil {
		return err
	}
	isAppointmentCreated, err := appointmentService.appointmentRepository.Exists(
		&appointment_repository.Criteria{
			ID: idAppointment,
			Or: []appointment_repository.Criteria{
				{Status: model.STATUS_CREATED},
				{Status: model.STATUS_SCHEDULED},
			},
		},
	)
	if err != nil {
		return err
	}
	if !isAppointmentCreated {
		return ErrStatusIsNotCreated
	}
	// Scheduled?
	isBussy, err := appointmentService.appointmentRepository.Exists(
		&appointment_repository.Criteria{
			IDNE:           idAppointment,
			ScheduledAtGTE: scheduledAt,
			FinishedAtLTE:  finishedAt,
		},
	)
	if err != nil {
		return err
	}
	if isBussy {
		return ErrScheduleIsBussy
	}

	var duration float64
	if !finishedAt.IsZero() {
		duration = finishedAt.Sub(scheduledAt).Seconds()
	} else {
		duration = 3600 // 1 hora por defecto
		finishedAt = scheduledAt.Add(1 * time.Hour)
	}

	return appointmentService.appointmentRepository.Update(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		&appointment_repository.UpdateData{
			Status:      model.STATUS_SCHEDULED,
			ScheduledAt: scheduledAt,
			FinishedAt:  finishedAt,
			Duration:    duration,
		},
	)
}

func (appointmentService *AppointmentService) RequestAppointment(
	appointmentDto *dto.AppointmentDto,
	idUser int64,
) error {
	isTattooArtist, err := appointmentService.userService.UserIsTattooArtist(
		appointmentDto.IDTattooArtist,
	)
	if err != nil {
		return err
	}
	if !isTattooArtist {
		return ErrUserIsNotTattooArtists
	}

	images, err := appointmentService.fileService.UploadImages(
		appointmentDto.Images,
		fmt.Sprintf("appointments/%d", appointmentDto.IDTattooArtist),
	)
	if err != nil {
		return err
	}
	appointment := appointmentDto.ToModel(idUser)
	appointment.Images = images
	appointment.IDUser = idUser
	fmt.Printf("appointment.IDUser: %v\n", appointment.IDUser)

	_, err = appointmentService.appointmentRepository.Insert(
		appointment,
	)

	return err
}

func NewAppointmentService(
	fileService fileService.FileService,
	appointmentRepository appointment_repository.AppointmentRepository,
	userService service.UserService,
) *AppointmentService {
	if appointmentService == nil {
		appointmentService = &AppointmentService{
			fileService:           fileService,
			appointmentRepository: appointmentRepository,
			userService:           userService,
		}
	}

	return appointmentService
}

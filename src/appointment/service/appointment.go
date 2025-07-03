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
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/calendar"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

type AppointmentService struct {
	fileService           fileService.FileService
	appointmentRepository appointment_repository.AppointmentRepository
	userService           service.UserService
	calendar              calendar.ICalendar
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

	err = appointmentService.appointmentRepository.Update(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		&appointment_repository.UpdateData{
			Status: model.STATUS_CANCELED,
		},
	)
	go func() {
		opts := appointment_repository.NewFindOneOptions().Select(
			appointment_repository.SelectOpts{
				IDCalendar: utils.Bool(true),
			},
		)

		appointment, err := appointmentService.appointmentRepository.FindOne(
			&appointment_repository.Criteria{
				ID: idAppointment,
			},
			opts,
		)
		if err != nil {
			return
		}

		appointmentService.calendar.Delete(calendar.EventID(appointment.IDCalendar))
	}()

	return err
}

func (appointmentService *AppointmentService) ScheduleAppointment(
	idAppointment,
	idTattooArtist int64,
	scheduleAppointment dto.ScheduleAppointmentDto,
	timezone string,
	localizer *i18n.Localizer,
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
	var duration float64
	if !finishedAt.IsZero() {
		duration = finishedAt.Sub(scheduledAt).Seconds()
	} else {
		duration = 3600 // 1 hora por defecto
		finishedAt = scheduledAt.Add(1 * time.Hour)
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
			IDTattooArtist: idTattooArtist,
		},
	)
	if err != nil {
		return err
	}
	if isBussy {
		return ErrScheduleIsBussy
	}

	isScheduled, err := appointmentService.appointmentRepository.Exists(&appointment_repository.Criteria{
		ID:     idAppointment,
		Status: model.STATUS_SCHEDULED,
	})
	if err != nil {
		return err
	}
	var idCalendarStr string
	if !isScheduled {
		idCalendar := appointmentService.calendar.GenerateEventID()
		idCalendarStr = string(idCalendar)
	}

	if err = appointmentService.appointmentRepository.Update(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		&appointment_repository.UpdateData{
			Status:      model.STATUS_SCHEDULED,
			ScheduledAt: scheduledAt,
			FinishedAt:  finishedAt,
			Duration:    duration,
			IDCalendar:  idCalendarStr,
		},
	); err != nil {
		return err
	}

	var timezoneCalendar string = timezone
	if timezoneCalendar == "" {
		timezoneCalendar = "America/Santiago"
	}

	go func() {
		opts := appointment_repository.NewFindOneOptions().Select(appointment_repository.SelectOpts{
			IDUser:     utils.Bool(true),
			IDCalendar: utils.Bool(true),
		})

		appointment, err := appointmentService.appointmentRepository.FindOne(
			&appointment_repository.Criteria{
				ID: idAppointment,
			},
			opts,
		)
		if err != nil {
			return
		}
		if !isScheduled {
			// Get users
			tattooArtist, err := appointmentService.userService.GetUserById(idTattooArtist)
			if err != nil {
				return
			}
			userEmail, err := appointmentService.userService.GetEmailUserById(appointment.IDUser)
			if err != nil {
				return
			}
			appointmentService.calendar.Send(calendar.Event{
				ID:       calendar.EventID(idCalendarStr),
				Start:    scheduledAt,
				End:      finishedAt,
				TimeZone: timezoneCalendar,
				Summary: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "appointment.scheduled",
					TemplateData: map[string]interface{}{
						"TattooArtist": tattooArtist.Name,
					},
				}),
				Location:           tattooArtist.Location,
				ParticipantsEmails: []string{tattooArtist.Email, userEmail},
			})
		} else {
			appointmentService.calendar.Update(calendar.Event{
				Start:    scheduledAt,
				End:      finishedAt,
				TimeZone: timezoneCalendar,
				ID:       calendar.EventID(appointment.IDCalendar),
			})
		}
	}()

	return nil
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
	iCalendar calendar.ICalendar,
) *AppointmentService {
	if appointmentService == nil {
		appointmentService = &AppointmentService{
			fileService:           fileService,
			appointmentRepository: appointmentRepository,
			userService:           userService,
			calendar:              iCalendar,
		}
	}

	return appointmentService
}

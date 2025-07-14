package service

import (
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/appointment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/review_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/calendar"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

type AppointmentService struct {
	fileService           fileService.FileService
	appointmentRepository appointment_repository.AppointmentRepository
	userService           service.UserService
	calendar              calendar.ICalendar
	reviewRepository      review_repository.ReviewRepository
	profileService        userService.ProfileService
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
		Review:        true,
	}
	if params.AllAppointments && isArtist {
		load.User = &user_repository.SelectOpts{
			Name:     utils.Bool(true),
			Username: utils.Bool(true),
			ID:       utils.Bool(true),
			Email:    utils.Bool(true),
		}
		load.TattooArtist = &user_repository.SelectOpts{
			Name:     utils.Bool(true),
			Username: utils.Bool(true),
			ID:       utils.Bool(true),
		}
	} else if isArtist {
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
	if !params.FromDate.IsZero() {
		criteria.ScheduledAtGTE = params.FromDate
	}
	if !params.ToDate.IsZero() {
		criteria.FinishedAt = &repository.CriteriaTime{
			LTE: params.ToDate,
		}
	}
	if params.Statuses != nil {
		or := []appointment_repository.Criteria{}

		for _, status := range params.Statuses {
			if params.AllAppointments && isArtist {
				or = append(or, appointment_repository.Criteria{
					Status: model.AppointmentStatus(status),
					IDUser: idUser,
				})
				or = append(or, appointment_repository.Criteria{
					Status:         model.AppointmentStatus(status),
					IDTattooArtist: idUser,
				})
			} else {
				or = append(or, appointment_repository.Criteria{
					Status: model.AppointmentStatus(status),
				})
			}
		}
		criteria.Or = or
	} else if params.AllAppointments && isArtist {
		or := []appointment_repository.Criteria{}

		or = append(or, appointment_repository.Criteria{
			IDUser: idUser,
		})
		or = append(or, appointment_repository.Criteria{
			IDTattooArtist: idUser,
		})
		criteria.Or = or
	}

	opts := appointment_repository.NewFindOptions().
		Sort(appointment_repository.Sort{
			CreatedAt: "DESC",
		}).
		Load(load)
	if params.Paginated {
		opts = opts.
			Limit(10).
			Skip(int64(params.Page) * 10)
	}

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

func (appointmentService *AppointmentService) GetPendingAppointments(
	idTattooArtist int64,
) (int64, error) {
	return appointmentService.appointmentRepository.Count(&appointment_repository.Criteria{
		IDTattooArtist: idTattooArtist,
		Status:         model.STATUS_CREATED,
	})
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
			ID: idAppointment,
			FinishedAt: &repository.CriteriaTime{
				LTE: time.Now(),
			},
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
			FinishedAt: &repository.CriteriaTime{
				LTE: finishedAt,
			},
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

func (appointmentService *AppointmentService) appointmentIsFinished(idAppointment int64) (bool, error) {
	return appointmentService.appointmentRepository.Exists(&appointment_repository.Criteria{
		FinishedAt: &repository.CriteriaTime{
			LT: time.Now(),
		},
		ID: idAppointment,
	})
}

func (appointmentService *AppointmentService) ReviewAppointment(
	review dto.ReviewDTO,
	idUser,
	idAppointment int64,
) error {
	if err := appointmentService.hasAccessToAppointment(
		idAppointment,
		idUser,
		false,
	); err != nil {
		return err
	}
	existsReview, err := appointmentService.reviewRepository.Exists(&review_repository.Criteria{
		IDAppointment: idAppointment,
	})
	if err != nil {
		return err
	}
	if existsReview {
		return ErrReviewExists
	}
	// Is finished ?
	isFinished, err := appointmentService.appointmentIsFinished(
		idAppointment,
	)
	if err != nil {
		return err
	}
	if !isFinished {
		return ErrAppointmentIsNotFinished
	}

	appointment, err := appointmentService.appointmentRepository.FindOne(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		appointment_repository.NewFindOneOptions().Select(appointment_repository.SelectOpts{
			IDTattooArtist: utils.Bool(true),
		}),
	)
	if err != nil {
		return err
	}
	idProfile, err := appointmentService.profileService.GetProfileIDFromIDUser(appointment.IDTattooArtist)
	if err != nil {
		return err
	}

	if err = appointmentService.reviewRepository.InsertOne(review.ToModel(
		idUser,
		idProfile,
		idAppointment,
	)); err != nil {
		return err
	}

	return appointmentService.appointmentRepository.Update(
		&appointment_repository.Criteria{
			ID: idAppointment,
		},
		&appointment_repository.UpdateData{
			Status: model.STATUS_REVIEWED,
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
	iCalendar calendar.ICalendar,
	reviewRepository review_repository.ReviewRepository,
	profileService userService.ProfileService,
) *AppointmentService {
	if appointmentService == nil {
		appointmentService = &AppointmentService{
			fileService:           fileService,
			appointmentRepository: appointmentRepository,
			userService:           userService,
			calendar:              iCalendar,
			reviewRepository:      reviewRepository,
			profileService:        profileService,
		}
	}

	return appointmentService
}

package service

import (
	"fmt"

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
		},
	}
	if isArtist {
		criteria.IDTattooArtist = idUser
		load.TattooArtist = &user_repository.SelectOpts{
			Name:     utils.Bool(true),
			Username: utils.Bool(true),
			ID:       utils.Bool(true),
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

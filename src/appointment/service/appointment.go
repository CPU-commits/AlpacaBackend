package service

import (
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/appointment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
)

type AppointmentService struct {
	fileService           fileService.FileService
	appointmentRepository appointment_repository.AppointmentRepository
	userService           service.UserService
}

var appointmentService *AppointmentService

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

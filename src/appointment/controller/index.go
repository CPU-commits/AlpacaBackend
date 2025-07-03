package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/appointment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	authServices "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileServices "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/calendar/googlecalendar"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
)

// Repositories
var (
	appointmentRepository = appointment_repository.NewSqlAppointmentRepository()
	userRepository        = user_repository.NewSQLUserRepository(db.DB)
	roleRepository        = role_repository.NewSQLRoleRepository()
)

// calendar
var googleCalendar = googlecalendar.NewGoogleCalendar()

// Store
var fileStore = cloudinary_store.NewCloudinaryImageStore()

// Services
var (
	fileService = fileServices.NewFileService(
		fileStore,
	)
	userService = authServices.NewUserService(
		userRepository,
		roleRepository,
	)
	appointmentService = service.NewAppointmentService(
		*fileService,
		appointmentRepository,
		*userService,
		googleCalendar,
	)
)

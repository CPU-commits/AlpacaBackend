package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/appointment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/repository/review_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileServices "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/calendar/googlecalendar"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	ipipinfo "github.com/CPU-commits/Template_Go-EventDriven/src/package/ip/ip_ipinfo"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_histories_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/design_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
	viewServices "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"

	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

// Repositories
var (
	appointmentRepository     = appointment_repository.NewSqlAppointmentRepository()
	userRepository            = user_repository.NewSQLUserRepository(db.DB)
	roleRepository            = role_repository.NewSQLRoleRepository()
	reviewRepository          = review_repository.NewSqlReviewRepository()
	profileRepository         = profile_repository.NewSqlProfileRepository(db.DB)
	followRepository          = follow_repository.NewSqlFollowRepository()
	publicationRDRepository   = publication_repository.NewRdPublicationRepository()
	peopleStudioRepository    = people_studio_repository.NewSqlPeopleStudioRepository()
	studioRepository          = studio_repository.NewSqlStudioRepository()
	viewRepository            = view_repository.NewSqlViewRepository()
	temporalViewRepository    = temporal_view_repository.NewRdTemportalViewRepository()
	peopleHistoriesRepository = people_histories_repository.NewSqlPeopleHistoriesRepository()
	designRepository          = design_repository.NewSqlDesignRepository(db.DB)
)

// UID Generator
var uidGenerator = nanoid.NewNanoIDGenerator()

// calendar
var googleCalendar = googlecalendar.NewGoogleCalendar()

// IP
var ipInfo = ipipinfo.NewIPIpInfo()

// Store
var fileStore = cloudinary_store.NewCloudinaryImageStore()

// Services
var (
	fileService = fileServices.NewFileService(
		fileStore,
	)
	viewService = viewServices.NewViewService(
		viewRepository,
		temporalViewRepository,
		ipInfo,
	)
)

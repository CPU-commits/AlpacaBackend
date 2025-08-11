package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	ipipinfo "github.com/CPU-commits/Template_Go-EventDriven/src/package/ip/ip_ipinfo"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_histories_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
	viewServices "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

// Store
var imageStore = cloudinary_store.NewCloudinaryImageStore()

// UID Generate
var uidGenerator = nanoid.NewNanoIDGenerator()

// IP
var ipConfig = ipipinfo.NewIPIpInfo()

// Repositories
var (
	studioRepository          = studio_repository.NewSqlStudioRepository()
	authRepository            = auth_repository.NewSQLAuthRepository(db.DB)
	userRepository            = user_repository.NewSQLUserRepository(db.DB)
	studioAdminRepository     = people_studio_repository.NewSqlPeopleStudioRepository()
	roleRepository            = role_repository.NewSQLRoleRepository()
	followRepository          = follow_repository.NewSqlFollowRepository()
	viewRepository            = view_repository.NewSqlViewRepository()
	temporalViewRepository    = temporal_view_repository.NewRdTemportalViewRepository()
	peopleHistoriesRepository = people_histories_repository.NewSqlPeopleHistoriesRepository()
)

// Services
var (
	fileService   = service.NewFileService(imageStore)
	followService = userService.NewFollowService(
		followRepository,
	)
	viewService = viewServices.NewViewService(
		viewRepository,
		temporalViewRepository,
		ipConfig,
	)
)

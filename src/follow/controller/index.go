package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	ipipinfo "github.com/CPU-commits/Template_Go-EventDriven/src/package/ip/ip_ipinfo"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

// Store
var imageStore = cloudinary_store.NewCloudinaryImageStore()

// UID Generator
var uidGenerator = nanoid.NewNanoIDGenerator()

// IP
var ipInfo = ipipinfo.NewIPIpInfo()

// Repositories
var (
	followRepository  = follow_repository.NewSqlFollowRepository()
	profileRepository = profile_repository.NewSqlProfileRepository(
		db.DB,
	)
	userRepository = user_repository.NewSQLUserRepository(
		db.DB,
	)
	roleRepository         = role_repository.NewSQLRoleRepository()
	publicationRepository  = publication_repository.NewRdPublicationRepository()
	viewRepository         = view_repository.NewSqlViewRepository()
	temporalViewRepository = temporal_view_repository.NewRdTemportalViewRepository()
)

// Services
var (
	viewService = service.NewViewService(
		viewRepository,
		temporalViewRepository,
		ipInfo,
	)
)

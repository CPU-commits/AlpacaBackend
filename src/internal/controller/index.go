package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	ipipinfo "github.com/CPU-commits/Template_Go-EventDriven/src/package/ip/ip_ipinfo"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/like_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/share_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_histories_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
	view_service "github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

var imageStore = cloudinary_store.NewCloudinaryImageStore()

// UID Generator
var uidGenerator = nanoid.NewNanoIDGenerator()

// IP
var ipInfo = ipipinfo.NewIPIpInfo()

// Repositories
var (
	profileRepository         = profile_repository.NewSqlProfileRepository(db.DB)
	userRepository            = user_repository.NewSQLUserRepository(db.DB)
	tattooRepository          = tattoo_repository.NewSqlTattooRepository(db.DB)
	likeRepository            = like_repository.NewSqlLikeRepository(db.DB)
	publicationRepository     = publication_repository.NewSqlPublicationRepository()
	publicationTSRepository   = publication_repository.NewTsPublicationRepository()
	roleRepository            = role_repository.NewSQLRoleRepository()
	publicationRDRepository   = publication_repository.NewRdPublicationRepository()
	followRepository          = follow_repository.NewSqlFollowRepository()
	adminStudioRepository     = people_studio_repository.NewSqlPeopleStudioRepository()
	studioRepository          = studio_repository.NewSqlStudioRepository()
	viewRepository            = view_repository.NewSqlViewRepository()
	shareRepository           = share_repository.NewSqlShareRepository(db.DB)
	temporalViewRepository    = temporal_view_repository.NewRdTemportalViewRepository()
	tattooTSRepository        = tattoo_repository.NewTsTattooRepository()
	peopleHistoriesRepository = people_histories_repository.NewSqlPeopleHistoriesRepository()
)

// Services
var (
	fileService = file_service.NewFileService(
		imageStore,
	)
	viewService = view_service.NewViewService(
		viewRepository,
		temporalViewRepository,
		ipInfo,
	)
)

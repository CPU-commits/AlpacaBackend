package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/like_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

var imageStore = cloudinary_store.NewCloudinaryImageStore()

// Repositories
var (
	profileRepository       = profile_repository.NewSqlProfileRepository(db.DB)
	userRepository          = user_repository.NewSQLUserRepository(db.DB)
	tattooRepository        = tattoo_repository.NewSqlTattooRepository(db.DB)
	likeRepository          = like_repository.NewSqlLikeRepository(db.DB)
	publicationRepository   = publication_repository.NewSqlPublicationRepository()
	publicationTSRepository = publication_repository.NewTsPublicationRepository()
	roleRepository          = role_repository.NewSQLRoleRepository()
	publicationRDRepository = publication_repository.NewRdPublicationRepository()
	followRepository        = follow_repository.NewSqlFollowRepository(db.DB)
	adminStudioRepository   = people_studio_repository.NewSqlPeopleStudioRepository()
	studioRepository        = studio_repository.NewSqlStudioRepository()
)

// Services
var (
	fileService = file_service.NewFileService(
		imageStore,
	)
)

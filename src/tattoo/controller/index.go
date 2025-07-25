package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	file_service "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/design_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

var imageStore = cloudinary_store.NewCloudinaryImageStore()

// UID Generator
var uidGenerator = nanoid.NewNanoIDGenerator()

// Repositories
var (
	profileRepository       = profile_repository.NewSqlProfileRepository(db.DB)
	tattooRepository        = tattoo_repository.NewSqlTattooRepository(db.DB)
	userRepository          = user_repository.NewSQLUserRepository(db.DB)
	roleRepository          = role_repository.NewSQLRoleRepository()
	followRepository        = follow_repository.NewSqlFollowRepository()
	publicationRDRepository = publication_repository.NewRdPublicationRepository()
	tattooTSRepository      = tattoo_repository.NewTsTattooRepository()
	designRepository        = design_repository.NewSqlDesignRepository(db.DB)
)

// Services
var (
	fileService = file_service.NewFileService(imageStore)
)

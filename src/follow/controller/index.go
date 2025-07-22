package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
)

// Store
var imageStore = cloudinary_store.NewCloudinaryImageStore()

// Repositories
var (
	followRepository  = follow_repository.NewSqlFollowRepository()
	profileRepository = profile_repository.NewSqlProfileRepository(
		db.DB,
	)
	userRepository = user_repository.NewSQLUserRepository(
		db.DB,
	)
	roleRepository        = role_repository.NewSQLRoleRepository()
	publicationRepository = publication_repository.NewRdPublicationRepository()
)

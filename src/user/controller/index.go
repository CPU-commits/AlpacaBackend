package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
)

// Repositories
var (
	profileRepository = profile_repository.NewSqlProfileRepository(db.DB)
	userRepository    = user_repository.NewSQLUserRepository(db.DB)
)

// Services
var (
	userService = authService.NewUserService(
		userRepository,
	)
	profileService = service.NewProfileService(
		profileRepository,
		*userService,
		cloudinary_store.NewCloudinaryImageStore(),
	)
)

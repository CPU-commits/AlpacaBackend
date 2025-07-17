package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
)

// Store
var imageStore = cloudinary_store.NewCloudinaryImageStore()

// Repositories
var (
	studioRepository      = studio_repository.NewSqlStudioRepository()
	authRepository        = auth_repository.NewSQLAuthRepository(db.DB)
	userRepository        = user_repository.NewSQLUserRepository(db.DB)
	studioAdminRepository = people_studio_repository.NewSqlPeopleStudioRepository()
	roleRepository        = role_repository.NewSQLRoleRepository()
)

// Services
var (
	fileService = service.NewFileService(imageStore)
)

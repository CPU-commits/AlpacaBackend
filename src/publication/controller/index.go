package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/like_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/category_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/repository/tattoo_repository"
	tattooServices "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
)

var imageStore = cloudinary_store.NewCloudinaryImageStore()

// Repositories
var (
	profileRepository     = profile_repository.NewSqlProfileRepository(db.DB)
	userRepository        = user_repository.NewSQLUserRepository(db.DB)
	tattooRepository      = tattoo_repository.NewSqlTattooRepository(db.DB)
	categoryRepository    = category_repository.NewSQLAccessRepository(db.DB)
	publicationRepository = publication_repository.NewSqlPublicationRepository(db.DB)
	likeRepository        = like_repository.NewSqlLikeRepository(db.DB)
)

// Services
var (
	userService = authService.NewUserService(
		userRepository,
	)
	profileService = userServices.NewProfileService(
		profileRepository,
		*userService,
		imageStore,
	)
	categoryService = tattooServices.NewCategoryService(
		categoryRepository,
	)
	tattooService = tattooServices.NewTattooService(
		imageStore,
		*profileService,
		tattooRepository,
		*categoryService,
	)
	publicationService = service.NewPublicationService(
		*tattooService,
		*categoryService,
		*profileService,
		imageStore,
		publicationRepository,
		likeRepository,
		tattooRepository,
	)
)

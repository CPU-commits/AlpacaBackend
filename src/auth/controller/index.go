package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/access_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/session_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/tokenpassword_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
)

// Generator
var generator = utils.NewGeneratorToken()

// Repositories
var (
	sqlAuthRepository          = auth_repository.NewSQLAuthRepository(db.DB)
	sqlSessionRepository       = session_repository.NewSQLSessionRepository(db.DB)
	sqlUserRepository          = user_repository.NewSQLUserRepository(db.DB)
	sqlAccessRepository        = access_repository.NewSQLAccessRepository(db.DB)
	sqlTokenPasswordRepository = tokenpassword_repository.NewSQLTokenPasswordRepository(db.DB)
)

// Events
const (
	INSERTED_USER bus.EventName = "user.token_created"
)

// Services
var (
	authService = service.NewAuthService(
		sqlAuthRepository,
		sqlUserRepository,
	)
	sessionService = service.NewSessionService(
		sqlSessionRepository,
		sqlAccessRepository,
		generator,
	)
	userService = service.NewUserService(
		sqlUserRepository,
	)
	tokenPasswordService = service.NewTokenPasswordService(
		sqlTokenPasswordRepository,
		generator,
	)
)

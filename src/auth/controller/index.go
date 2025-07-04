package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/access_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/auth_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/session_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/tokenpassword_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/token_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/jwt"
)

// Generator

// Repositories
var (
	sqlAuthRepository          = auth_repository.NewSQLAuthRepository(db.DB)
	sqlSessionRepository       = session_repository.NewSQLSessionRepository(db.DB)
	sqlUserRepository          = user_repository.NewSQLUserRepository(db.DB)
	sqlAccessRepository        = access_repository.NewSQLAccessRepository(db.DB)
	sqlTokenPasswordRepository = tokenpassword_repository.NewSQLTokenPasswordRepository(db.DB)
	sqlRoleRepository          = role_repository.NewSQLRoleRepository()
	tokenGenerator             = token_repository.NewGeneratorToken(jwt.JwtKey)
)

// Events
const (
	INSERTED_USER bus.EventName = "user.token_created"
)

// Services
var (
	sessionService = service.NewSessionService(
		sqlSessionRepository,
		sqlAccessRepository,
		tokenGenerator,
	)
	tokenPasswordService = service.NewTokenPasswordService(
		sqlTokenPasswordRepository,
		tokenGenerator,
	)
)

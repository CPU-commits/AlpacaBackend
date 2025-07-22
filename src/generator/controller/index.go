package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/code_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/token_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/jwt"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
)

// UID Generator
var uidGenerator = nanoid.NewNanoIDGenerator()

// Repositories
var (
	sqlRoleRepository  = role_repository.NewSQLRoleRepository()
	sqlUserRepository  = user_repository.NewSQLUserRepository(db.DB)
	sqlCodeRepository  = code_repository.NewSqlCodeRepository(db.DB)
	sqlTokenRepository = token_repository.NewSqlTokenRepository(db.DB)
	tokenGenerator     = token_repository.NewGeneratorToken(jwt.JwtKey)
	tokenRDRepository  = token_repository.NewRdTokenRepository()
)

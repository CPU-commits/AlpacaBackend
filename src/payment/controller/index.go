package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/payments/lemon"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/payment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/plan_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/subscription_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
)

var settingsData = settings.GetSettings()

// UID
var uidGenerator = nanoid.NewNanoIDGenerator()

// Repositories
var (
	subscriptionRepository = subscription_repository.NewSqlSubscriptionRepository()
	planRepository         = plan_repository.NewSqlPlanRepository()
	paymentRepository      = payment_repository.NewSqlPaymentRepository()
	userRepository         = user_repository.NewSQLUserRepository(db.DB)
	roleRepository         = role_repository.NewSQLRoleRepository()
	studioRepository       = studio_repository.NewSqlStudioRepository()
	peopleStudioRepository = people_studio_repository.NewSqlPeopleStudioRepository()
)

// Payments
var payments = lemon.NewLemonSqueezePayments()

// Services
var (
	paymentService = service.NewPaymentService(
		paymentRepository,
	)
)

package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	ipipinfo "github.com/CPU-commits/Template_Go-EventDriven/src/package/ip/ip_ipinfo"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid/nanoid"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/repository/link_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

// UID
var uidGenerator = nanoid.NewNanoIDGenerator()

// Repositories
var (
	linkRepository         = link_repository.NewSqlLinkRepository()
	viewRepository         = view_repository.NewSqlViewRepository()
	temporalViewRepository = temporal_view_repository.NewRdTemportalViewRepository()
	peopleStudioRepository = people_studio_repository.NewSqlPeopleStudioRepository()
	studioRepository       = studio_repository.NewSqlStudioRepository()
	userRepository         = user_repository.NewSQLUserRepository(db.DB)
	roleRepository         = role_repository.NewSQLRoleRepository()
)

// Settings
var settingsData = settings.GetSettings()

// IP
var ipConfig = ipipinfo.NewIPIpInfo()

// Services
var (
	viewService = service.NewViewService(
		viewRepository,
		temporalViewRepository,
		ipConfig,
	)
)

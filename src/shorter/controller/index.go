package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/repository/link_repository"
)

// Repositories
var (
	linkRepository = link_repository.NewSqlLinkRepository()
)

// Settings
var settingsData = settings.GetSettings()

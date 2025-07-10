package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/repository/email_repository"
)

var (
	smtpRepository = email_repository.NewISMTPRepository()
)

var ()

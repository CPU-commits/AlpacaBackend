package smtp

import (
	"net/smtp"

	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
)

var SMTPClient smtp.Auth
var SMTPHost string
var SMTPFrom string
var SMTPPort string

var settingsData = settings.GetSettings()

func init() {
	SMTPHost = settingsData.SMTPHOST
	SMTPFrom = settingsData.SMTPFROM
	SMTPPort = settingsData.SMTPPORT
	SMTPClient = smtp.PlainAuth("", SMTPFrom, settingsData.SMTPPASS, SMTPHost)

}

package email_repository

import (
	"net/smtp"

	smtpAr "github.com/CPU-commits/Template_Go-EventDriven/src/package/smtp"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

// SMTP = Infrastructure SMTP
type iSMTPRepository struct {
	smtpC smtp.Auth
}

func NewISMTPRepository() SMTPRepository {
	return iSMTPRepository{
		smtpC: smtpAr.SMTPClient,
	}
}

func (smtpRepository iSMTPRepository) SendCodeEmail(smtpParams SMTPParams) error {

	to := smtpParams.Senders
	subject := "Código de seguridad"
	body := smtpParams.Body
	addr := smtpAr.SMTPHost + ":" + smtpAr.SMTPPort

	msg := []byte("To: " + to[0] + "\r\n" +
		"Subject: " + subject + "\r\n" +
		"MIME-Version: 1.0\r\n" +
		"Content-Type: text/html; charset=\"utf-8\"\r\n" +
		"\r\n" +
		body + "\r\n")

	if err := smtp.SendMail(addr, smtpRepository.smtpC, smtpAr.SMTPFrom, to, msg); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

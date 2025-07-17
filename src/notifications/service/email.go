package service

import (
	"bytes"
	"embed"
	"fmt"
	"html/template"

	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/repository/email_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

var emailService *EmailService

//go:embed templates/*.html
var emailTemplates embed.FS

type EmailService struct {
	smtpRepository email_repository.SMTPRepository
	bus            bus.Bus
}

func NewEmailService(
	smtpRepository email_repository.SMTPRepository,
	bus bus.Bus,
) *EmailService {
	if emailService == nil {
		return &EmailService{
			smtpRepository: smtpRepository,
			bus:            bus,
		}
	}
	return emailService
}

func (emailService EmailService) SendPasswordResetCode(data model.TemplateData) error {

	tmpl, err := template.ParseFS(emailTemplates, "templates/change_password.html")
	if err != nil {
		return err
	}

	var body bytes.Buffer
	if err := tmpl.Execute(&body, data); err != nil {
		return fmt.Errorf("error rendering template: %w", err)
	}

	if err := emailService.smtpRepository.SendCodeEmail(email_repository.SMTPParams{
		Senders: []string{data.Email},
		Body:    body.String(),
	}); err != nil {
		return err
	}

	return nil
}

func (emailService EmailService) SendEmailResetCode(data model.TemplateData) error {

	tmpl, err := template.ParseFS(emailTemplates, "templates/change_email.html")
	if err != nil {
		return err
	}

	var body bytes.Buffer
	if err := tmpl.Execute(&body, data); err != nil {
		return fmt.Errorf("error rendering template: %w", err)
	}

	if err := emailService.smtpRepository.SendCodeEmail(email_repository.SMTPParams{
		Senders: []string{data.Email},
		Body:    body.String(),
	}); err != nil {
		return err
	}

	return nil
}

package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/repository/email_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/notifications/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

type QueueEmailController struct {
	smtpRepository email_repository.SMTPRepository
	emailService   service.EmailService
}

func NewEmailQueueController(bus bus.Bus) *QueueEmailController {
	return &QueueEmailController{
		smtpRepository: smtpRepository,
		emailService: *service.NewEmailService(
			smtpRepository,
			bus,
		),
	}
}

func (queue *QueueEmailController) SendPasswordResetCodeBus(c bus.Context) error {
	var data model.TemplateData

	if err := c.BindData(&data); err != nil {
		return c.Kill(err.Error())
	}

	if err := queue.emailService.SendPasswordResetCode(data); err != nil {
		return c.Kill(err.Error())
	}

	return nil
}

func (queue *QueueEmailController) SendEmailResetCode(c bus.Context) error {
	var data model.TemplateData

	if err := c.BindData(&data); err != nil {
		return c.Kill(err.Error())
	}

	if err := queue.emailService.SendEmailResetCode(data); err != nil {
		return c.Kill(err.Error())
	}

	return nil
}

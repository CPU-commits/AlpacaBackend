package controller

import (
	"errors"
	"time"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/service"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
)

type busPaymentsController struct {
	subscriptionService service.SubscriptionService
}

func (queueController *busPaymentsController) HandleEvent(c bus.Context) error {
	var paymentEventDto dto.PaymentEventDto
	if err := c.BindData(&paymentEventDto); err != nil {
		return c.Kill(err.Error())
	}

	err := queueController.subscriptionService.HandlePaymentEvent(paymentEventDto)
	if errors.Is(err, service.ErrSubscriptionNotExistsYet) {
		return c.FollowUp(time.Minute)
	} else if err != nil {
		return err
	}

	return nil
}

func (queueController *busPaymentsController) HandleRemoveBenefits(c bus.Context) error {
	var payload RemoveBenefitsPayload
	if err := c.BindData(&payload); err != nil {
		return c.Kill(err.Error())
	}

	retryIn, err := queueController.subscriptionService.HandleRemoveBenefits(service.ToSubscription{
		IDUser:   payload.IDUser,
		IDStudio: payload.IDStudio,
	}, model.CodePlan(payload.Code))
	if retryIn != nil {
		return c.FollowUp(time.Until(*retryIn))
	}
	if err != nil {
		return err
	}

	return nil
}

func NewBusPaymentsController(bus bus.Bus) *busPaymentsController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	peopleService := studioService.NewPeopleStudioService(
		peopleStudioRepository,
		studioRepository,
		*userService,
	)

	return &busPaymentsController{
		subscriptionService: *service.NewSubscriptionService(
			subscriptionRepository,
			planRepository,
			payments,
			paymentRepository,
			userRepository,
			roleRepository,
			studioRepository,
			*peopleService,
			bus,
		),
	}
}

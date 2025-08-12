package service

import (
	"fmt"
	"strconv"
	"time"

	authRole "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/role_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/payments"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/payment_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/plan_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/subscription_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/people_histories_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type SubscriptionService struct {
	subscriptionRepository    subscription_repository.SubscriptionRepository
	planRepository            plan_repository.PlanRepository
	payments                  payments.Payments
	paymentRepository         payment_repository.PaymentRepository
	userRepository            user_repository.UserRepository
	roleRepository            role_repository.RoleRepository
	studioRepository          studio_repository.StudioRepository
	bus                       bus.Bus
	peopleService             service.AdminStudioService
	peopleHistoriesRepository people_histories_repository.PeopleHistoriesRepository
}

var subscriptionService *SubscriptionService

const TIME_LIMIT_USER_IN_STUDIO = time.Hour * 6

func (subscriptionService *SubscriptionService) GetAllActivePlans(
	forStudios bool,
	all bool,
) ([]model.Plan, error) {
	var forStudiosPtr *bool
	if !all {
		forStudiosPtr = &forStudios
	}

	return subscriptionService.planRepository.Find(&plan_repository.Criteria{
		IsActive:   utils.Bool(true),
		ForStudios: forStudiosPtr,
	})
}

func (subscriptionService *SubscriptionService) removeBenefitsToUserForPlan(
	to ToSubscription,
	codePlan model.CodePlan,
) error {
	switch codePlan {
	case model.SubscriptionTattooer:
		if err := subscriptionService.roleRepository.Delete(&role_repository.Criteria{
			IDUser: to.IDUser,
			Role:   authRole.TATTOO_ARTIST_ROLE,
		}); err != nil {
			return err
		}
		if err := subscriptionService.studioRepository.Update(
			&studio_repository.Criteria{
				IDOwner: to.IDUser,
			},
			studio_repository.UpdateData{
				IsActive: utils.Bool(false),
			},
		); err != nil {
			return err
		}
	case model.SubscriptionStudio:
		if err := subscriptionService.studioRepository.Update(
			&studio_repository.Criteria{
				ID: to.IDStudio,
			},
			studio_repository.UpdateData{
				IsLimited: utils.Bool(false),
			},
		); err != nil {
			return err
		}
	}

	return nil
}

func (subscriptionService *SubscriptionService) HandleRemoveBenefits(
	to ToSubscription,
	codePlan model.CodePlan,
) (*time.Time, error) {
	plan, err := subscriptionService.planRepository.FindOne(&plan_repository.Criteria{
		Code: codePlan,
	})
	if err != nil {
		return nil, err
	}
	if plan == nil {
		return nil, nil
	}

	existsSubscriptionActive, err := subscriptionService.subscriptionRepository.Exists(
		&subscription_repository.Criteria{
			Identifier: plan.Identifier,
			StatusNE:   model.SUBSCRIPTION_CANCELED,
			IDUser:     to.IDUser,
			IDStudio:   to.IDStudio,
		},
	)
	if err != nil {
		return nil, err
	}
	if existsSubscriptionActive {
		return nil, nil
	}
	subscription, err := subscriptionService.subscriptionRepository.FindLast(
		&subscription_repository.Criteria{
			IDUser:   to.IDUser,
			IDStudio: to.IDStudio,
		},
		subscription_repository.NewFindOneOptions().
			Select(subscription_repository.SelectOpts{
				CanceledAt: utils.Bool(true),
				ID:         utils.Bool(true),
			}),
	)
	if err != nil {
		return nil, err
	}
	if subscription.CanceledAt != nil && time.Now().Before(*subscription.CanceledAt) {
		return subscription.CanceledAt, nil
	}

	return nil, subscriptionService.removeBenefitsToUserForPlan(
		to,
		codePlan,
	)
}

func (subscriptionService *SubscriptionService) WatchVolumeSubscription(
	idSubscription,
	idPlan int64,
	billingDate time.Time,
) (*time.Time, error) {
	plan, err := subscriptionService.planRepository.FindOne(&plan_repository.Criteria{
		ID: idPlan,
	})
	if err != nil {
		return nil, err
	}
	if plan == nil {
		return nil, nil
	}
	subscription, err := subscriptionService.subscriptionRepository.FindOne(
		&subscription_repository.Criteria{
			ID: idSubscription,
		},
		nil,
	)
	if err != nil {
		return nil, err
	}
	if subscription == nil {
		return nil, nil
	}
	subscriptionFetched, err := subscriptionService.payments.FetchSubscription(
		subscription.Identifier,
	)
	if err != nil {
		return nil, err
	}
	if subscriptionFetched.CanceledAt != nil {
		return nil, nil
	}

	if plan.Code == model.SubscriptionStudio {
		d := time.Until(billingDate)

		if d >= 0 && d <= TIME_LIMIT_USER_IN_STUDIO {
			return &billingDate, nil
		}

		periodStart := billingDate.AddDate(0, -1, 0)
		periodEnd := billingDate
		histories, err := subscriptionService.peopleHistoriesRepository.Find(
			&people_histories_repository.Criteria{
				IDStudio: subscription.IDStudio,
				CreatedAt: &repository.CriteriaTime{
					LTE: periodEnd,
				},
				OR: []people_histories_repository.Criteria{
					{
						RemoveAtIsNull: utils.Bool(true),
					},
					{
						RemovedAt: &repository.CriteriaTime{
							GTE: periodStart,
						},
					},
				},
			},
			nil,
		)
		if err != nil {
			return nil, err
		}
		totalUsersInStudio := make(map[int64]int32)
		users := map[int64]float64{}
		calHoursInStudio := func(createdAt time.Time, removedAt *time.Time) float64 {
			if removedAt == nil {
				return billingDate.Sub(createdAt).Hours()
			}

			return removedAt.Sub(createdAt).Hours()
		}

		for _, history := range histories {
			if hours, exists := users[history.IDUser]; exists {
				users[history.IDUser] = hours + calHoursInStudio(history.CreatedAt, history.RemovedAt)
			} else {
				users[history.IDUser] = calHoursInStudio(history.CreatedAt, history.RemovedAt)
			}
			if users[history.IDUser] >= TIME_LIMIT_USER_IN_STUDIO.Hours() {
				totalUsersInStudio[history.IDUser] = 1
			}
		}
		usersToBeCharged := len(totalUsersInStudio) - 3

		if usersToBeCharged > 0 {
			err = subscriptionService.payments.SetUsageInSubscription(subscriptionFetched.IdentifierItemsSubscription, usersToBeCharged)
			if err != nil {
				return nil, err
			}
		}
	}
	nextBillingDate := billingDate.AddDate(0, 1, 0)

	return nil, subscriptionService.bus.Publish(bus.Event{
		Name: WATCH_VOLUME_PLAN,
		Payload: utils.Payload(map[string]any{
			"idPlan":         plan.ID,
			"idSubscription": idSubscription,
			"billingDate":    nextBillingDate,
		}),
	})
}

func (subscriptionService *SubscriptionService) giveBenefitsForPlan(
	to ToSubscription,
	codePlan model.CodePlan,
) error {
	switch codePlan {
	case model.SubscriptionTattooer:
		exists, err := subscriptionService.roleRepository.Exists(&role_repository.Criteria{
			IDUser: to.IDUser,
			Role:   authRole.TATTOO_ARTIST_ROLE,
		})
		if err != nil {
			return err
		}
		if !exists {
			return subscriptionService.roleRepository.InsertOne(to.IDUser, authRole.TATTOO_ARTIST_ROLE)
		}
		if err := subscriptionService.studioRepository.Update(
			&studio_repository.Criteria{
				IDOwner: to.IDUser,
			},
			studio_repository.UpdateData{
				IsActive: utils.Bool(true),
			},
		); err != nil {
			return err
		}
	case model.SubscriptionStudio:
		if err := subscriptionService.studioRepository.Update(
			&studio_repository.Criteria{
				ID: to.IDStudio,
			},
			studio_repository.UpdateData{
				IsLimited: utils.Bool(true),
			},
		); err != nil {
			return err
		}
	}

	return nil
}

func (subscriptionService *SubscriptionService) handleSubscription(
	event dto.PaymentEventDto,
) error {
	subscription, err := subscriptionService.payments.FetchSubscription(event.Identifier)
	if err != nil {
		return err
	}
	existsSubscription, err := subscriptionService.subscriptionRepository.FindOne(
		&subscription_repository.Criteria{
			Identifier: subscription.Identifier,
		},
		nil,
	)
	if err != nil {
		return err
	}
	plan, err := subscriptionService.planRepository.FindOne(
		&plan_repository.Criteria{
			Identifier: subscription.PlanIdentifier,
		},
	)
	if err != nil {
		return err
	}
	if existsSubscription == nil {
		if err := subscriptionService.giveBenefitsForPlan(ToSubscription{
			IDUser:   event.IDUser,
			IDStudio: event.IDStudio,
		}, plan.Code); err != nil {
			return err
		}
		idSubscription, err := subscriptionService.subscriptionRepository.InsertOne(model.Subscription{
			IDUser:          event.IDUser,
			IDPlan:          plan.ID,
			IDStudio:        event.IDStudio,
			StartDate:       subscription.CreatedAt,
			CardBrand:       subscription.CardBrand,
			BillingAnchor:   subscription.BillingAnchor,
			CardLastFour:    subscription.CardLastFour,
			EndDate:         subscription.EndsAt,
			Status:          subscription.Status,
			CanceledAt:      &subscription.EndsAt,
			NextBillingDate: subscription.RenewsAt,
			Identifier:      event.Identifier,
		})
		if err != nil {
			return err
		}
		if plan.PricingModel == model.PRICING_MODEL_VOLUME {
			if err := subscriptionService.bus.Publish(bus.Event{
				Name: WATCH_VOLUME_PLAN,
				Payload: utils.Payload(map[string]any{
					"idPlan":         plan.ID,
					"idSubscription": idSubscription,
					"billingDate":    event.CreatedActionData.CreatedAt,
				}),
			}); err != nil {
				return err
			}
		}
	}
	if subscription.Status == model.SUBSCRIPTION_CANCELED {
		if err := subscriptionService.bus.Publish(bus.Event{
			Name: REMOVE_BENEFITS_USER_FROM_PLAN,
			Payload: utils.Payload(map[string]any{
				"idUser":   existsSubscription.IDUser,
				"idStudio": existsSubscription.IDStudio,
				"codePlan": plan.Code,
			}),
		}); err != nil {
			return err
		}
	}

	return subscriptionService.subscriptionRepository.Update(
		&subscription_repository.Criteria{
			Identifier: event.Identifier,
		},
		subscription_repository.UpdateData{
			CardBrand:       subscription.CardBrand,
			BillingAnchor:   subscription.BillingAnchor,
			CardLastFour:    subscription.CardLastFour,
			EndDate:         subscription.EndsAt,
			Status:          subscription.Status,
			CanceledAt:      &subscription.EndsAt,
			NextBillingDate: subscription.RenewsAt,
		},
	)
}

func (subscriptionService *SubscriptionService) handlePayment(
	event dto.PaymentEventDto,
) error {
	payment, err := subscriptionService.payments.FetchPayment(event.Identifier)
	if err != nil {
		return err
	}
	var idSubscription int64

	if payment.TargetIsSubscription {
		plan, err := subscriptionService.planRepository.FindOne(
			&plan_repository.Criteria{
				Identifier: payment.TargetIdentifier,
			},
		)
		if err != nil {
			return err
		}

		subscription, err := subscriptionService.subscriptionRepository.FindLast(
			&subscription_repository.Criteria{
				IDUser:   event.IDUser,
				IDStudio: event.IDStudio,
				IDPlan:   plan.ID,
			},
			subscription_repository.NewFindOneOptions().
				Select(subscription_repository.SelectOpts{
					ID: utils.Bool(true),
				}),
		)
		if err != nil {
			return err
		}
		if subscription == nil || subscription.Status == model.SUBSCRIPTION_CANCELED {
			return ErrSubscriptionNotExistsYet
		}
		idSubscription = subscription.ID
	}
	existsPayment, err := subscriptionService.paymentRepository.Exists(
		&payment_repository.Criteria{
			Identifier: event.Identifier,
		},
	)
	if err != nil {
		return err
	}
	if !existsPayment {
		return subscriptionService.paymentRepository.InsertOne(model.Payment{
			IDSubscription: idSubscription,
			IDStudio:       event.IDStudio,
			IDUser:         event.IDUser,
			Status:         payment.Status,
			Price:          payment.Price,
			Currency:       payment.Currency,
			Identifier:     event.Identifier,
			PriceFormated:  payment.TotalFormated,
		})
	}

	return subscriptionService.paymentRepository.Update(
		&payment_repository.Criteria{
			Identifier: event.Identifier,
		},
		payment_repository.UpdateData{
			Status:   payment.Status,
			Price:    payment.Price,
			Currency: payment.Currency,
		},
	)
}

func (subscriptionService *SubscriptionService) HandlePaymentEvent(
	event dto.PaymentEventDto,
) error {
	switch event.Event {
	case "subscriptions":
		return subscriptionService.handleSubscription(event)
	case "payments":
		return subscriptionService.handlePayment(event)
	}

	return nil
}

func (subscriptionService *SubscriptionService) GetMySubscription(
	to ToSubscription,
) (*model.Subscription, error) {
	subscription, err := subscriptionService.subscriptionRepository.FindLast(
		&subscription_repository.Criteria{
			IDUser:   to.IDUser,
			IDStudio: to.IDStudio,
		},
		subscription_repository.NewFindOneOptions().
			Load(subscription_repository.LoadOpts{
				Plan: true,
			}),
	)
	if err != nil {
		return nil, err
	}
	if subscription == nil {
		return nil, nil
	}
	if subscription.Status == model.SUBSCRIPTION_CANCELED &&
		subscription.CanceledAt != nil &&
		time.Now().Before(*subscription.CanceledAt) {
		return nil, nil
	}
	if subscription.Status != model.SUBSCRIPTION_CANCELED {
		subscriptionFetched, err := subscriptionService.payments.
			FetchSubscription(subscription.Identifier)
		if err != nil {
			return nil, err
		}
		subscription.UpdateMethodUrl = subscriptionFetched.Urls.UpdatePaymentMethod
	}

	return subscription, nil
}

func (subscriptionService *SubscriptionService) RequestSubscription(
	idUser,
	idPlan,
	idStudio int64,
) (string, error) {
	if idStudio != 0 {
		if err := subscriptionService.peopleService.ThrowAccessInStudioIfIsNotOwner(
			idUser,
			idStudio,
		); err != nil {
			return "", err
		}
	}

	plan, err := subscriptionService.planRepository.FindOne(
		&plan_repository.Criteria{
			ID: idPlan,
		},
	)

	if err != nil {
		return "", err
	}
	if plan == nil || !plan.IsActive {
		return "", ErrPlanNotExists
	}
	if plan.ForStudios && idStudio == 0 {
		return "", ErrPlanNeedStudio
	}
	criteria := &subscription_repository.Criteria{
		IDPlan:   idPlan,
		IDStudio: idStudio,
	}
	if !plan.ForStudios {
		criteria.IDUser = idUser
	}

	subscription, err := subscriptionService.subscriptionRepository.FindLast(
		criteria,
		subscription_repository.NewFindOneOptions().
			Select(subscription_repository.SelectOpts{
				ID:     utils.Bool(true),
				Status: utils.Bool(true),
			}),
	)
	if err != nil {
		return "", err
	}
	if subscription != nil && subscription.Status != model.SUBSCRIPTION_CANCELED {
		return "", ErrHasSubscriptionActiveWithPlan
	}
	user, err := subscriptionService.userRepository.FindOne(
		&user_repository.Criteria{
			ID: idUser,
		},
		user_repository.NewFindOneOptions().
			Select(user_repository.SelectOpts{
				ID:       utils.Bool(true),
				Name:     utils.Bool(true),
				Email:    utils.Bool(true),
				Username: utils.Bool(true),
			}),
	)
	if err != nil {
		return "", err
	}
	expiresAt := time.Now().Add(time.Minute * 10)
	customData := make(map[string]string)
	var to string

	if plan.ForStudios {
		customData["studio_id"] = strconv.Itoa(int(idStudio))
		to = fmt.Sprintf("/s/%d/billing", idStudio)
	} else {
		customData["user_id"] = strconv.Itoa(int(user.ID))
		to = fmt.Sprintf("/%s/billing/subscription", user.Username)
	}

	return subscriptionService.payments.RequestPayment(payments.PaymentConfig{
		Identifier: plan.Identifier,
		BacksUrls: payments.BacksUrls{
			Success: fmt.Sprintf("%s/payments/success?to=%s&idPlan=%d", settingsData.CLIENT_URL, to, plan.ID),
		},
		User: payments.BaseUser{
			Email: user.Email,
			Name:  user.Name,
		},
		CustomData: customData,
		ExpiresAt:  &expiresAt,
		SkipTrial:  subscription != nil,
	})
}

func (subscriptionService *SubscriptionService) CancelSubscription(
	idUser,
	idStudio int64,
) error {
	criteria := &subscription_repository.Criteria{
		IDStudio: idStudio,
	}
	if idStudio == 0 {
		criteria.IDUser = idUser
	}

	subscription, err := subscriptionService.subscriptionRepository.FindLast(
		criteria,
		nil,
	)
	if err != nil {
		return err
	}
	if subscription.Status == model.SUBSCRIPTION_CANCELED {
		return ErrSubscriptionIsCancelled
	}

	err = subscriptionService.payments.CancelSubscription(subscription.Identifier)
	if err != nil {
		return err
	}

	return nil
}

func NewSubscriptionService(
	subscriptionRepository subscription_repository.SubscriptionRepository,
	planRepository plan_repository.PlanRepository,
	payments payments.Payments,
	paymentRepository payment_repository.PaymentRepository,
	userRepository user_repository.UserRepository,
	roleRepository role_repository.RoleRepository,
	studioRepository studio_repository.StudioRepository,
	peopleService service.AdminStudioService,
	peopleHistoriesRepository people_histories_repository.PeopleHistoriesRepository,
	bus bus.Bus,
) *SubscriptionService {
	if subscriptionService == nil {
		subscriptionService = &SubscriptionService{
			subscriptionRepository:    subscriptionRepository,
			planRepository:            planRepository,
			payments:                  payments,
			paymentRepository:         paymentRepository,
			userRepository:            userRepository,
			roleRepository:            roleRepository,
			studioRepository:          studioRepository,
			peopleService:             peopleService,
			bus:                       bus,
			peopleHistoriesRepository: peopleHistoriesRepository,
		}
	}

	return subscriptionService
}

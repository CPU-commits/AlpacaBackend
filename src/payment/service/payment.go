package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/payment_repository"
)

type PaymentService struct {
	paymentRepository payment_repository.PaymentRepository
}

var paymentService *PaymentService

func (paymentService *PaymentService) GetPayments(
	idUser int64,
	params PaymentsParams,
) ([]model.Payment, int64, error) {
	criteria := &payment_repository.Criteria{
		IDUser:         idUser,
		IDSubscription: params.IDSubscription,
	}

	payments, err := paymentService.paymentRepository.Find(
		criteria,
		payment_repository.NewFindOptions().
			Limit(10).
			Skip(int64(params.Page)*10).
			Order(payment_repository.Order{
				UpdatedAt: repository.DESC,
			}),
	)
	if err != nil {
		return nil, 0, err
	}
	total, err := paymentService.paymentRepository.Count(
		criteria,
	)
	if err != nil {
		return nil, 0, err
	}

	return payments, total, nil
}

func NewPaymentService(
	paymentRepository payment_repository.PaymentRepository,
) *PaymentService {
	if paymentService == nil {
		return &PaymentService{
			paymentRepository: paymentRepository,
		}
	}

	return paymentService
}

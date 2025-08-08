package payment_repository

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlPaymentRepository struct {
	db *sql.DB
}

func (sqlPaymentRepository) sqlPaymentToModel(
	sqlPayment *models.Payment,
) *model.Payment {
	return &model.Payment{
		ID:             sqlPayment.ID,
		IDUser:         sqlPayment.IDUser.Int64,
		IDSubscription: sqlPayment.IDSubscription.Int64,
		Status:         model.PaymentStatus(sqlPayment.Status),
		Text:           sqlPayment.Text.String,
		Price:          sqlPayment.Price,
		Currency:       sqlPayment.Currency,
		Identifier:     sqlPayment.Identifier,
		PriceFormated:  sqlPayment.TotalFormated,
		CreatedAt:      sqlPayment.CreatedAt,
		UpdatedAt:      sqlPayment.UpdatedAt,
	}
}

func (sqlPR sqlPaymentRepository) InsertOne(payment model.Payment) error {
	sqlPayment := models.Payment{
		IDUser:         null.NewInt64(payment.IDUser, payment.IDUser != 0),
		IDStudio:       null.NewInt64(payment.IDStudio, payment.IDStudio != 0),
		Status:         string(payment.Status),
		Text:           null.NewString(payment.Text, payment.Text != ""),
		Price:          payment.Price,
		Currency:       payment.Currency,
		Identifier:     payment.Identifier,
		IDSubscription: null.NewInt64(payment.IDSubscription, payment.IDSubscription != 0),
		TotalFormated:  payment.PriceFormated,
	}

	if err := sqlPayment.Insert(context.Background(), sqlPR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlPaymentRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.Identifier != "" {
		where = append(where, models.PaymentWhere.Identifier.EQ(criteria.Identifier))
	}
	if criteria.IDUser != 0 {
		where = append(where, models.PaymentWhere.IDUser.EQ(null.Int64From(criteria.IDUser)))
	}
	if criteria.IDSubscription != 0 {
		where = append(where, models.PaymentWhere.IDSubscription.EQ(
			null.Int64From(criteria.IDSubscription),
		))
	}

	return where
}

func (sqlPR sqlPaymentRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlPR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.Currency != "" {
		cols[models.PaymentColumns.Currency] = data.Currency
	}
	if data.Price != 0 {
		cols[models.PaymentColumns.Price] = data.Price
	}
	if data.Status != "" {
		cols[models.PaymentColumns.Status] = string(data.Status)
	}

	_, err := models.Payments(where...).UpdateAll(context.Background(), sqlPR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlPR sqlPaymentRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlPR.criteriaToWhere(criteria)

	exists, err := models.Payments(where...).Exists(context.Background(), sqlPR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlPaymentRepository) orderToMod(order *Order) []QueryMod {
	mod := []QueryMod{}

	if order == nil {
		return nil
	}
	if order.UpdatedAt == repository.ASC {
		mod = append(mod, OrderBy(fmt.Sprintf("%s ASC", models.PaymentColumns.UpdatedAt)))
	} else if order.UpdatedAt == repository.DESC {
		mod = append(mod, OrderBy(fmt.Sprintf("%s DESC", models.PaymentColumns.UpdatedAt)))
	}

	return mod
}

func (sqlPR sqlPaymentRepository) findOptionsToMod(opts *findOptions) []QueryMod {
	mod := []QueryMod{}

	if opts == nil {
		return nil
	}
	if opts.limit != nil {
		mod = append(mod, Limit(int(*opts.limit)))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(int(*opts.skip)))
	}
	mod = append(mod, sqlPR.orderToMod(opts.order)...)

	return mod
}

func (sqlPR sqlPaymentRepository) Find(criteria *Criteria, opts *findOptions) ([]model.Payment, error) {
	where := sqlPR.criteriaToWhere(criteria)
	mod := sqlPR.findOptionsToMod(opts)

	sqlPayments, err := models.Payments(append(mod, where...)...).All(context.Background(), sqlPR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlPayments, func(sqlPayment *models.Payment) model.Payment {
		return *sqlPR.sqlPaymentToModel(sqlPayment)
	}), nil
}

func (sqlPR sqlPaymentRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlPR.criteriaToWhere(criteria)

	count, err := models.Payments(where...).Count(context.Background(), sqlPR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return count, nil
}

func NewSqlPaymentRepository() PaymentRepository {
	return sqlPaymentRepository{
		db: db.DB,
	}
}

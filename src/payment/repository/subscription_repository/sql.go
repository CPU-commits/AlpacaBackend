package subscription_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/repository/plan_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlSubscriptionRepository struct {
	db                *sql.DB
	sqlPlanRepository plan_repository.SqlPlanRepository
}

func (sqlSR sqlSubscriptionRepository) InsertOne(subscription model.Subscription) error {
	sqlSubscription := models.Subscription{
		IDUser:          null.NewInt64(subscription.IDUser, subscription.IDUser != 0),
		IDPlan:          subscription.IDPlan,
		StartDate:       subscription.StartDate,
		EndDate:         subscription.EndDate,
		Status:          string(subscription.Status),
		CanceledAt:      null.TimeFromPtr(subscription.CanceledAt),
		NextBillingDate: null.NewTime(subscription.NextBillingDate, !subscription.NextBillingDate.IsZero()),
		BillingAnchor:   null.NewInt(subscription.BillingAnchor, subscription.BillingAnchor != 0),
		CardBrand:       null.NewString(subscription.CardBrand, subscription.CardBrand != ""),
		CardLastFour:    null.NewString(subscription.CardLastFour, subscription.CardLastFour != ""),
		Identifier:      subscription.Identifier,
		IDStudio:        null.NewInt64(subscription.IDStudio, subscription.IDStudio != 0),
	}

	if err := sqlSubscription.Insert(context.Background(), sqlSR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlSR sqlSubscriptionRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlSR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.BillingAnchor != 0 {
		cols[models.SubscriptionColumns.BillingAnchor] = data.BillingAnchor
	}
	if data.CardBrand != "" {
		cols[models.SubscriptionColumns.CardBrand] = data.CardBrand
	}
	if data.CardLastFour != "" {
		cols[models.SubscriptionColumns.CardLastFour] = data.CardLastFour
	}
	if data.Status != "" {
		cols[models.SubscriptionColumns.Status] = string(data.Status)
	}
	if !data.EndDate.IsZero() {
		cols[models.SubscriptionColumns.EndDate] = data.EndDate
	}
	if data.CanceledAt != nil {
		cols[models.SubscriptionColumns.CanceledAt] = data.CanceledAt
	}
	if !data.NextBillingDate.IsZero() {
		cols[models.SubscriptionColumns.NextBillingDate] = data.NextBillingDate
	}

	_, err := models.Subscriptions(where...).UpdateAll(context.Background(), sqlSR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlSR sqlSubscriptionRepository) sqlSubscriptionToModel(
	sqlSubscription *models.Subscription,
) *model.Subscription {
	var plan *model.Plan
	if sqlSubscription.R != nil && sqlSubscription.R.IDPlanPlan != nil {
		sqlPlan := sqlSubscription.R.IDPlanPlan

		plan = sqlSR.sqlPlanRepository.SqlPlanToModel(sqlPlan)
	}

	return &model.Subscription{
		ID:              sqlSubscription.ID,
		IDUser:          sqlSubscription.IDUser.Int64,
		IDPlan:          sqlSubscription.IDPlan,
		StartDate:       sqlSubscription.StartDate,
		Status:          model.SubscriptionStatus(sqlSubscription.Status),
		EndDate:         sqlSubscription.EndDate,
		CanceledAt:      sqlSubscription.CanceledAt.Ptr(),
		NextBillingDate: sqlSubscription.NextBillingDate.Time,
		CreatedAt:       sqlSubscription.CreatedAt,
		CardBrand:       sqlSubscription.CardBrand.String,
		CardLastFour:    sqlSubscription.CardLastFour.String,
		BillingAnchor:   sqlSubscription.BillingAnchor.Int,
		Identifier:      sqlSubscription.Identifier,
		IDStudio:        sqlSubscription.IDStudio.Int64,
		Plan:            plan,
	}
}

func (sqlSubscriptionRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.IDUser != 0 {
		where = append(where, models.SubscriptionWhere.IDUser.EQ(null.Int64From(criteria.IDUser)))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.SubscriptionWhere.IDStudio.EQ(null.Int64From(criteria.IDStudio)))
	}
	if criteria.Identifier != "" {
		where = append(where, models.SubscriptionWhere.Identifier.EQ(criteria.Identifier))
	}
	if criteria.IDPlan != 0 {
		where = append(where, models.SubscriptionWhere.IDPlan.EQ(criteria.IDPlan))
	}
	if criteria.StatusNE != "" {
		where = append(where, models.SubscriptionWhere.Status.NEQ(string(criteria.StatusNE)))
	}

	return where
}

func (sqlSR sqlSubscriptionRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlSR.criteriaToWhere(criteria)

	exists, err := models.Subscriptions(where...).Exists(context.Background(), sqlSR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlSubscriptionRepository) selectToMod(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return nil
	}
	if selectOpts.ID != nil && *selectOpts.ID {
		mod = append(mod, Select(models.SubscriptionColumns.ID))
	}
	if selectOpts.Status != nil && *selectOpts.Status {
		mod = append(mod, Select(models.SubscriptionColumns.Status))
	}
	if selectOpts.CanceledAt != nil && *selectOpts.CanceledAt {
		mod = append(mod, Select(models.SubscriptionColumns.CanceledAt))
	}

	return mod
}

func (sqlSubscriptionRepository) loadToMod(loadOpts *LoadOpts) []QueryMod {
	mod := []QueryMod{}
	if loadOpts == nil {
		return nil
	}
	if loadOpts.Plan {
		mod = append(mod, Load(models.SubscriptionRels.IDPlanPlan))
	}

	return mod
}

func (sqlSR sqlSubscriptionRepository) findOneOptionsToMod(opts *findOneOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return nil
	}
	mod = append(mod, sqlSR.selectToMod(opts.selectOpts)...)
	mod = append(mod, sqlSR.loadToMod(opts.loadOpts)...)

	return mod
}

func (sqlSR sqlSubscriptionRepository) FindOne(criteria *Criteria, opts *findOneOptions) (*model.Subscription, error) {
	where := sqlSR.criteriaToWhere(criteria)
	mod := sqlSR.findOneOptionsToMod(opts)

	sqlSubscription, err := models.
		Subscriptions(append(mod, where...)...).
		One(context.Background(), sqlSR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlSR.sqlSubscriptionToModel(sqlSubscription), nil
}

func (sqlSR sqlSubscriptionRepository) FindLast(criteria *Criteria, opts *findOneOptions) (*model.Subscription, error) {
	where := sqlSR.criteriaToWhere(criteria)
	mod := sqlSR.findOneOptionsToMod(opts)

	sqlSubscriptions, err := models.
		Subscriptions(append(
			append([]QueryMod{
				Limit(1),
				OrderBy(fmt.Sprintf("%s DESC", models.SubscriptionColumns.ID)),
			}, where...),
			mod...,
		)...).
		All(context.Background(), sqlSR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	if len(sqlSubscriptions) == 0 {
		return nil, nil
	}

	return sqlSR.sqlSubscriptionToModel(sqlSubscriptions[0]), nil
}

func NewSqlSubscriptionRepository() SubscriptionRepository {
	return sqlSubscriptionRepository{
		db:                db.DB,
		sqlPlanRepository: plan_repository.ExplicitSqlPlanRepository(),
	}
}

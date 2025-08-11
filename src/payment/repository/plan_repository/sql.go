package plan_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlPlanRepository struct {
	db *sql.DB
}

type SqlPlanRepository = sqlPlanRepository

func (sqlPlanRepository) SqlPlanToModel(
	sqlPlan *models.Plan,
) *model.Plan {
	var features []model.PlanFeature
	if sqlPlan.Features.Ptr() != nil {
		sqlPlan.Features.Unmarshal(&features)
	}

	return &model.Plan{
		ID:           sqlPlan.ID,
		Name:         sqlPlan.Name,
		Description:  sqlPlan.Description.String,
		Price:        sqlPlan.Price,
		Currency:     sqlPlan.Currency,
		Features:     features,
		BillingCycle: sqlPlan.BillingCycle,
		TrialDays:    sqlPlan.TrialDays,
		IsActive:     sqlPlan.IsActive,
		CreatedAt:    sqlPlan.CreatedAt,
		Identifier:   sqlPlan.Identifier,
		BannerUrl:    sqlPlan.BannerURL.String,
		VolumeItem:   sqlPlan.VolumeItem.String,
		UpdatedAt:    sqlPlan.UpdatedAt,
		ForStudios:   sqlPlan.ForStudios,
		Code:         model.CodePlan(sqlPlan.Code),
		PricingModel: model.PricingModel(sqlPlan.PricingModel),
	}
}

func (sqlPlanRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.IsActive != nil {
		where = append(where, models.PlanWhere.IsActive.EQ(*criteria.IsActive))
	}
	if criteria.Identifier != "" {
		where = append(where, models.PlanWhere.Identifier.EQ(criteria.Identifier))
	}
	if criteria.Code != "" {
		where = append(where, models.PlanWhere.Code.EQ(string(criteria.Code)))
	}
	if criteria.ID != 0 {
		where = append(where, models.PlanWhere.ID.EQ(criteria.ID))
	}
	if criteria.ForStudios != nil {
		where = append(where, models.PlanWhere.ForStudios.EQ(*criteria.ForStudios))
	}

	return where
}

func (sqlPR sqlPlanRepository) FindOne(criteria *Criteria) (*model.Plan, error) {
	where := sqlPR.criteriaToWhere(criteria)

	sqlPlan, err := models.Plans(where...).One(context.Background(), sqlPR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return sqlPR.SqlPlanToModel(sqlPlan), nil
}

func (sqlPR sqlPlanRepository) Find(criteria *Criteria) ([]model.Plan, error) {
	where := sqlPR.criteriaToWhere(criteria)

	sqlPlans, err := models.Plans(where...).All(context.Background(), sqlPR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlPlans, func(sqlPlan *models.Plan) model.Plan {
		return *sqlPR.SqlPlanToModel(sqlPlan)
	}), nil
}

func NewSqlPlanRepository() PlanRepository {
	return sqlPlanRepository{
		db: db.DB,
	}
}

func ExplicitSqlPlanRepository() SqlPlanRepository {
	return sqlPlanRepository{
		db: db.DB,
	}
}

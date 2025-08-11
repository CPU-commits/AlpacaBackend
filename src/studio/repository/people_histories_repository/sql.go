package people_histories_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlPeopleHistoriesRepository struct {
	db *sql.DB
}

func (sqlPHR sqlPeopleHistoriesRepository) Insert(history model.PeopleHistory) error {
	sqlStudioUserHistory := models.StudioUserHistory{
		IDUser:   history.IDUser,
		IDStudio: history.IDStudio,
	}

	if err := sqlStudioUserHistory.Insert(context.Background(), sqlPHR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlPHR sqlPeopleHistoriesRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	if criteria == nil {
		return nil
	}
	where := []QueryMod{}
	if criteria.IDUser != 0 {
		where = append(where, models.StudioUserHistoryWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.StudioUserHistoryWhere.IDStudio.EQ(criteria.IDStudio))
	}
	if criteria.RemoveAtIsNull != nil && *criteria.RemoveAtIsNull {
		where = append(where, models.StudioUserHistoryWhere.RemovedAt.IsNull())
	}
	if criteria.RemoveAtIsNull != nil && !*criteria.RemoveAtIsNull {
		where = append(where, models.StudioUserHistoryWhere.RemovedAt.IsNotNull())
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.CreatedAt.LTE(
				criteria.CreatedAt.LTE,
			))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.CreatedAt.LT(
				criteria.CreatedAt.LT,
			))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.CreatedAt.GTE(
				criteria.CreatedAt.GTE,
			))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.CreatedAt.GT(
				criteria.CreatedAt.GT,
			))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.CreatedAt.EQ(
				criteria.CreatedAt.EQ,
			))
		}
	}
	if criteria.RemovedAt != nil {
		if !criteria.RemovedAt.LTE.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.RemovedAt.LTE(
				null.TimeFrom(criteria.RemovedAt.LTE),
			))
		}
		if !criteria.RemovedAt.LT.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.RemovedAt.LT(
				null.TimeFrom(criteria.RemovedAt.LT),
			))
		}
		if !criteria.RemovedAt.GTE.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.RemovedAt.GTE(
				null.TimeFrom(criteria.RemovedAt.GTE),
			))
		}
		if !criteria.RemovedAt.GT.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.RemovedAt.GT(
				null.TimeFrom(criteria.RemovedAt.GT),
			))
		}
		if !criteria.RemovedAt.EQ.IsZero() {
			where = append(where, models.StudioUserHistoryWhere.RemovedAt.EQ(
				null.TimeFrom(criteria.RemovedAt.EQ),
			))
		}
	}
	var orMods []QueryMod
	for _, clause := range criteria.OR {
		orWhere := sqlPHR.criteriaToWhere(&clause)
		orMods = append(orMods, Or2(Expr(orWhere...)))
	}
	if orMods != nil {
		where = append(where, Expr(orMods...))
	}

	return where
}

func (sqlPHR sqlPeopleHistoriesRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlPHR.criteriaToWhere(criteria)
	cols := models.M{}
	if !data.RemovedAt.IsZero() {
		cols[models.StudioUserHistoryColumns.RemovedAt] = null.TimeFrom(data.RemovedAt)
	}

	_, err := models.StudioUserHistories(where...).UpdateAll(context.Background(), sqlPHR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlPeopleHistoriesRepository) sqlStudioUserHistoryToModel(
	sqlHistory *models.StudioUserHistory,
) *model.PeopleHistory {
	return &model.PeopleHistory{
		ID:        sqlHistory.ID,
		IDUser:    sqlHistory.IDUser,
		IDStudio:  sqlHistory.IDStudio,
		CreatedAt: sqlHistory.CreatedAt,
		RemovedAt: sqlHistory.RemovedAt.Ptr(),
	}
}

func (sqlPHR sqlPeopleHistoriesRepository) sortToMod(opts *Sort) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return nil
	}
	if opts.CreatedAt == repository.ASC {
		mod = append(mod, OrderBy("created_at ASC"))
	}
	if opts.CreatedAt == repository.DESC {
		mod = append(mod, OrderBy("created_at DESC"))
	}

	return mod
}

func (sqlPHR sqlPeopleHistoriesRepository) findOneOptionsToMod(opts *findOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return nil
	}
	if opts.limit != nil {
		mod = append(mod, Limit(int(*opts.limit)))
	}
	mod = append(mod, sqlPHR.sortToMod(opts.sort)...)

	return mod
}

func (sqlPHR sqlPeopleHistoriesRepository) Find(criteria *Criteria, opts *findOptions) ([]model.PeopleHistory, error) {
	where := sqlPHR.criteriaToWhere(criteria)
	mod := sqlPHR.findOneOptionsToMod(opts)

	sqlStudioUserHistories, err := models.StudioUserHistories(append(
		mod,
		where...,
	)...).All(context.Background(), sqlPHR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlStudioUserHistories, func(sqlHistory *models.StudioUserHistory) model.PeopleHistory {
		return *sqlPHR.sqlStudioUserHistoryToModel(sqlHistory)
	}), nil
}

func NewSqlPeopleHistoriesRepository() PeopleHistoriesRepository {
	return sqlPeopleHistoriesRepository{
		db: db.DB,
	}
}

package category_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlCategoryRepository struct {
	db *sql.DB
}

func (sqlCategoryRepository) sqlCategoryToCategory(
	sqlCategory *models.Category,
) model.Category {
	return model.Category{
		ID:          sqlCategory.ID,
		CreatedAt:   sqlCategory.CreatedAt,
		Name:        sqlCategory.Name,
		Description: sqlCategory.Description,
		State:       sqlCategory.State,
		Slug:        sqlCategory.Slug,
	}
}

func (sqlCR sqlCategoryRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.ID != 0 {
		mod = append(mod, Where("id = ?", criteria.ID))
	}
	if criteria.State != nil {
		mod = append(mod, Where("state = ?", *criteria.State))
	}
	for _, andCriteria := range criteria.And {
		andMods := sqlCR.criteriaToWhere(&andCriteria)
		for _, and := range andMods {
			mod = append(mod, and)
		}
	}

	return mod
}

func (sqlCR sqlCategoryRepository) Find(criteria *Criteria) ([]model.Category, error) {
	where := sqlCR.criteriaToWhere(criteria)

	categories, err := models.Categories(where...).All(context.Background(), sqlCR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(categories, func(category *models.Category) model.Category {
		return sqlCR.sqlCategoryToCategory(category)
	}), nil
}

func (sqlCR sqlCategoryRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlCR.criteriaToWhere(criteria)

	exists, err := models.Categories(where...).Exists(context.Background(), sqlCR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func NewSQLAccessRepository(db *sql.DB) CategoryRepository {
	return sqlCategoryRepository{
		db: db,
	}
}

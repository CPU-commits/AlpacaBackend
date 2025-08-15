package view_repository

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/model"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlViewRepository struct {
	db *sql.DB
}

func (sqlViewRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return where
	}
	if criteria.IDPost != 0 {
		where = append(where, models.ViewWhere.IDPost.EQ(null.Int64From(criteria.IDPost)))
	}
	if criteria.IDProfile != 0 {
		where = append(where, models.ViewWhere.IDProfile.EQ(null.Int64From(criteria.IDProfile)))
	}
	if criteria.IDLink != 0 {
		where = append(where, models.ViewWhere.IDLink.EQ(null.Int64From(criteria.IDLink)))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.ViewWhere.IDStudio.EQ(null.Int64From(criteria.IDStudio)))
	}
	if criteria.IDTattoo != 0 {
		where = append(where, models.ViewWhere.IDTattoo.EQ(null.Int64From(criteria.IDTattoo)))
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			where = append(where, models.ViewWhere.CreatedAt.LTE(criteria.CreatedAt.LTE))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			where = append(where, models.ViewWhere.CreatedAt.LT(criteria.CreatedAt.LT))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			where = append(where, models.ViewWhere.CreatedAt.GTE(criteria.CreatedAt.GTE))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			where = append(where, models.ViewWhere.CreatedAt.GT(criteria.CreatedAt.GT))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			where = append(where, models.ViewWhere.CreatedAt.EQ(criteria.CreatedAt.EQ))
		}
	}

	return where
}
func (sqlViewRepository) sqlToModel(sqlView *models.View) model.View {
	return model.View{
		ID:        sqlView.ID,
		IDPost:    sqlView.IDPost.Int64,
		IDProfile: sqlView.IDProfile.Int64,
		IDUser:    sqlView.IDUser.Int64,
		IDLink:    sqlView.IDLink.Int64,
		IDStudio:  sqlView.IDStudio.Int64,
		IDTattoo:  sqlView.IDTattoo.Int64,
		Country:   sqlView.Country.String,
		Continent: sqlView.Continent.String,
		City:      sqlView.City.String,
		Region:    sqlView.Region.String,
		TimeZone:  sqlView.Timezone.String,
		CreatedAt: sqlView.CreatedAt,
	}
}

func (sqlVR sqlViewRepository) CountGroupByDayAndLocation(criteria *Criteria) ([]CountGroupByDayResultAndLocation, error) {
	where := sqlVR.criteriaToWhere(criteria)

	type SqlViewResult struct {
		Views   int64       `boil:"views"`
		City    null.String `boil:"city"`
		Region  null.String `boil:"region"`
		Country null.String `boil:"country"`
	}
	var views []SqlViewResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Views),
			Select("city"),
			Select("region"),
			Select("country"),
			Select("COUNT(*) as views"),
			GroupBy("city"),
			GroupBy("region"),
			GroupBy("country"),
		}, where...)...,
	).Bind(context.Background(), sqlVR.db, &views)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(views, func(view SqlViewResult) CountGroupByDayResultAndLocation {
		return CountGroupByDayResultAndLocation{
			Views:   view.Views,
			City:    view.City.String,
			Country: view.Country.String,
			Region:  view.Region.String,
		}
	}), nil
}

func (sqlVR sqlViewRepository) CountGroupByDay(criteria *Criteria) ([]CountGroupByDayResult, error) {
	where := sqlVR.criteriaToWhere(criteria)

	type SqlViewResult struct {
		Day   time.Time `boil:"day"`
		Views int64     `boil:"views"`
	}
	var views []SqlViewResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Views),
			Select("DATE(created_at) as day"),
			Select("COUNT(*) as views"),
			GroupBy("day"),
		}, where...)...,
	).Bind(context.Background(), sqlVR.db, &views)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(views, func(view SqlViewResult) CountGroupByDayResult {
		return CountGroupByDayResult{
			Views: view.Views,
			Day:   view.Day,
		}
	}), nil
}

func (sqlVR sqlViewRepository) Insert(view model.View) error {
	sqlView := models.View{
		IDUser:    null.NewInt64(view.IDUser, view.IDUser != 0),
		IDPost:    null.NewInt64(view.IDPost, view.IDPost != 0),
		IDProfile: null.NewInt64(view.IDProfile, view.IDProfile != 0),
		IDTattoo:  null.NewInt64(view.IDTattoo, view.IDTattoo != 0),
		Country:   null.NewString(view.Country, view.Country != ""),
		IDStudio:  null.NewInt64(view.IDStudio, view.IDStudio != 0),
		City:      null.NewString(view.City, view.City != ""),
		Continent: null.NewString(view.Continent, view.Continent != ""),
		Region:    null.NewString(view.Region, view.Region != ""),
		Timezone:  null.NewString(view.TimeZone, view.TimeZone != ""),
		IDLink:    null.NewInt64(view.IDLink, view.IDLink != 0),
	}
	fmt.Printf("sqlView: %v\n", sqlView)
	if err := sqlView.Insert(context.Background(), sqlVR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}
func (sqlVR sqlViewRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlVR.criteriaToWhere(criteria)

	views, err := models.Views(where...).Count(context.Background(), sqlVR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return views, nil
}

func NewSqlViewRepository() ViewRepository {
	return sqlViewRepository{
		db: db.DB,
	}
}

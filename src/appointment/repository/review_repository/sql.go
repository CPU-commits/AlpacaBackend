package review_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlReviewRepository struct {
	db *sql.DB
}

func (sqlRR sqlReviewRepository) InsertOne(review model.Review) error {
	sqlReview := models.Review{
		Stars:         int(review.Stars),
		IDProfile:     review.IDProfile,
		Content:       review.Review,
		IDUser:        review.IDUser,
		IDAppointment: review.IDAppointment,
	}
	if err := sqlReview.Insert(context.Background(), sqlRR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlReviewRepository) sqlReviewToModel(sqlReview *models.Review) *model.Review {
	return &model.Review{
		ID:            sqlReview.ID,
		IDUser:        sqlReview.IDUser,
		IDProfile:     sqlReview.IDProfile,
		IDAppointment: sqlReview.IDAppointment,
		Stars:         int16(sqlReview.Stars),
		Review:        sqlReview.Content,
	}
}

func (sqlReviewRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.IDUser != 0 {
		where = append(where, models.ReviewWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.IDAppointment != 0 {
		where = append(where, models.ReviewWhere.IDAppointment.EQ(criteria.IDUser))
	}

	return where
}

func (sqlRR sqlReviewRepository) FindOne(criteria *Criteria) (*model.Review, error) {
	where := sqlRR.criteriaToWhere(criteria)

	sqlReview, err := models.Reviews(where...).One(context.Background(), sqlRR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlRR.sqlReviewToModel(sqlReview), nil
}

func (sqlRR sqlReviewRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlRR.criteriaToWhere(criteria)

	exists, err := models.Reviews(where...).Exists(context.Background(), sqlRR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func NewSqlReviewRepository() ReviewRepository {
	return sqlReviewRepository{
		db: db.DB,
	}
}

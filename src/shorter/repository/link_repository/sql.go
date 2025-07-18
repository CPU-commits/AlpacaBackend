package link_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"

	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlLinkRepository struct {
	db *sql.DB
}

func (sqlLinkRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.ShortCode != "" {
		where = append(where, models.LinkWhere.ShortCode.EQ(criteria.ShortCode))
	}

	return where
}

func (sqlLinkRepository) sqlLinkToModel(sqlLink *models.Link) *model.Link {
	return &model.Link{
		ID:        sqlLink.ID,
		Link:      sqlLink.Link,
		ShortCode: sqlLink.ShortCode,
	}
}

func (sqlLR sqlLinkRepository) FindOne(criteria *Criteria) (*model.Link, error) {
	where := sqlLR.criteriaToWhere(criteria)

	sqlLink, err := models.Links(where...).One(context.Background(), sqlLR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlLR.sqlLinkToModel(sqlLink), nil
}

func NewSqlLinkRepository() LinkRepository {
	return sqlLinkRepository{
		db: db.DB,
	}
}

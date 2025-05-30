package role_repository

import (
	"context"
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlRoleRepository struct {
	db *sql.DB
}

func (sqlRR sqlRoleRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return where
	}
	if criteria.Role != "" {
		where = append(where, models.RolesUserWhere.Role.EQ(string(criteria.Role)))
	}
	if criteria.IDUser != 0 {
		where = append(where, models.RolesUserWhere.IDUser.EQ(criteria.IDUser))
	}

	return where
}

func (sqlRR sqlRoleRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlRR.criteriaToWhere(criteria)

	exists, err := models.RolesUsers(where...).Exists(context.Background(), sqlRR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func NewSQLRoleRepository() RoleRepository {
	return sqlRoleRepository{
		db: db.DB,
	}
}

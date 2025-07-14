package people_studio_repository

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"strings"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlPeopleStudioRepository struct {
	db                *sql.DB
	sqlUserRepository user_repository.SqlUserRepository
}

func (sqlASR sqlPeopleStudioRepository) sqlPeopleToModel(sqlPeople *models.StudioUser) *model.StudioPerson {
	var permissions []model.StudioPermission

	if sqlPeople.R != nil && sqlPeople.R.IDAdminStudioAdminsPermissions != nil {
		sqlPermissions := sqlPeople.R.IDAdminStudioAdminsPermissions

		for _, sqlPermission := range sqlPermissions {
			permissions = append(permissions, model.StudioPermission(sqlPermission.Permission))
		}
	}

	var people *authModel.User
	if sqlPeople.R != nil && sqlPeople.R.IDUserUser != nil {
		sqlUser := sqlPeople.R.IDUserUser

		people = sqlASR.sqlUserRepository.SqlUserToUser(sqlUser, nil)
	}

	var roles []string
	sqlPeople.Roles.Unmarshal(&roles)

	return &model.StudioPerson{
		ID:          sqlPeople.ID,
		IDStudio:    sqlPeople.IDStudio,
		IDUser:      sqlPeople.IDUser,
		Permissions: permissions,
		User:        people,
		Roles: utils.MapNoError(roles, func(role string) model.StudioRole {
			return model.StudioRole(role)
		}),
	}
}

func (sqlASR sqlPeopleStudioRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{
		LeftOuterJoin("studio_admins_permissions sap ON studio_users.id_user = sap.id_admin"),
		Load(models.StudioUserRels.IDAdminStudioAdminsPermissions),
	}
	if criteria == nil {
		return where
	}
	if criteria.IDUser != 0 {
		where = append(where, models.StudioUserWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.StudioUserWhere.IDStudio.EQ(criteria.IDStudio))
	}
	if criteria.Roles != nil {
		roles := strings.Join(utils.MapNoError(criteria.Roles, func(role model.StudioRole) string {
			return fmt.Sprintf("\"%s\"", role)
		}), ",")

		where = append(where, Where(fmt.Sprintf(`studio_users.roles @> '[%s]'::jsonb`, roles)))
	}
	if criteria.Permissions != nil {
		for _, permission := range criteria.Permissions {
			where = append(where, Where("sap.permission = ?", permission))
		}
	}

	return where
}

func (sqlASR sqlPeopleStudioRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlASR.criteriaToWhere(criteria)

	exists, err := models.StudioUsers(where...).Exists(context.Background(), sqlASR.db)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlASR sqlPeopleStudioRepository) InsertOne(people model.StudioPerson) error {
	roles, _ := json.Marshal(people.Roles)

	sqlPeople := models.StudioUser{
		IDStudio: people.IDStudio,
		IDUser:   people.IDUser,
		Roles:    roles,
	}

	if err := sqlPeople.Insert(context.Background(), sqlASR.db, boil.Infer()); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlASR sqlPeopleStudioRepository) FindOne(criteria *Criteria) (*model.StudioPerson, error) {
	where := sqlASR.criteriaToWhere(criteria)

	sqlPeople, err := models.StudioUsers(where...).One(context.Background(), sqlASR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlASR.sqlPeopleToModel(sqlPeople), nil
}

func (sqlASR sqlPeopleStudioRepository) includeToMod(include *Include) []QueryMod {
	mod := []QueryMod{}
	if include == nil {
		return nil
	}
	if include.User != nil {
		mod = append(mod,
			Load(models.StudioUserRels.IDUserUser, sqlASR.sqlUserRepository.SelectOpts(include.User)...),
		)
	}

	return mod
}

func (sqlASR sqlPeopleStudioRepository) findOptsToMod(opts *findOpts) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlASR.includeToMod(opts.include)...)

	return mod
}

func (sqlASR sqlPeopleStudioRepository) Find(criteria *Criteria, opts *findOpts) ([]model.StudioPerson, error) {
	where := sqlASR.criteriaToWhere(criteria)
	mod := sqlASR.findOptsToMod(opts)

	sqlPeoples, err := models.StudioUsers(append(mod, where...)...).All(context.Background(), sqlASR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlPeoples, func(sqlPeople *models.StudioUser) model.StudioPerson {
		return *sqlASR.sqlPeopleToModel(sqlPeople)
	}), nil
}

func (sqlASR sqlPeopleStudioRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlASR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.Roles != nil {
		roles, _ := json.Marshal(data.Roles)

		cols[models.StudioUserColumns.Roles] = roles
	}
	if data.Permission != nil {
		studioUser, err := models.StudioUsers(
			models.StudioUserWhere.IDUser.EQ(criteria.IDUser),
			models.StudioUserWhere.IDStudio.EQ(criteria.IDStudio),
			Select(models.StudioUserColumns.ID),
		).One(context.Background(), sqlASR.db)
		if err != nil {
			return utils.ErrRepositoryFailed
		}

		existsPermission, err := models.StudioAdminsPermissions(
			models.StudioAdminsPermissionWhere.IDAdmin.EQ(studioUser.ID),
			models.StudioAdminsPermissionWhere.Permission.EQ(string(data.Permission.Permission)),
		).Exists(context.Background(), sqlASR.db)
		if err != nil {
			return utils.ErrRepositoryFailed
		}
		if (existsPermission && data.Permission.Enabled) || (!existsPermission && !data.Permission.Enabled) {
			return nil
		}

		if data.Permission.Enabled {
			sqlPermission := models.StudioAdminsPermission{
				IDAdmin:    studioUser.ID,
				Permission: string(data.Permission.Permission),
			}

			if err := sqlPermission.Insert(context.Background(), sqlASR.db, boil.Infer()); err != nil {
				return utils.ErrRepositoryFailed
			}
		} else {
			_, err := models.StudioAdminsPermissions(
				models.StudioAdminsPermissionWhere.IDAdmin.EQ(studioUser.ID),
				models.StudioAdminsPermissionWhere.Permission.EQ(string(data.Permission.Permission)),
			).DeleteAll(context.Background(), sqlASR.db)
			if err != nil {
				return utils.ErrRepositoryFailed
			}
		}
	}

	_, err := models.StudioUsers(where...).UpdateAll(context.Background(), sqlASR.db, cols)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlASR sqlPeopleStudioRepository) Delete(criteria *Criteria) error {
	tx, err := sqlASR.db.Begin()
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	where := sqlASR.criteriaToWhere(criteria)

	_, err = models.StudioUsers(where...).DeleteAll(context.Background(), tx)
	if err != nil {
		tx.Rollback()

		return utils.ErrRepositoryFailed
	}
	if err := tx.Commit(); err != nil {
		tx.Rollback()

		return utils.ErrRepositoryFailed
	}

	return nil
}

func NewSqlPeopleStudioRepository(sqlUserRepository user_repository.SqlUserRepository) PeopleStudioRepository {
	return sqlPeopleStudioRepository{
		db:                db.DB,
		sqlUserRepository: sqlUserRepository,
	}
}

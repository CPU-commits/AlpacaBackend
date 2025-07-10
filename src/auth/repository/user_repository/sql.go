package user_repository

import (
	"context"
	"database/sql"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
	"golang.org/x/crypto/bcrypt"
)

type sqlUserRepository struct {
	db *sql.DB
}

type SqlUserRepository = sqlUserRepository

func (sqlUserRepository) sqlUserToUser(
	sqlUser *models.User,
	roles []string,
) *model.User {
	return &model.User{
		ID:    sqlUser.ID,
		Email: sqlUser.Email,
		Name:  sqlUser.Name,
		Phone: sqlUser.Phone.String,
		Roles: utils.MapNoError(roles, func(role string) model.Role {
			return model.Role(role)
		}),
		Username: sqlUser.Username,
	}
}

func (sqlUR sqlUserRepository) getUserRoles(user *models.User) ([]string, error) {
	roles, err := user.IDUserRolesUsers().All(context.Background(), sqlUR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(roles, func(role *models.RolesUser) string {
		return role.Role
	}), nil
}

func (sqlUR sqlUserRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	var mod []QueryMod
	if criteria == nil {
		return nil
	}
	if criteria.ID != 0 {
		mod = append(mod, Where("id = ?", criteria.ID))
	}
	if criteria.Email != "" {
		mod = append(mod, Where("email = ?", criteria.Email))
	}
	if criteria.Username != "" {
		mod = append(mod, Where("username = ?", criteria.Username))
	}
	var orMods []QueryMod
	for _, clause := range criteria.Or {
		orWhere := sqlUR.criteriaToWhere(&clause)
		orMods = append(orMods, orWhere...)
	}
	if orMods != nil {
		mod = append(mod, Expr(Or2(Expr(orMods...))))
	}

	return mod
}

func (sqlUserRepository) SelectOpts(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return mod
	}
	if selectOpts.ID != nil && *selectOpts.ID {
		mod = append(mod, Select(models.UserColumns.ID))
	}
	if selectOpts.Name != nil && *selectOpts.Name {
		mod = append(mod, Select(models.UserColumns.Name))
	}
	if selectOpts.Email != nil && *selectOpts.Email {
		mod = append(mod, Select(models.UserColumns.Email))
	}
	if selectOpts.Phone != nil && *selectOpts.Phone {
		mod = append(mod, Select(models.UserColumns.Phone))
	}
	if selectOpts.Username != nil && *selectOpts.Username {
		mod = append(mod, Select(models.UserColumns.Username))
	}

	return mod
}

func (sqlUR sqlUserRepository) findOneOptionsToMod(opts *FindOneOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlUR.SelectOpts(opts.selectOpts)...)

	return mod
}

func (sqlUR sqlUserRepository) FindOne(criteria *Criteria, opts *FindOneOptions) (*model.User, error) {
	mod := sqlUR.findOneOptionsToMod(opts)
	where := sqlUR.criteriaToWhere(criteria)

	sqlUser, err := models.Users(append(mod, where...)...).One(context.Background(), sqlUR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlUR.sqlUserToUser(sqlUser, nil), nil
}

func (sqlUR sqlUserRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlUR.criteriaToWhere(criteria)

	exists, err := models.Users(where...).Exists(context.Background(), sqlUR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlUR sqlUserRepository) FindOneByEmail(email string) (*model.User, error) {
	user, err := models.Users(
		Where("email = ?", email),
	).One(context.Background(), sqlUR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}
	roles, err := sqlUR.getUserRoles(user)
	if err != nil {
		return nil, err
	}

	return sqlUR.sqlUserToUser(user, roles), nil
}

func (sqlUR sqlUserRepository) FindOneByID(id int64) (*model.User, error) {
	user, err := models.Users(
		Where("id = ?", id),
	).One(context.Background(), sqlUR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}
	roles, err := sqlUR.getUserRoles(user)
	if err != nil {
		return nil, err
	}

	return sqlUR.sqlUserToUser(user, roles), nil
}

func (sqlUR sqlUserRepository) InsertOne(user *model.User, password string) (*model.User, error) {
	sqlUser := models.User{
		Email:    user.Email,
		Name:     user.Name,
		Username: user.Username,
	}
	ctx := context.Background()
	tx, err := sqlUR.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	if err := sqlUser.Insert(ctx, tx, boil.Infer()); err != nil {

		tx.Rollback()
		return nil, utils.ErrRepositoryFailed
	}
	for _, role := range user.Roles {
		sqlRole := models.RolesUser{
			IDUser: sqlUser.ID,
			Role:   string(role),
		}
		if err := sqlRole.Insert(ctx, tx, boil.Infer()); err != nil {

			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}
	}

	passwordHashed, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)
	if err != nil {
		tx.Rollback()
		return nil, err
	}
	sqlAuth := models.Auth{
		IDUser:   sqlUser.ID,
		Password: string(passwordHashed),
	}
	if err := sqlAuth.Insert(ctx, tx, boil.Infer()); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}
	sqlProfile := models.Profile{
		IDUser: sqlUser.ID,
	}
	if err := sqlProfile.Insert(ctx, tx, boil.Infer()); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return nil, utils.ErrRepositoryFailed
	}

	return sqlUR.sqlUserToUser(&sqlUser, utils.MapNoError(user.Roles, func(role model.Role) string {
		return string(role)
	})), nil
}

func (sqlUR sqlUserRepository) UpdateOne(userId int64, data UserUpdateData) error {
	user, err := models.FindUser(context.Background(), sqlUR.db, userId)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return utils.ErrRepositoryFailed
	}
	var cols []string

	if data.Email != nil {
		user.Email = *data.Email
		cols = append(cols, models.UserColumns.Email)
	}
	if data.Name != nil {
		user.Name = *data.Name
		cols = append(cols, models.UserColumns.Name)
	}

	if data.Phone != nil {
		user.Phone = null.StringFrom(*data.Phone)
		cols = append(cols, models.UserColumns.Phone)
	}

	_, err = user.Update(context.Background(), sqlUR.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	return nil
}

func NewSQLUserRepository(db *sql.DB) UserRepository {
	return sqlUserRepository{
		db: db,
	}
}

func SqlExplicitUserRepository(db *sql.DB) SqlUserRepository {
	return sqlUserRepository{
		db: db,
	}
}

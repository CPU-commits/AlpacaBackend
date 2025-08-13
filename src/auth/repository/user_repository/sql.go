package user_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	shorterModel "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/model"
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

func (sqlUserRepository) SqlUserToUser(
	sqlUser *models.User,
	roles []string,
) *model.User {
	var media []shorterModel.Media
	if sqlUser.R != nil && sqlUser.R.IDUserLinks != nil {
		sqlLinks := sqlUser.R.IDUserLinks

		for _, sqlLink := range sqlLinks {
			media = append(media, shorterModel.Media{
				ID:        sqlLink.ID,
				Link:      sqlLink.Link,
				ShortCode: sqlLink.ShortCode,
				Type:      shorterModel.TypeMedia(sqlLink.Type),
			})
		}
	}
	if sqlUser.R != nil && sqlUser.R.IDUserRolesUsers != nil && roles == nil {
		sqlRoles := sqlUser.R.IDUserRolesUsers

		for _, sqlRole := range sqlRoles {
			roles = append(roles, sqlRole.Role)
		}
	}

	return &model.User{
		ID:    sqlUser.ID,
		Email: sqlUser.Email,
		Name:  sqlUser.Name,
		Phone: sqlUser.Phone.String,
		Roles: utils.MapNoError(roles, func(role string) model.Role {
			return model.Role(role)
		}),
		Username:  sqlUser.Username,
		Location:  sqlUser.Location.String,
		Media:     media,
		UpdatedAt: sqlUser.UpdatedAt,
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
		mod = append(mod, models.UserWhere.ID.EQ(criteria.ID))
	}
	if criteria.Email.EQ != nil {
		mod = append(mod, models.UserWhere.Email.EQ(*criteria.Email.EQ))
	} else if criteria.Email.IContains != nil {
		mod = append(mod, models.UserWhere.Email.ILIKE(fmt.Sprintf("%%%s%%", *criteria.Email.IContains)))
	}
	if criteria.Username.EQ != nil {
		mod = append(mod, models.UserWhere.Username.EQ(*criteria.Username.EQ))
	} else if criteria.Username.IContains != nil {
		mod = append(mod, models.UserWhere.Username.ILIKE(fmt.Sprintf("%%%s%%", *criteria.Username.IContains)))
	}
	if criteria.Name.EQ != nil {
		mod = append(mod, models.UserWhere.Name.EQ(*criteria.Name.EQ))
	} else if criteria.Name.IContains != nil {
		mod = append(mod, models.UserWhere.Name.ILIKE(fmt.Sprintf("%%%s%%", *criteria.Name.IContains)))
	}
	if criteria.ID_NIN != nil {
		mod = append(mod, models.UserWhere.ID.NIN(criteria.ID_NIN))
	}
	if criteria.Roles != nil {
		mod = append(mod,
			Select(`"users"."id" AS "id"`),
			Load(models.UserRels.IDUserRolesUsers),
			LeftOuterJoin("roles_users ru on ru.id_user = users.id"),
			WhereIn("ru.role in ?", utils.MapNoError(criteria.Roles, func(role model.Role) any {
				return string(role)
			})...),
		)
	}

	var orMods []QueryMod
	for _, clause := range criteria.Or {
		orWhere := sqlUR.criteriaToWhere(&clause)
		if orWhere != nil {
			orMods = append(orMods, Or2(Expr(orWhere...)))
		}
	}
	if orMods != nil {
		mod = append(mod, Expr(orMods...))
	}
	return mod
}

func (sqlUserRepository) SelectOpts(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return mod
	}
	if selectOpts.ID != nil && *selectOpts.ID {
		mod = append(mod, Select(models.UserTableColumns.ID))
	}
	if selectOpts.Name != nil && *selectOpts.Name {
		mod = append(mod, Select(models.UserColumns.Name))
	}
	if selectOpts.Email != nil && *selectOpts.Email {
		mod = append(mod, Select(models.UserColumns.Email))
	}
	if selectOpts.UpdatedAt != nil && *selectOpts.UpdatedAt {
		mod = append(mod, Select(models.UserColumns.UpdatedAt))
	}
	if selectOpts.Phone != nil && *selectOpts.Phone {
		mod = append(mod, Select(models.UserColumns.Phone))
	}
	if selectOpts.Username != nil && *selectOpts.Username {
		mod = append(mod, Select(models.UserColumns.Username))
	}
	if selectOpts.Location != nil && *selectOpts.Location {
		mod = append(mod, Select(models.UserColumns.Location))
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
		fmt.Printf("err: %v\n", err)
		return nil, utils.ErrRepositoryFailed
	}

	return sqlUR.SqlUserToUser(sqlUser, nil), nil
}

func (sqlUR sqlUserRepository) findOptionsToMod(opts *FindOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlUR.SelectOpts(opts.selectOpts)...)
	if opts.limit != nil {
		mod = append(mod, Limit(int(*opts.limit)))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(int(*opts.skip)))
	}

	return mod
}

func (sqlUR sqlUserRepository) Find(criteria *Criteria, opts *FindOptions) ([]model.User, error) {
	mod := sqlUR.findOptionsToMod(opts)
	where := sqlUR.criteriaToWhere(criteria)

	sqlUsers, err := models.Users(append(mod, where...)...).All(context.Background(), sqlUR.db)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlUsers, func(sqlUser *models.User) model.User {
		return *sqlUR.SqlUserToUser(sqlUser, nil)
	}), nil
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

	return sqlUR.SqlUserToUser(user, roles), nil
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

	return sqlUR.SqlUserToUser(user, roles), nil
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

	return sqlUR.SqlUserToUser(&sqlUser, utils.MapNoError(user.Roles, func(role model.Role) string {
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
	if data.Location != nil {
		user.Location = null.StringFromPtr(data.Location)
		cols = append(cols, models.UserColumns.Location)
	}
	if data.AddMedia != nil {
		for _, media := range data.AddMedia {
			sqlMedia := models.Link{
				Type:      string(media.Type),
				Link:      media.Link,
				ShortCode: media.ShortCode,
				IDUser:    null.Int64From(media.IDUser),
			}

			if err := sqlMedia.Insert(context.Background(), sqlUR.db, boil.Infer()); err != nil {
				return utils.ErrRepositoryFailed
			}
		}
	}
	if data.RemoveMedia.IDUser != 0 {
		if _, err := models.Links(
			models.LinkWhere.ID.IN(data.RemoveMedia.IDs),
			models.LinkWhere.IDUser.EQ(null.Int64From(data.RemoveMedia.IDUser)),
		).DeleteAll(context.Background(), sqlUR.db); err != nil {
			return utils.ErrRepositoryFailed
		}
	}
	if len(cols) == 0 {
		return nil
	}

	_, err = user.Update(context.Background(), sqlUR.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	return nil
}

func (sqlUR sqlUserRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlUR.criteriaToWhere(criteria)

	total, err := models.Users(where...).Count(context.Background(), sqlUR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return total, nil
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

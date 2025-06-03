package profile_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/null/v8"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlProfileRepository struct {
	db                *sql.DB
	sqlUserRepository user_repository.SqlUserRepository
}

type SqlProfileRepository = sqlProfileRepository

func (sqlProfileRepository) SqlProfileToProfile(
	profile *models.Profile,
) model.Profile {
	var avatar *fileModel.Image
	var user *authModel.User
	if profile.R != nil {
		if profile.R.IDAvatarImage != nil {
			sqlAvatar := profile.R.IDAvatarImage

			avatar = &fileModel.Image{
				ID:        sqlAvatar.ID,
				Key:       sqlAvatar.Key,
				MimeType:  sqlAvatar.MimeType,
				Name:      sqlAvatar.Name,
				CreatedAt: sqlAvatar.CreatedAt,
			}

		}

	}
	if profile.R != nil && profile.R.IDUserUser != nil {
		sqlUser := profile.R.IDUserUser

		var roles []authModel.Role
		if profile.R.IDUserUser.R != nil && profile.R.IDUserUser.R.IDUserRolesUsers != nil {
			sqlRoles := profile.R.IDUserUser.R.IDUserRolesUsers

			for _, role := range sqlRoles {
				roles = append(roles, authModel.Role(role.Role))
			}
		}

		user = &authModel.User{
			ID:        sqlUser.ID,
			Roles:     roles,
			Name:      sqlUser.Name,
			Username:  sqlUser.Username,
			CreatedAt: sqlUser.CreatedAt,
		}
	}

	return model.Profile{
		ID:          profile.ID,
		IDUser:      profile.IDUser,
		Likes:       profile.Likes,
		CreatedAt:   profile.CreatedAt,
		Description: profile.Description.String,
		Avatar:      avatar,
		User:        user,
	}
}

func (sqlProfileRepository) SelectOpts(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return mod
	}

	if selectOpts.ID != nil && *selectOpts.ID {
		mod = append(mod, Select(models.ProfileColumns.ID))
	}
	if selectOpts.Avatar != nil && *selectOpts.Avatar {
		mod = append(mod, Select(models.ProfileColumns.IDAvatar))
	}

	return mod
}

func (sqlPR sqlProfileRepository) loadOpts(load *LoadOpts) []QueryMod {
	mod := []QueryMod{}
	if load == nil {
		return mod
	}
	if load.Avatar {
		mod = append(mod, Load(models.ProfileRels.IDAvatarImage))
	}
	if load.User != nil {
		mod = append(mod, Load(
			models.ProfileRels.IDUserUser,
			sqlPR.sqlUserRepository.SelectOpts(load.User)...,
		))
	}
	if load.Roles {
		mod = append(mod, Load(Rels(models.ProfileRels.IDUserUser, models.UserRels.IDUserRolesUsers)))
	}

	return mod
}

func (sqlPR sqlProfileRepository) findOneOptionsToMod(opts *FindOneOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlPR.SelectOpts(opts.SelectOpts)...)
	mod = append(mod, sqlPR.loadOpts(opts.load)...)

	return mod
}

func (sqlPR sqlProfileRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.ID != 0 {
		mod = append(mod, Where("id = ?", criteria.ID))
	}
	if criteria.IDUser != 0 {
		mod = append(mod, Where("id_user = ?", criteria.IDUser))
	}

	return mod
}

func (sqlPR sqlProfileRepository) FindOne(criteria *Criteria, opts *FindOneOptions) (*model.Profile, error) {
	mod := sqlPR.findOneOptionsToMod(opts)
	where := sqlPR.criteriaToWhere(criteria)

	sqlProfile, err := models.Profiles(append(mod, where...)...).One(context.Background(), sqlPR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}

	profile := sqlPR.SqlProfileToProfile(sqlProfile)

	return &profile, nil
}

func (sqlPR sqlProfileRepository) UpdateOne(criteria *Criteria, data UpdateData) error {
	where := sqlPR.criteriaToWhere(criteria)

	sqlProfile, err := models.Profiles(append([]QueryMod{
		Select("id"),
	}, where...)...).One(context.Background(), sqlPR.db)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	ctx := context.Background()
	tx, err := sqlPR.db.BeginTx(ctx, nil)
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	// Set columns
	var cols []string

	if data.Avatar != nil {
		sqlImage := models.Image{
			Key:      data.Avatar.Key,
			Name:     data.Avatar.Name,
			MimeType: data.Avatar.MimeType,
		}
		if err := sqlImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return utils.ErrRepositoryFailed
		}

		sqlProfile.IDAvatar = null.Int64From(sqlImage.ID)
		cols = append(cols, models.ProfileColumns.IDAvatar)
	}
	if data.Description != nil {
		sqlProfile.Description = null.StringFrom(*data.Description)
		cols = append(cols, models.ProfileColumns.Description)
	}

	_, err = sqlProfile.Update(ctx, tx, boil.Whitelist(cols...))
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

func (sqlPR sqlProfileRepository) Find(opts *FindOneOptions) (*[]model.Profile, error) {

	queryStr := `
		SELECT p.*
		FROM profiles p
		LEFT JOIN (
			SELECT
				id_profile,
				COUNT(*) AS total_posts,
				COALESCE(SUM(likes), 0) AS total_post_likes,
				MAX(created_at) AS last_post_date
			FROM posts
			GROUP BY id_profile
		) AS post_stats ON post_stats.id_profile = p.id
		ORDER BY
			post_stats.total_post_likes DESC,
			post_stats.total_posts DESC,
			post_stats.last_post_date DESC NULLS LAST;
	`
	if opts != nil {
		if opts.SelectOpts.Limit != nil {
			queryStr += fmt.Sprintf(" LIMIT %d", *opts.SelectOpts.Limit)
		}
		if opts.SelectOpts.OffSet != nil {
			queryStr += fmt.Sprintf(" OFFSET %d", *opts.SelectOpts.OffSet)
		}
	}

	query := SQL(queryStr)
	profilesSQl, err := models.Profiles(query).All(context.Background(), sqlPR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, err
	}
	profiles := utils.MapNoError(profilesSQl, func(profile *models.Profile) model.Profile {
		return sqlPR.SqlProfileToProfile(profile)
	})
	return &profiles, nil

}

func NewSqlProfileRepository(
	db *sql.DB,
) ProfileRepository {
	return sqlProfileRepository{
		db: db,
		sqlUserRepository: user_repository.SqlExplicitUserRepository(
			db,
		),
	}
}

func SqlExplicitProfileRepository(db *sql.DB) sqlProfileRepository {
	return sqlProfileRepository{
		db: db,
		sqlUserRepository: user_repository.SqlExplicitUserRepository(
			db,
		),
	}
}

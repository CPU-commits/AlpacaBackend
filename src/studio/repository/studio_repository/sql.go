package studio_repository

import (
	"context"
	"database/sql"
	"errors"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlStudioRepository struct {
	db *sql.DB
}

type SqlStudioRepository = sqlStudioRepository

func (sqlSR sqlStudioRepository) SqlStudioToModel(sqlStudio *models.Studio) *model.Studio {
	var avatar *fileModel.Image
	if sqlStudio.R != nil && sqlStudio.R.IDAvatarImage != nil {
		sqlImage := sqlStudio.R.IDAvatarImage

		avatar = &fileModel.Image{
			ID:        sqlImage.ID,
			Key:       sqlImage.Key,
			MimeType:  sqlImage.MimeType,
			Name:      sqlImage.Name,
			CreatedAt: sqlImage.CreatedAt,
		}
	}
	var banner *fileModel.Image
	if sqlStudio.R != nil && sqlStudio.R.IDBannerImage != nil {
		sqlBanner := sqlStudio.R.IDBannerImage

		banner = &fileModel.Image{
			ID:        sqlBanner.ID,
			Key:       sqlBanner.Key,
			MimeType:  sqlBanner.MimeType,
			Name:      sqlBanner.Name,
			CreatedAt: sqlBanner.CreatedAt,
		}
	}
	var owner *authModel.User
	if sqlStudio.IDOwner != 0 {
		owner = &authModel.User{
			ID: sqlStudio.IDOwner,
		}
	}
	var media []model.Media
	if sqlStudio.R != nil && sqlStudio.R.IDStudioLinks != nil {
		sqlMedias := sqlStudio.R.IDStudioLinks

		for _, sqlMedia := range sqlMedias {
			media = append(media, model.Media{
				ID:        sqlMedia.ID,
				Link:      sqlMedia.Link,
				ShortCode: sqlMedia.ShortCode,
				Type:      model.TypeMedia(sqlMedia.Type),
			})
		}
	}

	return &model.Studio{
		ID:          sqlStudio.ID,
		Name:        sqlStudio.Name,
		Username:    sqlStudio.Username,
		Email:       sqlStudio.Email,
		Avatar:      avatar,
		Banner:      banner,
		Description: sqlStudio.Description.String,
		FullAddress: sqlStudio.FullAddress,
		Phone:       sqlStudio.Phone.String,
		CreatedAt:   sqlStudio.CreatedAt,
		Owner:       owner,
		IDAvatar:    sqlStudio.IDAvatar.Int64,
		IDBanner:    sqlStudio.IDBanner.Int64,
		Media:       media,
	}
}

func (sqlSR sqlStudioRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return nil
	}
	if criteria.ID != 0 {
		where = append(where, models.StudioWhere.ID.EQ(criteria.ID))
	}
	if criteria.IDOwner != 0 {
		where = append(where, models.StudioWhere.IDOwner.EQ(criteria.IDOwner))
	}
	if criteria.Email != "" {
		where = append(where, models.StudioWhere.Email.EQ(criteria.Email))
	}
	if criteria.Username != "" {
		where = append(where, models.StudioWhere.Username.EQ(criteria.Username))
	}
	var orMods []QueryMod
	for _, clause := range criteria.OR {
		orWhere := sqlSR.criteriaToWhere(&clause)
		orMods = append(orMods, Or2(Expr(orWhere...)))
	}
	if orMods != nil {
		where = append(where, Expr(orMods...))
	}

	return where
}

func (sqlSR sqlStudioRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlSR.criteriaToWhere(criteria)

	exists, err := models.Studios(where...).Exists(context.Background(), sqlSR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlSR sqlStudioRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlSR.criteriaToWhere(criteria)

	count, err := models.Studios(where...).Count(context.Background(), sqlSR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return count, nil
}

func (sqlStudioRepository) includeToMod(include *Include, hasSelect bool) []QueryMod {
	if include == nil {
		return nil
	}
	mod := []QueryMod{}
	if include.AvatarImage {
		if hasSelect {
			mod = append(mod, Select(models.StudioColumns.IDAvatar))
		}

		mod = append(mod, Load(models.StudioRels.IDAvatarImage))
	}
	if include.BannerImage {
		if hasSelect {
			mod = append(mod, Select(models.StudioColumns.IDBanner))
		}

		mod = append(mod, Load(models.StudioRels.IDBannerImage))
	}
	if include.Media {
		mod = append(mod, Load(models.StudioRels.IDStudioLinks))
	}

	return mod
}

func (sqlSR sqlStudioRepository) SelectToMod(selectOpts *SelectOpts) []QueryMod {
	if selectOpts == nil {
		return nil
	}
	mod := []QueryMod{}
	if selectOpts.Name {
		mod = append(mod, Select(models.StudioColumns.Name))
	}
	if selectOpts.Username {
		mod = append(mod, Select(models.StudioColumns.Username))
	}
	if selectOpts.Description {
		mod = append(mod, Select(models.StudioColumns.Description))
	}
	if selectOpts.ID {
		mod = append(mod, Select(models.StudioColumns.ID))
	}
	if selectOpts.IDOwner {
		mod = append(mod, Select(models.StudioColumns.IDOwner))
	}
	if selectOpts.Address != nil && *selectOpts.Address {
		mod = append(mod, Select(models.StudioColumns.FullAddress))
	}
	if selectOpts.IDAvatar != nil && *selectOpts.IDAvatar {
		mod = append(mod, Select(models.StudioColumns.IDAvatar))
	}
	if selectOpts.IDBanner != nil && *selectOpts.IDBanner {
		mod = append(mod, Select(models.StudioColumns.IDBanner))
	}

	return mod
}

func (sqlSR sqlStudioRepository) findOptionsToMod(opts *findOptions) []QueryMod {
	if opts == nil {
		return nil
	}
	mod := []QueryMod{}
	mod = append(mod, sqlSR.includeToMod(opts.include, opts.selectOpts != nil)...)
	mod = append(mod, sqlSR.SelectToMod(opts.selectOpts)...)

	return mod
}

func (sqlSR sqlStudioRepository) Find(criteria *Criteria, opts *findOptions) ([]model.Studio, error) {
	where := sqlSR.criteriaToWhere(criteria)
	mod := sqlSR.findOptionsToMod(opts)

	sqlStudios, err := models.Studios(append(mod, where...)...).All(context.Background(), sqlSR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlStudios, func(sqlStudio *models.Studio) model.Studio {
		return *sqlSR.SqlStudioToModel(sqlStudio)
	}), nil
}

func (sqlSR sqlStudioRepository) findOneOptionsToMod(opts *findOneOptions) []QueryMod {
	if opts == nil {
		return nil
	}
	mod := []QueryMod{}
	mod = append(mod, sqlSR.includeToMod(opts.include, false)...)
	mod = append(mod, sqlSR.SelectToMod(opts.selectOpts)...)

	return mod
}

func (sqlSR sqlStudioRepository) FindOne(criteria *Criteria, opts *findOneOptions) (*model.Studio, error) {
	where := sqlSR.criteriaToWhere(criteria)
	mod := sqlSR.findOneOptionsToMod(opts)

	sqlStudio, err := models.Studios(append(mod, where...)...).One(context.Background(), sqlSR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlSR.SqlStudioToModel(sqlStudio), nil
}

func (sqlSR sqlStudioRepository) InsertOne(studio model.Studio) error {
	tx, err := sqlSR.db.Begin()
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	sqlStudio := models.Studio{
		Description: null.NewString(studio.Description, studio.Description != ""),
		Email:       studio.Email,
		Phone:       null.NewString(studio.Phone, studio.Phone != ""),
		Name:        studio.Name,
		Username:    studio.Username,
		FullAddress: studio.FullAddress,
		IDOwner:     studio.Owner.ID,
	}
	// Images
	if studio.Avatar != nil {
		avatar := studio.Avatar

		sqlImage := models.Image{
			Key:      avatar.Key,
			MimeType: avatar.MimeType,
			Name:     avatar.Name,
		}
		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return utils.ErrRepositoryFailed
		}
		sqlStudio.IDAvatar = null.Int64From(sqlImage.ID)
	}
	if studio.Banner != nil {
		banner := studio.Banner

		sqlImage := models.Image{
			Key:      banner.Key,
			MimeType: banner.MimeType,
			Name:     banner.Name,
		}
		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return utils.ErrRepositoryFailed
		}
		sqlStudio.IDBanner = null.Int64From(sqlImage.ID)
	}

	if err := sqlStudio.Insert(context.Background(), tx, boil.Infer()); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}
	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlSR sqlStudioRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlSR.criteriaToWhere(criteria)
	cols := models.M{}
	if data.Description != nil && *data.Description != "" {
		cols[models.StudioColumns.Description] = *data.Description
	} else if data.Description != nil {
		cols[models.StudioColumns.Description] = nil
	}
	if data.Name != "" {
		cols[models.StudioColumns.Name] = data.Name
	}
	if data.FullAddress != "" {
		cols[models.StudioColumns.FullAddress] = data.FullAddress
	}
	if data.Email != "" {
		cols[models.StudioColumns.Email] = data.Email
	}
	if data.Phone != nil && *data.Phone != "" {
		cols[models.StudioColumns.Phone] = *data.Phone
	} else if data.Phone != nil {
		cols[models.StudioColumns.Phone] = nil
	}
	tx, err := sqlSR.db.Begin()
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	if data.AddMedia != nil {
		for _, media := range data.AddMedia {
			sqlMedia := models.Link{
				Type:      string(media.Type),
				Link:      media.Link,
				ShortCode: media.ShortCode,
				IDStudio:  null.Int64From(media.IDStudio),
			}

			if err := sqlMedia.Insert(context.Background(), tx, boil.Infer()); err != nil {
				tx.Rollback()
				return utils.ErrRepositoryFailed
			}
		}
	}
	if data.RemoveMedia != nil {
		if _, err := models.Links(
			models.LinkWhere.ID.IN(data.RemoveMedia),
		).DeleteAll(context.Background(), tx); err != nil {
			tx.Rollback()
			return utils.ErrRepositoryFailed
		}
	}

	if data.Avatar != nil {
		sqlImage := models.Image{
			Key:       data.Avatar.Key,
			MimeType:  data.Avatar.MimeType,
			Name:      data.Avatar.Name,
			CreatedAt: data.Avatar.CreatedAt,
		}
		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()
			return utils.ErrRepositoryFailed
		}

		cols[models.StudioColumns.IDAvatar] = sqlImage.ID
	}
	if data.Banner != nil {
		sqlImage := models.Image{
			Key:       data.Banner.Key,
			MimeType:  data.Banner.MimeType,
			Name:      data.Banner.Name,
			CreatedAt: data.Banner.CreatedAt,
		}
		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()
			return utils.ErrRepositoryFailed
		}

		cols[models.StudioColumns.IDBanner] = sqlImage.ID
	}

	_, err = models.Studios(where...).UpdateAll(context.Background(), tx, cols)
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

func NewSqlStudioRepository() StudioRepository {
	return sqlStudioRepository{
		db: db.DB,
	}
}

func SqlExplicitStudioRepository() sqlStudioRepository {
	return sqlStudioRepository{
		db: db.DB,
	}
}

package tattoo_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strconv"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
	"github.com/typesense/typesense-go/v3/typesense"
)

type sqlTattooRepository struct {
	db                 *sql.DB
	ts                 *typesense.Client
	tsTattooRepository tsTattooRepository
}

func (sqlTattooRepository) sqlTattooToTattoo(sqlTattoo *models.Tattoo) model.Tattoo {
	tattoo := model.Tattoo{
		ID:            sqlTattoo.ID,
		Likes:         sqlTattoo.Likes,
		CreatedAt:     sqlTattoo.CreatedAt,
		Views:         sqlTattoo.Views,
		IDPublication: sqlTattoo.IDPost.Int64,
		Description:   sqlTattoo.Description.String,
		Categories:    sqlTattoo.Categories,
	}
	if sqlTattoo.R != nil && sqlTattoo.R.IDImageImage != nil {
		sqlImage := sqlTattoo.R.IDImageImage
		image := fileModel.Image{
			ID:        sqlImage.ID,
			Key:       sqlImage.Key,
			Name:      sqlImage.Name,
			MimeType:  sqlImage.MimeType,
			CreatedAt: sqlImage.CreatedAt,
		}

		tattoo.Image = image
	}

	if sqlTattoo.R != nil && sqlTattoo.R.IDProfileProfile != nil {
		profile := sqlTattoo.R.IDProfileProfile
		tattoo.Profile = &userModel.Profile{
			ID:          profile.ID,
			Description: profile.Description.String,
			IDUser:      profile.IDUser,
			Likes:       profile.Likes,
			CreatedAt:   profile.CreatedAt,
		}

		if sqlTattoo.R.IDProfileProfile.R.IDAvatarImage != nil {
			avatar := sqlTattoo.R.IDProfileProfile.R.IDAvatarImage
			tattoo.Profile.Avatar = &fileModel.Image{
				ID:       avatar.ID,
				Name:     avatar.Name,
				MimeType: avatar.MimeType,
				Key:      avatar.Key,
			}
		}
		if sqlTattoo.R.IDProfileProfile.R.IDUserUser != nil {
			sqlUser := sqlTattoo.R.IDProfileProfile.R.IDUserUser
			tattoo.Profile.User = &authModel.User{
				ID:       sqlTattoo.ID,
				Name:     sqlUser.Name,
				Username: sqlUser.Username,
			}
		}
	}

	return tattoo
}

func (sqlTattooRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.ID != 0 {
		mod = append(mod, models.TattooWhere.ID.EQ(criteria.ID))
	}
	if criteria.IDs != nil {
		mod = append(mod, models.TattooWhere.ID.IN(criteria.IDs))
	}
	if criteria.IDProfile != 0 {
		mod = append(mod, models.TattooWhere.IDProfile.EQ(criteria.IDProfile))
	}
	if criteria.IDStudio != 0 {
		mod = append(mod, models.TattooWhere.IDStudio.EQ(null.Int64From(criteria.IDStudio)))
	}

	return mod
}

func (sqlTattooRepository) tattooModelToSqlTattoo(tattoo model.Tattoo, idProfile, sqlImageID int64) models.Tattoo {
	sqlTattoo := models.Tattoo{
		Likes:      tattoo.Likes,
		IDProfile:  idProfile,
		IDImage:    sqlImageID,
		Categories: tattoo.Categories,
		IDStudio:   null.Int64FromPtr(tattoo.IDStudio),
	}
	if tattoo.Description != "" {
		sqlTattoo.Description = null.StringFrom(tattoo.Description)
	}
	if tattoo.Coord != nil {
		sqlTattoo.Coordinate = null.StringFrom(fmt.Sprintf("Point(%v %v)", tattoo.Coord.X, tattoo.Coord.Y))
	}
	if tattoo.IDPublication != 0 {
		sqlTattoo.IDPost = null.Int64From(tattoo.IDPublication)
	}

	return sqlTattoo
}

func (sqlTR sqlTattooRepository) Insert(tattoos []model.Tattoo, idProfile int64) ([]model.Tattoo, error) {
	ctx := context.Background()
	tx, err := sqlTR.db.BeginTx(ctx, nil)
	if err != nil {

		return nil, utils.ErrRepositoryFailed
	}
	newTattoos := tattoos

	for i, tattoo := range tattoos {
		sqlImage := models.Image{
			Key:      tattoo.Image.Key,
			Name:     tattoo.Image.Name,
			MimeType: tattoo.Image.MimeType,
		}
		if err := sqlImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
		tattoo.Image = fileModel.Image{
			ID:        sqlImage.ID,
			Key:       tattoo.Image.Key,
			Name:      tattoo.Image.Name,
			MimeType:  tattoo.Image.MimeType,
			CreatedAt: sqlImage.CreatedAt,
		}

		sqlTattoo := sqlTR.tattooModelToSqlTattoo(
			tattoo,
			idProfile,
			sqlImage.ID,
		)

		if err := sqlTattoo.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
		tattoo.ID = sqlTattoo.ID
		tattoo.CreatedAt = sqlTattoo.CreatedAt

		newTattoos[i] = tattoo
	}
	if err := tx.Commit(); err != nil {

		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}

	return newTattoos, nil
}

func (sqlTR sqlTattooRepository) ConvertImageInTattoo(
	idImages []int64,
	tattoos []model.Tattoo,
	idProfile int64,
) ([]model.Tattoo, error) {
	tx, err := sqlTR.db.BeginTx(context.Background(), nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	if _, err := models.PostImages(models.PostImageWhere.IDImage.IN(idImages)).DeleteAll(context.Background(), tx); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}
	var newTattoos []models.Tattoo
	for _, tattoo := range tattoos {
		sqlTattoo := sqlTR.tattooModelToSqlTattoo(
			tattoo,
			idProfile,
			tattoo.Image.ID,
		)

		if err := sqlTattoo.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}

		newTatto, err := models.Tattoos(
			Where("id_image = ?", tattoo.Image.ID),
			Load(models.TattooRels.IDImageImage),
			Load(models.TattooRels.IDProfileProfile),
		).One(context.Background(), tx)
		if err != nil {
			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}
		newTattoos = append(newTattoos, *newTatto)
	}
	if err := tx.Commit(); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(newTattoos, func(tattoo models.Tattoo) model.Tattoo {
		return sqlTR.sqlTattooToTattoo(&tattoo)
	}), nil
}

func (sqlTattooRepository) includeOpts(include *Include) []QueryMod {
	mod := []QueryMod{}
	if include == nil {
		return mod
	}
	if include.Image {
		mod = append(mod, Load(models.TattooRels.IDImageImage))
	}
	if include.ProfileAvatar || include.ProfileUser {
		mod = append(mod, Load(
			models.TattooRels.IDProfileProfile,
			Select("id"),
			Select("id_avatar"),
			Select("id_user"),
		))
	}
	if include.ProfileAvatar {
		mod = append(mod, Load(Rels(
			models.TattooRels.IDProfileProfile,
			models.ProfileRels.IDAvatarImage,
		)))
	}
	if include.ProfileUser {
		mod = append(mod, Load(Rels(
			models.TattooRels.IDProfileProfile,
			models.ProfileRels.IDUserUser,
		)))
	}

	return mod
}

func (sqlTattooRepository) sortOpts(sort *Sort) []QueryMod {
	mod := []QueryMod{}
	if sort == nil {
		return mod
	}
	if sort.CreatedAt != "" {
		mod = append(mod, OrderBy(fmt.Sprintf("created_at %s", sort.CreatedAt)))
	}

	return mod
}

func (sqlTR sqlTattooRepository) findOptionsToMod(opts *FindOpts) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlTR.includeOpts(opts.include)...)
	mod = append(mod, sqlTR.sortOpts(opts.sort)...)
	if opts.limit != nil {
		mod = append(mod, Limit(*opts.limit))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(*opts.skip))
	}

	return mod
}

func (sqlTR sqlTattooRepository) Find(criteria *Criteria, opts *FindOpts) ([]model.Tattoo, error) {
	mod := sqlTR.findOptionsToMod(opts)
	where := sqlTR.criteriaToWhere(criteria)

	tattoos, err := models.Tattoos(append(mod, where...)...).All(context.Background(), sqlTR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(tattoos, func(sqlTattoo *models.Tattoo) model.Tattoo {
		return sqlTR.sqlTattooToTattoo(sqlTattoo)
	}), nil
}

func (sqlTR sqlTattooRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlTR.criteriaToWhere(criteria)

	count, err := models.Tattoos(where...).Count(context.Background(), sqlTR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return count, nil
}

func (sqlTR sqlTattooRepository) UpdateViews(ids []int64) error {
	var in string = "("
	for i, id := range ids {
		if i != 0 {
			in += ","
		}

		in += strconv.Itoa(int(id))
	}

	in += ")"

	_, err := sqlTR.db.Exec(
		fmt.Sprintf("update tattoos set views = views + 1 where id in %s", in),
	)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlTR sqlTattooRepository) Update(criteria *Criteria, data UpdateData) error {
	where := sqlTR.criteriaToWhere(criteria)

	sqlTattoo, err := models.Tattoos(append([]QueryMod{
		Select("id"),
	}, where...)...).One(context.Background(), sqlTR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return utils.ErrRepositoryFailed
	}
	// Cols
	var cols []string
	if data.UnsetIDPublication {
		sqlTattoo.IDPost = null.Int64FromPtr(nil)
		cols = append(cols, models.TattooColumns.IDPost)
	}

	_, err = sqlTattoo.Update(context.Background(), sqlTR.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlTR sqlTattooRepository) tattooSimilarityOptionsToMod(opts *SimilarityOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlTR.includeOpts(opts.include)...)
	if opts.limit != nil {
		mod = append(mod, Limit(*opts.limit))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(*opts.skip))
	}

	return mod
}

func (sqlTR sqlTattooRepository) TattooSimilarity(
	params SimilarityParams,
	opts *SimilarityOptions,
) ([]model.Tattoo, int64, error) {
	idTattoos, found, err := sqlTR.tsTattooRepository.Search(
		params,
		opts,
	)
	if err != nil {
		return nil, 0, utils.ErrRepositoryFailed
	}
	mod := sqlTR.tattooSimilarityOptionsToMod(opts)

	tattoos, err := models.Tattoos(append(mod, models.TattooWhere.ID.IN(idTattoos))...).All(context.Background(), sqlTR.db)
	if err != nil {
		return nil, 0, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(tattoos, func(sqlTattoo *models.Tattoo) model.Tattoo {
		return sqlTR.sqlTattooToTattoo(sqlTattoo)
	}), found, nil
}

func (sqlTR sqlTattooRepository) findOneOptionsToMod(opts *FindOneOpts) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlTR.includeOpts(opts.include)...)

	return mod
}

func (sqlTR sqlTattooRepository) FindOne(criteria *Criteria, opts *FindOneOpts) (*model.Tattoo, error) {
	where := sqlTR.criteriaToWhere(criteria)
	mod := sqlTR.findOneOptionsToMod(opts)

	sqlTattoo, err := models.Tattoos(append(mod, where...)...).One(context.Background(), sqlTR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}
	tattoo := sqlTR.sqlTattooToTattoo(sqlTattoo)

	return &tattoo, nil
}

func NewSqlTattooRepository(sqlDB *sql.DB) TattooRepository {
	return sqlTattooRepository{
		db:                 sqlDB,
		ts:                 db.TSClient,
		tsTattooRepository: NewTsTattooRepository(),
	}
}

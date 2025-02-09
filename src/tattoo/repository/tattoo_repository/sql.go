package tattoo_repository

import (
	"context"
	"database/sql"
	"fmt"
	"strconv"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/null/v8"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlTattooRepository struct {
	db *sql.DB
}

func (sqlTattooRepository) sqlTattooToTattoo(sqlTattoo *models.Tattoo) model.Tattoo {
	tattoo := model.Tattoo{
		ID:          sqlTattoo.ID,
		Likes:       sqlTattoo.Likes,
		CreatedAt:   sqlTattoo.CreatedAt,
		Views:       sqlTattoo.Views,
		Description: sqlTattoo.Description.String,
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
	if sqlTattoo.R != nil && sqlTattoo.R.IDTattooTattooCategories != nil {
		var categories []model.Category

		for _, sqlCategoryTattoo := range sqlTattoo.R.IDTattooTattooCategories {
			sqlCategory := sqlCategoryTattoo.R.IDCategoryCategory

			categories = append(categories, model.Category{
				ID:          sqlCategory.ID,
				Name:        sqlCategory.Name,
				Description: sqlCategory.Description,
				Slug:        sqlCategory.Slug,
				CreatedAt:   sqlCategory.CreatedAt,
				State:       sqlCategory.State,
			})
		}
		tattoo.Categories = categories
	}
	if sqlTattoo.R != nil && sqlTattoo.R.IDProfileProfile != nil {
		tattoo.Profile = &userModel.Profile{}

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
	if criteria.IDs != nil {
		mod = append(mod, models.TattooWhere.ID.IN(criteria.IDs))
	}
	if criteria.IDProfile != 0 {
		mod = append(mod, models.TattooWhere.IDProfile.EQ(criteria.IDProfile))
	}

	return mod
}

func (sqlTR sqlTattooRepository) Insert(tattoos []model.Tattoo, idProfile int64) error {
	ctx := context.Background()
	tx, err := sqlTR.db.BeginTx(ctx, nil)
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	for _, tattoo := range tattoos {
		sqlImage := models.Image{
			Key:      tattoo.Image.Key,
			Name:     tattoo.Image.Name,
			MimeType: tattoo.Image.MimeType,
		}
		if err := sqlImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return utils.ErrRepositoryFailed
		}

		sqlTattoo := models.Tattoo{
			Likes:     tattoo.Likes,
			IDProfile: idProfile,
			IDImage:   sqlImage.ID,
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

		if err := sqlTattoo.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return utils.ErrRepositoryFailed
		}
		for _, idCategory := range tattoo.IDCategories {
			sqlCategoryTattoo := models.TattooCategory{
				IDTattoo:   sqlTattoo.ID,
				IDCategory: idCategory,
			}
			if err := sqlCategoryTattoo.Insert(ctx, tx, boil.Infer()); err != nil {
				tx.Rollback()

				return utils.ErrRepositoryFailed
			}
		}
	}
	if err := tx.Commit(); err != nil {
		tx.Rollback()

		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlTattooRepository) includeOpts(include *Include) []QueryMod {
	mod := []QueryMod{}
	if include == nil {
		return mod
	}
	if include.Image {
		mod = append(mod, Load(models.TattooRels.IDImageImage))
	}
	if include.Categories {
		mod = append(mod, Load(Rels(
			models.TattooRels.IDTattooTattooCategories,
			models.TattooCategoryRels.IDCategoryCategory,
		)))
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

func NewSqlTattooRepository(db *sql.DB) TattooRepository {
	return sqlTattooRepository{
		db: db,
	}
}

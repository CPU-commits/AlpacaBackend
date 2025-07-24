package design_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type sqlDesignRepository struct {
	db *sql.DB
}

func NewSqlDesignRepository(sqlDB *sql.DB) DesignRepository {
	return &sqlDesignRepository{db: sqlDB}
}

func (sqlDS *sqlDesignRepository) sqlDesignToDesign(sqlDR *models.Design) *model.Design {

	design := &model.Design{
		ID:          sqlDR.ID,
		IDProfile:   sqlDR.IDProfile,
		CreatedAt:   sqlDR.CreatedAt,
		Categories:  sqlDR.Categories,
		Price:       sqlDR.Price.Int64,
		Description: sqlDR.Description.String,
	}
	if sqlDR.R != nil && sqlDR.R.IDImageImage != nil {
		image := sqlDR.R.IDImageImage
		design.Image = fileModel.Image{
			ID:        image.ID,
			Key:       image.Key,
			Name:      image.Name,
			MimeType:  image.MimeType,
			CreatedAt: image.CreatedAt,
		}
	}
	if sqlDR.R != nil && sqlDR.R.IDProfileProfile != nil {
		profile := sqlDR.R.IDProfileProfile
		design.Profile = &userModel.Profile{
			ID:          profile.ID,
			Description: profile.Description.String,
			IDUser:      profile.IDUser,
			Likes:       profile.Likes,
			CreatedAt:   profile.CreatedAt,
		}
		if profile.R != nil && profile.R.IDAvatarImage != nil {
			avatar := profile.R.IDAvatarImage
			design.Profile.Avatar = &fileModel.Image{
				ID:        avatar.ID,
				Key:       avatar.Key,
				Name:      avatar.Name,
				MimeType:  avatar.MimeType,
				CreatedAt: avatar.CreatedAt,
			}
		}
		if profile.R != nil && profile.R.IDUserUser != nil {
			user := profile.R.IDUserUser
			design.Profile.User = &authModel.User{
				ID:       user.ID,
				Name:     user.Name,
				Username: user.Username,
			}
		}
	}

	return design
}

func (*sqlDesignRepository) criteriaToWhere(c *Criteria) []QueryMod {
	mods := []QueryMod{}
	if c == nil {
		return mods
	}
	if c.ID != 0 {
		mods = append(mods, models.DesignWhere.ID.EQ(c.ID))
	}
	if c.IDs != nil {
		mods = append(mods, models.DesignWhere.ID.IN(c.IDs))
	}
	if c.IDProfile != 0 {
		mods = append(mods, models.DesignWhere.IDProfile.EQ(c.IDProfile))
	}
	if c.Category != "" {
		mods = append(mods, Where("categories && ARRAY[?]::text[]", c.Category))
	}
	return mods
}

func (*sqlDesignRepository) includeOpts(i *Include) []QueryMod {
	mods := []QueryMod{}
	if i == nil {
		return mods
	}
	if i.Image {
		mods = append(mods, Load(models.DesignRels.IDImageImage))
	}
	if i.ProfileAvatar || i.ProfileUser {
		mods = append(mods, Load(
			models.DesignRels.IDProfileProfile,
			Select("id", "id_avatar", "id_user"),
		))
	}
	if i.ProfileAvatar {
		mods = append(mods, Load(Rels(
			models.DesignRels.IDProfileProfile,
			models.ProfileRels.IDAvatarImage,
		)))
	}
	if i.ProfileUser {
		mods = append(mods, Load(Rels(
			models.DesignRels.IDProfileProfile,
			models.ProfileRels.IDUserUser,
		)))
	}
	return mods
}

func (*sqlDesignRepository) sortOpts(s *Sort) []QueryMod {
	mods := []QueryMod{}
	if s == nil {
		return mods
	}

	var whereClauses []string
	var orderClauses []string

	if s.Price != "" {
		whereClauses = append(whereClauses, "price > 0")
		orderClauses = append(orderClauses, fmt.Sprintf("price %s", s.Price))
	}
	if s.CreatedAt != "" {
		orderClauses = append(orderClauses, fmt.Sprintf("created_at %s", s.CreatedAt))
	}
	if len(whereClauses) > 0 {
		mods = append(mods, Where(strings.Join(whereClauses, " AND ")))
	}
	if len(orderClauses) > 0 {
		mods = append(mods, OrderBy(strings.Join(orderClauses, ", ")))
	}

	return mods
}

func (sqlDS sqlDesignRepository) findOptionsToMod(o *FindOpts) []QueryMod {
	mods := []QueryMod{}
	if o == nil {
		return mods
	}
	mods = append(mods, sqlDS.includeOpts(o.include)...)
	mods = append(mods, sqlDS.sortOpts(o.sort)...)
	if o.limit != nil {
		mods = append(mods, Limit(*o.limit))
	}
	if o.skip != nil {
		mods = append(mods, Offset(*o.skip))
	}
	return mods
}

func (sqlDS sqlDesignRepository) findOneOptionsToMod(o *FindOneOpts) []QueryMod {
	mod := []QueryMod{}

	if o == nil {
		return mod
	}
	mod = append(mod, sqlDS.includeOpts(o.include)...)
	return mod
}

func (sqlDS *sqlDesignRepository) Find(c *Criteria, o *FindOpts) ([]model.Design, error) {
	mods := append(sqlDS.findOptionsToMod(o), sqlDS.criteriaToWhere(c)...)
	sqlRes, err := models.Designs(mods...).All(context.Background(), sqlDS.db)
	if err != nil {
		fmt.Printf("err: %v\n", err)
		return nil, utils.ErrRepositoryFailed
	}
	return utils.MapNoError(sqlRes, func(d *models.Design) model.Design {
		return *sqlDS.sqlDesignToDesign(d)
	}), nil
}

func (sqlDS *sqlDesignRepository) Count(c *Criteria) (int64, error) {
	count, err := models.Designs(sqlDS.criteriaToWhere(c)...).Count(context.Background(), sqlDS.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}
	return count, nil
}

func (sqlDS *sqlDesignRepository) Insert(designs []model.Design, idProfile int64) ([]model.Design, error) {
	ctx := context.Background()
	tx, err := sqlDS.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	newDesigns := designs
	for i, design := range designs {
		sqlImage := models.Image{
			Key:      design.Image.Key,
			Name:     design.Image.Name,
			MimeType: design.Image.MimeType,
		}
		if err := sqlImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}
		sqlDesign := models.Design{
			IDProfile:  idProfile,
			IDImage:    sqlImage.ID,
			Categories: design.Categories,
			Price:      null.Int64From(design.Price),
		}
		if design.Description != "" {
			sqlDesign.Description = null.StringFrom(design.Description)
		}
		if err := sqlDesign.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()
			return nil, utils.ErrRepositoryFailed
		}

		design.ID = sqlDesign.ID
		design.CreatedAt = sqlDesign.CreatedAt

		newDesigns[i] = design
	}
	if err := tx.Commit(); err != nil {
		tx.Rollback()
		return nil, utils.ErrRepositoryFailed
	}
	return newDesigns, nil
}

func (sqlDS *sqlDesignRepository) Update(c *Criteria, data UpdateData) error {
	ctx := context.Background()

	sd, err := models.Designs(append([]QueryMod{Select("id")}, sqlDS.criteriaToWhere(c)...)...).One(ctx, sqlDS.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		return utils.ErrRepositoryFailed
	}
	cols := []string{}
	if data.Description != nil {
		sd.Description = null.StringFromPtr(data.Description)
		cols = append(cols, models.DesignColumns.Description)
	}
	if data.Categories != nil {
		sd.Categories = *data.Categories
		cols = append(cols, models.DesignColumns.Categories)
	}
	if data.Price != nil {
		sd.Price = null.Int64FromPtr(data.Price)
		cols = append(cols, models.DesignColumns.Price)
	}
	if len(cols) == 0 {
		return nil
	}
	_, err = sd.Update(ctx, sqlDS.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	return nil
}

func (sqlDS *sqlDesignRepository) FindOne(c *Criteria, o *FindOneOpts) (*model.Design, error) {
	where := sqlDS.criteriaToWhere(c)
	mod := sqlDS.findOneOptionsToMod(o)
	sqlDesign, err := models.Designs(append(where, mod...)...).One(context.Background(), sqlDS.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}
		return nil, utils.ErrRepositoryFailed
	}

	return sqlDS.sqlDesignToDesign(sqlDesign), nil
}

func (sqlDS *sqlDesignRepository) Delete(c *Criteria) error {
	where := sqlDS.criteriaToWhere(c)

	if _, err := models.Designs(where...).DeleteAll(context.Background(), sqlDS.db); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}
func (sqlDS *sqlDesignRepository) GetCategories(c *Criteria) ([]string, error) {
	where := sqlDS.criteriaToWhere(c)
	sqlDesigns, err := models.Designs(where...).All(context.Background(), sqlDS.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	set := utils.NewSet[string]()
	utils.ConcurrentForEach(sqlDesigns, func(d *models.Design) error {
		set.Add(d.Categories...)
		return nil
	}, nil)

	return set.Values(), nil
}

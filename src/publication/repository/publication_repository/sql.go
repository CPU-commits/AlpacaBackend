package publication_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/volatiletech/sqlboiler/v4/boil"
	. "github.com/volatiletech/sqlboiler/v4/queries/qm"
)

type sqlPublicationRepository struct {
	db                *sql.DB
	sqlUserRepository user_repository.SqlUserRepository
}

func (sqlPublicationRepository) sqlPostToPublication(
	post *models.Post,
) *model.Publication {
	var images []fileModel.Image
	var tattoos []tattooModel.Tattoo
	var categories []tattooModel.Category
	var profile *userModel.Profile

	if post.R != nil && post.R.IDPostPostImages != nil {
		for _, postImage := range post.R.IDPostPostImages {
			image := postImage.R.IDImageImage
			images = append(images, fileModel.Image{
				ID:        image.ID,
				Key:       image.Key,
				MimeType:  image.MimeType,
				Name:      image.Name,
				CreatedAt: image.CreatedAt,
			})
		}
	}
	if post.R != nil && post.R.IDPostTattoos != nil {
		for _, sqlTattoo := range post.R.IDPostTattoos {
			tattoo := tattooModel.Tattoo{
				ID:          sqlTattoo.ID,
				Likes:       sqlTattoo.Likes,
				Description: sqlTattoo.Description.String,
				CreatedAt:   sqlTattoo.CreatedAt,
			}
			if sqlTattoo.R != nil && sqlTattoo.R.IDImageImage != nil {
				sqlImage := sqlTattoo.R.IDImageImage

				tattoo.Image = fileModel.Image{
					ID:        sqlImage.ID,
					Name:      sqlImage.Name,
					MimeType:  sqlImage.MimeType,
					Key:       sqlImage.Key,
					CreatedAt: sqlImage.CreatedAt,
				}
			}

			tattoos = append(tattoos, tattoo)
		}
	}
	if post.R != nil && post.R.IDPostPostCategories != nil {
		for _, postCategory := range post.R.IDPostPostCategories {
			sqlCategory := postCategory.R.IDCategoryCategory

			categories = append(categories, tattooModel.Category{
				ID:          sqlCategory.ID,
				Name:        sqlCategory.Name,
				State:       sqlCategory.State,
				Slug:        sqlCategory.Slug,
				Description: sqlCategory.Description,
			})
		}
	}
	if post.R != nil && post.R.IDProfileProfile != nil {
		sqlProfile := post.R.IDProfileProfile
		profile = &userModel.Profile{
			ID: sqlProfile.ID,
		}
		if sqlProfile.R != nil && sqlProfile.R.IDAvatarImage != nil {
			sqlAvatar := sqlProfile.R.IDAvatarImage
			profile.Avatar = &fileModel.Image{
				ID:        sqlAvatar.ID,
				Key:       sqlAvatar.Key,
				MimeType:  sqlAvatar.MimeType,
				Name:      sqlAvatar.Name,
				CreatedAt: sqlAvatar.CreatedAt,
			}
		}
		if sqlProfile.R != nil && sqlProfile.R.IDUserUser != nil {
			sqlUser := sqlProfile.R.IDUserUser
			profile.User = &authModel.User{
				ID:        sqlUser.ID,
				Email:     sqlUser.Email,
				Name:      sqlUser.Name,
				Username:  sqlUser.Username,
				CreatedAt: sqlUser.CreatedAt,
			}
		}
	}

	return &model.Publication{
		Content:    post.Content,
		ID:         post.ID,
		Likes:      post.Likes,
		Images:     images,
		CreatedAt:  post.CreatedAt,
		Tattoos:    tattoos,
		Categories: categories,
		Profile:    profile,
		IDProfile:  post.IDProfile,
	}
}

func (sqlPublicationRepository) criteriaToWhere(criteria *Criteria) []QueryMod {
	mod := []QueryMod{}
	if criteria == nil {
		return mod
	}
	if criteria.ID != 0 {
		mod = append(mod, Where("id = ?", criteria.ID))
	}
	if criteria.IDProfile != 0 {
		mod = append(mod, Where("id_profile = ?", criteria.IDProfile))
	}

	return mod
}

func (sqlPR sqlPublicationRepository) Count(criteria *Criteria) (int64, error) {
	where := sqlPR.criteriaToWhere(criteria)

	count, err := models.Posts(where...).Count(context.Background(), sqlPR.db)
	if err != nil {
		return 0, err
	}
	return count, nil
}

func (sqlPR sqlPublicationRepository) Insert(
	publication model.Publication,
	idProfile int64,
) (*model.Publication, error) {
	sqlPost := models.Post{
		IDProfile: idProfile,
		Content:   publication.Content,
	}

	ctx := context.Background()
	tx, err := sqlPR.db.BeginTx(ctx, nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	if err := sqlPost.Insert(ctx, tx, boil.Infer()); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}
	for _, image := range publication.Images {
		sqlImage := models.Image{
			Key:      image.Key,
			MimeType: image.MimeType,
			Name:     image.Name,
		}

		if err := sqlImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
		sqlPostImage := models.PostImage{
			IDImage: sqlImage.ID,
			IDPost:  sqlPost.ID,
		}
		if err := sqlPostImage.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
	}
	for _, idCategory := range publication.IDCategories {
		sqlPostCategory := models.PostCategory{
			IDPost:     sqlPost.ID,
			IDCategory: idCategory,
		}
		if err := sqlPostCategory.Insert(ctx, tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}

	return sqlPR.sqlPostToPublication(&sqlPost), nil
}

func (sqlPublicationRepository) selectOpts(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return mod
	}
	if selectOpts.IDProfile != nil && *selectOpts.IDProfile {
		mod = append(mod, Select(models.PostColumns.IDProfile))
	}
	if selectOpts.ID != nil && *selectOpts.ID {
		mod = append(mod, Select(models.PostColumns.ID))
	}
	return mod
}

func (sqlPR sqlPublicationRepository) includeOpts(include *Include, selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if include == nil {
		return mod
	}
	if include.Images {
		mod = append(mod, Load(Rels(
			models.PostRels.IDPostPostImages,
			models.PostImageRels.IDImageImage,
		)))
	}
	if include.Tattoos {
		mod = append(mod, Load(models.PostRels.IDPostTattoos))
	}
	if include.Tattoos && include.TattoosImage {
		mod = append(mod, Load(Rels(
			models.PostRels.IDPostTattoos,
			models.TattooRels.IDImageImage,
		)))
	}
	if include.Categories {
		mod = append(mod, Load(Rels(
			models.PostRels.IDPostPostCategories,
			models.PostCategoryRels.IDCategoryCategory,
		)))
	}
	if include.Profile {
		mod = append(mod, Load(
			models.PostRels.IDProfileProfile,
		))
	}
	if include.Profile && include.ProfileAvatar {
		mod = append(mod, Load(Rels(
			models.PostRels.IDProfileProfile,
			models.ProfileRels.IDAvatarImage,
		)))
	}
	if include.Profile && include.ProfileUser {
		var toSelect []QueryMod
		if selectOpts != nil {
			toSelect = append(toSelect, sqlPR.sqlUserRepository.SelectOpts(selectOpts.User)...)
		}

		mod = append(mod, Load(Rels(
			models.PostRels.IDProfileProfile,
			models.ProfileRels.IDUserUser,
		), toSelect...))
	}

	return mod
}

func (sqlPR sqlPublicationRepository) findOneOptionsToMod(opts *FindOneOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlPR.includeOpts(opts.include, opts.selectOpts)...)
	mod = append(mod, sqlPR.selectOpts(opts.selectOpts)...)

	return mod
}

func (sqlPublicationRepository) sortOpts(sortOpts *Sort) []QueryMod {
	mod := []QueryMod{}
	if sortOpts == nil {
		return mod
	}
	if sortOpts.CreatedAt != "" {
		mod = append(mod, OrderBy(fmt.Sprintf("created_at %s", sortOpts.CreatedAt)))
	}

	return mod
}

func (sqlPR sqlPublicationRepository) findOptionsToMod(opts *FindOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlPR.includeOpts(opts.include, opts.selectOpts)...)
	mod = append(mod, sqlPR.selectOpts(opts.selectOpts)...)
	mod = append(mod, sqlPR.sortOpts(opts.sort)...)
	if opts.limit != nil {
		mod = append(mod, Limit(*opts.limit))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(*opts.skip))
	}

	return mod
}

func (sqlPR sqlPublicationRepository) Find(criteria *Criteria, opts *FindOptions) ([]model.Publication, error) {
	mod := sqlPR.findOptionsToMod(opts)
	where := sqlPR.criteriaToWhere(criteria)

	sqlPublications, err := models.Posts(append(mod, where...)...).All(context.Background(), sqlPR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(sqlPublications, func(post *models.Post) model.Publication {
		return *sqlPR.sqlPostToPublication(post)
	}), nil
}

func (sqlPR sqlPublicationRepository) FindOne(criteria *Criteria, opts *FindOneOptions) (*model.Publication, error) {
	mod := sqlPR.findOneOptionsToMod(opts)
	where := sqlPR.criteriaToWhere(criteria)

	publication, err := models.Posts(append(mod, where...)...).One(context.Background(), sqlPR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, nil
		}

		return nil, utils.ErrRepositoryFailed
	}

	return sqlPR.sqlPostToPublication(publication), nil
}

func (sqlPR sqlPublicationRepository) Exists(criteria *Criteria) (bool, error) {
	where := sqlPR.criteriaToWhere(criteria)

	exists, err := models.Posts(where...).Exists(context.Background(), sqlPR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlPR sqlPublicationRepository) Delete(criteria *Criteria) error {
	where := sqlPR.criteriaToWhere(criteria)

	publications, err := models.Posts(append([]QueryMod{
		Select("id"),
	}, where...)...).All(context.Background(), sqlPR.db)
	if err != nil {
		return err
	}
	idPublications := utils.MapNoError(publications, func(publication *models.Post) int64 {
		return publication.ID
	})
	postImages, err := models.PostImages(models.PostImageWhere.IDPost.IN(idPublications)).
		All(context.Background(), sqlPR.db)
	if err != nil {
		return utils.ErrRepositoryFailed
	}
	idImages := utils.MapNoError(postImages, func(postImage *models.PostImage) int64 {
		return postImage.IDImage
	})

	tx, err := sqlPR.db.BeginTx(context.Background(), nil)
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	if _, err := models.PostImages(models.PostImageWhere.IDPost.IN(idPublications)).
		DeleteAll(context.Background(), tx); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}
	if _, err := models.Images(
		models.ImageWhere.ID.IN(idImages),
	).DeleteAll(context.Background(), tx); err != nil {
		return utils.ErrRepositoryFailed
	}
	if _, err := models.Likes(models.LikeWhere.IDPost.IN(idPublications)).
		DeleteAll(context.Background(), tx); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}
	if _, err := models.PostCategories(models.PostCategoryWhere.IDPost.IN(idPublications)).
		DeleteAll(context.Background(), tx); err != nil {
		tx.Rollback()
		return utils.ErrRepositoryFailed
	}

	_, err = models.Posts(where...).DeleteAll(context.Background(), tx)
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

func (sqlPR sqlPublicationRepository) FindImages(idPublication int64) ([]fileModel.Image, error) {
	mod := []QueryMod{
		models.PostImageWhere.IDPost.EQ(idPublication),
		Load(models.PostImageRels.IDImageImage),
	}

	images, err := models.PostImages(mod...).All(context.Background(), sqlPR.db)
	if err != nil {
		return nil, err
	}

	return utils.MapNoError(images, func(image *models.PostImage) fileModel.Image {
		sqlImage := image.R.IDImageImage

		return fileModel.Image{
			ID:        sqlImage.ID,
			Key:       sqlImage.Key,
			MimeType:  sqlImage.MimeType,
			Name:      sqlImage.Name,
			CreatedAt: sqlImage.CreatedAt,
		}
	}), nil
}

func (sqlPR sqlPublicationRepository) UpdateOne(criteria *Criteria, data UpdateData) error {
	where := sqlPR.criteriaToWhere(criteria)

	sqlPublication, err := models.Posts(where...).One(context.Background(), sqlPR.db)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}

		return utils.ErrRepositoryFailed
	}
	// Cols
	var cols []string
	if data.SumLikes != 0 {
		sqlPublication.Likes += data.SumLikes

		cols = append(cols, models.PostColumns.Likes)
	}

	_, err = sqlPublication.Update(context.Background(), sqlPR.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func NewSqlPublicationRepository(db *sql.DB) PublicationRepository {
	return sqlPublicationRepository{
		db: db,
		sqlUserRepository: user_repository.SqlExplicitUserRepository(
			db,
		),
	}
}

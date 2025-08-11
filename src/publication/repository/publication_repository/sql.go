package publication_repository

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"sort"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
	"github.com/typesense/typesense-go/v3/typesense"
)

type sqlPublicationRepository struct {
	db                      *sql.DB
	ts                      *typesense.Client
	sqlUserRepository       user_repository.SqlUserRepository
	tsPublicationRepository TypeSensePublicationRepository
}

func (sqlPublicationRepository) sqlPostToPublication(
	post *models.Post,
) *model.Publication {
	var images []fileModel.Image
	var tattoos []tattooModel.Tattoo
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
		Views:      post.Views,
		Images:     images,
		CreatedAt:  post.CreatedAt,
		Categories: post.Categories,
		Mentions:   post.Mentions,
		Tattoos:    tattoos,
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
	if criteria.IDStudio != nil {
		if criteria.IDStudio.EQ == nil {
			mod = append(mod, models.PostWhere.IDStudio.IsNull())
		} else {
			mod = append(mod, models.PostWhere.IDStudio.EQ(null.Int64FromPtr(criteria.IDStudio.EQ)))
		}
	}
	if criteria.Content.EQ != nil {
		mod = append(mod, models.PostWhere.Content.EQ(*criteria.Content.EQ))
	} else if criteria.Content.IContains != nil {
		mod = append(mod, models.PostWhere.Content.ILIKE(fmt.Sprintf("%%%s%%", *criteria.Content.IContains)))
	}
	if criteria.CreatedAt != nil {
		if !criteria.CreatedAt.LTE.IsZero() {
			mod = append(mod, models.PostWhere.CreatedAt.LTE(criteria.CreatedAt.LTE))
		}
		if !criteria.CreatedAt.LT.IsZero() {
			mod = append(mod, models.PostWhere.CreatedAt.LT(criteria.CreatedAt.LT))
		}
		if !criteria.CreatedAt.GTE.IsZero() {
			mod = append(mod, models.PostWhere.CreatedAt.GTE(criteria.CreatedAt.GTE))
		}
		if !criteria.CreatedAt.GT.IsZero() {
			mod = append(mod, models.PostWhere.CreatedAt.GT(criteria.CreatedAt.GT))
		}
		if !criteria.CreatedAt.EQ.IsZero() {
			mod = append(mod, models.PostWhere.CreatedAt.EQ(criteria.CreatedAt.EQ))
		}
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
		IDProfile:  idProfile,
		Content:    publication.Content,
		Categories: publication.Categories,
		Mentions:   publication.Mentions,
		IDStudio:   null.NewInt64(publication.IDStudio, publication.IDStudio != 0),
	}

	tx, err := sqlPR.db.BeginTx(context.Background(), nil)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}
	if err := sqlPost.Insert(context.Background(), tx, boil.Infer()); err != nil {
		tx.Rollback()

		return nil, utils.ErrRepositoryFailed
	}
	for _, image := range publication.Images {
		sqlImage := models.Image{
			Key:      image.Key,
			MimeType: image.MimeType,
			Name:     image.Name,
		}

		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()

			return nil, utils.ErrRepositoryFailed
		}
		sqlPostImage := models.PostImage{
			IDImage: sqlImage.ID,
			IDPost:  sqlPost.ID,
		}
		if err := sqlPostImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
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
	if selectOpts.IDStudio != nil && *selectOpts.IDStudio {
		mod = append(mod, Select(models.PostColumns.IDStudio))
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

func (sqlPR sqlPublicationRepository) searchOptionsToMod(opts *SearchOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlPR.includeOpts(opts.include, opts.selectOpts)...)
	if opts.limit != nil {
		mod = append(mod, Limit(*opts.limit))
	}
	if opts.skip != nil {
		mod = append(mod, Offset(*opts.skip))
	}

	return mod
}

func (sqlPR sqlPublicationRepository) Search(
	q string,
	criteria *Criteria,
	opts *SearchOptions,
) ([]model.Publication, int64, error) {
	idPublications, found, err := sqlPR.tsPublicationRepository.Search(q, criteria, opts)
	if err != nil {
		return nil, 0, utils.ErrRepositoryFailed
	}

	mod := sqlPR.searchOptionsToMod(opts)
	sqlPublications, err := models.Posts(append(
		mod,
		models.PostWhere.ID.IN(idPublications),
	)...).All(context.Background(), sqlPR.db)
	if err != nil {
		return nil, 0, utils.ErrRepositoryFailed
	}
	order := make(map[int64]int)
	for i, id := range idPublications {
		order[id] = i
	}

	sort.Slice(sqlPublications, func(i, j int) bool {
		return order[sqlPublications[i].ID] < order[sqlPublications[j].ID]
	})

	return utils.MapNoError(sqlPublications, func(post *models.Post) model.Publication {
		return *sqlPR.sqlPostToPublication(post)
	}), int64(found), nil
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
	if data.SumViews != 0 {
		sqlPublication.Views += data.SumViews
		cols = append(cols, models.PostColumns.Views)
	}
	if data.SumShares != 0 {
		sqlPublication.Shares += data.SumShares
		cols = append(cols, models.PostColumns.Shares)
	}

	_, err = sqlPublication.Update(context.Background(), sqlPR.db, boil.Whitelist(cols...))
	if err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func NewSqlPublicationRepository() PublicationRepository {
	return sqlPublicationRepository{
		db: db.DB,
		sqlUserRepository: user_repository.SqlExplicitUserRepository(
			db.DB,
		),
		ts:                      db.TSClient,
		tsPublicationRepository: NewTsPublicationRepository(),
	}
}

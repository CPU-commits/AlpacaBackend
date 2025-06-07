package publication_repository

import (
	"context"
	"strconv"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/typesense/typesense-go/v3/typesense"
	"github.com/typesense/typesense-go/v3/typesense/api"
	"github.com/typesense/typesense-go/v3/typesense/api/pointer"
)

type tsPublicationRepository struct {
	ts *typesense.Client
}

func NewTsPublicationRepository() tsPublicationRepository {
	return tsPublicationRepository{
		ts: db.TSClient,
	}
}

type TSPublication struct {
	ID         string   `json:"id"`
	IDProfile  int64    `json:"id_profile"`
	Content    string   `json:"content"`
	Likes      int32    `json:"likes"`
	Views      int32    `json:"views"`
	Categories []string `json:"categories"`
	Mentions   []int64  `json:"mentions"`
	CreatedAt  int64    `json:"created_at"`
	Rating     float64  `json:"rating"`
}

func (tsPR *tsPublicationRepository) DeletePublication(publication *model.Publication) error {
	strID := strconv.FormatInt(publication.ID, 10)

	_, err := tsPR.ts.Collection("publications").Document(strID).Delete(context.Background())
	if err != nil {
		panic(err)
	}

	return nil
}

func (tsPR *tsPublicationRepository) IndexPublication(publication *model.Publication) error {

	params := &api.DocumentIndexParameters{}
	strID := strconv.FormatInt(publication.ID, 10)
	tsPublication := TSPublication{
		ID:         strID,
		IDProfile:  publication.IDProfile,
		Content:    publication.Content,
		Likes:      int32(publication.Likes),
		Categories: publication.Categories,
		Mentions:   publication.Mentions,
		CreatedAt:  publication.CreatedAt.Unix(),
		Rating:     0.3,
	}
	_, err := tsPR.ts.Collection("publications").Documents().Upsert(
		context.Background(),
		tsPublication,
		params,
	)
	if err != nil {
		return err
	}
	return nil
}

// Actualiza el rating con los nuevos valores
func (tsPR *tsPublicationRepository) UpdatePublication(
	publication *model.Publication,
	daysSincePublished int,
	followers int,
) error {

	params := &api.DocumentIndexParameters{}
	strID := strconv.FormatInt(publication.ID, 10)

	rating := utils.CalculateRanting(daysSincePublished, publication.Likes, publication.Views, 0, followers)
	tsPublication := TSPublication{
		ID:         strID,
		IDProfile:  publication.IDProfile,
		Content:    publication.Content,
		Likes:      int32(publication.Likes),
		Views:      int32(publication.Views),
		Categories: publication.Categories,
		Mentions:   publication.Mentions,
		CreatedAt:  publication.CreatedAt.Unix(),
		Rating:     rating,
	}

	_, err := tsPR.ts.Collection("publications").Document(tsPublication.ID).Update(context.Background(), tsPublication, params)
	if err != nil {
		panic(err)
	}

	return nil
}

func init() {
	client := db.TSClient

	// Publications
	fieldsPublication := []api.Field{
		{Name: "id", Type: "string", Facet: pointer.True()},
		{Name: "id_profile", Type: "int64", Facet: pointer.True()},
		{Name: "content", Type: "string", Index: pointer.True()},
		{Name: "likes", Type: "int32", Facet: pointer.True()},
		{Name: "views", Type: "int32", Facet: pointer.True()},
		{Name: "categories", Type: "string[]", Facet: pointer.True()},
		{Name: "mentions", Type: "int64[]", Facet: pointer.True()},
		{Name: "created_at", Type: "int64", Sort: pointer.True(), Facet: pointer.True()},
		{Name: "rating", Type: "float"},
	}
	publicationsSchema := &api.CollectionSchema{
		Name:                "publications",
		Fields:              fieldsPublication,
		DefaultSortingField: pointer.String("rating"),
	}

	_, err := client.Collection("publications").Retrieve(context.Background())
	if err != nil {
		_, err := client.Collections().Create(context.Background(), publicationsSchema)
		if err != nil {
			panic(err)
		}
	}

}

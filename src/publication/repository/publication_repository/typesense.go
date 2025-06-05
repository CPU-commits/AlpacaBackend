package publication_repository

import (
	"context"
	"encoding/base64"
	"strconv"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/typesense/typesense-go/v3/typesense"
	"github.com/typesense/typesense-go/v3/typesense/api"
	"github.com/typesense/typesense-go/v3/typesense/api/pointer"
)

type tsPublicationRepository struct {
	ts *typesense.Client
}

type TSPublication struct {
	ID         string   `json:"id_publication"`
	IDProfile  int64    `json:"id_profile"`
	Content    string   `json:"content"`
	Likes      int32    `json:"likes"`
	Views      int32    `json:"views"`
	Categories []string `json:"categories"`
	CreatedAt  int64    `json:"created_at"`
	Rating     float64  `json:"rating"`
	Image1     string   `json:"image_1,omitempty"`
	Image2     string   `json:"image_2,omitempty"`
	Image3     string   `json:"image_3,omitempty"`
	Image4     string   `json:"image_4,omitempty"`
	Image5     string   `json:"image_5,omitempty"`
}

func (tsPR *tsPublicationRepository) DeletePublication(publication *model.Publication) error {
	strID := strconv.FormatInt(publication.ID, 10)

	_, err := tsPR.ts.Collection("publications").Document(strID).Delete(context.Background())
	if err != nil {
		return err
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
		CreatedAt:  publication.CreatedAt.Unix(),
		Rating:     0.3,
	}
	imageNumber := 1
	for _, image := range publication.Images {
		imageBytes, err := cloudinary_store.NewCloudinaryImageStore().Download(
			image.Key,
		)
		if err != nil {
			return err
		}
		imageBase64 := base64.StdEncoding.EncodeToString(imageBytes)

		if imageNumber == 1 {
			tsPublication.Image1 = imageBase64
		} else if imageNumber == 2 {
			tsPublication.Image2 = imageBase64
		} else if imageNumber == 3 {
			tsPublication.Image3 = imageBase64
		} else if imageNumber == 4 {
			tsPublication.Image4 = imageBase64
		} else if imageNumber == 5 {
			tsPublication.Image5 = imageBase64
		}
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
		CreatedAt:  publication.CreatedAt.Unix(),
		Rating:     rating,
	}

	_, err := tsPR.ts.Collection("publications").Document(tsPublication.ID).Update(context.Background(), tsPublication, params)
	if err != nil {
		return err
	}

	return nil
}

func NewTsPublicationRepository() tsPublicationRepository {
	return tsPublicationRepository{
		ts: db.TSClient,
	}
}

func init() {
	client := db.TSClient

	fields := []api.Field{
		{Name: "id", Type: "string", Facet: pointer.True()},
		{Name: "id_profile", Type: "int64", Facet: pointer.True()},
		{Name: "content", Type: "string", Index: pointer.True()},
		{Name: "likes", Type: "int32", Facet: pointer.True()},
		{Name: "views", Type: "int32", Facet: pointer.True()},
		{Name: "categories", Type: "string[]", Facet: pointer.True()},
		{Name: "created_at", Type: "int64", Sort: pointer.True(), Facet: pointer.True()},
		{Name: "rating", Type: "float"},
		{Name: "image_1", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "image_2", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "image_3", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "image_4", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "image_5", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{
			Name: "embedding",
			Type: "float[]",
			Embed: &struct {
				From        []string "json:\"from\""
				ModelConfig struct {
					AccessToken    *string "json:\"access_token,omitempty\""
					ApiKey         *string "json:\"api_key,omitempty\""
					ClientId       *string "json:\"client_id,omitempty\""
					ClientSecret   *string "json:\"client_secret,omitempty\""
					IndexingPrefix *string "json:\"indexing_prefix,omitempty\""
					ModelName      string  "json:\"model_name\""
					ProjectId      *string "json:\"project_id,omitempty\""
					QueryPrefix    *string "json:\"query_prefix,omitempty\""
					RefreshToken   *string "json:\"refresh_token,omitempty\""
					Url            *string "json:\"url,omitempty\""
				} "json:\"model_config\""
			}{
				From: []string{"image_1", "image_2", "image_3", "image_4", "image_5"},
				ModelConfig: struct {
					AccessToken    *string "json:\"access_token,omitempty\""
					ApiKey         *string "json:\"api_key,omitempty\""
					ClientId       *string "json:\"client_id,omitempty\""
					ClientSecret   *string "json:\"client_secret,omitempty\""
					IndexingPrefix *string "json:\"indexing_prefix,omitempty\""
					ModelName      string  "json:\"model_name\""
					ProjectId      *string "json:\"project_id,omitempty\""
					QueryPrefix    *string "json:\"query_prefix,omitempty\""
					RefreshToken   *string "json:\"refresh_token,omitempty\""
					Url            *string "json:\"url,omitempty\""
				}{
					ModelName: "ts/clip-vit-b-p32",
				},
			},
			Optional: pointer.True(),
		},
	}
	publicationsSchema := &api.CollectionSchema{
		Name:                "publications",
		Fields:              fields,
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

package tattoo_repository

import (
	"context"
	"encoding/base64"
	"strconv"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/typesense/typesense-go/v3/typesense"
	"github.com/typesense/typesense-go/v3/typesense/api"
	"github.com/typesense/typesense-go/v3/typesense/api/pointer"
)

type tsTattooRepository struct {
	ts *typesense.Client
}

func NewTsTattooRepository() tsTattooRepository {
	return tsTattooRepository{
		ts: db.TSClient,
	}
}

type TSTattoo struct {
	ID            string  `json:"id"`
	IDProfile     int64   `json:"id_profile"`
	IDPublication int64   `json:"id_publication"`
	IDImage       int64   `json:"id_image"`
	Likes         int32   `json:"likes"`
	Views         int32   `json:"views"`
	Popularity    int64   `json:"popularity"`
	Image         string  `json:"image,omitempty"`
	CreatedAt     int64   `json:"created_at"`
	Rating        float64 `json:"rating"`
}

// func (tsTR *tsTattooRepository) DeleteTattoo(Tattoo *model.Tattoo) error {
// 	strID := strconv.FormatInt(Tattoo.ID, 10)

// 	_, err := tsTR.ts.Collection("tattoos").Document(strID).Delete(context.Background())
// 	if err != nil {
// 		panic(err)
// 	}

// 	return nil
// }

func (tsTR *tsTattooRepository) IndexTattoo(tattoo *model.Tattoo) error {

	strID := strconv.FormatInt(tattoo.ID, 10)
	params := &api.DocumentIndexParameters{}
	tsTattoo := TSTattoo{
		ID:            strID,
		IDProfile:     tattoo.Profile.ID,
		IDPublication: tattoo.IDPublication,
		IDImage:       tattoo.Image.ID,
		Likes:         int32(tattoo.Likes),
		Views:         int32(tattoo.Views),
		Popularity:    0,
		CreatedAt:     tattoo.CreatedAt.Unix(),
		Rating:        0.3,
	}
	imageBytes, err := cloudinary_store.NewCloudinaryImageStore().Download(
		tattoo.Image.Key,
	)
	if err != nil {
		panic(err)
	}

	imageBase64 := base64.StdEncoding.EncodeToString(imageBytes)
	tsTattoo.Image = imageBase64

	_, err = tsTR.ts.Collection("tattoos").Documents().Upsert(
		context.Background(),
		tsTattoo,
		params,
	)
	if err != nil {
		panic(err)
	}
	return nil
}

// Actualiza el rating con los nuevos valores
// func (tsTR *tsTattooRepository) UpdateTattoo(
// 	tattoo *model.Tattoo,
// 	daysSincePublished int,
// 	followers int,
// ) error {

// 	params := &api.DocumentIndexParameters{}
// 	strID := strconv.FormatInt(tattoo.ID, 10)

// 	rating := utils.CalculateRanting(daysSincePublished, tattoo.Likes, tattoo.Views, 0, followers)
// 	tsTattoo := TSTattoo{
// 		ID:            strID,
// 		IDProfile:     tattoo.Profile.ID,
// 		IDPublication: tattoo.IDPublication,
// 		IDImage:       tattoo.Image.ID,
// 		Likes:         int32(tattoo.Likes),
// 		Views:         int32(tattoo.Views),
// 		CreatedAt:     tattoo.CreatedAt.Unix(),
// 		Rating:        rating,
// 	}

// 	_, err := tsTR.ts.Collection("tattoos").Document(tsTattoo.ID).Update(context.Background(), tsTattoo, params)
// 	if err != nil {
// 		panic(err)
// 	}

// 	return nil
// }

func init() {
	client := db.TSClient

	fields := []api.Field{
		{Name: "id", Type: "string", Facet: pointer.True()},
		{Name: "id_profile", Type: "int64", Facet: pointer.True()},
		{Name: "id_image", Type: "int64", Index: pointer.True()},
		{Name: "id_publication", Type: "int64", Index: pointer.True()},
		{Name: "likes", Type: "int32", Facet: pointer.True()},
		{Name: "views", Type: "int32", Facet: pointer.True()},
		{Name: "popularity", Type: "int64", Facet: pointer.True()},
		{Name: "image", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "created_at", Type: "int64", Sort: pointer.True(), Facet: pointer.True()},
		{Name: "rating", Type: "float"},
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
				From: []string{"image"},
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
		},
	}

	tattoosSchema := &api.CollectionSchema{
		Name:                "tattoos",
		Fields:              fields,
		DefaultSortingField: pointer.String("rating"),
	}

	_, err := client.Collection("tattoos").Retrieve(context.Background())
	if err != nil {
		_, err := client.Collections().Create(context.Background(), tattoosSchema)
		if err != nil {
			panic(err)
		}
	}

}

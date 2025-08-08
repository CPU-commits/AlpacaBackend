package tattoo_repository

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
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
	ID                     string   `json:"id"`
	IDProfile              int64    `json:"id_profile"`
	IDPublication          string   `json:"id_publication"`
	IDImage                int64    `json:"id_image"`
	Likes                  int32    `json:"likes"`
	Categories             []string `json:"categories,omitempty"`
	Description            string   `json:"description"`
	PublicationDescription string   `json:"publication_description"`
	Mentions               []int64  `json:"mentions"`
	Views                  int32    `json:"views"`
	Popularity             int64    `json:"popularity"`
	Image                  string   `json:"image,omitempty"`
	CreatedAt              int64    `json:"created_at"`
	Color                  string   `json:"color"`
	Rating                 float64  `json:"rating"`
}

// func (tsTR *tsTattooRepository) DeleteTattoo(Tattoo *model.Tattoo) error {
// 	strID := strconv.FormatInt(Tattoo.ID, 10)

// 	_, err := tsTR.ts.Collection("tattoos").Document(strID).Delete(context.Background())
// 	if err != nil {
// 		panic(err)
// 	}

// 	return nil
// }

func (tsTR *tsTattooRepository) Search(
	params SimilarityParams,
	opts *SimilarityOptions,
) (idTattoos []int64, found int64, err error) {
	var perPage *int
	var page *int
	if opts != nil {
		perPage = opts.limit
		if opts.skip != nil && opts.limit != nil {
			pageInt := (*opts.skip) * (*opts.limit)

			page = &pageInt
		}
	}

	var vectorQuery string
	if params.Embedding != nil {
		jsonData, _ := json.Marshal(params.Embedding)

		vectorQuery = fmt.Sprintf("image_embedding:(%v)", string(jsonData))
	} else if params.IDTattoo != 0 {
		vectorQuery = fmt.Sprintf("image_embedding:([], id:%d)", params.IDTattoo)
	} else if params.ImageBase64 != "" {
		vectorQuery = fmt.Sprintf("image_embedding:([], image:%s)", params.ImageBase64)
	}

	multiSearchResult, err := tsTR.ts.MultiSearch.Perform(
		context.Background(),
		&api.MultiSearchParams{},
		api.MultiSearchSearchesParameter{Searches: []api.MultiSearchCollectionParameters{{
			Q:           pointer.String("*"),
			VectorQuery: pointer.String(vectorQuery),
			SortBy:      pointer.String("_vector_distance:asc,rating:desc"),
			PerPage:     perPage,
			Page:        page,
			Collection:  pointer.String("tattoos"),
		}}},
	)
	if err != nil {
		return nil, 0, err
	}
	result := multiSearchResult.Results[0]

	found = int64(*result.Found)
	if result.Hits != nil {
		hits := *result.Hits
		for _, hit := range hits {
			tattoo := (*hit.Document)
			idTattooStr := tattoo["id"].(string)
			idTattoo, err := strconv.Atoi(idTattooStr)
			if err != nil {
				return nil, 0, err
			}

			idTattoos = append(idTattoos, int64(idTattoo))
		}
	}

	return
}

func (tsTR *tsTattooRepository) IndexTattoo(tattoo *model.Tattoo) error {
	strID := strconv.FormatInt(tattoo.ID, 10)
	strIDPublication := strconv.FormatInt(tattoo.IDPublication, 10)
	params := &api.DocumentIndexParameters{}
	tsTattoo := TSTattoo{
		ID:                     strID,
		IDProfile:              tattoo.Profile.ID,
		IDPublication:          strIDPublication,
		IDImage:                tattoo.Image.ID,
		Likes:                  int32(tattoo.Likes),
		Views:                  int32(tattoo.Views),
		Popularity:             0,
		CreatedAt:              tattoo.CreatedAt.Unix(),
		Rating:                 0.3,
		Description:            tattoo.LLMDescription,
		PublicationDescription: tattoo.Description,
		Categories:             tattoo.Categories,
		Color:                  tattoo.Color,
		Mentions:               tattoo.Mentions,
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
	//client.Collection("tattoos").Delete(context.Background())

	fields := []api.Field{
		{Name: "id_profile", Type: "int64", Facet: pointer.True()},
		{Name: "id_image", Type: "int64", Facet: pointer.True()},
		{
			Name:      "id_publication",
			Type:      "string",
			Reference: pointer.String("publications.id"),
			Facet:     pointer.True(),
		},
		{Name: "likes", Type: "int32", Facet: pointer.True()},
		{Name: "views", Type: "int32", Facet: pointer.True()},
		{Name: "categories", Type: "string[]", Facet: pointer.True(), Optional: pointer.True()},
		{Name: "mentions", Type: "int64[]", Facet: pointer.True(), Optional: pointer.True()},
		{Name: "popularity", Type: "int64", Facet: pointer.True()},
		{Name: "image", Type: "image", Store: pointer.False(), Optional: pointer.True()},
		{Name: "color", Type: "string", Optional: pointer.True()},
		{Name: "description", Type: "string", Index: pointer.True()},
		{Name: "publication_description", Type: "string", Index: pointer.True()},
		{Name: "created_at", Type: "int64", Sort: pointer.True(), Facet: pointer.True()},
		{Name: "rating", Type: "float"},
		{
			Name: "image_embedding",
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
		{
			Name: "descr_embedding",
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
				From: []string{"description", "publication_description"},
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
					ModelName:      "ts/multilingual-e5-small",
					IndexingPrefix: pointer.String("passage: "),
					QueryPrefix:    pointer.String("query: "),
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

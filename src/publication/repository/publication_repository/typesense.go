package publication_repository

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/typesense/typesense-go/v3/typesense"
	"github.com/typesense/typesense-go/v3/typesense/api"
	"github.com/typesense/typesense-go/v3/typesense/api/pointer"
)

type tsPublicationRepository struct {
	ts *typesense.Client
}

func NewTsPublicationRepository() TypeSensePublicationRepository {
	return &tsPublicationRepository{
		ts: db.TSClient,
	}
}

func (tsPublicationRepository) criteriaToFilterBy(criteria *Criteria) *string {
	if criteria == nil {
		return nil
	}
	var filterBy string
	if criteria.Categories != nil {
		filterBy += "categories:["
		filterBy += strings.Join(criteria.Categories, ", ")
		filterBy += "]"
	}
	if criteria.TattooCriteria != nil {
		if criteria.TattooCriteria.Color != "" {
			if filterBy != "" {
				filterBy += " && "
			}

			filterBy += fmt.Sprintf("color:%s", criteria.TattooCriteria.Color)
		}
		if criteria.TattooCriteria.Areas != nil {
			if filterBy != "" {
				filterBy += " && "
			}

			filterBy += "areas:["
			filterBy += strings.Join(utils.MapNoError(
				criteria.TattooCriteria.Areas,
				func(area tattooModel.TattooArea) string {
					return string(area)
				},
			), ", ")
			filterBy += "]"
		}
	}
	fmt.Printf("filterBy: %v\n", filterBy)

	return pointer.String(filterBy)
}

func (tsPR tsPublicationRepository) Search(
	q string,
	criteria *Criteria,
	opts *SearchOptions,
) (idPublications []int64, found int, err error) {
	var perPage *int
	var page *int
	if opts != nil {
		perPage = opts.limit
		if opts.skip != nil && opts.limit != nil {
			pageInt := (*opts.skip) * (*opts.limit)

			page = &pageInt
		}
	}

	apiResult, err := tsPR.ts.Collection("tattoos").Documents().Search(
		context.Background(),
		&api.SearchCollectionParams{
			Q:           pointer.String(q),
			QueryBy:     pointer.String("description,categories,publication_description,descr_embedding"),
			PerPage:     perPage,
			FilterBy:    tsPR.criteriaToFilterBy(criteria),
			Page:        page,
			VectorQuery: pointer.String("descr_embedding:([], distance_threshold: 0.8)"),
			SortBy:      pointer.String("_text_match:desc,rating:desc"),
			GroupBy:     utils.String("id_publication"),
			GroupLimit:  utils.Int(1),
		},
	)
	if err != nil {
		return nil, 0, err
	}

	found = *apiResult.Found
	appendHits := func(hit api.SearchResultHit) error {
		tattoo := (*hit.Document)
		idTattooStr := tattoo["id_publication"].(string)
		idTattoo, err := strconv.Atoi(idTattooStr)
		if err != nil {
			return err
		}

		idPublications = append(idPublications, int64(idTattoo))
		return nil
	}

	if apiResult.Hits != nil {
		hits := *apiResult.Hits
		for _, hit := range hits {
			if err := appendHits(hit); err != nil {
				return nil, 0, err
			}
		}
		return
	}
	if apiResult.GroupedHits != nil {
		gHits := *apiResult.GroupedHits

		for _, gHit := range gHits {
			hits := gHit.Hits
			for _, hit := range hits {
				if err := appendHits(hit); err != nil {
					return nil, 0, err
				}
			}
		}
	}
	fmt.Printf("idPublications: %v\n", idPublications)

	return
}

func (tsPR *tsPublicationRepository) DeletePublication(publication *model.Publication) error {
	strID := strconv.FormatInt(publication.ID, 10)

	_, err := tsPR.ts.Collection("publications").Document(strID).Delete(context.Background())
	if err != nil {
		var httpError *typesense.HTTPError

		if ok := errors.As(err, &httpError); ok {
			if httpError.Status == 404 {
				return nil
			}
		}
		panic(err)
	}

	return nil
}

func (tsPR *tsPublicationRepository) IndexPublication(publication *model.Publication) error {
	params := &api.DocumentIndexParameters{}
	strID := strconv.FormatInt(publication.ID, 10)
	tsPublication := model.TSPublication{
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
		panic(err)
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
	tsPublication := model.TSPublication{
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
		var httpError *typesense.HTTPError

		if ok := errors.As(err, &httpError); ok {
			if httpError.Status == 404 {
				return nil
			}
		}
		panic(err)
	}

	return nil
}

func init() {
	client := db.TSClient
	//client.Collection("publications").Delete(context.Background())

	// Publications
	fieldsPublication := []api.Field{
		{Name: "id_profile", Type: "int64", Facet: pointer.True()},
		{Name: "content", Type: "string", Index: pointer.True()},
		{Name: "likes", Type: "int32", Facet: pointer.True()},
		{Name: "views", Type: "int32", Facet: pointer.True()},
		{Name: "categories", Type: "string[]", Facet: pointer.True()},
		{Name: "mentions", Type: "int64[]", Facet: pointer.True()},
		{Name: "created_at", Type: "int64", Sort: pointer.True(), Facet: pointer.True()},
		{Name: "rating", Type: "float", Sort: pointer.True()},
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
				From: []string{"content"},
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
			Optional: pointer.True(),
		},
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

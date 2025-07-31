package utils

import (
	"encoding/json"
	"fmt"
	"os"
)

type GEO_KEY string

var (
	SANTIAGO GEO_KEY = "santiago"
)

var memoGeo map[GEO_KEY][]string = make(map[GEO_KEY][]string)

func GetTagsGeo(key GEO_KEY) []string {
	if key == "" {
		return nil
	}
	if tags, exists := memoGeo[key]; exists {
		return tags
	}

	data, err := os.ReadFile(fmt.Sprintf("assets/geojson/%s.geojson", key))
	if err != nil {
		return nil
	}

	var geoData map[string]interface{}
	if err := json.Unmarshal(data, &geoData); err != nil {
		panic(err)
	}
	features := geoData["features"].([]interface{})
	var tags []string

	for _, feature := range features {
		f := feature.(map[string]interface{})
		tags = append(tags, f["properties"].(map[string]interface{})["Comuna"].(string))
	}
	memoGeo[key] = tags

	return tags
}

package db

import (
	"strings"
	"time"

	"github.com/typesense/typesense-go/v3/typesense"
)

var TSClient *typesense.Client

func init() {
	TSClient = typesense.NewClient(
		typesense.WithNodes(strings.Split(settingsData.TYPESENSE_HOSTS, ",")),
		typesense.WithAPIKey(settingsData.TYPESENS_API_KEY),
		typesense.WithConnectionTimeout(15*time.Second),
	)
}

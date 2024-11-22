package settings

import (
	"fmt"
	"log"
	"os"
	"strings"
	"sync"

	"github.com/joho/godotenv"
)

var lock = &sync.Mutex{}
var singleSettingsInstace *settings

type settings struct {
	JWT_SECRET_KEY string
	CLIENT_DOMAIN  string
	CORS_DOMAINS   string
	GO_ENV         string
	NATS_HOSTS     string
	DB_CONNECTION  string
}

func validateSettings(settings *settings) {
	var missing []string

	if settings.CLIENT_DOMAIN == "" {
		missing = append(missing, "CLIENT_DOMAIN")
	}
	if settings.CORS_DOMAINS == "" {
		missing = append(missing, "CORS_DOMAINS")
	}
	if settings.GO_ENV == "" {
		missing = append(missing, "GO_ENV")
	}
	if settings.JWT_SECRET_KEY == "" {
		missing = append(missing, "JWT_SECRET_KEY")
	}
	if settings.NATS_HOSTS == "" {
		missing = append(missing, "NATS_HOSTS")
	}
	if settings.DB_CONNECTION == "" {
		missing = append(missing, "DB_CONNECTION")
	}

	if len(missing) > 0 {
		panic(fmt.Sprintf("Missing variables: %s", strings.Join(missing, ", ")))
	}
}

func newSettings() *settings {
	settings := &settings{
		JWT_SECRET_KEY: os.Getenv("JWT_SECRET_KEY"),
		CLIENT_DOMAIN:  os.Getenv("CLIENT_DOMAIN"),
		GO_ENV:         os.Getenv("GO_ENV"),
		CORS_DOMAINS:   os.Getenv("CORS_DOMAINS"),
		NATS_HOSTS:     os.Getenv("NATS_HOSTS"),
		DB_CONNECTION:  os.Getenv("DB_CONNECTION"),
	}
	validateSettings(settings)

	return settings
}

func init() {
	if os.Getenv("GO_ENV") != "prod" {
		if err := godotenv.Load(); err != nil {
			log.Fatalf("No .env file found")
		}
	}
}

func GetSettings() *settings {
	if singleSettingsInstace == nil {
		lock.Lock()
		defer lock.Unlock()
		singleSettingsInstace = newSettings()
	}
	return singleSettingsInstace
}

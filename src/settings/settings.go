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
	JWT_SECRET_KEY              string
	CLIENT_DOMAIN               string
	CORS_DOMAINS                string
	GO_ENV                      string
	NATS_HOSTS                  string
	DB_CONNECTION               string
	CLOUDINARY_URL              string
	TYPESENSE_HOSTS             string
	TYPESENS_API_KEY            string
	REDIS_CONNECTION            string
	REDIS_PASS                  string
	OPENAI_KEY                  string
	SMTPHOST                    string
	SMTPFROM                    string
	SMTPPASS                    string
	SMTPPORT                    string
	REPLICATE_KEY               string
	BACKEND_URL                 string
	CLIENT_URL                  string
	IPINFO_TOKEN                string
	LEMONSQUEEZY_TOKEN          string
	LEMONSQUEEZY_SIGNING_SECRET string
	LEMONSQUEEZY_STORE          string
	CRON_KEY                    string
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
	if settings.CLOUDINARY_URL == "" {
		missing = append(missing, "CLOUDINARY_URL")
	}
	if settings.TYPESENSE_HOSTS == "" {
		missing = append(missing, "TYPESENSE_HOSTS")
	}
	if settings.TYPESENS_API_KEY == "" {
		missing = append(missing, "TYPESENS_API_KEY")
	}
	if settings.REDIS_CONNECTION == "" {
		missing = append(missing, "REDIS_CONNECTION")
	}
	if settings.REDIS_PASS == "" {
		missing = append(missing, "REDIS_PASS")
	}
	if settings.OPENAI_KEY == "" {
		missing = append(missing, "OPENAI_KEY")
	}
	if settings.REPLICATE_KEY == "" {
		missing = append(missing, "REPLICATE_KEY")
	}
	if settings.SMTPHOST == "" {
		missing = append(missing, "SMTPHOST")
	}
	if settings.SMTPFROM == "" {
		missing = append(missing, "SMTPFROM")
	}
	if settings.SMTPPASS == "" {
		missing = append(missing, "SMTPPASS")
	}
	if settings.SMTPPORT == "" {
		missing = append(missing, "SMTPPORT")
	}
	if settings.BACKEND_URL == "" {
		missing = append(missing, "BACKEND_URL")
	}
	if settings.CLIENT_URL == "" {
		missing = append(missing, "CLIENT_URL")
	}
	if settings.IPINFO_TOKEN == "" {
		missing = append(missing, "IPINFO_TOKEN")
	}
	if settings.LEMONSQUEEZY_TOKEN == "" {
		missing = append(missing, "LEMONSQUEEZY_TOKEN")
	}
	if settings.LEMONSQUEEZY_SIGNING_SECRET == "" {
		missing = append(missing, "LEMONSQUEEZY_SIGNING_SECRET")
	}
	if settings.LEMONSQUEEZY_STORE == "" {
		missing = append(missing, "LEMONSQUEEZY_STORE")
	}
	if settings.CRON_KEY == "" {
		missing = append(missing, "CRON_KEY")
	}
	if len(missing) > 0 {
		panic(fmt.Sprintf("Missing variables: %s", strings.Join(missing, ", ")))
	}
}

func newSettings() *settings {
	settings := &settings{
		JWT_SECRET_KEY:              os.Getenv("JWT_SECRET_KEY"),
		CLIENT_DOMAIN:               os.Getenv("CLIENT_DOMAIN"),
		GO_ENV:                      os.Getenv("GO_ENV"),
		CORS_DOMAINS:                os.Getenv("CORS_DOMAINS"),
		NATS_HOSTS:                  os.Getenv("NATS_HOSTS"),
		DB_CONNECTION:               os.Getenv("DB_CONNECTION"),
		CLOUDINARY_URL:              os.Getenv("CLOUDINARY_URL"),
		TYPESENSE_HOSTS:             os.Getenv("TYPESENSE_HOSTS"),
		TYPESENS_API_KEY:            os.Getenv("TYPESENS_API_KEY"),
		REDIS_CONNECTION:            os.Getenv("REDIS_CONNECTION"),
		REDIS_PASS:                  os.Getenv("REDIS_PASS"),
		OPENAI_KEY:                  os.Getenv("OPENAI_KEY"),
		REPLICATE_KEY:               os.Getenv("REPLICATE_KEY"),
		SMTPHOST:                    os.Getenv("SMTPHOST"),
		SMTPFROM:                    os.Getenv("SMTPFROM"),
		SMTPPASS:                    os.Getenv("SMTPPASS"),
		SMTPPORT:                    os.Getenv("SMTPPORT"),
		BACKEND_URL:                 os.Getenv("BACKEND_URL"),
		CLIENT_URL:                  os.Getenv("CLIENT_URL"),
		IPINFO_TOKEN:                os.Getenv("IPINFO_TOKEN"),
		LEMONSQUEEZY_TOKEN:          os.Getenv("LEMONSQUEEZY_TOKEN"),
		LEMONSQUEEZY_SIGNING_SECRET: os.Getenv("LEMONSQUEEZY_SIGNING_SECRET"),
		LEMONSQUEEZY_STORE:          os.Getenv("LEMONSQUEEZY_STORE"),
		CRON_KEY:                    os.Getenv("CRON_KEY"),
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

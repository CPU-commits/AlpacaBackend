package http

import (
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strings"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/docs"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-contrib/cors"
	"github.com/gin-contrib/secure"

	ginzap "github.com/gin-contrib/zap"
	"github.com/gin-gonic/gin"

	swaggerFiles "github.com/swaggo/files"

	// swagger embed files
	ginSwagger "github.com/swaggo/gin-swagger"
	"go.uber.org/zap"
)

var settingsData = settings.GetSettings()

func Init(zapLogger *zap.Logger, logger logger.Logger) {
	router := gin.New()
	// Proxies
	router.SetTrustedProxies([]string{"localhost"})
	// Logger
	router.Use(ginzap.GinzapWithConfig(zapLogger, &ginzap.Config{
		TimeFormat: time.RFC3339,
		UTC:        true,
		SkipPathRegexps: []*regexp.Regexp{
			regexp.MustCompile(`(?i)(swagger|healthz)`),
		},
	}))
	router.Use(ginzap.RecoveryWithZap(zapLogger, true))

	router.Use(gin.CustomRecovery(func(c *gin.Context, recovered interface{}) {
		if err, ok := recovered.(string); ok {
			c.String(http.StatusInternalServerError, fmt.Sprintf("Server Internal Error: %s", err))
		}
		c.AbortWithStatusJSON(http.StatusInternalServerError, gin.H{
			"title": "Server internal error",
		})
	}))
	// Docs
	docs.SwaggerInfo.BasePath = "/api"
	docs.SwaggerInfo.Version = "v1"
	docs.SwaggerInfo.Host = "localhost:8080"
	// CORS
	if settingsData.GO_ENV == "prod" {
		router.Use(cors.New(cors.Config{
			AllowOrigins:     strings.Split(settingsData.CORS_DOMAINS, ","),
			AllowMethods:     []string{"GET", "OPTIONS", "PUT", "DELETE", "POST"},
			AllowCredentials: true,
			AllowHeaders:     []string{"*"},
			AllowWebSockets:  false,
			MaxAge:           12 * time.Hour,
		}))
	} else {
		router.Use(cors.Default())
	}
	// Secure
	sslUrl := "ssl." + settingsData.CLIENT_DOMAIN
	secureConfig := secure.Config{
		SSLHost:              sslUrl,
		STSSeconds:           315360000,
		STSIncludeSubdomains: true,
		FrameDeny:            true,
		ContentTypeNosniff:   true,
		BrowserXssFilter:     true,
		IENoOpen:             true,
		ReferrerPolicy:       "strict-origin-when-cross-origin",
		SSLProxyHeaders: map[string]string{
			"X-Fowarded-Proto": "https",
		},
	}
	router.Use(secure.New(secureConfig))
	// I18n
	router.Use(func(ctx *gin.Context) {
		lang := ctx.DefaultQuery("lang", "es")
		ctx.Set("localizer", utils.GetLocalizer(lang))
	})
	// Init bus
	bus := queue.New(logger)
	// Routes
	dog := router.Group("api/dogs")
	{
		// Controllers
		dogController := controller.NewHTTPDogController(
			bus,
		)
		// Define routes
		dog.GET("/:idDog", dogController.GetDog)
		dog.POST("", dogController.InsertDog)
	}
	// Route docs
	router.GET("/swagger/*any", ginSwagger.WrapHandler(swaggerFiles.Handler))
	// Route healthz
	router.GET("/healthz", func(ctx *gin.Context) {
		ctx.String(200, "OK")
	})
	// No route
	router.NoRoute(func(ctx *gin.Context) {
		ctx.String(404, "Not found")
	})
	// Init server
	if err := router.Run(); err != nil {
		log.Fatalf("Error init server")
	}
}

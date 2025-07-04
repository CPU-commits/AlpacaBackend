package http

import (
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strings"
	"time"

	appointmentController "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/controller"
	authController "github.com/CPU-commits/Template_Go-EventDriven/src/auth/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/docs"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/middleware"
	httpUtils "github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	generatorCon "github.com/CPU-commits/Template_Go-EventDriven/src/generator/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	publicationController "github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	tattooController "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/controller"
	userController "github.com/CPU-commits/Template_Go-EventDriven/src/user/controller"
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
	// Custom validators
	httpUtils.RegisterCustomValidators()
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
	router.Use(cors.New(cors.Config{
		AllowOrigins:     strings.Split(settingsData.CORS_DOMAINS, ","),
		AllowMethods:     []string{"GET", "OPTIONS", "PUT", "DELETE", "POST", "PATCH"},
		AllowCredentials: true,
		AllowHeaders:     []string{"*"},
		AllowWebSockets:  false,
		MaxAge:           12 * time.Hour,
		ExposeHeaders:    []string{"X-Total", "X-Per-Page"},
	}))
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
	// Bus

	bus := queue.New(logger)
	// tes
	auth := router.Group("api/auth")
	{
		// Controllers
		authControlle := authController.NewAuthHttpController(bus)
		userController := authController.NewUserHttpController(bus)
		// Define routes
		auth.POST("/login", authControlle.Login)
		auth.POST("/refresh", authControlle.Refresh)
		auth.POST("/register", authControlle.Register)
		auth.PATCH("/password", middleware.JWTMiddleware(), authControlle.UpdatePassword)

		auth.PATCH("/user", middleware.JWTMiddleware(), userController.UpdateUser)
		auth.PATCH("/email", middleware.JWTMiddleware(), userController.UpdateEmail)
		auth.GET("", middleware.JWTMiddleware(), userController.IsOwner)
	}

	tattoo := router.Group("api/tattoos")
	{
		// Controllers
		tattooController := tattooController.NewTattooHttpController(bus)
		// Define routes
		tattoo.GET("/:username", tattooController.GetTattoos)
		tattoo.GET("/urlKey/:idTattoo", tattooController.GetUrlImageTattoo)
		tattoo.POST("/searchByImage", tattooController.SearchByImage)
		tattoo.GET("/latest/:username", tattooController.GetLatestTattoos)
		tattoo.POST("", middleware.JWTMiddleware(), tattooController.UploadTattoos)
	}
	profile := router.Group("api/profiles")
	{
		// Controllers
		profileController := userController.NewHTTProfileController()
		// Define routes
		profile.GET("/:username", profileController.GetProfile)
		profile.GET("/views/:identifier", profileController.GetAllUserViews) // Se debe cambiar username por Identificador IP o algo asi
		profile.PATCH("/avatar", middleware.JWTMiddleware(), profileController.ChangeAvatar)
		profile.PATCH("", middleware.JWTMiddleware(), profileController.UpdateProfile)
	}
	publication := router.Group("api/publications")
	{
		// Controllers
		publicationController := publicationController.NewPublicationHttpController(bus)
		// Define routes
		publication.GET("/username/:username", publicationController.GetPublications)
		publication.GET("/:idPost", publicationController.GetPublication)
		publication.GET("/:idPost/like", publicationController.GetMyLike)
		publication.GET("/search", publicationController.Search)
		publication.POST("", middleware.JWTMiddleware(), publicationController.Publish)
		publication.POST("/:idPost/like", middleware.JWTMiddleware(), publicationController.Like)
		publication.POST("/:idPost/view", publicationController.AddViewPublication)
		publication.DELETE("/:idPublication", middleware.JWTMiddleware(), publicationController.DeletePublication)
	}
	appointment := router.Group("api/appointments", middleware.JWTMiddleware())
	{
		// Controllers
		appointmentController := appointmentController.NewHTTPAppointmentController(bus)
		// Define routes
		appointment.GET("", appointmentController.GetAppointments)
		appointment.GET(
			"/pendingCount",
			middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}),
			appointmentController.GetAppointmentsPending,
		)
		appointment.POST("", appointmentController.RequestAppointment)
		appointment.POST(":idAppointment/review", appointmentController.ReviewAppointment)
		appointment.PATCH(
			":idAppointment/schedule",
			middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}),
			appointmentController.ScheduleAppointment,
		)
		appointment.PATCH(":idAppointment/cancel", appointmentController.CancelAppointment)
	}
	generator := router.Group("api/generators", middleware.JWTMiddleware())
	{
		codeController := generatorCon.NewCodeHttpController(bus)

		generator.POST("code", middleware.JWTMiddleware(), codeController.CreateCode)
		generator.GET("code/verify/:code", middleware.JWTMiddleware(), codeController.VerifyCode)
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

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
	followController "github.com/CPU-commits/Template_Go-EventDriven/src/follow/controller"
	generatorCon "github.com/CPU-commits/Template_Go-EventDriven/src/generator/controller"
	internalController "github.com/CPU-commits/Template_Go-EventDriven/src/internal/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	paymentControllers "github.com/CPU-commits/Template_Go-EventDriven/src/payment/controller"
	publicationController "github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	shorterController "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/controller"
	studioController "github.com/CPU-commits/Template_Go-EventDriven/src/studio/controller"
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
	// HTML
	router.LoadHTMLGlob("templates/*")
	router.Static("/assets", "./assets")
	// I18n
	router.Use(func(ctx *gin.Context) {
		lang := ctx.DefaultQuery("lang", "es")
		ctx.Set("localizer", utils.GetLocalizer(lang))
	})
	// Bus

	bus := queue.New(logger)
	// tes
	auth := router.Group("api/auth")
	user := router.Group("api/users")
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
		user.GET("/search", userController.SearchUsers)
		user.GET("/__sitemap__", userController.GetSitemapData)
	}

	tattoo := router.Group("api/tattoos")
	{
		// Controllers
		tattooController := tattooController.NewTattooHttpController(bus)
		// Define routes
		tattoo.GET("", tattooController.GetTattoos)
		tattoo.GET("/urlKey/:idTattoo", tattooController.GetUrlImageTattoo)
		tattoo.POST("/searchByImage", tattooController.SearchByImage)
		tattoo.GET("/latest", tattooController.GetLatestTattoos)
		tattoo.POST("", middleware.JWTMiddleware(), tattooController.UploadTattoos)
	}
	design := router.Group("api/designs")
	{
		designController := tattooController.NewDesignHttpController(bus)

		design.POST("", middleware.JWTMiddleware(), designController.UploadDesigns)
		design.GET("/:username", designController.GetDesigns)
		design.GET("/:username/design", designController.GetDesign)
		design.GET("/latest/:username", designController.GetLatestDesigns)
		design.DELETE("/:id", middleware.JWTMiddleware(), designController.DeleteDesign)
		design.PATCH("/:id", middleware.JWTMiddleware(), designController.UpdateDesign)
		design.GET("/categories/:username", designController.GetCategories)
	}
	profile := router.Group("api/profiles")
	{
		// Controllers
		profileController := userController.NewHTTProfileController()
		// Define routes
		profile.GET("/:username", middleware.OptionalJWTMiddleware(), profileController.GetProfile)
		profile.GET("/:username/metrics", middleware.JWTMiddleware(), middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}), profileController.GetMetricsProfile)
		profile.GET("/search", profileController.SearchProfiles)
		profile.GET("/views/:identifier", profileController.GetAllUserViews) // Se debe cambiar username por Identificador IP o algo asi
		profile.GET("/user/:idUser/avatar", profileController.GetAvatar)
		profile.PATCH("/avatar", middleware.JWTMiddleware(), profileController.ChangeAvatar)
		profile.PATCH("", middleware.JWTMiddleware(), profileController.UpdateProfile)
	}
	publication := router.Group("api/publications")
	{
		// Controllers
		publicationController := publicationController.NewPublicationHttpController(bus)
		// Define routes
		publication.GET("/__sitemap__", publicationController.GetSitemapData)
		publication.GET("/username/:username", publicationController.GetUserPublications)
		publication.GET("/studio/:idStudio", publicationController.GetStudioPublications)
		publication.GET("/:idPost", publicationController.GetPublication)
		publication.GET("/:idPost/like", publicationController.GetMyLike)
		publication.GET("/search", publicationController.Search)
		publication.GET("/:idPost/metrics", middleware.JWTMiddleware(), publicationController.GetMetricsPublication)
		publication.POST("", middleware.JWTMiddleware(), publicationController.Publish)
		publication.POST("/:idPost/share", middleware.JWTMiddleware(), publicationController.Share)
		publication.POST("/:idPost/like", middleware.JWTMiddleware(), publicationController.Like)
		publication.POST("/:idPost/view", middleware.OptionalJWTMiddleware(), publicationController.AddViewPublication)
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
			appointmentController.GetAppointmentsPending,
		)
		appointment.GET("/metrics", middleware.JWTMiddleware(), appointmentController.GetMetricsAppointments)
		appointment.POST("", appointmentController.RequestAppointment)
		appointment.POST(":idAppointment/review", appointmentController.ReviewAppointment)
		appointment.PATCH(
			":idAppointment/schedule",
			middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}),
			appointmentController.ScheduleAppointment,
		)
		appointment.PATCH(":idAppointment/cancel", appointmentController.CancelAppointment)
		appointment.PATCH("/:idAppointment/assignTattooArtist/:idUser", appointmentController.AssignTattooArtist)
	}
	generator := router.Group("api/generators", middleware.JWTMiddleware())
	{
		codeController := generatorCon.NewCodeHttpController(bus)

		generator.POST("code", codeController.CreateCode)
		generator.GET("code/verify/:code", codeController.VerifyCode)
	}
	studio := router.Group("api/studios")
	{
		adminStudioController := studioController.NewHttpAdminStudioController(bus)
		studioController := studioController.NewHttpStudioController(bus)

		studio.GET("/__sitemap__", studioController.GetSitemapData)
		studio.GET("/:idStudio", middleware.OptionalJWTMiddleware(), studioController.GetStudio)
		studio.GET("/:idStudio/metrics", middleware.JWTMiddleware(), studioController.GetStudioMetrics)
		studio.GET("/:idStudio/username", studioController.GetStudioUsername)
		studio.GET("/search", studioController.SearchStudios)
		studio.GET("/permissions", studioController.GetPermissions)
		studio.GET("/:idStudio/my_permissions", middleware.JWTMiddleware(), adminStudioController.GetPermissionsInStudio)
		studio.GET("/:idStudio/people", middleware.JWTMiddleware(), adminStudioController.GetStudioPeople)
		studio.GET("/:idStudio/tattooArtists", adminStudioController.GetStudioTattooArtists)
		studio.GET("/my", middleware.JWTMiddleware(), middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}), studioController.GetStudios)
		studio.POST("", middleware.JWTMiddleware(), middleware.RolesMiddleware([]model.Role{model.TATTOO_ARTIST_ROLE}), studioController.CreateStudio)
		studio.PATCH("/:idStudio/user/:idUser/roles", middleware.JWTMiddleware(), adminStudioController.ChangeRole)
		studio.POST("/:idStudio/join/:idUser", middleware.JWTMiddleware(), adminStudioController.JoinPeople)
		studio.PATCH("/:idStudio/user/:idUser/permissions", middleware.JWTMiddleware(), adminStudioController.SetPermission)
		studio.PATCH("/:idStudio", middleware.JWTMiddleware(), studioController.UpdateStudio)
		studio.DELETE("/:idStudio/user/:idUser", middleware.JWTMiddleware(), adminStudioController.RemovePerson)
	}
	follow := router.Group("api/follows", middleware.JWTMiddleware())
	{
		followController := followController.NewFollowController(
			bus,
		)

		follow.GET("/my", followController.GetMyFollow)
		follow.POST("", followController.ToggleFollow(true))
		follow.POST("/unfollow", followController.ToggleFollow(false))
	}
	s := router.Group("s")
	{
		shorterController := shorterController.NewHttpAdminStudioController(bus)

		s.GET("/:shortCode", shorterController.Relink)
	}
	links := router.Group("api/links")
	{
		shorterController := shorterController.NewHttpAdminStudioController(bus)

		links.GET("/:idLink/metrics", shorterController.GetLinkMetrics)
	}
	payments := router.Group("api/payments")
	subscriptions := router.Group("api/subscriptions")
	{
		subscriptionController := paymentControllers.NewHttpSubscriptionController(bus)
		paymentController := paymentControllers.NewHttpPaymentController(bus)

		subscriptions.GET("/my", middleware.JWTMiddleware(), subscriptionController.GetMySubscription)
		subscriptions.GET("/plans", subscriptionController.GetAllActivePlans)
		subscriptions.POST("/plans/:idPlan/request", middleware.JWTMiddleware(), subscriptionController.RequestSubscription)
		subscriptions.DELETE("/cancel", middleware.JWTMiddleware(), subscriptionController.CancelSubscription)
		payments.POST("/lemonsqueezy/webhook", paymentController.LemonSqueezyPagoWebhook)
		payments.GET("", paymentController.GetPayments)
	}
	internal := router.Group("api/internals", middleware.CronApiKeyMiddleware())
	{
		internalController := internalController.NewHTTPInternalController(bus)
		internal.POST("/update_ratings", internalController.UpdateRatings)

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

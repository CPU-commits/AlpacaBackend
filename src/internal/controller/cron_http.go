package controller

import (
	"log"
	"net/http"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/internal/cron"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/gin-gonic/gin"
)

type HttpInternalController struct {
	bus             bus.Bus
	publicationCron cron.PublicationCron
}

func NewHTTPInternalController(bus bus.Bus) *HttpInternalController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	profileService := *userServices.NewProfileService(
		profileRepository,
		*userService,
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		userServices.SinglentonFollowService(),
	)

	adminStudioService := studioService.NewPeopleStudioService(
		adminStudioRepository,
		studioRepository,
		*userService,
		peopleHistoriesRepository,
	)

	return &HttpInternalController{
		bus: bus,
		publicationCron: *cron.NewCronPublication(
			bus,
			*service.NewPublicationService(
				*tattooService.NewTattooService(
					imageStore,
					profileService,
					tattooRepository,
					*fileService,
					embeddingapi.NewAPIEmbedding(),
					bus,
				),
				profileService,
				imageStore,
				publicationRepository,
				likeRepository,
				tattooRepository,
				userRepository,
				*fileService,
				*adminStudioService,
				*viewService,
				shareRepository,
				bus,
			),
		),
	}
}

func (internalController *HttpInternalController) UpdateRatings(c *gin.Context) {
	internalController.publicationCron.UpdateRatings()
	log.Println("Update ratings cron")
	c.JSON(http.StatusOK, nil)
}

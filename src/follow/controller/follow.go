package controller

import (
	"net/http"
	"strconv"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/gin-gonic/gin"
)

type httpFollowController struct {
	followService service.FollowService
}

func (httpFollowController *httpFollowController) GetMyFollow(c *gin.Context) {
	idStudioStr := c.DefaultQuery("idStudio", "0")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	username := c.Query("username")

	claims, _ := utils.NewClaimsFromContext(c)
	follow, err := httpFollowController.followService.GetMyFollow(
		service.GetFollowParam{
			IDStudio: int64(idStudio),
			Username: username,
		},
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"follow": follow,
	})
}

func (httpFollowController *httpFollowController) ToggleFollow(follow bool) func(c *gin.Context) {
	return func(c *gin.Context) {
		var followDto *dto.FollowDto

		if err := c.BindJSON(&followDto); err != nil {
			utils.ResErrValidators(c, err)
			return
		}

		claims, _ := utils.NewClaimsFromContext(c)
		if err := httpFollowController.followService.ToggleFollow(followDto, claims.ID, follow); err != nil {
			utils.ResFromErr(c, err)
			return
		}

		c.JSON(http.StatusCreated, nil)
	}
}

func NewFollowController(bus bus.Bus) httpFollowController {
	userServices := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	fileService := fileService.NewFileService(imageStore)
	profileService := userService.NewProfileService(
		profileRepository,
		*userServices,
		imageStore,
		*fileService,
		followRepository,
		publicationRepository,
	)
	followService := service.NewFollowService(
		followRepository,
		*profileService,
	)

	return httpFollowController{
		followService: *followService,
	}
}

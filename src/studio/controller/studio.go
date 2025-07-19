package controller

import (
	"fmt"
	"net/http"
	"strconv"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

type httpStudioController struct {
	studioService *service.StudioService
}

func (httpStudioController httpStudioController) GetPermissions(c *gin.Context) {
	permissions := httpStudioController.studioService.GetPermissions()
	localizer := utils.GetI18nLocalizer(c)
	tPermissions, err := domainUtils.Map(permissions, func(permission model.Permission) (gin.H, error) {
		t, err := localizer.Localize(&i18n.LocalizeConfig{
			MessageID: fmt.Sprintf("studio.permissions.%s", permission.Permission),
		})
		if err != nil {
			return gin.H{}, err
		}

		return gin.H{
			"permission": permission.Permission,
			"t":          t,
			"dependsOn":  permission.DependsOn,
		}, nil
	})
	if err != nil {
		utils.ResWithMessageID(c, "server.no_t", http.StatusInternalServerError, err)
		return
	}

	c.JSON(http.StatusOK, tPermissions)
}

func (httpStudioController httpStudioController) GetStudio(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	studio, err := httpStudioController.studioService.GetStudio(
		int64(idStudio),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, studio)
}

func (httpStudioController httpStudioController) GetStudioUsername(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	studio, err := httpStudioController.studioService.GetStudioUsername(
		int64(idStudio),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"username": studio,
	})
}

func (httpStudioController httpStudioController) GetStudios(c *gin.Context) {
	claims, _ := utils.NewClaimsFromContext(c)

	studios, err := httpStudioController.studioService.GetMyStudios(
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, studios)
}

func (httpStudioController httpStudioController) SearchStudios(c *gin.Context) {
	q := c.Query("q")

	studios, err := httpStudioController.studioService.SearchStudios(
		q,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, studios)
}

func (httpStudioController httpStudioController) CreateStudio(c *gin.Context) {
	var studioDto *dto.StudioDTO
	if err := c.Bind(&studioDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	avatarImage, err := utils.OpenFormFile(c, "avatar", utils.Mime("image"))
	if err != nil {
		return
	}
	studioDto.AvatarImage = avatarImage
	bannerImage, err := utils.OpenFormFile(c, "banner", utils.Mime("image"))
	if err != nil {
		return
	}
	studioDto.BannerImage = bannerImage

	claims, _ := utils.NewClaimsFromContext(c)
	if err := httpStudioController.studioService.CreateStudio(studioDto, claims.ID); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func (httpStudioController httpStudioController) UpdateStudio(c *gin.Context) {
	m, _ := c.MultipartForm()
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	var studioDto *dto.UpdateStudioDTO
	if len(m.Value) > 0 {
		if err := c.Bind(&studioDto); err != nil {
			utils.ResErrValidators(c, err)
			return
		}
	} else {
		studioDto = &dto.UpdateStudioDTO{}
	}
	avatarImage, err := utils.OpenFormFile(c, "avatar", utils.Mime("image"))
	if err != nil {
		return
	}
	studioDto.AvatarImage = avatarImage
	bannerImage, err := utils.OpenFormFile(c, "banner", utils.Mime("image"))
	if err != nil {
		return
	}
	studioDto.BannerImage = bannerImage

	claims, _ := utils.NewClaimsFromContext(c)
	if err := httpStudioController.studioService.UpdateStudio(
		studioDto,
		claims.ID,
		int64(idStudio),
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func NewHttpStudioController(bus bus.Bus) httpStudioController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		bus,
	)
	authService := authService.NewAuthService(
		authRepository,
		userRepository,
		bus,
	)
	adminStudio := service.NewPeopleStudioService(
		studioAdminRepository,
		studioRepository,
		*userService,
	)
	studioService := service.NewStudioService(
		studioRepository,
		*authService,
		*fileService,
		*adminStudio,
		imageStore,
		uidGenerator,
	)

	return httpStudioController{
		studioService: studioService,
	}
}

package controller

import (
	"fmt"
	"net/http"
	"strconv"
	"time"

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
	var identifier string
	claims, exists := utils.NewClaimsFromContext(c)
	if exists {
		identifier = strconv.Itoa(int(claims.ID))
	} else {
		identifier = utils.GetIP(c)
	}

	studio, err := httpStudioController.studioService.GetStudio(
		int64(idStudio),
		identifier,
		utils.GetIP(c),
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

func (httpStudioController httpStudioController) GetStudioMetrics(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	fromDateStr := c.Query("from")
	now := time.Now()
	var fromDate time.Time = time.Date(now.Year(), now.Month(), 1, 0, 0, 0, 0, now.Location())

	if fromDateStr != "" {
		var err error

		fromDate, err = time.Parse(time.RFC3339, fromDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	toDateStr := c.Query("to")
	var toDate time.Time = fromDate.AddDate(0, 1, 0).Add(-time.Nanosecond)
	if toDateStr != "" {
		var err error

		toDate, err = time.Parse(time.RFC3339, toDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	fromComparativeDateStr := c.Query("fromComparative")
	var fromComparativeDate time.Time

	if fromComparativeDateStr != "" {
		var err error

		fromComparativeDate, err = time.Parse(time.RFC3339, fromComparativeDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	toComparativeDateStr := c.Query("toComparative")
	var toComparativeDate time.Time

	if toComparativeDateStr != "" {
		var err error

		toComparativeDate, err = time.Parse(time.RFC3339, toComparativeDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	if fromComparativeDateStr == "" && toComparativeDateStr == "" {
		fromComparativeDate = fromDate.AddDate(0, -1, 0)
	} else if fromComparativeDateStr == "" {
		fromComparativeDate = toComparativeDate.Add(
			-fromDate.Sub(toDate),
		)
	}
	if fromComparativeDateStr == "" && toComparativeDateStr == "" {
		toComparativeDate = toDate.AddDate(0, -1, 0)
	} else if fromComparativeDateStr == "" {
		toComparativeDate = fromComparativeDate.Add(
			-fromDate.Sub(toDate),
		)
	}

	claims, _ := utils.NewClaimsFromContext(c)
	metrics, err := httpStudioController.studioService.GetStudioMetrics(
		int64(idStudio),
		claims.ID,
		service.MetricsParams{
			To:              toDate,
			From:            fromDate,
			FromComparative: fromComparativeDate,
			ToComparative:   toComparativeDate,
		},
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, metrics)
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
		uidGenerator,
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
		*viewService,
		*followService,
	)

	return httpStudioController{
		studioService: studioService,
	}
}

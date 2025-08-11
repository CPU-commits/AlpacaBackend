package controller

import (
	"mime"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpProfileController struct {
	profileService service.ProfileService
}

// Get godoc
//
//	@Summary	Recibir el perfil de un usuario
//	@Tags		profile
//	@Success	200			{object}	controller.GetTattoosResponse
//	@Param		username	path		string					true	"username"
//	@Param		page		query		int						true	"numero de pagina"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/profiles/{username} [Get]
func (httpProfile *HttpProfileController) GetProfile(c *gin.Context) {
	username := c.Param("username")
	var identifier string
	claims, exists := utils.NewClaimsFromContext(c)
	if exists {
		identifier = strconv.Itoa(int(claims.ID))
	} else {
		identifier = utils.GetIP(c)
	}

	profile, err := httpProfile.profileService.GetProfile(username, identifier, utils.GetIP(c))
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, profile)
}

func (httpProfile *HttpProfileController) GetMetricsProfile(c *gin.Context) {
	username := c.Param("username")
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
	metrics, err := httpProfile.profileService.GetProfileMetrics(
		username,
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

func (httpProfile *HttpProfileController) SearchProfiles(c *gin.Context) {
	q := c.Query("q")
	filterRoles := domainUtils.FilterNoError(strings.Split(c.Query("roles"), ","), func(role string) bool {
		return role != ""
	})

	profile, err := httpProfile.profileService.SearchProfile(q, domainUtils.MapNoError(filterRoles, func(role string) model.Role {
		return model.Role(role)
	})...)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, profile)
}

func (httpProfile *HttpProfileController) GetAvatar(c *gin.Context) {
	idUserStr := c.Param("idUser")
	idUser, err := strconv.Atoi(idUserStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	avatar, err := httpProfile.profileService.GetAvatarFromIDUser(int64(idUser))
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, avatar)
}

// Patch godoc
//
//	@Summary	Actualizar el perfil de un usuario
//	@Tags		profile
//	@Success	200
//	@Param		UpdateProfileDto	body	dto.UpdateProfileDto	true	"username"
//	@Failure	503					object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/profiles [patch]
func (httpProfile *HttpProfileController) UpdateProfile(c *gin.Context) {
	var updateProfileDto *dto.UpdateProfileDto
	if err := c.BindJSON(&updateProfileDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	if err := httpProfile.profileService.UpdateProfile(updateProfileDto, claims.ID); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, nil)
}

// Change Avatar godoc
//
//	@Summary	Actualizar el avatar del perfil de un usuario
//	@Tags		profile
//	@Accept		multipart/form-data
//	@Param		avatar	formData	file								true	"Imagen del avatar (jpg/png/webp)"
//	@Success	201		{object}	controller.UpdateProfileResponse	"Clave del nuevo avatar"
//	@Failure	400		{object}	utils.ProblemDetails				"Archivo no recibido o inválido"
//	@Failure	503		{object}	utils.ProblemDetails				"Error con la base de datos"
//	@Router		/api/profiles/avatar [patch]
func (httpProfile *HttpProfileController) ChangeAvatar(c *gin.Context) {
	avatar, err := c.FormFile("avatar")
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	file, err := avatar.Open()
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	fileSplit := strings.Split(avatar.Filename, ".")
	claims, _ := utils.NewClaimsFromContext(c)
	mimeType := mime.TypeByExtension("." + fileSplit[len(fileSplit)-1])

	key, err := httpProfile.profileService.ChangeAvatar(store.ImageDto{
		File:     file,
		Name:     avatar.Filename,
		MimeType: mimeType,
	}, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, gin.H{
		"key": key,
	})
}

// Get godoc
//
//	@Summary	Recibir los views temporales de un usuario
//	@Tags		profile
//	@Success	200		{object}	controller.GetUserViews
//	@Param		idUser	path		string					true	"idUser"
//	@Failure	503		object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/profiles/{idUser}/views [Get]
func (httpProfile *HttpProfileController) GetAllUserViews(c *gin.Context) {
	identifier := c.Param("identifier")

	userViews, err := httpProfile.profileService.GetAllUserView(identifier)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	c.JSON(http.StatusOK, GetUserViews{
		Views: userViews,
	})
}

func NewHTTProfileController() HttpProfileController {
	return HttpProfileController{
		profileService: *service.NewProfileService(
			profileRepository,
			*authService.NewUserService(
				userRepository,
				roleRepository,
				uidGenerator,
				nil,
			),
			cloudinary_store.NewCloudinaryImageStore(),
			*fileService,
			followRepository,
			publicationRDRepository,
			*viewService,
			followService,
		),
	}
}

package controller

import (
	"mime"
	"net/http"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/dto"
	"github.com/gin-gonic/gin"
)

type HttpProfileController struct {
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
func (*HttpProfileController) GetProfile(c *gin.Context) {
	username := c.Param("username")

	profile, err := profileService.GetProfile(username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, profile)
}

// Patch godoc
//
//	@Summary	Actualizar el perfil de un usuario
//	@Tags		profile
//	@Success	200
//	@Param		UpdateProfileDto	body	dto.UpdateProfileDto	true	"username"
//	@Failure	503					object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/profiles [patch]
func (*HttpProfileController) UpdateProfile(c *gin.Context) {
	var updateProfileDto *dto.UpdateProfileDto
	if err := c.BindJSON(&updateProfileDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	if err := profileService.UpdateProfile(updateProfileDto, claims.ID); err != nil {
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
func (profileHTTP *HttpProfileController) ChangeAvatar(c *gin.Context) {
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

	key, err := profileService.ChangeAvatar(store.ImageDto{
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
//	@Success	200			{object}	controller.GetUserViews
//	@Param		idUser	path		string					true	"idUser"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/profiles/{idUser}/views [Get]
func (*HttpProfileController) GetAllUserViews(c *gin.Context) {
	identifier := c.Param("identifier")

	userViews, err := profileService.GetAllUserView(identifier)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, GetUserViews{
		Views: userViews,
	})
}

func NewHTTProfileController() HttpProfileController {
	return HttpProfileController{}
}

package controller

import (
	"fmt"
	"mime"
	"net/http"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpTattooController struct{}

// Get godoc
//
//	@Summary	Recibir los tatuajes de un usuario
//	@Tags		tattoos
//	@Success	200			{object}	controller.GetTattoosResponse
//	@Param		username	path		string					true	"username"
//	@Param		page		query		int						true	"numero de pagina"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/tattoos/{username} [Get]
func (*HttpTattooController) GetTattoos(c *gin.Context) {
	username := c.Param("username")
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	tattoos, metadata, err := tattooService.GetTattoos(username, page)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, tattoos)
}

// Get godoc
//
//	@Summary	Recibir los ultimos tatuajes de un usuario
//	@Tags		tattoos
//	@Success	200			{object}	controller.GetTattoosResponse
//	@Param		username	path		string					true	"username"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/tattoos/latest/{username} [Get]
func (*HttpTattooController) GetLatestTattoos(c *gin.Context) {
	username := c.Param("username")

	tattoos, err := tattooService.GetLatestTattoos(username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, tattoos)
}

// Publish Tattoos godoc
//
//	@Summary		Publicar nuevos tatuajes {admin}
//	@Tags			tattoos
//	@description	```json
//	@description	{
//	@description	"tattoos": [
//	@description	{
//	@description	"description": "Descripción del tatuaje",
//	@description	"image": "archivo_imagen",
//	@description	"coord": {"x": 100, "y": 200},
//	@description	"idPublication": 123
//	@description	}]
//	@description	}
//	@description	```
//	@Accept			multipart/form-data
//	@Param			image	formData	file	true	"Imagen del tatuaje (jpg/png/webp)"
//	@Success		201
//	@Failure		502	{object}	utils.ProblemDetails	"Falló el repositorio"
//	@Failure		415	{object}	utils.ProblemDetails	"El tipo de imagen es inválido, debe ser jpg/png/webp"
//	@Failure		400	{object}	utils.ProblemDetails	"Formulario inválido o sin datos de tatuajes"
//	@Router			/api/tattoos [post]
func (*HttpTattooController) UploadTattoos(c *gin.Context) {
	var tattoosDto *dto.TattoosDto
	if err := c.Bind(&tattoosDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	i := 0
	tattooDto, err := domainUtils.Map(tattoosDto.TattooDto, func(tattoo dto.TattooDto) (dto.TattooDto, error) {
		file, err := c.FormFile(fmt.Sprintf("image[%d]", i))
		if err != nil {

			return dto.TattooDto{}, err
		}
		i++
		openedFile, err := file.Open()
		if err != nil {
			return dto.TattooDto{}, err
		}
		splitName := strings.Split(file.Filename, ".")
		// Get metadata
		coord, err := utils.GetCoordTattoo(file)
		if err != nil {
			tattoo.Coord = coord
		}

		tattoo.Image = store.ImageDto{
			File:     openedFile,
			Name:     file.Filename,
			MimeType: mime.TypeByExtension("." + splitName[len(splitName)-1]),
		}

		return tattoo, nil
	})
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	_, err = tattooService.PublishTattoos(tattooDto, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, gin.H{})
}

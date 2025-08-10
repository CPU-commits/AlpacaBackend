package controller

import (
	"fmt"
	"mime"
	"mime/multipart"
	"net/http"
	"strconv"
	"strings"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"

	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpTattooController struct {
	bus           bus.Bus
	tattooService service.TattooService
}

// Get godoc
//
//	@Summary	Recibir los tatuajes de un usuario
//	@Tags		tattoos
//	@Success	200			{object}	controller.GetTattoosResponse
//	@Param		username	path		string					true	"username"
//	@Param		page		query		int						true	"numero de pagina"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/tattoos/{username} [Get]
func (httpTattoo *HttpTattooController) GetTattoos(c *gin.Context) {
	username := c.Query("username")
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	var idStudio int64
	idStudioStr := c.Query("idStudio")
	if idStudioStr != "" {
		idStudio, err = strconv.ParseInt(idStudioStr, 10, 64)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}

	tattoos, metadata, err := httpTattoo.tattooService.GetTattoos(service.GetTattoosParams{
		Username: username,
		IDStudio: idStudio,
	}, page)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, tattoos)
}

func (httpTattoo *HttpTattooController) GetUrlImageTattoo(c *gin.Context) {
	idTattooStr := c.Param("idTattoo")
	idTattoo, err := strconv.Atoi(idTattooStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	tattooKey, err := httpTattoo.tattooService.GetUrlImageTattoo(int64(idTattoo))
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"key": tattooKey,
	})
}

func (httpTattoo *HttpTattooController) SearchByImage(c *gin.Context) {
	var errNoSuchFile error = fmt.Errorf("http: no such file")

	tattooImageForm, err := c.FormFile("image")
	if err != nil && err.Error() != errNoSuchFile.Error() {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	var tattooImage multipart.File
	if tattooImageForm != nil {
		tattooImage, err = tattooImageForm.Open()
		if err != nil || tattooImage == nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	idTattooStr := c.DefaultQuery("isLikeidTattoo", "0")
	idTattoo, err := strconv.Atoi(idTattooStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	tattoos, metadata, err := httpTattoo.tattooService.SearchByImage(
		service.SearchByImageParams{
			Image:          tattooImage,
			IsLikeTattooID: int64(idTattoo),
		},
		page,
	)
	if err != nil {
		fmt.Printf("err: %v\n", err)
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
func (httpTattoo *HttpTattooController) GetLatestTattoos(c *gin.Context) {
	username := c.Query("username")
	var idStudio int64
	idStudioStr := c.Query("idStudio")
	if idStudioStr != "" {
		var err error

		idStudio, err = strconv.ParseInt(idStudioStr, 10, 64)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}

	tattoos, err := httpTattoo.tattooService.GetLatestTattoos(service.GetTattoosParams{
		Username: username,
		IDStudio: idStudio,
	})
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
func (httpTattoo *HttpTattooController) UploadTattoos(c *gin.Context) {
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
		coord, err := utils.GetCoordFile(file)
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

	_, err = httpTattoo.tattooService.PublishTattoos(tattooDto, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, gin.H{})
}

func NewTattooHttpController(bus bus.Bus) *HttpTattooController {
	return &HttpTattooController{
		bus: bus,
		tattooService: *service.NewTattooService(
			imageStore,
			*userServices.NewProfileService(
				profileRepository,
				*authService.NewUserService(
					userRepository,
					roleRepository,
					uidGenerator,
					bus,
				),
				cloudinary_store.NewCloudinaryImageStore(),
				*fileService,
				followRepository,
				publicationRDRepository,
				*viewService,
				userServices.SinglentonFollowService(),
			),
			tattooRepository,
			*fileService,
			embeddingapi.NewAPIEmbedding(),
			bus,
		),
	}
}

package controller

import (
	"fmt"
	"mime"
	"net/http"
	"strconv"
	"strings"

	appointmentService "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store/cloudinary_store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/gin-gonic/gin"
)

type HttpDesignController struct {
	bus           bus.Bus
	designService service.DesignService
}

// Publish Designs godoc
//
//	@Summary		Publicar nuevos diseños {tatuador}
//	@Tags			designs
//	@description	```json
//	@description	{
//	@description	"design": [
//	@description	{
//	@description	"description": "Descripción del diseño",
//	@description	"price" : "Precio del diseño"
//	@description	"image": "archivo_imagen",
//	@description	"coord": {"x": 100, "y": 200},
//	@description	}]
//	@description	}
//	@description	```
//	@Accept			multipart/form-data
//	@Param			image	formData	file	true	"Imagen del diseño (jpg/png/webp)"
//	@Success		201
//	@Failure		502	{object}	utils.ProblemDetails	"Falló el repositorio"
//	@Failure		415	{object}	utils.ProblemDetails	"El tipo de imagen es inválido, debe ser jpg/png/webp"
//	@Failure		400	{object}	utils.ProblemDetails	"Formulario inválido o sin datos de tatuajes"
//	@Router			/api/designs [post]
func (httpDesign *HttpDesignController) UploadDesigns(c *gin.Context) {
	var designsDto *dto.DesignsDto

	if err := c.Bind(&designsDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	i := 0
	desingsDto, err := domainUtils.Map(designsDto.DesignDto, func(design dto.DesignDto) (dto.DesignDto, error) {
		file, err := c.FormFile(fmt.Sprintf("image[%d]", i))
		if err != nil {
			return dto.DesignDto{}, err
		}
		i++
		openedFile, err := file.Open()
		if err != nil {
			return dto.DesignDto{}, err
		}
		splitName := strings.Split(file.Filename, ".")

		coord, err := utils.GetCoordFile(file)
		if err != nil {
			design.Coord = coord
		}

		design.Image = store.ImageDto{
			File:     openedFile,
			Name:     file.Filename,
			MimeType: mime.TypeByExtension("." + splitName[len(splitName)-1]),
		}
		return design, nil
	})
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	designs, err := httpDesign.designService.PublishDesigns(desingsDto, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, designs)

}

// Get doc
//
//	@Summary	Recibir los diseños de un usuario
//	@Tags		designs
//	@Success	200			{object}	controller.GetDesignsResponse
//	@Param		username	path		string					true	"username"
//	@Param		page		query		int						true	"numero de pagina"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/designs/{username} [Get]
func (httpDesign *HttpDesignController) GetDesigns(c *gin.Context) {
	var designFind *dto.DesignFindDto

	if err := c.ShouldBindUri(&designFind); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	if err := c.ShouldBindQuery(&designFind); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	designs, metadata, err := httpDesign.designService.GetDesigns(
		designFind.Username,
		*designFind,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, designs)
}

// Get doc
//
//	@Summary	Recibir los diseños de un usuario
//	@Tags		designs
//	@Success	200			{object}	controller.GetDesignsResponse
//	@Param		username	path		string					true	"username"
//	@Param		id		query		int						true	"id de diseño"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/design/{username} [Get]
func (httpDesign *HttpDesignController) GetDesign(c *gin.Context) {
	var designFind *dto.DesignFindDto

	if err := c.ShouldBindUri(&designFind); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	if err := c.ShouldBindQuery(&designFind); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	design, err := httpDesign.designService.GetDesign(
		designFind.ID,
		designFind.Username,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, design)
}

// Get godoc
//
//	@Summary	Recibir los ultimos diseños de un usuario
//	@Tags		designs
//	@Success	200			{object}	controller.GetDesignsResponse
//	@Param		username	path		string					true	"username"
//	@Failure	503			object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/designs/latest/{username} [Get]
func (httpDesign *HttpDesignController) GetLatestDesigns(c *gin.Context) {
	var designFind *dto.DesignFindDto

	if err := c.ShouldBindUri(&designFind); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	designs, err := httpDesign.designService.GetLatestDesigns(designFind.Username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, designs)
}

// Update godoc
//
//	@Summary	Actualizar un diseño del usuario
//	@Tags		designs
//	@Success	200
//	@Param		id			path	string					true	"id del diseño"
//	@Param		description	body	string					false	"descripcion"
//	@Param		price		body	int						false	"precio"
//	@Failure	503			object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/designs/{id} [Patch]
func (httpDesign *HttpDesignController) UpdateDesign(c *gin.Context) {
	var dataUpdate *dto.DataUpdate

	if err := c.ShouldBindUri(&dataUpdate); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	if err := c.ShouldBind(&dataUpdate); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	err := httpDesign.designService.UpdateDesign(claims.ID, *dataUpdate)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

// Delete godoc
//
//	@Summary	Eliminar un diseño del usuario
//	@Tags		designs
//	@Success	200
//	@Param		id			path	string					true	"id del diseño"
//	@Failure	503			object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/designs/{id} [Delete]
func (httpDesign *HttpDesignController) DeleteDesign(c *gin.Context) {
	var param *dto.DesignParam

	if err := c.ShouldBindUri(&param); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	err := httpDesign.designService.DeleteDesign(claims.ID, param.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

// Get godoc
//
//	@Summary	Recibe todas las categorias
//	@Tags		designs
//	@Success	200 {object} controller.GetCategoriesResponse
//	@Failure	503			object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/designs/categories/username [get]
func (httpDesign *HttpDesignController) GetCategories(c *gin.Context) {

	var param *dto.DesignFindDto

	if err := c.ShouldBindUri(&param); err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	categories, err := httpDesign.designService.GetDesignCategories(param.Username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, GetCategoriesResponse{
		Categories: categories,
	})
}

func NewDesignHttpController(bus bus.Bus) *HttpDesignController {
	userService := *authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	profileService := *userServices.NewProfileService(
		profileRepository,
		userService,
		cloudinary_store.NewCloudinaryImageStore(),
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		userServices.SinglentonFollowService(),
	)
	peopleStudioService := *studioService.NewPeopleStudioService(
		peopleStudioRepository,
		studioRepository,
		userService,
		peopleHistoriesRepository,
	)
	return &HttpDesignController{
		bus: bus,
		designService: *service.NewDesignService(
			imageStore,
			profileService,
			designRepository,
			*fileService,
			*appointmentService.NewAppointmentService(
				*fileService,
				appointmentRepository,
				userService,
				googleCalendar,
				reviewRepository,
				profileService,
				peopleStudioService,
				studioRepository,
			),
		),
	}
}

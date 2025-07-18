package controller

import (
	"fmt"
	"mime"
	"net/http"
	"strconv"
	"strings"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpPublicationController struct {
	bus                bus.Bus
	publicationService *service.PublicationService
}

// Get godoc
//
//	@Summary	Recibir publicaciones de un usuario
//	@Tags		publications
//	@Success	200			{object}	controller.GetPublicationsResponse
//	@Param		username	path		string					true	"username"
//	@Param		page		query		int						true	"N° page"
//	@Failure	503			{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Failure	403			{object}	utils.ProblemDetails	"Credenciales inválidas"
//	@Failure	409			{object}	utils.ProblemDetails	"La sesión no existe. Probablemente porque la eliminaron"
//
//	@Router		/api/publications/username/{username} [get]
func (httpPC *HttpPublicationController) GetUserPublications(c *gin.Context) {
	username := c.Param("username")
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	publications, metadata, err := httpPC.publicationService.GetPublications(
		service.PublicationsParams{
			Username: username,
		},
		page,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, publications)
}

func (httpPC *HttpPublicationController) GetStudioPublications(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
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

	publications, metadata, err := httpPC.publicationService.GetPublications(
		service.PublicationsParams{
			IDStudio: int64(idStudio),
		},
		page,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, publications)
}

func (httpPC *HttpPublicationController) GetPublication(c *gin.Context) {
	idPostStr := c.Param("idPost")
	idPost, err := strconv.Atoi(idPostStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	publications, err := httpPC.publicationService.GetPublication(
		int64(idPost),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	c.JSON(http.StatusOK, publications)
}

func (httpPC *HttpPublicationController) Search(c *gin.Context) {
	q := c.Query("q")
	categories := domainUtils.FilterNoError(strings.Split(c.Query("categories"), ","), func(category string) bool {
		return category != ""
	})
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	publications, metadata, err := httpPC.publicationService.Search(
		q,
		categories,
		page,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, publications)
}

// Get godoc
//
//	@Summary	Recibir el like de un usuario sobre una publicacion
//	@Tags		publications
//	@Success	200		{object}	controller.GetLikeResponse
//	@Param		idPost	path		int						true	"id de post"
//	@Failure	503		{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/publications/{idPost}/like [get]
func (httpPC *HttpPublicationController) GetMyLike(c *gin.Context) {
	idPostStr := c.Param("idPost")
	idPost, err := strconv.Atoi(idPostStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	isLiked, err := httpPC.publicationService.GetMyLike(
		int64(idPost),
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, GetLikeResponse{
		Isliked: isLiked,
	})
}

// Post godoc
//
//	@Summary	Dar Like o dislike
//	@Tags		publications
//	@Success	200		{object}	controller.GetLikeResponse
//	@Param		idPost	path		int						true	"id de post"
//	@Failure	503		{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/publications/{idPost}/like [post]
func (httpPC *HttpPublicationController) Like(c *gin.Context) {
	idPostStr := c.Param("idPost")
	idPost, err := strconv.Atoi(idPostStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	isLike, err := httpPC.publicationService.HandleLike(
		int64(idPost),
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, gin.H{
		"isLike": isLike,
	})
}

// Post godoc
//
//	@Summary	Publicar una publicacion
//	@Tags		publications
//	@Success	200				{object}	controller.GetPublicationResponse
//	@Param		PublicationDto	body		controller.PublicationDtoResponse	true	"objeto XD"
//	@Param		images			body		object								true	"Imagenes[]"
//	@Failure	503				object		utils.ProblemDetails				"Error con la base de datos"
//	@Router		/api/publications [post]
func (httpPC *HttpPublicationController) Publish(c *gin.Context) {
	var publicationDto *dto.PublicationDto
	if err := c.Bind(&publicationDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	// Load images
	i := 0
	imagesDto, err := domainUtils.Map(
		publicationDto.Images,
		func(image dto.PublicationImageDto) (dto.PublicationImageDto, error) {
			file, err := c.FormFile(fmt.Sprintf("image[%d]", i))
			if err != nil {
				return dto.PublicationImageDto{}, err
			}
			i++
			openedFile, err := file.Open()
			if err != nil {
				return dto.PublicationImageDto{}, err
			}
			splitName := strings.Split(file.Filename, ".")
			// Get metadata
			coord, err := utils.GetCoordTattoo(file)
			if err != nil {
				image.Coord = coord
			}

			image.Image = store.ImageDto{
				File:     openedFile,
				Name:     file.Filename,
				MimeType: mime.TypeByExtension("." + splitName[len(splitName)-1]),
			}
			return image, nil
		},
	)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	publicationDto.Images = imagesDto

	claims, _ := utils.NewClaimsFromContext(c)
	publication, err := httpPC.publicationService.Publish(publicationDto, claims.ID, claims.Roles)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, publication)
}

// Delete godoc
//
//	@Summary	Eliminar una publicacion
//	@Tags		publications
//	@Success	200
//	@Param		idPublication	path		int	true	"Id de publicacion"
//	@Failure	503				object		utils.ProblemDetails				"Error con la base de datos"
//	@Router		/api/publications/{idPublication} [delete]
func (httpPC *HttpPublicationController) DeletePublication(c *gin.Context) {
	idPublicationStr := c.Param("idPublication")
	idPublication, err := strconv.Atoi(idPublicationStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	if err := httpPC.publicationService.DeletePublication(
		int64(idPublication),
		claims.ID,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, nil)
}

// Post godoc
//
//	@Summary	Añadir una "view" a una publicacion
//	@Tags		publications
//	@Success	200
//	@Param		idPublication	path		int	true	"Id de publicacion"
//	@Failure	503				object		utils.ProblemDetails				"Error con la base de datos"
//	@Router		/api/publications/{idPublication}/view [post]
func (httpPC *HttpPublicationController) AddViewPublication(c *gin.Context) {
	idPublicationStr := c.Param("idPost")
	idPublication, err := strconv.Atoi(idPublicationStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	identifier := c.Query("identifier") // Se debe cambiar por IP o identificador

	if err := httpPC.publicationService.AddView(
		int64(idPublication),
		identifier,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func NewPublicationHttpController(bus bus.Bus) *HttpPublicationController {
	profileService := *userServices.NewProfileService(
		profileRepository,
		*authService.NewUserService(
			userRepository,
			roleRepository,
			bus,
		),
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
	)
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		bus,
	)
	adminStudioService := studioService.NewPeopleStudioService(
		adminStudioRepository,
		studioRepository,
		*userService,
	)

	return &HttpPublicationController{
		bus: bus,
		publicationService: service.NewPublicationService(
			*tattooService.NewTattooService(
				imageStore,
				profileService,
				tattooRepository,
				*fileService,
				embeddingapi.NewAPIEmbedding(),
			),
			profileService,
			imageStore,
			publicationRepository,
			likeRepository,
			tattooRepository,
			userRepository,
			*fileService,
			*adminStudioService,
			bus,
		),
	}
}

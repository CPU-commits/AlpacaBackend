package controller

import (
	"fmt"
	"mime"
	"net/http"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/dto"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpPublicationController struct{}

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
func (*HttpPublicationController) GetPublications(c *gin.Context) {
	username := c.Param("username")
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	publications, metadata, err := publicationService.GetPublications(
		username,
		page,
	)
	// Headers
	c.Header("X-Per-Page", strconv.Itoa(metadata.Limit))
	c.Header("X-Total", strconv.Itoa(metadata.Total))

	c.JSON(http.StatusOK, publications)
}

// Get godoc
//
//	@Summary	Recibir el like de un usuario sobre una publicacion
//	@Tags		publications
//	@Success	200			{object}	controller.GetLikeResponse
//	@Param		idPost	path		int					true	"id de post"
//	@Failure	503			{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/auth/register [post]
func (*HttpPublicationController) GetMyLike(c *gin.Context) {
	idPostStr := c.Param("idPost")
	idPost, err := strconv.Atoi(idPostStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	isLike, err := publicationService.GetMyLike(
		int64(idPost),
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, GetLikeResponse{
		Islike: isLike,
	})
}

func (*HttpPublicationController) Like(c *gin.Context) {
	idPostStr := c.Param("idPost")
	idPost, err := strconv.Atoi(idPostStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	isLike, err := publicationService.HandleLike(
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

func (*HttpPublicationController) Publish(c *gin.Context) {
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
				MimeType: mime.TypeByExtension(splitName[len(splitName)-1]),
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
	publication, err := publicationService.Publish(publicationDto, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, publication)
}

func (*HttpPublicationController) DeletePublication(c *gin.Context) {
	idPublicationStr := c.Param("idPublication")
	idPublication, err := strconv.Atoi(idPublicationStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	if err := publicationService.DeletePublication(
		int64(idPublication),
		claims.ID,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, nil)
}

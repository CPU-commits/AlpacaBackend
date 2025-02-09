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

func (*HttpTattooController) GetLatestTattoos(c *gin.Context) {
	username := c.Param("username")

	tattoos, err := tattooService.GetLatestTattoos(username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, tattoos)
}

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
			MimeType: mime.TypeByExtension(splitName[len(splitName)-1]),
		}

		return tattoo, nil
	})
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	err = tattooService.PublishTattoos(tattooDto, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, gin.H{})
}

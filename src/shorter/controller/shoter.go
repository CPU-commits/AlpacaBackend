package controller

import (
	"errors"
	"fmt"
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/service"
	"github.com/gin-gonic/gin"
)

type httpShorterController struct {
	shorterService service.ShorterService
}

func (httpShorterController *httpShorterController) Relink(c *gin.Context) {
	shortCode := c.Param("shortCode")
	from := c.Query("from")

	link, err := httpShorterController.shorterService.GetLinkFromShortCode(
		shortCode,
	)
	if err != nil {
		var linkToRedirect string
		if from != "" {
			linkToRedirect = fmt.Sprintf("%s%s", settingsData.CLIENT_URL, from)
		}
		if linkToRedirect == "" {
			linkToRedirect = fmt.Sprintf("%s", settingsData.CLIENT_URL)
		}

		if errors.Is(err, service.ErrNoLink) {
			c.HTML(http.StatusOK, "nolink.html", gin.H{
				"link": linkToRedirect,
			})
			return
		}

		utils.ResFromErr(c, err)
		return
	}
	fmt.Printf("link: %v\n", link)

	c.Redirect(http.StatusFound, link)
}

func NewHttpAdminStudioController() httpShorterController {
	return httpShorterController{
		shorterService: *service.NewShorterService(
			linkRepository,
		),
	}
}

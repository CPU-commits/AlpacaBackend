package controller

import (
	"errors"
	"fmt"
	"net/http"
	"strconv"
	"time"

	authServices "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/service"
	studioServices "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	"github.com/gin-gonic/gin"
)

type httpShorterController struct {
	shorterService service.ShorterService
}

func (httpShorterController *httpShorterController) GetLinkMetrics(c *gin.Context) {
	idLinkStr := c.Param("idLink")
	idLink, err := strconv.Atoi(idLinkStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadGateway, err)
		return
	}
	idStudioStr := c.DefaultQuery("idStudio", "0")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadGateway, err)
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
	metrics, err := httpShorterController.shorterService.GetLinkMetrics(
		claims.ID,
		int64(idStudio),
		int64(idLink),
		fromDate,
		toDate,
		fromComparativeDate,
		toComparativeDate,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, metrics)
}

func (httpShorterController *httpShorterController) Relink(c *gin.Context) {
	shortCode := c.Param("shortCode")
	from := c.Query("from")

	link, err := httpShorterController.shorterService.GetLinkFromShortCode(
		shortCode,
		utils.GetIP(c),
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

	c.Redirect(http.StatusFound, link)
}

func NewHttpAdminStudioController(bus bus.Bus) httpShorterController {
	userService := authServices.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	peopleService := studioServices.NewPeopleStudioService(
		peopleStudioRepository,
		studioRepository,
		*userService,
		peopleHistoriesRepository,
	)

	return httpShorterController{
		shorterService: *service.NewShorterService(
			linkRepository,
			*viewService,
			peopleService,
		),
	}
}

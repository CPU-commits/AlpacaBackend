package controller

import (
	"net/http"
	"strconv"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/dogs/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/gin-gonic/gin"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

type httpControllerDog struct {
	dogService service.DogService
}

// GetDog godoc
//
//	@Summary	Obtener un perro por ID
//	@Tags		dogs
//	@Success	200	{object}	model.Dog
//	@Failure	404	{object}	utils.ProblemDetails
//	@Param		id	path		int	true	"ID Dog"
//	@Router		/api/dogs/{idDog} [get]
func (dogController *httpControllerDog) GetDog(c *gin.Context) {
	idDog := c.Param("idDog")
	idIntDog, err := strconv.Atoi(idDog)
	if err != nil {
		c.AbortWithStatusJSON(
			http.StatusBadRequest,
			utils.ProblemDetails{
				Title: err.Error(),
			},
		)
		return
	}

	dog, err := dogController.dogService.GetDogById(idIntDog)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(200, dog)
}

// InsertDog godoc
//
//	@Summary	Insertar un perro
//	@Tags		dogs
//	@Success	200	{string}	ok
//	@Failure	409	{object}	utils.ProblemDetails
//	@Router		/api/dogs [post]
func (dogController *httpControllerDog) InsertDog(c *gin.Context) {
	var dogDto *dto.DogDTO

	if err := c.BindJSON(&dogDto); err != nil {
		localizer := utils.GetI18nLocalizer(c)
		errors := utils.ValidatorErrorToErrorProblemDetails(err, localizer)

		c.AbortWithStatusJSON(
			http.StatusBadRequest,
			utils.ProblemDetails{
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "form.error",
				}),
				Errors: errors,
			},
		)
		return
	}
	if err := dogController.dogService.InsertDog(*dogDto); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.String(201, "ok")
}

func NewHTTPDogController(bus bus.Bus) *httpControllerDog {
	return &httpControllerDog{
		dogService: *service.NewDogService(
			dogRepositoryMemory,
			bus,
		),
	}
}

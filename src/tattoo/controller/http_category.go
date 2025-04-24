package controller

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/gin-gonic/gin"
)

type HttpCategoryController struct{}

// Get godoc
//
//	@Summary	Recibir categorias de tatuajes
//	@Tags		categories
//	@Success	200	{object}	controller.GetCategoriesResponse
//	@Failure	503	object		utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/categories [Get]
func (*HttpCategoryController) GetCategories(c *gin.Context) {
	categories, err := categoryService.GetCategories()
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, categories)
}

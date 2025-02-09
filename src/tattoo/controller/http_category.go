package controller

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/gin-gonic/gin"
)

type HttpCategoryController struct{}

func (*HttpCategoryController) GetCategories(c *gin.Context) {
	categories, err := categoryService.GetCategories()
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, categories)
}

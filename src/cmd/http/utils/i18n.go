package utils

import (
	"github.com/gin-gonic/gin"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

func GetI18nLocalizer(c *gin.Context) *i18n.Localizer {
	localizer, _ := c.Get("localizer")

	return localizer.(*i18n.Localizer)
}

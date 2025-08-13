package middleware

import (
	"crypto/subtle"
	"net/http"
	"os"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/gin-gonic/gin"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

func JWTMiddleware() gin.HandlerFunc {
	return func(ctx *gin.Context) {
		localizer := utils.GetI18nLocalizer(ctx)
		token, err := utils.VerifyToken(ctx.Request.Header.Get("Authorization"))
		if err != nil {
			ctx.AbortWithStatusJSON(http.StatusUnauthorized, &utils.ProblemDetails{
				Detail: err.Error(),
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "unauthorized",
				}),
			})
			return
		}
		if !token.Valid {
			ctx.AbortWithStatusJSON(http.StatusUnauthorized, &utils.ProblemDetails{
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "unauthorized",
				}),
			})
			return
		}
		metadata, err := utils.ExtractTokenMetadata(token)
		if err != nil {
			ctx.AbortWithStatusJSON(http.StatusBadRequest, &utils.ProblemDetails{
				Detail: err.Error(),
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "unauthorized",
				}),
			})
			return
		}
		ctx.Set("user", metadata)
		ctx.Next()
	}
}
func CronApiKeyMiddleware() gin.HandlerFunc {
	return func(ctx *gin.Context) {
		localizer := utils.GetI18nLocalizer(ctx)

		expected := os.Getenv("CRON_KEY") // define esta env en docker-compose

		got := ctx.GetHeader("x-cron-api-key") // Go trata los headers sin sensibilidad a mayúsculas
		if subtle.ConstantTimeCompare([]byte(got), []byte(expected)) != 1 {
			ctx.AbortWithStatusJSON(http.StatusUnauthorized, &utils.ProblemDetails{
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{MessageID: "unauthorized"}),
			})
			return
		}

		ctx.Set("cron", true)
		ctx.Next()
	}
}

func OptionalJWTMiddleware() gin.HandlerFunc {
	return func(ctx *gin.Context) {
		localizer := utils.GetI18nLocalizer(ctx)
		token, err := utils.VerifyToken(ctx.Request.Header.Get("Authorization"))
		if err != nil {

			return
		}

		metadata, err := utils.ExtractTokenMetadata(token)
		if err != nil {
			ctx.AbortWithStatusJSON(http.StatusBadRequest, &utils.ProblemDetails{
				Detail: err.Error(),
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: "unauthorized",
				}),
			})
			return
		}
		ctx.Set("user", metadata)
		ctx.Next()
	}
}

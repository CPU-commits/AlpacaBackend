package utils

import (
	"errors"

	"github.com/gin-gonic/gin"
	"github.com/go-playground/validator/v10"
	"github.com/nicksnyder/go-i18n/v2/i18n"
)

// https://www.rfc-editor.org/rfc/rfc9457.html
type ErrorProblemDetails struct {
	Title   string `json:"title" example:"name"`
	Pointer string `json:"pointer" example:"max"`
	Param   string `json:"param,omitempty" example:"3"`
}

type ProblemDetails struct {
	Type   string                `json:"type,omitempty" example:"/docs/errors/errorPointer"`
	Title  string                `json:"title" example:"Descripción del problema para mostrar al usuario" validate:"required"`
	Detail string                `json:"detail,omitempty" example:"Detalle técnico del error"`
	Errors []ErrorProblemDetails `json:"errors,omitempty"`
}

func msgForTag(tag string) string {
	switch tag {
	case "required":
		return "form.required"
	case "min":
		return "form.min"
	}
	return ""
}

func ValidatorErrorToErrorProblemDetails(
	err error,
	localizer *i18n.Localizer,
) []ErrorProblemDetails {
	var ve validator.ValidationErrors
	if errors.As(err, &ve) {
		out := make([]ErrorProblemDetails, len(ve))
		for i, fe := range ve {
			out[i] = ErrorProblemDetails{
				Pointer: fe.Field(),
				Title: localizer.MustLocalize(&i18n.LocalizeConfig{
					MessageID: msgForTag(fe.Tag()),
				}),
				Param: fe.Param(),
			}
		}
		return out
	}
	return nil
}

func ResFromErr(c *gin.Context, err error) {
	localizer := GetI18nLocalizer(c)

	errRes := GetErrRes(err)
	c.AbortWithStatusJSON(
		errRes.StatusCode,
		ProblemDetails{
			Title: localizer.MustLocalize(&i18n.LocalizeConfig{
				MessageID: errRes.MessageId,
			}),
		},
	)
}

func ResWithMessageID(
	c *gin.Context,
	messageId string,
	statusCode int,
) {
	localizer := GetI18nLocalizer(c)

	c.AbortWithStatusJSON(
		statusCode,
		ProblemDetails{
			Title: localizer.MustLocalize(&i18n.LocalizeConfig{
				MessageID: messageId,
			}),
		},
	)
}

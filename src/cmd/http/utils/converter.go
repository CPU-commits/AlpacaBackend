package utils

import (
	"net/http"

	appointmentService "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	fileService "github.com/CPU-commits/Template_Go-EventDriven/src/file/service"
	generatorService "github.com/CPU-commits/Template_Go-EventDriven/src/generator/services"
	publicationService "github.com/CPU-commits/Template_Go-EventDriven/src/publication/service"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type errRes struct {
	StatusCode  int
	MessageId   string
	TypeDetails string
}

var errorsService map[error]errRes = make(map[error]errRes)

func GetErrRes(err error) errRes {
	errResOk, exists := errorsService[err]
	if !exists {
		return errRes{
			StatusCode: http.StatusInternalServerError,
			MessageId:  "server.internal_error",
		}
	}

	return errResOk
}

func init() {
	errorsService[authService.ErrInvalidCredentials] = errRes{
		StatusCode: http.StatusForbidden,
		MessageId:  "auth.err_credentials",
	}
	errorsService[authService.ErrUserLoginNotFound] = errRes{
		StatusCode: http.StatusForbidden,
		MessageId:  "auth.err_credentials",
	}
	errorsService[utils.ErrRepositoryFailed] = errRes{
		StatusCode: http.StatusServiceUnavailable,
		MessageId:  "server.db_error",
	}
	errorsService[authService.ErrSessionNotExists] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "session.not_exists",
	}
	errorsService[authService.ErrUserNotFound] = errRes{
		StatusCode: http.StatusNotFound,
		MessageId:  "user.not_found",
	}
	errorsService[authService.ErrExistsEmailOrUsername] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "auth.exists_email_or_username",
	}
	errorsService[dto.ErrUnableToRegisterRole] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "auth.unable_role",
	}
	errorsService[userService.ErrUserNoHasProfile] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "user.no_profile",
	}
	errorsService[tattooService.ErrCategoriesNotExists] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "category.not_exists",
	}
	errorsService[authService.ErrUsernameNotExists] = errRes{
		StatusCode: http.StatusNotFound,
		MessageId:  "user.username_not_exists",
	}
	errorsService[publicationService.ErrPublicationNotExists] = errRes{
		StatusCode: http.StatusNotFound,
		MessageId:  "publication.not_exists",
	}
	errorsService[publicationService.ErrPublicationNotAccess] = errRes{
		StatusCode: http.StatusUnauthorized,
		MessageId:  "publication.not_have_access",
	}
	errorsService[publicationService.ErrTooManyImages] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "publication.too_images",
	}
	errorsService[fileService.ErrInvalidMimeType] = errRes{
		StatusCode: http.StatusUnsupportedMediaType,
		MessageId:  "file.invalid_mime_type",
	}
	errorsService[appointmentService.ErrUserIsNotTattooArtists] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "appointment.not_tattoo_artist",
	}
	errorsService[appointmentService.ErrUserHasNoAccessToAppointment] = errRes{
		StatusCode: http.StatusUnauthorized,
		MessageId:  "appointment.no_have_access",
	}
	errorsService[appointmentService.ErrStatusIsNotCreated] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "appointment.not_created",
	}
	errorsService[appointmentService.ErrScheduleDateMustBeAfterNow] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "appointment.scheduled_at_now",
	}
	errorsService[appointmentService.ErrScheduleDateMusteBeAfterFinished] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "appointment.scheduled_at_finished",
	}
	errorsService[appointmentService.ErrScheduleIsBussy] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "appointment.bussy",
	}
	errorsService[appointmentService.ErrAppointmentIsFinished] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "appointment.finished",
	}
	errorsService[generatorService.ErrCodeNotValid] = errRes{
		StatusCode: http.StatusUnprocessableEntity,
		MessageId:  "code.not_valid",
	}
	errorsService[generatorService.ErrTokenNotValid] = errRes{
		StatusCode: http.StatusUnprocessableEntity,
		MessageId:  "token.not_valid",
	}
	errorsService[authService.ErrNotValidToken] = errRes{
		StatusCode: http.StatusUnauthorized,
		MessageId:  "auth.not_valid_token",
	}
	errorsService[generatorService.ErrCodeTypeNotValid] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "code.type_not_valid",
	}
	errorsService[authService.ErrInvalidParams] = errRes{
		StatusCode: http.StatusBadRequest,
		MessageId:  "auth.invalid_params",
	}
	errorsService[authService.ErrExistsEmail] = errRes{
		StatusCode: http.StatusConflict,
		MessageId:  "auth.exist_email",
	}
}

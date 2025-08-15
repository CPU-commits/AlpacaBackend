package utils

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	authDto "github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	studioDto "github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/gin-gonic/gin/binding"
	"github.com/go-playground/validator/v10"
)

func RegisterCustomValidators() {
	if v, ok := binding.Validator.Engine().(*validator.Validate); ok {
		v.RegisterValidation("areavalidator", dto.IsAppointmentArea)
		v.RegisterValidation("colorvalidator", dto.IsAppointmentColor)
		v.RegisterValidation("username", authDto.IsUsername)
		//v.RegisterValidation("name", authDto.IsName)
		v.RegisterValidation("studioRole", studioDto.IsRole)
		v.RegisterValidation("permission", studioDto.IsPermission)
		v.RegisterValidation("mediaType", studioDto.IsMediaType)
		v.RegisterValidation("phone", authDto.IsPhone)
	}
}

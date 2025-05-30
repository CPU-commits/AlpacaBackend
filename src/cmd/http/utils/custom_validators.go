package utils

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/gin-gonic/gin/binding"
	"github.com/go-playground/validator/v10"
)

func RegisterCustomValidators() {
	if v, ok := binding.Validator.Engine().(*validator.Validate); ok {
		v.RegisterValidation("areavalidator", dto.IsAppointmentArea)
		v.RegisterValidation("colorvalidator", dto.IsAppointmentColor)
	}
}

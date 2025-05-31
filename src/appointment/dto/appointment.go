package dto

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/go-playground/validator/v10"
)

type AppointmentDto struct {
	IDTattooArtist int64            `form:"idTattooArtist" binding:"required"`
	Phone          string           `form:"phone" binding:"max=20"`
	HasIdea        *bool            `form:"hasIdea" binding:"required"`
	Area           string           `form:"area" binding:"required_if=HasIdea true,omitempty,areavalidator"`
	Height         float32          `form:"height" binding:"required_if=HasIdea true"`
	Width          float32          `form:"width" binding:"required_if=HasIdea true"`
	Color          string           `form:"color" binding:"required_if=HasIdea true,omitempty,colorvalidator"`
	Description    string           `form:"description" binding:"required,max=500"`
	Images         []store.ImageDto `form:"-"`
}

func (appointment *AppointmentDto) ToModel(idUser int64) *model.Appointment {
	return &model.Appointment{
		Status:         model.STATUS_CREATED,
		Phone:          appointment.Phone,
		HasIdea:        appointment.HasIdea,
		Area:           model.AppointmentArea(appointment.Area),
		Height:         appointment.Height,
		Width:          appointment.Width,
		IDUser:         idUser,
		Color:          model.AppointmentColor(appointment.Color),
		Description:    appointment.Description,
		IDTattooArtist: appointment.IDTattooArtist,
	}
}

var IsAppointmentArea validator.Func = func(fl validator.FieldLevel) bool {
	area, ok := fl.Field().Interface().(string)
	if ok {
		return model.IsAppointmentArea(area)
	}
	return false
}

var IsAppointmentColor validator.Func = func(fl validator.FieldLevel) bool {
	color, ok := fl.Field().Interface().(string)
	if ok {
		return model.IsAppointmentColor(color)
	}
	return false
}

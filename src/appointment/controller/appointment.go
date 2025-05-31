package controller

import (
	"mime"
	"mime/multipart"
	"net/http"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpAppointmentController struct {
	appointmentService *service.AppointmentService
}

func (appointmentController *HttpAppointmentController) RequestAppointment(c *gin.Context) {
	var appointmentDto *dto.AppointmentDto

	if err := c.ShouldBind(&appointmentDto); err != nil {
		utils.ResFromErr(c, err)
		return
	}
	form, err := c.MultipartForm()
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}
	files := form.File["images"]
	images, err := domainUtils.Map(files, func(file *multipart.FileHeader) (store.ImageDto, error) {
		openedFile, err := file.Open()
		if err != nil {
			return store.ImageDto{}, nil
		}
		splitName := strings.Split(file.Filename, ".")

		return store.ImageDto{
			File:     openedFile,
			Name:     file.Filename,
			MimeType: mime.TypeByExtension("." + splitName[len(splitName)-1]),
		}, nil
	})
	appointmentDto.Images = images
	claims, _ := utils.NewClaimsFromContext(c)

	if err := appointmentController.appointmentService.RequestAppointment(
		appointmentDto,
		claims.ID,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func NewHTTPAppointmentController() *HttpAppointmentController {
	return &HttpAppointmentController{
		appointmentService: appointmentService,
	}
}

package controller

import (
	"mime"
	"mime/multipart"
	"net/http"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	authServices "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type HttpAppointmentController struct {
	appointmentService *service.AppointmentService
	bus                bus.Bus
}

func (appointmentController *HttpAppointmentController) GetAppointments(c *gin.Context) {
	claims, _ := utils.NewClaimsFromContext(c)
	// Params
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	params := service.AppointmentParams{
		Page: page,
	}

	appointments, count, err := appointmentController.appointmentService.GetAppointments(
		claims.ID,
		domainUtils.Includes(claims.Roles, string(model.TATTOO_ARTIST_ROLE)),
		params,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.Header("X-Total", strconv.Itoa(int(count)))
	c.Header("X-Per-Page", "10")
	c.JSON(http.StatusOK, appointments)
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

func (appointmentController *HttpAppointmentController) CancelAppointment(c *gin.Context) {
	idAppointmentStr := c.Param("idAppointment")
	idAppointment, err := strconv.Atoi(idAppointmentStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	if err := appointmentController.appointmentService.CancelAppointment(
		int64(idAppointment),
		claims.ID,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func (appointmentController *HttpAppointmentController) ScheduleAppointment(c *gin.Context) {
	var scheduleAppointment *dto.ScheduleAppointmentDto
	if err := c.BindJSON(&scheduleAppointment); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	idAppointmentStr := c.Param("idAppointment")
	idAppointment, err := strconv.Atoi(idAppointmentStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	if err := appointmentController.appointmentService.ScheduleAppointment(
		int64(idAppointment),
		claims.ID,
		*scheduleAppointment,
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func NewHTTPAppointmentController(bus bus.Bus) *HttpAppointmentController {
	return &HttpAppointmentController{
		appointmentService: service.NewAppointmentService(
			*fileService,
			appointmentRepository,
			*authServices.NewUserService(
				userRepository,
				roleRepository,
				bus,
			),
		),
	}
}

package controller

import (
	"mime"
	"mime/multipart"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	authServices "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	userServices "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
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
	paginated := c.DefaultQuery("paginated", "true")

	fromDateStr := c.Query("from")
	var fromDate time.Time
	if fromDateStr != "" {
		var err error

		fromDate, err = time.Parse(time.RFC3339, fromDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	toDateStr := c.Query("to")
	var toDate time.Time
	if toDateStr != "" {
		var err error

		toDate, err = time.Parse(time.RFC3339, toDateStr)
		if err != nil {
			utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return
		}
	}
	statuses := domainUtils.FilterNoError(strings.Split(c.Query("statuses"), ","), func(x string) bool {
		return x != ""
	})
	allAppointments := c.DefaultQuery("allAppointments", "true")

	params := service.AppointmentParams{
		Page:            page,
		Paginated:       paginated == "true",
		FromDate:        fromDate,
		ToDate:          toDate,
		Statuses:        statuses,
		AllAppointments: allAppointments == "true",
	}

	appointments, count, err := appointmentController.appointmentService.GetAppointments(
		claims.ID,
		domainUtils.Includes(claims.Roles, model.TATTOO_ARTIST_ROLE),
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

func (appointmentController *HttpAppointmentController) GetAppointmentsPending(c *gin.Context) {
	claims, _ := utils.NewClaimsFromContext(c)

	pendingAppointments, err := appointmentController.appointmentService.GetPendingAppointments(
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"count": pendingAppointments,
	})
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
	timezone := c.Query("timezone")

	if err := appointmentController.appointmentService.ScheduleAppointment(
		int64(idAppointment),
		claims.ID,
		*scheduleAppointment,
		timezone,
		utils.GetI18nLocalizer(c),
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func (appointmentController *HttpAppointmentController) ReviewAppointment(c *gin.Context) {
	var reviewDto *dto.ReviewDTO
	if err := c.BindJSON(&reviewDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	idAppointmentStr := c.Param("idAppointment")
	idAppointment, err := strconv.Atoi(idAppointmentStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	if err := appointmentController.appointmentService.ReviewAppointment(
		*reviewDto,
		claims.ID,
		int64(idAppointment),
	); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func NewHTTPAppointmentController(bus bus.Bus) *HttpAppointmentController {
	userService := authServices.NewUserService(
		userRepository,
		roleRepository,
		bus,
	)
	profileService := userServices.NewProfileService(
		profileRepository,
		*userService,
		fileStore,
		*fileService,
		followRepository,
		publicationRDRepository,
	)

	return &HttpAppointmentController{
		appointmentService: service.NewAppointmentService(
			*fileService,
			appointmentRepository,
			*userService,
			googleCalendar,
			reviewRepository,
			*profileService,
		),
	}
}

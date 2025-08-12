package controller

import (
	"net/http"
	"strconv"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/service"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	"github.com/gin-gonic/gin"
)

type httpSubscriptionController struct {
	subscriptionService service.SubscriptionService
}

func (httpSubscriptionController *httpSubscriptionController) GetAllActivePlans(c *gin.Context) {
	forStudiosStr := c.DefaultQuery("forStudios", "false")
	forStudios, err := strconv.ParseBool(forStudiosStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	allStr := c.DefaultQuery("all", "false")
	all, err := strconv.ParseBool(allStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	plans, err := httpSubscriptionController.subscriptionService.GetAllActivePlans(forStudios, all)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, plans)
}

func (httpSubscriptionController *httpSubscriptionController) RequestSubscription(c *gin.Context) {
	idPlanStr := c.Param("idPlan")
	idPlan, err := strconv.Atoi(idPlanStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idStudioStr := c.DefaultQuery("idStudio", "0")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	link, err := httpSubscriptionController.subscriptionService.RequestSubscription(
		claims.ID,
		int64(idPlan),
		int64(idStudio),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"link": link,
	})
}

func (httpSubscriptionController *httpSubscriptionController) CancelSubscription(c *gin.Context) {
	claims, _ := utils.NewClaimsFromContext(c)
	idStudioStr := c.DefaultQuery("idStudio", "0")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	err = httpSubscriptionController.subscriptionService.CancelSubscription(claims.ID, int64(idStudio))
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, nil)
}

func (httpSubscriptionController *httpSubscriptionController) GetMySubscription(c *gin.Context) {
	idStudioStr := c.DefaultQuery("idStudio", "0")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	var idUser int64
	if idStudio == 0 {
		idUser = claims.ID
	}

	subscription, err := httpSubscriptionController.subscriptionService.GetMySubscription(
		service.ToSubscription{
			IDStudio: int64(idStudio),
			IDUser:   idUser,
		},
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, subscription)
}

func NewHttpSubscriptionController(bus bus.Bus) *httpSubscriptionController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		uidGenerator,
		bus,
	)
	peopleService := studioService.NewPeopleStudioService(
		peopleStudioRepository,
		studioRepository,
		*userService,
		peopleHistoriesRepository,
	)

	return &httpSubscriptionController{
		subscriptionService: *service.NewSubscriptionService(
			subscriptionRepository,
			planRepository,
			payments,
			paymentRepository,
			userRepository,
			roleRepository,
			studioRepository,
			*peopleService,
			peopleHistoriesRepository,
			bus,
		),
	}
}

package controller

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/gin-gonic/gin"
)

type HttpUserController struct {
	bus         bus.Bus
	userService *service.UserService
}

func NewUserHttpController(bus bus.Bus) *HttpUserController {
	return &HttpUserController{
		bus: bus,
		userService: service.NewUserService(
			sqlUserRepository,
			sqlRoleRepository,
			bus,
		),
	}
}

// UpdateEmail godoc
//
//	@Summary	Actualizar el correo electronico
//	@Tags		auth
//	@Success	200
//	@Failure	503				object		utils.ProblemDetails				"Error con la base de datos"
//	@Router		/api/auth/email [patch]
func (httpUser *HttpUserController) UpdateEmail(c *gin.Context) {
	var newEmail *dto.UpdateAuthEmailDTO
	if err := c.BindJSON(&newEmail); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	if err := httpUser.userService.UpdateEmail(*newEmail, claims.ID); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

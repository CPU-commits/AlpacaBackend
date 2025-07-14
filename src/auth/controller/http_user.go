package controller

import (
	"net/http"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
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
//	@Param		newEmail	body	dto.UpdateAuthEmailDTO	false	"nuevo email"
//	@Success	200
//	@Failure	503	object	utils.ProblemDetails	"Error con la base de datos"
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

// User Update godoc
//
//	@Summary	Actualizar informacion del usuario, name || phone
//	@Tags		auth
//	@Success	200
//	@Param		UserUpdateData	body	dto.UserUpdateData		false	"name || phone"
//	@Failure	503				object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/auth/user [patch]
func (httpUser *HttpUserController) UpdateUser(c *gin.Context) {
	var userUpdateData *dto.UserUpdateData
	if err := c.BindJSON(&userUpdateData); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	if err := httpUser.userService.UserUpdate(claims.ID, *userUpdateData); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func (httpUser *HttpUserController) SearchUsers(c *gin.Context) {
	q := c.Query("q")
	filterUsersStr := domainUtils.FilterNoError(strings.Split(c.Query("filterUsers"), ","), func(user string) bool {
		return user != ""
	})
	filterUsers, err := domainUtils.Map(filterUsersStr, func(user string) (int, error) {
		return strconv.Atoi(user)
	})
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	users, err := httpUser.userService.SearchUsers(
		q,
		domainUtils.MapNoError(filterUsers, func(idUser int) int64 {
			return int64(idUser)
		}),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, users)
}

// IsOwner godoc
//
//	@Summary	Ver si el usuario es owner
//	@Tags		auth
//	@Success	200
//	@Param		id			query	string					true	"idUser"
//	@Param		userName	query	string					true	"userName"
//	@Failure	503			object	utils.ProblemDetails	"Error con la base de datos"
//	@Router		/api/auth [Get]
func (httpUser *HttpUserController) IsOwner(c *gin.Context) {
	var Query *dto.QueryIsOwner

	if err := c.ShouldBindQuery(&Query); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)
	if err := httpUser.userService.IsOwner(claims.ID, *Query); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

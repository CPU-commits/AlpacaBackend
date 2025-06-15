package controller

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/services"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/gin-gonic/gin"
)

type HttpCodeController struct {
	bus         bus.Bus
	codeService *services.CodeService
}

func NewCodeHttpController(bus bus.Bus) *HttpCodeController {
	userService := *service.NewUserService(sqlUserRepository, sqlRoleRepository, bus)
	return &HttpCodeController{
		bus: bus,
		codeService: services.NewCodeService(
			sqlCodeRepository,
			userService,
			bus,
			*services.NewTokenService(
				sqlTokenRepository,
				tokenGenerator,
				userService,
			),
		),
	}
}

// Create Code godoc
//
//	@Summary	Crear codigo
//	@Tags		code
//	@Success	201
//	@Param		newCodeDTO	body		dto.NewCodeDTO			true	"usesRemaining, type y duration"
//	@Failure	503		{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Failure	404		{object}	utils.ProblemDetails	"Usuario no encontrado"
//	@Router		/api/generators/code [post]
func (httpCode *HttpCodeController) CreateCode(c *gin.Context) {
	var newCodeDTO *dto.NewCodeDTO

	if err := c.BindJSON(&newCodeDTO); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	claims, _ := utils.NewClaimsFromContext(c)

	newCodeDTO.IDUser = claims.ID

	if _, err := httpCode.codeService.CreateCode(*newCodeDTO); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

// Verify Code godoc
//
//	@Summary	Verificar si un codigo es valido
//	@Tags		code
//	@Success	200
//	@Param		CodeDTO	body		dto.CodeDTO			true	"code"
//	@Failure	503		{object}	utils.ProblemDetails	"Error con la base de datos"
//	@Failure	404		{object}	utils.ProblemDetails	"Usuario no encontrado"
//	@Router		/api/generators/code/verify/{code} [get]
func (httpCode *HttpCodeController) VerifyCode(c *gin.Context) {
	var codeParams dto.CodeParams

	if err := c.BindUri(&codeParams); err != nil {
		utils.ResErrValidators(c, err)
		return
	}
	if err := c.BindQuery(&codeParams); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	if err := httpCode.codeService.IsCodeValid(codeParams, claims.ID); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

package controller

import (
	"net/http"
	"strconv"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	domainUtils "github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/gin-gonic/gin"
)

type httpAdminStudioController struct {
	studioService *service.AdminStudioService
}

func (httpAdminStudioController httpAdminStudioController) GetStudioPeople(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	people, err := httpAdminStudioController.studioService.GetStudioPeople(
		claims.ID,
		int64(idStudio),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, people)
}

func (httpAdminStudioController httpAdminStudioController) GetPermissionsInStudio(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	permissions, isOwner, err := httpAdminStudioController.studioService.GetPermissionsInStudio(
		claims.ID,
		int64(idStudio),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, gin.H{
		"permissions": permissions,
		"isOwner":     isOwner,
	})
}

func (httpAdminStudioController httpAdminStudioController) JoinPeople(c *gin.Context) {
	var personDto *dto.PersonDTO
	if err := c.BindJSON(&personDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idUserStr := c.Param("idUser")
	idUser, err := strconv.Atoi(idUserStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	err = httpAdminStudioController.studioService.JoinPerson(
		int64(idUser),
		int64(idStudio),
		claims.ID,
		domainUtils.MapNoError(personDto.Roles, func(role string) model.StudioRole {
			return model.StudioRole(role)
		}),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func (httpAdminStudioController httpAdminStudioController) SetPermission(c *gin.Context) {
	var permissionDto *dto.PermissionDTO
	if err := c.BindJSON(&permissionDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idUserStr := c.Param("idUser")
	idUser, err := strconv.Atoi(idUserStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	err = httpAdminStudioController.studioService.SetPermission(
		int64(idUser),
		int64(idStudio),
		claims.ID,
		permissionDto,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func (httpAdminStudioController httpAdminStudioController) ChangeRole(c *gin.Context) {
	var personDto *dto.PersonDTO
	if err := c.BindJSON(&personDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idUserStr := c.Param("idUser")
	idUser, err := strconv.Atoi(idUserStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	err = httpAdminStudioController.studioService.ChangeRoles(
		int64(idUser),
		int64(idStudio),
		claims.ID,
		domainUtils.MapNoError(personDto.Roles, func(role string) model.StudioRole {
			return model.StudioRole(role)
		}),
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, nil)
}

func (httpAdminStudioController httpAdminStudioController) RemovePerson(c *gin.Context) {
	idStudioStr := c.Param("idStudio")
	idStudio, err := strconv.Atoi(idStudioStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idUserStr := c.Param("idUser")
	idUser, err := strconv.Atoi(idUserStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)

	err = httpAdminStudioController.studioService.RemovePerson(
		int64(idUser),
		int64(idStudio),
		claims.ID,
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, nil)
}

func NewHttpAdminStudioController(bus bus.Bus) httpAdminStudioController {
	userService := authService.NewUserService(
		userRepository,
		roleRepository,
		bus,
	)
	studioPeopleService := service.NewPeopleStudioService(
		studioAdminRepository,
		studioRepository,
		*userService,
	)

	return httpAdminStudioController{
		studioService: studioPeopleService,
	}
}

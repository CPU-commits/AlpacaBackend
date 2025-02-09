package controller

import (
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/dto"
	"github.com/gin-gonic/gin"
)

type HttpProfileController struct{}

func (*HttpProfileController) GetProfile(c *gin.Context) {
	username := c.Param("username")

	profile, err := profileService.GetProfile(username)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusOK, profile)
}

func (*HttpProfileController) UpdateProfile(c *gin.Context) {
	var updateProfileDto *dto.UpdateProfileDto
	if err := c.BindJSON(&updateProfileDto); err != nil {
		utils.ResErrValidators(c, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	if err := profileService.UpdateProfile(updateProfileDto, claims.ID); err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusNoContent, nil)
}

func (*HttpProfileController) ChangeAvatar(c *gin.Context) {
	avatar, err := c.FormFile("avatar")
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	file, err := avatar.Open()
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	claims, _ := utils.NewClaimsFromContext(c)
	key, err := profileService.ChangeAvatar(store.ImageDto{
		File: file,
	}, claims.ID)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.JSON(http.StatusCreated, gin.H{
		"key": key,
	})
}

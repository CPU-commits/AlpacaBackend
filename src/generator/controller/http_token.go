package controller

type HttpTokenController struct{}

// // Create Code godoc
// //
// //	@Summary	Crear codigo
// //	@Tags		code
// //	@Success	201
// //	@Param		newCodeDTO	body		dto.NewCodeDTO			true	"usesRemaining, type y duration"
// //	@Failure	503		{object}	utils.ProblemDetails	"Error con la base de datos"
// //	@Failure	404		{object}	utils.ProblemDetails	"Usuario no encontrado"
// //	@Router		/api/code [post]
// func (*HttpTokenController) CreateCode(c *gin.Context) {
// 	var newCodeDTO *dto.TokenDTO

// 	if err := c.BindJSON(&newCodeDTO); err != nil {
// 		utils.ResErrValidators(c, err)
// 		return
// 	}
// 	claims, _ := utils.NewClaimsFromContext(c)

// 	newCodeDTO.IDUser = claims.ID

// 	if _, err := codeService.CreateCode(*newCodeDTO); err != nil {
// 		utils.ResFromErr(c, err)
// 		return
// 	}

// 	c.JSON(http.StatusCreated, nil)
// }

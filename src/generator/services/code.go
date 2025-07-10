package services

import (
	"time"

	userService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/code_repository"
	notificationsM "github.com/CPU-commits/Template_Go-EventDriven/src/notifications/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var codeService *CodeService

type CodeService struct {
	codeRepository code_repository.CodeRepository
	userService    userService.UserService
	tokenService   TokenService
	bus            bus.Bus
}

func NewCodeService(
	codeRepository code_repository.CodeRepository,
	userService userService.UserService,
	bus bus.Bus,
	tokenService TokenService,

) *CodeService {
	if codeService == nil {
		codeService = &CodeService{
			codeRepository: codeRepository,
			userService:    userService,
			bus:            bus,
			tokenService:   tokenService,
		}
	}
	return codeService
}

const (
	token_expires = time.Hour * 3
)

func (codeService *CodeService) CheckCodeType(code model.Code) error {
	if check := utils.Includes(model.CodeTypeList, code.Type); !check {
		return ErrCodeTypeNotValid
	}

	return nil
}

func (codeService *CodeService) CreateCode(newCode dto.NewCodeDTO) (*model.Code, error) {
	codeModel := newCode.ToModel()

	if err := codeService.CheckCodeType(*codeModel); err != nil {
		return nil, ErrCodeTypeNotValid
	}

	user, err := codeService.userService.GetUserById(codeModel.IDUser)
	if err != nil {
		return nil, err
	}
	randomString, err := utils.GenerateRandomString(6)
	if err != nil {
		return nil, err
	}

	codeModel.Code = randomString

	duration := time.Duration(newCode.Duration)

	code, err := codeService.codeRepository.InsertOne(*codeModel, duration)
	if err != nil {
		return nil, err
	}

	dataSendEmail := notificationsM.TemplateData{
		Name:  user.Username,
		Code:  code.Code,
		Email: user.Email,
	}

	switch codeModel.Type {
	case model.CodeTypeList[0]:
		go codeService.bus.Publish(bus.Event{
			Name:    SEND_EMAIL_RESET,
			Payload: utils.ToPayload(dataSendEmail),
		})
	case model.CodeTypeList[1]:
		go codeService.bus.Publish(bus.Event{
			Name:    SEND_EMAIL_PASSWORD_RESET,
			Payload: utils.ToPayload(dataSendEmail),
		})
	}

	return code, nil
}
func (codeService *CodeService) IsCodeValid(codeParams dto.CodeParams, idUser int64) error {
	codeModel := model.Code{
		Code:   codeParams.Code,
		IDUser: idUser,
	}
	var nameEvent bus.EventName
	var codeType string

	switch codeParams.Type {
	case model.TokenType[0]:
		codeType = model.CodeTypeList[0]
		nameEvent = NEW_TOKEN_EMAIL_UPDATE
	case model.TokenType[1]:
		codeType = model.CodeTypeList[1]
		nameEvent = NEW_TOKEN_PASSWORD_UPDATE
	default:
		return ErrCodeTypeNotValid
	}
	resultCode, err := codeService.codeRepository.VerifyCode(codeModel, codeType)
	if err != nil {
		return ErrCodeNotValid
	}
	if err := utils.VerifyNotExpiredAt(resultCode.ExpiresAt, "local", ErrCodeNotValid); err != nil {
		return err
	}

	user, err := codeService.userService.GetUserById(codeModel.IDUser)
	if err != nil {
		return err
	}

	expiredAt := time.Now().Add(token_expires)

	token, err := codeService.tokenService.CreateRecoveryToken(expiredAt, *user)
	if err != nil {
		return err
	}

	go codeService.bus.Publish(bus.Event{
		Name:    nameEvent,
		Payload: utils.Payload(token),
	})

	return nil
}

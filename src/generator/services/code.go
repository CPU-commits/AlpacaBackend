package services

import (
	userService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/repository/code_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type CodeService struct {
	codeRepository code_repository.CodeRepository
	userService    userService.UserService
}

func NewCodeService(
	codeRepository code_repository.CodeRepository,
	userService userService.UserService,

) *CodeService {
	return &CodeService{
		codeRepository: codeRepository,
		userService:    userService,
	}
}

func (codeService *CodeService) CreateCode(newCode dto.NewCodeDTO) (*model.Code, error) {
	codeModel := newCode.ToModel()

	if _, err := codeService.userService.GetUserById(codeModel.IDUser); err != nil {
		return nil, err
	}
	randomString, err := utils.GenerateRandomString(6)
	if err != nil {
		return nil, err
	}

	codeModel.Code = randomString

	return codeService.codeRepository.InsertOne(*codeModel, newCode.Duration)
}
func (codeService *CodeService) IsCodeValid(code dto.CodeDTO) error {
	codeModel := code.ToModel()

	resultCode, err := codeService.codeRepository.VerifyCode(*codeModel)
	if err != nil {
		return ErrCodeNotValid
	}
	if err := utils.VerifyNotExpiredAt(resultCode.ExpiresAt, "local", ErrCodeNotValid); err != nil {
		return err
	}

	return nil
}

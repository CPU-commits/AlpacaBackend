package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var fileServiceInstance *FileService

type FileService struct{}

func (fileService *FileService) CheckImageMimeType(file store.ImageDto) error {

	check := utils.Includes(model.FileImageMimeTypeList, file.MimeType)
	if !check {
		return ErrInvalidMimeType
	}

	return nil
}

func NewFileService() *FileService {
	if fileServiceInstance == nil {
		fileServiceInstance = &FileService{}
	}
	return fileServiceInstance
}

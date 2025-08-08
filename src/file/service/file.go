package service

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

var fileServiceInstance *FileService

type FileService struct {
	imageStore store.ImageStore
}

func (fileService *FileService) UploadImages(imagesDto []store.ImageDto, workspace string) ([]model.Image, error) {
	images, err := utils.ConcurrentMap(imagesDto, func(imageDto store.ImageDto) (model.Image, error) {
		err := fileService.CheckImageMimeType(imageDto)
		if err != nil {
			return model.Image{}, err
		}
		image, err := fileService.imageStore.Upload(imageDto, workspace)
		if err != nil {
			return model.Image{}, err
		}

		return *image, nil
	}, nil)
	if err != nil {
		return nil, err
	}
	return images, nil
}

func (fileService *FileService) CheckImageMimeType(file store.ImageDto) error {
	check := utils.Includes(model.FileImageMimeTypeList, file.MimeType)
	if !check {
		return ErrInvalidMimeType
	}

	return nil
}
func (fileService *FileService) DeleteImg(key string) error {
	return fileService.imageStore.Delete(key)
}

func NewFileService(imageStore store.ImageStore) *FileService {
	if fileServiceInstance == nil {
		fileServiceInstance = &FileService{
			imageStore: imageStore,
		}
	}
	return fileServiceInstance
}

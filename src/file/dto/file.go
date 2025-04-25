package dto

import (
	"io"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
)

type FileDTO struct {
	Name     string
	MimeType string
	File     io.Reader
}

func (fileDTO *FileDTO) ToModel(key string) model.Image {
	return model.Image{
		Name:     fileDTO.Name,
		MimeType: fileDTO.MimeType,
		Key:      key,
	}
}

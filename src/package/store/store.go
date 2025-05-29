package store

import (
	"io"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
)

type ImageDto struct {
	Name     string
	MimeType string
	File     io.Reader
}

type ImageStore interface {
	Upload(image ImageDto, namespace string) (*model.Image, error)
	Delete(key string) error
	Download(key string) ([]byte, error)
}

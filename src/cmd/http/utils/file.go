package utils

import (
	"errors"
	"fmt"
	"mime"
	"net/http"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/gin-gonic/gin"
)

type formFileValidator struct {
	name   string
	params map[string]any
}

func Mime(mime string) formFileValidator {
	return formFileValidator{
		name: "mime",
		params: map[string]any{
			"mime": mime,
		},
	}
}

func OpenFormFile(c *gin.Context, field string, validators ...formFileValidator) (*store.ImageDto, error) {
	var errNoSuchFile error = fmt.Errorf("http: no such file")

	fileForm, err := c.FormFile(field)
	if err != nil && err.Error() != errNoSuchFile.Error() {
		ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return nil, err
	}
	var imageDto *store.ImageDto

	if fileForm != nil {
		avatarImage, err := fileForm.Open()
		if err != nil || avatarImage == nil {
			ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
			return nil, err
		}
		splitName := strings.Split(fileForm.Filename, ".")
		mimeType := mime.TypeByExtension("." + splitName[len(splitName)-1])

		imageDto = &store.ImageDto{
			File:     avatarImage,
			Name:     fileForm.Filename,
			MimeType: mimeType,
		}
		for _, validator := range validators {
			if validator.name == "mime" && !strings.Contains(mimeType, validator.params["mime"].(string)) {
				ResWithMessageID(c, "form.error", http.StatusBadRequest, errors.New("err: validator mime"))
				return nil, errors.New("err: validator mime")
			}
		}
	}

	return imageDto, nil
}

package cloudinary_store

import (
	"context"
	"errors"

	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/store"
	"github.com/cloudinary/cloudinary-go/v2"
	"github.com/cloudinary/cloudinary-go/v2/api/uploader"
)

func credentials() (*cloudinary.Cloudinary, context.Context) {
	cld, err := cloudinary.New()
	if err != nil {
		panic(err)
	}
	cld.Config.URL.Secure = true
	ctx := context.Background()

	return cld, ctx
}

type cloudinaryImageStore struct {
	cld *cloudinary.Cloudinary
	ctx context.Context
}

func (cloudImageStore cloudinaryImageStore) Upload(image store.ImageDto, namespace string) (*model.Image, error) {
	result, err := cloudImageStore.cld.Upload.Upload(cloudImageStore.ctx, image.File, uploader.UploadParams{
		Folder: namespace,
	})
	if err != nil {
		return nil, err
	}
	if result.Error.Message != "" {
		return nil, errors.New(result.Error.Message)
	}

	return &model.Image{
		Key:      result.PublicID,
		Name:     image.Name,
		MimeType: image.MimeType,
	}, nil
}

func (cloudImageStore cloudinaryImageStore) Delete(key string) error {
	_, err := cloudImageStore.cld.Upload.Destroy(context.Background(), uploader.DestroyParams{
		PublicID: key,
	})
	return err
}

func NewCloudinaryImageStore() store.ImageStore {
	cld, ctx := credentials()
	return cloudinaryImageStore{
		cld: cld,
		ctx: ctx,
	}
}

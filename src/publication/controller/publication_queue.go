package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
)

type QueuePublicationController struct{}

func (*QueuePublicationController) IndexPublication(c bus.Context) error {
	var publication model.Publication

	if err := c.BindData(&publication); err != nil {
		return c.Kill(err.Error())
	}
	return publicationTSRepository.IndexPublication(
		&publication,
	)
}

func NewPublicationQueueController() *QueuePublicationController {
	return &QueuePublicationController{}
}

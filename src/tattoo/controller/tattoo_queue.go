package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
)

type QueueTattooController struct{}

func (*QueueTattooController) IndexTattoo(c bus.Context) error {
	var tattoos model.Tattoo
	if err := c.BindData(&tattoos); err != nil {
		return c.Kill(err.Error())
	}

	return tattooTSRepository.IndexTattoo(
		&tattoos,
	)
}

func NewTattooQueueController() *QueueTattooController {
	return &QueueTattooController{}
}

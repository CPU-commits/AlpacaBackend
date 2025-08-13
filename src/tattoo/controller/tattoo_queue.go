package controller

import (
	"fmt"

	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	profileService "github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type QueueTattooController struct {
	tattoService service.TattooService
}

func (*QueueTattooController) IndexTattoo(c bus.Context) error {
	var tattoos model.Tattoo
	if err := c.BindData(&tattoos); err != nil {
		return c.Kill(err.Error())
	}
	return tattooTSRepository.IndexTattoo(
		&tattoos,
	)
}

func (*QueueTattooController) UpdateRatings(c bus.Context) error {
	var data service.DataUpdateRating
	BatchSize := 10
	if err := c.BindData(&data); err != nil {
		return c.Kill(err.Error())
	}
	fmt.Printf("data.TSPublication.ID: %v\n", data.TSPublication.ID)

	err := utils.ConcurrentForEach(data.Tattoos, func(tattoo model.Tattoo, setError func(err error)) {
		err := tattooTSRepository.UpdateRatingTattoo(&tattoo, &data.TSPublication)
		if err != nil {
			setError(err)
			return
		}

	}, &utils.OptionsConcurrentForEach{
		MaxConcurrency: BatchSize,
	})
	if err != nil {
		return c.Kill(err.Error())
	}
	return nil
}

func (*QueueTattooController) DeleteTattoos(c bus.Context) error {
	var tattoos []model.Tattoo
	if tattoos == nil {
		return nil
	}
	BatchSize := 5
	if err := c.BindData(&tattoos); err != nil {
		return c.Kill(err.Error())
	}

	err := utils.ConcurrentForEach(tattoos, func(tattoo model.Tattoo, setError func(err error)) {
		if err := tattooTSRepository.DeleteTattoo(&tattoo); err != nil {
			setError(err)
			return
		}
	}, &utils.OptionsConcurrentForEach{
		MaxConcurrency: BatchSize,
	})
	if err != nil {
		return c.Kill(err.Error())
	}
	return nil
}

func NewTattooQueueController(bus bus.Bus) *QueueTattooController {
	profileService := *profileService.NewProfileService(
		profileRepository,
		*authService.NewUserService(
			userRepository,
			roleRepository,
			uidGenerator,
			bus),
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		profileService.NewFollowService(followRepository),
	)
	return &QueueTattooController{
		tattoService: *service.NewTattooService(
			imageStore,
			profileService,
			tattooRepository,
			*fileService,
			embeddingapi.NewAPIEmbedding(),
			bus,
		),
	}
}

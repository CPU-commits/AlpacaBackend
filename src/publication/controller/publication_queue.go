package controller

import (
	authService "github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	embeddingapi "github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding/embedding_api"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	tattooService "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type QueuePublicationController struct {
	profileService service.ProfileService
	tattooService  tattooService.TattooService
}

func NewPublicationQueueController(
	bus bus.Bus,
) *QueuePublicationController {
	profileService := *service.NewProfileService(
		profileRepository,
		*authService.NewUserService(userRepository, roleRepository, uidGenerator, bus),
		imageStore,
		*fileService,
		followRepository,
		publicationRDRepository,
		*viewService,
		service.SinglentonFollowService(),
	)
	return &QueuePublicationController{
		profileService: profileService,
		tattooService: *tattooService.NewTattooService(
			imageStore,
			profileService,
			tattooRepository,
			*fileService,
			embeddingapi.NewAPIEmbedding(),
			bus,
			*viewService,
		),
	}
}

func (*QueuePublicationController) IndexPublication(c bus.Context) error {
	var publication model.Publication

	if err := c.BindData(&publication); err != nil {
		return c.Kill(err.Error())
	}
	return publicationTSRepository.IndexPublication(
		&publication,
	)
}

func (*QueuePublicationController) InteractionEvent(c bus.Context) error {
	var publication model.Publication

	if err := c.BindData(&publication); err != nil {
		return c.Kill(err.Error())
	}
	return publicationRDRepository.AddInteraction(
		&publication,
	)
}

func (queueController *QueuePublicationController) UpdateRatings(c bus.Context) error {
	BatchSize := 20

	rPublication, err := publicationRDRepository.GetAllPublications()
	if err != nil {
		return err
	}

	err = utils.ConcurrentForEach(rPublication, func(rPublication model.RedisPublication, setError func(err error)) {
		publication, err := publicationRepository.FindOne(&publication_repository.Criteria{
			ID: rPublication.IDPublication,
		}, nil)
		if err != nil {
			setError(err)
			return
		}
		if publication == nil {
			return
		}
		follows, err := queueController.profileService.GetFollows(rPublication.IDProfile)
		if err != nil {
			setError(err)
			return
		}
		daysSincePublish, err := utils.DaysSinceCreation(rPublication.CreatedAt)
		if err != nil {
			setError(err)
			return
		}
		// Se recibe pub actualizada
		// Se reciben todos los tatuajes vinculadas a esta pub
		// Se entrega al servicio todos los tatuajes y la publicacion actualizada
		tsPublication, err := publicationTSRepository.UpdatePublication(publication, daysSincePublish, int(follows))
		if err != nil {
			setError(err)
			return
		}
		tattoos, _, err := queueController.tattooService.GetTattoos(tattooService.GetTattoosParams{
			IDPublication: publication.ID,
		}, 0, "", "")
		if err != nil {
			setError(err)
			return
		}
		queueController.tattooService.UpdateRating(*tsPublication, tattoos)

		err = publicationRDRepository.DeleteRedisPublications(&rPublication)
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

func (*QueuePublicationController) AddTemporalView(c bus.Context) error {
	var data model.TemporalViewPublication
	if err := c.BindData(&data); err != nil {
		return c.Kill(err.Error())
	}

	return publicationRDRepository.AddView(
		data.IDPublication, data.Identifier,
	)
}
func (queueController *QueuePublicationController) ExistsTemporalView(c bus.Context) (*bus.BusResponse, error) {
	var data model.TemporalViewPublication
	if err := c.BindData(&data); err != nil {
		return nil, c.Kill(err.Error())
	}
	views, err := publicationRDRepository.GetAllUserView(data.Identifier)
	if err != nil {
		return nil, err
	}

	exists := utils.Includes(views, data.IDPublication)

	return &bus.BusResponse{
		Success: true,
		Data:    exists,
	}, nil
}
func (queueController *QueuePublicationController) DeletePublication(c bus.Context) error {
	var publication model.Publication

	if err := c.BindData(&publication); err != nil {
		return c.Kill(err.Error())
	}
	if err := publicationTSRepository.DeletePublication(&publication); err != nil {
		return c.Kill(err.Error())
	}

	if err := queueController.tattooService.DeleteTattoos(publication.Tattoos); err != nil {
		return c.Kill(err.Error())
	}
	return nil
}

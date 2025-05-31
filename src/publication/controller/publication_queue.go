package controller

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/repository/publication_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
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

func (*QueuePublicationController) InteractionEvent(c bus.Context) error {
	var publication model.Publication

	if err := c.BindData(&publication); err != nil {
		return c.Kill(err.Error())
	}
	return publicationRDRepository.AddInteraction(
		&publication,
	)
}

func (*QueuePublicationController) UpdateRatings(c bus.Context) error {
	BatchSize := 15

	rPublication, err := publicationRDRepository.GetAllPublications()
	if err != nil {
		return err
	}

	err = utils.ConcurrentForEach(rPublication, func(rPublication publication_repository.RedisPublication) error {
		publication, err := publicationRepository.FindOne(&publication_repository.Criteria{
			ID: rPublication.IDPublication,
		}, nil)
		if err != nil {
			return err
		}
		follows, err := profileService.GetFollows(rPublication.IDProfile)
		if err != nil {
			return err
		}
		daysSincePublish, err := utils.DaysSinceCreation(rPublication.CreatedAt)
		if err != nil {
			return err
		}

		if publication != nil {
			err = publicationTSRepository.UpdatePublication(publication, daysSincePublish, int(follows))
			if err != nil {
				return err
			}
			err = publicationRDRepository.Delete(&rPublication)
			if err != nil {
				return err
			}
		}

		return nil
	}, &utils.OptionsConcurrentForEach{
		MaxConcurrency: BatchSize,
	})

	if err != nil {
		return c.Kill(err.Error())
	}
	return nil
}

func NewPublicationQueueController() *QueuePublicationController {
	return &QueuePublicationController{}
}

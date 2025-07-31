package service

import (
	"fmt"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/shorter/repository/link_repository"
	studioModel "github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	studioService "github.com/CPU-commits/Template_Go-EventDriven/src/studio/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/service"
)

type ShorterService struct {
	linkRepository link_repository.LinkRepository
	viewService    service.ViewService
	peopleService  *studioService.AdminStudioService
}

var shorterService *ShorterService

func (shorterService *ShorterService) GetLinkMetrics(
	idUser,
	idStudio,
	idLink int64,
	from time.Time,
	to time.Time,
	fromComparative time.Time,
	toComparative time.Time,
) (*Metrics, error) {
	if idStudio != 0 {
		fmt.Printf("shorterService.peopleService: %v\n", shorterService.peopleService)
		if err := shorterService.peopleService.ThrowAccessInStudio(
			idUser,
			idStudio,
			studioModel.SHOW_METRICS_PERMISSION,
		); err != nil {
			return nil, err
		}
	} else {
		hasAccess, err := shorterService.linkRepository.Exists(
			&link_repository.Criteria{
				IDUser: idUser,
				ID:     idLink,
			},
		)
		if err != nil {
			return nil, err
		}
		if !hasAccess {
			return nil, ErrNoAccessToLink
		}
	}

	stats, count, err := shorterService.viewService.StatsViews(
		service.ToView{
			IDLink: idLink,
		},
		from,
		to,
	)
	if err != nil {
		return nil, err
	}
	statsC, countC, err := shorterService.viewService.StatsViews(
		service.ToView{
			IDLink: idLink,
		},
		fromComparative,
		toComparative,
	)
	if err != nil {
		return nil, err
	}

	return &Metrics{
		ByTime: MetricByTime{
			Stats: stats,
			Count: count,
		},
		ByTimeComparative: MetricByTime{
			Stats: statsC,
			Count: countC,
		},
	}, nil
}

func (shorterService *ShorterService) GetLinkFromShortCode(
	shortCode string,
	ip string,
) (string, error) {
	link, err := shorterService.linkRepository.FindOne(
		&link_repository.Criteria{
			ShortCode: shortCode,
		},
	)
	if err != nil {
		return "", err
	}
	if link == nil {
		return "", ErrNoLink
	}
	go shorterService.viewService.AddPermanentViewIfTemporalViewNotExists(
		ip,
		service.ToView{
			IDLink: link.ID,
		},
		&ip,
	)

	return link.Link, nil
}

func NewShorterService(
	linkRepository link_repository.LinkRepository,
	viewService service.ViewService,
	peopleService *studioService.AdminStudioService,
) *ShorterService {
	if shorterService == nil {
		shorterService = &ShorterService{
			linkRepository: linkRepository,
			viewService:    viewService,
			peopleService:  peopleService,
		}
	}

	return shorterService
}

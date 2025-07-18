package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/shorter/repository/link_repository"

type ShorterService struct {
	linkRepository link_repository.LinkRepository
}

var shorterService *ShorterService

func (shorterService *ShorterService) GetLinkFromShortCode(
	shortCode string,
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

	return link.Link, nil
}

func NewShorterService(
	linkRepository link_repository.LinkRepository,
) *ShorterService {
	if shorterService == nil {
		shorterService = &ShorterService{
			linkRepository: linkRepository,
		}
	}

	return shorterService
}

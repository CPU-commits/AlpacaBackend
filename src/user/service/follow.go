package service

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/follow/repository/follow_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
)

type FollowService struct {
	followRepository follow_repository.FollowRepository
}

var followService *FollowService

func (followService *FollowService) StatsFollows(
	idProfile,
	idStudio int64,
	from time.Time,
	toTime time.Time,
) ([]StatFollow, int64, error) {
	follows, err := followService.followRepository.CountGroupByDay(&follow_repository.Criteria{
		IDProfile: idProfile,
		IDStudio:  idStudio,
		CreatedAt: &repository.CriteriaTime{
			GTE: from,
			LTE: toTime,
		},
	})
	if err != nil {
		return nil, 0, err
	}
	var count int64

	stats := utils.IterateDates(from.Truncate(24*time.Hour), toTime.Truncate(24*time.Hour), func(d time.Time) StatFollow {
		follow := utils.FindNoError(follows, func(follow follow_repository.CountGroupByDayResult) bool {
			return follow.Day.Equal(d)
		})
		if follow != nil {
			count += follow.Follows

			return StatFollow{
				Follows: follow.Follows,
				Day:     follow.Day,
			}
		}

		return StatFollow{
			Day: d,
		}
	})

	return stats, count, nil
}

func NewFollowService(
	followRepository follow_repository.FollowRepository,
) *FollowService {
	if followService == nil {
		followService = &FollowService{
			followRepository: followRepository,
		}
	}

	return followService
}

func SinglentonFollowService() *FollowService {
	return followService
}

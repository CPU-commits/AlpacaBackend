package service

import (
	"fmt"
	"net"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/common/repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/ip"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/temporal_view_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/view/repository/view_repository"
)

type ViewService struct {
	viewRepository         view_repository.ViewRepository
	temporalViewRepository temporal_view_repository.TemporalViewRepository
	ip                     ip.IP
}

var viewService *ViewService

type ToView struct {
	IDPost    int64
	IDProfile int64
	IDLink    int64
	IDStudio  int64
}

func (viewService *ViewService) StatsViewsByLocation(
	to ToView,
	from time.Time,
	toTime time.Time,
	geo ...string,
) (map[string]int64, int64, error) {
	views, err := viewService.viewRepository.CountGroupByDayAndLocation(&view_repository.Criteria{
		IDPost:    to.IDPost,
		IDProfile: to.IDProfile,
		IDStudio:  to.IDStudio,
		IDLink:    to.IDLink,
		CreatedAt: &repository.CriteriaTime{
			GTE: from,
			LTE: toTime,
		},
	})
	if err != nil {
		return nil, 0, err
	}
	var count int64
	var stats map[string]int64 = make(map[string]int64)

	utils.ConcurrentForEach(geo, func(tag string, setError func(err error)) {
		statsLocation := utils.FilterNoError(views, func(view view_repository.CountGroupByDayResultAndLocation) bool {
			return view.City == tag || view.Country == tag || view.Region == tag
		})

		stats[tag] = utils.ReduceNoError(statsLocation, func(acum int64, view view_repository.CountGroupByDayResultAndLocation) int64 {
			return acum + view.Views
		}, 0)
		count += stats[tag]
	}, nil)

	return stats, count, nil
}

func (viewService *ViewService) StatsViews(
	to ToView,
	from time.Time,
	toTime time.Time,
) ([]StatView, int64, error) {
	views, err := viewService.viewRepository.CountGroupByDay(&view_repository.Criteria{
		IDPost:    to.IDPost,
		IDProfile: to.IDProfile,
		IDLink:    to.IDLink,
		IDStudio:  to.IDStudio,
		CreatedAt: &repository.CriteriaTime{
			GTE: from,
			LTE: toTime,
		},
	})
	if err != nil {
		return nil, 0, err
	}
	var count int64

	stats := utils.IterateDates(from.Truncate(24*time.Hour), toTime.Truncate(24*time.Hour), func(d time.Time) StatView {
		view := utils.FindNoError(views, func(view view_repository.CountGroupByDayResult) bool {
			return view.Day.Equal(d)
		})
		if view != nil {
			count += view.Views

			return StatView(*view)
		}

		return StatView{
			Day: d,
		}
	})

	return stats, count, nil
}

func (viewService *ViewService) AddPermanentViewIfTemporalViewNotExists(
	identifier string,
	to ToView,
	ip *string,
) error {
	var key string
	if to.IDPost != 0 {
		key = fmt.Sprintf("post-%d", to.IDPost)
	} else if to.IDProfile != 0 {
		key = fmt.Sprintf("profile-%d", to.IDPost)
	}
	exists, err := viewService.temporalViewRepository.ExistsView(key, identifier)
	if err != nil {
		return err
	}
	if exists {
		return nil
	}

	if err = viewService.temporalViewRepository.AddView(key, identifier); err != nil {
		return err
	}
	var country string
	var city string
	var continent string
	var region string
	var timezone string

	if ip != nil {
		info, err := viewService.ip.Info(net.ParseIP(*ip))
		if err == nil {
			country = info.Country
			city = info.City
			continent = info.ContinentCode
			region = info.Region
			timezone = info.TimeZone
		}
	}

	return viewService.viewRepository.Insert(model.View{
		IDProfile: to.IDProfile,
		IDPost:    to.IDPost,
		IDLink:    to.IDLink,
		IDStudio:  to.IDStudio,
		Country:   country,
		City:      city,
		Continent: continent,
		Region:    region,
		TimeZone:  timezone,
	})
}

func (viewService *ViewService) AddPermanentView(
	idUser int64,
	to ToView,
	ip *string,
) error {
	var country string
	var city string
	var continent string
	var region string
	var timezone string

	if ip != nil {
		info, err := viewService.ip.Info(net.ParseIP(*ip))
		if err == nil {
			country = info.Country
			city = info.City
			continent = info.ContinentCode
			region = info.Region
			timezone = info.TimeZone
		}
	}

	return viewService.viewRepository.Insert(model.View{
		IDUser:    idUser,
		IDPost:    to.IDPost,
		IDProfile: to.IDProfile,
		IDStudio:  to.IDStudio,
		IDLink:    to.IDLink,
		Country:   country,
		City:      city,
		Continent: continent,
		Region:    region,
		TimeZone:  timezone,
	})
}

func NewViewService(
	viewRepository view_repository.ViewRepository,
	temporalViewRepository temporal_view_repository.TemporalViewRepository,
	ip ip.IP,
) *ViewService {
	if viewService == nil {
		viewService = &ViewService{
			viewRepository:         viewRepository,
			temporalViewRepository: temporalViewRepository,
			ip:                     ip,
		}
	}

	return viewService
}

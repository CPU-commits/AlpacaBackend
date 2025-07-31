package ipipinfo

import (
	"net"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/ip"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/ipinfo/go/v2/ipinfo"
)

var settingsData = settings.GetSettings()

type i struct {
	client *ipinfo.Client
}

func (i i) Info(ipd net.IP) (*ip.IPInfo, error) {
	info, err := i.client.GetIPInfo(ipd)
	if err != nil {
		return nil, err
	}

	return &ip.IPInfo{
		City:          info.City,
		Country:       info.Country,
		ContinentCode: info.Continent.Code,
		TimeZone:      info.Timezone,
		Region:        info.Region,
	}, nil
}

func NewIPIpInfo() ip.IP {
	return i{
		client: ipinfo.NewClient(nil, nil, settingsData.IPINFO_TOKEN),
	}
}

package ip

import "net"

type IPInfo struct {
	City          string
	Country       string
	ContinentCode string
	TimeZone      string
	Region        string
}

type IP interface {
	Info(ip net.IP) (*IPInfo, error)
}

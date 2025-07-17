package controller

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

type HttpEmailComtroller struct {
	bus bus.Bus
}

func NewEmailHttpController(bus bus.Bus) *HttpEmailComtroller {
	return &HttpEmailComtroller{
		bus: bus,
	}
}

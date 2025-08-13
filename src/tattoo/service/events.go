package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

var (
	UPDATE_TATTOOS_RATINGS bus.EventName = "tattoo.update_tattoos_rating"
	DELETE_TATTOO          bus.EventName = "tattoo.delete_tattoo"
)

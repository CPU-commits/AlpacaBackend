package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

const (
	NEW_PUBLICATION           bus.EventName = "publication.new_publication"
	PUBLICATION_INTERACTION   bus.EventName = "publication.publication_interaction"
	PUBLICATION_UPDATE_RATING bus.EventName = "publication.update_rating"
	ADD_TEMPORAL_VIEW         bus.EventName = "publication.add_temporal_view"
	DELETE_PUBLICATION        bus.EventName = "publication.delete_publication"

	NEW_TATTOO bus.EventName = "tattoo.new_tattoo"
)

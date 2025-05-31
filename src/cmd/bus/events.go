package bus

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

const (
	NEW_PUBLICATION           bus.EventName = "publication.new_publication"
	PUBLICATION_INTERACTION   bus.EventName = "publication.publication_interaction"
	PUBLICATION_UPDATE_RATING bus.EventName = "publication.update_rating"
)

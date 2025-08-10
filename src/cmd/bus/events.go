package bus

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

const (
	NEW_PUBLICATION           bus.EventName = "publication.new_publication"
	PUBLICATION_INTERACTION   bus.EventName = "publication.publication_interaction"
	PUBLICATION_UPDATE_RATING bus.EventName = "publication.update_rating"
	ADD_TEMPORAL_VIEW         bus.EventName = "publication.add_temporal_view"
	DELETE_PUBLICATION        bus.EventName = "publication.delete_publication"
	VERIFY_TEMPORAL_VIEW      bus.EventName = "publications.verify_temporal_view"

	NEW_TATTOO             bus.EventName = "tattoo.new_tattoo"
	UPDATE_TATTOOS_RATINGS bus.EventName = "tattoo.update_tattoos_rating"
	DELETE_TATTOO          bus.EventName = "tattoo.delete_tattoo"

	NEW_TOKEN_EMAIL_UPDATE    bus.EventName = "token.new_token_email_update"
	NEW_TOKEN_PASSWORD_UPDATE bus.EventName = "token.new_token_password_update"

	UPDATE_TOKEN_STATUS bus.EventName = "token.update_token_status"

	GET_TOKEN_EMAIL_UPDATE    bus.EventName = "users.get_token_email_update"
	GET_TOKEN_PASSWORD_UPDATE bus.EventName = "users.get_token_password_update"

	SEND_EMAIL_PASSWORD_RESET bus.EventName = "code.send_email_password_reset"
	SEND_EMAIL_RESET          bus.EventName = "code.send_email_reset"

	HANDLE_PAYMENTS_EVENTS         bus.EventName = "payment.event"
	REMOVE_BENEFITS_USER_FROM_PLAN bus.EventName = "payment.remove_benefits"
)

package services

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

var (
	NEW_TOKEN_EMAIL_UPDATE    bus.EventName = "token.new_token_email_update"
	NEW_TOKEN_PASSWORD_UPDATE bus.EventName = "token.new_token_password_update"
	UPDATE_TOKEN_STATUS       bus.EventName = "token.update_token_status"
	SEND_EMAIL_PASSWORD_RESET bus.EventName = "code.send_email_password_reset"
	SEND_EMAIL_RESET          bus.EventName = "code.send_email_reset"
)

package service

import "github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"

var (
	GET_TOKEN_EMAIL_UPDATE    bus.EventName = "users.get_token_email_update"
	GET_TOKEN_PASSWORD_UPDATE bus.EventName = "users.get_token_password_update"
	UPDATE_TOKEN_STATUS       bus.EventName = "token.update_token_status"
)

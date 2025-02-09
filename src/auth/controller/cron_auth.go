package controller

type CronAuthController struct{}

func (*CronAuthController) DeleteTokens() {
	sessionService.DeleteRevokedTokens()
}

func (*CronAuthController) DeleteExpiredSessions() {
	sessionService.DeleteExpiredSessions()
}

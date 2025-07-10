package email_repository

type SMTPParams struct {
	Senders []string
	Body    string
}

type SMTPRepository interface {
	SendCodeEmail(smtp SMTPParams) error
}

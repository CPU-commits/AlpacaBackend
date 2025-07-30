package service

import "errors"

var (
	ErrPublicationNotExists           = errors.New("err: publication not exists")
	ErrPublicationNotAccess           = errors.New("err: no access no publication")
	ErrTooManyImages                  = errors.New("err: too many images")
	ErrUnauthorizedPublishPublication = errors.New("err: no authorized publish")
	ErrInvalidIdentifier              = errors.New("err: invalid identifier")
	ErrTemporalViewExists             = errors.New("err: temporal view exists")
)

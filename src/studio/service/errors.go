package service

import "errors"

var (
	ErrMaxStudios            = errors.New("err: max studios created")
	ErrExistsEmailOrUsername = errors.New("err: exists email or username")
	ErrUserIsNotAdmin        = errors.New("err: user is not studio admin")
	ErrTheUserIsNotAdmin     = errors.New("err: the user is not studio admin")
	ErrNoHasPermission       = errors.New("err: no permission")
	ErrNoExistStudio         = errors.New("err: no exists studio")
	ErrStudioIsNotActive     = errors.New("err: studio is not active")
	ErrStudioNotExists       = errors.New("err: studio not exists")
	ErrCantCreateOwner       = errors.New("err: cant create owner")
	ErrUserNotInStudio       = errors.New("err: user not in studio")
)

package service

import "errors"

var ErrUserLoginNotFound = errors.New("err: user login not found")
var ErrInvalidCredentials = errors.New("err: invalid credentials")
var ErrSessionNotExists = errors.New("err: session not exists")
var ErrUserNotFound = errors.New("err: user not found")
var ErrTokenRevokedOrNotExists = errors.New("err: token revoked")
var ErrExistsEmailOrUsername = errors.New("err: exists email or username")
var ErrUsernameNotExists = errors.New("err: username not exists")
var ErrNotValidToken = errors.New("err: not valid token")
var ErrNewPasswordMustBeDifferent = errors.New("err: the new password must be different from the current one")
var ErrInvalidParams = errors.New("err: invalid params")
var ErrExistsEmail = errors.New("err: exists email")

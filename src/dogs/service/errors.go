package service

import "errors"

var ErrDogNotExists error = errors.New("dog does not exists")
var ErrOwnerDogHasOneDog error = errors.New("owner has dog")

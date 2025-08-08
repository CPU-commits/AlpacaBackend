package service

import "errors"

var (
	ErrSubscriptionNotExistsYet      = errors.New("err: subscription not exists yet")
	ErrSubscriptionIsCancelled       = errors.New("err: subscription is cancelled")
	ErrPlanNotExists                 = errors.New("err: plan not exists")
	ErrHasSubscriptionActiveWithPlan = errors.New("err: has subscription active with plan")
	ErrPlanNeedStudio                = errors.New("err: plan need studio")
)

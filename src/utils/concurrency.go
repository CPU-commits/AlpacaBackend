package utils

import (
	"context"
	"errors"
	"sync"

	"golang.org/x/sync/semaphore"
)

type key int

type Context struct {
	Ctx    *context.Context
	Cancel context.CancelFunc
	Key    key
}

func setContextAndCancel(err error, ctx *Context) {
	*ctx.Ctx = context.WithValue(*ctx.Ctx, ctx.Key, err)
	ctx.Cancel()
}

func Concurrency(
	semWight int64,
	count int,
	do func(index int, setError func(err error)),
) error {
	// Check if exists all users
	var wg sync.WaitGroup
	sem := semaphore.NewWeighted(semWight)
	// Ctx with cancel if error
	ctx, cancel := context.WithCancel(context.Background())
	// Ctx error
	const keyPrincipalID key = iota
	ctx = context.WithValue(ctx, keyPrincipalID, nil)

	wg.Add(count)
	for i := 0; i < count; i++ {
		if err := sem.Acquire(ctx, 1); err != nil {
			wg.Done()
			// Close go routines
			cancel()
			if errors.Is(err, context.Canceled) {
				if errRes := ctx.Value(keyPrincipalID); errRes != nil {
					return errRes.(error)
				}
			}
			return err
		}
		// Wrapper
		go func(wg *sync.WaitGroup, index int) {
			defer wg.Done()

			context := &Context{
				Ctx:    &ctx,
				Cancel: cancel,
				Key:    keyPrincipalID,
			}
			do(index, func(err error) {
				setContextAndCancel(err, context)
			})
			// Free semaphore
			sem.Release(1)
		}(&wg, i)
	}
	// Close all
	wg.Wait()
	cancel()
	// Catch error
	if err := ctx.Value(keyPrincipalID); err != nil {
		return err.(error)
	}
	return nil
}

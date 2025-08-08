package utils

import (
	"context"
	"errors"
	"reflect"
	"sync"
)

func Some[T comparable](slice []T, validorFunc func(x T) bool) bool {
	for i := 0; i < len(slice); i++ {
		v := slice[i]

		if !validorFunc(v) {
			return false
		}
	}

	return true
}

func AnyMatch[T comparable](slice []T, condition func(x T) (bool, error)) (bool, error) {
	v := reflect.ValueOf(slice)
	if v.Kind() != reflect.Slice && v.Kind() != reflect.Array {
		return false, errors.New("first arg. is not a slice")
	}

	for i := 0; i < v.Len(); i++ {
		v, err := condition(v.Index(i).Interface().(T))
		if err != nil {
			return false, err
		}
		if v {
			return true, nil
		}
	}
	return false, nil
}

func AllMatch(slice interface{}, condition func(x interface{}) bool) (bool, error) {
	v := reflect.ValueOf(slice)
	if v.Kind() != reflect.Slice && v.Kind() != reflect.Array {
		return false, errors.New("first arg. is not a slice")
	}

	for i := 0; i < v.Len(); i++ {
		if !condition(v.Index(i).Interface()) {
			return false, nil
		}
	}
	return true, nil
}

func Filter[T any](
	slice []T,
	condition func(x T) (bool, error),
) ([]T, error) {
	var newSlice []T
	for _, v := range slice {
		assert, err := condition(v)
		if err != nil {
			return nil, err
		}

		if assert {
			newSlice = append(newSlice, v)
		}

	}
	return newSlice, nil
}

func FilterNoError[T any](
	slice []T,
	condition func(x T) bool,
) []T {
	var newSlice []T
	for _, v := range slice {
		if condition(v) {
			newSlice = append(newSlice, v)
		}
	}
	return newSlice
}

func ConcurrentFilter[T any](
	slide []T,
	condition func(x T) (bool, error),
) ([]T, error) {
	// Data
	var newSlide []T

	// Sync
	var locker sync.RWMutex
	var wg sync.WaitGroup
	// Handle
	var err error

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for i, v := range slide {
		wg.Add(1)

		go func(v T, i int, err *error, wg *sync.WaitGroup, locker *sync.RWMutex) {
			defer wg.Done()

			// Handle error
			select {
			case <-ctx.Done():
				return
			default:
				assert, errT := condition(v)
				if errT != nil {
					*err = errT
					cancel()
					return
				}
				if assert {
					locker.Lock()
					newSlide = append(newSlide, v)
					locker.Unlock()
				}
			}
		}(v, i, &err, &wg, &locker)
	}
	wg.Wait()
	if err != nil {
		return nil, err
	}

	return newSlide, nil
}

func Map[T any, R any](slide []T, transformer func(x T) (R, error)) ([]R, error) {
	var newSlide []R

	for _, v := range slide {
		newValue, err := transformer(v)
		if err != nil {
			return nil, err
		}

		newSlide = append(
			newSlide,
			newValue,
		)
	}
	return newSlide, nil
}

func MapNoError[T any, R any](slide []T, transformer func(x T) R) []R {
	var newSlide []R

	for _, v := range slide {
		newValue := transformer(v)

		newSlide = append(
			newSlide,
			newValue,
		)
	}
	return newSlide
}

func MapNoErrorIndex[T any, R any](slide []T, transformer func(x T, index int) R) []R {
	var newSlide []R

	i := 0
	for _, v := range slide {
		newValue := transformer(v, i)

		newSlide = append(
			newSlide,
			newValue,
		)
		i++
	}
	return newSlide
}

func Flat[T any](slide interface{}) []T {
	var result []T

	vSlide := reflect.ValueOf(slide)
	if vSlide.Kind() == reflect.Array || vSlide.Kind() == reflect.Slice {
		for i := 0; i < vSlide.Len(); i++ {
			single := vSlide.Index(i).Interface()

			switch v := single.(type) {
			case []T:
				result = append(result, Flat[T](v)...)
			case T:
				result = append(result, v)
			}
		}
	}
	return result
}

type OptionsConcurrentMap struct {
	Congruent      bool
	SemaphoreWight int64
}

func ConcurrentMap[T any, R any](
	slide []T,
	transformer func(v T) (R, error),
	options *OptionsConcurrentMap,
) ([]R, error) {
	return Map(slide, transformer)
}

func Reduce[T any, R any](
	slide []T,
	reducer func(acum R, v T) (R, error),
	initialValue R,
) (reduce R, err error) {
	reduce = initialValue

	for _, v := range slide {
		reduce, err = reducer(reduce, v)
		if err != nil {
			return
		}
	}
	return
}

func ReduceNoError[T any, R any](
	slide []T,
	reducer func(acum R, v T) R,
	initialValue R,
) (reduce R) {
	reduce = initialValue

	for _, v := range slide {
		reduce = reducer(reduce, v)
	}
	return
}

func Find[T any](slide []T, cond func(v T) (bool, error)) (findValue *T, err error) {
	for _, v := range slide {
		isValue, errCond := cond(v)
		if errCond != nil {
			err = errCond
			return
		}
		if isValue {
			return &v, err
		}
	}
	return
}

func FindNoError[T any](slide []T, cond func(v T) bool) (findValue *T) {
	for _, v := range slide {
		isValue := cond(v)
		if isValue {
			return &v
		}
	}
	return
}

type OptionsConcurrentForEach struct {
	MaxConcurrency int
}

func ConcurrentForEach[T any](
	slice []T,
	do func(v T, setError func(err error)),
	options *OptionsConcurrentForEach,
) error {
	return ForEach(slice, func(v T) error {
		var err error

		do(v, func(cerr error) {
			err = cerr
		})
		return err
	})

	/*
		maxConcurrency := 10
		if options != nil && options.MaxConcurrency != 0 {
			maxConcurrency = options.MaxConcurrency
		}

		var wg sync.WaitGroup
		sem := semaphore.NewWeighted(int64(maxConcurrency))
		// Ctx with cancel if error
		ctx, cancel := context.WithCancel(context.Background())
		// Ctx error
		const keyPrincipalID key = iota
		ctx = context.WithValue(ctx, keyPrincipalID, nil)
		count := len(slice)

		wg.Add(count)
		for _, v := range slice {
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
			go func(wg *sync.WaitGroup, v T) {
				defer wg.Done()

				context := &Context{
					Ctx:    &ctx,
					Cancel: cancel,
					Key:    keyPrincipalID,
				}
				do(v, func(err error) {
					setContextAndCancel(err, context)
				})
				// Free semaphore
				sem.Release(1)
			}(&wg, v)
		}
		// Close all
		wg.Wait()
		cancel()
		// Catch error
		if err := ctx.Value(keyPrincipalID); err != nil {
			return err.(error)
		}
		return nil*/
}

func ForEach[T any](slide []T, toDo func(v T) error) error {
	for _, v := range slide {
		err := toDo(v)
		if err != nil {
			return err
		}
	}
	return nil
}

func Includes[T any](slide []T, equalTo T) bool {
	for _, v := range slide {
		valueOfAny := reflect.ValueOf(equalTo)
		valueOfV := reflect.ValueOf(v)

		if valueOfAny.Type() == valueOfV.Type() && valueOfAny.Equal(valueOfV) {
			return true
		}
	}
	return false
}

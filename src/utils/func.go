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

func AnyMatch[T comparable](slice []T, condition func(x T) bool) (bool, error) {
	v := reflect.ValueOf(slice)
	if v.Kind() != reflect.Slice && v.Kind() != reflect.Array {
		return false, errors.New("first arg. is not a slice")
	}

	for i := 0; i < v.Len(); i++ {
		if condition(v.Index(i).Interface().(T)) {
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
	// Init options
	congruent := options != nil && options.Congruent
	var wight int64 = 10
	if options != nil && options.SemaphoreWight != 0 {
		wight = options.SemaphoreWight
	}
	// Data
	var newSlide []R

	if congruent {
		newSlide = make([]R, len(slide))
	}
	// Sync
	var locker sync.RWMutex
	var wg sync.WaitGroup
	// Handle
	var err error
	// Buffered
	c := make(chan struct{}, wight)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for i, v := range slide {
		wg.Add(1)

		go func(v T, i int, err *error, wg *sync.WaitGroup, locker *sync.RWMutex, c chan struct{}) {
			defer wg.Done()
			c <- struct{}{}

			// Handle error
			select {
			case <-ctx.Done():
				return
			default:
				newValue, errT := transformer(v)
				if errT != nil {
					*err = errT
					cancel()
					return
				}
				if congruent {
					newSlide[i] = newValue
				} else {
					locker.Lock()
					newSlide = append(newSlide, newValue)
					locker.Unlock()
				}
				<-c
			}
		}(v, i, &err, &wg, &locker, c)
	}
	wg.Wait()
	if err != nil {
		return nil, err
	}

	return newSlide, nil
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

func ConcurrentForEach[T any](slide []T, toDo func(v T) error) error {
	// Sync
	var wg sync.WaitGroup
	// Handle
	var err error

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	for i, v := range slide {
		wg.Add(1)

		go func(v T, i int, err *error, wg *sync.WaitGroup) {
			defer wg.Done()

			// Handle error
			select {
			case <-ctx.Done():
				return
			default:
				errRet := toDo(v)
				if errRet != nil {
					*err = errRet
				}
			}
		}(v, i, &err, &wg)
	}
	wg.Wait()
	return err
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

func Includes[T any](slide []T, equalTo interface{}) bool {
	for _, v := range slide {
		valueOfAny := reflect.ValueOf(equalTo)
		valueOfV := reflect.ValueOf(v)

		if valueOfAny.Type() == valueOfV.Type() && valueOfAny.Equal(valueOfV) {
			return true
		}
	}
	return false
}

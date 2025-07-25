package utils

import (
	"crypto/rand"
	"encoding/base32"
	"encoding/json"
	"errors"
	"reflect"
	"strings"
	"time"
)

func ToPayload(data interface{}) []byte {
	payload, _ := json.Marshal(data)

	return payload
}

func ExtractWords[T comparable](text string, delimiter string) []T {
	words := strings.Fields(text)
	var result []T
	for _, word := range words {
		if strings.Contains(word, delimiter) {
			result = append(result, reflect.ValueOf(word[len(delimiter):]).Interface().(T))
		}
	}
	return result
}

func DaysSinceCreation(createdAt time.Time) (int, error) {

	createdDate := time.Date(createdAt.Year(), createdAt.Month(), createdAt.Day(), 0, 0, 0, 0, time.UTC)
	today := time.Now().UTC().Truncate(24 * time.Hour)

	days := int(today.Sub(createdDate).Hours() / 24)

	return days, nil
}

func VerifyNotExpiredAt(expiration time.Time, clockType string, err error) error {
	var now time.Time

	switch clockType {
	case "utc":
		now = time.Now().UTC()
	case "local":
		now = time.Now()
	default:
		return errors.New("invalid clock type: must be 'utc' or 'local'")
	}

	if now.After(expiration) {
		return err
	}

	return nil
}

func GenerateRandomString(length int) (string, error) {
	b := make([]byte, length)
	_, err := rand.Read(b)
	if err != nil {
		return "", err
	}

	code := strings.ToUpper(base32.StdEncoding.WithPadding(base32.NoPadding).EncodeToString(b))
	return code[:length], nil
}

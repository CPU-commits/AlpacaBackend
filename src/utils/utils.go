package utils

import (
	"bufio"
	"crypto/rand"
	"encoding/base32"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
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

func IterateDates[T any](from time.Time, to time.Time, fun func(d time.Time) T) (result []T) {
	for d := from; !d.After(to); d = d.AddDate(0, 0, 1) {
		r := fun(d)
		result = append(result, r)
	}

	return result
}

func ReaderImageToBase64(r io.Reader) (string, error) {
	br := bufio.NewReader(r)

	data, err := io.ReadAll(br)
	if err != nil {
		return "", err
	}
	b64 := base64.StdEncoding.EncodeToString(data)
	return b64, nil
}

func ReaderImageToBase64DataURI(r io.Reader) (string, error) {
	br := bufio.NewReader(r)

	head, _ := br.Peek(512)
	mime := http.DetectContentType(head)

	data, err := io.ReadAll(br)
	if err != nil {
		return "", err
	}
	b64 := base64.StdEncoding.EncodeToString(data)
	return fmt.Sprintf("data:%s;base64,%s", mime, b64), nil
}

func CleanAndLower(s string) string {
	cleaned := strings.TrimSpace(s)
	return strings.ToLower(cleaned)
}

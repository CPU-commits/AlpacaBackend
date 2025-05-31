package utils

import (
	"encoding/json"
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

	// Truncar ambas fechas al día para contar días calendario
	createdDate := time.Date(createdAt.Year(), createdAt.Month(), createdAt.Day(), 0, 0, 0, 0, time.UTC)
	today := time.Now().UTC().Truncate(24 * time.Hour)

	days := int(today.Sub(createdDate).Hours() / 24)

	return days, nil
}

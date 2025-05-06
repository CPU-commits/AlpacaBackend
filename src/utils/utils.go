package utils

import (
	"encoding/json"
	"reflect"
	"strings"
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

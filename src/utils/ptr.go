package utils

import (
	"encoding/json"
)

func Float64(number float64) *float64 {
	return &number
}

func Bool(v bool) *bool {
	return &v
}
func Int(number int) *int {
	return &number
}

func Int64(number int64) *int64 {
	return &number
}

func String(str string) *string {
	return &str
}

func Payload(data interface{}) []byte {
	bytes, _ := json.Marshal(data)

	return bytes
}

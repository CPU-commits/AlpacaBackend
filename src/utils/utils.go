package utils

import "encoding/json"

func ToPayload(data interface{}) []byte {
	payload, _ := json.Marshal(data)

	return payload
}

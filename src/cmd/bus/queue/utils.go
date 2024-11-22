package queue

import (
	"encoding/json"
)

type natsUtils struct{}

// Nats Golang
type NatsGolangReq struct {
	Pattern string      `json:"pattern"`
	Data    interface{} `json:"data"`
}

// Default Nats response
type DefaultNatsResponse[T any] struct {
	Success bool   `json:"success"`
	Message string `json:"message,omitempty"`
	Data    T      `json:"data"`
	Status  int    `json:"status,omitempty"`
}

func (utils *natsUtils) BindJSON(data []byte, structData interface{}) error {
	var dataNest NatsGolangReq

	err := json.Unmarshal(data, &dataNest)
	if err != nil || (dataNest.Data == nil && dataNest.Pattern == "") {
		if err := json.Unmarshal(data, structData); err != nil {
			return err
		}
		return nil
	}
	// Marshal
	payload, err := json.Marshal(dataNest.Data)
	if err != nil {
		return err
	}
	if err := json.Unmarshal(payload, &structData); err != nil {
		return err
	}
	return nil
}

func (*natsUtils) NewError(message string, statusCode int) *DefaultNatsResponse[interface{}] {
	return &DefaultNatsResponse[interface{}]{
		Success: false,
		Message: message,
		Data:    nil,
		Status:  statusCode,
	}
}

func (utils *natsUtils) Respond(data interface{}) *DefaultNatsResponse[interface{}] {
	return &DefaultNatsResponse[interface{}]{
		Success: true,
		Data:    data,
	}
}

func (utils *natsUtils) NoRespond() *DefaultNatsResponse[interface{}] {
	return nil
}

func NewNatsUtils() *natsUtils {
	return &natsUtils{}
}

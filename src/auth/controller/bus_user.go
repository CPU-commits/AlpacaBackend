package controller

import (
	"encoding/json"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

type BusUserController struct {
	Bus bus.Bus
}

func (busController *BusUserController) CreateUserPassword(c bus.Context) error {
	var userCreatedEvent dto.UserCreatedEvent
	if err := c.BindData(&userCreatedEvent); err != nil {
		return c.Kill(err.Error())
	}
	token, err := tokenPasswordService.NewTokenPassword(userCreatedEvent.IDUser)
	if err != nil {
		return c.FollowUp(time.Second * 5)
	}

	type Payload struct {
		Name  string `json:"name"`
		Email string `json:"email"`
		Token string `json:"token"`
	}
	payload := Payload{
		Name:  userCreatedEvent.Name,
		Email: userCreatedEvent.Email,
		Token: token,
	}
	jsonData, err := json.Marshal(payload)
	if err != nil {
		return err
	}

	busController.Bus.Publish(bus.Event{
		Name:    INSERTED_USER,
		Payload: jsonData,
	})
	return nil
}

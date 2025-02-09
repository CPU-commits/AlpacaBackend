package controller

import (
	"errors"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
)

type BusAuthController struct{}

func (*BusAuthController) ValidateToken(c bus.Context) (*bus.BusResponse, error) {
	tokenJwt := string(c.Data)
	if tokenJwt == "" {
		return nil, c.Kill("empty token jwt")
	}
	partsJwt := strings.Split(tokenJwt, "Bearer ")
	if len(partsJwt) != 2 {
		return nil, c.Kill("not bearer token")
	}
	// Check token
	err := sessionService.CheckToken(partsJwt[1])
	if err != nil {
		if errors.Is(err, service.ErrTokenRevokedOrNotExists) {
			return &bus.BusResponse{
				Success: true,
				Data: map[string]interface{}{
					"hasAccess": false,
				},
			}, nil
		}
		return nil, err
	}

	return &bus.BusResponse{
		Success: true,
		Data: map[string]interface{}{
			"hasAccess": true,
		},
	}, nil
}

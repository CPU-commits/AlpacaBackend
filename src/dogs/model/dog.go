package model

type Dog struct {
	ID        int    `json:"id"`
	Name      string `json:"name"`
	OwnerName string `json:"owner"`
}

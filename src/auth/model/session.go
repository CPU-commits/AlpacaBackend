package model

import "time"

type Session struct {
	ID        int64
	Token     string
	Device    string
	IP        string
	Browser   string
	Location  string
	IDAuth    int64
	ExpiresAt time.Time
	CreatedAt time.Time
}

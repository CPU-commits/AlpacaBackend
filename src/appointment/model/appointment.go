package model

import (
	"time"

	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	studioModel "github.com/CPU-commits/Template_Go-EventDriven/src/studio/model"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	userModel "github.com/CPU-commits/Template_Go-EventDriven/src/user/model"
)

// Status
type AppointmentStatus string

const (
	STATUS_CREATED   AppointmentStatus = "created"
	STATUS_CANCELED  AppointmentStatus = "canceled"
	STATUS_SCHEDULED AppointmentStatus = "scheduled"
	STATUS_REVIEWED  AppointmentStatus = "reviewed"
)

// Area
type AppointmentArea string

const (
	ARM        AppointmentArea = "arm"
	LEG        AppointmentArea = "leg"
	BACK       AppointmentArea = "back"
	CHEST      AppointmentArea = "chest"
	ABDOMEN    AppointmentArea = "abdomen"
	NECK       AppointmentArea = "neck"
	HEAD       AppointmentArea = "head"
	HAND       AppointmentArea = "hand"
	FOOT       AppointmentArea = "foot"
	HIP        AppointmentArea = "hip"
	OTHER_AREA AppointmentArea = "other"
)

func IsAppointmentArea(area string) bool {
	return area == string(ARM) || area == string(LEG) || area == string(BACK) || area == string(CHEST) || area == string(ABDOMEN) || area == string(NECK) || area == string(HEAD) || area == string(HAND) || area == string(FOOT) || area == string(HIP) || area == string(OTHER_AREA)
}

// Color
type AppointmentColor string

const (
	BLACK      AppointmentColor = "black"
	FULL_COLOR AppointmentColor = "full_color"
)

func IsAppointmentColor(color string) bool {
	return color == string(BLACK) || color == string(FULL_COLOR)
}

type Appointment struct {
	ID                  int64               `json:"id"`
	IDStudio            int64               `json:"idStudio,omitempty"`
	IDTattooArtist      int64               `json:"idTattooArtist"`
	TattooArtist        *authModel.User     `json:"tattooArtist,omitempty"`
	TattooArtistProfile *userModel.Profile  `json:"tattooArtistProfile,omitempty"`
	IDUser              int64               `json:"idUser"`
	IDCalendar          string              `json:"idCalendar,omitempty"`
	UserProfile         *userModel.Profile  `json:"userProfile,omitempty"`
	User                *authModel.User     `json:"user,omitempty"`
	Status              AppointmentStatus   `json:"status"`
	Phone               string              `json:"phone,omitempty"`
	HasIdea             *bool               `json:"hasIdea,omitempty"`
	HasDesign           *bool               `json:"hasDesign,omitempty"`
	IDDesign            *int64              `json:"idDesign,omitempty"`
	Design              tattooModel.Design  `json:"design,omitempty"`
	Review              *Review             `json:"review,omitempty"`
	Area                AppointmentArea     `json:"area,omitempty"`
	Height              float32             `json:"height,omitempty"`
	Width               float32             `json:"width,omitempty"`
	Color               AppointmentColor    `json:"color,omitempty"`
	Description         string              `json:"description"`
	Duration            float32             `json:"duration,omitempty"`
	ScheduledAt         *time.Time          `json:"scheduledAt,omitempty"`
	FinishedAt          *time.Time          `json:"finishedAt,omitempty"`
	Images              []model.Image       `json:"images,omitempty"`
	Studio              *studioModel.Studio `json:"studio,omitempty"`
	CreatedAt           time.Time           `json:"createdAt"`
}

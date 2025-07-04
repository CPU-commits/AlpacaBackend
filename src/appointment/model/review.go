package model

type Review struct {
	ID            int64  `json:"id"`
	IDUser        int64  `json:"idUsder,omitempty"`
	IDProfile     int64  `json:"idProfile,omitempty"`
	IDAppointment int64  `json:"idAppointment,omitempty"`
	Stars         int16  `json:"stars"`
	Review        string `json:"review"`
}

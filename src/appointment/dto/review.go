package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"

type ReviewDTO struct {
	Review string `json:"review" binding:"required,max=250"`
	Stars  int16  `json:"stars" binding:"required,min=0,max=5"`
}

func (r ReviewDTO) ToModel(idUser, idProfile, idAppointment int64) model.Review {
	return model.Review{
		Review:        r.Review,
		Stars:         r.Stars,
		IDUser:        idUser,
		IDProfile:     idProfile,
		IDAppointment: idAppointment,
	}
}

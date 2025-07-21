package dto

import "github.com/CPU-commits/Template_Go-EventDriven/src/follow/model"

type FollowDto struct {
	IDStudio int64  `json:"idStudio"`
	Username string `json:"username"`
}

func (f *FollowDto) ToModel(idUser int64) (*model.Follow, error) {
	if f.IDStudio == 0 && f.Username == "" {
		return nil, ErrNeedProfileOrStudio
	}

	return &model.Follow{
		IDUser:   idUser,
		IDStudio: f.IDStudio,
	}, nil
}

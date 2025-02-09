package like_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/publication/model"

type Criteria struct {
	IDProfile int64
	IDPost    int64
	IDUser    int64
}

type LikeRepository interface {
	Insert(like model.Like) error
	Exists(criteria *Criteria) (bool, error)
	Delete(criteria *Criteria) error
}

package follow_repository

type FollowCriteria struct {
	IDUser    int64
	IDProfile int64
}

type FollowRepository interface {
	// InsertOne(model model.Follow) (*model.Follow, error)
	CountProfileFollowers(criteria *FollowCriteria) (int64, error)
}

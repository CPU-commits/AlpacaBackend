package temporal_view_repository

type TemporalViewRepository interface {
	AddView(key string, identifier string) error
	ExistsView(key string, identifier string) (bool, error)
}

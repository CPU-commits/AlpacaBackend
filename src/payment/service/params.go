package service

type PaymentsParams struct {
	IDSubscription int64
	Page           int
}

type ToSubscription struct {
	IDUser   int64
	IDStudio int64
}

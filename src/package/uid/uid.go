package uid

type UIDGenerator interface {
	Generate() (string, error)
}

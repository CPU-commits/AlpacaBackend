package nanoid

import (
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/uid"
	gonanoid "github.com/matoous/go-nanoid/v2"
)

type nanoIdGenerator struct{}

func (n nanoIdGenerator) Generate() (string, error) {
	return gonanoid.New()
}

func NewNanoIDGenerator() uid.UIDGenerator {
	return nanoIdGenerator{}
}

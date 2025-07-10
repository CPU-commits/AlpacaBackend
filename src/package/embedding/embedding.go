package embedding

import "io"

type Embedding interface {
	EmbedImage(image io.Reader) ([]float64, error)
}

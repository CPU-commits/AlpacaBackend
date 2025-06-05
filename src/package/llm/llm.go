package llm

type PredictSchema struct {
	Name        string
	Description string
}

type Message struct {
	String   string
	ImageURL string
}

type Predict[T any] func(
	schema PredictSchema,
	llmMessage string,
	message Message,
) (string, error)

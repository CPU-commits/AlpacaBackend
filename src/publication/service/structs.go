package service

const AssistanteMessageTattoo = `
	Task: You are an image analysis assistant. Your job is to analyze a given image and determine whether it depicts a tattoo or not.

	Definition:
	- A tattoo is defined as permanent body art applied to human skin using ink.
	- If the image shows a clear, visible tattoo on human skin, label it as isTattoo: true.
	- If the image shows no tattoo, or it's ambiguous, label it as isTattoo: false.

	Be cautious of:
	- Drawings or illustrations of tattoos not on human skin — these should be false.
	- Temporary tattoos (stickers, henna, etc.) — these should also be false.
	- If it's extremely ambiguous, err on the side of false.
`

type TattooPredict struct {
	IsTattoo bool `json:"isTattoo" jsonschema_description:"Is tattoo or not"`
}

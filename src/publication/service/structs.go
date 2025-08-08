package service

const AssistanteMessageTattoo = `
	Task: You are an image analysis assistant. Your job is to analyze a given image and determine whether it depicts a tattoo or not, if it's, returns tattoo Area.

	Definition:
	- A tattoo is defined as permanent body art applied to human skin using ink.
	- If the image shows a clear, visible tattoo on human skin, label it as isTattoo: true.
	- If the image shows no tattoo, or it's ambiguous, label it as isTattoo: false.

	tattooArea: array of one or more of ["arm","leg","back","chest","abdomen","neck","head","hand","foot","hip","other"]

	description: 20-240 chars in spanish, concise, neutral; mention motifs, style (e.g., linework, realism, tribal), colors/blackwork, and size; avoid guesses about identity or text transcription unless clearly legible.

	color: of one ["black", "full_color"]

	Be cautious of:
	- Drawings or illustrations of tattoos not on human skin — these should be false.
	- Temporary tattoos (stickers, henna, etc.) — these should also be false.
	- If it's extremely ambiguous, err on the side of false.
`

type TattooPredict struct {
	IsTattoo    bool     `json:"isTattoo" jsonschema:"description=Is tattoo or not,required"`
	Area        []string `json:"tattooArea" jsonschema:"description=List of tattoo body areas,type=array,items.type=string,items.enum=arm,items.enum=leg,items.enum=back,items.enum=chest,items.enum=abdomen,items.enum=neck,items.enum=head,items.enum=hand,items.enum=foot,items.enum=hip,items.enum=other,required"`
	Description string   `json:"description" jsonschema:"description=Short neutral description of the tattoo,language-agnostic in spanish,minLength=20,maxLength=240"`
	Color       string   `json:"color" jsonschema:"description=Tattoo Color,enum=full_color,enum=black"`
}

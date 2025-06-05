package openaiprovider

import (
	"context"
	"encoding/json"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/llm"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/invopop/jsonschema"
	"github.com/openai/openai-go"
	"github.com/openai/openai-go/option"
)

// Settings
var settingsData = settings.GetSettings()

// Client
var client openai.Client

func GenerateSchema[T any]() any {
	reflector := jsonschema.Reflector{
		AllowAdditionalProperties: false,
		DoNotReference:            true,
	}
	var v T
	schema := reflector.Reflect(v)
	return schema
}

func Predict[T any](
	schema llm.PredictSchema,
	llmMessage string,
	message llm.Message,
) (dataJson T, content string, err error) {
	var Schema = GenerateSchema[T]()

	schemaParam := openai.ResponseFormatJSONSchemaJSONSchemaParam{
		Name:        schema.Name,
		Description: openai.String(schema.Description),
		Strict:      openai.Bool(true),
		Schema:      Schema,
	}
	messageToOpenaiMessage := func() openai.ChatCompletionMessageParamUnion {
		if message.String != "" && message.ImageURL == "" {
			return openai.UserMessage(message.String)
		}

		return openai.ChatCompletionMessageParamUnion{
			OfUser: &openai.ChatCompletionUserMessageParam{
				Content: openai.ChatCompletionUserMessageParamContentUnion{
					OfArrayOfContentParts: []openai.ChatCompletionContentPartUnionParam{{
						OfImageURL: &openai.ChatCompletionContentPartImageParam{
							ImageURL: openai.ChatCompletionContentPartImageImageURLParam{
								URL:    message.ImageURL,
								Detail: "low",
							},
						},
					}},
				},
			},
		}
	}

	chatCompletion, err := client.Chat.Completions.New(
		context.Background(),
		openai.ChatCompletionNewParams{
			Messages: []openai.ChatCompletionMessageParamUnion{
				openai.SystemMessage(llmMessage),
				messageToOpenaiMessage(),
			},
			Model: openai.ChatModelO4Mini,
			ResponseFormat: openai.ChatCompletionNewParamsResponseFormatUnion{
				OfJSONSchema: &openai.ResponseFormatJSONSchemaParam{
					JSONSchema: schemaParam,
				},
			},
		},
	)
	if err != nil {
		return
	}
	content = chatCompletion.Choices[0].Message.Content
	// Extract JSON
	_ = json.Unmarshal([]byte(chatCompletion.Choices[0].Message.Content), &dataJson)

	return
}

func init() {
	client = openai.NewClient(
		option.WithAPIKey(settingsData.OPENAI_KEY),
	)
}

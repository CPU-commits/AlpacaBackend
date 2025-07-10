package embeddingapi

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"mime/multipart"
	"net/http"

	"github.com/CPU-commits/Template_Go-EventDriven/src/package/embedding"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
)

var settingsData = settings.GetSettings()

type APIEmbedding struct {
	apiToken string
	url      string
}

type APIResponse struct {
	Embedding []float64 `json:"embedding"`
}

func (api APIEmbedding) EmbedImage(image io.Reader) ([]float64, error) {
	body := &bytes.Buffer{}
	writter := multipart.NewWriter(body)
	part, err := writter.CreateFormFile("file", "image.jpg")
	if err != nil {
		return nil, err
	}
	_, err = io.Copy(part, image)
	if err != nil {
		return nil, err
	}
	err = writter.Close()
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequest("POST", api.url, body)
	if err != nil {
		panic(err)
	}

	// Headers
	// req.Header.Set("Authorization", "Bearer "+re.apiToken)
	req.Header.Set("Content-Type", writter.FormDataContentType())

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	resBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		return nil, errors.New("err create")
	}
	// Response
	var response APIResponse
	err = json.Unmarshal(resBody, &response)
	if err != nil {
		return nil, err
	}

	return response.Embedding, nil
}

func StreamToByte(stream io.Reader) []byte {
	buf := new(bytes.Buffer)
	buf.ReadFrom(stream)
	return buf.Bytes()
}

func NewAPIEmbedding() embedding.Embedding {
	return APIEmbedding{
		url: "https://felipecardenas301--test-alpaca-embed-image-dev.modal.run",
	}
}

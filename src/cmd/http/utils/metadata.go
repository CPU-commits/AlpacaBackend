package utils

import (
	"encoding/json"
	"errors"
	"mime/multipart"
	"strconv"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/dto"
	"github.com/rwcarlsen/goexif/exif"
)

func GetCoordFile(file *multipart.FileHeader) (*dto.CoordDto, error) {
	// Get metadata
	var coord *dto.CoordDto = &dto.CoordDto{}
	openedForMetadata, err := file.Open()
	if err != nil {
		return nil, err
	}

	x, err := exif.Decode(openedForMetadata)
	if err == nil {
		parseFraction := func(frac string) (float64, error) {
			numDen := strings.Split(frac, "/")
			if len(numDen) != 2 {
				return 0, errors.New("err")
			}
			numerator, err := strconv.ParseFloat(numDen[0], 64)
			if err != nil {
				return 0, errors.New("err")
			}
			denominator, err := strconv.ParseFloat(numDen[1], 64)
			if err != nil {
				return 0, errors.New("err")
			}

			return numerator / denominator, nil
		}
		getCoord := func(coord string) float64 {
			var values []string

			if err := json.Unmarshal([]byte(coord), &values); err == nil {
				if len(values) != 3 {
					return 0
				}
				degrees, err := parseFraction(values[0])
				if err != nil {
					return 0
				}
				minutes, err := parseFraction(values[1])
				if err != nil {
					return 0
				}
				seconds, err := parseFraction(values[2])
				if err != nil {
					return 0
				}

				decimal := degrees + (minutes / 60) + (seconds / 3600)
				return decimal
			}
			return 0
		}

		latitude, err := x.Get(exif.GPSLatitude)
		if err == nil {
			coordY := getCoord(latitude.String())
			if coordY != 0 {
				coord.Y = coordY
			}
		}
		longitude, err := x.Get(exif.GPSLongitude)
		if err == nil {
			coordX := getCoord(longitude.String())
			if coordX != 0 {
				coord.X = coordX
			}
		}
	}
	if coord.X != 0 && coord.Y != 0 {
		return coord, nil
	}

	return nil, errors.New("no coord")
}

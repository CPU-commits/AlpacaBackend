package jwt

import "github.com/CPU-commits/Template_Go-EventDriven/src/settings"

var JwtKey string

var settingsData = settings.GetSettings()

func init() {
	JwtKey = settingsData.JWT_SECRET_KEY
}

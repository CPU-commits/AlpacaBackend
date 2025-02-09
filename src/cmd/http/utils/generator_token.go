package utils

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/service"
	"github.com/golang-jwt/jwt"
)

type tokenGenerateJWT struct{}

func (tokenGenerateJWT) NewFirstTimeToken(IDUser int64) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat":   time.Now().Unix(),
		"exp":   "",
		"_id":   IDUser,
		"roles": []string{""},
		"name":  "",
	})
	tokenString, err := token.SignedString(jwtKeyByte)
	if err != nil {
		return "", err
	}
	return tokenString, err
}

func (tokenGenerateJWT) NewSessionToken(
	expiredAt time.Time,
	idUser int64,
) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat": time.Now().Unix(),
		"exp": expiredAt.Unix(),
		"uid": idUser,
		"sub": "refresh",
	})

	tokenString, err := token.SignedString(jwtKeyByte)
	if err != nil {
		return "", err
	}

	return tokenString, err
}

func (tokenGenerateJWT) NewAccessToken(
	expiredAt time.Time,
	user model.User,
) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat":   time.Now().Unix(),
		"exp":   expiredAt.Unix(),
		"uid":   user.ID,
		"roles": user.Roles,
		"name":  user.Name,
	})
	tokenString, err := token.SignedString(jwtKeyByte)
	if err != nil {
		return "", err
	}

	return tokenString, nil
}

func NewGeneratorToken() service.TokenGenerator {
	return tokenGenerateJWT{}
}

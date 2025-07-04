package token_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/golang-jwt/jwt"
)

type tokenGenerateJWT struct {
	jwtKey string
}

func (tokenGenerateJWT tokenGenerateJWT) NewFirstTimeToken(IDUser int64) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat":   time.Now().Unix(),
		"exp":   "",
		"_id":   IDUser,
		"roles": []string{""},
		"name":  "",
	})
	tokenString, err := token.SignedString([]byte(tokenGenerateJWT.jwtKey))
	if err != nil {
		return "", err
	}
	return tokenString, err
}

func (tokenGenerateJWT tokenGenerateJWT) NewSessionToken(
	expiredAt time.Time,
	idUser int64,
) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat": time.Now().Unix(),
		"exp": expiredAt.Unix(),
		"uid": idUser,
		"sub": "refresh",
	})

	tokenString, err := token.SignedString([]byte(tokenGenerateJWT.jwtKey))
	if err != nil {
		return "", err
	}

	return tokenString, err
}

func (tokenGenerateJWT tokenGenerateJWT) NewAccessToken(
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
	tokenString, err := token.SignedString([]byte(tokenGenerateJWT.jwtKey))
	if err != nil {
		return "", err
	}

	return tokenString, nil
}

func (tokenGenerateJWT tokenGenerateJWT) NewRecoveryToken(
	expiredAt time.Time,
	user model.User,
) (string, error) {
	token := jwt.NewWithClaims(jwt.SigningMethodHS256, jwt.MapClaims{
		"iat":   time.Now().Unix(),
		"exp":   expiredAt.Unix(),
		"uid":   user.ID,
		"email": user.Email,
	})
	tokenString, err := token.SignedString([]byte(tokenGenerateJWT.jwtKey))
	if err != nil {
		return "", err
	}

	return tokenString, nil
}

func NewGeneratorToken(jwtKey string) TokenGenerator {
	return &tokenGenerateJWT{
		jwtKey: jwtKey,
	}
}

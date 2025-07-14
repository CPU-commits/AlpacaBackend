package utils

import (
	"errors"
	"fmt"
	"strings"

	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	"github.com/gin-gonic/gin"
	jwt "github.com/golang-jwt/jwt/v4"
)

var jwtKey = settings.GetSettings().JWT_SECRET_KEY

type Claims struct {
	ID    int64
	Roles []model.Role
	Name  string
}

type RefreshClaims struct {
	Exp float64
	Iat float64
	Sub string
	UID float64
}

func extractToken(bearerToken string) string {
	strArr := strings.Split(bearerToken, " ")
	if len(strArr) == 2 {
		return strArr[1]
	}
	return ""
}

func VerifyToken(bearerToken string) (*jwt.Token, error) {
	tokenString := extractToken(bearerToken)
	token, err := jwt.Parse(tokenString, func(t *jwt.Token) (interface{}, error) {
		if _, ok := t.Method.(*jwt.SigningMethodHMAC); !ok {
			return nil, fmt.Errorf("unexpected signing method: %v", t.Header["alg"])
		}
		return []byte(jwtKey), nil
	})
	if err != nil {
		return nil, err
	}
	if !token.Valid {
		return nil, errors.New("Unauthorized")
	}
	return token, nil
}

func ExtractTokenMetadata(token *jwt.Token) (*Claims, error) {
	claim := token.Claims.(jwt.MapClaims)
	var rolesIn []model.Role
	if roles, ok := claim["roles"].([]interface{}); ok {
		for _, role := range roles {
			if roleStr, ok := role.(string); ok {
				rolesIn = append(rolesIn, model.Role(roleStr))
			}
		}
	}
	return &Claims{
		ID:    int64(claim["uid"].(float64)),
		Roles: rolesIn,
		Name:  fmt.Sprintf("%v", claim["name"]),
	}, nil
}

func ExtractRefreshTokeMetadata(token *jwt.Token) (*RefreshClaims, error) {
	claim := token.Claims.(jwt.MapClaims)

	return &RefreshClaims{
		Iat: claim["iat"].(float64),
		Exp: claim["exp"].(float64),
		Sub: claim["sub"].(string),
		UID: claim["uid"].(float64),
	}, nil
}

func NewClaimsFromContext(ctx *gin.Context) (*Claims, bool) {
	user, exists := ctx.Get("user")
	if !exists {
		return &Claims{}, false
	}
	return &Claims{
		ID:    user.(*Claims).ID,
		Roles: user.(*Claims).Roles,
		Name:  user.(*Claims).Name,
	}, true
}

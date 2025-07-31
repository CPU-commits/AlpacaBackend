package utils

import (
	"github.com/gin-gonic/gin"
)

func GetIP(c *gin.Context) string {
	var ip string

	ip = c.Request.Header.Get("CF-Connecting-IP")
	if ip == "" {
		ip = c.Request.Header.Get("CF-Connecting-IPv6")
	}
	if ip == "" {
		ip = c.Request.Header.Get("X-Forwarded-For")
	}
	if ip == "" {
		ip = c.ClientIP()
	}

	return ip
}

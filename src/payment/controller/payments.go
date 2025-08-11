package controller

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"crypto/subtle"
	"encoding/hex"
	"encoding/json"
	"io"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/dto"
	"github.com/CPU-commits/Template_Go-EventDriven/src/payment/service"
	"github.com/gin-gonic/gin"
)

type httpPaymentController struct {
	bus            bus.Bus
	paymentService service.PaymentService
}

func (httpPaymentController *httpPaymentController) GetPayments(c *gin.Context) {
	claims, _ := utils.NewClaimsFromContext(c)
	pageStr := c.DefaultQuery("page", "0")
	page, err := strconv.Atoi(pageStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}

	idSubscriptionStr := c.DefaultQuery("idSubscription", "0")
	idSubscriptionInt, err := strconv.Atoi(idSubscriptionStr)
	if err != nil {
		utils.ResWithMessageID(c, "form.error", http.StatusBadRequest, err)
		return
	}
	idSubscription := int64(idSubscriptionInt)

	payments, total, err := httpPaymentController.paymentService.GetPayments(
		claims.ID,
		service.PaymentsParams{
			Page:           page,
			IDSubscription: idSubscription,
		},
	)
	if err != nil {
		utils.ResFromErr(c, err)
		return
	}

	c.Header("X-Total", strconv.Itoa(int(total)))
	c.Header("X-Per-Page", "10")

	c.JSON(http.StatusOK, payments)
}

func (httpPaymentController *httpPaymentController) LemonSqueezyPagoWebhook(c *gin.Context) {
	// Verify signature
	body, err := io.ReadAll(c.Request.Body)
	if err != nil {
		c.AbortWithStatusJSON(http.StatusBadRequest, gin.H{"error": "Failed to read body"})
		return
	}
	c.Request.Body = io.NopCloser(bytes.NewBuffer(body))

	mac := hmac.New(sha256.New, []byte(settingsData.LEMONSQUEEZY_SIGNING_SECRET))
	mac.Write(body)
	expectedMAC := mac.Sum(nil)

	signatureHex := c.Request.Header.Get("X-Signature")
	if signatureHex == "" {
		c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"error": "Missing signature"})
		return
	}

	signature, err := hex.DecodeString(signatureHex)
	if err != nil {
		c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"error": "Invalid signature format"})
		return
	}

	if len(signature) != len(expectedMAC) || subtle.ConstantTimeCompare(expectedMAC, signature) != 1 {
		c.AbortWithStatusJSON(http.StatusUnauthorized, gin.H{"error": "Invalid signature"})
		return
	}

	var event *LemonSqueezeWebhookEvent
	if err := c.BindJSON(&event); err != nil {
		c.AbortWithStatusJSON(http.StatusBadRequest, gin.H{})
		return
	}
	var identifier string
	eventType := event.Data["type"]

	if eventType == "orders" {
		eventType = "payments"
	}
	if eventType == "subscription-invoices" {
		eventType = "subscriptions"
		identifier = strconv.Itoa(int(event.Data["attributes"].(map[string]any)["subscription_id"].(float64)))
	} else {
		identifier = event.Data["id"].(string)
	}
	var idUserStr string
	var idStudioStr string
	if event.Meta.CustomData != nil {
		if idUser, ok := event.Meta.CustomData["user_id"]; ok {
			idUserStr = idUser.(string)
		}
		if idStudio, ok := event.Meta.CustomData["studio_id"]; ok {
			idStudioStr = idStudio.(string)
		}
	}

	var idStudio int
	if idStudioStr != "" {
		idStudio, err = strconv.Atoi(idStudioStr)
		if err != nil {
			c.AbortWithStatusJSON(http.StatusBadRequest, gin.H{})
			return
		}
	}
	var idUser int
	if idUserStr != "" {
		idUser, err = strconv.Atoi(idUserStr)
		if err != nil {
			c.AbortWithStatusJSON(http.StatusBadRequest, gin.H{})
			return
		}
	}
	var actionEvent string
	if strings.Contains(event.Meta.EventName, "created") {
		actionEvent = "create"
	} else if strings.Contains(event.Meta.EventName, "cancelled") || strings.Contains(event.Meta.EventName, "expired") {
		actionEvent = "cancel"
	} else {
		actionEvent = "update"
	}
	eventToSend := map[string]any{
		"idUser":     idUser,
		"event":      eventType,
		"action":     actionEvent,
		"identifier": identifier,
		"idStudio":   idStudio,
	}
	// Data
	if actionEvent == "create" {
		createdAtStr := event.Data["attributes"].(map[string]any)["created_at"].(string)
		t, _ := time.Parse(time.RFC3339Nano, createdAtStr)

		eventToSend["createdActionData"] = &dto.CreatedActionData{
			CreatedAt: t,
		}
	}

	payload, _ := json.Marshal(eventToSend)

	if err := httpPaymentController.bus.Publish(bus.Event{
		Name:    "payment.event",
		Payload: payload,
	}); err != nil {
		c.AbortWithStatusJSON(http.StatusBadRequest, gin.H{})
		return
	}

	c.JSON(http.StatusOK, nil)
}

func NewHttpPaymentController(bus bus.Bus) *httpPaymentController {
	return &httpPaymentController{
		bus:            bus,
		paymentService: *paymentService,
	}
}

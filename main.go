package main

import (
	"encoding/json"
	"log"
	"os"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http"
	utilsHttp "github.com/CPU-commits/Template_Go-EventDriven/src/cmd/http/utils"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/natefinch/lumberjack"
	"github.com/nicksnyder/go-i18n/v2/i18n"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
	"golang.org/x/text/language"
)

type loggerFromZap struct {
	zapLogger *zap.Logger
}

func (zap loggerFromZap) Info(msg string) {
	zap.zapLogger.Info(msg)
}

func (zap loggerFromZap) Error(msg string) {
	zap.zapLogger.Error(msg)
}

func (zap loggerFromZap) Warn(msg string) {
	zap.zapLogger.Warn(msg)
}

func newLogger() (*zap.Logger, logger.Logger) {
	// Log file
	// Create folder if not exists
	if _, err := os.Stat("logs"); os.IsNotExist(err) {
		err := os.Mkdir("logs", os.ModePerm)
		if err != nil {
			panic(err)
		}
	}
	logEncoder := zapcore.NewJSONEncoder(zap.NewProductionEncoderConfig())
	fileCore := zapcore.NewCore(logEncoder, zapcore.AddSync(&lumberjack.Logger{
		Filename:   "logs/app.log",
		MaxSize:    10,
		MaxBackups: 3,
		MaxAge:     7,
	}), zap.InfoLevel)
	// Log console
	consoleEncoder := zapcore.NewConsoleEncoder(zap.NewProductionEncoderConfig())
	consoleCore := zapcore.NewCore(consoleEncoder, zapcore.AddSync(os.Stdout), zap.InfoLevel)
	// Combine cores for multi-output logging
	teeCore := zapcore.NewTee(fileCore, consoleCore)
	zapLogger := zap.New(teeCore)

	return zapLogger, loggerFromZap{zapLogger: zapLogger}
}

//	@title			API
//	@version		1.0
//	@description	API Server
//	@termsOfService	http://swagger.io/terms/

//	@contact.name	API Support
//	@contact.url	http://www.swagger.io/support
//	@contact.email	support@swagger.io

// lincense.name  Apache 2.0
//	@license.url	http://www.apache.org/licenses/LICENSE-2.0.html

//	@host		localhost:8080
//	@BasePath	/api

//	@securityDefinitions.apikey	ApiKeyAuth
//	@in							header
//	@name						Authorization
//	@description				BearerJWTToken in Authorization Header

//	@accept		json
//	@produce	json

// @schemes	http https
func main() {
	// I18n
	bundle := i18n.NewBundle(language.Spanish)
	bundle.RegisterUnmarshalFunc("json", json.Unmarshal)

	_, err := bundle.LoadMessageFile("langs/es.json")
	if err != nil {
		log.Fatalf("Error loading es messages %v", err)
	}
	_, err = bundle.LoadMessageFile("langs/en.json")
	if err != nil {
		log.Fatalf("Error loading en messages %v", err)
	}
	utils.Bundle = bundle
	// Logger
	zapLogger, logger := newLogger()
	// Cmd
	utilsHttp.Bus = queue.New(logger)
	bus.Init(logger)
	http.Init(zapLogger, logger)
}

package cron

import (
	"log"
	"os"

	"github.com/CPU-commits/Template_Go-EventDriven/src/cmd/bus/queue"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/logger"
	"github.com/CPU-commits/Template_Go-EventDriven/src/publication/controller"
	"github.com/natefinch/lumberjack"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
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

func main() {
	_, logger := newLogger()

	// Inicializa tus dependencias como lo haces en tu servicio
	bus := queue.New(logger)
	cronPublication := controller.NewCronPublication(bus)

	cronPublication.UpdateRatings()

	log.Println("update_ratings done")
}

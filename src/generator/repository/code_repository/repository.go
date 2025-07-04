package code_repository

import (
	"time"

	"github.com/CPU-commits/Template_Go-EventDriven/src/generator/model"
)

type Criteria struct {
	Code     string
	IsActive *bool
	IDUser   int64
	Type     string
}

type CodeRepository interface {
	InsertOne(code model.Code, duration time.Duration) (*model.Code, error) // Crea un codigo
	VerifyCode(code model.Code, codeType string) (*model.Code, error)       // Verifica un codigo, true - Valido, False - no valido PD: Si el codigo es valido lo pasa a no valido
	// DeleteExpiredCodes() error                      // Elimina todos los codigos expirados
}

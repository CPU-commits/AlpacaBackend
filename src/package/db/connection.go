package db

import (
	"database/sql"

	"github.com/CPU-commits/Template_Go-EventDriven/src/settings"
	_ "github.com/jackc/pgx/v5/stdlib"
	"github.com/volatiletech/sqlboiler/v4/boil"
)

var settingsData = settings.GetSettings()
var DB *sql.DB

func init() {
	var err error
	DB, err = sql.Open("pgx", settingsData.DB_CONNECTION)
	if err != nil {
		panic(err)
	}
	if err := DB.Ping(); err != nil {
		panic(err)
	}

	boil.SetDB(DB)
}

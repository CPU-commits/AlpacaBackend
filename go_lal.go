package main

import (
	"fmt"
	"math"
)

// calculateRating calcula el puntaje total con base en:
// - días desde la publicación
// - interacción (likes, views, shares)
// - alcance del perfil (followers)
func calculateRating(daysSincePublished int, likes int, views int, shares int, followers int) float64 {
	// Parámetros de ponderación
	alpha := 0.3 // peso para frescura
	beta := 0.5  // peso para interacción
	gamma := 0.2 // peso para alcance

	// FreshnessScore: decae exponencialmente con el tiempo
	freshnessScore := math.Exp(-0.05 * float64(daysSincePublished))

	// InteractionScore: logaritmo para controlar grandes volúmenes
	interactionScore := math.Log(1 + 2*float64(likes) + 0.5*float64(views) + 3*float64(shares))

	// ReachScore: logaritmo del total de seguidores
	reachScore := math.Log(1 + float64(followers))

	// Rating final
	rating := alpha*freshnessScore + beta*interactionScore + gamma*reachScore

	return rating
}

func main() {
	// Ejemplo de prueba
	days := 1
	likes := 20000
	views := 80000
	shares := 1000
	followers := 100000

	rating := calculateRating(days, likes, views, shares, followers)
	fmt.Printf("Rating final: %.4f\n", rating)
}

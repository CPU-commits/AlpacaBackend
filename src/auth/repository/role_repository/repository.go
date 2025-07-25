package role_repository

import "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"

type Criteria struct {
	Role   model.Role
	IDUser int64
}

type RoleRepository interface {
	Exists(criteria *Criteria) (bool, error)
}

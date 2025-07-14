package model

import "github.com/CPU-commits/Template_Go-EventDriven/src/utils"

// Permissions
type StudioPermission string

const (
	PUBLISH_PERMISSION           StudioPermission = "s.publish"
	EDIT_PUBLICATIONS_PERMISSION StudioPermission = "s.edit_publication"
	SHOW_PEOPLE_PERMISSION       StudioPermission = "s.show_people"
	JOIN_PEOPLE_PERMISSION       StudioPermission = "s.join_people"
	GIVE_ROLES_PERMISSION        StudioPermission = "s.give_roles"
	GIVE_PERMISSIONS_PERMISSION  StudioPermission = "s.give_permissions"
	DELETE_PEOPLE_PERMISSION     StudioPermission = "s.delete_people"
)

var ALL_PERMISSIONS []StudioPermission = []StudioPermission{
	PUBLISH_PERMISSION,
	EDIT_PUBLICATIONS_PERMISSION,
	SHOW_PEOPLE_PERMISSION,
	JOIN_PEOPLE_PERMISSION,
	GIVE_ROLES_PERMISSION,
	GIVE_PERMISSIONS_PERMISSION,
	DELETE_PEOPLE_PERMISSION,
}

func IsPermission(permission string) bool {
	return utils.Includes(ALL_PERMISSIONS, StudioPermission(permission))
}

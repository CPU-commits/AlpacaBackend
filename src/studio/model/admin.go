package model

import "github.com/CPU-commits/Template_Go-EventDriven/src/utils"

// Permissions
type StudioPermission string

const (
	PUBLISH_PERMISSION              StudioPermission = "s.publish"
	EDIT_PUBLICATIONS_PERMISSION    StudioPermission = "s.edit_publication"
	SHOW_PEOPLE_PERMISSION          StudioPermission = "s.show_people"
	JOIN_PEOPLE_PERMISSION          StudioPermission = "s.join_people"
	GIVE_ROLES_PERMISSION           StudioPermission = "s.give_roles"
	GIVE_PERMISSIONS_PERMISSION     StudioPermission = "s.give_permissions"
	DELETE_PEOPLE_PERMISSION        StudioPermission = "s.delete_people"
	ASSIGN_TATTOO_ARTIST_PERMISSION StudioPermission = "s.assign_to_appointment"
	SCHEDULE_APPOINTMENT_PERMISSION StudioPermission = "s.schedule_appointment"
	CANCEL_APPOINTMENT_PERMISSION   StudioPermission = "s.cancel_appointment"
)

var ALL_PERMISSIONS []StudioPermission = []StudioPermission{
	PUBLISH_PERMISSION,
	EDIT_PUBLICATIONS_PERMISSION,
	SHOW_PEOPLE_PERMISSION,
	JOIN_PEOPLE_PERMISSION,
	GIVE_ROLES_PERMISSION,
	GIVE_PERMISSIONS_PERMISSION,
	DELETE_PEOPLE_PERMISSION,
	ASSIGN_TATTOO_ARTIST_PERMISSION,
	SCHEDULE_APPOINTMENT_PERMISSION,
	CANCEL_APPOINTMENT_PERMISSION,
}

// Struct permissions
type Permission struct {
	Permission StudioPermission   `json:"permission"`
	DependsOn  []StudioPermission `json:"dependsOn"`
}

var (
	PublishPermission = Permission{
		Permission: PUBLISH_PERMISSION,
	}
	EditPublicationsPermission = Permission{
		Permission: EDIT_PUBLICATIONS_PERMISSION,
	}
	ShowPeoplePermission = Permission{
		Permission: SHOW_PEOPLE_PERMISSION,
	}
	JoinPeoplePermission = Permission{
		Permission: JOIN_PEOPLE_PERMISSION,
		DependsOn: []StudioPermission{
			SHOW_PEOPLE_PERMISSION,
		},
	}
	GiveRolesPermission = Permission{
		Permission: GIVE_ROLES_PERMISSION,
		DependsOn: []StudioPermission{
			SHOW_PEOPLE_PERMISSION,
		},
	}
	GivePermissionsPermission = Permission{
		Permission: GIVE_PERMISSIONS_PERMISSION,
		DependsOn: []StudioPermission{
			SHOW_PEOPLE_PERMISSION,
		},
	}
	DeletePeoplePermission = Permission{
		Permission: DELETE_PEOPLE_PERMISSION,
		DependsOn: []StudioPermission{
			SHOW_PEOPLE_PERMISSION,
		},
	}
	AssignTattooArtistPermission = Permission{
		Permission: ASSIGN_TATTOO_ARTIST_PERMISSION,
		DependsOn: []StudioPermission{
			SHOW_PEOPLE_PERMISSION,
		},
	}
	ScheduleAppointmentPermission = Permission{
		Permission: SCHEDULE_APPOINTMENT_PERMISSION,
	}
	CancelAppointmentPermission = Permission{
		Permission: CANCEL_APPOINTMENT_PERMISSION,
	}
)

var AllPermissionsTree = []Permission{
	PublishPermission,
	EditPublicationsPermission,
	ShowPeoplePermission,
	JoinPeoplePermission,
	GiveRolesPermission,
	GivePermissionsPermission,
	DeletePeoplePermission,
	AssignTattooArtistPermission,
	CancelAppointmentPermission,
	ScheduleAppointmentPermission,
}

func IsPermission(permission string) bool {
	return utils.Includes(ALL_PERMISSIONS, StudioPermission(permission))
}

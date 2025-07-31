package appointment_repository

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/CPU-commits/Template_Go-EventDriven/src/appointment/model"
	authModel "github.com/CPU-commits/Template_Go-EventDriven/src/auth/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/auth/repository/user_repository"
	fileModel "github.com/CPU-commits/Template_Go-EventDriven/src/file/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db"
	"github.com/CPU-commits/Template_Go-EventDriven/src/package/db/models"
	"github.com/CPU-commits/Template_Go-EventDriven/src/studio/repository/studio_repository"
	tattooModel "github.com/CPU-commits/Template_Go-EventDriven/src/tattoo/model"
	"github.com/CPU-commits/Template_Go-EventDriven/src/user/repository/profile_repository"
	"github.com/CPU-commits/Template_Go-EventDriven/src/utils"
	"github.com/aarondl/null/v8"
	"github.com/aarondl/sqlboiler/v4/boil"
	. "github.com/aarondl/sqlboiler/v4/queries/qm"
)

type appointmentRepositorySql struct {
	db                   *sql.DB
	sqlUserRepository    user_repository.SqlUserRepository
	sqlProfileRepository profile_repository.SqlProfileRepository
	sqlStudioRepository  studio_repository.SqlStudioRepository
}

func (sqlAR appointmentRepositorySql) sqlAppointmentToModel(
	sqlAppointment *models.Appointment,
) *model.Appointment {
	appointment := &model.Appointment{
		ID:             sqlAppointment.ID,
		IDTattooArtist: sqlAppointment.IDTattooArtist.Int64,
		IDUser:         sqlAppointment.IDUser,
		IDCalendar:     sqlAppointment.IDCalendar.String,
		Status:         model.AppointmentStatus(sqlAppointment.Status),
		Description:    sqlAppointment.Description,
		Phone:          sqlAppointment.Phone.String,
		HasIdea:        &sqlAppointment.HasIdea,
		HasDesign:      &sqlAppointment.HasDesign,
		IDDesign:       &sqlAppointment.IDDesign.Int64,
		Area:           model.AppointmentArea(sqlAppointment.Area.String),
		Height:         float32(sqlAppointment.Height.Float64),
		Width:          float32(sqlAppointment.Width.Float64),
		Color:          model.AppointmentColor(sqlAppointment.Color.String),
		CreatedAt:      sqlAppointment.CreatedAt,
		FinishedAt:     sqlAppointment.FinishedAt.Ptr(),
		Duration:       float32(sqlAppointment.Duration.Float64),
		ScheduledAt:    sqlAppointment.ScheduledAt.Ptr(),
		IDStudio:       sqlAppointment.IDStudio.Int64,
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDAppointmentAppointmentImages != nil {
		var images []fileModel.Image
		sqlAppImages := sqlAppointment.R.IDAppointmentAppointmentImages

		for _, sqlAppImage := range sqlAppImages {
			if sqlAppImage.R != nil && sqlAppImage.R.IDImageImage != nil {
				sqlImage := sqlAppImage.R.IDImageImage

				images = append(images, fileModel.Image{
					ID:        sqlImage.ID,
					Key:       sqlImage.Key,
					Name:      sqlImage.Name,
					MimeType:  sqlImage.MimeType,
					CreatedAt: sqlAppointment.CreatedAt,
				})
			}
		}
		appointment.Images = images
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDUserUser != nil {
		var user *authModel.User

		sqlUser := sqlAppointment.R.IDUserUser

		user = &authModel.User{
			ID:       sqlUser.ID,
			Name:     sqlUser.Name,
			Username: sqlUser.Username,
			Email:    sqlUser.Email,
		}
		if sqlUser.R != nil && sqlUser.R.IDUserProfile != nil {
			profile := sqlAR.sqlProfileRepository.SqlProfileToProfile(
				sqlUser.R.IDUserProfile,
				"user",
			)
			appointment.UserProfile = &profile
		}
		appointment.User = user
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDTattooArtistUser != nil {
		var user *authModel.User

		sqlUser := sqlAppointment.R.IDTattooArtistUser

		user = &authModel.User{
			ID:       sqlUser.ID,
			Name:     sqlUser.Name,
			Username: sqlUser.Username,
			Email:    sqlUser.Email,
		}
		if sqlUser.R != nil && sqlUser.R.IDUserProfile != nil {
			profile := sqlAR.sqlProfileRepository.SqlProfileToProfile(
				sqlUser.R.IDUserProfile,
				"user",
			)
			appointment.TattooArtistProfile = &profile
		}
		appointment.TattooArtist = user
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDAppointmentReview != nil {
		sqlReview := sqlAppointment.R.IDAppointmentReview
		review := &model.Review{
			ID:            sqlReview.ID,
			IDUser:        sqlReview.IDUser,
			IDProfile:     sqlReview.IDProfile,
			IDAppointment: sqlReview.IDAppointment,
			Stars:         int16(sqlReview.Stars),
			Review:        sqlReview.Content,
		}

		appointment.Review = review
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDStudioStudio != nil {
		sqlStudio := sqlAppointment.R.IDStudioStudio

		studio := sqlAR.sqlStudioRepository.SqlStudioToModel(sqlStudio)

		appointment.Studio = studio
	}
	if sqlAppointment.R != nil && sqlAppointment.R.IDDesignDesign != nil {
		sqlDesign := sqlAppointment.R.IDDesignDesign
		sqlImage := sqlDesign.R.IDImageImage
		design := tattooModel.Design{
			ID: sqlDesign.ID,
			Image: fileModel.Image{
				ID:        sqlImage.ID,
				Key:       sqlImage.Key,
				MimeType:  sqlImage.MimeType,
				Name:      sqlImage.Name,
				CreatedAt: sqlImage.CreatedAt,
			},
			Description: sqlDesign.Description.String,
			Price:       sqlDesign.Price.Int64,
			Categories:  sqlDesign.Categories,
			CreatedAt:   sqlDesign.CreatedAt,
		}
		appointment.Design = design
	}

	return appointment
}

func (sqlAR appointmentRepositorySql) criteriaToWhere(criteria *Criteria) []QueryMod {
	where := []QueryMod{}
	if criteria == nil {
		return where
	}
	if criteria.ID != 0 {
		where = append(where, models.AppointmentWhere.ID.EQ(criteria.ID))
	}
	if criteria.IDNE != 0 {
		where = append(where, models.AppointmentWhere.ID.NEQ(criteria.ID))
	}
	if criteria.IDUser != 0 {
		where = append(where, models.AppointmentWhere.IDUser.EQ(criteria.IDUser))
	}
	if criteria.IDTattooArtist != 0 {
		where = append(where, models.AppointmentWhere.IDTattooArtist.EQ(null.Int64From(criteria.IDTattooArtist)))
	}
	if criteria.Status != "" {
		where = append(where, models.AppointmentWhere.Status.EQ(string(criteria.Status)))
	}
	if !criteria.ScheduledAtGTE.IsZero() {
		where = append(where, models.AppointmentWhere.ScheduledAt.IsNotNull())
		where = append(where, models.AppointmentWhere.ScheduledAt.GTE(null.TimeFrom(criteria.ScheduledAtGTE)))
	}
	if criteria.IDStudio != 0 {
		where = append(where, models.AppointmentWhere.IDStudio.EQ(null.Int64From(criteria.IDStudio)))
	}
	if criteria.FinishedAt != nil {
		where = append(where, models.AppointmentWhere.FinishedAt.IsNotNull())
		if !criteria.FinishedAt.LTE.IsZero() {
			where = append(where, models.AppointmentWhere.FinishedAt.LTE(null.TimeFrom(criteria.FinishedAt.LTE)))
		}
		if !criteria.FinishedAt.LT.IsZero() {
			where = append(where, models.AppointmentWhere.FinishedAt.LT(null.TimeFrom(criteria.FinishedAt.LT)))
		}
		if !criteria.FinishedAt.GTE.IsZero() {
			where = append(where, models.AppointmentWhere.FinishedAt.GTE(null.TimeFrom(criteria.FinishedAt.GTE)))
		}
		if !criteria.FinishedAt.GT.IsZero() {
			where = append(where, models.AppointmentWhere.FinishedAt.GT(null.TimeFrom(criteria.FinishedAt.GT)))
		}
		if !criteria.FinishedAt.EQ.IsZero() {
			where = append(where, models.AppointmentWhere.FinishedAt.EQ(null.TimeFrom(criteria.FinishedAt.EQ)))
		}
	}

	var orMods []QueryMod
	for _, clause := range criteria.Or {
		orWhere := sqlAR.criteriaToWhere(&clause)
		orMods = append(orMods, Or2(Expr(orWhere...)))
	}
	if orMods != nil {
		where = append(where, Expr(orMods...))
	}

	return where
}

func (appointmentRepositorySql) sortToMod(sort *Sort) []QueryMod {
	mod := []QueryMod{}
	if sort == nil {
		return mod
	}
	switch sort.CreatedAt {
	case "ASC":
		mod = append(mod, OrderBy(fmt.Sprintf("%s asc", models.AppointmentColumns.CreatedAt)))
	case "DESC":
		mod = append(mod, OrderBy(fmt.Sprintf("%s desc", models.AppointmentColumns.CreatedAt)))
	}

	return mod
}

func (sqlAR appointmentRepositorySql) loadToMod(load *LoadOpts) []QueryMod {
	mod := []QueryMod{}
	if load == nil {
		return mod
	}
	if load.Images {
		mod = append(mod, Load(Rels(models.AppointmentRels.IDAppointmentAppointmentImages, models.AppointmentImageRels.IDImageImage)))
	}
	if load.Review {
		mod = append(mod, Load(models.AppointmentRels.IDAppointmentReview))
	}
	if load.Design {
		mod = append(mod, Load(Rels(
			models.AppointmentRels.IDDesignDesign,
			models.DesignRels.IDImageImage,
		)))
	}
	if load.User != nil {
		mod = append(mod, Load(
			models.AppointmentRels.IDUserUser,
			sqlAR.sqlUserRepository.SelectOpts(load.User)...,
		))
		if load.Profile != nil {
			mod = append(mod, Load(
				Rels(models.AppointmentRels.IDUserUser, models.UserRels.IDUserProfile),
				sqlAR.sqlProfileRepository.SelectOpts(load.Profile)...,
			))
		}
	}
	if load.TattooArtist != nil {
		mod = append(mod, Load(
			models.AppointmentRels.IDTattooArtistUser,
			sqlAR.sqlUserRepository.SelectOpts(load.User)...,
		))
		if load.Profile != nil {
			mod = append(mod, Load(
				Rels(models.AppointmentRels.IDTattooArtistUser, models.UserRels.IDUserProfile),
				sqlAR.sqlProfileRepository.SelectOpts(load.Profile)...,
			))
			if load.ProfileAvatar {
				mod = append(mod, Load(
					Rels(models.AppointmentRels.IDTattooArtistUser, models.UserRels.IDUserProfile, models.ProfileRels.IDAvatarImage),
				))
			}
		}
	}

	if load.Studio != nil {
		mod = append(mod, Load(
			models.AppointmentRels.IDStudioStudio,
			sqlAR.sqlStudioRepository.SelectToMod(load.Studio)...),
		)
	}

	return mod
}

func (sqlAR appointmentRepositorySql) CountGroupByStatus(criteria *Criteria) ([]CountGroupByStatusResult, error) {
	where := sqlAR.criteriaToWhere(criteria)

	type SqlCountResult struct {
		Count  int64  `boil:"count"`
		Status string `boil:"status"`
	}
	var count []SqlCountResult

	err := models.NewQuery(
		append([]QueryMod{
			From(models.TableNames.Appointments),
			Select("status"),
			Select("COUNT(*) as count"),
			GroupBy("status"),
		}, where...)...,
	).Bind(context.Background(), sqlAR.db, &count)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(count, func(count SqlCountResult) CountGroupByStatusResult {
		return CountGroupByStatusResult{
			Count:  count.Count,
			Status: model.AppointmentStatus(count.Status),
		}
	}), nil
}

func (sqlAR appointmentRepositorySql) findOptionsToMod(opts *findOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	if opts.skip != nil {
		mod = append(mod, Offset(int(*opts.skip)))
	}
	if opts.limit != nil {
		mod = append(mod, Limit(int(*opts.limit)))
	}
	if opts.sort != nil {
		mod = append(mod, sqlAR.sortToMod(opts.sort)...)
	}
	if opts.load != nil {
		mod = append(mod, sqlAR.loadToMod(opts.load)...)
	}

	return mod
}

func (sqlAR appointmentRepositorySql) Find(
	criteria *Criteria,
	opts *findOptions,
) ([]model.Appointment, error) {
	where := sqlAR.criteriaToWhere(criteria)
	mod := sqlAR.findOptionsToMod(opts)

	appointments, err := models.Appointments(append(where, mod...)...).All(context.Background(), sqlAR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return utils.MapNoError(appointments, func(sqlAppointment *models.Appointment) model.Appointment {
		return *sqlAR.sqlAppointmentToModel(sqlAppointment)
	}), nil
}

func (appointmentRepositorySql) selectToMod(selectOpts *SelectOpts) []QueryMod {
	mod := []QueryMod{}
	if selectOpts == nil {
		return mod
	}
	if selectOpts.IDUser != nil && *selectOpts.IDUser {
		mod = append(mod, Select(models.AppointmentColumns.IDUser))
	}
	if selectOpts.IDCalendar != nil && *selectOpts.IDCalendar {
		mod = append(mod, Select(models.AppointmentColumns.IDCalendar))
	}
	if selectOpts.IDTattooArtist != nil && *selectOpts.IDTattooArtist {
		mod = append(mod, Select(models.AppointmentColumns.IDTattooArtist))
	}
	if selectOpts.IDStudio != nil && *selectOpts.IDStudio {
		mod = append(mod, Select(models.AppointmentColumns.IDStudio))
	}

	return mod
}

func (sqlAR appointmentRepositorySql) findOneOptionsToMod(opts *findOneOptions) []QueryMod {
	mod := []QueryMod{}
	if opts == nil {
		return mod
	}
	mod = append(mod, sqlAR.selectToMod(opts.selectOpts)...)

	return mod
}

func (sqlAR appointmentRepositorySql) FindOne(
	criteria *Criteria,
	opts *findOneOptions,
) (*model.Appointment, error) {
	where := sqlAR.criteriaToWhere(criteria)
	mod := sqlAR.findOneOptionsToMod(opts)

	sqlAppointment, err := models.Appointments(append(where, mod...)...).One(context.Background(), sqlAR.db)
	if err != nil {
		return nil, utils.ErrRepositoryFailed
	}

	return sqlAR.sqlAppointmentToModel(sqlAppointment), nil
}

func (sqlAR appointmentRepositorySql) Count(criteria *Criteria) (int64, error) {
	where := sqlAR.criteriaToWhere(criteria)

	count, err := models.Appointments(where...).Count(context.Background(), sqlAR.db)
	if err != nil {
		return 0, utils.ErrRepositoryFailed
	}

	return count, nil
}

func (sqlAR appointmentRepositorySql) Exists(criteria *Criteria) (bool, error) {
	where := sqlAR.criteriaToWhere(criteria)

	exists, err := models.Appointments(where...).Exists(context.Background(), sqlAR.db)
	if err != nil {
		return false, utils.ErrRepositoryFailed
	}

	return exists, nil
}

func (sqlAR appointmentRepositorySql) Update(criteria *Criteria, data *UpdateData) error {
	where := sqlAR.criteriaToWhere(criteria)
	// Data
	cols := models.M{}
	if data.Status != "" {
		cols[models.AppointmentColumns.Status] = data.Status
	}
	if data.UnsetDuration {
		cols[models.AppointmentColumns.Duration] = null.Float64FromPtr(nil)
	} else if data.Duration != 0 {
		cols[models.AppointmentColumns.Duration] = data.Duration
	}
	if !data.ScheduledAt.IsZero() {
		cols[models.AppointmentColumns.ScheduledAt] = data.ScheduledAt
	}
	if data.UnsetFinishedAt {
		cols[models.AppointmentColumns.FinishedAt] = null.TimeFromPtr(nil)
	} else if !data.FinishedAt.IsZero() {
		cols[models.AppointmentColumns.FinishedAt] = data.FinishedAt
	}
	if data.IDCalendar != "" {
		cols[models.AppointmentColumns.IDCalendar] = data.IDCalendar
	}
	if data.IDTattooArtist != 0 {
		cols[models.AppointmentColumns.IDTattooArtist] = data.IDTattooArtist
	}

	if _, err := models.Appointments(where...).UpdateAll(context.Background(), sqlAR.db, cols); err != nil {
		return utils.ErrRepositoryFailed
	}

	return nil
}

func (sqlAR appointmentRepositorySql) Insert(
	appointment *model.Appointment,
) (*model.Appointment, error) {
	tx, err := sqlAR.db.BeginTx(context.Background(), nil)
	if err != nil {
		fmt.Printf("err: %v\n", err)

		return nil, utils.ErrRepositoryFailed
	}

	sqlAppointment := models.Appointment{
		IDUser:         appointment.IDUser,
		IDTattooArtist: null.NewInt64(appointment.IDTattooArtist, appointment.IDTattooArtist != 0),
		Status:         string(appointment.Status),
		Area:           null.NewString(string(appointment.Area), appointment.Area != ""),
		Color:          null.NewString(string(appointment.Color), appointment.Color != ""),
		Description:    appointment.Description,
		HasIdea:        *appointment.HasIdea,
		HasDesign:      *appointment.HasDesign,
		IDDesign:       null.Int64FromPtr(appointment.IDDesign),
		Height:         null.Float64From(float64(appointment.Height)),
		Width:          null.Float64From(float64(appointment.Width)),
		Phone:          null.StringFrom(appointment.Phone),
		IDStudio:       null.NewInt64(appointment.IDStudio, appointment.IDStudio != 0),
	}
	if err := sqlAppointment.Insert(context.Background(), tx, boil.Infer()); err != nil {
		tx.Rollback()
		fmt.Printf("err: %v\n", err)

		return nil, utils.ErrRepositoryFailed
	}
	// Upload images
	for _, image := range appointment.Images {
		sqlImage := models.Image{
			Key:      image.Key,
			Name:     image.Name,
			MimeType: image.MimeType,
		}

		if err := sqlImage.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()
			fmt.Printf("err: %v\n", err)

			return nil, utils.ErrRepositoryFailed
		}
		sqlImageAppointment := models.AppointmentImage{
			IDAppointment: sqlAppointment.ID,
			IDImage:       sqlImage.ID,
		}
		if err := sqlImageAppointment.Insert(context.Background(), tx, boil.Infer()); err != nil {
			tx.Rollback()
			fmt.Printf("err: %v\n", err)

			return nil, utils.ErrRepositoryFailed
		}
	}

	if err := tx.Commit(); err != nil {
		tx.Rollback()
		fmt.Printf("err: %v\n", err)
		return nil, utils.ErrRepositoryFailed
	}

	return sqlAR.sqlAppointmentToModel(&sqlAppointment), nil
}

func NewSqlAppointmentRepository() AppointmentRepository {
	return appointmentRepositorySql{
		db: db.DB,
		sqlUserRepository: user_repository.SqlExplicitUserRepository(
			db.DB,
		),
		sqlProfileRepository: profile_repository.SqlExplicitProfileRepository(
			db.DB,
		),
		sqlStudioRepository: studio_repository.SqlExplicitStudioRepository(),
	}
}

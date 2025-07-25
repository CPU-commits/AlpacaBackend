// Code generated by SQLBoiler 4.19.5 (https://github.com/aarondl/sqlboiler). DO NOT EDIT.
// This file is meant to be re-generated in place and/or deleted at any time.

package models

import (
	"context"
	"database/sql"
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/aarondl/sqlboiler/v4/boil"
	"github.com/aarondl/sqlboiler/v4/queries"
	"github.com/aarondl/sqlboiler/v4/queries/qm"
	"github.com/aarondl/sqlboiler/v4/queries/qmhelper"
	"github.com/aarondl/strmangle"
	"github.com/friendsofgo/errors"
)

// StudioAdminsPermission is an object representing the database table.
type StudioAdminsPermission struct {
	ID         int64     `boil:"id" json:"id" toml:"id" yaml:"id"`
	IDAdmin    int64     `boil:"id_admin" json:"id_admin" toml:"id_admin" yaml:"id_admin"`
	Permission string    `boil:"permission" json:"permission" toml:"permission" yaml:"permission"`
	CreatedAt  time.Time `boil:"created_at" json:"created_at" toml:"created_at" yaml:"created_at"`

	R *studioAdminsPermissionR `boil:"-" json:"-" toml:"-" yaml:"-"`
	L studioAdminsPermissionL  `boil:"-" json:"-" toml:"-" yaml:"-"`
}

var StudioAdminsPermissionColumns = struct {
	ID         string
	IDAdmin    string
	Permission string
	CreatedAt  string
}{
	ID:         "id",
	IDAdmin:    "id_admin",
	Permission: "permission",
	CreatedAt:  "created_at",
}

var StudioAdminsPermissionTableColumns = struct {
	ID         string
	IDAdmin    string
	Permission string
	CreatedAt  string
}{
	ID:         "studio_admins_permissions.id",
	IDAdmin:    "studio_admins_permissions.id_admin",
	Permission: "studio_admins_permissions.permission",
	CreatedAt:  "studio_admins_permissions.created_at",
}

// Generated where

var StudioAdminsPermissionWhere = struct {
	ID         whereHelperint64
	IDAdmin    whereHelperint64
	Permission whereHelperstring
	CreatedAt  whereHelpertime_Time
}{
	ID:         whereHelperint64{field: "\"studio_admins_permissions\".\"id\""},
	IDAdmin:    whereHelperint64{field: "\"studio_admins_permissions\".\"id_admin\""},
	Permission: whereHelperstring{field: "\"studio_admins_permissions\".\"permission\""},
	CreatedAt:  whereHelpertime_Time{field: "\"studio_admins_permissions\".\"created_at\""},
}

// StudioAdminsPermissionRels is where relationship names are stored.
var StudioAdminsPermissionRels = struct {
	IDAdminStudioUser string
}{
	IDAdminStudioUser: "IDAdminStudioUser",
}

// studioAdminsPermissionR is where relationships are stored.
type studioAdminsPermissionR struct {
	IDAdminStudioUser *StudioUser `boil:"IDAdminStudioUser" json:"IDAdminStudioUser" toml:"IDAdminStudioUser" yaml:"IDAdminStudioUser"`
}

// NewStruct creates a new relationship struct
func (*studioAdminsPermissionR) NewStruct() *studioAdminsPermissionR {
	return &studioAdminsPermissionR{}
}

func (o *StudioAdminsPermission) GetIDAdminStudioUser() *StudioUser {
	if o == nil {
		return nil
	}

	return o.R.GetIDAdminStudioUser()
}

func (r *studioAdminsPermissionR) GetIDAdminStudioUser() *StudioUser {
	if r == nil {
		return nil
	}

	return r.IDAdminStudioUser
}

// studioAdminsPermissionL is where Load methods for each relationship are stored.
type studioAdminsPermissionL struct{}

var (
	studioAdminsPermissionAllColumns            = []string{"id", "id_admin", "permission", "created_at"}
	studioAdminsPermissionColumnsWithoutDefault = []string{"id_admin", "permission"}
	studioAdminsPermissionColumnsWithDefault    = []string{"id", "created_at"}
	studioAdminsPermissionPrimaryKeyColumns     = []string{"id"}
	studioAdminsPermissionGeneratedColumns      = []string{}
)

type (
	// StudioAdminsPermissionSlice is an alias for a slice of pointers to StudioAdminsPermission.
	// This should almost always be used instead of []StudioAdminsPermission.
	StudioAdminsPermissionSlice []*StudioAdminsPermission
	// StudioAdminsPermissionHook is the signature for custom StudioAdminsPermission hook methods
	StudioAdminsPermissionHook func(context.Context, boil.ContextExecutor, *StudioAdminsPermission) error

	studioAdminsPermissionQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	studioAdminsPermissionType                 = reflect.TypeOf(&StudioAdminsPermission{})
	studioAdminsPermissionMapping              = queries.MakeStructMapping(studioAdminsPermissionType)
	studioAdminsPermissionPrimaryKeyMapping, _ = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, studioAdminsPermissionPrimaryKeyColumns)
	studioAdminsPermissionInsertCacheMut       sync.RWMutex
	studioAdminsPermissionInsertCache          = make(map[string]insertCache)
	studioAdminsPermissionUpdateCacheMut       sync.RWMutex
	studioAdminsPermissionUpdateCache          = make(map[string]updateCache)
	studioAdminsPermissionUpsertCacheMut       sync.RWMutex
	studioAdminsPermissionUpsertCache          = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force qmhelper dependency for where clause generation (which doesn't
	// always happen)
	_ = qmhelper.Where
)

var studioAdminsPermissionAfterSelectMu sync.Mutex
var studioAdminsPermissionAfterSelectHooks []StudioAdminsPermissionHook

var studioAdminsPermissionBeforeInsertMu sync.Mutex
var studioAdminsPermissionBeforeInsertHooks []StudioAdminsPermissionHook
var studioAdminsPermissionAfterInsertMu sync.Mutex
var studioAdminsPermissionAfterInsertHooks []StudioAdminsPermissionHook

var studioAdminsPermissionBeforeUpdateMu sync.Mutex
var studioAdminsPermissionBeforeUpdateHooks []StudioAdminsPermissionHook
var studioAdminsPermissionAfterUpdateMu sync.Mutex
var studioAdminsPermissionAfterUpdateHooks []StudioAdminsPermissionHook

var studioAdminsPermissionBeforeDeleteMu sync.Mutex
var studioAdminsPermissionBeforeDeleteHooks []StudioAdminsPermissionHook
var studioAdminsPermissionAfterDeleteMu sync.Mutex
var studioAdminsPermissionAfterDeleteHooks []StudioAdminsPermissionHook

var studioAdminsPermissionBeforeUpsertMu sync.Mutex
var studioAdminsPermissionBeforeUpsertHooks []StudioAdminsPermissionHook
var studioAdminsPermissionAfterUpsertMu sync.Mutex
var studioAdminsPermissionAfterUpsertHooks []StudioAdminsPermissionHook

// doAfterSelectHooks executes all "after Select" hooks.
func (o *StudioAdminsPermission) doAfterSelectHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionAfterSelectHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *StudioAdminsPermission) doBeforeInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionBeforeInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *StudioAdminsPermission) doAfterInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionAfterInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *StudioAdminsPermission) doBeforeUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionBeforeUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *StudioAdminsPermission) doAfterUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionAfterUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *StudioAdminsPermission) doBeforeDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionBeforeDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *StudioAdminsPermission) doAfterDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionAfterDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *StudioAdminsPermission) doBeforeUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionBeforeUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *StudioAdminsPermission) doAfterUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range studioAdminsPermissionAfterUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddStudioAdminsPermissionHook registers your hook function for all future operations.
func AddStudioAdminsPermissionHook(hookPoint boil.HookPoint, studioAdminsPermissionHook StudioAdminsPermissionHook) {
	switch hookPoint {
	case boil.AfterSelectHook:
		studioAdminsPermissionAfterSelectMu.Lock()
		studioAdminsPermissionAfterSelectHooks = append(studioAdminsPermissionAfterSelectHooks, studioAdminsPermissionHook)
		studioAdminsPermissionAfterSelectMu.Unlock()
	case boil.BeforeInsertHook:
		studioAdminsPermissionBeforeInsertMu.Lock()
		studioAdminsPermissionBeforeInsertHooks = append(studioAdminsPermissionBeforeInsertHooks, studioAdminsPermissionHook)
		studioAdminsPermissionBeforeInsertMu.Unlock()
	case boil.AfterInsertHook:
		studioAdminsPermissionAfterInsertMu.Lock()
		studioAdminsPermissionAfterInsertHooks = append(studioAdminsPermissionAfterInsertHooks, studioAdminsPermissionHook)
		studioAdminsPermissionAfterInsertMu.Unlock()
	case boil.BeforeUpdateHook:
		studioAdminsPermissionBeforeUpdateMu.Lock()
		studioAdminsPermissionBeforeUpdateHooks = append(studioAdminsPermissionBeforeUpdateHooks, studioAdminsPermissionHook)
		studioAdminsPermissionBeforeUpdateMu.Unlock()
	case boil.AfterUpdateHook:
		studioAdminsPermissionAfterUpdateMu.Lock()
		studioAdminsPermissionAfterUpdateHooks = append(studioAdminsPermissionAfterUpdateHooks, studioAdminsPermissionHook)
		studioAdminsPermissionAfterUpdateMu.Unlock()
	case boil.BeforeDeleteHook:
		studioAdminsPermissionBeforeDeleteMu.Lock()
		studioAdminsPermissionBeforeDeleteHooks = append(studioAdminsPermissionBeforeDeleteHooks, studioAdminsPermissionHook)
		studioAdminsPermissionBeforeDeleteMu.Unlock()
	case boil.AfterDeleteHook:
		studioAdminsPermissionAfterDeleteMu.Lock()
		studioAdminsPermissionAfterDeleteHooks = append(studioAdminsPermissionAfterDeleteHooks, studioAdminsPermissionHook)
		studioAdminsPermissionAfterDeleteMu.Unlock()
	case boil.BeforeUpsertHook:
		studioAdminsPermissionBeforeUpsertMu.Lock()
		studioAdminsPermissionBeforeUpsertHooks = append(studioAdminsPermissionBeforeUpsertHooks, studioAdminsPermissionHook)
		studioAdminsPermissionBeforeUpsertMu.Unlock()
	case boil.AfterUpsertHook:
		studioAdminsPermissionAfterUpsertMu.Lock()
		studioAdminsPermissionAfterUpsertHooks = append(studioAdminsPermissionAfterUpsertHooks, studioAdminsPermissionHook)
		studioAdminsPermissionAfterUpsertMu.Unlock()
	}
}

// One returns a single studioAdminsPermission record from the query.
func (q studioAdminsPermissionQuery) One(ctx context.Context, exec boil.ContextExecutor) (*StudioAdminsPermission, error) {
	o := &StudioAdminsPermission{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(ctx, exec, o)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for studio_admins_permissions")
	}

	if err := o.doAfterSelectHooks(ctx, exec); err != nil {
		return o, err
	}

	return o, nil
}

// All returns all StudioAdminsPermission records from the query.
func (q studioAdminsPermissionQuery) All(ctx context.Context, exec boil.ContextExecutor) (StudioAdminsPermissionSlice, error) {
	var o []*StudioAdminsPermission

	err := q.Bind(ctx, exec, &o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to StudioAdminsPermission slice")
	}

	if len(studioAdminsPermissionAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(ctx, exec); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// Count returns the count of all StudioAdminsPermission records in the query.
func (q studioAdminsPermissionQuery) Count(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count studio_admins_permissions rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table.
func (q studioAdminsPermissionQuery) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if studio_admins_permissions exists")
	}

	return count > 0, nil
}

// IDAdminStudioUser pointed to by the foreign key.
func (o *StudioAdminsPermission) IDAdminStudioUser(mods ...qm.QueryMod) studioUserQuery {
	queryMods := []qm.QueryMod{
		qm.Where("\"id\" = ?", o.IDAdmin),
	}

	queryMods = append(queryMods, mods...)

	return StudioUsers(queryMods...)
}

// LoadIDAdminStudioUser allows an eager lookup of values, cached into the
// loaded structs of the objects. This is for an N-1 relationship.
func (studioAdminsPermissionL) LoadIDAdminStudioUser(ctx context.Context, e boil.ContextExecutor, singular bool, maybeStudioAdminsPermission interface{}, mods queries.Applicator) error {
	var slice []*StudioAdminsPermission
	var object *StudioAdminsPermission

	if singular {
		var ok bool
		object, ok = maybeStudioAdminsPermission.(*StudioAdminsPermission)
		if !ok {
			object = new(StudioAdminsPermission)
			ok = queries.SetFromEmbeddedStruct(&object, &maybeStudioAdminsPermission)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", object, maybeStudioAdminsPermission))
			}
		}
	} else {
		s, ok := maybeStudioAdminsPermission.(*[]*StudioAdminsPermission)
		if ok {
			slice = *s
		} else {
			ok = queries.SetFromEmbeddedStruct(&slice, maybeStudioAdminsPermission)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", slice, maybeStudioAdminsPermission))
			}
		}
	}

	args := make(map[interface{}]struct{})
	if singular {
		if object.R == nil {
			object.R = &studioAdminsPermissionR{}
		}
		args[object.IDAdmin] = struct{}{}

	} else {
		for _, obj := range slice {
			if obj.R == nil {
				obj.R = &studioAdminsPermissionR{}
			}

			args[obj.IDAdmin] = struct{}{}

		}
	}

	if len(args) == 0 {
		return nil
	}

	argsSlice := make([]interface{}, len(args))
	i := 0
	for arg := range args {
		argsSlice[i] = arg
		i++
	}

	query := NewQuery(
		qm.From(`studio_users`),
		qm.WhereIn(`studio_users.id in ?`, argsSlice...),
	)
	if mods != nil {
		mods.Apply(query)
	}

	results, err := query.QueryContext(ctx, e)
	if err != nil {
		return errors.Wrap(err, "failed to eager load StudioUser")
	}

	var resultSlice []*StudioUser
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice StudioUser")
	}

	if err = results.Close(); err != nil {
		return errors.Wrap(err, "failed to close results of eager load for studio_users")
	}
	if err = results.Err(); err != nil {
		return errors.Wrap(err, "error occurred during iteration of eager loaded relations for studio_users")
	}

	if len(studioUserAfterSelectHooks) != 0 {
		for _, obj := range resultSlice {
			if err := obj.doAfterSelectHooks(ctx, e); err != nil {
				return err
			}
		}
	}

	if len(resultSlice) == 0 {
		return nil
	}

	if singular {
		foreign := resultSlice[0]
		object.R.IDAdminStudioUser = foreign
		if foreign.R == nil {
			foreign.R = &studioUserR{}
		}
		foreign.R.IDAdminStudioAdminsPermissions = append(foreign.R.IDAdminStudioAdminsPermissions, object)
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.IDAdmin == foreign.ID {
				local.R.IDAdminStudioUser = foreign
				if foreign.R == nil {
					foreign.R = &studioUserR{}
				}
				foreign.R.IDAdminStudioAdminsPermissions = append(foreign.R.IDAdminStudioAdminsPermissions, local)
				break
			}
		}
	}

	return nil
}

// SetIDAdminStudioUser of the studioAdminsPermission to the related item.
// Sets o.R.IDAdminStudioUser to related.
// Adds o to related.R.IDAdminStudioAdminsPermissions.
func (o *StudioAdminsPermission) SetIDAdminStudioUser(ctx context.Context, exec boil.ContextExecutor, insert bool, related *StudioUser) error {
	var err error
	if insert {
		if err = related.Insert(ctx, exec, boil.Infer()); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"studio_admins_permissions\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"id_admin"}),
		strmangle.WhereClause("\"", "\"", 2, studioAdminsPermissionPrimaryKeyColumns),
	)
	values := []interface{}{related.ID, o.ID}

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, updateQuery)
		fmt.Fprintln(writer, values)
	}
	if _, err = exec.ExecContext(ctx, updateQuery, values...); err != nil {
		return errors.Wrap(err, "failed to update local table")
	}

	o.IDAdmin = related.ID
	if o.R == nil {
		o.R = &studioAdminsPermissionR{
			IDAdminStudioUser: related,
		}
	} else {
		o.R.IDAdminStudioUser = related
	}

	if related.R == nil {
		related.R = &studioUserR{
			IDAdminStudioAdminsPermissions: StudioAdminsPermissionSlice{o},
		}
	} else {
		related.R.IDAdminStudioAdminsPermissions = append(related.R.IDAdminStudioAdminsPermissions, o)
	}

	return nil
}

// StudioAdminsPermissions retrieves all the records using an executor.
func StudioAdminsPermissions(mods ...qm.QueryMod) studioAdminsPermissionQuery {
	mods = append(mods, qm.From("\"studio_admins_permissions\""))
	q := NewQuery(mods...)
	if len(queries.GetSelect(q)) == 0 {
		queries.SetSelect(q, []string{"\"studio_admins_permissions\".*"})
	}

	return studioAdminsPermissionQuery{q}
}

// FindStudioAdminsPermission retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindStudioAdminsPermission(ctx context.Context, exec boil.ContextExecutor, iD int64, selectCols ...string) (*StudioAdminsPermission, error) {
	studioAdminsPermissionObj := &StudioAdminsPermission{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"studio_admins_permissions\" where \"id\"=$1", sel,
	)

	q := queries.Raw(query, iD)

	err := q.Bind(ctx, exec, studioAdminsPermissionObj)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from studio_admins_permissions")
	}

	if err = studioAdminsPermissionObj.doAfterSelectHooks(ctx, exec); err != nil {
		return studioAdminsPermissionObj, err
	}

	return studioAdminsPermissionObj, nil
}

// Insert a single record using an executor.
// See boil.Columns.InsertColumnSet documentation to understand column list inference for inserts.
func (o *StudioAdminsPermission) Insert(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) error {
	if o == nil {
		return errors.New("models: no studio_admins_permissions provided for insertion")
	}

	var err error
	if !boil.TimestampsAreSkipped(ctx) {
		currTime := time.Now().In(boil.GetLocation())

		if o.CreatedAt.IsZero() {
			o.CreatedAt = currTime
		}
	}

	if err := o.doBeforeInsertHooks(ctx, exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(studioAdminsPermissionColumnsWithDefault, o)

	key := makeCacheKey(columns, nzDefaults)
	studioAdminsPermissionInsertCacheMut.RLock()
	cache, cached := studioAdminsPermissionInsertCache[key]
	studioAdminsPermissionInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := columns.InsertColumnSet(
			studioAdminsPermissionAllColumns,
			studioAdminsPermissionColumnsWithDefault,
			studioAdminsPermissionColumnsWithoutDefault,
			nzDefaults,
		)

		cache.valueMapping, err = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"studio_admins_permissions\" (\"%s\") %%sVALUES (%s)%%s", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.UseIndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"studio_admins_permissions\" %sDEFAULT VALUES%s"
		}

		var queryOutput, queryReturning string

		if len(cache.retMapping) != 0 {
			queryReturning = fmt.Sprintf(" RETURNING \"%s\"", strings.Join(returnColumns, "\",\""))
		}

		cache.query = fmt.Sprintf(cache.query, queryOutput, queryReturning)
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, cache.query)
		fmt.Fprintln(writer, vals)
	}

	if len(cache.retMapping) != 0 {
		err = exec.QueryRowContext(ctx, cache.query, vals...).Scan(queries.PtrsFromMapping(value, cache.retMapping)...)
	} else {
		_, err = exec.ExecContext(ctx, cache.query, vals...)
	}

	if err != nil {
		return errors.Wrap(err, "models: unable to insert into studio_admins_permissions")
	}

	if !cached {
		studioAdminsPermissionInsertCacheMut.Lock()
		studioAdminsPermissionInsertCache[key] = cache
		studioAdminsPermissionInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(ctx, exec)
}

// Update uses an executor to update the StudioAdminsPermission.
// See boil.Columns.UpdateColumnSet documentation to understand column list inference for updates.
// Update does not automatically update the record in case of default values. Use .Reload() to refresh the records.
func (o *StudioAdminsPermission) Update(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) (int64, error) {
	var err error
	if err = o.doBeforeUpdateHooks(ctx, exec); err != nil {
		return 0, err
	}
	key := makeCacheKey(columns, nil)
	studioAdminsPermissionUpdateCacheMut.RLock()
	cache, cached := studioAdminsPermissionUpdateCache[key]
	studioAdminsPermissionUpdateCacheMut.RUnlock()

	if !cached {
		wl := columns.UpdateColumnSet(
			studioAdminsPermissionAllColumns,
			studioAdminsPermissionPrimaryKeyColumns,
		)

		if !columns.IsWhitelist() {
			wl = strmangle.SetComplement(wl, []string{"created_at"})
		}
		if len(wl) == 0 {
			return 0, errors.New("models: unable to update studio_admins_permissions, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"studio_admins_permissions\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, studioAdminsPermissionPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, append(wl, studioAdminsPermissionPrimaryKeyColumns...))
		if err != nil {
			return 0, err
		}
	}

	values := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), cache.valueMapping)

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, cache.query)
		fmt.Fprintln(writer, values)
	}
	var result sql.Result
	result, err = exec.ExecContext(ctx, cache.query, values...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update studio_admins_permissions row")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by update for studio_admins_permissions")
	}

	if !cached {
		studioAdminsPermissionUpdateCacheMut.Lock()
		studioAdminsPermissionUpdateCache[key] = cache
		studioAdminsPermissionUpdateCacheMut.Unlock()
	}

	return rowsAff, o.doAfterUpdateHooks(ctx, exec)
}

// UpdateAll updates all rows with the specified column values.
func (q studioAdminsPermissionQuery) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
	queries.SetUpdate(q.Query, cols)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all for studio_admins_permissions")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected for studio_admins_permissions")
	}

	return rowsAff, nil
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o StudioAdminsPermissionSlice) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
	ln := int64(len(o))
	if ln == 0 {
		return 0, nil
	}

	if len(cols) == 0 {
		return 0, errors.New("models: update all requires at least one column argument")
	}

	colNames := make([]string, len(cols))
	args := make([]interface{}, len(cols))

	i := 0
	for name, value := range cols {
		colNames[i] = name
		args[i] = value
		i++
	}

	// Append all of the primary key values for each column
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), studioAdminsPermissionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf("UPDATE \"studio_admins_permissions\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), len(colNames)+1, studioAdminsPermissionPrimaryKeyColumns, len(o)))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all in studioAdminsPermission slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected all in update all studioAdminsPermission")
	}
	return rowsAff, nil
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
// See boil.Columns documentation for how to properly use updateColumns and insertColumns.
func (o *StudioAdminsPermission) Upsert(ctx context.Context, exec boil.ContextExecutor, updateOnConflict bool, conflictColumns []string, updateColumns, insertColumns boil.Columns, opts ...UpsertOptionFunc) error {
	if o == nil {
		return errors.New("models: no studio_admins_permissions provided for upsert")
	}
	if !boil.TimestampsAreSkipped(ctx) {
		currTime := time.Now().In(boil.GetLocation())

		if o.CreatedAt.IsZero() {
			o.CreatedAt = currTime
		}
	}

	if err := o.doBeforeUpsertHooks(ctx, exec); err != nil {
		return err
	}

	nzDefaults := queries.NonZeroDefaultSet(studioAdminsPermissionColumnsWithDefault, o)

	// Build cache key in-line uglily - mysql vs psql problems
	buf := strmangle.GetBuffer()
	if updateOnConflict {
		buf.WriteByte('t')
	} else {
		buf.WriteByte('f')
	}
	buf.WriteByte('.')
	for _, c := range conflictColumns {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	buf.WriteString(strconv.Itoa(updateColumns.Kind))
	for _, c := range updateColumns.Cols {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	buf.WriteString(strconv.Itoa(insertColumns.Kind))
	for _, c := range insertColumns.Cols {
		buf.WriteString(c)
	}
	buf.WriteByte('.')
	for _, c := range nzDefaults {
		buf.WriteString(c)
	}
	key := buf.String()
	strmangle.PutBuffer(buf)

	studioAdminsPermissionUpsertCacheMut.RLock()
	cache, cached := studioAdminsPermissionUpsertCache[key]
	studioAdminsPermissionUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		insert, _ := insertColumns.InsertColumnSet(
			studioAdminsPermissionAllColumns,
			studioAdminsPermissionColumnsWithDefault,
			studioAdminsPermissionColumnsWithoutDefault,
			nzDefaults,
		)

		update := updateColumns.UpdateColumnSet(
			studioAdminsPermissionAllColumns,
			studioAdminsPermissionPrimaryKeyColumns,
		)

		if updateOnConflict && len(update) == 0 {
			return errors.New("models: unable to upsert studio_admins_permissions, could not build update column list")
		}

		ret := strmangle.SetComplement(studioAdminsPermissionAllColumns, strmangle.SetIntersect(insert, update))

		conflict := conflictColumns
		if len(conflict) == 0 && updateOnConflict && len(update) != 0 {
			if len(studioAdminsPermissionPrimaryKeyColumns) == 0 {
				return errors.New("models: unable to upsert studio_admins_permissions, could not build conflict column list")
			}

			conflict = make([]string, len(studioAdminsPermissionPrimaryKeyColumns))
			copy(conflict, studioAdminsPermissionPrimaryKeyColumns)
		}
		cache.query = buildUpsertQueryPostgres(dialect, "\"studio_admins_permissions\"", updateOnConflict, ret, update, conflict, insert, opts...)

		cache.valueMapping, err = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, insert)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(studioAdminsPermissionType, studioAdminsPermissionMapping, ret)
			if err != nil {
				return err
			}
		}
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	vals := queries.ValuesFromMapping(value, cache.valueMapping)
	var returns []interface{}
	if len(cache.retMapping) != 0 {
		returns = queries.PtrsFromMapping(value, cache.retMapping)
	}

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, cache.query)
		fmt.Fprintln(writer, vals)
	}
	if len(cache.retMapping) != 0 {
		err = exec.QueryRowContext(ctx, cache.query, vals...).Scan(returns...)
		if errors.Is(err, sql.ErrNoRows) {
			err = nil // Postgres doesn't return anything when there's no update
		}
	} else {
		_, err = exec.ExecContext(ctx, cache.query, vals...)
	}
	if err != nil {
		return errors.Wrap(err, "models: unable to upsert studio_admins_permissions")
	}

	if !cached {
		studioAdminsPermissionUpsertCacheMut.Lock()
		studioAdminsPermissionUpsertCache[key] = cache
		studioAdminsPermissionUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(ctx, exec)
}

// Delete deletes a single StudioAdminsPermission record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *StudioAdminsPermission) Delete(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if o == nil {
		return 0, errors.New("models: no StudioAdminsPermission provided for delete")
	}

	if err := o.doBeforeDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), studioAdminsPermissionPrimaryKeyMapping)
	sql := "DELETE FROM \"studio_admins_permissions\" WHERE \"id\"=$1"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete from studio_admins_permissions")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by delete for studio_admins_permissions")
	}

	if err := o.doAfterDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	return rowsAff, nil
}

// DeleteAll deletes all matching rows.
func (q studioAdminsPermissionQuery) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if q.Query == nil {
		return 0, errors.New("models: no studioAdminsPermissionQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from studio_admins_permissions")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for studio_admins_permissions")
	}

	return rowsAff, nil
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o StudioAdminsPermissionSlice) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if len(o) == 0 {
		return 0, nil
	}

	if len(studioAdminsPermissionBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(ctx, exec); err != nil {
				return 0, err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), studioAdminsPermissionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "DELETE FROM \"studio_admins_permissions\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, studioAdminsPermissionPrimaryKeyColumns, len(o))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from studioAdminsPermission slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for studio_admins_permissions")
	}

	if len(studioAdminsPermissionAfterDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterDeleteHooks(ctx, exec); err != nil {
				return 0, err
			}
		}
	}

	return rowsAff, nil
}

// Reload refetches the object from the database
// using the primary keys with an executor.
func (o *StudioAdminsPermission) Reload(ctx context.Context, exec boil.ContextExecutor) error {
	ret, err := FindStudioAdminsPermission(ctx, exec, o.ID)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *StudioAdminsPermissionSlice) ReloadAll(ctx context.Context, exec boil.ContextExecutor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	slice := StudioAdminsPermissionSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), studioAdminsPermissionPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "SELECT \"studio_admins_permissions\".* FROM \"studio_admins_permissions\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, studioAdminsPermissionPrimaryKeyColumns, len(*o))

	q := queries.Raw(sql, args...)

	err := q.Bind(ctx, exec, &slice)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in StudioAdminsPermissionSlice")
	}

	*o = slice

	return nil
}

// StudioAdminsPermissionExists checks if the StudioAdminsPermission row exists.
func StudioAdminsPermissionExists(ctx context.Context, exec boil.ContextExecutor, iD int64) (bool, error) {
	var exists bool
	sql := "select exists(select 1 from \"studio_admins_permissions\" where \"id\"=$1 limit 1)"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, iD)
	}
	row := exec.QueryRowContext(ctx, sql, iD)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if studio_admins_permissions exists")
	}

	return exists, nil
}

// Exists checks if the StudioAdminsPermission row exists.
func (o *StudioAdminsPermission) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	return StudioAdminsPermissionExists(ctx, exec, o.ID)
}

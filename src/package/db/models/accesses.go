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

// Access is an object representing the database table.
type Access struct {
	ID        int64     `boil:"id" json:"id" toml:"id" yaml:"id"`
	Token     string    `boil:"token" json:"token" toml:"token" yaml:"token"`
	ExpiresAt time.Time `boil:"expires_at" json:"expires_at" toml:"expires_at" yaml:"expires_at"`
	IsRevoked bool      `boil:"is_revoked" json:"is_revoked" toml:"is_revoked" yaml:"is_revoked"`
	IDSession int64     `boil:"id_session" json:"id_session" toml:"id_session" yaml:"id_session"`
	CreatedAt time.Time `boil:"created_at" json:"created_at" toml:"created_at" yaml:"created_at"`

	R *accessR `boil:"-" json:"-" toml:"-" yaml:"-"`
	L accessL  `boil:"-" json:"-" toml:"-" yaml:"-"`
}

var AccessColumns = struct {
	ID        string
	Token     string
	ExpiresAt string
	IsRevoked string
	IDSession string
	CreatedAt string
}{
	ID:        "id",
	Token:     "token",
	ExpiresAt: "expires_at",
	IsRevoked: "is_revoked",
	IDSession: "id_session",
	CreatedAt: "created_at",
}

var AccessTableColumns = struct {
	ID        string
	Token     string
	ExpiresAt string
	IsRevoked string
	IDSession string
	CreatedAt string
}{
	ID:        "accesses.id",
	Token:     "accesses.token",
	ExpiresAt: "accesses.expires_at",
	IsRevoked: "accesses.is_revoked",
	IDSession: "accesses.id_session",
	CreatedAt: "accesses.created_at",
}

// Generated where

type whereHelperint64 struct{ field string }

func (w whereHelperint64) EQ(x int64) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.EQ, x) }
func (w whereHelperint64) NEQ(x int64) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.NEQ, x) }
func (w whereHelperint64) LT(x int64) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.LT, x) }
func (w whereHelperint64) LTE(x int64) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.LTE, x) }
func (w whereHelperint64) GT(x int64) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.GT, x) }
func (w whereHelperint64) GTE(x int64) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.GTE, x) }
func (w whereHelperint64) IN(slice []int64) qm.QueryMod {
	values := make([]interface{}, 0, len(slice))
	for _, value := range slice {
		values = append(values, value)
	}
	return qm.WhereIn(fmt.Sprintf("%s IN ?", w.field), values...)
}
func (w whereHelperint64) NIN(slice []int64) qm.QueryMod {
	values := make([]interface{}, 0, len(slice))
	for _, value := range slice {
		values = append(values, value)
	}
	return qm.WhereNotIn(fmt.Sprintf("%s NOT IN ?", w.field), values...)
}

type whereHelperstring struct{ field string }

func (w whereHelperstring) EQ(x string) qm.QueryMod      { return qmhelper.Where(w.field, qmhelper.EQ, x) }
func (w whereHelperstring) NEQ(x string) qm.QueryMod     { return qmhelper.Where(w.field, qmhelper.NEQ, x) }
func (w whereHelperstring) LT(x string) qm.QueryMod      { return qmhelper.Where(w.field, qmhelper.LT, x) }
func (w whereHelperstring) LTE(x string) qm.QueryMod     { return qmhelper.Where(w.field, qmhelper.LTE, x) }
func (w whereHelperstring) GT(x string) qm.QueryMod      { return qmhelper.Where(w.field, qmhelper.GT, x) }
func (w whereHelperstring) GTE(x string) qm.QueryMod     { return qmhelper.Where(w.field, qmhelper.GTE, x) }
func (w whereHelperstring) LIKE(x string) qm.QueryMod    { return qm.Where(w.field+" LIKE ?", x) }
func (w whereHelperstring) NLIKE(x string) qm.QueryMod   { return qm.Where(w.field+" NOT LIKE ?", x) }
func (w whereHelperstring) ILIKE(x string) qm.QueryMod   { return qm.Where(w.field+" ILIKE ?", x) }
func (w whereHelperstring) NILIKE(x string) qm.QueryMod  { return qm.Where(w.field+" NOT ILIKE ?", x) }
func (w whereHelperstring) SIMILAR(x string) qm.QueryMod { return qm.Where(w.field+" SIMILAR TO ?", x) }
func (w whereHelperstring) NSIMILAR(x string) qm.QueryMod {
	return qm.Where(w.field+" NOT SIMILAR TO ?", x)
}
func (w whereHelperstring) IN(slice []string) qm.QueryMod {
	values := make([]interface{}, 0, len(slice))
	for _, value := range slice {
		values = append(values, value)
	}
	return qm.WhereIn(fmt.Sprintf("%s IN ?", w.field), values...)
}
func (w whereHelperstring) NIN(slice []string) qm.QueryMod {
	values := make([]interface{}, 0, len(slice))
	for _, value := range slice {
		values = append(values, value)
	}
	return qm.WhereNotIn(fmt.Sprintf("%s NOT IN ?", w.field), values...)
}

type whereHelpertime_Time struct{ field string }

func (w whereHelpertime_Time) EQ(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.EQ, x)
}
func (w whereHelpertime_Time) NEQ(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.NEQ, x)
}
func (w whereHelpertime_Time) LT(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.LT, x)
}
func (w whereHelpertime_Time) LTE(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.LTE, x)
}
func (w whereHelpertime_Time) GT(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.GT, x)
}
func (w whereHelpertime_Time) GTE(x time.Time) qm.QueryMod {
	return qmhelper.Where(w.field, qmhelper.GTE, x)
}

type whereHelperbool struct{ field string }

func (w whereHelperbool) EQ(x bool) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.EQ, x) }
func (w whereHelperbool) NEQ(x bool) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.NEQ, x) }
func (w whereHelperbool) LT(x bool) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.LT, x) }
func (w whereHelperbool) LTE(x bool) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.LTE, x) }
func (w whereHelperbool) GT(x bool) qm.QueryMod  { return qmhelper.Where(w.field, qmhelper.GT, x) }
func (w whereHelperbool) GTE(x bool) qm.QueryMod { return qmhelper.Where(w.field, qmhelper.GTE, x) }

var AccessWhere = struct {
	ID        whereHelperint64
	Token     whereHelperstring
	ExpiresAt whereHelpertime_Time
	IsRevoked whereHelperbool
	IDSession whereHelperint64
	CreatedAt whereHelpertime_Time
}{
	ID:        whereHelperint64{field: "\"accesses\".\"id\""},
	Token:     whereHelperstring{field: "\"accesses\".\"token\""},
	ExpiresAt: whereHelpertime_Time{field: "\"accesses\".\"expires_at\""},
	IsRevoked: whereHelperbool{field: "\"accesses\".\"is_revoked\""},
	IDSession: whereHelperint64{field: "\"accesses\".\"id_session\""},
	CreatedAt: whereHelpertime_Time{field: "\"accesses\".\"created_at\""},
}

// AccessRels is where relationship names are stored.
var AccessRels = struct {
	IDSessionSession string
}{
	IDSessionSession: "IDSessionSession",
}

// accessR is where relationships are stored.
type accessR struct {
	IDSessionSession *Session `boil:"IDSessionSession" json:"IDSessionSession" toml:"IDSessionSession" yaml:"IDSessionSession"`
}

// NewStruct creates a new relationship struct
func (*accessR) NewStruct() *accessR {
	return &accessR{}
}

func (o *Access) GetIDSessionSession() *Session {
	if o == nil {
		return nil
	}

	return o.R.GetIDSessionSession()
}

func (r *accessR) GetIDSessionSession() *Session {
	if r == nil {
		return nil
	}

	return r.IDSessionSession
}

// accessL is where Load methods for each relationship are stored.
type accessL struct{}

var (
	accessAllColumns            = []string{"id", "token", "expires_at", "is_revoked", "id_session", "created_at"}
	accessColumnsWithoutDefault = []string{"token", "expires_at", "id_session"}
	accessColumnsWithDefault    = []string{"id", "is_revoked", "created_at"}
	accessPrimaryKeyColumns     = []string{"id"}
	accessGeneratedColumns      = []string{}
)

type (
	// AccessSlice is an alias for a slice of pointers to Access.
	// This should almost always be used instead of []Access.
	AccessSlice []*Access
	// AccessHook is the signature for custom Access hook methods
	AccessHook func(context.Context, boil.ContextExecutor, *Access) error

	accessQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	accessType                 = reflect.TypeOf(&Access{})
	accessMapping              = queries.MakeStructMapping(accessType)
	accessPrimaryKeyMapping, _ = queries.BindMapping(accessType, accessMapping, accessPrimaryKeyColumns)
	accessInsertCacheMut       sync.RWMutex
	accessInsertCache          = make(map[string]insertCache)
	accessUpdateCacheMut       sync.RWMutex
	accessUpdateCache          = make(map[string]updateCache)
	accessUpsertCacheMut       sync.RWMutex
	accessUpsertCache          = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force qmhelper dependency for where clause generation (which doesn't
	// always happen)
	_ = qmhelper.Where
)

var accessAfterSelectMu sync.Mutex
var accessAfterSelectHooks []AccessHook

var accessBeforeInsertMu sync.Mutex
var accessBeforeInsertHooks []AccessHook
var accessAfterInsertMu sync.Mutex
var accessAfterInsertHooks []AccessHook

var accessBeforeUpdateMu sync.Mutex
var accessBeforeUpdateHooks []AccessHook
var accessAfterUpdateMu sync.Mutex
var accessAfterUpdateHooks []AccessHook

var accessBeforeDeleteMu sync.Mutex
var accessBeforeDeleteHooks []AccessHook
var accessAfterDeleteMu sync.Mutex
var accessAfterDeleteHooks []AccessHook

var accessBeforeUpsertMu sync.Mutex
var accessBeforeUpsertHooks []AccessHook
var accessAfterUpsertMu sync.Mutex
var accessAfterUpsertHooks []AccessHook

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Access) doAfterSelectHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessAfterSelectHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Access) doBeforeInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessBeforeInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Access) doAfterInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessAfterInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Access) doBeforeUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessBeforeUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Access) doAfterUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessAfterUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Access) doBeforeDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessBeforeDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Access) doAfterDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessAfterDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Access) doBeforeUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessBeforeUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Access) doAfterUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range accessAfterUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddAccessHook registers your hook function for all future operations.
func AddAccessHook(hookPoint boil.HookPoint, accessHook AccessHook) {
	switch hookPoint {
	case boil.AfterSelectHook:
		accessAfterSelectMu.Lock()
		accessAfterSelectHooks = append(accessAfterSelectHooks, accessHook)
		accessAfterSelectMu.Unlock()
	case boil.BeforeInsertHook:
		accessBeforeInsertMu.Lock()
		accessBeforeInsertHooks = append(accessBeforeInsertHooks, accessHook)
		accessBeforeInsertMu.Unlock()
	case boil.AfterInsertHook:
		accessAfterInsertMu.Lock()
		accessAfterInsertHooks = append(accessAfterInsertHooks, accessHook)
		accessAfterInsertMu.Unlock()
	case boil.BeforeUpdateHook:
		accessBeforeUpdateMu.Lock()
		accessBeforeUpdateHooks = append(accessBeforeUpdateHooks, accessHook)
		accessBeforeUpdateMu.Unlock()
	case boil.AfterUpdateHook:
		accessAfterUpdateMu.Lock()
		accessAfterUpdateHooks = append(accessAfterUpdateHooks, accessHook)
		accessAfterUpdateMu.Unlock()
	case boil.BeforeDeleteHook:
		accessBeforeDeleteMu.Lock()
		accessBeforeDeleteHooks = append(accessBeforeDeleteHooks, accessHook)
		accessBeforeDeleteMu.Unlock()
	case boil.AfterDeleteHook:
		accessAfterDeleteMu.Lock()
		accessAfterDeleteHooks = append(accessAfterDeleteHooks, accessHook)
		accessAfterDeleteMu.Unlock()
	case boil.BeforeUpsertHook:
		accessBeforeUpsertMu.Lock()
		accessBeforeUpsertHooks = append(accessBeforeUpsertHooks, accessHook)
		accessBeforeUpsertMu.Unlock()
	case boil.AfterUpsertHook:
		accessAfterUpsertMu.Lock()
		accessAfterUpsertHooks = append(accessAfterUpsertHooks, accessHook)
		accessAfterUpsertMu.Unlock()
	}
}

// One returns a single access record from the query.
func (q accessQuery) One(ctx context.Context, exec boil.ContextExecutor) (*Access, error) {
	o := &Access{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(ctx, exec, o)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for accesses")
	}

	if err := o.doAfterSelectHooks(ctx, exec); err != nil {
		return o, err
	}

	return o, nil
}

// All returns all Access records from the query.
func (q accessQuery) All(ctx context.Context, exec boil.ContextExecutor) (AccessSlice, error) {
	var o []*Access

	err := q.Bind(ctx, exec, &o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Access slice")
	}

	if len(accessAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(ctx, exec); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// Count returns the count of all Access records in the query.
func (q accessQuery) Count(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count accesses rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table.
func (q accessQuery) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if accesses exists")
	}

	return count > 0, nil
}

// IDSessionSession pointed to by the foreign key.
func (o *Access) IDSessionSession(mods ...qm.QueryMod) sessionQuery {
	queryMods := []qm.QueryMod{
		qm.Where("\"id\" = ?", o.IDSession),
	}

	queryMods = append(queryMods, mods...)

	return Sessions(queryMods...)
}

// LoadIDSessionSession allows an eager lookup of values, cached into the
// loaded structs of the objects. This is for an N-1 relationship.
func (accessL) LoadIDSessionSession(ctx context.Context, e boil.ContextExecutor, singular bool, maybeAccess interface{}, mods queries.Applicator) error {
	var slice []*Access
	var object *Access

	if singular {
		var ok bool
		object, ok = maybeAccess.(*Access)
		if !ok {
			object = new(Access)
			ok = queries.SetFromEmbeddedStruct(&object, &maybeAccess)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", object, maybeAccess))
			}
		}
	} else {
		s, ok := maybeAccess.(*[]*Access)
		if ok {
			slice = *s
		} else {
			ok = queries.SetFromEmbeddedStruct(&slice, maybeAccess)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", slice, maybeAccess))
			}
		}
	}

	args := make(map[interface{}]struct{})
	if singular {
		if object.R == nil {
			object.R = &accessR{}
		}
		args[object.IDSession] = struct{}{}

	} else {
		for _, obj := range slice {
			if obj.R == nil {
				obj.R = &accessR{}
			}

			args[obj.IDSession] = struct{}{}

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
		qm.From(`sessions`),
		qm.WhereIn(`sessions.id in ?`, argsSlice...),
	)
	if mods != nil {
		mods.Apply(query)
	}

	results, err := query.QueryContext(ctx, e)
	if err != nil {
		return errors.Wrap(err, "failed to eager load Session")
	}

	var resultSlice []*Session
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice Session")
	}

	if err = results.Close(); err != nil {
		return errors.Wrap(err, "failed to close results of eager load for sessions")
	}
	if err = results.Err(); err != nil {
		return errors.Wrap(err, "error occurred during iteration of eager loaded relations for sessions")
	}

	if len(sessionAfterSelectHooks) != 0 {
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
		object.R.IDSessionSession = foreign
		if foreign.R == nil {
			foreign.R = &sessionR{}
		}
		foreign.R.IDSessionAccesses = append(foreign.R.IDSessionAccesses, object)
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.IDSession == foreign.ID {
				local.R.IDSessionSession = foreign
				if foreign.R == nil {
					foreign.R = &sessionR{}
				}
				foreign.R.IDSessionAccesses = append(foreign.R.IDSessionAccesses, local)
				break
			}
		}
	}

	return nil
}

// SetIDSessionSession of the access to the related item.
// Sets o.R.IDSessionSession to related.
// Adds o to related.R.IDSessionAccesses.
func (o *Access) SetIDSessionSession(ctx context.Context, exec boil.ContextExecutor, insert bool, related *Session) error {
	var err error
	if insert {
		if err = related.Insert(ctx, exec, boil.Infer()); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"accesses\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"id_session"}),
		strmangle.WhereClause("\"", "\"", 2, accessPrimaryKeyColumns),
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

	o.IDSession = related.ID
	if o.R == nil {
		o.R = &accessR{
			IDSessionSession: related,
		}
	} else {
		o.R.IDSessionSession = related
	}

	if related.R == nil {
		related.R = &sessionR{
			IDSessionAccesses: AccessSlice{o},
		}
	} else {
		related.R.IDSessionAccesses = append(related.R.IDSessionAccesses, o)
	}

	return nil
}

// Accesses retrieves all the records using an executor.
func Accesses(mods ...qm.QueryMod) accessQuery {
	mods = append(mods, qm.From("\"accesses\""))
	q := NewQuery(mods...)
	if len(queries.GetSelect(q)) == 0 {
		queries.SetSelect(q, []string{"\"accesses\".*"})
	}

	return accessQuery{q}
}

// FindAccess retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindAccess(ctx context.Context, exec boil.ContextExecutor, iD int64, selectCols ...string) (*Access, error) {
	accessObj := &Access{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"accesses\" where \"id\"=$1", sel,
	)

	q := queries.Raw(query, iD)

	err := q.Bind(ctx, exec, accessObj)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from accesses")
	}

	if err = accessObj.doAfterSelectHooks(ctx, exec); err != nil {
		return accessObj, err
	}

	return accessObj, nil
}

// Insert a single record using an executor.
// See boil.Columns.InsertColumnSet documentation to understand column list inference for inserts.
func (o *Access) Insert(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) error {
	if o == nil {
		return errors.New("models: no accesses provided for insertion")
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

	nzDefaults := queries.NonZeroDefaultSet(accessColumnsWithDefault, o)

	key := makeCacheKey(columns, nzDefaults)
	accessInsertCacheMut.RLock()
	cache, cached := accessInsertCache[key]
	accessInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := columns.InsertColumnSet(
			accessAllColumns,
			accessColumnsWithDefault,
			accessColumnsWithoutDefault,
			nzDefaults,
		)

		cache.valueMapping, err = queries.BindMapping(accessType, accessMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(accessType, accessMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"accesses\" (\"%s\") %%sVALUES (%s)%%s", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.UseIndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"accesses\" %sDEFAULT VALUES%s"
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
		return errors.Wrap(err, "models: unable to insert into accesses")
	}

	if !cached {
		accessInsertCacheMut.Lock()
		accessInsertCache[key] = cache
		accessInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(ctx, exec)
}

// Update uses an executor to update the Access.
// See boil.Columns.UpdateColumnSet documentation to understand column list inference for updates.
// Update does not automatically update the record in case of default values. Use .Reload() to refresh the records.
func (o *Access) Update(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) (int64, error) {
	var err error
	if err = o.doBeforeUpdateHooks(ctx, exec); err != nil {
		return 0, err
	}
	key := makeCacheKey(columns, nil)
	accessUpdateCacheMut.RLock()
	cache, cached := accessUpdateCache[key]
	accessUpdateCacheMut.RUnlock()

	if !cached {
		wl := columns.UpdateColumnSet(
			accessAllColumns,
			accessPrimaryKeyColumns,
		)

		if !columns.IsWhitelist() {
			wl = strmangle.SetComplement(wl, []string{"created_at"})
		}
		if len(wl) == 0 {
			return 0, errors.New("models: unable to update accesses, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"accesses\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, accessPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(accessType, accessMapping, append(wl, accessPrimaryKeyColumns...))
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
		return 0, errors.Wrap(err, "models: unable to update accesses row")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by update for accesses")
	}

	if !cached {
		accessUpdateCacheMut.Lock()
		accessUpdateCache[key] = cache
		accessUpdateCacheMut.Unlock()
	}

	return rowsAff, o.doAfterUpdateHooks(ctx, exec)
}

// UpdateAll updates all rows with the specified column values.
func (q accessQuery) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
	queries.SetUpdate(q.Query, cols)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all for accesses")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected for accesses")
	}

	return rowsAff, nil
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o AccessSlice) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), accessPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf("UPDATE \"accesses\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), len(colNames)+1, accessPrimaryKeyColumns, len(o)))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all in access slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected all in update all access")
	}
	return rowsAff, nil
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
// See boil.Columns documentation for how to properly use updateColumns and insertColumns.
func (o *Access) Upsert(ctx context.Context, exec boil.ContextExecutor, updateOnConflict bool, conflictColumns []string, updateColumns, insertColumns boil.Columns, opts ...UpsertOptionFunc) error {
	if o == nil {
		return errors.New("models: no accesses provided for upsert")
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

	nzDefaults := queries.NonZeroDefaultSet(accessColumnsWithDefault, o)

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

	accessUpsertCacheMut.RLock()
	cache, cached := accessUpsertCache[key]
	accessUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		insert, _ := insertColumns.InsertColumnSet(
			accessAllColumns,
			accessColumnsWithDefault,
			accessColumnsWithoutDefault,
			nzDefaults,
		)

		update := updateColumns.UpdateColumnSet(
			accessAllColumns,
			accessPrimaryKeyColumns,
		)

		if updateOnConflict && len(update) == 0 {
			return errors.New("models: unable to upsert accesses, could not build update column list")
		}

		ret := strmangle.SetComplement(accessAllColumns, strmangle.SetIntersect(insert, update))

		conflict := conflictColumns
		if len(conflict) == 0 && updateOnConflict && len(update) != 0 {
			if len(accessPrimaryKeyColumns) == 0 {
				return errors.New("models: unable to upsert accesses, could not build conflict column list")
			}

			conflict = make([]string, len(accessPrimaryKeyColumns))
			copy(conflict, accessPrimaryKeyColumns)
		}
		cache.query = buildUpsertQueryPostgres(dialect, "\"accesses\"", updateOnConflict, ret, update, conflict, insert, opts...)

		cache.valueMapping, err = queries.BindMapping(accessType, accessMapping, insert)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(accessType, accessMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert accesses")
	}

	if !cached {
		accessUpsertCacheMut.Lock()
		accessUpsertCache[key] = cache
		accessUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(ctx, exec)
}

// Delete deletes a single Access record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Access) Delete(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if o == nil {
		return 0, errors.New("models: no Access provided for delete")
	}

	if err := o.doBeforeDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), accessPrimaryKeyMapping)
	sql := "DELETE FROM \"accesses\" WHERE \"id\"=$1"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete from accesses")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by delete for accesses")
	}

	if err := o.doAfterDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	return rowsAff, nil
}

// DeleteAll deletes all matching rows.
func (q accessQuery) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if q.Query == nil {
		return 0, errors.New("models: no accessQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from accesses")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for accesses")
	}

	return rowsAff, nil
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o AccessSlice) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if len(o) == 0 {
		return 0, nil
	}

	if len(accessBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(ctx, exec); err != nil {
				return 0, err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), accessPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "DELETE FROM \"accesses\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, accessPrimaryKeyColumns, len(o))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from access slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for accesses")
	}

	if len(accessAfterDeleteHooks) != 0 {
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
func (o *Access) Reload(ctx context.Context, exec boil.ContextExecutor) error {
	ret, err := FindAccess(ctx, exec, o.ID)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *AccessSlice) ReloadAll(ctx context.Context, exec boil.ContextExecutor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	slice := AccessSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), accessPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "SELECT \"accesses\".* FROM \"accesses\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, accessPrimaryKeyColumns, len(*o))

	q := queries.Raw(sql, args...)

	err := q.Bind(ctx, exec, &slice)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in AccessSlice")
	}

	*o = slice

	return nil
}

// AccessExists checks if the Access row exists.
func AccessExists(ctx context.Context, exec boil.ContextExecutor, iD int64) (bool, error) {
	var exists bool
	sql := "select exists(select 1 from \"accesses\" where \"id\"=$1 limit 1)"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, iD)
	}
	row := exec.QueryRowContext(ctx, sql, iD)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if accesses exists")
	}

	return exists, nil
}

// Exists checks if the Access row exists.
func (o *Access) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	return AccessExists(ctx, exec, o.ID)
}

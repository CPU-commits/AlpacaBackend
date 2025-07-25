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

// Token is an object representing the database table.
type Token struct {
	ID        int64     `boil:"id" json:"id" toml:"id" yaml:"id"`
	Token     string    `boil:"token" json:"token" toml:"token" yaml:"token"`
	IDUser    int64     `boil:"id_user" json:"id_user" toml:"id_user" yaml:"id_user"`
	IsActive  bool      `boil:"is_active" json:"is_active" toml:"is_active" yaml:"is_active"`
	CreatedAt time.Time `boil:"created_at" json:"created_at" toml:"created_at" yaml:"created_at"`
	ExpiresAt time.Time `boil:"expires_at" json:"expires_at" toml:"expires_at" yaml:"expires_at"`

	R *tokenR `boil:"-" json:"-" toml:"-" yaml:"-"`
	L tokenL  `boil:"-" json:"-" toml:"-" yaml:"-"`
}

var TokenColumns = struct {
	ID        string
	Token     string
	IDUser    string
	IsActive  string
	CreatedAt string
	ExpiresAt string
}{
	ID:        "id",
	Token:     "token",
	IDUser:    "id_user",
	IsActive:  "is_active",
	CreatedAt: "created_at",
	ExpiresAt: "expires_at",
}

var TokenTableColumns = struct {
	ID        string
	Token     string
	IDUser    string
	IsActive  string
	CreatedAt string
	ExpiresAt string
}{
	ID:        "tokens.id",
	Token:     "tokens.token",
	IDUser:    "tokens.id_user",
	IsActive:  "tokens.is_active",
	CreatedAt: "tokens.created_at",
	ExpiresAt: "tokens.expires_at",
}

// Generated where

var TokenWhere = struct {
	ID        whereHelperint64
	Token     whereHelperstring
	IDUser    whereHelperint64
	IsActive  whereHelperbool
	CreatedAt whereHelpertime_Time
	ExpiresAt whereHelpertime_Time
}{
	ID:        whereHelperint64{field: "\"tokens\".\"id\""},
	Token:     whereHelperstring{field: "\"tokens\".\"token\""},
	IDUser:    whereHelperint64{field: "\"tokens\".\"id_user\""},
	IsActive:  whereHelperbool{field: "\"tokens\".\"is_active\""},
	CreatedAt: whereHelpertime_Time{field: "\"tokens\".\"created_at\""},
	ExpiresAt: whereHelpertime_Time{field: "\"tokens\".\"expires_at\""},
}

// TokenRels is where relationship names are stored.
var TokenRels = struct {
	IDUserUser string
}{
	IDUserUser: "IDUserUser",
}

// tokenR is where relationships are stored.
type tokenR struct {
	IDUserUser *User `boil:"IDUserUser" json:"IDUserUser" toml:"IDUserUser" yaml:"IDUserUser"`
}

// NewStruct creates a new relationship struct
func (*tokenR) NewStruct() *tokenR {
	return &tokenR{}
}

func (o *Token) GetIDUserUser() *User {
	if o == nil {
		return nil
	}

	return o.R.GetIDUserUser()
}

func (r *tokenR) GetIDUserUser() *User {
	if r == nil {
		return nil
	}

	return r.IDUserUser
}

// tokenL is where Load methods for each relationship are stored.
type tokenL struct{}

var (
	tokenAllColumns            = []string{"id", "token", "id_user", "is_active", "created_at", "expires_at"}
	tokenColumnsWithoutDefault = []string{"token", "id_user", "expires_at"}
	tokenColumnsWithDefault    = []string{"id", "is_active", "created_at"}
	tokenPrimaryKeyColumns     = []string{"id"}
	tokenGeneratedColumns      = []string{}
)

type (
	// TokenSlice is an alias for a slice of pointers to Token.
	// This should almost always be used instead of []Token.
	TokenSlice []*Token
	// TokenHook is the signature for custom Token hook methods
	TokenHook func(context.Context, boil.ContextExecutor, *Token) error

	tokenQuery struct {
		*queries.Query
	}
)

// Cache for insert, update and upsert
var (
	tokenType                 = reflect.TypeOf(&Token{})
	tokenMapping              = queries.MakeStructMapping(tokenType)
	tokenPrimaryKeyMapping, _ = queries.BindMapping(tokenType, tokenMapping, tokenPrimaryKeyColumns)
	tokenInsertCacheMut       sync.RWMutex
	tokenInsertCache          = make(map[string]insertCache)
	tokenUpdateCacheMut       sync.RWMutex
	tokenUpdateCache          = make(map[string]updateCache)
	tokenUpsertCacheMut       sync.RWMutex
	tokenUpsertCache          = make(map[string]insertCache)
)

var (
	// Force time package dependency for automated UpdatedAt/CreatedAt.
	_ = time.Second
	// Force qmhelper dependency for where clause generation (which doesn't
	// always happen)
	_ = qmhelper.Where
)

var tokenAfterSelectMu sync.Mutex
var tokenAfterSelectHooks []TokenHook

var tokenBeforeInsertMu sync.Mutex
var tokenBeforeInsertHooks []TokenHook
var tokenAfterInsertMu sync.Mutex
var tokenAfterInsertHooks []TokenHook

var tokenBeforeUpdateMu sync.Mutex
var tokenBeforeUpdateHooks []TokenHook
var tokenAfterUpdateMu sync.Mutex
var tokenAfterUpdateHooks []TokenHook

var tokenBeforeDeleteMu sync.Mutex
var tokenBeforeDeleteHooks []TokenHook
var tokenAfterDeleteMu sync.Mutex
var tokenAfterDeleteHooks []TokenHook

var tokenBeforeUpsertMu sync.Mutex
var tokenBeforeUpsertHooks []TokenHook
var tokenAfterUpsertMu sync.Mutex
var tokenAfterUpsertHooks []TokenHook

// doAfterSelectHooks executes all "after Select" hooks.
func (o *Token) doAfterSelectHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenAfterSelectHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeInsertHooks executes all "before insert" hooks.
func (o *Token) doBeforeInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenBeforeInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterInsertHooks executes all "after Insert" hooks.
func (o *Token) doAfterInsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenAfterInsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpdateHooks executes all "before Update" hooks.
func (o *Token) doBeforeUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenBeforeUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpdateHooks executes all "after Update" hooks.
func (o *Token) doAfterUpdateHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenAfterUpdateHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeDeleteHooks executes all "before Delete" hooks.
func (o *Token) doBeforeDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenBeforeDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterDeleteHooks executes all "after Delete" hooks.
func (o *Token) doAfterDeleteHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenAfterDeleteHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doBeforeUpsertHooks executes all "before Upsert" hooks.
func (o *Token) doBeforeUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenBeforeUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// doAfterUpsertHooks executes all "after Upsert" hooks.
func (o *Token) doAfterUpsertHooks(ctx context.Context, exec boil.ContextExecutor) (err error) {
	if boil.HooksAreSkipped(ctx) {
		return nil
	}

	for _, hook := range tokenAfterUpsertHooks {
		if err := hook(ctx, exec, o); err != nil {
			return err
		}
	}

	return nil
}

// AddTokenHook registers your hook function for all future operations.
func AddTokenHook(hookPoint boil.HookPoint, tokenHook TokenHook) {
	switch hookPoint {
	case boil.AfterSelectHook:
		tokenAfterSelectMu.Lock()
		tokenAfterSelectHooks = append(tokenAfterSelectHooks, tokenHook)
		tokenAfterSelectMu.Unlock()
	case boil.BeforeInsertHook:
		tokenBeforeInsertMu.Lock()
		tokenBeforeInsertHooks = append(tokenBeforeInsertHooks, tokenHook)
		tokenBeforeInsertMu.Unlock()
	case boil.AfterInsertHook:
		tokenAfterInsertMu.Lock()
		tokenAfterInsertHooks = append(tokenAfterInsertHooks, tokenHook)
		tokenAfterInsertMu.Unlock()
	case boil.BeforeUpdateHook:
		tokenBeforeUpdateMu.Lock()
		tokenBeforeUpdateHooks = append(tokenBeforeUpdateHooks, tokenHook)
		tokenBeforeUpdateMu.Unlock()
	case boil.AfterUpdateHook:
		tokenAfterUpdateMu.Lock()
		tokenAfterUpdateHooks = append(tokenAfterUpdateHooks, tokenHook)
		tokenAfterUpdateMu.Unlock()
	case boil.BeforeDeleteHook:
		tokenBeforeDeleteMu.Lock()
		tokenBeforeDeleteHooks = append(tokenBeforeDeleteHooks, tokenHook)
		tokenBeforeDeleteMu.Unlock()
	case boil.AfterDeleteHook:
		tokenAfterDeleteMu.Lock()
		tokenAfterDeleteHooks = append(tokenAfterDeleteHooks, tokenHook)
		tokenAfterDeleteMu.Unlock()
	case boil.BeforeUpsertHook:
		tokenBeforeUpsertMu.Lock()
		tokenBeforeUpsertHooks = append(tokenBeforeUpsertHooks, tokenHook)
		tokenBeforeUpsertMu.Unlock()
	case boil.AfterUpsertHook:
		tokenAfterUpsertMu.Lock()
		tokenAfterUpsertHooks = append(tokenAfterUpsertHooks, tokenHook)
		tokenAfterUpsertMu.Unlock()
	}
}

// One returns a single token record from the query.
func (q tokenQuery) One(ctx context.Context, exec boil.ContextExecutor) (*Token, error) {
	o := &Token{}

	queries.SetLimit(q.Query, 1)

	err := q.Bind(ctx, exec, o)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: failed to execute a one query for tokens")
	}

	if err := o.doAfterSelectHooks(ctx, exec); err != nil {
		return o, err
	}

	return o, nil
}

// All returns all Token records from the query.
func (q tokenQuery) All(ctx context.Context, exec boil.ContextExecutor) (TokenSlice, error) {
	var o []*Token

	err := q.Bind(ctx, exec, &o)
	if err != nil {
		return nil, errors.Wrap(err, "models: failed to assign all query results to Token slice")
	}

	if len(tokenAfterSelectHooks) != 0 {
		for _, obj := range o {
			if err := obj.doAfterSelectHooks(ctx, exec); err != nil {
				return o, err
			}
		}
	}

	return o, nil
}

// Count returns the count of all Token records in the query.
func (q tokenQuery) Count(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to count tokens rows")
	}

	return count, nil
}

// Exists checks if the row exists in the table.
func (q tokenQuery) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	var count int64

	queries.SetSelect(q.Query, nil)
	queries.SetCount(q.Query)
	queries.SetLimit(q.Query, 1)

	err := q.Query.QueryRowContext(ctx, exec).Scan(&count)
	if err != nil {
		return false, errors.Wrap(err, "models: failed to check if tokens exists")
	}

	return count > 0, nil
}

// IDUserUser pointed to by the foreign key.
func (o *Token) IDUserUser(mods ...qm.QueryMod) userQuery {
	queryMods := []qm.QueryMod{
		qm.Where("\"id\" = ?", o.IDUser),
	}

	queryMods = append(queryMods, mods...)

	return Users(queryMods...)
}

// LoadIDUserUser allows an eager lookup of values, cached into the
// loaded structs of the objects. This is for an N-1 relationship.
func (tokenL) LoadIDUserUser(ctx context.Context, e boil.ContextExecutor, singular bool, maybeToken interface{}, mods queries.Applicator) error {
	var slice []*Token
	var object *Token

	if singular {
		var ok bool
		object, ok = maybeToken.(*Token)
		if !ok {
			object = new(Token)
			ok = queries.SetFromEmbeddedStruct(&object, &maybeToken)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", object, maybeToken))
			}
		}
	} else {
		s, ok := maybeToken.(*[]*Token)
		if ok {
			slice = *s
		} else {
			ok = queries.SetFromEmbeddedStruct(&slice, maybeToken)
			if !ok {
				return errors.New(fmt.Sprintf("failed to set %T from embedded struct %T", slice, maybeToken))
			}
		}
	}

	args := make(map[interface{}]struct{})
	if singular {
		if object.R == nil {
			object.R = &tokenR{}
		}
		args[object.IDUser] = struct{}{}

	} else {
		for _, obj := range slice {
			if obj.R == nil {
				obj.R = &tokenR{}
			}

			args[obj.IDUser] = struct{}{}

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
		qm.From(`users`),
		qm.WhereIn(`users.id in ?`, argsSlice...),
	)
	if mods != nil {
		mods.Apply(query)
	}

	results, err := query.QueryContext(ctx, e)
	if err != nil {
		return errors.Wrap(err, "failed to eager load User")
	}

	var resultSlice []*User
	if err = queries.Bind(results, &resultSlice); err != nil {
		return errors.Wrap(err, "failed to bind eager loaded slice User")
	}

	if err = results.Close(); err != nil {
		return errors.Wrap(err, "failed to close results of eager load for users")
	}
	if err = results.Err(); err != nil {
		return errors.Wrap(err, "error occurred during iteration of eager loaded relations for users")
	}

	if len(userAfterSelectHooks) != 0 {
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
		object.R.IDUserUser = foreign
		if foreign.R == nil {
			foreign.R = &userR{}
		}
		foreign.R.IDUserTokens = append(foreign.R.IDUserTokens, object)
		return nil
	}

	for _, local := range slice {
		for _, foreign := range resultSlice {
			if local.IDUser == foreign.ID {
				local.R.IDUserUser = foreign
				if foreign.R == nil {
					foreign.R = &userR{}
				}
				foreign.R.IDUserTokens = append(foreign.R.IDUserTokens, local)
				break
			}
		}
	}

	return nil
}

// SetIDUserUser of the token to the related item.
// Sets o.R.IDUserUser to related.
// Adds o to related.R.IDUserTokens.
func (o *Token) SetIDUserUser(ctx context.Context, exec boil.ContextExecutor, insert bool, related *User) error {
	var err error
	if insert {
		if err = related.Insert(ctx, exec, boil.Infer()); err != nil {
			return errors.Wrap(err, "failed to insert into foreign table")
		}
	}

	updateQuery := fmt.Sprintf(
		"UPDATE \"tokens\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, []string{"id_user"}),
		strmangle.WhereClause("\"", "\"", 2, tokenPrimaryKeyColumns),
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

	o.IDUser = related.ID
	if o.R == nil {
		o.R = &tokenR{
			IDUserUser: related,
		}
	} else {
		o.R.IDUserUser = related
	}

	if related.R == nil {
		related.R = &userR{
			IDUserTokens: TokenSlice{o},
		}
	} else {
		related.R.IDUserTokens = append(related.R.IDUserTokens, o)
	}

	return nil
}

// Tokens retrieves all the records using an executor.
func Tokens(mods ...qm.QueryMod) tokenQuery {
	mods = append(mods, qm.From("\"tokens\""))
	q := NewQuery(mods...)
	if len(queries.GetSelect(q)) == 0 {
		queries.SetSelect(q, []string{"\"tokens\".*"})
	}

	return tokenQuery{q}
}

// FindToken retrieves a single record by ID with an executor.
// If selectCols is empty Find will return all columns.
func FindToken(ctx context.Context, exec boil.ContextExecutor, iD int64, selectCols ...string) (*Token, error) {
	tokenObj := &Token{}

	sel := "*"
	if len(selectCols) > 0 {
		sel = strings.Join(strmangle.IdentQuoteSlice(dialect.LQ, dialect.RQ, selectCols), ",")
	}
	query := fmt.Sprintf(
		"select %s from \"tokens\" where \"id\"=$1", sel,
	)

	q := queries.Raw(query, iD)

	err := q.Bind(ctx, exec, tokenObj)
	if err != nil {
		if errors.Is(err, sql.ErrNoRows) {
			return nil, sql.ErrNoRows
		}
		return nil, errors.Wrap(err, "models: unable to select from tokens")
	}

	if err = tokenObj.doAfterSelectHooks(ctx, exec); err != nil {
		return tokenObj, err
	}

	return tokenObj, nil
}

// Insert a single record using an executor.
// See boil.Columns.InsertColumnSet documentation to understand column list inference for inserts.
func (o *Token) Insert(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) error {
	if o == nil {
		return errors.New("models: no tokens provided for insertion")
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

	nzDefaults := queries.NonZeroDefaultSet(tokenColumnsWithDefault, o)

	key := makeCacheKey(columns, nzDefaults)
	tokenInsertCacheMut.RLock()
	cache, cached := tokenInsertCache[key]
	tokenInsertCacheMut.RUnlock()

	if !cached {
		wl, returnColumns := columns.InsertColumnSet(
			tokenAllColumns,
			tokenColumnsWithDefault,
			tokenColumnsWithoutDefault,
			nzDefaults,
		)

		cache.valueMapping, err = queries.BindMapping(tokenType, tokenMapping, wl)
		if err != nil {
			return err
		}
		cache.retMapping, err = queries.BindMapping(tokenType, tokenMapping, returnColumns)
		if err != nil {
			return err
		}
		if len(wl) != 0 {
			cache.query = fmt.Sprintf("INSERT INTO \"tokens\" (\"%s\") %%sVALUES (%s)%%s", strings.Join(wl, "\",\""), strmangle.Placeholders(dialect.UseIndexPlaceholders, len(wl), 1, 1))
		} else {
			cache.query = "INSERT INTO \"tokens\" %sDEFAULT VALUES%s"
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
		return errors.Wrap(err, "models: unable to insert into tokens")
	}

	if !cached {
		tokenInsertCacheMut.Lock()
		tokenInsertCache[key] = cache
		tokenInsertCacheMut.Unlock()
	}

	return o.doAfterInsertHooks(ctx, exec)
}

// Update uses an executor to update the Token.
// See boil.Columns.UpdateColumnSet documentation to understand column list inference for updates.
// Update does not automatically update the record in case of default values. Use .Reload() to refresh the records.
func (o *Token) Update(ctx context.Context, exec boil.ContextExecutor, columns boil.Columns) (int64, error) {
	var err error
	if err = o.doBeforeUpdateHooks(ctx, exec); err != nil {
		return 0, err
	}
	key := makeCacheKey(columns, nil)
	tokenUpdateCacheMut.RLock()
	cache, cached := tokenUpdateCache[key]
	tokenUpdateCacheMut.RUnlock()

	if !cached {
		wl := columns.UpdateColumnSet(
			tokenAllColumns,
			tokenPrimaryKeyColumns,
		)

		if !columns.IsWhitelist() {
			wl = strmangle.SetComplement(wl, []string{"created_at"})
		}
		if len(wl) == 0 {
			return 0, errors.New("models: unable to update tokens, could not build whitelist")
		}

		cache.query = fmt.Sprintf("UPDATE \"tokens\" SET %s WHERE %s",
			strmangle.SetParamNames("\"", "\"", 1, wl),
			strmangle.WhereClause("\"", "\"", len(wl)+1, tokenPrimaryKeyColumns),
		)
		cache.valueMapping, err = queries.BindMapping(tokenType, tokenMapping, append(wl, tokenPrimaryKeyColumns...))
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
		return 0, errors.Wrap(err, "models: unable to update tokens row")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by update for tokens")
	}

	if !cached {
		tokenUpdateCacheMut.Lock()
		tokenUpdateCache[key] = cache
		tokenUpdateCacheMut.Unlock()
	}

	return rowsAff, o.doAfterUpdateHooks(ctx, exec)
}

// UpdateAll updates all rows with the specified column values.
func (q tokenQuery) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
	queries.SetUpdate(q.Query, cols)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all for tokens")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected for tokens")
	}

	return rowsAff, nil
}

// UpdateAll updates all rows with the specified column values, using an executor.
func (o TokenSlice) UpdateAll(ctx context.Context, exec boil.ContextExecutor, cols M) (int64, error) {
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
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), tokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := fmt.Sprintf("UPDATE \"tokens\" SET %s WHERE %s",
		strmangle.SetParamNames("\"", "\"", 1, colNames),
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), len(colNames)+1, tokenPrimaryKeyColumns, len(o)))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to update all in token slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to retrieve rows affected all in update all token")
	}
	return rowsAff, nil
}

// Upsert attempts an insert using an executor, and does an update or ignore on conflict.
// See boil.Columns documentation for how to properly use updateColumns and insertColumns.
func (o *Token) Upsert(ctx context.Context, exec boil.ContextExecutor, updateOnConflict bool, conflictColumns []string, updateColumns, insertColumns boil.Columns, opts ...UpsertOptionFunc) error {
	if o == nil {
		return errors.New("models: no tokens provided for upsert")
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

	nzDefaults := queries.NonZeroDefaultSet(tokenColumnsWithDefault, o)

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

	tokenUpsertCacheMut.RLock()
	cache, cached := tokenUpsertCache[key]
	tokenUpsertCacheMut.RUnlock()

	var err error

	if !cached {
		insert, _ := insertColumns.InsertColumnSet(
			tokenAllColumns,
			tokenColumnsWithDefault,
			tokenColumnsWithoutDefault,
			nzDefaults,
		)

		update := updateColumns.UpdateColumnSet(
			tokenAllColumns,
			tokenPrimaryKeyColumns,
		)

		if updateOnConflict && len(update) == 0 {
			return errors.New("models: unable to upsert tokens, could not build update column list")
		}

		ret := strmangle.SetComplement(tokenAllColumns, strmangle.SetIntersect(insert, update))

		conflict := conflictColumns
		if len(conflict) == 0 && updateOnConflict && len(update) != 0 {
			if len(tokenPrimaryKeyColumns) == 0 {
				return errors.New("models: unable to upsert tokens, could not build conflict column list")
			}

			conflict = make([]string, len(tokenPrimaryKeyColumns))
			copy(conflict, tokenPrimaryKeyColumns)
		}
		cache.query = buildUpsertQueryPostgres(dialect, "\"tokens\"", updateOnConflict, ret, update, conflict, insert, opts...)

		cache.valueMapping, err = queries.BindMapping(tokenType, tokenMapping, insert)
		if err != nil {
			return err
		}
		if len(ret) != 0 {
			cache.retMapping, err = queries.BindMapping(tokenType, tokenMapping, ret)
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
		return errors.Wrap(err, "models: unable to upsert tokens")
	}

	if !cached {
		tokenUpsertCacheMut.Lock()
		tokenUpsertCache[key] = cache
		tokenUpsertCacheMut.Unlock()
	}

	return o.doAfterUpsertHooks(ctx, exec)
}

// Delete deletes a single Token record with an executor.
// Delete will match against the primary key column to find the record to delete.
func (o *Token) Delete(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if o == nil {
		return 0, errors.New("models: no Token provided for delete")
	}

	if err := o.doBeforeDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	args := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(o)), tokenPrimaryKeyMapping)
	sql := "DELETE FROM \"tokens\" WHERE \"id\"=$1"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args...)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete from tokens")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by delete for tokens")
	}

	if err := o.doAfterDeleteHooks(ctx, exec); err != nil {
		return 0, err
	}

	return rowsAff, nil
}

// DeleteAll deletes all matching rows.
func (q tokenQuery) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if q.Query == nil {
		return 0, errors.New("models: no tokenQuery provided for delete all")
	}

	queries.SetDelete(q.Query)

	result, err := q.Query.ExecContext(ctx, exec)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from tokens")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for tokens")
	}

	return rowsAff, nil
}

// DeleteAll deletes all rows in the slice, using an executor.
func (o TokenSlice) DeleteAll(ctx context.Context, exec boil.ContextExecutor) (int64, error) {
	if len(o) == 0 {
		return 0, nil
	}

	if len(tokenBeforeDeleteHooks) != 0 {
		for _, obj := range o {
			if err := obj.doBeforeDeleteHooks(ctx, exec); err != nil {
				return 0, err
			}
		}
	}

	var args []interface{}
	for _, obj := range o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), tokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "DELETE FROM \"tokens\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, tokenPrimaryKeyColumns, len(o))

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, args)
	}
	result, err := exec.ExecContext(ctx, sql, args...)
	if err != nil {
		return 0, errors.Wrap(err, "models: unable to delete all from token slice")
	}

	rowsAff, err := result.RowsAffected()
	if err != nil {
		return 0, errors.Wrap(err, "models: failed to get rows affected by deleteall for tokens")
	}

	if len(tokenAfterDeleteHooks) != 0 {
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
func (o *Token) Reload(ctx context.Context, exec boil.ContextExecutor) error {
	ret, err := FindToken(ctx, exec, o.ID)
	if err != nil {
		return err
	}

	*o = *ret
	return nil
}

// ReloadAll refetches every row with matching primary key column values
// and overwrites the original object slice with the newly updated slice.
func (o *TokenSlice) ReloadAll(ctx context.Context, exec boil.ContextExecutor) error {
	if o == nil || len(*o) == 0 {
		return nil
	}

	slice := TokenSlice{}
	var args []interface{}
	for _, obj := range *o {
		pkeyArgs := queries.ValuesFromMapping(reflect.Indirect(reflect.ValueOf(obj)), tokenPrimaryKeyMapping)
		args = append(args, pkeyArgs...)
	}

	sql := "SELECT \"tokens\".* FROM \"tokens\" WHERE " +
		strmangle.WhereClauseRepeated(string(dialect.LQ), string(dialect.RQ), 1, tokenPrimaryKeyColumns, len(*o))

	q := queries.Raw(sql, args...)

	err := q.Bind(ctx, exec, &slice)
	if err != nil {
		return errors.Wrap(err, "models: unable to reload all in TokenSlice")
	}

	*o = slice

	return nil
}

// TokenExists checks if the Token row exists.
func TokenExists(ctx context.Context, exec boil.ContextExecutor, iD int64) (bool, error) {
	var exists bool
	sql := "select exists(select 1 from \"tokens\" where \"id\"=$1 limit 1)"

	if boil.IsDebug(ctx) {
		writer := boil.DebugWriterFrom(ctx)
		fmt.Fprintln(writer, sql)
		fmt.Fprintln(writer, iD)
	}
	row := exec.QueryRowContext(ctx, sql, iD)

	err := row.Scan(&exists)
	if err != nil {
		return false, errors.Wrap(err, "models: unable to check if tokens exists")
	}

	return exists, nil
}

// Exists checks if the Token row exists.
func (o *Token) Exists(ctx context.Context, exec boil.ContextExecutor) (bool, error) {
	return TokenExists(ctx, exec, o.ID)
}

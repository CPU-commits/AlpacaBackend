// Code generated by SQLBoiler 4.19.5 (https://github.com/aarondl/sqlboiler). DO NOT EDIT.
// This file is meant to be re-generated in place and/or deleted at any time.

package models

import (
	"bytes"
	"context"
	"reflect"
	"testing"

	"github.com/aarondl/randomize"
	"github.com/aarondl/sqlboiler/v4/boil"
	"github.com/aarondl/sqlboiler/v4/queries"
	"github.com/aarondl/strmangle"
)

var (
	// Relationships sometimes use the reflection helper queries.Equal/queries.Assign
	// so force a package dependency in case they don't.
	_ = queries.Equal
)

func testCodes(t *testing.T) {
	t.Parallel()

	query := Codes()

	if query.Query == nil {
		t.Error("expected a query, got nothing")
	}
}

func testCodesDelete(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	if rowsAff, err := o.Delete(ctx, tx); err != nil {
		t.Error(err)
	} else if rowsAff != 1 {
		t.Error("should only have deleted one row, but affected:", rowsAff)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testCodesQueryDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	if rowsAff, err := Codes().DeleteAll(ctx, tx); err != nil {
		t.Error(err)
	} else if rowsAff != 1 {
		t.Error("should only have deleted one row, but affected:", rowsAff)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testCodesSliceDeleteAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	slice := CodeSlice{o}

	if rowsAff, err := slice.DeleteAll(ctx, tx); err != nil {
		t.Error(err)
	} else if rowsAff != 1 {
		t.Error("should only have deleted one row, but affected:", rowsAff)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 0 {
		t.Error("want zero records, got:", count)
	}
}

func testCodesExists(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	e, err := CodeExists(ctx, tx, o.ID)
	if err != nil {
		t.Errorf("Unable to check if Code exists: %s", err)
	}
	if !e {
		t.Errorf("Expected CodeExists to return true, but got false.")
	}
}

func testCodesFind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	codeFound, err := FindCode(ctx, tx, o.ID)
	if err != nil {
		t.Error(err)
	}

	if codeFound == nil {
		t.Error("want a record, got nil")
	}
}

func testCodesBind(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	if err = Codes().Bind(ctx, tx, o); err != nil {
		t.Error(err)
	}
}

func testCodesOne(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	if x, err := Codes().One(ctx, tx); err != nil {
		t.Error(err)
	} else if x == nil {
		t.Error("expected to get a non nil record")
	}
}

func testCodesAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	codeOne := &Code{}
	codeTwo := &Code{}
	if err = randomize.Struct(seed, codeOne, codeDBTypes, false, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}
	if err = randomize.Struct(seed, codeTwo, codeDBTypes, false, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = codeOne.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}
	if err = codeTwo.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	slice, err := Codes().All(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 2 {
		t.Error("want 2 records, got:", len(slice))
	}
}

func testCodesCount(t *testing.T) {
	t.Parallel()

	var err error
	seed := randomize.NewSeed()
	codeOne := &Code{}
	codeTwo := &Code{}
	if err = randomize.Struct(seed, codeOne, codeDBTypes, false, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}
	if err = randomize.Struct(seed, codeTwo, codeDBTypes, false, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = codeOne.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}
	if err = codeTwo.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 2 {
		t.Error("want 2 records, got:", count)
	}
}

func codeBeforeInsertHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeAfterInsertHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeAfterSelectHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeBeforeUpdateHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeAfterUpdateHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeBeforeDeleteHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeAfterDeleteHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeBeforeUpsertHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func codeAfterUpsertHook(ctx context.Context, e boil.ContextExecutor, o *Code) error {
	*o = Code{}
	return nil
}

func testCodesHooks(t *testing.T) {
	t.Parallel()

	var err error

	ctx := context.Background()
	empty := &Code{}
	o := &Code{}

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, o, codeDBTypes, false); err != nil {
		t.Errorf("Unable to randomize Code object: %s", err)
	}

	AddCodeHook(boil.BeforeInsertHook, codeBeforeInsertHook)
	if err = o.doBeforeInsertHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doBeforeInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeInsertHook function to empty object, but got: %#v", o)
	}
	codeBeforeInsertHooks = []CodeHook{}

	AddCodeHook(boil.AfterInsertHook, codeAfterInsertHook)
	if err = o.doAfterInsertHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doAfterInsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterInsertHook function to empty object, but got: %#v", o)
	}
	codeAfterInsertHooks = []CodeHook{}

	AddCodeHook(boil.AfterSelectHook, codeAfterSelectHook)
	if err = o.doAfterSelectHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doAfterSelectHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterSelectHook function to empty object, but got: %#v", o)
	}
	codeAfterSelectHooks = []CodeHook{}

	AddCodeHook(boil.BeforeUpdateHook, codeBeforeUpdateHook)
	if err = o.doBeforeUpdateHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpdateHook function to empty object, but got: %#v", o)
	}
	codeBeforeUpdateHooks = []CodeHook{}

	AddCodeHook(boil.AfterUpdateHook, codeAfterUpdateHook)
	if err = o.doAfterUpdateHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doAfterUpdateHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpdateHook function to empty object, but got: %#v", o)
	}
	codeAfterUpdateHooks = []CodeHook{}

	AddCodeHook(boil.BeforeDeleteHook, codeBeforeDeleteHook)
	if err = o.doBeforeDeleteHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doBeforeDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeDeleteHook function to empty object, but got: %#v", o)
	}
	codeBeforeDeleteHooks = []CodeHook{}

	AddCodeHook(boil.AfterDeleteHook, codeAfterDeleteHook)
	if err = o.doAfterDeleteHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doAfterDeleteHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterDeleteHook function to empty object, but got: %#v", o)
	}
	codeAfterDeleteHooks = []CodeHook{}

	AddCodeHook(boil.BeforeUpsertHook, codeBeforeUpsertHook)
	if err = o.doBeforeUpsertHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doBeforeUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected BeforeUpsertHook function to empty object, but got: %#v", o)
	}
	codeBeforeUpsertHooks = []CodeHook{}

	AddCodeHook(boil.AfterUpsertHook, codeAfterUpsertHook)
	if err = o.doAfterUpsertHooks(ctx, nil); err != nil {
		t.Errorf("Unable to execute doAfterUpsertHooks: %s", err)
	}
	if !reflect.DeepEqual(o, empty) {
		t.Errorf("Expected AfterUpsertHook function to empty object, but got: %#v", o)
	}
	codeAfterUpsertHooks = []CodeHook{}
}

func testCodesInsert(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testCodesInsertWhitelist(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Whitelist(strmangle.SetMerge(codePrimaryKeyColumns, codeColumnsWithoutDefault)...)); err != nil {
		t.Error(err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

func testCodeToOneUserUsingIDUserUser(t *testing.T) {
	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()

	var local Code
	var foreign User

	seed := randomize.NewSeed()
	if err := randomize.Struct(seed, &local, codeDBTypes, false, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}
	if err := randomize.Struct(seed, &foreign, userDBTypes, false, userColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize User struct: %s", err)
	}

	if err := foreign.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Fatal(err)
	}

	local.IDUser = foreign.ID
	if err := local.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Fatal(err)
	}

	check, err := local.IDUserUser().One(ctx, tx)
	if err != nil {
		t.Fatal(err)
	}

	if check.ID != foreign.ID {
		t.Errorf("want: %v, got %v", foreign.ID, check.ID)
	}

	ranAfterSelectHook := false
	AddUserHook(boil.AfterSelectHook, func(ctx context.Context, e boil.ContextExecutor, o *User) error {
		ranAfterSelectHook = true
		return nil
	})

	slice := CodeSlice{&local}
	if err = local.L.LoadIDUserUser(ctx, tx, false, (*[]*Code)(&slice), nil); err != nil {
		t.Fatal(err)
	}
	if local.R.IDUserUser == nil {
		t.Error("struct should have been eager loaded")
	}

	local.R.IDUserUser = nil
	if err = local.L.LoadIDUserUser(ctx, tx, true, &local, nil); err != nil {
		t.Fatal(err)
	}
	if local.R.IDUserUser == nil {
		t.Error("struct should have been eager loaded")
	}

	if !ranAfterSelectHook {
		t.Error("failed to run AfterSelect hook for relationship")
	}
}

func testCodeToOneSetOpUserUsingIDUserUser(t *testing.T) {
	var err error

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()

	var a Code
	var b, c User

	seed := randomize.NewSeed()
	if err = randomize.Struct(seed, &a, codeDBTypes, false, strmangle.SetComplement(codePrimaryKeyColumns, codeColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &b, userDBTypes, false, strmangle.SetComplement(userPrimaryKeyColumns, userColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}
	if err = randomize.Struct(seed, &c, userDBTypes, false, strmangle.SetComplement(userPrimaryKeyColumns, userColumnsWithoutDefault)...); err != nil {
		t.Fatal(err)
	}

	if err := a.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Fatal(err)
	}
	if err = b.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Fatal(err)
	}

	for i, x := range []*User{&b, &c} {
		err = a.SetIDUserUser(ctx, tx, i != 0, x)
		if err != nil {
			t.Fatal(err)
		}

		if a.R.IDUserUser != x {
			t.Error("relationship struct not set to correct value")
		}

		if x.R.IDUserCodes[0] != &a {
			t.Error("failed to append to foreign relationship struct")
		}
		if a.IDUser != x.ID {
			t.Error("foreign key was wrong value", a.IDUser)
		}

		zero := reflect.Zero(reflect.TypeOf(a.IDUser))
		reflect.Indirect(reflect.ValueOf(&a.IDUser)).Set(zero)

		if err = a.Reload(ctx, tx); err != nil {
			t.Fatal("failed to reload", err)
		}

		if a.IDUser != x.ID {
			t.Error("foreign key was wrong value", a.IDUser, x.ID)
		}
	}
}

func testCodesReload(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	if err = o.Reload(ctx, tx); err != nil {
		t.Error(err)
	}
}

func testCodesReloadAll(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	slice := CodeSlice{o}

	if err = slice.ReloadAll(ctx, tx); err != nil {
		t.Error(err)
	}
}

func testCodesSelect(t *testing.T) {
	t.Parallel()

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	slice, err := Codes().All(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if len(slice) != 1 {
		t.Error("want one record, got:", len(slice))
	}
}

var (
	codeDBTypes = map[string]string{`ID`: `bigint`, `IDUser`: `bigint`, `Code`: `text`, `IsActive`: `boolean`, `UsesRemaining`: `bigint`, `Type`: `text`, `ExpiresAt`: `timestamp without time zone`, `CreatedAt`: `timestamp without time zone`}
	_           = bytes.MinRead
)

func testCodesUpdate(t *testing.T) {
	t.Parallel()

	if 0 == len(codePrimaryKeyColumns) {
		t.Skip("Skipping table with no primary key columns")
	}
	if len(codeAllColumns) == len(codePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, o, codeDBTypes, true, codePrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	if rowsAff, err := o.Update(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	} else if rowsAff != 1 {
		t.Error("should only affect one row but affected", rowsAff)
	}
}

func testCodesSliceUpdateAll(t *testing.T) {
	t.Parallel()

	if len(codeAllColumns) == len(codePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	o := &Code{}
	if err = randomize.Struct(seed, o, codeDBTypes, true, codeColumnsWithDefault...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Insert(ctx, tx, boil.Infer()); err != nil {
		t.Error(err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}

	if count != 1 {
		t.Error("want one record, got:", count)
	}

	if err = randomize.Struct(seed, o, codeDBTypes, true, codePrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	// Remove Primary keys and unique columns from what we plan to update
	var fields []string
	if strmangle.StringSliceMatch(codeAllColumns, codePrimaryKeyColumns) {
		fields = codeAllColumns
	} else {
		fields = strmangle.SetComplement(
			codeAllColumns,
			codePrimaryKeyColumns,
		)
	}

	value := reflect.Indirect(reflect.ValueOf(o))
	typ := reflect.TypeOf(o).Elem()
	n := typ.NumField()

	updateMap := M{}
	for _, col := range fields {
		for i := 0; i < n; i++ {
			f := typ.Field(i)
			if f.Tag.Get("boil") == col {
				updateMap[col] = value.Field(i).Interface()
			}
		}
	}

	slice := CodeSlice{o}
	if rowsAff, err := slice.UpdateAll(ctx, tx, updateMap); err != nil {
		t.Error(err)
	} else if rowsAff != 1 {
		t.Error("wanted one record updated but got", rowsAff)
	}
}

func testCodesUpsert(t *testing.T) {
	t.Parallel()

	if len(codeAllColumns) == len(codePrimaryKeyColumns) {
		t.Skip("Skipping table with only primary key columns")
	}

	seed := randomize.NewSeed()
	var err error
	// Attempt the INSERT side of an UPSERT
	o := Code{}
	if err = randomize.Struct(seed, &o, codeDBTypes, true); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	ctx := context.Background()
	tx := MustTx(boil.BeginTx(ctx, nil))
	defer func() { _ = tx.Rollback() }()
	if err = o.Upsert(ctx, tx, false, nil, boil.Infer(), boil.Infer()); err != nil {
		t.Errorf("Unable to upsert Code: %s", err)
	}

	count, err := Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}

	// Attempt the UPDATE side of an UPSERT
	if err = randomize.Struct(seed, &o, codeDBTypes, false, codePrimaryKeyColumns...); err != nil {
		t.Errorf("Unable to randomize Code struct: %s", err)
	}

	if err = o.Upsert(ctx, tx, true, nil, boil.Infer(), boil.Infer()); err != nil {
		t.Errorf("Unable to upsert Code: %s", err)
	}

	count, err = Codes().Count(ctx, tx)
	if err != nil {
		t.Error(err)
	}
	if count != 1 {
		t.Error("want one record, got:", count)
	}
}

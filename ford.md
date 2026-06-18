---
project: sqr
summary: Lightweight pure-Fortran relational store — direct-access binary tables with sorted secondary indices
author: Simon Geard
project_url:
src_dir: src
output_dir: ford_docs
exclude_dir: build obj_intel_release obj_gfortran_release obj_gfortran_debug
dbg: False
---

`sqr` is a small relational store written in pure modern Fortran with no
external dependencies.  A database is just a directory; tables are
fixed-size direct-access binary records, with stream-binary catalog and
schema metadata and sorted secondary indices.  An interactive shell,
`sqrsh`, is driven by the sibling `cmdgraph` engine.

It is deliberately *not* a SQL engine: there is no parser, planner,
networked access, or multi-writer concurrency.  It is the storage
primitive — record layout, indexing, persistence, a typed row buffer —
and nothing above it.

## On-disk layout

A database directory contains, per table:

| File | Access | Contents |
|------|--------|----------|
| `_catalog.dat` | stream binary | magic `SQRC`, schema version, table names |
| `<table>.schema` | stream binary | magic `SQRT`, header, column + index definitions |
| `<table>.dat` | direct, `recl = record_size` | status byte + columns packed at fixed offsets |
| `<table>__i<slot>.idx` | direct, `recl = key_size+4` | `(key_bytes, int32 row_id)` sorted ascending |
| `<table>.blob` | stream binary | append-only bytes for `DT_TEXT` columns |

Each data record is one `character(len=record_size)` buffer: byte 1 is the
status (`ROW_ALIVE` / `ROW_TOMBSTONE`), the rest is column data packed by
`layout_columns` and accessed with `transfer` through the `row_*` helpers.

## Column types

| Constant | Width | Notes |
|----------|-------|-------|
| `DT_INT`  | 4 B  | `int32` |
| `DT_REAL` | 8 B  | `real64`; `db_find_by_real` is exact bit-for-bit |
| `DT_CHAR` | 1..65536 B | NUL-padded, read to first NUL — **not** binary-safe |
| `DT_TEXT` | 12 B descriptor | arbitrary length; bytes in `<table>.blob` |

## API surface

| Group | Procedures | File |
|-------|-----------|------|
| Lifecycle | `db_open`, `db_close` | `sqr_table` |
| Schema    | `db_create_table`, `db_drop_table`, `db_compact`, `db_list_tables`, `db_table_index` | `sqr_table` |
| Rows      | `db_insert`, `db_get`, `db_update`, `db_delete`, `db_scan` | `sqr_record` |
| Text      | `db_set_text`, `db_get_text` | `sqr_record` |
| Indices   | `db_create_index`, `db_find_by_int`, `db_find_by_real`, `db_find_by_char` | `sqr_index` |
| Range / cursor | `db_open_cursor`, `db_find_range_int`, `db_find_range_real`, `db_find_range_char`, `db_cursor_next` | `sqr_index` |
| Natural keys | `db_get_by_key`, `db_update_by_key`, `db_delete_by_key` | `sqr_index` |
| Maintenance | `db_drop_index`, `db_insert_many`, `db_verify` | `sqr_admin` |
| Row buffer | `row_alloc`, `row_clear`, `row_status`, `row_set_*`, `row_get_*` | `sqr_rowbuf` |

Errors follow idiomatic Fortran library style: optional `stat` / `errmsg`
out arguments and no `error stop` in library code.

## Modules

| Module | Role |
|--------|------|
| `sqr` | Public API — types, constants, procedure interfaces |
| `sqr:sqr_base` | Shared engine core — paths, validation, catalog/schema I/O, key compare, B+-tree rebuild |
| `sqr:sqr_base:sqr_table` | Table lifecycle — open/close, create/drop table, compact, list |
| `sqr:sqr_base:sqr_record` | Per-row API — insert/get/update/delete/scan, text, per-row index upkeep |
| `sqr:sqr_base:sqr_index` | Index query/maintenance — create index, find-by, cursors, ranges, by-key |
| `sqr:sqr_base:sqr_admin` | Whole-table maintenance — drop index, batch insert, verify |
| `sqr:sqr_base:sqr_rowbuf` | Typed row-buffer accessors (`row_*`) |
| `clib_wrap` | Generic `iso_c_binding` shims for `rename`/`remove`/`mkdir`/`access`/`nftw` |
| `clib_wrap_sm` | Implementation submodule (not documented here) |

# sqr — a pure-Fortran relational store

`sqr` is a lightweight, embeddable relational storage engine written entirely
in modern Fortran. It stores tables as fixed-record binary files in a
directory, with on-disk B+-tree secondary indices, a physical rollback
journal for crash-safe transactions, and two interactive front-ends — a
state-graph shell (`sqrsh`) and a small SQL-subset REPL (`sqlsh`).

It is deliberately scoped for the small-to-medium workloads a single program
needs (10⁴–10⁶ rows), not for postgres-scale concurrency. Access is
**single-writer / multi-reader**: an advisory lock admits one read-write
connection *or* any number of read-only ones at a time — there is no
concurrent-writer (per-row / MVCC) isolation. The design goal is
**integrity first**: every mutation is write-ahead journalled and survives a
crash, even at the cost of an `fsync` per write.

---

## Features

- **A database is a directory.** No server, no daemon — open a path.
- **Typed columns:** `DT_INT` (32-bit), `DT_REAL` (64-bit), `DT_CHAR`
  (fixed-width, NUL-padded), `DT_TEXT` (length-prefixed blob, binary-safe).
- **Per-row NULL bitmap** — a NULL column reads back as absent and is omitted
  from any index it belongs to (partial-index semantics).
- **On-disk B+-tree indices**, including composite keys, uniqueness, range
  scans and cursors. The `b_tree` module is fully decoupled from `sqr` and is
  independently reusable.
- **Schema evolution:** `ADD COLUMN` / `DROP COLUMN` by table rewrite, with
  row ids preserved (no index rebuild) and `DROP` cascading to dependent
  indices.
- **Crash-safe transactions:** explicit `db_begin`/`db_commit`/`db_rollback`,
  plus auto-commit brackets around every single-row mutator. See
  [ACID guarantees](#acid-guarantees) below.
- **Single-writer / multi-reader locking:** an advisory lock taken on open
  admits one writer or many readers; contention is reported as `SQR_LOCKED`,
  and `db_set_readonly` demotes a writer to let readers in.
- **No `error stop` in library code** — every fallible entry point reports via
  optional `stat` / `errmsg` arguments, and the handle keeps sticky error
  state: `db_last_error` returns the most recent operation's code and failure
  detail, with `sqr_errstr` supplying the canonical text per code.
- **Metadata accessors:** `db_describe` (column snapshot), `db_row_count` and
  `db_in_txn`, so front-ends never need to walk the handle's internals.
- **Procedural *and* object-oriented APIs:** call `db_insert(db, ...)` or
  `db%insert(...)`.
- **Two front-ends:** `sqrsh`, a cmdgraph state-graph shell over the engine,
  and `sqlsh`, a small SQL subset (a separate `sql` front-end layer that
  only calls the public API — no SQL in the store). See below.
- **Portable:** clean on `ifx` and `gfortran`, builds under `fpm`, and
  cross-builds to Windows (mingw-w64, wine-validated).

---

## Building

The primary build is `make` with `ifx` (load the Intel environment first):

```sh
make all            # build + run unit, B+-tree and fault tests
make utest          # unit tests + B+-tree tests
make faulttest      # fault-injection sweep (FAULT=on, debug build)
make destruct       # destruction/fuzz corpora against a -check all build
make bench          # micro-benchmarks
make docs           # FORD API docs -> ford_docs/
make distclean      # remove ALL generated files
```

Compiler and build-mode selection:

```sh
make F=gfortran all   # secondary compiler (gfortran)
make F=ifx all        # default
make debug=1 ...      # debug build
make windows          # mingw-w64 cross-build (.exe)
```

`fpm` is also supported for the core library, app and tests (it globs `src/`,
`app/` and `test/` only):

```sh
fpm test
fpm run sqrsh
fpm run sqlsh        # the SQL-subset REPL (front-end over the same API)
```

> Note: the regex-dependent `match` action and its test link the external
> `tcl_re` binding and are built via `make sqrsh-regex` / `make test-regex`,
> not through `fpm`.

---

## Quick start (Fortran API)

```fortran
use :: sqr
type(db_t), target :: db
type(column_t)     :: cols(2)
character(len=:), allocatable :: buf
integer        :: st, ti
integer(int32) :: rid

call db_open(db, 'mydb', stat=st)                 ! a database is a directory

cols(1)%name = 'id';   cols(1)%dtype = DT_INT;  cols(1)%csize = 4
cols(2)%name = 'name'; cols(2)%dtype = DT_CHAR; cols(2)%csize = 32
call db_create_table(db, 'people', cols, st)
ti = db_table_index(db, 'people')

call row_alloc(buf, db%tables(ti)%record_size)
call row_set_int (buf, db%tables(ti)%cols(1), 1_int32)
call row_set_char(buf, db%tables(ti)%cols(2), 'Ada')
call db_insert(db, 'people', buf, rid, st)        ! rid = new row id

call db_create_index(db, 'people', 'id', st)      ! on-disk B+-tree
call db_find_by_int(db, 'people', 'id', 1_int32, rid, st)

call db_close(db, st)
```

Wrap a group of changes in an explicit transaction when you need them to
commit (and fail) as a unit:

```fortran
call db_begin(db, st)
! ... several inserts / updates / deletes ...
call db_commit(db, st)     ! durable here; or db_rollback(db, st)
```

---

## Public API surface

- **Lifecycle:** `db_open`, `db_close`, `db_set_readonly`
- **Tables:** `db_create_table`, `db_drop_table`, `db_compact`,
  `db_add_column`, `db_drop_column`, `db_list_tables`, `db_table_index`,
  `idx_live`
- **Rows:** `db_insert`, `db_get`, `db_update`, `db_delete`, `db_scan`,
  `db_insert_many`
- **Text/blob columns:** `db_set_text`, `db_get_text`
- **Indices:** `db_create_index`, `db_drop_index`, `db_verify`
- **Key lookups:** `db_find_by_int` / `_real` / `_char`, `db_get_by_key`,
  `db_update_by_key`, `db_delete_by_key`
- **Range scans / cursors:** `db_open_cursor`, `db_find_range`,
  `db_cursor_next`
- **Transactions:** `db_begin`, `db_commit`, `db_rollback`
- **Row buffers:** `row_alloc`, `row_clear`, `row_status`, `row_set_status`,
  `row_set_null`, `row_clear_null`, `row_is_null`, `row_set_int` /
  `row_get_int`, `row_set_real` / `row_get_real`, `row_set_char` /
  `row_get_char`

Every `db_*` procedure also has a type-bound equivalent on `db_t`
(`db%open`, `db%insert`, `db%create_index`, …).

---

## The `sqrsh` shell

`sqrsh` is a small state-graph REPL over the engine. The command set:

```
root:    open <dir>   close   readonly   tables   desc <table>
         create <table>   use <table>   drop <table>   quit
creator: col <name> <type>   done   cancel   quit
table:   insert ...   select   get <id>   delete <id>   compact
         addcolumn <name> <type>   dropcolumn <name>
         index [unique] <col>...   dropindex <col>...   verify
         find <col> <value>   range <col> <lo> <hi>   match <col> <regex>
         getk ...   delk ...   back   quit
```

---

## The `sqlsh` shell (SQL subset)

`sqlsh` is a second, independent front-end: a familiar SQL "shop window"
over the same engine. It is a **front-end layer only** — the `sql` module
(lexer, parser, executor) and the REPL call nothing but the public `db_*`
API, so the dependency runs one way (`sql` uses `sqr`, never the reverse)
and nothing about the on-disk format changes. The store itself has no
notion of SQL.

```
sqlsh mydb < script.sql        # run a script (results on stdout)
sqlsh mydb                     # interactive (prompts/errors on stderr)
```

Meta-commands: `.open <dir>`, `.close`, `.tables`, `.schema [table]`,
`.help`, `.quit`. Everything else is SQL:

```sql
CREATE TABLE employee (id INTEGER, name CHAR(20), dept CHAR(12), salary REAL);
CREATE INDEX ON employee (dept);
INSERT INTO employee VALUES (1,'Alice','eng',55000.0), (2,'Bob','eng',48000.0);
SELECT name, salary FROM employee WHERE dept = 'eng' ORDER BY salary DESC LIMIT 5;
UPDATE employee SET salary = 50000.0 WHERE dept = 'sales' AND salary < 50000.0;
DELETE FROM employee WHERE salary < 40000.0;
```

The supported subset: DDL (`CREATE`/`DROP TABLE`, `CREATE [UNIQUE] INDEX`,
`DROP INDEX`, `ALTER TABLE … ADD/DROP COLUMN`), DML (`INSERT`, `DELETE`,
`UPDATE`), `SELECT col|* … [WHERE][ORDER BY col [ASC|DESC]][LIMIT n]`, and
`BEGIN`/`COMMIT`/`ROLLBACK`. The `WHERE` predicate combines comparisons
(`= <> < <= > >=`), `BETWEEN`, and `IS [NOT] NULL` with `AND`/`OR`. Types
are `INTEGER`/`INT`, `REAL`, `CHAR(n)`, `TEXT`. A single equality on an
indexed column is driven through the B+-tree; everything else is a scan,
with the full predicate re-applied either way (identical results). Out of
scope (kept honest): JOINs, subqueries, aggregates / `GROUP BY`,
expressions in the projection, and column constraints such as `NOT NULL` —
the engine has no constraint store.

---

## Serving and backing up over the wire (`sqrd`, `sqrbak`)

`sqrd` serves one database over a loopback socket so out-of-process clients —
the LibreOffice Calc macro in `calc/`, the ODBC driver in the sibling
`sqr_odbc` project — can reach it:

```
sqrd <db-dir> <port>          # port 0 asks the OS for an ephemeral one
```

It prints `LISTENING <port>` on stdout and, on stderr, the **absolute path** of
the database it is serving. That matters more than it looks: a client picks a
host and a port, so without it there is nothing to tell you *which* database on
disk you have reached — or which of several `sqrd` instances is which.

`sqrbak` asks a running server exactly that, and takes backups:

```
sqrbak info   [<host>:]<port>            # name, absolute dir, version, txn state
sqrbak backup [<host>:]<port> <file>     # write a .sqr container
```

The backup is taken **without closing the database**: the server flushes and
releases its file units, reads the snapshot, and reopens them, all while
holding its own advisory lock, so no client is disconnected and no other
process can take the database meanwhile. It is refused while a transaction is
open — a directory mid-gesture is not a state worth archiving. Restore with
`sqlsh`'s `.unpack`, which is the same container format as `db_pack`.

```
$ sqrbak info 7477
name = profile_01
dir = /home/simon/development/projects/sqr/profile_01
server = sqrd 1.0.0
protocol = 1
readonly = no
txn = no
tables = 12
$ sqrbak backup 7477 profile_01-$(date +%F).sqr
packed /home/simon/development/projects/sqr/profile_01 -> /home/simon/backups/profile_01-2026-08-15.sqr
```

Relative destinations are resolved against `sqrbak`'s working directory, not
the server's — the server itself insists on an absolute path, precisely so a
name can never quietly mean a different place at each end.

See `reports/DESIGN-wire-protocol.md` for the protocol itself.

---

## On-disk layout

A database is a directory containing:

| File                  | Contents                                            |
|-----------------------|-----------------------------------------------------|
| `_catalog.dat`        | top-level catalog: the list of table names          |
| `<table>.schema`      | per-table schema header + column definitions        |
| `<table>.dat`         | fixed-size records (`recl = record_size`)           |
| `<table>.blob`        | length-prefixed `DT_TEXT` values                    |
| `<table>__i<slot>.idx`| one paged B+-tree per secondary index               |
| `_journal.dat`        | rollback (undo) journal; present only while a txn is open or pending recovery |
| `_lock`               | zero-byte sentinel carrying the advisory open lock  |

Each record is: 1 status byte (`ROW_ALIVE` / `ROW_TOMBSTONE`), then a
`(ncols+7)/8`-byte NULL bitmap, then column data at fixed offsets.

---

## ACID guarantees

`sqr` is honest about what it provides. The store is **single-writer /
multi-reader**: at most one read-write connection, or any number of
read-only connections, at a time — enforced by an advisory lock taken on
`db_open`. It does *not* provide fine-grained (per-row / MVCC) isolation
between concurrent writers.

| Property        | Status | How                                                                                                  |
|-----------------|--------|------------------------------------------------------------------------------------------------------|
| **Atomicity**   | ✅ Yes | Physical undo journal; a failed or rolled-back transaction restores every touched region.            |
| **Consistency** | ✅ Yes, including across a crash | Recovery on `db_open` replays the journal, restoring data, blob and index files together. |
| **Durability**  | ✅ Yes | Strict write-ahead: each undo image is `fsync`'d to the hot journal *before* the base write it guards. Commit `fsync`s every modified file, then voids the journal header — the single durable commit point. |
| **Isolation**   | ◑ Coarse | An advisory lock on `_lock` admits one writer **xor** many readers. A second writer (or a reader while a writer is active) is refused with `SQR_LOCKED`. `db_set_readonly` downgrades a writer so readers may attach. No concurrent-writer / row-level isolation. |

Locking is whole-database advisory: `flock(2)` on POSIX, `LockFileEx` on
Windows. It is released on `db_close` and automatically by the OS if the
process dies, so a crashed writer never wedges the database.

The durability path is deliberately conservative — an `fsync` per write —
because the project prioritises integrity over throughput.

That choice has a measurable price, and it is worth knowing the shape of it:
a statement outside an explicit transaction is its own transaction, costing
two to five durability barriers. On real storage the barrier dominates
everything else — on the machine this was measured on, one `fsync` costs
~9 ms, so an autocommit indexed insert costs ~42 ms while the same insert
inside an explicit transaction costs ~35 µs. **Batch your writes in
`db_begin` / `db_commit`**; the journal's dedup means the barriers stop
scaling with the row count, so per-insert cost *falls* as the transaction
grows.

---

## Robustness

Three test layers back the guarantees above:

```sh
make utest          # unit + B+-tree tests    (1118 assertions)
make faulttest      # every I/O call site made to fail in turn   (67)
make destruct       # corrupt input: ~3000 cases across 4 corpora
```

`destruct/` is a black-box harness built against a `-check all` library. It
holds the engine to one contract: **it may return any error it likes, but it
may not crash, abort, hang, or read or write outside its buffers.** Four
corpora exercise it — targeted and random mutation of every on-disk artefact
(catalog, schema, data, blob and index files, and the journal), the `.sqr`
single-file container, hostile SQL text, and hostile arguments to the public
API. The blind-mutation sections are seeded so a run is reproducible;
`make destruct DESTRUCT_SEED=n` sweeps ground the default seed never reaches.

The sweep that introduced it found eight defects — out-of-bounds reads and
writes reachable from ordinary calls on a database whose index or journal had
a single wrong `int32`. All eight are fixed: B+-tree page bodies are now
validated on every read, child and leaf-chain pointers are bounded at each
dereference, descent is depth-limited, and every journal field is bounded
against the bytes actually remaining rather than a sum that can overflow.

---

## Why Fortran?

Partly because it is the author's language of choice — but the fit is
better than it might first appear. Three language features carry real
architectural weight here:

- **Direct-access files.** `open(..., access='direct', recl=...)` is
  fixed-size record I/O, native to the language: one B+-tree page or one
  table row is one record, read and written by number. The entire on-disk
  layer is built on it — no `mmap`, no `pread` wrappers, no serialization
  framework — which is why that layer is as small as it is.

- **Allocatable components.** Rows, keys, blob buffers and per-cursor
  scratch are `allocatable` — sized at run time, freed deterministically
  when they go out of scope, never leaked and never manually released.
  Memory management with neither a garbage collector nor a single
  `free`: the test suites run valgrind-clean essentially by construction.

- **Compiler discipline.** The standard's strictness about argument
  intent, `pure` procedures and interface checking turns whole bug
  classes into compile-time errors. The project builds warning-clean on
  two compilers (ifx and gfortran), and the destruction harness runs
  against a `-check all` build, so bounds and shape violations fail loudly
  in test rather than silently in production.

Honesty requires the debit column too:

- `.and.` / `.or.` do **not** short-circuit; a guard and the expression it
  guards must be separate `if` statements. (The sweep found exactly this
  bug once: a checksum folding an uninitialised buffer.)
- There are no unsigned integers, so bounds arithmetic on hostile values
  near `huge(int32)` must be arranged never to overflow an intermediate —
  fields are checked against the bytes *remaining*, not accumulated sums.
- The standard has no notion of durability or file locking. `fsync`,
  `flock`/`LockFileEx` and `chmod` live in a small C shim — the only
  non-Fortran code in the store, confined to the OS-facing edge.

---

## Documentation

`make docs` generates the full FORD API reference into `ford_docs/`.

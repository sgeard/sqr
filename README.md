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
- **One file when you need one.** `db_pack` writes the whole database to a
  single `.sqr` container and `db_unpack` restores it to a new directory —
  for backup, transport, or an application that keeps its document as one
  file and unpacks it on open. See
  [Single-file container](#single-file-container-sqr) below.
- **Typed columns:** `DT_INT` (32-bit), `DT_REAL` (64-bit), `DT_CHAR`
  (fixed-width, NUL-padded), `DT_TEXT` (length-prefixed blob, binary-safe).
- **Per-row NULL bitmap** — a NULL column reads back as absent. A row with
  a NULL in an indexed column is in the index like any other: a NULL orders
  before every value, matches no value, and is not constrained by a unique
  index.
- **On-disk B+-tree indices**, including composite keys, uniqueness, range
  scans and cursors. The `b_tree` module is fully decoupled from `sqr` and is
  independently reusable.
- **Schema evolution:** `ADD COLUMN` / `DROP COLUMN` by table rewrite, with
  row ids preserved (no index rebuild) and `DROP` cascading to dependent
  indices; `db_reshape_table` makes a table exactly a declared column list
  (drop, add and reorder matched by name) in one rewrite.  Every layout change
  stages its rewritten data and schema, commits with a durable marker and
  swaps by renames, so an interrupted change is rolled forward on open.
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
- **Portable:** clean on `ifx`, `gfortran`, LLVM `flang` and AMD `aof`, builds under `fpm`, and
  cross-builds to Windows (mingw-w64, wine-validated).

---

## Building

The primary build is `make` with `ifx` (load the Intel environment first):

```sh
make all            # build + run unit, B+-tree and fault tests
make utest          # unit tests + B+-tree tests
make faulttest      # fault-injection sweep (FAULT=on, debug build in its own obj_*_fault)
make destruct       # destruction/fuzz corpora against a -check all build
make bench          # micro-benchmarks
make docs           # FORD API docs -> ford_docs/
make distclean      # remove ALL generated files
```

Compiler and build-mode selection:

```sh
make F=gfortran all   # secondary compiler (gfortran)
make F=flang all      # LLVM flang (23+)
make F=aof all        # AMD AOCC aof (6.0)
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

> **ifx needs `-assume byterecl`.** Table and index files are direct-access
> with `recl` given in bytes. gfortran, flang and aof count `recl` in bytes;
> ifx counts 4-byte words unless built with `-assume byterecl`, and without
> it every record lands at the wrong offset (the tests crash). The Makefile
> passes the flag. Under `fpm` add it yourself:
>
> ```sh
> fpm test --compiler ifx --flag '-assume byterecl'
> ```
>
> fpm 0.12 added it in its default debug profile; fpm 0.14 does not (its
> release profile still does).

> Note: the regex-dependent `match` action and its test link the external
> `tcl_re` binding and are built via `make sqrsh-regex` / `make test-regex`,
> not through `fpm`. The command is chosen at link time (an alternative
> submodule of `sqrsh_extras`), not by a preprocessor symbol.

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

The handle is declared `target`, and must be: while a transaction is open
(every write opens one) the index trees hold a pointer to it.  A handle held
inside another type needs that variable to be the target.  `db_open`
refuses a handle that is already open (`SQR_INVALID`, the open database
left as it was): `db_close` it first.  Trailing blanks in a path are not
significant; a directory whose name ends in a space cannot be opened.

Wrap a group of changes in an explicit transaction when you need them to
commit (and fail) as a unit:

```fortran
call db_begin(db, st)
! ... several inserts / updates / deletes ...
call db_commit(db, st)     ! durable here; or db_rollback(db, st)
```

Outside a transaction every call is atomic by itself. Inside one, a call that
fails after it has begun to write cannot be taken back on its own, so it
*dooms* the transaction: every later write returns `SQR_INVALID`, reads still
work, and `db_commit` rolls the transaction back and returns `SQR_INVALID`
instead of committing a half-applied change. A call that fails before it
writes anything (a duplicate key, a missing row) leaves the transaction
usable. A caller whose own statement spans several calls can apply the same
rule with `db_txn_mark` and `db_doom_txn`, as the SQL layer does.

Give the transaction a `label` and it becomes a *gesture*: one step on an
in-memory Undo/Redo history, for an application that offers Undo.

```fortran
call db_begin(db, st, label='Move point')
! ... the changes that make up one user action ...
call db_commit(db, st)

call db_undo(db, st, label)    ! the gesture is gone; label = 'Move point'
call db_redo(db, st, label)    ! ...and back
```

- The history lives for the session. Applying a step is itself a journalled
  transaction, so an undo or redo is all-or-nothing and survives a crash like
  any other commit.
- A step restores the bytes its gesture changed, so it is only valid while
  nothing else has changed them. **A committed change that is not a gesture —
  any auto-committed mutator, any `db_begin` without a label — empties the
  history**, as do the structural operations (`db_compact`, `db_create_index`,
  column changes, ...). An application that offers Undo makes every write a
  gesture.
- **A row id is never reused.** Undoing a gesture that inserted rows leaves
  them in the data file as tombstones and does not lower the table's next
  row id, so an insert after an undo gets a new id and an id the caller still
  holds reads as `SQR_NOT_FOUND`, never as some other row. Redo brings the
  same rows back at the same ids. `db_compact` reclaims the tombstones (and
  renumbers, as it always has). A rolled-back transaction is different: its
  rows were never committed, and their ids are handed out again.
- A gesture enters the history only when its `db_commit` succeeds. A commit
  that fails is rolled back and leaves both the undo and the redo steps as
  they were.
- `db_undo` and `db_redo` are refused inside an open transaction
  (`SQR_INVALID`), and return `SQR_NO_UNDO` when there is nothing to apply.

---

## Public API surface

- **Lifecycle:** `db_open`, `db_close`, `db_set_readonly`
- **Single-file container:** `db_pack`, `db_unpack`, `db_pack_live`
- **Tables:** `db_create_table`, `db_drop_table`, `db_compact`,
  `db_add_column`, `db_drop_column`, `db_reshape_table`, `db_list_tables`,
  `db_table_index`,
  `idx_live`
- **Rows:** `db_insert`, `db_get`, `db_update`, `db_delete`, `db_scan`,
  `db_insert_many`
- **Text/blob columns:** `db_set_text`, `db_get_text`
- **Indices:** `db_create_index`, `db_drop_index`, `db_verify`
- **Key lookups:** `db_find_by_int` / `_real` / `_char`, `db_get_by_key`,
  `db_update_by_key`, `db_delete_by_key`
- **Range scans / cursors:** `db_open_cursor`, `db_find_range`,
  `db_cursor_next`
- **Transactions:** `db_begin`, `db_commit`, `db_rollback`, `db_in_txn`,
  `db_txn_mark`, `db_doom_txn`, `db_txn_doomed`
- **Undo / Redo history:** `db_undo`, `db_redo`, `db_can_undo`, `db_can_redo`,
  `db_reset_history`
- **Row buffers:** `row_alloc`, `row_clear`, `row_status`, `row_set_status`,
  `row_set_null`, `row_clear_null`, `row_is_null`, `row_set_int` /
  `row_get_int`, `row_set_real` / `row_get_real`, `row_set_char` /
  `row_get_char`; `real_text` gives the text form every shell shows a real in

Every `db_*` procedure also has a type-bound equivalent on `db_t`
(`db%open`, `db%insert`, `db%create_index`, …).

Declare every handle `type(db_t), target` (see the quick start).

---

## The `sqrsh` shell

`sqrsh` is a small state-graph REPL over the engine; [`SQRSH-REFERENCE.md`](SQRSH-REFERENCE.md)
is its definition. The command set:

```
root:    open <dir>   close   readonly   tables   desc <table>
         create <table>   use <table>   drop <table>
         pack <dir> <file>   unpack <file> <dir>   quit
creator: col <name> <type>   done   cancel   quit
table:   insert ...   select   get <id>   delete <id>   compact
         addcolumn <name> <type>   dropcolumn <name>
         index [unique] <col>...   dropindex <col>...   verify
         find <col> <value>   range <col> <lo> <hi>   match <col> <regex>
         getk ...   delk ...   back   quit
```

Results go to standard output and failures to standard error; the banner
and prompts appear only at a terminal. `sqrsh` ends with status 1 when any
command failed, and 0 when none did.

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
sqlsh --version                # the release; --help prints the usage
```

The directory is created if it does not exist, so an argument that starts
with `-` is always taken as an option and an unknown one is refused (exit
status 2) rather than opened; `sqrsh` and `sqrd` refuse one likewise. A
directory whose name really starts with `-` is given as `./-name`.
`sqlsh` ends with status 1 when any statement or meta-command failed, and 0
when none did.

Meta-commands: `.open <dir>`, `.close`, `.tables`, `.schema [table]`,
`.pack <dir> <file>`, `.unpack <file> <dir>`, `.help`, `.quit`. Everything else is SQL:

**Input encoding.** `sqlsh` and `sqrsh` read UTF-8 (plain ASCII included).
Text is stored and compared as bytes, so UTF-8 values round-trip and sort in
code-point order; `CHAR(n)` counts bytes, not characters, and a non-ASCII
table or column name must be double-quoted. A double-quoted name may hold
spaces and any punctuation but no control character (a tab or a line break,
for instance). A UTF-8 byte-order mark at the start of the input is ignored.

Nothing else is supported, and input that can be recognised as something
else is refused by name, with exit status 1:

- by its leading signature (table below);
- by NUL bytes in a line: a 16- or 32-bit encoding written without a
  byte-order mark. Lines before the first such line have already been run.

| First bytes   | Reported as                    | `sql` constant        | Read? |
|---------------|--------------------------------|-----------------------|-------|
| (none)        | ASCII / UTF-8                  | `SQL_ENC_PLAIN`       | yes   |
| `EF BB BF`    | UTF-8 with a byte-order mark   | `SQL_ENC_UTF8`        | yes, mark dropped |
| `FF FE 00 00` | UTF-32 / UCS-4, little-endian  | `SQL_ENC_UTF32_LE`    | no    |
| `00 00 FE FF` | UTF-32 / UCS-4, big-endian     | `SQL_ENC_UTF32_BE`    | no    |
| `FF FE`       | UTF-16 / UCS-2, little-endian  | `SQL_ENC_UTF16_LE`    | no    |
| `FE FF`       | UTF-16 / UCS-2, big-endian     | `SQL_ENC_UTF16_BE`    | no    |
| `2B 2F 76`    | UTF-7                          | `SQL_ENC_UTF7`        | no    |
| `F7 64 4C`    | UTF-1                          | `SQL_ENC_UTF1`        | no    |
| `DD 73 66 73` | UTF-EBCDIC                     | `SQL_ENC_UTF_EBCDIC`  | no    |
| `0E FE FF`    | SCSU                           | `SQL_ENC_SCSU`        | no    |
| `FB EE 28`    | BOCU-1                         | `SQL_ENC_BOCU1`       | no    |
| `84 31 95 33` | GB18030                        | `SQL_ENC_GB18030`     | no    |
| NUL in a line | a wide encoding with no mark   | `SQL_ENC_WIDE_NO_BOM` | no    |

UCS-2 and UTF-16 (and UCS-4 and UTF-32) begin with the same bytes, so each
pair is reported under one name. UTF-32 little-endian is tested before
UTF-16 little-endian, whose mark it starts with. The UTF-7 signature is the
printable text `+/v`: input whose first line begins with those three
characters is refused. Plain EBCDIC has neither a signature nor NUL bytes
and is not detected.

The message is `unsupported encoding (<name>); expected ASCII or UTF-8`.
The usual cause is Windows PowerShell 5.1, whose `>` writes UTF-16: re-save
the script as UTF-8 (`Out-File -Encoding utf8`, or use PowerShell 7).

Legacy 8-bit sets (Latin-1, CP1252, ...) have no signature and cannot be
told from UTF-8; they are stored as the bytes they are, unvalidated.

Both shells apply the same table: `sqlsh` through `sql_screen_line` in the
library (public, with `sql_strip_bom`, `sql_encoding_name`,
`sql_encoding_supported` and `sql_encoding_message`, for a program that
reads its own SQL), `sqrsh` through cmdgraph 1.4.0's public `strip_bom`,
`encoding_name` and `encoding_supported`, on lines it reads itself.

```sql
CREATE TABLE employee (id INTEGER, name CHAR(20), dept CHAR(12), salary REAL);
CREATE INDEX ON employee (dept);
INSERT INTO employee VALUES (1,'Alice','eng',55000.0), (2,'Bob','eng',48000.0);
SELECT name, salary FROM employee WHERE dept = 'eng' ORDER BY salary DESC LIMIT 5;
UPDATE employee SET salary = 50000.0 WHERE dept = 'sales' AND salary < 50000.0;
DELETE FROM employee WHERE salary < 40000.0;
SELECT COUNT(*) AS staff, AVG(salary), MAX(salary) FROM employee WHERE dept IN ('eng', 'ops');
SELECT name FROM employee WHERE name LIKE 'A%' AND dept NOT IN ('sales');
```

What follows is a summary; [`SQL-REFERENCE.md`](SQL-REFERENCE.md) is the definition.

The supported subset: DDL (`CREATE`/`DROP TABLE`, `CREATE [UNIQUE] INDEX`,
`DROP INDEX`, `ALTER TABLE … ADD/DROP COLUMN`), DML (`INSERT`, `DELETE`,
`UPDATE`), `SELECT item,…|* … [WHERE][ORDER BY col [ASC|DESC],…][LIMIT n]`, and
`BEGIN`/`COMMIT`/`ROLLBACK`. Types are `INTEGER`/`INT`, `REAL`, `CHAR(n)`,
`TEXT`.

A statement is atomic. Outside `BEGIN` a statement that fails changes
nothing. Inside `BEGIN`, a statement that fails after writing some of its
rows (the third row of a multi-row `INSERT` is a duplicate, say) dooms the
transaction: later `INSERT`/`UPDATE`/`DELETE` statements are refused,
`SELECT` still works, and `COMMIT` reports an error and rolls back. Issue
`ROLLBACK` and start again. A statement that fails before it writes anything
leaves the transaction as it was.

A projection `item` is a column, a literal or an aggregate, optionally
renamed with `AS name`:

- **Aggregates** — `COUNT(*)`, `COUNT(col)`, `MIN`, `MAX`, `SUM`, `AVG`.
  There is no `GROUP BY`, so a projection holding an aggregate returns
  exactly one row and may hold only aggregates and literals. NULL values are
  skipped; over no rows `COUNT` is 0 and the others are NULL. `AVG` is always
  `REAL`; `SUM` has its column's type, and a `SUM` of an `INTEGER` column
  that leaves the 32-bit range is an error, not a wrapped value.
- **Literals** — `SELECT 1` needs no table; `SELECT name, 'x' FROM t` gives
  the literal on every row.

The `WHERE` predicate combines these with `AND`/`OR` (no parentheses):

| Condition | Notes |
|---|---|
| `col = <> < <= > >= literal` | |
| `col [NOT] BETWEEN a AND b` | inclusive |
| `col [NOT] IN (literal, …)` | a list, not a subquery |
| `col [NOT] LIKE 'pattern' [ESCAPE 'c']` | `CHAR` columns; `%` is any run of characters, `_` exactly one (one UTF-8 character, not one byte) |
| `col IS [NOT] NULL` | the only condition a NULL value satisfies |

`LIKE` compares bytes, so it is **case-sensitive**, as in the SQL standard
and PostgreSQL (SQLite's ignores ASCII case). There is no regular-expression
operator.

`ORDER BY` puts NULLs first when ascending and last with `DESC`, as SQLite
does; it is also the order an index keeps its keys in. Rows that compare
equal keep the order they were found in. Several keys may be given, each
with its own direction (`ORDER BY dept, salary DESC`): a later key only
separates rows that are equal on all the keys before it.

A single comparison or `BETWEEN` on an indexed column is driven through the
B+-tree; everything else is a scan, with the full predicate re-applied
either way (identical results). Out of scope (kept honest): JOINs,
subqueries, `GROUP BY` / `HAVING`, `DISTINCT`, arithmetic in the
projection, and column constraints such as `NOT NULL` — the engine has no
constraint store.

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

On startup it also records where it is listening in `<db-dir>/_sqrd`, so
clients can name the **database** rather than a port:

```
pid = 1459619
port = 7477
host = 127.0.0.1
```

That file is a hint, not a fact — nothing removes it when a daemon dies — so
every reader connects and checks which directory is really being served
before believing it. That one check also catches a reused pid and a recycled
port.

Install the programs and the launcher side by side and there is nothing else
to wire up — `run_sqrd` looks for `sqrd` and `sqrbak` in its own directory,
falling back to the `obj_*` build directories when it is run straight out of
the source tree:

```
make install                      # /usr/local/bin (needs root)
make install PREFIX=~/.local      # no root; ~/.local/bin is usually on PATH
make uninstall
```

`sqrbak` asks a running server exactly that, and takes backups:

```
sqrbak info   <db-dir>|[<host>:]<port>            # name, dir, version, txn state
sqrbak backup <db-dir>|[<host>:]<port> <file>     # write a .sqr container
```

The endpoint is a database directory unless it looks like a port (all digits,
optionally after `<host>:`).

The backup is taken **without closing the database**: the server flushes and
releases its file units, reads the snapshot, and reopens them, all while
holding its own advisory lock, so no client is disconnected and no other
process can take the database meanwhile. It is refused while a transaction is
open — a directory mid-gesture is not a state worth archiving. Restore with
`sqlsh`'s `.unpack`, which is the same container format as `db_pack` (see
[Single-file container](#single-file-container-sqr)).

```
$ sqrbak info ~/db/current
name = current
dir = /home/simon/db/current
realdir = /home/simon/db/accounts-2026
server = sqrd 1.0.0
protocol = 1
readonly = no
txn = no
tables = 12
$ sqrbak backup ~/db/current accounts-$(date +%F).sqr
packed /home/simon/db/current -> /home/simon/backups/accounts-2026-08-16.sqr
```

Symbolic links are kept, not corrected: `dir` is the directory as the daemon
was told to open it, and `realdir` (shown only when it differs) is the
resolved identity. A link like `db/current` is usually the name that matters,
so resolving it away would report something you never chose — but anything
that needs to know whether two paths are the same database still has an
answer. Every client matches permissively against both.

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
| `_dropping`           | names a table whose `db_drop_table` is under way; present only until the drop finishes |

A table's files are named after it, so `db_create_table` and
`CREATE TABLE` hold a new table's name to a plain set:

- of ASCII, only letters, digits, space and `_ - ! + . ,`;
- any non-ASCII character, written as well-formed UTF-8 (the control
  characters U+0080 to U+009F apart);
- no space at either end, no dot at the end, no `..`;
- no leading `_`: that prefix belongs to the engine's own files;
- not the name of an existing table in a different case (`SQR_DUP`). Lookup
  itself stays case-sensitive;
- not a name whose files are already in the directory (`SQR_DUP`).

The first four return `SQR_INVALID`. In SQL, a name with anything beyond
ASCII letters, digits and `_` must be quoted: `CREATE TABLE "café" (...)`.

sqr compares names as bytes and carries no Unicode tables. The case rule
therefore folds ASCII letters only (`café` and `CAFÉ` are two tables), and
two spellings of one accented letter - precomposed, or a letter followed by
a combining accent - are two names. A name is at most 32 bytes, which is
fewer than 32 characters once any of them is non-ASCII. The rules apply to creation only: a
table already in a database opens under whatever name it has. Column names
are not restricted in this way.

The file system has the last word. Windows and macOS ignore case in every
script, so there `CAFÉ` is refused once `café` exists: its files are
already present. The same check refuses a table whose files were left in
the directory by something else; remove them to create it.

**Windows and non-ASCII names.** Windows reads a file name in the process's
code page, which is UTF-8 only if the executable asks for it. An executable
that uses sqr should therefore carry `src/sqr_utf8.manifest` (Windows 10
version 1903 or later):

```sh
windres -I src src/sqr_utf8.rc -o sqr_utf8.o     # then link sqr_utf8.o
```

Without it a name with a non-ASCII character is refused (`SQR_INVALID`,
"non-ASCII characters need the UTF-8 code page on Windows") and ASCII names
work as usual. `make windows` links the manifest into the test executables,
and builds `utest_sqr_acp.exe` without it to check the refusal.

Each record is: 1 status byte (`ROW_ALIVE` / `ROW_TOMBSTONE`), then a
`(ncols+7)/8`-byte NULL bitmap, then column data at fixed offsets.

An index entry is the key and the row id. Each member of the key is one
flag byte (`00` NULL, `01` present) followed by the member's value bytes, so
a NULL member is part of the key and every live row has an entry in every
index. NULL orders before any value.

### Upgrading a 1.x database

sqr 2.0 changed the index key (schema version 2): 1.x keys had no flag
byte, and a row with a NULL in an indexed column was left out of the index,
which could hide it from a query that used a composite index. Table data,
blobs, the journal, the `.sqr` container and the wire protocol are
unchanged.

- **Opening a 1.x database read-write upgrades it in place**, once: every
  index is rebuilt and the schema version rewritten. No table row is
  touched. An upgrade interrupted by a crash is completed by the next
  read-write open. It takes about 0.3 s for a 100 000-row table with two
  indices.
- **A read-only open of a 1.x database is refused** with `SQR_VERSION`,
  because it cannot rebuild. Open it read-write once, or read it with
  sqr 1.8.2, the last release that reads the 1.x format. A database on
  read-only media must be copied to writable storage first.
- **A `.sqr` container** holding a 1.x database unpacks as 1.x and upgrades
  on its first read-write open.
- **sqr 1.x cannot open an upgraded database** (`sqlsh` 1.8.2 reports
  "cannot read catalog"). Keep a copy if 1.x programs still need it.

Two results change with the format. `db_open_cursor` over a whole index now
yields the rows with a NULL leading member, first; and `ORDER BY` puts
NULLs first when ascending and last with `DESC`, where 1.x put them last
and first.

---

## Single-file container (`.sqr`)

The working form of a database is always the directory above: `db_open`
opens a directory and nothing else. When one file is wanted — a backup, a
copy to another machine, a document an application saves and reopens — the
directory is packed into a `.sqr` container and unpacked again before use.

| Routine                      | What it does                                         |
|------------------------------|------------------------------------------------------|
| `db_pack(dir, file)`         | pack a database directory that no writer has open into `file` |
| `db_unpack(file, dir)`       | restore a container into a **new** directory `dir`   |
| `db_pack_live(db, file)`     | pack the database an open handle is serving, without closing it (`db%pack_live`) |

All three take the optional `stat` and `errmsg` arguments.

- **Pack is a consistent snapshot.** `db_pack` opens the database read-only
  under the shared lock and does not wait: a database open for writing
  elsewhere is `SQR_LOCKED`, and one that needs journal recovery is
  `SQR_READONLY` (open it read-write once, then pack). The lock file and the
  journal are not archived. The container is written to a temporary file,
  synced and renamed into place, so an existing `file` is replaced whole or
  not at all.
- **Size limit: 2 GiB.** The pack holds the database in memory, so a
  database with a file larger than 2 GiB, or more than 2 GiB in total, is
  refused with `SQR_FULL` and nothing is written. Copy the directory instead
  (with no writer attached).
- **Unpack never overwrites.** An existing `dir` is `SQR_DUP` (`out/` and
  `out` are the same target). The checksum is verified before anything is
  written. Only the names `db_pack` writes are accepted: `_catalog.dat`, and
  a table's `.schema`, `.dat`, `.blob` and `.idx` files; a journal, a lock
  file or anything else in the container is `SQR_INVALID`, and a name that
  appears twice is `SQR_ERR`. The files are extracted into a temporary
  directory `<dir>.unpack-tmp` that is renamed into place, so a failure
  leaves no partial database. A truncated or damaged file is `SQR_ERR`; a
  newer container version or the other byte order is `SQR_VERSION`.
- **One unpack to a given target at a time.** An unpack removes a
  `<dir>.unpack-tmp` left by an abandoned one, so two unpacks to the same
  target at once would work in each other's temporary directory.
- **`db_pack_live` is the hot backup.** It is what `sqrd` runs for a `PACK`
  request and so what `sqrbak backup` uses: the handle's file units are
  flushed and released, the snapshot is read, and the units are reopened, all
  under the handle's own lock. It is refused (`SQR_INVALID`) while a
  transaction is open.

The format is a plain archive, not a compressed one: a header (magic `SQRP`,
version, byte-order mark, file count, checksum), a table of contents, then
each file's bytes. A container is therefore about the size of the directory
it came from, and is read on a machine of the same byte order. Containers
are written as version 2, whose checksum covers the table of contents as
well as the file bytes; version 1 containers (sqr 2.0.0 and earlier) are
still read. sqr 2.0.0 and earlier refuse a version 2 container with
`SQR_VERSION`.

From the shells:

```
sql> .pack   db/accounts accounts.sqr
sql> .unpack accounts.sqr restored

sqr> pack   db/accounts accounts.sqr
sqr> unpack accounts.sqr restored
```

A program that wants a single-file document does the same around its own
open and save: `db_unpack` into a scratch directory, `db_open` that, and
`db_pack` (or `db_pack_live`) back to the document on save.

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

Three behaviours of that path are worth knowing:

- **A damaged journal stops the open.** If `db_open` finds a journal that
  is live but fails its checks (bad length, checksum or records), the
  interrupted transaction can no longer be rolled back. The open fails with
  `SQR_ERR` and the journal is left untouched, rather than being discarded
  and the half-applied transaction served as a clean database. The message
  names the way out: removing `_journal.dat` opens the database as it
  stands (then `db_verify` each table).
- **A rollback that fails latches the handle.** After an I/O failure in the
  middle of a rollback the journal stays on disk and every write on that
  handle is refused with `SQR_ERR` until the database is closed and
  reopened, which completes the rollback. Reads still work.
- **A compact or column change that fails past its commit point wedges the
  table**, as does a compact, column change or drop table whose marker
  could not be written or withdrawn cleanly. The files on disk are consistent and the next read-write open
  finishes the job, but for the rest of the session reads of that table fail
  and every write on the handle is refused with `SQR_ERR` ('table "<t>"
  needs recovery: close and reopen the database'). `db_close` still
  succeeds.
- **A drop table interrupted by a crash is finished by the next open.**
  `db_drop_table` writes `_dropping` first; the next read-write open (or the
  same handle's `db_close`, after a failure) removes whatever is left of the
  table, so its name can be used again. A read-only open that finds the
  marker is refused with `SQR_READONLY`. A marker that was never completely
  written (power lost while it was being made) means no drop began: the
  next read-write open removes it and a read-only open ignores it.
- **One transaction can journal at most 2 GiB of undo data**
  (`SQR_JRNL_MAX_PAYLOAD`). The write that would pass it fails with
  `SQR_FULL`; an explicit transaction is then doomed and rolls back whole.
  This bounds what a single transaction changes, not the size of a table
  or a database — split a very large update across several transactions.

A store the host can rebuild, or one it will make durable itself when it
is finished (an application's working scratch, a cache, a copy being
assembled before an atomic rename), can be opened with
`db_open(..., durable=.false.)`: every `fsync` is skipped — journal, commit
barrier and the schema/catalog replace alike — while the writes, flushes,
transactions, rollback and the atomic renames all stay, so the running
process never sees a torn file. Only a crash can, and that is the trade the
flag names: after a power loss such a store may have a torn journal, and
the open is then refused as described above. Its `_dropping` marker is not
synced either, so after a power loss during `db_drop_table` the marker may
be found incomplete and discarded although the drop had begun; the table's
leftover files then stay where they are.

Two things the engine avoids on its own account, because their cost grows
with the number of open tables: a name-based `inquire`/`open` (the ifx
runtime resolves the real path of every connected unit to answer one — file
lengths come from `stat` via `c_file_size`, and the journal keeps one unit
open for the session instead of reopening per write), and rewriting
metadata that has not changed (a close or quiesce rewrites a table's schema
only when its counters moved, and the catalog only when the table list
did — a clean open+close touches nothing on disk).

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
  classes into compile-time errors. The project builds warning-clean and
  passes every suite on four compilers (ifx, gfortran, LLVM flang and AMD aof), and
  the destruction harness runs
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

[`SQRSH-REFERENCE.md`](SQRSH-REFERENCE.md) defines the `sqrsh` shell in the same way: states,
commands, arguments, output, messages and open points.

[`SQL-REFERENCE.md`](SQL-REFERENCE.md) defines the SQL subset: lexical rules, types, every
statement, results, error messages, limits, a grammar, and the points still
open.

`make docs` generates the full FORD API reference into `ford_docs/`.

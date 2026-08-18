# sqrd wire protocol — design (v1)

Agreed 2026-08-10. Protocol between `sqrd` (the sqr database server) and its
clients: the LibreOffice Calc Python macro and the ODBC driver. Goal: expose
one `.sqr` database read/write over a socket with zero growth of the engine
beyond the two touches listed at the end.

## Architecture

- One `sqrd` process owns one database, named at startup. A DSN is host:port;
  serving a different database means another instance on another port. Because
  the endpoint is the only thing a client chooses, the *physical* location must
  be discoverable from the connection — `INFO` reports the absolute directory
  (added 2026-08-15), and `sqrd` prints it in its startup banner. Without that,
  a client can be connected to the wrong database and never know.
- TCP, localhost by default. Once ready, `sqrd` prints `LISTENING <port>` on
  stdout (the one machine-readable line — port 0 requests an ephemeral port
  and this is how a launcher learns the choice); all chatter goes to stderr.
- Strict request/response: one outstanding request per connection. The server
  poll()-multiplexes connections but executes statements serially — matching
  the engine's single-transaction journal, so no engine locking is needed.
- Statements (including BEGIN/COMMIT/ROLLBACK) go through the existing
  `sql_run`/`sql_exec` path; responses mirror `sql_result_t` directly.

## Framing

Every message is one printable header line, space-separated tokens,
terminated by `\n`, optionally followed immediately by a byte-counted
payload. Headers are human-readable (telnet-debuggable); payloads are
counted, so arbitrary bytes — including newlines and NULs — need no escaping.

Limits: SQL payload capped at 1 MiB; header line at 1 KiB. Exceeding either
is a protocol error and the server closes the connection.

## Session

```
C: HELLO 1 <client-name>            protocol major version + free-text name
S: OK 1 sqrd <version> <dbname>     or ERR on version mismatch
```

The protocol major version only changes on incompatible framing changes;
additive requests or wire types do not bump it (an unknown request gets ERR).

## Requests

```
SQL <nbytes>        payload = one SQL statement
TABLES              catalogue: table names (ROWS response)
COLUMNS <table>     catalogue: name, type, csize, nullable, key-ordinal per column
INFO                where and what this database is (MSG response)
PACK <nbytes>       payload = absolute destination path; back up (MSG response)
PING                liveness check (NONE response)
QUIT                orderly close
```

`INFO` and `PACK` were added 2026-08-15 (additively — no version bump) after a
Base session turned out to be pointed at a CAD scratch database with no way to
tell from the client. They are the enquiry and backup halves of the same
problem: knowing which directory on disk a connection actually reaches.

`INFO`'s payload is one `key = value` line per fact, so keys can be added
without a protocol change and a human can read it straight off a telnet
session:

```
name = profile_01
dir = /home/simon/db/current
realdir = /home/simon/db/accounts-2026
server = sqrd 1.0.0
protocol = 1
readonly = no
txn = no
tables = 12
```

`dir` is the point of the request: it is absolute, and nothing else on the
wire carries it. It is the directory **as the daemon was told to open it**,
made absolute but not resolved — a symlink is usually a deliberate name
(`db/current` pointing at a dated directory) and resolving it would report
something the caller never chose. `realdir` carries the resolved identity and
appears only when the two differ, so anything that must decide whether two
paths are the same database has an answer without the name being taken away
from anyone. Note that `..` cannot be folded out of `dir` textually:
`a/link/..` is not `a` when `link` is a symlink.

Clients match permissively — any spelling that lands on the same directory is
accepted, against either key.

`PACK` writes a `.sqr` container — the same format `db_pack`/`db_unpack`
already use — **without closing the database**. See "Backup" below.

`TABLES` and `COLUMNS` exist so the ODBC driver's `SQLTables`, `SQLColumns`
and `SQLPrimaryKeys` are answered from `sqr_admin` — no catalogue SQL, no
fake system tables. The key-ordinal column is what lets LO Base decide a
result set is editable.

## Responses

One per request, mirroring the four `sql_result_t` kinds:

```
NONE                              SQLRES_NONE (PING, QUIT)
COUNT <n>                         SQLRES_COUNT (DML row count)
MSG <nbytes> + payload            SQLRES_MSG (DDL and BEGIN/COMMIT/ROLLBACK
                                  status text — the engine reports these as
                                  messages, and responses mirror the engine)
ERR <stat> <nbytes> + payload     engine stat code + errmsg text
ROWS <nrows> <ncols>
COL <name> <type> <csize> <null|notnull> <key-ordinal>    x ncols
<cells, row-major>
END
```

Each cell is `C <nbytes>\n<bytes>`, or bare `N` for SQL NULL. NULL travels
as its own token — the engine's `is_null` flag is the source of truth, so a
CHAR value containing the string `NULL` stays unambiguous. `END` is
redundant given the counts but serves as a cheap frame-sync check.

Result sets are fully materialised (they already are engine-side); no
cursor/pagination machinery in v1.

## Value encoding

Cell payloads are typed binary; the client uses the `COL` type to decode.
Wire byte order is little-endian (a straight `transfer` on x86-64/ARM).

| wire type | payload                              | ODBC mapping        |
|-----------|--------------------------------------|---------------------|
| `INT`     | 4 bytes, stored value verbatim       | `SQL_INTEGER`       |
| `REAL`    | 8 bytes, IEEE 754 double bits        | `SQL_DOUBLE`        |
| `CHAR`    | bytes, trailing blanks/NULs trimmed  | `SQL_VARCHAR(csize)`|
| `TEXT`    | bytes as-is                          | `SQL_LONGVARCHAR`   |

Binary numerics make the read path exact by construction. CHAR is trimmed on
the wire — trailing blanks are never significant (agreed 2026-08-10) — and
therefore maps to `SQL_VARCHAR`, not `SQL_CHAR`, so no client re-pads.

No boolean: sqr has no logical column type, matching SQLite (whose semantics
the sqllogictest harness validates against; TRUE/FALSE there are aliases for
1/0), MySQL (TINYINT alias) and SQL Server (BIT). The convention is an INT
column holding 0/1. If DT_BOOL is ever added to the engine, `BOOL` (1-byte
payload, SQL_BIT) joins this table additively — no version bump.

### Write-path precision rule (driver-side)

Writes travel as SQL text (`UPDATE t SET x = <literal>` through
`sql_parse`). Drivers MUST format doubles with 17 significant digits
(`%.17g` / `es23.16e3`-class): text -> double -> text round-trips exactly at
17 digits. No binary literal form is added to the SQL grammar; `%.17g`
already solves the problem the grammar extension would address.

## Transactions and contention

No protocol-level transaction messages — BEGIN/COMMIT/ROLLBACK are SQL. The
server tracks which connection holds the open transaction; a write from any
other connection while one is open gets `ERR` with a distinct BUSY stat
code — 100, outside the engine's `SQR_*` range (ODBC driver maps it to
SQLSTATE 40001). Immediate-error is the v1
policy; a bounded wait can be added later without protocol change. ODBC
autocommit on/off maps to the driver emitting BEGIN/COMMIT around
statements.

A dropped connection with an open transaction is rolled back by the server.

## Backup

Agreed 2026-08-15. `PACK` produces a consistent snapshot **without closing the
database** — the database a client is connected to must not go away because a
backup ran.

Blocking and draining need no machinery: `serve_step` is single-threaded and
services one request per poll cycle, so while the server is inside the `PACK`
handler nothing else can be running. The only state that straddles requests is
an explicit transaction, so `PACK` is refused with `SQRD_STAT_BUSY` (100) while
`txn_owner` is set — a directory mid-gesture is not a committed state, and the
journal would be hot.

What the snapshot *does* need is the file units. `table_t` holds persistent
units for `<table>.dat` and `.blob`, `index_t` for its B+-tree, and the packer
reads those same files through fresh stream units — which F2018 12.5.1 forbids
("a file shall not be connected to more than one unit at the same time"). Hence
the engine pair behind the request:

- `db_quiesce` — persist what lives only in memory (the catalog, and the
  per-table `next_id`/`live_count` that are otherwise written at `db_close`),
  then flush and close every data, blob and index unit. The handle stays open,
  the tables and schemas stay in memory, and the exclusive advisory lock stays
  held, so no other process can take the database while the snapshot is read.
- `db_resume` — reopen those units from the same on-disk state.

`db_pack_live` is `quiesce → pack → resume`. It skips the shared lock that the
offline `db_pack` takes, because the handle's own exclusive lock is stronger;
that lock is also why offline `db_pack` cannot be used here even from inside
the same process — `flock` treats a second open of `_lock` as an independent
holder and denies it. A failed resume leaves the handle wedged exactly as a
failed `db_compact` reopen does (units = -1, on-disk state intact): the caller
must reopen, and `PACK` says so rather than pretending to have recovered.

The destination must be an **absolute** path. A relative one would resolve
against sqrd's working directory, which no client can see — the very confusion
that motivated `INFO`. A destination inside the database directory is refused
too: the container and its `.tmp` sibling would then sit among the files a
later reader enumerates. `sqrbak` resolves relative names against its own
working directory before sending, so the rule never reaches the user.

Note the capability this grants: any client that can reach the socket can make
`sqrd` write a file anywhere its user can write. sqrd binds loopback only and
runs as the user whose data it serves, so no privilege boundary is crossed, but
it is a real widening of what a connection can do and is recorded here as such.

## Finding the daemon: `<db-dir>/_sqrd`

Agreed 2026-08-16. A port number is only a *rendezvous* — it exists because
two unrelated processes must agree on an address before they share any
channel. Where a channel already exists it is unnecessary, which is why every
test harness starts `sqrd <db> 0` and reads the port back off the `LISTENING`
line. ODBC is the awkward case: LibreOffice is nobody's child, and the DSN is
the only channel. But a DSN is just a rendezvous file, and a port number is a
poor thing to put in it — it identifies nothing a user recognises, and it goes
stale the moment a daemon restarts elsewhere.

The database directory is the better rendezvous, and it is already in hand. On
startup `sqrd` writes `<db-dir>/_sqrd`:

```
pid = 1459619
port = 7477
host = 127.0.0.1
```

so a client names the database and looks the port up. A DSN becomes
`DATABASE=/home/simon/db/accounts` with no `PORT` at all; `sqrbak` takes a
directory wherever it takes an endpoint.

**The file is a hint, never a fact.** `sqrd` has no orderly shutdown — it is
killed, and the journal covers that — so nothing removes it: a stale `_sqrd`
outlives every daemon that ever served the directory, and startup simply
overwrites it. Every reader therefore *verifies*: connect, ask `INFO`, and
check the directory it reports against the one asked for. That single test
also disposes of a reused pid and a recycled port, which is why no liveness
check is attempted on the file itself — a pid that exists proves nothing, and
neither does something listening on the port.

Two consequences worth knowing:

- `db_pack` enumerates a fixed file list (catalog, per-table schema/data/blob/
  index), so `_sqrd` is skipped from containers exactly as `_lock` and
  `_journal.dat` are. A restored directory carries no stale runtime state.
- The advisory lock admits many read-only opens of one directory, and each
  would overwrite the others' `_sqrd`. Last writer wins; the verify step keeps
  it honest, but a client can only ever find one of them. Not worth machinery
  until there is a reason to run several.

Because the file lives *in* the directory, it is found by whatever path
reaches it — the whole scheme is symlink-agnostic by construction, which is a
further argument for it over a number.

## Engine touches (the complete list)

1. **Binary cell mode** — the server cannot use `render_cell`'s formatted
   numerics (`es15.8` loses REAL precision). Add a render-mode flag so cells
   carry `transfer(value)` bytes for INT/REAL instead of formatted text.
   `cell%text` is deferred-length `character` and holds arbitrary bytes, so
   `sql_result_t` needs no structural change. The REPL keeps the formatted
   path; the test suite's golden values are untouched.
2. **Column metadata in `sql_result_t`** — add `coltypes(:)`/`colsizes(:)`
   (+ nullable/key-ordinal) filled during exec, which already has the column
   descriptors in hand. Load-bearing: clients need the type to decode binary
   cells, and `COL` lines need the rest.

Everything else is additive: the socket shim extends `osshim.c`/`clib_wrap`,
`sqrd` is a new app alongside `sqlsh`/`sqrsh`, and the ODBC driver is its own
project consuming this protocol.

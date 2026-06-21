# sqlt — sqllogictest-subset functional tests for `sqlsh`

A small Tcl harness that replays [sqllogictest](https://www.sqlite.org/sqllogictest/)
records through the `sqlsh` REPL and diffs the results against expectations.

sqllogictest is the engine-agnostic record/replay format Richard Hipp created to
cross-check SQLite against PostgreSQL/MySQL, so it carries no SQLite-specific
assumptions — which makes it a natural body of black-box tests to point at sqr's
SQL subset. It implements the slice of the format that sqr's subset can actually
exercise, and runs curated, hand-authored records rather than ingesting SQLite's
multi-million-query corpus wholesale.

## Running

```sh
make sqlttest                 # builds sqlsh, runs sqlt/tests/*.test
# or directly:
tclsh sqlt/run_sqlt.tcl <path/to/sqlsh> sqlt/tests/*.test
```

Requires `tclsh` and the system `md5sum` (for the hash record form). No tcllib.
Exit status is non-zero if any record fails, and each failure prints its
location, SQL and an expected-vs-got diff.

## How it drives sqlsh

`sqlsh` takes a database directory as its first argument, reads SQL from stdin,
writes query results to **stdout** and errors to **stderr** (`error: ...`), and
always exits 0. The harness runs **each record as its own `sqlsh` invocation**
against a single database directory that persists on disk for the whole file —
sqr auto-commits and fsyncs per mutator, so a later process sees an earlier
one's writes. That makes output attribution trivial: an invocation's stdout is
exactly that statement's result, and its stderr flags failure.

Query results are parsed from sqlsh's rendered table: the row of dashes under
the header gives the exact column slices, so each data row is cut at those fixed
offsets (robust to spaces inside values).

## Supported record format

```
# comment

statement ok
<sql>

statement error
<sql>

query <types> [nosort|rowsort|valuesort] [label]
<sql>
----
<expected value per line>          # or:  N values hashing to <md5hex>
```

- **types** — one letter per column: `I` integer, `R` real, `T` text/char.
- **sort modes** — `nosort` (default), `rowsort` (sort whole rows), `valuesort`
  (sort individual values).
- **value canonicalisation** (matches sqllogictest): `NULL` → `NULL`; empty text
  → `(empty)`; reals → 3 decimal places (sqlsh renders `es15.8`, the harness
  reformats).
- **directives** — `halt`, `hash-threshold N` (ignored), and `skipif <db>` /
  `onlyif <db>` conditionals (the engine name is `sqr`).

## Scope / limitations

- sqr's subset has **no JOIN, aggregates/GROUP BY, subqueries, LIKE or IN**, and
  is **statically typed per column** (not SQLite's dynamic type affinity). The
  great majority of the upstream corpus exercises exactly those features, so
  porting is a matter of **filtering to the supported subset**, not bulk import.
  `tests/errors.test` documents the boundaries via `statement error` records.
- The harness parses sqlsh's human-readable table. That is robust here (rows are
  cut at the dashes-line column offsets), but a machine-readable `.mode list`
  output in sqlsh (one value per line) would make result parsing exact rather
  than layout-derived — the natural next step if the suite grows substantially.
- Crash/durability tests from upstream are not portable and not needed: sqr's
  own fault-injection sweep (`fault/`) already covers that dimension.

## Files

- `run_sqlt.tcl` — the harness.
- `tests/select1.test` — projection, WHERE, ORDER BY, LIMIT, rowsort.
- `tests/where_null.test` — NULL, `IS [NOT] NULL`, `BETWEEN`, `(empty)`.
- `tests/errors.test` — statements the subset rejects (boundary documentation).

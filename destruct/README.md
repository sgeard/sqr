# destruct — destruction testing

Black-box sibling of `fault/`. Where the fault sweep injects **I/O failures**
through the `sqr_fault` seam, this injects **bad data**: deliberately corrupt
bytes in every on-disk artefact, hostile text into the SQL front-end, and
hostile arguments into the public API.

The contract under test, in one line:

> The engine may return any error it likes, but it may not crash, abort, hang
> or read/write outside its buffers.

Run it with `make destruct` (Make-only; `destruct/` sits outside the
directories `fpm` globs, like `fault/` and `bench/`). It takes several minutes
and is deliberately not part of `all`.

## The programs

| | |
|---|---|
| `mkdb.f90` | Builds the victim database: INT/REAL/CHAR/TEXT columns, NULLs, tombstones, a populated blob, two tables, and four multi-level B+-trees (single-column, composite, unique). Everything downstream corrupts a copy of it. |
| `hammer.f90` | Drives the whole public API over a (corrupted) directory — open, verify, per-row typed reads, `db_scan`, every `find_by_*`, cursors and ranges, insert/update/delete/text, transactions, undo/redo, create/drop index, compact, add/drop column, pack/unpack, close. Exit 0 means it survived; 3 means a cursor or scan ran away. Modes: default read-write, `ro` read-only, `wo` write-only (skips the read probes so a defect that only the write path reaches is not masked by an earlier read). |
| `unpack_probe.f90` | Feeds a corrupt `.sqr` container to `db_unpack`, then opens whatever came out. |
| `apiabuse.f90` | One hostile-argument case per process (short row buffers, wrong column descriptors, oversize names, use-after-close, nested transactions, …). |

Cases 10–15 report `did not return`, and are meant to. They call the `row_*`
accessors with a buffer shorter than the record, or with a `column_t` whose
offsets fall outside it — the one documented precondition the library does not
re-check, because those helpers are the hot path of every insert and every row
read (see the `Row buffer helpers` block in `sqr.f90`). Under `-check all` that
is a bounds abort, which is the diagnostic a caller wants. Every other case
must return a status.

## The corpora

| | |
|---|---|
| `corrupt.tcl` | On-disk corruption. Field-by-field mutation of `_catalog.dat`, `*.schema`, `*.dat`, `*.blob`, `*.idx` and `_lock`, hand-built hot `_journal.dat` files (including path traversal and lying length fields), plus blind bit-flips, byte-runs and truncations across all nine files. ~1900 cases. |
| `corrupt_pack.tcl` | The `.sqr` single-file container: header, TOC entry names/sizes/offsets, truncation, and random mutation. ~370 cases. |
| `sqlfuzz.tcl` | SQL text through `sqlsh`: handcrafted nasties (100 kB identifiers, 5000-deep nesting, unterminated literals, embedded NULs, 400-digit numerals), grammar-aware token soup, raw byte garbage, and the dot meta-commands. ~680 cases. |

Each driver takes an optional filter glob as its last argument for a quick
pass — `tclsh corrupt.tcl pristine 'idx-*'`.

A failing case is kept: the corrupted database (or container, or SQL script)
is copied into `findings/` alongside a `.log` of the run, so it can be
replayed directly against a debugger or a differently-built binary.

## Adding a case

`corrupt.tcl` cases are one line each:

```tcl
run_case <name> {<script that mutates $WORK>} ?ro|rw?
```

The script runs with `$WORK` set to a fresh copy of the pristine database.
`p32`/`p64`/`p8` poke a native scalar at a 0-based offset, `patch` splices raw
bytes, `truncate_to` and `append_junk` do the obvious. `sqr_checksum`
reproduces the engine's rolling payload checksum so a hand-built journal can
carry a valid one and reach the record decoder rather than being turned away
early. The layout constants of the victim database are documented in a block
comment above the first case.

## Environment knobs

| | |
|---|---|
| `HAMMER`, `PROBE` | Path to the exerciser / unpack probe (default `./hammer`, `./unpack_probe`). |
| `KEEPDIR` | Where failing cases are kept (default `findings`). |
| `ASAN` | Set when the binaries are AddressSanitizer builds: drops the `ulimit -v` cap, which ASan's shadow mapping cannot live under, and raises the per-case timeout. |

## Running against other builds

The corpora find *reachability*; the build decides what a defect looks like
when it is reached. Worth running all three:

```sh
make destruct                                  # ifx, -check all: precise diagnostics
make F=gfortran destruct                       # second front end
```

and, for anything that survives, replaying `findings/` under an optimised
build (where an out-of-bounds access is silent until it is not) and under
`gfortran -fsanitize=address` (which names the overflow and its direction).
The 2026-08-06 sweep used exactly that trio; see
`reports/DESTRUCTION-TEST-2026-08-06.md`.

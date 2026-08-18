# LibreOffice Calc client for sqr

A two-file client that makes a Calc spreadsheet a read/write front-end for
one sqr database served by `sqrd` (the wire protocol is defined in
`../reports/DESIGN-wire-protocol.md`).

| file          | role                                                        |
|---------------|-------------------------------------------------------------|
| `tclcalc.py`  | generic pyuno macro host: sheets/cells/message box only, no sqr knowledge; embeds a Tcl interpreter (`tkinter.Tcl()`) per invocation |
| `sqr_calc.tcl`| the director: wire-protocol client, configuration, pull/push logic |
| `test_calc.tcl`| functional test (`make calctest`): the real director against a mock servant and a real spawned sqrd |

All application logic lives in the Tcl script; the Python file is glue that
should never need editing. The servant contract between them (six commands,
tagged cells `{V number}` / `{S text}` / `{N}`) is documented at the top of
`sqr_calc.tcl`.

Numbers cross every boundary — Calc → pyuno → Tcl → SQL text — as
shortest-round-trip values, so REAL columns survive pull and push
bit-exactly.

## Install

```
make install-calc
```

copies both files to `~/.config/libreoffice/4/user/Scripts/python/`.
Requires the system Python's tkinter (Mageia package `python3-tkinter` —
LibreOffice's scripting Python is the system Python). Re-run after any
change to either file; LibreOffice picks the new copies up on the next
macro invocation (restart LO if it had already cached the Python module).

## Use

1. Start the server: `sqrd <db-dir> 7477` (any port; 7477 is the
   configuration default).
2. In Calc: Tools → Macros → Run Macro → My Macros → tclcalc →
   `sqr_pull`. The first run creates a configuration sheet named `sqr`:

   | A      | B          |
   |--------|------------|
   | host   | 127.0.0.1  |
   | port   | 7477       |
   | table  | *(fill in)*|

   Set the table name and run `sqr_pull` again.
3. The table appears on a sheet named after it: headers in row 1, one row
   per record, NULLs as empty cells, sorted by the table's unique key.
4. Edit values, add or delete rows, then run `sqr_push`.

For one-click use, bind the two macros to toolbar buttons or shortcuts
(Tools → Customize → Events/Toolbars → Macro → My Macros → tclcalc).

## Semantics and contract

- **Push replaces the whole table** inside one transaction (`BEGIN`,
  `DELETE`, `INSERT` per row, `COMMIT`). Any failure — bad value, unique
  violation, server BUSY — rolls back and the table is left untouched; the
  error appears in a message box.
- **The data sheet belongs to the macros**: row 1 must hold column names
  (any subset/order of the table's columns), data rows sit contiguously
  below, and pull clears the sheet before writing. Keep formulas and
  derived views on other sheets referencing this one.
- **Empty cells are NULL**, in both directions; an empty string is treated
  as NULL too (blanks are never significant).
- INTEGER columns reject non-integral numbers; text typed into a numeric
  column is passed through for the server to parse, so `42` typed as text
  still lands in an INTEGER column.

## Testing

`make calctest` runs `test_calc.tcl`: the real `sqr_calc.tcl` driven over a
dict-backed mock servant against a real spawned `sqrd` — configuration
bootstrap, pull exactness (0.1, -1.5e-300), NULL round-trip, push,
rollback-on-error and header validation (22 checks). Only the pyuno lines
in `tclcalc.py` are outside its reach; smoke-test those in Calc once per
change there:

1. `make install-calc`, start `sqrd` on a scratch db, create a small table
   via `sqlsh` (or pull an existing one).
2. Run `sqr_pull` twice (config bootstrap, then data), edit a value, run
   `sqr_push`, and check the change with `sqlsh`.

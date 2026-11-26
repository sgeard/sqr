# sqrsh reference

`sqrsh` is the command shell that works directly on the sqr engine, with no
SQL: tables are built column by column, and rows are reached by row id, by
key or by scan. This document defines its commands, what each prints and what
each refuses. It describes sqr 2.0.0 and its trunk as of 2026-11-19, and it is
written so that tests can be derived from it without reading the source.

`SQL-REFERENCE.md` is the companion for `sqlsh`. The two shells work on the
same databases, and that document's rules for types, names and indexes apply
here too; this one states only where `sqrsh` differs. Where the present
behaviour is known to be wrong or is undecided, the text says so and points to
section 9; a test should not fix such behaviour in place.

## 1. Running it

```
sqrsh [<db-dir>]
```

- With a directory, that database is opened (and created if it does not
  exist) before the first command. If the open fails, `open <dir> failed:
  <reason>` is written to standard error, the shell starts with no database
  open, and the exit status will be 1.
- An argument beginning with `-` is refused with exit status 2: `sqrsh` has no
  options. A directory really so named is given as `./-name`.
- Commands are read from standard input, one per line of any length, until
  `quit` or the end of the input. A failed command does not stop the input.
  The database is then closed.
- A database serves one connection at a time. If another process has it, the
  open fails with `database is locked by another connection`.

**Streams.** Results and confirmations (`inserted row 3`, listings,
`(no tables)`, the `help` list) go to standard output. Every failure and
usage message goes to standard error, one line each. Two messages are
written by cmdgraph, which reads the command word, and go to standard
output: `unknown: <word>` and `ambiguous: ...`; both still count as failures.

**Prompts and the banner are written only when standard input is a
terminal**, to standard output. The prompt is `sqr> `, `cols> ` or `row> `
according to the state (section 3). Output read from a pipe or a file has
neither.

**Exit status** is 0 when nothing failed. It is 1 when any command failed
(an unknown or ambiguous command word included), the start-up open failed,
or the input is in an unsupported encoding (README, "Input encoding"), and 2
for an option.

**There are no transactions.** Each command that changes the database is
complete and durable when it reports success. There is no undo.

## 2. Command lines

A line is a command word followed by arguments, separated by spaces.

- **Command words are lower case** and case-sensitive: `OPEN` is unknown.
- **Abbreviations.** A command written `o(pen)` in section 4 may be given as
  `o`, `op`, `ope` or `open`: the part before the bracket is the least that
  must be typed. A command shown without brackets must be typed in full. If
  what is typed fits two commands the line is refused with
  `ambiguous: <word> matches ...`, naming them.
- **Arguments** are of three kinds, decided by how they are written:
  - an **integer**: an optional sign and digits, within −2147483648 to
    2147483647 (`7`, `007`, `+5`, `-9`);
  - a **real**: a number with a point or an exponent (`1.5`, `1.`, `.5`,
    `1e3`, `-1e-3`);
  - **text**: anything else, and anything in double quotes. Quotes are needed
    for text that contains a space and for the empty text `""`. Single quotes
    are ordinary characters: `'abc'` is five characters.
  - A quoted text cannot contain a double quote; there is no escape.
- A blank line does nothing. There are no comments and no way to put two
  commands on one line: `#` and `;` are ordinary arguments.
- `help` and `?` list the commands of the current state. Arguments to them
  are ignored.
- A line whose first word is not a command of the current state prints
  `unknown: <word>`.
- Arguments beyond those a command takes are ignored by `select`, `back`,
  `cancel`, `done`, `tables`, `close`, `readonly` and `quit`; the other
  commands refuse the line with their usage message.

**The word `null`.** Where a command takes a value for a column, the argument
`null`, in any letter case, quoted or not, means NULL. The text `null`
therefore cannot be stored through `sqrsh`.

## 3. States

The shell is in one of three states, shown by its prompt. Each has its own
commands; a command of another state is `unknown`.

| State | Prompt | Entered by | Left by |
|---|---|---|---|
| root | `sqr> ` | start-up | `quit` |
| creator | `cols> ` | `create <table>` | `done` (if it succeeds), `cancel` |
| table | `row> ` | `use <table>` | `back` |

`quit` is available in all three and ends the shell. A table half-defined in
the creator state is discarded by `quit`.

## 4. Commands

In this section `<table>` and `<col>` are names, `<id>` is a row id (an
integer argument) and `<v>` is a value. A command given the wrong number of
arguments prints its usage line, `usage: ...`, and does nothing.

### 4.1 Root state

Every command here except `open`, `pack`, `unpack`, `help` and `quit` needs a
database open, and otherwise prints ``no database open — use `open <dir>` ``.

| Command | Does | Prints on success |
|---|---|---|
| `o(pen) <dir>` | closes the current database, then opens or creates `<dir>` | `opened <dir> (<n> tables)` |
| `cl(ose)` | closes the database | `closed` |
| `readonly` | makes the open database read-only for the rest of the session | `database is now read-only (concurrent readers allowed)` |
| `t(ables)` | lists the tables | one line each, `  <table>  (<r> rows, <c> cols)`, or `(no tables)` |
| `d(esc) <table>` | describes a table | see below |
| `cr(eate) <table>` | starts a new table and enters the creator state | nothing |
| `u(se) <table>` | enters the table state for that table | nothing |
| `dr(op) <table>` | removes a table, its rows and its indexes | `dropped <table>` |
| `pack <dir> <file>` | archives a database directory into one file | `packed <dir> -> <file>` |
| `unpack <file> <dir>` | restores such a file into a new directory | `unpacked <file> -> <dir>` |

Failures: `open failed: <reason>`; `no database open` (`close`);
`set read-only failed: <reason>`; `no such table: <table>` (`desc`, `use`);
`table already exists: <table>` (`create`, which then stays in the root
state); `drop failed: <table>: <reason>`; `pack of "<dir>" failed: <reason>`;
`unpack of "<file>" failed: <code>`.

`desc` prints one line for the table, one per column and one per index:

```
table t: record_size=34, next_id=8, live=6
  col 1 id (int, 4 bytes)
  col 2 name (char, 8 bytes)
  col 3 x (real, 8 bytes)
  col 4 note (text, 12 bytes)
  unique index on name (6 entries, deleted rows included)
  index on        id,x (6 entries, deleted rows included)
```

`next_id` is the id the next inserted row will get; `live` is the number of
rows. The `entries` figure of an index also counts rows deleted since the
last `compact`, as it says: `delete` leaves a row's entries in the indexes,
lookups skip them and `compact` removes them (`update` does remove the old
entries). The types are `int`, `real`, `char` and `text`.

A table name with a space in it must be quoted: `use "my table"`.

### 4.2 Creator state

| Command | Does | Prints on success |
|---|---|---|
| `col <name> <type>` | adds a column to the table being defined | `  +col <n> <name> (<type>, <size>)` |
| `done` | creates the table and returns to the root state | `created table <table> with <n> columns` |
| `cancel` | discards the definition and returns to the root state | nothing |

`<type>` is one of:

| Written | Type | `<size>` shown |
|---|---|---|
| `int`, `i` | 32-bit integer | 4 |
| `real`, `r` | 64-bit real | 8 |
| `text`, `t` | text of any length | 12 |
| `char <N>`, `c <N>` | character, at most `<N>` bytes | `<N>` |

- `col` checks only the type: `unknown type: <word>`, and for `char` a
  missing width (usage) or `char size must be a positive integer`. The names
  and the table as a whole are checked by `done`.
- At most 256 columns, as `CREATE TABLE` allows: a 257th `col` prints
  `too many columns`.
- `done` with no columns prints ``no columns defined — add some with `col`
  or `cancel` `` and stays in the creator state.
- `done` applies the rules of `CREATE TABLE` (SQL reference, 4.1). If the
  table is refused it prints `create failed: <reason>` and **stays in the
  creator state with the columns as they were**. A pending column cannot be
  removed or changed: the way out of a bad definition is `cancel`, then
  `create` again.

### 4.3 Table state: rows by id

Every row has a **row id**, a positive integer given by the engine when the
row is inserted: 1 for the first row of a table, then 2, and so on. The id of
a deleted row is not used again, until `compact` renumbers the rows.

| Command | Does | Prints on success |
|---|---|---|
| `i(nsert) <v>...` | adds a row; one value per column, in column order | `inserted row <id>` |
| `u(pdate) <id> <v>...` | replaces every column of a row; one value per column | `updated row <id>` |
| `g(et) <id>` | shows one row | header and the row |
| `del(ete) <id>` | deletes one row | `deleted row <id>` |
| `s(elect)` | shows every row, in row-id order | header and the rows |

**Values.** Each `<v>` must suit its column:

| Column | Accepts | Otherwise |
|---|---|---|
| int | an integer argument | `col <n>: expected integer` |
| real | an integer or real argument | `col <n>: expected number` |
| char | any argument no longer than the column; a number is taken as its text | `value too long for column "<name>"` |
| text | any argument; a number is taken as its text | — |
| any | `null` | — |

`<n>` is the column's position, from 1. A real argument is not accepted for
an int column, even `1.0`.

- A char value longer than its column, in bytes and leaving out trailing
  blanks, is refused before anything is written. Trailing blanks of a char
  value are not kept; those of a text value are.
- The wrong number of values prints `expected <n> values` (`insert`) or
  `usage: update <row_id> followed by <n> values`.
- A row that breaks a unique index is refused, and the message gives the
  engine's reason: `insert failed: unique-key violation on index over "id"`,
  and the same for `update failed: ...`.
- `get` and `delete` of an id that is not a live row print
  `no such row: <id>`, and `update` prints `update failed: no such row
  <id>`. Any other failure is `<command> failed: <reason>`. An id that is not
  an integer prints `row_id must be an integer`.
- `insert` and `update` are each one step: the row and its text values are
  written in one transaction, so a command that fails leaves the table as it
  was.
- `select` that cannot finish its scan prints `scan failed: <reason>` after
  the rows it printed.

### 4.4 Table state: indexes and keys

| Command | Does | Prints on success |
|---|---|---|
| `ind(ex) [unique] <col>...` | creates an index on the columns, in that order | `indexed <n> column(s)` |
| `dropindex <col>...` | removes the index with exactly that column list | `index dropped` |
| `f(ind) <col> <v>` | shows one row whose column equals the value | header and the row |
| `r(ange) <col> <lo> <hi>` | shows the rows with `<lo>` ≤ column ≤ `<hi>` | header and the rows |
| `getk <col> <v> [<col> <v>]...` | shows the row with that key | header and the row |
| `delk <col> <v> [<col> <v>]...` | deletes the row with that key | `deleted by key` |

`index` must be typed at least as `ind`; `in` is `insert`.

The rules for indexes are those of the SQL reference, 4.4: no text column, no
repeated column, one index per column list, and a unique index exempts rows
with NULL in an indexed column. `index` fails with `create_index failed:
<reason>`; `dropindex` with `dropindex: no index over those columns`.

**`find` and `range` need an index whose first column is that column.**

- `find` shows one matching row; if several match, which one is not
  specified. With no match it prints `no match`. With no index it prints
  `no index on that column`. A char value too long for the column is
  refused as for `insert`.
- `range` shows the matching rows in ascending order of the column, and rows
  equal in the column in row-id order. Rows with NULL in the column are never
  shown. With `<lo>` greater than `<hi>`, or nothing in the band, it prints
  the header and `(no rows in range)`. With no index it prints
  `no index on that column`.
- The value or bounds must suit the column: `value must be an integer`,
  `value must be a number`, `bounds must be integers`, `bounds must be
  numbers`. `null` is not accepted here; it is read as text. A text column
  prints `find on this column type not supported` or `range on this column
  type not supported`. An unknown column prints `no such column: <col>`.

**`getk` and `delk` need a unique index whose column list is exactly the
columns given, in the same order.** With an index on `(k1, k2)`,
`getk k1 1 k2 aa` finds the row and `getk k2 aa k1 1` does not. When there is
no such index they print `no unique index over those columns`; when there is
no such row, `no row for that key`. Arguments not in pairs print
`usage: <col> <value> [<col> <value> ...]`. The values are checked as for
`insert`, except that `null` is ordinary text here.

### 4.5 Table state: the table itself

| Command | Does | Prints on success |
|---|---|---|
| `addcolumn <name> <type>` | adds a column after the others; existing rows have NULL in it | `added column <name> (<type>)` |
| `dropcolumn <name>` | removes a column, and every index that includes it | `dropped column <name>` |
| `co(mpact)` | reclaims the space of deleted rows and **renumbers the rows** 1, 2, ... in their existing order | `compacted <n> live rows` |
| `v(erify)` | checks the table and its indexes for damage | `verify: OK` |
| `b(ack)` | returns to the root state | nothing |

`<type>` is as in 4.2. Failures: `addcolumn failed: <reason>`,
`dropcolumn failed: <reason>` (an unknown column; the only column of the
table), `compact failed: <reason>`, `verify: FAILED — <reason>`.

Row ids survive `addcolumn` and `dropcolumn`. They do not survive `compact`:
an id noted before it may name a different row afterwards.

### 4.6 The optional `match` command

```
m(atch) <col> <regex>
```

`match` exists only in a shell built with `make sqrsh-regex`; in the ordinary
build it is `unknown`. It shows every row whose char column matches a Tcl
advanced regular expression, anywhere in the value, by scanning the table;
no index is needed or used. A NULL matches nothing, whatever the pattern;
an empty value is matched like any other. With no match it prints the header
and `(no rows match)`. Failures: `no such table: <table>` (the table was
dropped meanwhile), `no such column: <col>`, `match is only supported on char
columns`, `bad regex: <regex>`, `scan failed: <reason>`. The regex engine
also writes its own line, `***Syntax error in re: <regex>`, to standard
output for a bad pattern.

## 5. How rows are shown

`select`, `get`, `find`, `range`, `getk` and `match` print a header line and
then one line per row:

```
  row_id                id              name                 x              note
       1                10            Alicia    9.50000000E+00  changed
       3                 3                 c              NULL              NULL
```

- `row_id` is right-aligned in 8 columns.
- Each further column is preceded by two blanks and, except a text column,
  right-aligned in 16 characters:
  - an int in plain decimal;
  - a real as `d.ddddddddE±dd`, eight decimals, as `sqlsh` shows it: a
    three-digit exponent is written in full (`1.00000000E-300`) and zero
    is `0.00000000E+00`, without a sign;
  - a char value without its trailing blanks;
  - NULL as the four letters `NULL`.
- A text value is printed in full, not aligned and not padded.
- A column name or a char value longer than 16 characters is written in
  full, and the later fields of that line move right. Width is counted in
  characters, not display cells: a wide or combining character still
  misaligns.
- There is no count line. `select` on an empty table prints the header only.

## 6. Read-only

After `readonly`, every command that would change the database fails, and it
cannot be undone except by `close` and `open`. Each failure message gives the
engine's reason, which says the database is read-only: `insert failed:
<reason>`, `delete failed: <reason>`, `compact failed: <reason>`,
`create_index failed: <reason>`, `drop failed: <table>: <reason>`.

## 7. What `sqrsh` cannot do

- Choose columns, filter on more than one condition, order or count rows.
  `sqlsh` does these.
- Change one column of a row: `update` replaces the whole row.
- Group changes into a transaction, or undo one.
- Store the text `null`, a text containing a double quote, or a char value
  with trailing blanks.

## 8. Limits

| Limit | Value |
|---|---|
| columns in a table made by `create` | 256, as `CREATE TABLE` |
| name | 32 bytes |
| `char <N>` | 1 ≤ N ≤ 65536 |
| int | −2147483648 to 2147483647 |
| field width of the row display | 16 characters; longer values are shown in full |

## 9. Open points

Behaviour here is wrong, inconsistent or undecided. Each will be ruled on and
this document changed to match. Labels are those of
`reports/FABLE-REVIEW-2026-11-13.md`; the unlabelled ones were found while
writing this document.

| | Present behaviour | Question |
|---|---|---|
| — | The word `null` is NULL even when quoted, so the text cannot be stored. | Quoted `"null"` should be text. Waits for cmdgraph. |
| — | A backslash before a double quote inside a quoted text is kept, and the quote does not end the text: `"x \"q\" y"` is stored as `x \"q\" y`. cmdgraph documents no escape. | Decide in cmdgraph. Waits for cmdgraph. |
| — | A real with an empty exponent (`1e`) is 1.0 in the ifx build and is not a number in the gfortran build. | Refuse it. Waits for cmdgraph. |
| — | Commands are case-sensitive; SQL keywords are not. | Intended (cmdgraph); stated so it is not taken for a defect. |

## 10. Notes for writing tests

- Feed commands on standard input; read results from standard output and
  failures from standard error. Input that is not a terminal gets no banner
  and no prompts, so there is nothing to strip.
- A change is durable when its command reports success, so a database may be
  built in one process and examined in another, by `sqrsh` or by `sqlsh`.
  Only one process may have it open at a time.
- The exit status is 1 when any command in the run failed.
- Compare reals as numbers parsed from the display, not as text.
- `sqlt/run_shell.tcl` holds the existing functional tests of both shells.
- Do not write a test that requires the behaviour listed in section 9.

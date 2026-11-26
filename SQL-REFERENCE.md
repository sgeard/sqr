# sqr SQL reference

This document defines the SQL that sqr accepts: what may be written, what it
means, what comes back, and what is refused. It describes the `sql` module of
sqr 2.0.0 and its trunk as of 2026-11-19, and it is written so that tests can
be derived from it without reading the source.

Three things run this SQL, and all three share one lexer, parser and executor:

| Route | How a statement arrives |
|---|---|
| `sqlsh` | one statement per input line |
| `sqrd` (wire protocol) | one statement per `SQL` request, at most 1 MiB |
| the `sql` module | `sql_run(db, text, res, stat, errmsg)`, one statement per call |

Sections 1 to 9 hold for all three. Section 10 is `sqlsh` alone. Where the
present behaviour is known to be wrong or is undecided, the text says so and
points to section 13; a test should not fix such behaviour in place.

## 1. Statements and lines

- One statement is one line of text. A statement cannot continue onto a second
  line, and a line cannot hold two statements.
- A statement may end with one `;`. Nothing may follow it.
- A blank line, and a line holding only `;`, do nothing and report nothing.
- There are no comments: `--` and `/* */` are errors.
- A line ending of LF or CR LF is accepted by `sqlsh`.

## 2. Lexical structure

**Encoding.** Text is ASCII or UTF-8 and is handled as bytes. Lengths, widths
and column positions in error messages count bytes, not characters. Other
encodings are refused before any statement is read (README, "Input encoding").

**White space** is the space and the tab. It separates tokens and is otherwise
ignored. It is required only where two tokens would otherwise run together.

**Words** begin with an ASCII letter or `_` and continue with ASCII letters,
digits and `_`. A word is a keyword or a name according to where it stands.

**Keywords** are matched without regard to case (`select`, `SELECT`,
`SeLeCt`). The keywords are:

```
ADD ALTER AND AS ASC BEGIN BETWEEN BY CHAR CHARACTER COLUMN COMMIT CREATE
DELETE DESC DROP ESCAPE FROM IN INDEX INSERT INT INTEGER INTO IS LIKE LIMIT
NOT NULL ON OR ORDER REAL ROLLBACK SELECT SET TABLE TEXT UNIQUE UPDATE
VALUES WHERE    AVG COUNT MAX MIN SUM
```

No word is reserved. Where the grammar expects a name, any word is taken as a
name, so `CREATE TABLE select (from INT, where INT)` is valid, and so is
`SELECT from, where FROM select WHERE where = 2`. Two exceptions, both in a
select list: an unquoted `NULL` is refused, and a word directly followed by
`(` is read as an aggregate function. A quoted name avoids both.

**Names** (of tables, columns and aliases) are written as a word or as a
quoted name. The two forms are interchangeable: `id` and `"id"` are the same
name.

- A quoted name is enclosed in double quotes; `""` inside it stands for one
  double quote. It may hold any bytes except control characters (bytes 0 to 31
  and 127), and it may not be empty.
- Names are case-sensitive: `id`, `Id` and `ID` are three names.
- A name is at most 32 bytes.
- Further rules for table and column names are in section 4.

**String literals** are enclosed in single quotes; `''` inside one stands for
one single quote. A string may hold any byte the input can carry, including a
tab. Double quotes never make a string: `"Alice"` is a name.

**Integer literals** are a run of digits: `0`, `42`, `007`. With its sign
(below) the value must lie in −2147483648 to 2147483647.

**Real literals** begin with a digit and carry a fraction, an exponent or
both: `1.5`, `1.`, `1e3`, `1.5E-3`, `1.e2`. A leading point (`.5`) is not a
number. A value too large for a 64-bit IEEE double (`1e999`) is refused.

**Signs.** A literal value may be preceded by one `+` or `-`, with or without
white space between: `-6`, `- 6`, `+5`. Two signs (`--6`) are an error, and so
is a sign before a string or before `NULL`.

**`NULL`** is the keyword for the absent value. It is a literal value wherever
section 12 writes `value`, except in a select list.

**Punctuation**: `( ) , * ; = <> < <= > >= + -`. There is no `!=` and no `==`.

## 3. Types and values

Every column has one of four types, and every column may hold `NULL`.

| Type | Written | Holds |
|---|---|---|
| integer | `INTEGER`, `INT` | 32-bit signed integer |
| real | `REAL` | 64-bit IEEE double |
| character | `CHAR(n)`, `CHARACTER(n)` | a string of at most `n` bytes, 1 ≤ `n` ≤ 65536 |
| text | `TEXT` | a string of any length |

There are no other types (`VARCHAR`, `BOOLEAN`, `DATE`, ... are refused), no
column constraints (`NOT NULL`, `PRIMARY KEY`, `DEFAULT`, ...) and no
expressions: a value in a statement is always a single literal.

### 3.1 Storing a value

`INSERT` and `UPDATE` store a literal in a column under these rules. Nothing
else is converted.

| Column | Accepts | Refuses |
|---|---|---|
| integer | integer literal, `NULL` | real literal (even `1.0`), string |
| real | integer or real literal, `NULL` | string |
| character | string of at most `n` bytes, `NULL` | number; a longer string |
| text | string, `NULL` | number |

### 3.2 Character values

- **Trailing blanks are not significant and are not kept.** `'abc   '` stored
  in a `CHAR` column is `'abc'`, compares equal to `'abc'` and is returned as
  `abc`. A string of blanks is the empty string. The length limit applies
  after trailing blanks are removed.
- Leading and embedded blanks are significant.
- The empty string `''` is a value, distinct from `NULL`.
- Comparison and ordering are by byte value, so they are case-sensitive and
  put `'AB'` before `'ab'`, and every ASCII character before any non-ASCII
  one. No collation is applied.
- A character value containing control characters compares inconsistently
  (section 13, Q3).

### 3.3 Text values

A `TEXT` value is kept exactly, trailing blanks included. It can be stored,
replaced, selected, counted with `COUNT(col)` and tested with `IS [NOT] NULL`.
It cannot be compared, ordered, indexed, matched with `LIKE` or given to
`MIN`/`MAX`.

### 3.4 NULL

- `NULL` satisfies no condition except `IS NULL`. In particular a row whose
  column is `NULL` matches neither `col = 1` nor `col <> 1`, neither
  `col IN (...)` nor `col NOT IN (...)`, and so on for every other condition.
- `col = NULL` is an error, not a condition that is never true.
- Aggregates skip `NULL` (section 6.3). `ORDER BY` puts it first (section 6.2).

## 4. Tables, columns and indexes

### 4.1 CREATE TABLE

```
CREATE TABLE table ( column type [, column type]... )
```

Creates an empty table. Reports `table "<table>" created (<n> columns)`.

- 1 to 256 columns. Column names must differ (exactly; `a` and `A` differ).
- A table name must be new, and must not differ only in ASCII letter case
  from an existing table: with `t` present, `CREATE TABLE T` is refused. Yet
  lookup is exact: with `t` present, `SELECT * FROM T` is "no such table".
- A **table name** is 1 to 32 bytes, made of ASCII letters and digits, space
  and `_ - ! + . ,`, and any non-ASCII character in well-formed UTF-8 (the
  controls U+0080 to U+009F apart). It may not begin with `_` (reserved) or a
  space, may not end with a dot, and may not contain `..`.
- A **column name** is 1 to 32 bytes and may hold anything a quoted name may,
  except `/`, `\` and the sequence `..`.
- The row may not be too large: the column sizes (integer 4, real 8,
  `CHAR(n)` n, text 12) plus one byte, plus one bit per column rounded up to
  whole bytes, must not exceed 1048576 bytes.

### 4.2 DROP TABLE

```
DROP TABLE table
```

Removes the table, its rows and its indexes. Reports `table "<table>" dropped`.
A table that does not exist is an error.

### 4.3 ALTER TABLE

```
ALTER TABLE table ADD COLUMN column type
ALTER TABLE table DROP COLUMN column
```

`ADD COLUMN` appends the column after the existing ones; every existing row
has `NULL` in it. Reports `column "<column>" added`. The name must be new.

`DROP COLUMN` removes the column and its values. Every index that includes
the column is dropped with it; other indexes are kept. Reports
`column "<column>" dropped`. The only column of a table cannot be dropped.

The word `COLUMN` is required in both.

### 4.4 Indexes

```
CREATE [UNIQUE] INDEX ON table ( column [, column]... )
DROP INDEX ON table ( column [, column]... )
```

An index has no name: it is identified by its table and its column list, in
order. `(a, b)` and `(b, a)` are two indexes; `(a)` is a third. Reports
`index created`, `unique index created` or `index dropped`.

- 1 to 64 columns, none repeated, none of type text.
- At most one index per column list: with an index on `(a)`, neither
  `CREATE INDEX ON t (a)` nor `CREATE UNIQUE INDEX ON t (a)` succeeds.
- `DROP INDEX` must give the exact column list of an existing index.
- A plain index changes no result. It may change the speed of a statement and
  the order of rows returned without `ORDER BY`.
- A **unique** index refuses a second row with the same values in all its
  columns. A row with `NULL` in any indexed column is exempt, so any number of
  such rows may exist. `CREATE UNIQUE INDEX` fails if the table already holds
  duplicates. An `INSERT` or `UPDATE` that would create a duplicate fails.

## 5. Changing rows

All three statements are atomic: one that fails changes nothing (inside
`BEGIN`, see section 7). Each reports `<n> row(s) affected`.

### 5.1 INSERT

```
INSERT INTO table [ ( column [, column]... ) ] VALUES ( value [, value]... ) [, ( ... )]...
```

- Without a column list, each row gives one value per column of the table, in
  the table's column order.
- With a column list, each row gives one value per listed column; columns not
  listed are `NULL`. A column may be listed once only. At most 256 columns.
- Every row must have the same number of values.
- Values are stored under the rules of section 3.1.
- `<n>` is the number of rows inserted.

### 5.2 UPDATE

```
UPDATE table SET column = value [, column = value]... [ WHERE condition ]
```

Sets the named columns in every row that satisfies the condition; in every
row of the table if there is no `WHERE`. `<n>` is the number of rows matched,
whether or not their values changed. There is no `ORDER BY` or `LIMIT`.

A column named twice in `SET` is at present accepted and the later value wins
(section 13, Q5).

### 5.3 DELETE

```
DELETE FROM table [ WHERE condition ]
```

Deletes every row that satisfies the condition; every row of the table if
there is no `WHERE`. `<n>` is the number of rows deleted, which may be 0.

## 6. SELECT

```
SELECT * | item [, item]...
  [ FROM table
    [ WHERE condition ]
    [ ORDER BY column [ASC | DESC] [, column [ASC | DESC]]... ]
    [ LIMIT n ] ]
```

The clauses must appear in this order. One table only: there are no joins, no
subqueries, no `DISTINCT`, no `GROUP BY`, no `OFFSET` and no table aliases or
`table.column` names.

### 6.1 The select list

`*` is every column of the table, in table order. It cannot be combined with
other items. Otherwise the list is 1 to 256 items, each one of:

| Item | Result column type | Default label |
|---|---|---|
| a column | the column's | the column name |
| an integer literal | integer | the literal as written, without a `+` sign |
| a real literal | real | the literal as written, without a `+` sign |
| a string literal | character | `?column?` |
| an aggregate (6.3) | see 6.3 | `COUNT(*)`, or `FN(column)` with `FN` in capitals |

Any item may be followed by `AS name` to set its label; the word `AS` is
required. Labels need not be unique, and a column may be selected more than
once. A label is not a name: it cannot be used in `WHERE` or `ORDER BY`.

`NULL` is not allowed as an item.

**Without `FROM`** the list must hold literals only, and the result is one
row: `SELECT 1, 'a'`. No database need be open. `WHERE`, `ORDER BY` and
`LIMIT` are not allowed without `FROM`.

### 6.2 ORDER BY and LIMIT

- Each `ORDER BY` key is a column of the table, whether or not it is selected.
  It may not be a text column, a label or a position number.
- `ASC` (the default) is ascending; `DESC` is descending.
- Integers and reals order numerically, character values by byte (3.2).
- `NULL` comes first ascending and last descending.
- A later key only orders rows that are equal on every earlier key.
- Rows equal on every key keep the order they would have had with no
  `ORDER BY`.
- **Without `ORDER BY` the order of rows is unspecified.** It can change when
  an index is created or dropped.
- `LIMIT n` keeps the first `n` rows after ordering. `n` is an unsigned
  integer literal; `LIMIT 0` gives no rows; a sign, a real or a negative value
  is an error.

### 6.3 Aggregates

| Aggregate | Columns allowed | Result type | Over no values |
|---|---|---|---|
| `COUNT(*)` | — | integer | 0 |
| `COUNT(col)` | any type | integer | 0 |
| `MIN(col)`, `MAX(col)` | integer, real, character | the column's | `NULL` |
| `SUM(col)` | integer, real | the column's | `NULL` |
| `AVG(col)` | integer, real | real | `NULL` |

- `COUNT(*)` counts the rows that satisfy the `WHERE`; the others take the
  non-`NULL` values of their column in those rows.
- A list holding an aggregate returns exactly one row, and may hold only
  aggregates and literals: a plain column beside an aggregate is an error.
- `SUM` of an integer column is an error if the total lies outside the 32-bit
  range. `AVG` of an integer column is computed without overflow.
- `MIN` and `MAX` of a character column use byte order.
- `ORDER BY` is not allowed with an aggregate. `LIMIT 0` gives no rows.
- The argument is one column name; `COUNT(DISTINCT x)`, `SUM(a + b)` and
  nested aggregates are errors.

## 7. Transactions

```
BEGIN
COMMIT
ROLLBACK
```

Report `transaction started`, `committed` and `rolled back`. No other words
are accepted (`BEGIN TRANSACTION` is an error).

- Outside a transaction every statement commits by itself, and is durable
  when it reports success.
- `BEGIN` opens a transaction; `COMMIT` makes every change since then
  permanent, together; `ROLLBACK` discards them all. A `SELECT` inside the
  transaction sees its changes.
- Transactions do not nest: `BEGIN` inside one is an error and leaves it open.
  `COMMIT` or `ROLLBACK` with none open is an error.
- **Table and index statements are refused inside a transaction**
  (`CREATE`, `DROP` and `ALTER` of either kind), and leave it open.
- A transaction left open when the database is closed, or when the program
  ends, is rolled back.
- **A statement that fails inside a transaction:**
  - If it failed before changing anything (a syntax error, an unknown column,
    a value of the wrong type in the first row), the transaction continues as
    if the statement had not been given.
  - If it failed after changing something (the second row of a multi-row
    `INSERT` is a duplicate, say), the rows it had written remain visible to
    `SELECT`, and the transaction is *doomed*: every later `INSERT`, `UPDATE`
    and `DELETE` is refused, `SELECT` still works, and `COMMIT` reports an
    error and rolls everything back. `ROLLBACK` is the way out.
- `BEGIN`, `COMMIT` and `ROLLBACK` are at present accepted with no database
  open (section 13, Q4).

A database serves one connection at a time. A second process that opens it is
refused (`database is locked by another connection`).

## 8. Conditions

```
condition := term [ AND term ]... [ OR term [ AND term ]... ]...
```

`AND` binds tighter than `OR`: `a OR b AND c` is `a OR (b AND c)`. There are
no parentheses and no `NOT` in front of a term. Every term begins with a
column of the table; a literal cannot stand on the left, and two columns
cannot be compared.

| Term | True when the column's value... |
|---|---|
| `col = v`, `col <> v`, `col < v`, `col <= v`, `col > v`, `col >= v` | compares so with `v` |
| `col BETWEEN a AND b` | is ≥ `a` and ≤ `b`. With `a` > `b` no row matches. |
| `col NOT BETWEEN a AND b` | is < `a` or > `b` |
| `col IN (v [, v]...)` | equals at least one `v`. The list may not be empty. |
| `col NOT IN (v [, v]...)` | equals no `v` |
| `col LIKE 'p' [ESCAPE 'c']` | matches the pattern (8.1) |
| `col NOT LIKE 'p' [ESCAPE 'c']` | does not match it |
| `col IS NULL` | is `NULL` |
| `col IS NOT NULL` | is not `NULL` |

A `NULL` column value makes every term false except `IS NULL` (3.4).

**Types.** The value in a term must suit the column, or the statement is an
error:

- integer or real column: an integer or a real literal, either with either.
  Both sides are compared as 64-bit reals, so `id = 2.0` matches the integer
  2 and `id = 2.5` matches nothing. An `IN` list may mix the two.
- character column: a string literal. A string longer than the column is not
  an error; it simply cannot be equal to any value.
- text column: only `IS NULL` and `IS NOT NULL`.
- `NULL` as the value is an error in every term but `IS [NOT] NULL`.

### 8.1 LIKE

For character columns only. The pattern is a string literal in which `%`
matches any run of characters, including none, and `_` matches exactly one
character: one UTF-8 character, not one byte. Every other byte matches itself,
so the match is case-sensitive. The whole value must match. The value is
matched without its trailing blanks.

`ESCAPE 'c'` names one single-byte character. In the pattern, that character
followed by `%`, `_` or itself stands for that character literally; followed
by anything else, or at the end of the pattern, it is an error.

## 9. Results and errors

A statement yields exactly one of:

| Result | From | In the `sql` module |
|---|---|---|
| rows | `SELECT` | `SQLRES_ROWS`: labels, types, a grid of cells |
| a count | `INSERT`, `UPDATE`, `DELETE` | `SQLRES_COUNT` |
| a message | table, index and transaction statements | `SQLRES_MSG` |
| nothing | a blank statement | `SQLRES_NONE` |
| an error | anything | a status other than `SQR_OK`, and a message |

An error leaves the database as it was (inside a transaction, see section 7).

### 9.1 Error messages

A syntax error is `col <n>: <what>`, where `<n>` is the 1-based byte position
in the statement of the token at fault. Only the first error is reported.
These messages are stable:

| Message (after `col <n>: `) | Cause |
|---|---|
| `unexpected character "<c>"` | a byte that begins no token |
| `unterminated string` / `unterminated quoted identifier` | no closing quote |
| `empty quoted identifier` | `""` |
| `control character in a quoted identifier` | |
| `unknown statement "<word>"` | the first token starts no statement |
| `unexpected token "<t>" after statement` | text after a complete statement |
| `expected <keyword>` | keyword in lower case: `expected from`, `expected into` |
| `expected "<p>"` | punctuation: `expected ")"` |
| `expected table name` / `column name` / `alias` | |
| `<what> too long (max 32)` | a name over 32 bytes |
| `expected a literal value` | |
| `expected a comparison operator` | |
| `expected between, like or in after not` | |
| `integer literal out of range` | |
| `malformed real literal` | |
| `sign not allowed before a string literal` / `before NULL` | |
| `unknown column type "<word>"` | |
| `CHAR width must be a positive integer` / `must be positive` | |
| `LIMIT must be a non-negative integer` | |
| `LIKE needs a string pattern` | |
| `ESCAPE needs a single-character string` | |
| `NULL is not allowed in the projection` | |
| `unknown function "<word>"` | a word before `(` that is not an aggregate |
| `every VALUES tuple must have the same number of values` | |
| `VALUES count does not match the column list` | |
| `too many columns` / `column names` / `index columns` / `projection columns` | over 256, or 64 for an index |
| `expected TABLE, INDEX or UNIQUE after CREATE` | |
| `expected TABLE or INDEX after DROP` | |
| `expected ADD or DROP after ALTER TABLE` | |

Errors found when the statement is run carry no position. These are stable:

| Message | Cause |
|---|---|
| `no database open` | |
| `no such table: <t>` | `INSERT`, `UPDATE`, `DELETE`, `SELECT` |
| `no such column: <c>` | in a list, `SET`, `WHERE` or `ORDER BY` |
| `duplicate column: <c>` | `INSERT` column list |
| `expected <n> values per row, got <m>` | `INSERT` without a column list |
| `column "<c>" needs an integer` / `a number` / `a string` | 3.1 |
| `value too long for column "<c>"` | 3.1 |
| `numeric column "<c>" needs a numeric literal` | a condition |
| `character column "<c>" needs a string literal` | a condition |
| `use IS NULL / IS NOT NULL to test column "<c>" for null` | `col = NULL` |
| `cannot compare TEXT column "<c>" (only IS [NOT] NULL)` | |
| `LIKE needs a character column, "<c>" is numeric` | |
| `LIKE pattern: the escape character must be followed by %, _ or itself` | |
| `cannot ORDER BY a TEXT column` | |
| `ORDER BY is not allowed with an aggregate` | |
| `column "<c>" must appear in an aggregate` | |
| `SUM needs a numeric column, "<c>" is not` (also `AVG`) | |
| `MIN cannot take a TEXT column` (also `MAX`) | |
| `integer overflow in <label>` | `SUM` |
| `SELECT without FROM takes literals only` | |
| `an earlier statement in this transaction failed part-way; ROLLBACK is required` | section 7 |
| `not committed: a statement in this transaction failed part-way; it has been rolled back` | section 7 |
| `create table: <detail>` | 4.1; the detail names the rule broken |
| `add column: <detail>` | 4.3 |
| `no such column: "<c>"`, `cannot drop the only column of "<t>"` | `DROP COLUMN` |
| `refused: a transaction is in flight` | a table statement inside `BEGIN` (some carry a prefix) |

**Messages that say too little.** These fixed texts stand in for several
causes, and are to be replaced by the real cause (section 13, Q6). A test
should require that the statement fails, not that it fails with this text:

| Message | Given for |
|---|---|
| `create index failed` | every failure of `CREATE INDEX` |
| `operation failed` | every failure of `DROP TABLE` and `DROP INDEX` |
| `transaction control failed` | every failure of `BEGIN`, `COMMIT`, `ROLLBACK` |
| `insert failed (duplicate key or I/O error)` | a unique-index violation, among others |
| `update failed`, `delete failed`, `scan failed`, `commit failed` | likewise |

## 10. `sqlsh`

```
sqlsh [<db-dir>]        sqlsh --help        sqlsh --version
```

`sqlsh` reads lines from standard input. A line whose **first byte** is `.`
is a meta-command; every other non-blank line is a statement. The database
directory is created if it does not exist.

**Streams.** Results go to standard output. Errors, prompts, the banner and
the notices of meta-commands go to standard error. An error is one line,
`error: <message>`, and the next line is then read: a script is not stopped
by an error.

**Exit status** is 0, whatever errors the statements gave (section 13, P3).
It is 1 when the input is in an unsupported encoding and 2 for a bad command
line.

**Output of a statement:**

- a message result: the message, on one line;
- a count: `<n> row(s) affected`;
- rows: a line of labels, a line of dashes, one line per row, and a last line
  `(<n> row(s))`, where:
  - each column is as wide, in bytes, as its longest cell or its label;
  - cells are left-aligned and padded with blanks to that width, the last
    column included, and columns are separated by two blanks;
  - an integer is in plain decimal;
  - a real is in the form `d.ddddddddE±dd`: eight decimals, so nine
    significant digits, with a leading `-` if negative. `1.5` is
    `1.50000000E+00`. (The stored value is exact; only the display is
    rounded. The wire protocol carries the exact value.)
  - a character value is shown without trailing blanks, a text value exactly;
  - `NULL` is shown as the four letters `NULL`, which cannot be told from the
    string `'NULL'`.

```
sql> SELECT id, name, x FROM t WHERE id < 3;
id  name   x
--  -----  --------------
1   Alice  1.50000000E+00
2   NULL   NULL
(2 row(s))
```

**Meta-commands** (the command word is not case-sensitive):

| Command | Effect |
|---|---|
| `.open <dir>` | close the current database and open (or create) another |
| `.close` | close the current database; an open transaction is rolled back |
| `.tables` | one line per table: `<name>  (<r> rows, <c> cols)` |
| `.schema [<table>]` | the columns of one table, or of all |
| `.pack <dir> <file>` | archive a database directory into one file |
| `.unpack <file> <dir>` | restore such a file into a new directory |
| `.help`, `.h` | list these |
| `.quit`, `.exit`, `.q` | leave |

## 11. Limits

| Limit | Value |
|---|---|
| name (table, column, alias) | 32 bytes |
| columns in a table, a select list, an `INSERT` column list | 256 |
| columns in an index | 64 |
| `CHAR(n)` | 1 ≤ n ≤ 65536 |
| row size | 1048576 bytes (4.1) |
| integer | −2147483648 to 2147483647 |
| rows in a table | about 2.1 × 10⁹ over its lifetime |
| statement length | none in `sqlsh`; 1 MiB over the wire |
| values in `VALUES`, in an `IN` list, terms in a condition | none |

## 12. Grammar

Keywords are in capitals and are case-insensitive. `[ ]` is optional, `{ }`
is zero or more, `|` separates alternatives; quoted characters are literal.

```
line        := [ statement ] [ ";" ]

statement   := create-table | drop-table | add-column | drop-column
             | create-index | drop-index
             | insert | update | delete | select
             | BEGIN | COMMIT | ROLLBACK

create-table := CREATE TABLE name "(" column-def { "," column-def } ")"
column-def   := name type
type         := INTEGER | INT | REAL | TEXT
              | ( CHAR | CHARACTER ) "(" integer ")"
drop-table   := DROP TABLE name
add-column   := ALTER TABLE name ADD COLUMN column-def
drop-column  := ALTER TABLE name DROP COLUMN name
create-index := CREATE [ UNIQUE ] INDEX ON name "(" name-list ")"
drop-index   := DROP INDEX ON name "(" name-list ")"
name-list    := name { "," name }

insert      := INSERT INTO name [ "(" name-list ")" ] VALUES row { "," row }
row         := "(" value { "," value } ")"
update      := UPDATE name SET assignment { "," assignment } [ where ]
assignment  := name "=" value
delete      := DELETE FROM name [ where ]

select      := SELECT literal-item { "," literal-item }
             | SELECT select-list FROM name [ where ] [ order ] [ limit ]
select-list := "*" | item { "," item }
item        := ( name | number | string | aggregate ) [ AS name ]
literal-item := ( number | string ) [ AS name ]
aggregate   := COUNT "(" "*" ")"
             | ( COUNT | MIN | MAX | SUM | AVG ) "(" name ")"
order       := ORDER BY order-key { "," order-key }
order-key   := name [ ASC | DESC ]
limit       := LIMIT integer

where       := WHERE and-group { OR and-group }
and-group   := term { AND term }
term        := name compare value
             | name [ NOT ] BETWEEN value AND value
             | name [ NOT ] IN "(" value { "," value } ")"
             | name [ NOT ] LIKE string [ ESCAPE string ]
             | name IS [ NOT ] NULL
compare     := "=" | "<>" | "<" | "<=" | ">" | ">="

value       := number | string | NULL
number      := [ "+" | "-" ] ( integer | real )
integer     := digit { digit }
real        := integer "." { digit } [ exponent ] | integer exponent
exponent    := ( "e" | "E" ) [ "+" | "-" ] digit { digit }
string      := "'" { any byte except "'" | "''" } "'"
name        := word | '"' { any byte except '"' and controls | '""' } '"'
word        := ( letter | "_" ) { letter | digit | "_" }
```

`value` is checked against its column afterwards (3.1, section 8): the
grammar admits `NULL` in a term, and the statement is then refused when run.

## 13. Open points

Behaviour here is wrong, inconsistent or undecided. Each will be ruled on and
this document changed to match. Labels are those of
`reports/FABLE-REVIEW-2026-11-13.md`; the unlabelled ones were found while
writing this document.

| | Present behaviour | Question |
|---|---|---|
| Q3 | A `CHAR` value with a control character in it compares differently in a scan and through an index. | Which order is right; or should such values be refused? |
| Q4 | `BEGIN`, `COMMIT` and `ROLLBACK` succeed with no database open. | Should be `no database open`. |
| Q5 | `UPDATE t SET x = 1, x = 2` is accepted; the later value wins. | Refuse, as `INSERT` refuses a repeated column? |
| Q6 | Fixed messages hide the cause (9.1, last table). | Report the cause. |
| W5 | A statement cannot span lines. | Allow a statement to continue until `;`? |
| P3 | `sqlsh` exits 0 after errors. | A non-zero status, perhaps under an option? |
| — | A real literal with an empty exponent (`1e`) is 1.0 in the ifx build and `malformed real literal` in the gfortran build. | Refuse it in the lexer. |
| — | A real with a three-digit exponent is displayed without its `E`: `1.00000000-300`. | Use a form that always has it. |
| — | `-0.0` is displayed `0.00000000E+00` by the ifx build and `-0.00000000E+00` by the gfortran build. | Decide and make the builds agree. |
| — | `.schema` output is not SQL: no parentheses, names unquoted. | Print a statement that could be read back. |
| — | Columns of the row display are padded by bytes, so non-ASCII values misalign, and the last column carries trailing blanks. | Pad by characters; do not pad the last column. |
| — | A name over 32 bytes in a select list is reported as `projection columns too long`. | `column name too long`. |
| — | `CREATE TABLE` refuses a name differing only in case, but lookup is case-sensitive. | Intended (file systems that fold case); stated here so it is not taken for a defect. |

## 14. Notes for writing tests

- Run each statement in its own `sqlsh <dir>` process, or many in one; a
  change is durable when its statement reports success, so both give the same
  database. Transactions need one process.
- Only one process may have a database open at a time.
- Take results from standard output and errors from standard error. The exit
  status says nothing about statements.
- Use `ORDER BY` whenever the order of rows matters; nothing else fixes it.
- Compare reals as numbers parsed from the display, to nine significant
  digits, not as text.
- `sqlt/` holds an existing harness in the sqllogictest format
  (`sqlt/README.md`), and `sqlt/tests/` its records.
- Do not write a test that requires the behaviour listed in section 13.

!! A small SQL subset over the `sqr` typed store — a *front-end layer only*.
!!
!! This module is deliberately **not** part of the storage engine: the
!! lexer, parser and executor here only ever call the public `db_*`
!! procedures of `sqr`.  `sqr` itself stays a typed programmatic API with no
!! notion of SQL (the dependency runs one way: `sql` uses `sqr`, never the
!! reverse).  The point is a familiar "shop window" on top of the engine and
!! the `sqlsh` REPL demo — not a DBMS.
!!
!! ## Supported subset
!!
!!   * **DDL** — `CREATE TABLE t (col TYPE, …)`, `DROP TABLE t`,
!!     `CREATE [UNIQUE] INDEX ON t (col, …)`, `DROP INDEX ON t (col, …)`,
!!     `ALTER TABLE t ADD COLUMN col TYPE`, `ALTER TABLE t DROP COLUMN col`.
!!     Types: `INTEGER`/`INT`, `REAL`, `CHAR(n)`, `TEXT`.
!!   * **DML** — `INSERT INTO t [(cols)] VALUES (…)[, (…)]`,
!!     `DELETE FROM t [WHERE …]`, `UPDATE t SET col=val[,…] [WHERE …]`.
!!   * **Query** — `SELECT item[,…]|* FROM t [WHERE pred]
!!     [ORDER BY col [ASC|DESC]] [LIMIT n]`, where an `item` is a column, a
!!     literal or an aggregate, optionally followed by `AS name`.  The
!!     aggregates are `COUNT(*)`, `COUNT(col)`, `MIN(col)`, `MAX(col)`,
!!     `SUM(col)` and `AVG(col)`; a projection that holds one returns a
!!     single row and may hold only aggregates and literals.  `SELECT` of
!!     literals alone needs no `FROM`.
!!   * **Transactions** — `BEGIN`, `COMMIT`, `ROLLBACK`.
!!
!! `pred` is `col {= <> < <= > >=} literal`, `col [NOT] BETWEEN a AND b`,
!! `col [NOT] LIKE pattern [ESCAPE c]`, `col [NOT] IN (literal, …)`,
!! `col IS [NOT] NULL`, combined with `AND` / `OR` (AND binds tighter; no
!! parentheses in this subset).  `LIKE` compares bytes (it is
!! case-sensitive): `%` matches any run of characters, `_` exactly one
!! UTF-8 character.  A NULL value satisfies no condition but `IS NULL`.
!!
!! ## Deliberately out of scope (kept honest, same spirit as the ROADMAP)
!!
!! JOINs / multiple tables, subqueries, `GROUP BY` / `HAVING`, `DISTINCT`,
!! arithmetic in the projection, views, foreign keys / `CHECK`, and column
!! constraints such as `NOT NULL` (the engine has no constraint store — every
!! column is nullable).  Keywords are case-insensitive; identifiers (table and
!! column names) are case-sensitive, matching the engine's byte-exact names.
!! A name may also be written as a standard delimited identifier — double
!! quotes with `""` as the escape, e.g. `"Rate %"` — allowing any printable
!! character and names that collide with keywords.

module sql
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use :: sqr
    implicit none
    private

    ! --- Token kinds (lexer output) ---
    integer, parameter, public :: TK_EOF   = 0  !! End of input
    integer, parameter, public :: TK_IDENT = 1  !! Word: keyword or identifier (case decided by the parser)
    integer, parameter, public :: TK_INT   = 2  !! Integer literal
    integer, parameter, public :: TK_REAL  = 3  !! Real literal
    integer, parameter, public :: TK_STR   = 4  !! Quoted string literal (already unescaped)
    integer, parameter, public :: TK_PUNCT = 5  !! Operator / punctuation (`=`, `<=`, `(`, `,`, `*`, …)
    integer, parameter, public :: TK_QIDENT = 6 !! Double-quoted (delimited) identifier, already unescaped; never a keyword

    !! One lexical token.  `text` is the verbatim source slice (for `TK_STR`
    !! and `TK_QIDENT` it is the unescaped body); `col` is the 1-based start
    !! column in the source line, used for parse-error reporting.
    type, public :: sql_token_t
        integer :: kind = TK_EOF
        character(len=:), allocatable :: text
        integer :: col = 0
    end type

    ! --- Literal kinds ---
    integer, parameter, public :: LIT_NULL = 0  !! SQL NULL
    integer, parameter, public :: LIT_INT  = 1  !! Integer literal
    integer, parameter, public :: LIT_REAL = 2  !! Real literal
    integer, parameter, public :: LIT_STR  = 3  !! String literal

    !! A parsed literal value, tagged by `ltype`.
    type, public :: sql_lit_t
        integer        :: ltype = LIT_NULL
        integer(int32) :: ival  = 0_int32
        real(real64)   :: rval  = 0.0_real64
        character(len=:), allocatable :: sval
    end type

    ! --- Comparison operators (WHERE conditions) ---
    integer, parameter, public :: OP_EQ        = 1  !! `=`
    integer, parameter, public :: OP_NE        = 2  !! `<>`
    integer, parameter, public :: OP_LT        = 3  !! `<`
    integer, parameter, public :: OP_LE        = 4  !! `<=`
    integer, parameter, public :: OP_GT        = 5  !! `>`
    integer, parameter, public :: OP_GE        = 6  !! `>=`
    integer, parameter, public :: OP_BETWEEN   = 7  !! `BETWEEN a AND b`
    integer, parameter, public :: OP_ISNULL    = 8  !! `IS NULL`
    integer, parameter, public :: OP_ISNOTNULL = 9  !! `IS NOT NULL`
    integer, parameter, public :: OP_NOTBETWEEN = 10  !! `NOT BETWEEN a AND b`
    integer, parameter, public :: OP_LIKE       = 11  !! `LIKE pattern`
    integer, parameter, public :: OP_NOTLIKE    = 12  !! `NOT LIKE pattern`
    integer, parameter, public :: OP_IN         = 13  !! `IN (literal, …)`
    integer, parameter, public :: OP_NOTIN      = 14  !! `NOT IN (literal, …)`

    !! One predicate condition: a column compared to one literal (`lit`), a
    !! `BETWEEN` band (`lit`..`lit2`), a `LIKE` pattern (`lit`, with `esc`),
    !! an `IN` list (`lits`), or a null test (no literal).
    type, public :: sql_cond_t
        character(len=SQR_NAME_LEN) :: col = ''
        integer          :: op = 0
        type(sql_lit_t)  :: lit
        type(sql_lit_t)  :: lit2     !! BETWEEN upper bound only
        type(sql_lit_t), allocatable :: lits(:)  !! IN list only
        logical          :: has_esc = .false.    !! LIKE: an ESCAPE character was given
        character(len=1) :: esc = ' '            !! LIKE: that character
        integer          :: col_ord = 0  !! Cached column ordinal in the target table (0 = unresolved); filled once per statement so per-row evaluation skips the name lookup
    end type

    !! One AND-group of conditions.  A WHERE clause is a disjunction (OR) of
    !! these groups; a row matches the clause iff it matches every condition of
    !! *some* group (disjunctive normal form — no parentheses needed).
    type, public :: sql_cond_group_t
        type(sql_cond_t), allocatable :: conds(:)
    end type

    ! --- SELECT projection items ---
    integer, parameter, public :: PROJ_COL = 1  !! A column
    integer, parameter, public :: PROJ_LIT = 2  !! A literal
    integer, parameter, public :: PROJ_AGG = 3  !! An aggregate

    integer, parameter, public :: AGG_COUNT_STAR = 1  !! `COUNT(*)`
    integer, parameter, public :: AGG_COUNT      = 2  !! `COUNT(col)`
    integer, parameter, public :: AGG_MIN        = 3  !! `MIN(col)`
    integer, parameter, public :: AGG_MAX        = 4  !! `MAX(col)`
    integer, parameter, public :: AGG_SUM        = 5  !! `SUM(col)`
    integer, parameter, public :: AGG_AVG        = 6  !! `AVG(col)`

    !! Length of a result-column label: long enough for an aggregate over a
    !! column whose name is `SQR_NAME_LEN` bytes (`COUNT(` + name + `)`).
    integer, parameter, public :: SQL_LABEL_LEN = SQR_NAME_LEN + 8

    !! One SELECT projection item, tagged by `kind` (the same fat-record
    !! style as `sql_stmt_t`).  `label` names the result column: the `AS`
    !! alias if one was given, otherwise the column name, the aggregate in
    !! canonical form (`MAX(salary)`), a numeric literal's source text, or
    !! `?column?` for a string literal.
    type, public :: sql_proj_t
        integer :: kind = PROJ_COL
        character(len=SQR_NAME_LEN)  :: col = ''   !! `PROJ_COL`, and the argument of a `PROJ_AGG`
        type(sql_lit_t)              :: lit        !! `PROJ_LIT`
        integer                      :: agg = 0    !! `PROJ_AGG`: one of `AGG_*`
        character(len=SQL_LABEL_LEN) :: label = '' !! Result-column name
    end type

    ! --- Statement kinds ---
    integer, parameter, public :: ST_NONE         = 0
    integer, parameter, public :: ST_CREATE_TABLE = 1
    integer, parameter, public :: ST_DROP_TABLE   = 2
    integer, parameter, public :: ST_CREATE_INDEX = 3
    integer, parameter, public :: ST_DROP_INDEX   = 4
    integer, parameter, public :: ST_ADD_COLUMN   = 5
    integer, parameter, public :: ST_DROP_COLUMN  = 6
    integer, parameter, public :: ST_INSERT       = 7
    integer, parameter, public :: ST_DELETE       = 8
    integer, parameter, public :: ST_UPDATE       = 9
    integer, parameter, public :: ST_SELECT       = 10
    integer, parameter, public :: ST_BEGIN        = 11
    integer, parameter, public :: ST_COMMIT       = 12
    integer, parameter, public :: ST_ROLLBACK     = 13

    !! One parsed statement.  A single fat record tagged by `kind` (a
    !! dispatch-table style union) rather than a polymorphic node hierarchy:
    !! each statement form populates the components it needs and leaves the
    !! rest at their defaults.
    type, public :: sql_stmt_t
        integer :: kind = ST_NONE
        character(len=SQR_NAME_LEN) :: table = ''

        ! CREATE TABLE / ALTER ADD COLUMN: full column definitions.
        type(column_t), allocatable :: coldefs(:)

        ! Generic ordered name list: INSERT target columns, index members,
        ! ALTER DROP COLUMN target.
        character(len=SQR_NAME_LEN), allocatable :: names(:)

        ! SELECT projection: `proj` unless `select_star`.  `has_from` is
        ! false for a SELECT of literals with no table.
        type(sql_proj_t), allocatable :: proj(:)
        logical :: select_star = .false.   !! SELECT *
        logical :: has_from    = .false.   !! SELECT … FROM table
        logical :: unique      = .false.   !! CREATE UNIQUE INDEX

        ! INSERT values: (nrows, ncols) grid of literals.  `ncols` matches
        ! either `names` (explicit column list) or the table arity (positional).
        type(sql_lit_t), allocatable :: values(:,:)
        logical :: insert_named = .false.  !! `.true.` if an explicit column list was given

        ! UPDATE SET assignments (parallel arrays).
        character(len=SQR_NAME_LEN), allocatable :: set_cols(:)
        type(sql_lit_t),             allocatable :: set_vals(:)

        ! WHERE clause (DNF).  `has_where = .false.` ⇒ all rows.
        logical :: has_where = .false.
        type(sql_cond_group_t), allocatable :: where_groups(:)

        ! ORDER BY / LIMIT.
        logical :: has_order = .false.
        character(len=SQR_NAME_LEN) :: order_col = ''
        logical :: order_desc = .false.
        logical :: has_limit = .false.
        integer :: limit_n = 0
    end type

    ! --- Result of executing a statement ---
    integer, parameter, public :: SQLRES_NONE  = 0  !! Nothing to report (e.g. BEGIN)
    integer, parameter, public :: SQLRES_ROWS  = 1  !! A SELECT result set
    integer, parameter, public :: SQLRES_COUNT = 2  !! A DML row count
    integer, parameter, public :: SQLRES_MSG   = 3  !! A short status message (DDL)

    !! One rendered output cell of a SELECT result.
    type, public :: sql_cell_t
        character(len=:), allocatable :: text
        logical :: is_null = .false.
    end type

    !! The outcome of executing one statement.  A SELECT fills `colnames`,
    !! `coltypes`/`colsizes` and the `(nrows, ncols)` `cells` grid; a DML
    !! statement fills `count`; DDL fills `message`.  Both the REPL and the
    !! test suite consume this same structure, so output is asserted directly
    !! rather than scraped from text.
    type, public :: sql_result_t
        integer :: kind = SQLRES_NONE
        character(len=SQL_LABEL_LEN), allocatable :: colnames(:)
        integer,                     allocatable :: coltypes(:)  !! Per output column: `DT_INT`/`DT_REAL`/`DT_CHAR`/`DT_TEXT`
        integer,                     allocatable :: colsizes(:)  !! Per output column: declared `csize` in bytes
        type(sql_cell_t),            allocatable :: cells(:,:)
        integer :: nrows = 0
        integer :: ncols = 0
        integer :: count = 0
        character(len=:), allocatable :: message
    end type

    ! --- Limits of the SQL layer (the engine itself has no column-count cap) ---
    integer, parameter, public :: SQL_MAX_COLS       = 256  !! Columns in a `CREATE TABLE`, a projection or an `INSERT` column list
    integer, parameter, public :: SQL_MAX_INDEX_COLS = 64   !! Columns in a `CREATE` / `DROP INDEX` column list

    ! --- Input-text encodings reported by `sql_strip_bom` / `sql_screen_line` ---
    ! sqr text is UTF-8 handled as bytes.  Legacy 8-bit sets cannot be told
    ! apart from it and pass through as opaque bytes (`SQL_ENC_PLAIN`).  Every
    ! value except the first two is refused - see `sql_encoding_supported`.
    ! The table, the names and the wording match cmdgraph's, so `sqlsh` and
    ! `sqrsh` give the same answer for the same input.
    integer, parameter, public :: SQL_ENC_PLAIN       =  0  !! No signature: ASCII, UTF-8, or an 8-bit set (not validated)
    integer, parameter, public :: SQL_ENC_UTF8        =  1  !! UTF-8 byte-order mark found and removed
    integer, parameter, public :: SQL_ENC_UTF16_LE    =  2  !! UTF-16 / UCS-2, little-endian
    integer, parameter, public :: SQL_ENC_UTF16_BE    =  3  !! UTF-16 / UCS-2, big-endian
    integer, parameter, public :: SQL_ENC_UTF32_LE    =  4  !! UTF-32 / UCS-4, little-endian
    integer, parameter, public :: SQL_ENC_UTF32_BE    =  5  !! UTF-32 / UCS-4, big-endian
    integer, parameter, public :: SQL_ENC_UTF7        =  6  !! UTF-7
    integer, parameter, public :: SQL_ENC_UTF1        =  7  !! UTF-1
    integer, parameter, public :: SQL_ENC_UTF_EBCDIC  =  8  !! UTF-EBCDIC
    integer, parameter, public :: SQL_ENC_SCSU        =  9  !! SCSU
    integer, parameter, public :: SQL_ENC_BOCU1       = 10  !! BOCU-1
    integer, parameter, public :: SQL_ENC_GB18030     = 11  !! GB18030
    !! No signature, but NUL bytes: a 16- or 32-bit encoding written without
    !! a byte-order mark.
    integer, parameter, public :: SQL_ENC_WIDE_NO_BOM = 12

    ! --- Public entry points ---
    public :: sql_strip_bom !! first line of input -> encoding, UTF-8 BOM removed
    public :: sql_screen_line        !! any line of input -> encoding
    public :: sql_encoding_name      !! encoding -> display name
    public :: sql_encoding_supported !! encoding -> can sqr read it?
    public :: sql_encoding_message   !! refused encoding -> the message to show
    public :: sql_like     !! does a string match a LIKE pattern?
    public :: sql_lex      !! source line  -> token stream
    public :: sql_parse    !! source line  -> one statement AST
    public :: sql_exec     !! statement AST -> result (drives the engine)
    public :: sql_run      !! source line  -> result (lex + parse + exec)
    public :: sql_render   !! result        -> printed to a unit (the REPL renderer)

    interface
        !! Classify the start of an input stream by its leading bytes and
        !! make it lexable.  sqr text is UTF-8 handled as bytes, so:
        !!
        !! - a UTF-8 mark (`EF BB BF`, written by e.g. Windows PowerShell 5.1
        !!   `Out-File -Encoding utf8`) is removed from `text` and `enc` is
        !!   `SQL_ENC_UTF8`;
        !! - the signature of an encoding sqr cannot read leaves `text` alone
        !!   and names it in `enc`: UTF-16 / UCS-2 (`FF FE` or `FE FF`, the
        !!   default of Windows PowerShell 5.1 `>` redirection), UTF-32 /
        !!   UCS-4, UTF-7, UTF-1, UTF-EBCDIC, SCSU, BOCU-1, GB18030;
        !! - text with no signature but with NUL bytes is
        !!   `SQL_ENC_WIDE_NO_BOM`: a 16- or 32-bit encoding without a mark;
        !! - anything else is `SQL_ENC_PLAIN`, `text` untouched.
        !!
        !! The caller should refuse input for which `sql_encoding_supported`
        !! is false: neither the lexer nor `DT_CHAR` storage can hold it.
        !!
        !! Call it on the **first** line of a stream only (`sql_screen_line`
        !! does that, and handles the later lines too).  An unallocated
        !! `text` is `SQL_ENC_PLAIN`.
        pure module subroutine sql_strip_bom(text, enc)
            character(len=:), allocatable, intent(inout) :: text  !! First input line
            integer,                       intent(out)   :: enc   !! One of the `SQL_ENC_*` values
        end subroutine

        !! Classify one line of an input stream.  The first line is tested
        !! for a signature (and loses a UTF-8 mark) by `sql_strip_bom`; every
        !! later line is tested only for NUL bytes, which catch a wide
        !! encoding that has no mark.
        pure module subroutine sql_screen_line(text, first, enc)
            character(len=:), allocatable, intent(inout) :: text   !! The line
            logical,                       intent(in)    :: first  !! Is it the first line of the stream?
            integer,                       intent(out)   :: enc    !! One of the `SQL_ENC_*` values
        end subroutine

        !! Display name of an `SQL_ENC_*` value.
        pure module function sql_encoding_name(enc) result(name)
            integer, intent(in)           :: enc
            character(len=:), allocatable :: name
        end function

        !! `.true.` for the encodings sqr reads: `SQL_ENC_PLAIN` and
        !! `SQL_ENC_UTF8`.
        pure elemental module function sql_encoding_supported(enc) result(ok)
            integer, intent(in) :: enc
            logical             :: ok
        end function

        !! The message for a refused encoding:
        !! `unsupported encoding (<name>); expected ASCII or UTF-8`.
        pure module function sql_encoding_message(enc) result(msg)
            integer, intent(in)           :: enc
            character(len=:), allocatable :: msg
        end function

        !! Does `s` match the SQL `LIKE` pattern `pattern`?  `%` matches any
        !! run of characters (including none) and `_` exactly one UTF-8
        !! character: a lead byte with the continuation bytes that follow
        !! it; a byte that is not part of a valid sequence counts as one
        !! character.  Everything else matches itself, byte for byte, so the
        !! test is case-sensitive.  With `esc`, that character makes the
        !! `%`, `_` or `esc` after it literal.
        pure module function sql_like(s, pattern, esc) result(yes)
            character(len=*), intent(in)           :: s        !! The value
            character(len=*), intent(in)           :: pattern  !! The pattern
            character(len=1), intent(in), optional :: esc      !! Escape character
            logical                                :: yes
        end function

        !! Tokenise one source line.  Always succeeds for well-formed tokens;
        !! reports `SQR_INVALID` with `errmsg` (carrying the offending column)
        !! for an unterminated string or a stray character.  `toks(1:ntok)` are
        !! the tokens; a trailing `TK_EOF` is **not** appended (callers use
        !! `ntok`).
        module subroutine sql_lex(text, toks, ntok, stat, errmsg)
            character(len=*),              intent(in)            :: text  !! Source line
            type(sql_token_t), allocatable, intent(out)          :: toks(:)  !! Tokens
            integer,                        intent(out)          :: ntok  !! Token count
            integer,                        intent(out), optional :: stat  !! `SQR_OK` / `SQR_INVALID`
            character(len=*),               intent(inout), optional :: errmsg  !! Failure detail
        end subroutine

        !! Parse one statement.  Recursive descent into `sql_stmt_t`.  A
        !! trailing `;` is permitted.  Reports `SQR_INVALID` with a
        !! column-anchored `errmsg` on a syntax error.
        module subroutine sql_parse(text, stmt, stat, errmsg)
            character(len=*),  intent(in)              :: text  !! Source line
            type(sql_stmt_t),  intent(out)             :: stmt  !! Parsed statement
            integer,           intent(out),  optional  :: stat  !! `SQR_OK` / `SQR_INVALID`
            character(len=*),  intent(inout), optional :: errmsg  !! Failure detail
        end subroutine

        !! Execute one parsed statement against an open database, choosing an
        !! index-driven or scan-driven plan as appropriate, and fill `res`.
        !! Engine errors propagate through `stat`/`errmsg`.  `db` is `target`
        !! because the transaction façade the executor calls needs it.
        !! With `binary_cells` true a SELECT fills INT/REAL cells with the
        !! value's native bytes (`transfer`: 4 B int32 / 8 B IEEE double —
        !! exact, for wire clients) instead of formatted text, and NULL cells
        !! carry zero-length text (`is_null` is the marker either way);
        !! CHAR/TEXT cells are identical in both modes.  `sql_render` expects
        !! a text-mode result.
        module subroutine sql_exec(db, stmt, res, stat, errmsg, binary_cells)
            type(db_t),        intent(inout), target   :: db  !! Open database handle
            type(sql_stmt_t),  intent(in)              :: stmt  !! Statement to run
            type(sql_result_t), intent(out)            :: res  !! Execution result
            integer,           intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*),  intent(inout), optional :: errmsg  !! Failure detail
            logical,           intent(in),   optional  :: binary_cells  !! Native-byte INT/REAL cells (default text)
        end subroutine

        !! Convenience: lex + parse + execute one source line.  Equivalent to
        !! `sql_parse` then `sql_exec` (`binary_cells` as there).
        module subroutine sql_run(db, text, res, stat, errmsg, binary_cells)
            type(db_t),        intent(inout), target   :: db  !! Open database handle
            character(len=*),  intent(in)              :: text  !! Source line
            type(sql_result_t), intent(out)            :: res  !! Execution result
            integer,           intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*),  intent(inout), optional :: errmsg  !! Failure detail
            logical,           intent(in),   optional  :: binary_cells  !! Native-byte INT/REAL cells (default text)
        end subroutine

        !! Render a result to a formatted unit: an aligned table for a SELECT,
        !! a `"N row(s)"` line for DML, or the message for DDL.  Used by the
        !! REPL; tests inspect `sql_result_t` directly instead.
        module subroutine sql_render(res, unit)
            type(sql_result_t), intent(in) :: res   !! Result to print
            integer,            intent(in) :: unit  !! Output unit
        end subroutine
    end interface

contains

end module sql

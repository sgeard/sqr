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
!!   * **Query** — `SELECT col[,…|*] FROM t [WHERE pred]
!!     [ORDER BY col [ASC|DESC]] [LIMIT n]`.
!!   * **Transactions** — `BEGIN`, `COMMIT`, `ROLLBACK`.
!!
!! `pred` is `col {= <> < <= > >=} literal`, `col BETWEEN a AND b`,
!! `col IS [NOT] NULL`, combined with `AND` / `OR` (AND binds tighter; no
!! parentheses in this subset).
!!
!! ## Deliberately out of scope (kept honest, same spirit as the ROADMAP)
!!
!! JOINs / multiple tables, subqueries, aggregates / `GROUP BY` / `HAVING`,
!! expressions in the projection, views, foreign keys / `CHECK`, and column
!! constraints such as `NOT NULL` (the engine has no constraint store — every
!! column is nullable).  Keywords are case-insensitive; identifiers (table and
!! column names) are case-sensitive, matching the engine's byte-exact names.

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

    !! One lexical token.  `text` is the verbatim source slice (for `TK_STR`
    !! it is the unescaped string body); `col` is the 1-based start column in
    !! the source line, used for parse-error reporting.
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

    !! One predicate condition: a column compared to one literal (`lit`), or a
    !! `BETWEEN` band (`lit`..`lit2`), or a null test (no literal).
    type, public :: sql_cond_t
        character(len=SQR_NAME_LEN) :: col = ''
        integer          :: op = 0
        type(sql_lit_t)  :: lit
        type(sql_lit_t)  :: lit2   !! BETWEEN upper bound only
    end type

    !! One AND-group of conditions.  A WHERE clause is a disjunction (OR) of
    !! these groups; a row matches the clause iff it matches every condition of
    !! *some* group (disjunctive normal form — no parentheses needed).
    type, public :: sql_cond_group_t
        type(sql_cond_t), allocatable :: conds(:)
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
        ! SELECT projection columns, ALTER DROP COLUMN target.
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        logical :: select_star = .false.   !! SELECT *
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

    !! The outcome of executing one statement.  A SELECT fills `colnames` and
    !! the `(nrows, ncols)` `cells` grid; a DML statement fills `count`; DDL
    !! fills `message`.  Both the REPL and the test suite consume this same
    !! structure, so output is asserted directly rather than scraped from text.
    type, public :: sql_result_t
        integer :: kind = SQLRES_NONE
        character(len=SQR_NAME_LEN), allocatable :: colnames(:)
        type(sql_cell_t),            allocatable :: cells(:,:)
        integer :: nrows = 0
        integer :: ncols = 0
        integer :: count = 0
        character(len=:), allocatable :: message
    end type

    ! --- Public entry points ---
    public :: sql_lex      !! source line  -> token stream
    public :: sql_parse    !! source line  -> one statement AST
    public :: sql_exec     !! statement AST -> result (drives the engine)
    public :: sql_run      !! source line  -> result (lex + parse + exec)
    public :: sql_render   !! result        -> printed to a unit (the REPL renderer)

    interface
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
        module subroutine sql_exec(db, stmt, res, stat, errmsg)
            type(db_t),        intent(inout), target   :: db  !! Open database handle
            type(sql_stmt_t),  intent(in)              :: stmt  !! Statement to run
            type(sql_result_t), intent(out)            :: res  !! Execution result
            integer,           intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*),  intent(inout), optional :: errmsg  !! Failure detail
        end subroutine

        !! Convenience: lex + parse + execute one source line.  Equivalent to
        !! `sql_parse` then `sql_exec`.
        module subroutine sql_run(db, text, res, stat, errmsg)
            type(db_t),        intent(inout), target   :: db  !! Open database handle
            character(len=*),  intent(in)              :: text  !! Source line
            type(sql_result_t), intent(out)            :: res  !! Execution result
            integer,           intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*),  intent(inout), optional :: errmsg  !! Failure detail
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

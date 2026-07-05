!! Unit + functional tests for the `sql` subset front-end.
!!
!! Three layers are exercised independently: the lexer (token streams), the
!! parser (AST shape and error reporting) and the executor (against a real
!! temporary `sqr` database, asserting on the structured `sql_result_t`
!! rather than scraping rendered text).  A dedicated parity test checks that
!! an index-driven plan and a scan-driven plan return identical rows, and a
!! final scripted example stands in for the functional "simple example".

program utest_sql
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use sqr
    use sql
    use clib_wrap, only: c_rmtree
    implicit none

    integer :: pass = 0, fail = 0
    character(len=*), parameter :: TEST_DIR = 'utest_sql_db'

    call cleanup_dir()

    call test_lexer()
    call test_parser()
    call test_parse_errors()
    call test_ddl_and_insert()
    call test_ddl_drop_alter()
    call test_select_projection()
    call test_where_predicates()
    call test_order_and_limit()
    call test_null_handling()
    call test_text_column()
    call test_update_delete()
    call test_index_scan_parity()
    call test_index_scan_parity_sweep()
    call test_transactions()
    call test_exec_errors()
    call test_functional_example()

    call cleanup_dir()

    print '(a,i0,a,i0,a)', 'sql tests: ', pass, ' passed, ', fail, ' failed'
    if (fail > 0) error stop 1

contains

    subroutine check(cond, label)
        logical,          intent(in) :: cond
        character(len=*), intent(in) :: label
        if (cond) then
            pass = pass + 1
            print '(a,a)', '  OK   ', label
        else
            fail = fail + 1
            print '(a,a)', '  FAIL ', label
        end if
    end subroutine

    subroutine cleanup_dir()
        integer :: ios
        ios = c_rmtree(TEST_DIR)
    end subroutine

    ! Open a fresh database (removing any previous one first).
    subroutine fresh_db(db)
        type(db_t), intent(out) :: db
        integer :: rs
        call cleanup_dir()
        call db_open(db, TEST_DIR, rs)
        if (rs /= SQR_OK) then
            print '(a)', 'FATAL: cannot open test db'
            error stop 2
        end if
    end subroutine

    ! Run one SQL line, returning the result and status.
    subroutine run(db, text, res, rs)
        type(db_t),         intent(inout), target :: db
        character(len=*),   intent(in)  :: text
        type(sql_result_t), intent(out) :: res
        integer,            intent(out) :: rs
        character(len=200) :: em
        em = ''
        call sql_run(db, text, res, rs, em)
    end subroutine

    ! Cell text of a SELECT result at (row, col), trimmed.
    function cell(res, r, c) result(s)
        type(sql_result_t), intent(in) :: res
        integer,            intent(in) :: r, c
        character(len=:), allocatable :: s
        s = res%cells(r, c)%text
    end function

    ! ===================== lexer =====================

    subroutine test_lexer()
        type(sql_token_t), allocatable :: toks(:)
        integer :: ntok, rs

        call sql_lex("SELECT * FROM t WHERE x >= 10", toks, ntok, rs)
        call check(rs == SQR_OK, 'lex: simple select succeeds')
        call check(ntok == 8, 'lex: token count')
        call check(toks(1)%kind == TK_IDENT .and. toks(1)%text == 'SELECT', 'lex: keyword token')
        call check(toks(2)%kind == TK_PUNCT .and. toks(2)%text == '*', 'lex: star punct')
        call check(toks(7)%kind == TK_PUNCT .and. toks(7)%text == '>=', 'lex: two-char operator')
        call check(toks(8)%kind == TK_INT  .and. toks(8)%text == '10', 'lex: integer literal')

        call sql_lex("a = 'O''Brien'", toks, ntok, rs)
        call check(rs == SQR_OK, 'lex: quoted string succeeds')
        call check(ntok == 3, 'lex: string token count')
        call check(toks(3)%kind == TK_STR .and. toks(3)%text == "O'Brien", 'lex: escaped quote unescaped')

        call sql_lex("3.5e2 .25", toks, ntok, rs)
        call check(toks(1)%kind == TK_REAL .and. toks(1)%text == '3.5e2', 'lex: real with exponent')

        call sql_lex("x = 'unterminated", toks, ntok, rs)
        call check(rs == SQR_INVALID, 'lex: unterminated string flagged')

        call sql_lex("a ~ b", toks, ntok, rs)
        call check(rs == SQR_INVALID, 'lex: stray character flagged')
    end subroutine

    ! ===================== parser =====================

    subroutine test_parser()
        type(sql_stmt_t) :: st
        integer :: rs

        call sql_parse("CREATE TABLE t (id INTEGER, name CHAR(20), v REAL, note TEXT)", st, rs)
        call check(rs == SQR_OK, 'parse: create table ok')
        call check(st%kind == ST_CREATE_TABLE, 'parse: kind = CREATE TABLE')
        call check(trim(st%table) == 't', 'parse: table name')
        call check(size(st%coldefs) == 4, 'parse: four columns')
        call check(st%coldefs(1)%dtype == DT_INT, 'parse: INTEGER -> DT_INT')
        call check(st%coldefs(2)%dtype == DT_CHAR .and. st%coldefs(2)%csize == 20, 'parse: CHAR(20)')
        call check(st%coldefs(3)%dtype == DT_REAL, 'parse: REAL')
        call check(st%coldefs(4)%dtype == DT_TEXT, 'parse: TEXT')

        call sql_parse("INSERT INTO t VALUES (1,'a',2.0),(2,'b',3.0)", st, rs)
        call check(rs == SQR_OK, 'parse: multi-row insert ok')
        call check(st%kind == ST_INSERT, 'parse: kind = INSERT')
        call check(size(st%values,1) == 2 .and. size(st%values,2) == 3, 'parse: values 2x3 grid')
        call check(st%values(1,1)%ltype == LIT_INT .and. st%values(1,1)%ival == 1, 'parse: literal int')
        call check(st%values(2,2)%ltype == LIT_STR .and. st%values(2,2)%sval == 'b', 'parse: literal str')

        call sql_parse("SELECT a, b FROM t WHERE x = 1 AND y > 2 OR z <= 3 " // &
                       "ORDER BY a DESC LIMIT 5", st, rs)
        call check(rs == SQR_OK, 'parse: full select ok')
        call check(st%select_star .eqv. .false., 'parse: not select-star')
        call check(size(st%names) == 2, 'parse: two projection columns')
        call check(st%has_where, 'parse: has where')
        call check(size(st%where_groups) == 2, 'parse: two OR-groups (DNF)')
        call check(size(st%where_groups(1)%conds) == 2, 'parse: first group has two ANDed conds')
        call check(st%has_order .and. st%order_desc, 'parse: order by desc')
        call check(st%has_limit .and. st%limit_n == 5, 'parse: limit 5')

        call sql_parse("DELETE FROM t WHERE id BETWEEN 1 AND 10", st, rs)
        call check(st%where_groups(1)%conds(1)%op == OP_BETWEEN, 'parse: between op')

        call sql_parse("UPDATE t SET a = 1, b = 'x' WHERE id IS NOT NULL", st, rs)
        call check(st%kind == ST_UPDATE .and. size(st%set_cols) == 2, 'parse: update set list')
        call check(st%where_groups(1)%conds(1)%op == OP_ISNOTNULL, 'parse: is not null op')

        call sql_parse("CREATE UNIQUE INDEX ON t (a, b)", st, rs)
        call check(st%kind == ST_CREATE_INDEX .and. st%unique, 'parse: create unique index')

        call sql_parse("BEGIN", st, rs)
        call check(st%kind == ST_BEGIN, 'parse: begin')
        call sql_parse("", st, rs)
        call check(rs == SQR_OK .and. st%kind == ST_NONE, 'parse: blank line is a no-op')
        call sql_parse("SELECT * FROM t;", st, rs)
        call check(rs == SQR_OK, 'parse: trailing semicolon allowed')
    end subroutine

    subroutine test_parse_errors()
        ! Explicit init: works around a spurious gfortran-16 -Wuninitialized on
        ! this intent(out) derived-type arg (same allocatable-component family
        ! as the documented ICE). sql_parse resets it on every call regardless.
        type(sql_stmt_t) :: st = sql_stmt_t()
        integer :: rs

        call sql_parse("SELECT FROM t", st, rs)
        call check(rs == SQR_INVALID, 'parse err: missing projection')
        call sql_parse("INSERT INTO t VALUES (1,2),(3)", st, rs)
        call check(rs == SQR_INVALID, 'parse err: ragged value tuples')
        call sql_parse("SELECT * FROM", st, rs)
        call check(rs == SQR_INVALID, 'parse err: missing table name')
        call sql_parse("CREATE TABLE t (id WHAT)", st, rs)
        call check(rs == SQR_INVALID, 'parse err: unknown type')
        call sql_parse("FROBNICATE t", st, rs)
        call check(rs == SQR_INVALID, 'parse err: unknown statement')
        call sql_parse("SELECT * FROM t extra", st, rs)
        call check(rs == SQR_INVALID, 'parse err: trailing tokens')
    end subroutine

    ! ===================== DDL + INSERT =====================

    subroutine test_ddl_and_insert()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, name CHAR(16), v REAL)", res, rs)
        call check(rs == SQR_OK .and. res%kind == SQLRES_MSG, 'ddl: create table message')
        call check(db_table_index(db, 't') > 0, 'ddl: table now exists')

        call run(db, "INSERT INTO t VALUES (1,'Alice',1.5),(2,'Bob',2.5),(3,'Carol',3.5)", res, rs)
        call check(rs == SQR_OK .and. res%kind == SQLRES_COUNT, 'insert: count result')
        call check(res%count == 3, 'insert: three rows')

        call run(db, "INSERT INTO t (id, name) VALUES (4, 'Dave')", res, rs)
        call check(rs == SQR_OK .and. res%count == 1, 'insert: named-column insert')

        call run(db, "SELECT * FROM t", res, rs)
        call check(rs == SQR_OK .and. res%nrows == 4, 'insert: four rows present')

        call db_close(db)
    end subroutine

    ! ===================== DROP + ALTER DDL =====================

    ! The "shrinking" DDL — DROP TABLE / DROP INDEX and ALTER ADD/DROP
    ! COLUMN — exercised end-to-end through the SQL front-end, since the
    ! happy-path DDL test above only covers CREATE.  Each op is followed by
    ! a query that observes the structural change it made.
    subroutine test_ddl_drop_alter()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, name CHAR(16), v REAL)", res, rs)
        call run(db, "CREATE INDEX ON t (id)", res, rs)
        call check(rs == SQR_OK, 'alter: index created for drop test')
        call run(db, "INSERT INTO t VALUES (1,'Alice',1.5),(2,'Bob',2.5)", res, rs)

        ! ADD COLUMN: a new trailing column appears, NULL on existing rows.
        call run(db, "ALTER TABLE t ADD COLUMN flag INTEGER", res, rs)
        call check(rs == SQR_OK .and. res%kind == SQLRES_MSG, 'alter: add column message')
        call run(db, "SELECT * FROM t ORDER BY id", res, rs)
        call check(res%ncols == 4, 'alter: added column visible')
        call check(res%cells(1,4)%is_null, 'alter: added column NULL on existing rows')

        ! DROP COLUMN: the column vanishes, remaining data intact.
        call run(db, "ALTER TABLE t DROP COLUMN v", res, rs)
        call check(rs == SQR_OK, 'alter: drop column ok')
        call run(db, "SELECT * FROM t ORDER BY id", res, rs)
        call check(res%ncols == 3, 'alter: dropped column gone')
        call check(cell(res,1,2) == 'Alice', 'alter: surviving data intact after drop')

        ! DROP INDEX: removes the index; queries still answer (now via scan).
        call run(db, "DROP INDEX ON t (id)", res, rs)
        call check(rs == SQR_OK .and. res%kind == SQLRES_MSG, 'drop: index dropped')
        call run(db, "SELECT name FROM t WHERE id = 2", res, rs)
        call check(res%nrows == 1 .and. cell(res,1,1) == 'Bob', 'drop: query works after index drop')

        ! DROP TABLE: the table is gone.
        call run(db, "DROP TABLE t", res, rs)
        call check(rs == SQR_OK .and. res%kind == SQLRES_MSG, 'drop: table dropped')
        call check(db_table_index(db, 't') <= 0, 'drop: table no longer exists')
        call run(db, "SELECT * FROM t", res, rs)
        call check(rs == SQR_NOT_FOUND, 'drop: dropped table not selectable')

        call db_close(db)
    end subroutine

    ! ===================== SELECT projection =====================

    subroutine test_select_projection()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, name CHAR(16))", res, rs)
        call run(db, "INSERT INTO t VALUES (1,'Alice'),(2,'Bob')", res, rs)

        call run(db, "SELECT name FROM t ORDER BY id", res, rs)
        call check(rs == SQR_OK .and. res%ncols == 1, 'select: single column projection')
        call check(trim(res%colnames(1)) == 'name', 'select: projected column name')
        call check(cell(res,1,1) == 'Alice' .and. cell(res,2,1) == 'Bob', 'select: projected values')

        call run(db, "SELECT * FROM t ORDER BY id", res, rs)
        call check(res%ncols == 2, 'select: star yields all columns')
        call check(cell(res,1,1) == '1' .and. cell(res,1,2) == 'Alice', 'select: star row 1 cells')

        call db_close(db)
    end subroutine

    ! ===================== WHERE predicates =====================

    subroutine test_where_predicates()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, age INTEGER, name CHAR(8))", res, rs)
        call run(db, "INSERT INTO t VALUES (1,20,'a'),(2,30,'b'),(3,40,'c'),(4,50,'d')", res, rs)

        call run(db, "SELECT id FROM t WHERE age = 30", res, rs)
        call check(res%nrows == 1 .and. cell(res,1,1) == '2', 'where: equality')
        call run(db, "SELECT id FROM t WHERE age <> 30 ORDER BY id", res, rs)
        call check(res%nrows == 3, 'where: not-equal')
        call run(db, "SELECT id FROM t WHERE age >= 40 ORDER BY id", res, rs)
        call check(res%nrows == 2 .and. cell(res,1,1) == '3', 'where: >=')
        call run(db, "SELECT id FROM t WHERE age BETWEEN 25 AND 45 ORDER BY id", res, rs)
        call check(res%nrows == 2, 'where: between band')

        ! AND binds tighter than OR (DNF): age=20 OR (age>=40 AND name='c')
        call run(db, "SELECT id FROM t WHERE age = 20 OR age >= 40 AND name = 'c' ORDER BY id", res, rs)
        call check(res%nrows == 2 .and. cell(res,1,1) == '1' .and. cell(res,2,1) == '3', 'where: AND/OR precedence')

        call run(db, "SELECT id FROM t WHERE name = 'b'", res, rs)
        call check(res%nrows == 1 .and. cell(res,1,1) == '2', 'where: char equality')

        call db_close(db)
    end subroutine

    ! ===================== ORDER BY / LIMIT =====================

    subroutine test_order_and_limit()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, v INTEGER)", res, rs)
        call run(db, "INSERT INTO t VALUES (1,30),(2,10),(3,20),(4,40)", res, rs)

        call run(db, "SELECT id FROM t ORDER BY v ASC", res, rs)
        call check(cell(res,1,1) == '2' .and. cell(res,4,1) == '4', 'order: ascending')
        call run(db, "SELECT id FROM t ORDER BY v DESC", res, rs)
        call check(cell(res,1,1) == '4' .and. cell(res,4,1) == '2', 'order: descending')
        call run(db, "SELECT id FROM t ORDER BY v DESC LIMIT 2", res, rs)
        call check(res%nrows == 2 .and. cell(res,1,1) == '4', 'order: limit caps rows')

        call db_close(db)
    end subroutine

    ! ===================== NULL handling =====================

    subroutine test_null_handling()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, age INTEGER)", res, rs)
        call run(db, "INSERT INTO t (id) VALUES (1)", res, rs)           ! age NULL
        call run(db, "INSERT INTO t VALUES (2, 25)", res, rs)
        call run(db, "INSERT INTO t VALUES (3, NULL)", res, rs)

        call run(db, "SELECT id FROM t WHERE age IS NULL ORDER BY id", res, rs)
        call check(res%nrows == 2 .and. cell(res,1,1) == '1' .and. cell(res,2,1) == '3', 'null: IS NULL')
        call run(db, "SELECT id FROM t WHERE age IS NOT NULL", res, rs)
        call check(res%nrows == 1 .and. cell(res,1,1) == '2', 'null: IS NOT NULL')
        ! a comparison against NULL never matches
        call run(db, "SELECT id FROM t WHERE age >= 0 ORDER BY id", res, rs)
        call check(res%nrows == 1, 'null: comparison excludes NULLs')
        ! NULL renders as the NULL cell and sorts last ascending
        call run(db, "SELECT id, age FROM t ORDER BY age", res, rs)
        call check(res%cells(3,2)%is_null, 'null: NULL sorts last ascending')

        call db_close(db)
    end subroutine

    ! ===================== TEXT column =====================

    subroutine test_text_column()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, note TEXT)", res, rs)
        call run(db, "INSERT INTO t VALUES (1, 'the quick brown fox')", res, rs)
        call check(rs == SQR_OK, 'text: insert with text value')
        call run(db, "SELECT note FROM t WHERE id = 1", res, rs)
        call check(cell(res,1,1) == 'the quick brown fox', 'text: round-trips through blob')
        ! comparing a TEXT column is rejected
        call run(db, "SELECT id FROM t WHERE note = 'x'", res, rs)
        call check(rs == SQR_INVALID, 'text: comparison rejected')

        call db_close(db)
    end subroutine

    ! ===================== UPDATE / DELETE =====================

    subroutine test_update_delete()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, v INTEGER)", res, rs)
        call run(db, "INSERT INTO t VALUES (1,10),(2,20),(3,30)", res, rs)

        call run(db, "UPDATE t SET v = 99 WHERE id = 2", res, rs)
        call check(rs == SQR_OK .and. res%count == 1, 'update: one row affected')
        call run(db, "SELECT v FROM t WHERE id = 2", res, rs)
        call check(cell(res,1,1) == '99', 'update: value changed')

        ! Rows are now (1,10),(2,99),(3,30); v>=30 matches id 2 and 3.
        call run(db, "DELETE FROM t WHERE v >= 30", res, rs)
        call check(res%count == 2, 'delete: count')
        call run(db, "SELECT id FROM t ORDER BY id", res, rs)
        call check(res%nrows == 1, 'delete: rows removed')

        call run(db, "DELETE FROM t", res, rs)
        call check(res%count == 1, 'delete: no-where deletes all')

        call db_close(db)
    end subroutine

    ! ===================== index vs scan parity =====================

    ! The same equality query must return identical rows whether driven by a
    ! full scan (no index) or an index cursor.  Build the answer first without
    ! an index, then create the index and re-run, comparing cell by cell.
    subroutine test_index_scan_parity()
        type(db_t) :: db
        type(sql_result_t) :: scan_res, idx_res
        integer :: rs, i
        logical :: same

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, k INTEGER, name CHAR(8))", scan_res, rs)
        call run(db, "INSERT INTO t VALUES (1,5,'a'),(2,7,'b'),(3,5,'c'),(4,9,'d'),(5,5,'e')", scan_res, rs)

        call run(db, "SELECT id, name FROM t WHERE k = 5 ORDER BY id", scan_res, rs)
        call check(rs == SQR_OK .and. scan_res%nrows == 3, 'parity: scan finds 3 rows')

        call run(db, "CREATE INDEX ON t (k)", idx_res, rs)
        call check(rs == SQR_OK, 'parity: index created')
        call run(db, "SELECT id, name FROM t WHERE k = 5 ORDER BY id", idx_res, rs)
        call check(rs == SQR_OK .and. idx_res%nrows == 3, 'parity: index finds 3 rows')

        same = scan_res%nrows == idx_res%nrows .and. scan_res%ncols == idx_res%ncols
        if (same) then
            do i = 1, scan_res%nrows
                if (cell(scan_res,i,1) /= cell(idx_res,i,1)) same = .false.
                if (cell(scan_res,i,2) /= cell(idx_res,i,2)) same = .false.
            end do
        end if
        call check(same, 'parity: scan and index results identical')

        call db_close(db)
    end subroutine

    ! Build a table, populate it, run `qry` as a scan, then again with an index
    ! on `idxcol`, and assert both plans return the same row count (and that the
    ! scan count is the expected one).  The invariant under test: creating an
    ! index must never change a query's result.  Covers the literal-type ×
    ! column-type × trailing-blank matrix that the INT-on-INT test above misses.
    subroutine parity_probe(tag, ddl, ins, idxcol, qry, expect)
        character(len=*), intent(in) :: tag, ddl, ins, idxcol, qry
        integer,          intent(in) :: expect
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs, nscan

        call fresh_db(db)
        call run(db, ddl, res, rs)
        call check(rs == SQR_OK, tag // ': table created')
        call run(db, ins, res, rs)
        call check(rs == SQR_OK, tag // ': rows inserted')

        call run(db, qry, res, rs)
        nscan = res%nrows
        call check(rs == SQR_OK .and. nscan == expect, tag // ': scan count as expected')

        call run(db, "CREATE INDEX ON " // trim(idxcol), res, rs)
        call check(rs == SQR_OK, tag // ': index created')
        call run(db, qry, res, rs)
        call check(rs == SQR_OK .and. res%nrows == nscan, tag // ': index result == scan result')

        call db_close(db)
    end subroutine

    ! The parity sweep proper: each case is a distinct planner divergence that a
    ! plain INT-on-INT parity check would not surface.
    subroutine test_index_scan_parity_sweep()
        ! --- INT column, literal type varied (H4 divergence 1) ---
        call parity_probe('sweep int/int', &
            "CREATE TABLE t (i INTEGER)", "INSERT INTO t VALUES (1),(3),(3),(5)", &
            't (i)', "SELECT i FROM t WHERE i = 3", 2)
        call parity_probe('sweep int/real-integral', &
            "CREATE TABLE t (i INTEGER)", "INSERT INTO t VALUES (1),(3),(3),(5)", &
            't (i)', "SELECT i FROM t WHERE i = 3.0", 2)
        call parity_probe('sweep int/real-fractional', &
            "CREATE TABLE t (i INTEGER)", "INSERT INTO t VALUES (1),(3),(3),(5)", &
            't (i)', "SELECT i FROM t WHERE i = 3.5", 0)

        ! --- REAL column, literal type varied (already parity-safe; guard it) ---
        call parity_probe('sweep real/int', &
            "CREATE TABLE t (r REAL)", "INSERT INTO t VALUES (1.0),(3.0),(3.0),(5.0)", &
            't (r)', "SELECT r FROM t WHERE r = 3", 2)
        call parity_probe('sweep real/real', &
            "CREATE TABLE t (r REAL)", "INSERT INTO t VALUES (1.0),(3.0),(3.0),(5.0)", &
            't (r)', "SELECT r FROM t WHERE r = 3.0", 2)

        ! --- CHAR column, trailing blanks in store and/or query (H4 divergence 2) ---
        call parity_probe('sweep char plain', &
            "CREATE TABLE t (c CHAR(8))", "INSERT INTO t VALUES ('a'),('b'),('a')", &
            't (c)', "SELECT c FROM t WHERE c = 'a'", 2)
        call parity_probe('sweep char query-blank', &
            "CREATE TABLE t (c CHAR(8))", "INSERT INTO t VALUES ('a'),('b'),('a')", &
            't (c)', "SELECT c FROM t WHERE c = 'a  '", 2)
        call parity_probe('sweep char store-blank', &
            "CREATE TABLE t (c CHAR(8))", "INSERT INTO t VALUES ('b  '),('x'),('b ')", &
            't (c)', "SELECT c FROM t WHERE c = 'b'", 2)
        call parity_probe('sweep char store+query-blank', &
            "CREATE TABLE t (c CHAR(8))", "INSERT INTO t VALUES ('b  '),('x'),('b ')", &
            't (c)', "SELECT c FROM t WHERE c = 'b '", 2)
    end subroutine

    ! ===================== transactions =====================

    subroutine test_transactions()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, v INTEGER)", res, rs)
        call run(db, "INSERT INTO t VALUES (1,10)", res, rs)

        call run(db, "BEGIN", res, rs)
        call check(rs == SQR_OK, 'txn: begin')
        call run(db, "INSERT INTO t VALUES (2,20),(3,30)", res, rs)
        call run(db, "ROLLBACK", res, rs)
        call check(rs == SQR_OK, 'txn: rollback')
        call run(db, "SELECT id FROM t", res, rs)
        call check(res%nrows == 1, 'txn: rolled-back inserts vanish')

        call run(db, "BEGIN", res, rs)
        call run(db, "INSERT INTO t VALUES (4,40)", res, rs)
        call run(db, "COMMIT", res, rs)
        call check(rs == SQR_OK, 'txn: commit')
        call run(db, "SELECT id FROM t ORDER BY id", res, rs)
        call check(res%nrows == 2, 'txn: committed insert persists')

        call db_close(db)
    end subroutine

    ! ===================== executor error paths =====================

    subroutine test_exec_errors()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE t (id INTEGER, name CHAR(8))", res, rs)

        call run(db, "SELECT * FROM nope", res, rs)
        call check(rs == SQR_NOT_FOUND, 'exec err: unknown table')
        call run(db, "SELECT bogus FROM t", res, rs)
        call check(rs == SQR_INVALID, 'exec err: unknown projection column')
        call run(db, "SELECT * FROM t WHERE bogus = 1", res, rs)
        call check(rs == SQR_INVALID, 'exec err: unknown where column')
        call run(db, "INSERT INTO t VALUES (1)", res, rs)
        call check(rs == SQR_INVALID, 'exec err: wrong value count')
        call run(db, "INSERT INTO t VALUES ('x', 'y')", res, rs)
        call check(rs == SQR_INVALID, 'exec err: type mismatch on insert')
        call run(db, "SELECT * FROM t WHERE id = 'x'", res, rs)
        call check(rs == SQR_INVALID, 'exec err: numeric column vs string literal')

        ! M7: an over-long CHAR literal must be rejected, not silently truncated
        ! (row_set_char would clip 'toolongname' to 8 chars and report success).
        ! Trailing blanks are insignificant, so an exact-fit-plus-blanks is OK.
        call run(db, "INSERT INTO t VALUES (1, 'toolongname')", res, rs)
        call check(rs == SQR_INVALID, 'exec err: over-long CHAR rejected on insert')
        call run(db, "INSERT INTO t VALUES (2, 'exactly8')", res, rs)
        call check(rs == SQR_OK, 'exec err: exact-fit CHAR accepted')
        call run(db, "UPDATE t SET name = 'muchtoolong' WHERE id = 2", res, rs)
        call check(rs == SQR_INVALID, 'exec err: over-long CHAR rejected on update')
        ! The rejected rows/updates left nothing behind.
        call run(db, "SELECT id FROM t", res, rs)
        call check(rs == SQR_OK .and. res%nrows == 1, 'exec err: only the valid row persisted')

        call db_close(db)
    end subroutine

    ! ===================== functional example =====================

    ! A small end-to-end story: a typed schema, an index, queries with
    ! filtering / ordering / limiting, an update, a delete, and a re-query —
    ! the "simple example" required of every project.
    subroutine test_functional_example()
        type(db_t) :: db
        type(sql_result_t) :: res
        integer :: rs

        call fresh_db(db)
        call run(db, "CREATE TABLE employee (id INTEGER, name CHAR(20), dept CHAR(12), salary REAL)", res, rs)
        call check(rs == SQR_OK, 'example: schema created')
        call run(db, "CREATE INDEX ON employee (dept)", res, rs)

        call run(db, "INSERT INTO employee VALUES " // &
            "(1,'Alice','eng',55000.0)," // &
            "(2,'Bob','eng',48000.0)," // &
            "(3,'Carol','sales',52000.0)," // &
            "(4,'Dave','sales',45000.0)," // &
            "(5,'Eve','eng',61000.0)", res, rs)
        call check(rs == SQR_OK .and. res%count == 5, 'example: five employees inserted')

        ! Index-driven equality on dept, ordered by salary.
        call run(db, "SELECT name, salary FROM employee WHERE dept = 'eng' ORDER BY salary DESC", res, rs)
        call check(res%nrows == 3 .and. cell(res,1,1) == 'Eve', 'example: top eng by salary')

        ! Top earner overall.
        call run(db, "SELECT name FROM employee ORDER BY salary DESC LIMIT 1", res, rs)
        call check(cell(res,1,1) == 'Eve', 'example: highest paid')

        ! Give every sales person a raise, then check the band.
        call run(db, "UPDATE employee SET salary = 50000.0 WHERE dept = 'sales' AND salary < 50000.0", res, rs)
        call check(res%count == 1, 'example: targeted raise')
        call run(db, "SELECT name FROM employee WHERE salary BETWEEN 50000.0 AND 60000.0 ORDER BY name", res, rs)
        call check(res%nrows == 3, 'example: salary band query')

        ! Remove the lowest earner and confirm the headcount.
        call run(db, "DELETE FROM employee WHERE salary < 50000.0", res, rs)
        call check(res%count == 1, 'example: trim lowest earner')
        call run(db, "SELECT * FROM employee", res, rs)
        call check(res%nrows == 4, 'example: final headcount')

        call db_close(db)
    end subroutine

end program utest_sql

!! `sql` executor submodule: turn a parsed `sql_stmt_t` into engine calls and
!! a `sql_result_t`, plus the REPL renderer.  Front-end only — every data
!! operation goes through the public `db_*` API of `sqr`.  Shares the helpers
!! of `sql_base` by host association.
!!
!! The planner is intentionally small: a SELECT/DELETE/UPDATE whose WHERE is a
!! single equality on an indexed column is driven through `db_find_range`
!! (an ordered cursor); everything else is a full `db_scan`.  Either way the
!! complete WHERE predicate is re-evaluated on every gathered row, so an
!! index-driven plan and a scan plan always yield identical results.

submodule (sql:sql_base) sql_executor
    implicit none

    !! Gathering context for a `db_scan` (and reused by the cursor path): the
    !! table being read, the WHERE clause to apply, and the growing set of
    !! matching rows (ids + record buffers).
    type :: row_match_ctx_t
        type(table_t), pointer :: t => null()
        logical :: has_where = .false.
        type(sql_cond_group_t), allocatable :: groups(:)
        integer :: reclen = 0
        integer(int32),   allocatable :: rids(:)
        character(len=:), allocatable :: bufs(:)
        integer :: n = 0
    end type

contains

    ! ===================== top-level entry points =====================

    module subroutine sql_run(db, text, res, stat, errmsg)
        type(db_t),         intent(inout), target   :: db
        character(len=*),   intent(in)              :: text
        type(sql_result_t), intent(out)             :: res
        integer,            intent(out),  optional  :: stat
        character(len=*),   intent(inout), optional :: errmsg
        type(sql_stmt_t) :: stmt
        integer :: rs
        character(len=200) :: emsg
        if (present(stat)) stat = SQR_OK
        emsg = ''
        call sql_parse(text, stmt, rs, emsg)
        if (rs /= SQR_OK) then
            call set_err(stat, errmsg, rs, trim(emsg))
            return
        end if
        if (stmt%kind == ST_NONE) then
            res%kind = SQLRES_NONE        ! blank line
            return
        end if
        call sql_exec(db, stmt, res, stat, errmsg)
    end subroutine

    module subroutine sql_exec(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target   :: db
        type(sql_stmt_t),   intent(in)              :: stmt
        type(sql_result_t), intent(out)             :: res
        integer,            intent(out),  optional  :: stat
        character(len=*),   intent(inout), optional :: errmsg

        if (present(stat)) stat = SQR_OK

        ! Transaction control statements need no open table.
        select case (stmt%kind)
        case (ST_BEGIN, ST_COMMIT, ST_ROLLBACK)
            call exec_txn(db, stmt, res, stat, errmsg)
            return
        end select

        if (.not. db%opened) then
            call set_err(stat, errmsg, SQR_INVALID, 'no database open')
            return
        end if

        select case (stmt%kind)
        case (ST_CREATE_TABLE); call exec_create_table(db, stmt, res, stat, errmsg)
        case (ST_DROP_TABLE);   call exec_simple_ddl(db, stmt, res, stat, errmsg)
        case (ST_CREATE_INDEX); call exec_create_index(db, stmt, res, stat, errmsg)
        case (ST_DROP_INDEX);   call exec_simple_ddl(db, stmt, res, stat, errmsg)
        case (ST_ADD_COLUMN);   call exec_add_column(db, stmt, res, stat, errmsg)
        case (ST_DROP_COLUMN);  call exec_simple_ddl(db, stmt, res, stat, errmsg)
        case (ST_INSERT);       call exec_insert(db, stmt, res, stat, errmsg)
        case (ST_DELETE);       call exec_delete(db, stmt, res, stat, errmsg)
        case (ST_UPDATE);       call exec_update(db, stmt, res, stat, errmsg)
        case (ST_SELECT);       call exec_select(db, stmt, res, stat, errmsg)
        case default
            call set_err(stat, errmsg, SQR_INVALID, 'unsupported statement')
        end select
    end subroutine

    ! ===================== DDL =====================

    subroutine exec_create_table(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout) :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        integer :: rs
        character(len=160) :: em
        em = ''
        call db_create_table(db, trim(stmt%table), stmt%coldefs, rs, em)
        if (rs /= SQR_OK) then
            call set_err(stat, errmsg, rs, 'create table: ' // trim(em))
            return
        end if
        call msg_result(res, 'table "' // trim(stmt%table) // '" created (' // &
                         itoa(size(stmt%coldefs)) // ' columns)')
    end subroutine

    subroutine exec_create_index(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout) :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        integer :: rs
        call db_create_index(db, trim(stmt%table), stmt%names, rs, unique=stmt%unique)
        if (rs /= SQR_OK) then
            call set_err(stat, errmsg, rs, 'create index failed')
            return
        end if
        call msg_result(res, merge('unique index created', 'index created       ', stmt%unique))
    end subroutine

    subroutine exec_add_column(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout) :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        integer :: rs
        character(len=160) :: em
        em = ''
        call db_add_column(db, trim(stmt%table), stmt%coldefs(1), rs, em)
        if (rs /= SQR_OK) then
            call set_err(stat, errmsg, rs, 'add column: ' // trim(em))
            return
        end if
        call msg_result(res, 'column "' // trim(stmt%coldefs(1)%name) // '" added')
    end subroutine

    ! DROP TABLE / DROP INDEX / ALTER ... DROP COLUMN — each a single engine
    ! call with a uniform message.
    subroutine exec_simple_ddl(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout) :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        integer :: rs
        character(len=160) :: em
        em = ''
        rs = SQR_OK                          ! every arm below sets it; satisfies -Wall
        select case (stmt%kind)
        case (ST_DROP_TABLE)
            call db_drop_table(db, trim(stmt%table), rs)
            if (rs == SQR_OK) call msg_result(res, 'table "' // trim(stmt%table) // '" dropped')
        case (ST_DROP_INDEX)
            call db_drop_index(db, trim(stmt%table), stmt%names, rs)
            if (rs == SQR_OK) call msg_result(res, 'index dropped')
        case (ST_DROP_COLUMN)
            call db_drop_column(db, trim(stmt%table), trim(stmt%names(1)), rs, em)
            if (rs == SQR_OK) call msg_result(res, 'column "' // trim(stmt%names(1)) // '" dropped')
        end select
        if (rs /= SQR_OK) then
            if (len_trim(em) > 0) then
                call set_err(stat, errmsg, rs, trim(em))
            else
                call set_err(stat, errmsg, rs, 'operation failed')
            end if
        end if
    end subroutine

    ! ===================== transactions =====================

    subroutine exec_txn(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        integer :: rs
        rs = SQR_OK                          ! every arm below sets it; satisfies -Wall
        select case (stmt%kind)
        case (ST_BEGIN)
            call db_begin(db, rs)
            if (rs == SQR_OK) call msg_result(res, 'transaction started')
        case (ST_COMMIT)
            call db_commit(db, rs)
            if (rs == SQR_OK) call msg_result(res, 'committed')
        case (ST_ROLLBACK)
            call db_rollback(db, rs)
            if (rs == SQR_OK) call msg_result(res, 'rolled back')
        end select
        if (rs /= SQR_OK) call set_err(stat, errmsg, rs, 'transaction control failed')
    end subroutine

    ! ===================== INSERT =====================

    subroutine exec_insert(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg

        integer :: ti, rs, r, c, nrows, ncols, ci
        integer, allocatable :: target(:)        ! value-column k -> table column index
        character(len=:), allocatable :: buf
        integer(int32) :: rid
        logical :: own
        character(len=160) :: em

        ti = require_table(db, stmt%table, stat, errmsg)
        if (ti == 0) return
        nrows = size(stmt%values, 1)
        ncols = size(stmt%values, 2)

        ! Map each VALUES position to a table column.
        allocate(target(ncols))
        associate (t => db%tables(ti))
            if (stmt%insert_named) then
                do c = 1, ncols
                    ci = col_index(t, trim(stmt%names(c)))
                    if (ci == 0) then
                        call set_err(stat, errmsg, SQR_INVALID, 'no such column: ' // trim(stmt%names(c)))
                        return
                    end if
                    target(c) = ci
                end do
            else
                if (ncols /= t%ncols) then
                    call set_err(stat, errmsg, SQR_INVALID, &
                        'expected ' // itoa(t%ncols) // ' values per row, got ' // itoa(ncols))
                    return
                end if
                target = [(c, c = 1, ncols)]
            end if
        end associate

        own = .not. db%jrnl%active           ! own the txn unless inside an explicit one
        if (own) then
            call db_begin(db, rs)
            if (rs /= SQR_OK) then
                call set_err(stat, errmsg, rs, 'cannot begin transaction')
                return
            end if
        end if

        rows: do r = 1, nrows
            associate (t => db%tables(ti))
                call row_alloc(buf, t%record_size)
                ! Columns not named default to NULL.
                do c = 1, t%ncols
                    call row_set_null(buf, t%cols(c))
                end do
                do c = 1, ncols
                    em = ''
                    call put_lit(buf, db%tables(ti)%cols(target(c)), stmt%values(r, c), rs, em)
                    if (rs /= SQR_OK) then
                        call fail_txn(db, own)
                        call set_err(stat, errmsg, rs, trim(em))
                        return
                    end if
                end do
            end associate
            call db_insert(db, trim(stmt%table), buf, rid, rs)
            if (rs /= SQR_OK) then
                call fail_txn(db, own)
                call set_err(stat, errmsg, rs, 'insert failed (duplicate key or I/O error)')
                return
            end if
            ! Write any TEXT column values now that the row exists.
            do c = 1, ncols
                if (db%tables(ti)%cols(target(c))%dtype /= DT_TEXT) cycle
                if (stmt%values(r, c)%ltype == LIT_NULL) cycle
                call db_set_text(db, trim(stmt%table), rid, &
                    trim(db%tables(ti)%cols(target(c))%name), stmt%values(r, c)%sval, rs)
                if (rs /= SQR_OK) then
                    call fail_txn(db, own)
                    call set_err(stat, errmsg, rs, 'failed to write text value')
                    return
                end if
            end do
        end do rows

        if (own) then
            call db_commit(db, rs)
            if (rs /= SQR_OK) then
                call set_err(stat, errmsg, rs, 'commit failed')
                return
            end if
        end if
        call count_result(res, nrows)
    end subroutine

    ! ===================== DELETE =====================

    subroutine exec_delete(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        type(row_match_ctx_t) :: g
        integer :: ti, rs, k
        logical :: own
        ti = require_table(db, stmt%table, stat, errmsg)
        if (ti == 0) return
        if (.not. validate_where(db%tables(ti), stmt, stat, errmsg)) return
        call gather(db, ti, stmt, g, rs, errmsg)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        own = .not. db%jrnl%active
        if (own) then
            call db_begin(db, rs)
            if (rs /= SQR_OK) then; call set_err(stat, errmsg, rs, 'cannot begin transaction'); return; end if
        end if
        do k = 1, g%n
            call db_delete(db, trim(stmt%table), g%rids(k), rs)
            if (rs /= SQR_OK) then
                call fail_txn(db, own)
                call set_err(stat, errmsg, rs, 'delete failed')
                return
            end if
        end do
        if (own) then
            call db_commit(db, rs)
            if (rs /= SQR_OK) then; call set_err(stat, errmsg, rs, 'commit failed'); return; end if
        end if
        call count_result(res, g%n)
    end subroutine

    ! ===================== UPDATE =====================

    subroutine exec_update(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        type(row_match_ctx_t) :: g
        integer :: ti, rs, k, s, ci
        integer, allocatable :: setcol(:)
        character(len=:), allocatable :: buf
        logical :: own
        character(len=160) :: em

        ti = require_table(db, stmt%table, stat, errmsg)
        if (ti == 0) return
        if (.not. validate_where(db%tables(ti), stmt, stat, errmsg)) return

        ! Resolve SET target columns.
        allocate(setcol(size(stmt%set_cols)))
        associate (t => db%tables(ti))
            do s = 1, size(stmt%set_cols)
                ci = col_index(t, trim(stmt%set_cols(s)))
                if (ci == 0) then
                    call set_err(stat, errmsg, SQR_INVALID, 'no such column: ' // trim(stmt%set_cols(s)))
                    return
                end if
                setcol(s) = ci
            end do
        end associate

        call gather(db, ti, stmt, g, rs, errmsg)
        if (rs /= SQR_OK) then; if (present(stat)) stat = rs; return; end if

        own = .not. db%jrnl%active
        if (own) then
            call db_begin(db, rs)
            if (rs /= SQR_OK) then; call set_err(stat, errmsg, rs, 'cannot begin transaction'); return; end if
        end if
        rows: do k = 1, g%n
            call row_alloc(buf, db%tables(ti)%record_size)
            call db_get(db, trim(stmt%table), g%rids(k), buf, rs)
            if (rs /= SQR_OK) then
                call fail_txn(db, own); call set_err(stat, errmsg, rs, 'update: row vanished'); return
            end if
            do s = 1, size(setcol)
                em = ''
                call put_lit(buf, db%tables(ti)%cols(setcol(s)), stmt%set_vals(s), rs, em)
                if (rs /= SQR_OK) then
                    call fail_txn(db, own); call set_err(stat, errmsg, rs, trim(em)); return
                end if
            end do
            call db_update(db, trim(stmt%table), g%rids(k), buf, rs)
            if (rs /= SQR_OK) then
                call fail_txn(db, own); call set_err(stat, errmsg, rs, 'update failed'); return
            end if
            ! TEXT columns in the SET list.
            do s = 1, size(setcol)
                if (db%tables(ti)%cols(setcol(s))%dtype /= DT_TEXT) cycle
                if (stmt%set_vals(s)%ltype == LIT_NULL) cycle
                call db_set_text(db, trim(stmt%table), g%rids(k), &
                    trim(db%tables(ti)%cols(setcol(s))%name), stmt%set_vals(s)%sval, rs)
                if (rs /= SQR_OK) then
                    call fail_txn(db, own); call set_err(stat, errmsg, rs, 'failed to write text value'); return
                end if
            end do
        end do rows
        if (own) then
            call db_commit(db, rs)
            if (rs /= SQR_OK) then; call set_err(stat, errmsg, rs, 'commit failed'); return; end if
        end if
        call count_result(res, g%n)
    end subroutine

    ! ===================== SELECT =====================

    subroutine exec_select(db, stmt, res, stat, errmsg)
        type(db_t),         intent(inout), target :: db
        type(sql_stmt_t),   intent(in)    :: stmt
        type(sql_result_t), intent(out)   :: res
        integer,            intent(out),  optional :: stat
        character(len=*),   intent(inout), optional :: errmsg
        type(row_match_ctx_t) :: g
        integer :: ti, rs, nproj, j, i, oc, nout
        integer, allocatable :: proj(:), perm(:)

        ti = require_table(db, stmt%table, stat, errmsg)
        if (ti == 0) return
        if (.not. validate_where(db%tables(ti), stmt, stat, errmsg)) return
        if (.not. validate_projection(db%tables(ti), stmt, proj, stat, errmsg)) return
        nproj = size(proj)

        ! ORDER BY column (must exist and be sortable).
        oc = 0
        if (stmt%has_order) then
            oc = col_index(db%tables(ti), trim(stmt%order_col))
            if (oc == 0) then
                call set_err(stat, errmsg, SQR_INVALID, 'no such column: ' // trim(stmt%order_col))
                return
            end if
            if (db%tables(ti)%cols(oc)%dtype == DT_TEXT) then
                call set_err(stat, errmsg, SQR_INVALID, 'cannot ORDER BY a TEXT column')
                return
            end if
        end if

        call gather(db, ti, stmt, g, rs, errmsg)
        if (rs /= SQR_OK) then; if (present(stat)) stat = rs; return; end if

        ! Order the gathered rows.
        allocate(perm(g%n))
        perm = [(i, i = 1, g%n)]
        if (stmt%has_order .and. g%n > 1) call order_rows(g, oc, stmt%order_desc, perm)

        ! Apply LIMIT.
        nout = g%n
        if (stmt%has_limit) nout = min(nout, stmt%limit_n)

        ! Build the result grid.
        res%kind  = SQLRES_ROWS
        res%nrows = nout
        res%ncols = nproj
        allocate(res%colnames(nproj))
        do j = 1, nproj
            res%colnames(j) = db%tables(ti)%cols(proj(j))%name
        end do
        allocate(res%cells(nout, nproj))
        do i = 1, nout
            do j = 1, nproj
                call render_cell(db, ti, g%rids(perm(i)), g%bufs(perm(i)), proj(j), res%cells(i, j))
            end do
        end do
    end subroutine

    ! ===================== row gathering (index or scan) =====================

    ! Gather every live row matching `stmt`'s WHERE into `g`.  Uses an index
    ! cursor when the WHERE is a single equality on an indexed column,
    ! otherwise a full scan; the full predicate is re-applied either way.
    subroutine gather(db, ti, stmt, g, rs, errmsg)
        type(db_t),            intent(inout), target :: db
        integer,               intent(in)    :: ti
        type(sql_stmt_t),      intent(in)    :: stmt
        type(row_match_ctx_t), intent(out)   :: g
        integer,               intent(out)   :: rs
        character(len=*),      intent(inout), optional :: errmsg
        logical :: used_index

        rs = SQR_OK
        g%t => db%tables(ti)
        g%reclen = db%tables(ti)%record_size
        g%has_where = stmt%has_where
        if (stmt%has_where) g%groups = stmt%where_groups
        g%n = 0

        used_index = .false.
        if (stmt%has_where) call try_index_gather(db, ti, stmt, g, used_index, rs, errmsg)
        if (rs /= SQR_OK) return
        if (used_index) return

        call db_scan(db, trim(stmt%table), collect_cb, g, rs)
        if (rs /= SQR_OK .and. present(errmsg)) errmsg = 'scan failed'
    end subroutine

    ! If the WHERE is exactly one equality (`col = literal`) on a column with a
    ! usable index, drive it through db_find_range as a single-key band and set
    ! used_index.  Any non-equality, multi-condition, or un-indexed case leaves
    ! used_index .false. (caller falls back to a scan).
    subroutine try_index_gather(db, ti, stmt, g, used_index, rs, errmsg)
        type(db_t),            intent(inout), target :: db
        integer,               intent(in)    :: ti
        type(sql_stmt_t),      intent(in)    :: stmt
        type(row_match_ctx_t), intent(inout) :: g
        logical,               intent(out)   :: used_index
        integer,               intent(out)   :: rs
        character(len=*),      intent(inout), optional :: errmsg
        type(sql_cond_t) :: c
        type(db_cursor_t) :: cur
        integer :: ci, frs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        logical :: ok

        used_index = .false.
        rs = SQR_OK
        if (size(stmt%where_groups) /= 1) return
        if (size(stmt%where_groups(1)%conds) /= 1) return
        c = stmt%where_groups(1)%conds(1)
        if (c%op /= OP_EQ) return
        ci = col_index(db%tables(ti), trim(c%col))
        if (ci == 0) return

        ! Open a single-key band on the column's type; SQR_NOT_FOUND ⇒ no index.
        associate (col => db%tables(ti)%cols(ci))
            select case (col%dtype)
            case (DT_INT)
                call db_find_range(db, trim(stmt%table), trim(c%col), &
                    c%lit%ival, c%lit%ival, cur, frs)
            case (DT_REAL)
                block
                    real(real64) :: key
                    key = lit_num(c%lit)
                    call db_find_range(db, trim(stmt%table), trim(c%col), key, key, cur, frs)
                end block
            case (DT_CHAR)
                if (c%lit%ltype /= LIT_STR) return
                call db_find_range(db, trim(stmt%table), trim(c%col), &
                    c%lit%sval, c%lit%sval, cur, frs)
            case default
                return                      ! TEXT or unknown: scan
            end select
        end associate

        if (frs == SQR_NOT_FOUND) return    ! no index on the column: fall back to scan
        if (frs /= SQR_OK) then
            rs = frs
            if (present(errmsg)) errmsg = 'index lookup failed'
            return
        end if

        call row_alloc(buf, g%reclen)
        pull: do
            call db_cursor_next(db, cur, rid, buf, ok, frs)
            if (frs /= SQR_OK) then
                rs = frs
                if (present(errmsg)) errmsg = 'cursor read failed'
                return
            end if
            if (.not. ok) exit pull
            if (row_matches(g%t, g%groups, buf)) call ctx_append(g, rid, buf)
        end do pull
        used_index = .true.
    end subroutine

    ! db_scan callback: append every (matching) live row to the context.
    subroutine collect_cb(scan_db, row_id, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        stop = .false.
        select type (ctx)
        type is (row_match_ctx_t)
            if (.not. ctx%has_where) then
                call ctx_append(ctx, row_id, buf)
            else if (row_matches(ctx%t, ctx%groups, buf)) then
                call ctx_append(ctx, row_id, buf)
            end if
        end select
    end subroutine

    subroutine ctx_append(g, rid, buf)
        type(row_match_ctx_t), intent(inout) :: g
        integer(int32),        intent(in)    :: rid
        character(len=*),      intent(in)    :: buf
        integer :: newcap
        integer(int32),   allocatable :: tr(:)
        character(len=:), allocatable :: tb(:)
        if (.not. allocated(g%rids)) then
            allocate(g%rids(16))
            allocate(character(len=g%reclen) :: g%bufs(16))
        else if (g%n == size(g%rids)) then
            newcap = 2 * size(g%rids)
            allocate(tr(newcap)); tr(1:g%n) = g%rids(1:g%n); call move_alloc(tr, g%rids)
            allocate(character(len=g%reclen) :: tb(newcap))
            tb(1:g%n) = g%bufs(1:g%n); call move_alloc(tb, g%bufs)
        end if
        g%n = g%n + 1
        g%rids(g%n) = rid
        g%bufs(g%n) = buf
    end subroutine

    ! ===================== WHERE evaluation =====================

    ! Does a row buffer satisfy the WHERE clause (OR of AND-groups)?
    function row_matches(t, groups, buf) result(yes)
        type(table_t),          intent(in) :: t
        type(sql_cond_group_t), intent(in) :: groups(:)
        character(len=*),       intent(in) :: buf
        logical :: yes
        integer :: gi, ci
        logical :: grp
        yes = .false.
        do gi = 1, size(groups)
            grp = .true.
            do ci = 1, size(groups(gi)%conds)
                if (.not. cond_true(t, groups(gi)%conds(ci), buf)) then
                    grp = .false.
                    exit
                end if
            end do
            if (grp) then
                yes = .true.
                return
            end if
        end do
    end function

    ! Evaluate one validated condition against a row.  A NULL column value
    ! makes any comparison / BETWEEN false (SQL three-valued logic collapsed to
    ! "not matched"); IS NULL / IS NOT NULL test the null bit directly.
    function cond_true(t, c, buf) result(yes)
        type(table_t),    intent(in) :: t
        type(sql_cond_t), intent(in) :: c
        character(len=*), intent(in) :: buf
        logical :: yes
        integer :: ci
        logical :: isnull
        real(real64) :: v
        character(len=:), allocatable :: s
        yes = .false.
        ci = col_index(t, trim(c%col))
        if (ci == 0) return
        associate (col => t%cols(ci))
            isnull = row_is_null(buf, col)
            select case (c%op)
            case (OP_ISNULL);    yes = isnull;        return
            case (OP_ISNOTNULL); yes = .not. isnull;  return
            end select
            if (isnull) return                ! comparison vs NULL: never matches
            select case (col%dtype)
            case (DT_INT, DT_REAL)
                if (col%dtype == DT_INT) then
                    v = real(row_get_int(buf, col), real64)
                else
                    v = row_get_real(buf, col)
                end if
                select case (c%op)
                case (OP_EQ);      yes = v == lit_num(c%lit)
                case (OP_NE);      yes = v /= lit_num(c%lit)
                case (OP_LT);      yes = v <  lit_num(c%lit)
                case (OP_LE);      yes = v <= lit_num(c%lit)
                case (OP_GT);      yes = v >  lit_num(c%lit)
                case (OP_GE);      yes = v >= lit_num(c%lit)
                case (OP_BETWEEN); yes = v >= lit_num(c%lit) .and. v <= lit_num(c%lit2)
                end select
            case (DT_CHAR)
                s = trim(row_get_char(buf, col))
                select case (c%op)
                case (OP_EQ);      yes = s == trim(c%lit%sval)
                case (OP_NE);      yes = s /= trim(c%lit%sval)
                case (OP_LT);      yes = s <  trim(c%lit%sval)
                case (OP_LE);      yes = s <= trim(c%lit%sval)
                case (OP_GT);      yes = s >  trim(c%lit%sval)
                case (OP_GE);      yes = s >= trim(c%lit%sval)
                case (OP_BETWEEN); yes = s >= trim(c%lit%sval) .and. s <= trim(c%lit2%sval)
                end select
            end select
        end associate
    end function

    ! ===================== validation =====================

    ! Validate every WHERE condition against the table schema: columns exist,
    ! operators match the column type, literals are compatible, and TEXT
    ! columns are only null-tested.  Doing this once up front keeps per-row
    ! evaluation branch-free of error handling.
    function validate_where(t, stmt, stat, errmsg) result(ok)
        type(table_t),    intent(in) :: t
        type(sql_stmt_t), intent(in) :: stmt
        integer,          intent(out),  optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        logical :: ok
        integer :: gi, ci
        ok = .true.
        if (.not. stmt%has_where) return
        do gi = 1, size(stmt%where_groups)
            do ci = 1, size(stmt%where_groups(gi)%conds)
                if (.not. validate_cond(t, stmt%where_groups(gi)%conds(ci), stat, errmsg)) then
                    ok = .false.
                    return
                end if
            end do
        end do
    end function

    function validate_cond(t, c, stat, errmsg) result(ok)
        type(table_t),    intent(in) :: t
        type(sql_cond_t), intent(in) :: c
        integer,          intent(out),  optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        logical :: ok
        integer :: ci
        ok = .false.
        ci = col_index(t, trim(c%col))
        if (ci == 0) then
            call set_err(stat, errmsg, SQR_INVALID, 'no such column: ' // trim(c%col))
            return
        end if
        select case (c%op)
        case (OP_ISNULL, OP_ISNOTNULL)
            ok = .true.
            return
        end select
        associate (col => t%cols(ci))
            if (col%dtype == DT_TEXT) then
                call set_err(stat, errmsg, SQR_INVALID, &
                    'cannot compare TEXT column "' // trim(c%col) // '" (only IS [NOT] NULL)')
                return
            end if
            if (.not. lit_ok_for(col%dtype, c%lit, c%col, stat, errmsg)) return
            if (c%op == OP_BETWEEN) then
                if (.not. lit_ok_for(col%dtype, c%lit2, c%col, stat, errmsg)) return
            end if
        end associate
        ok = .true.
    end function

    ! Is literal `lit` usable against a column of type `dt`?
    function lit_ok_for(dt, lit, colname, stat, errmsg) result(ok)
        integer,          intent(in) :: dt
        type(sql_lit_t),  intent(in) :: lit
        character(len=*), intent(in) :: colname
        integer,          intent(out),  optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        logical :: ok
        ok = .false.
        if (lit%ltype == LIT_NULL) then
            call set_err(stat, errmsg, SQR_INVALID, &
                'use IS NULL / IS NOT NULL to test column "' // trim(colname) // '" for null')
            return
        end if
        select case (dt)
        case (DT_INT, DT_REAL)
            if (lit%ltype /= LIT_INT .and. lit%ltype /= LIT_REAL) then
                call set_err(stat, errmsg, SQR_INVALID, &
                    'numeric column "' // trim(colname) // '" needs a numeric literal')
                return
            end if
        case (DT_CHAR)
            if (lit%ltype /= LIT_STR) then
                call set_err(stat, errmsg, SQR_INVALID, &
                    'character column "' // trim(colname) // '" needs a string literal')
                return
            end if
        end select
        ok = .true.
    end function

    function validate_projection(t, stmt, proj, stat, errmsg) result(ok)
        type(table_t),       intent(in)  :: t
        type(sql_stmt_t),    intent(in)  :: stmt
        integer, allocatable, intent(out) :: proj(:)
        integer,             intent(out),  optional :: stat
        character(len=*),    intent(inout), optional :: errmsg
        logical :: ok
        integer :: j, ci
        ok = .false.
        if (stmt%select_star) then
            proj = [(j, j = 1, t%ncols)]
            ok = .true.
            return
        end if
        allocate(proj(size(stmt%names)))
        do j = 1, size(stmt%names)
            ci = col_index(t, trim(stmt%names(j)))
            if (ci == 0) then
                call set_err(stat, errmsg, SQR_INVALID, 'no such column: ' // trim(stmt%names(j)))
                return
            end if
            proj(j) = ci
        end do
        ok = .true.
    end function

    ! ===================== helpers =====================

    ! Resolve a table name to its slot, reporting SQR_NOT_FOUND if absent.
    function require_table(db, name, stat, errmsg) result(ti)
        type(db_t),       intent(in) :: db
        character(len=*), intent(in) :: name
        integer,          intent(out),  optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti
        ti = db_table_index(db, trim(name))
        if (ti == 0) call set_err(stat, errmsg, SQR_NOT_FOUND, 'no such table: ' // trim(name))
    end function

    pure function col_index(t, name) result(ci)
        type(table_t),    intent(in) :: t
        character(len=*), intent(in) :: name
        integer :: ci, j
        ci = 0
        do j = 1, t%ncols
            if (trim(t%cols(j)%name) == trim(name)) then
                ci = j
                return
            end if
        end do
    end function

    pure function lit_num(lit) result(x)
        type(sql_lit_t), intent(in) :: lit
        real(real64) :: x
        if (lit%ltype == LIT_INT) then
            x = real(lit%ival, real64)
        else
            x = lit%rval
        end if
    end function

    ! Place a literal into a row-buffer column with type coercion.  TEXT
    ! columns are left for db_set_text (the descriptor is written by the
    ! engine); a NULL literal sets the null bit.
    subroutine put_lit(buf, col, lit, rs, errmsg)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        type(sql_lit_t),  intent(in)    :: lit
        integer,          intent(out)   :: rs
        character(len=*), intent(inout) :: errmsg
        rs = SQR_OK
        if (lit%ltype == LIT_NULL) then
            call row_set_null(buf, col)
            return
        end if
        call row_clear_null(buf, col)
        select case (col%dtype)
        case (DT_INT)
            if (lit%ltype /= LIT_INT) then
                rs = SQR_INVALID; errmsg = 'column "' // trim(col%name) // '" needs an integer'; return
            end if
            call row_set_int(buf, col, lit%ival)
        case (DT_REAL)
            if (lit%ltype == LIT_INT) then
                call row_set_real(buf, col, real(lit%ival, real64))
            else if (lit%ltype == LIT_REAL) then
                call row_set_real(buf, col, lit%rval)
            else
                rs = SQR_INVALID; errmsg = 'column "' // trim(col%name) // '" needs a number'; return
            end if
        case (DT_CHAR)
            if (lit%ltype /= LIT_STR) then
                rs = SQR_INVALID; errmsg = 'column "' // trim(col%name) // '" needs a string'; return
            end if
            call row_set_char(buf, col, lit%sval)
        case (DT_TEXT)
            if (lit%ltype /= LIT_STR) then
                rs = SQR_INVALID; errmsg = 'column "' // trim(col%name) // '" needs a string'; return
            end if
            ! value written by db_set_text after the row exists; ensure not NULL
        end select
    end subroutine

    ! Render one column of a row into an output cell.
    subroutine render_cell(db, ti, rid, buf, ci, cell)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        integer,          intent(in)    :: ci
        type(sql_cell_t), intent(out)   :: cell
        character(len=32) :: nb
        integer :: rs
        associate (col => db%tables(ti)%cols(ci))
            if (row_is_null(buf, col)) then
                cell%is_null = .true.
                cell%text = 'NULL'
                return
            end if
            select case (col%dtype)
            case (DT_INT)
                write(nb, '(i0)') row_get_int(buf, col)
                cell%text = trim(nb)
            case (DT_REAL)
                write(nb, '(es15.8)') row_get_real(buf, col)
                cell%text = trim(adjustl(nb))
            case (DT_CHAR)
                cell%text = trim(row_get_char(buf, col))
            case (DT_TEXT)
                call db_get_text(db, trim(db%tables(ti)%name), rid, trim(col%name), cell%text, rs)
                if (rs /= SQR_OK .or. .not. allocated(cell%text)) cell%text = ''
            end select
        end associate
    end subroutine

    ! Stable insertion sort of `perm` ordering rows by column `oc`.  Ascending
    ! puts NULLs last; descending inverts the comparison (NULLs first).
    subroutine order_rows(g, oc, desc, perm)
        type(row_match_ctx_t), intent(in)    :: g
        integer,               intent(in)    :: oc
        logical,               intent(in)    :: desc
        integer,               intent(inout) :: perm(:)
        integer :: i, j, key
        do i = 2, size(perm)
            key = perm(i)
            j = i - 1
            do while (j >= 1)
                if (.not. row_gt(g, oc, perm(j), key, desc)) exit
                perm(j + 1) = perm(j)
                j = j - 1
            end do
            perm(j + 1) = key
        end do
    end subroutine

    ! Should row a sort after row b (i.e. a > b in the requested order)?
    function row_gt(g, oc, a, b, desc) result(gt)
        type(row_match_ctx_t), intent(in) :: g
        integer,               intent(in) :: oc, a, b
        logical,               intent(in) :: desc
        logical :: gt
        integer :: cmp
        cmp = cmp_rows(g, oc, a, b)        ! ascending, NULL last
        if (desc) cmp = -cmp
        gt = cmp > 0
    end function

    ! Compare rows a and b on column oc for ascending order, NULLs last.
    ! Returns -1 / 0 / 1.
    function cmp_rows(g, oc, a, b) result(cmp)
        type(row_match_ctx_t), intent(in) :: g
        integer,               intent(in) :: oc, a, b
        integer :: cmp
        logical :: na, nb
        real(real64) :: va, vb
        character(len=:), allocatable :: sa, sb
        associate (col => g%t%cols(oc))
            na = row_is_null(g%bufs(a), col)
            nb = row_is_null(g%bufs(b), col)
            if (na .or. nb) then
                ! NULL sorts last in ascending order.
                if (na .and. nb) then; cmp = 0
                else if (na) then;     cmp = 1
                else;                  cmp = -1
                end if
                return
            end if
            select case (col%dtype)
            case (DT_INT, DT_REAL)
                if (col%dtype == DT_INT) then
                    va = real(row_get_int(g%bufs(a), col), real64)
                    vb = real(row_get_int(g%bufs(b), col), real64)
                else
                    va = row_get_real(g%bufs(a), col)
                    vb = row_get_real(g%bufs(b), col)
                end if
                if (va < vb) then; cmp = -1; else if (va > vb) then; cmp = 1; else; cmp = 0; end if
            case (DT_CHAR)
                sa = trim(row_get_char(g%bufs(a), col))
                sb = trim(row_get_char(g%bufs(b), col))
                if (sa < sb) then; cmp = -1; else if (sa > sb) then; cmp = 1; else; cmp = 0; end if
            case default
                cmp = 0
            end select
        end associate
    end function

    ! Abort an owned transaction on an error path (best effort).
    subroutine fail_txn(db, own)
        type(db_t), intent(inout) :: db
        logical,    intent(in)    :: own
        integer :: rs
        if (own) call db_rollback(db, rs)
    end subroutine

    subroutine msg_result(res, text)
        type(sql_result_t), intent(out) :: res
        character(len=*),   intent(in)  :: text
        res%kind = SQLRES_MSG
        res%message = trim(text)
    end subroutine

    subroutine count_result(res, n)
        type(sql_result_t), intent(out) :: res
        integer,            intent(in)  :: n
        res%kind  = SQLRES_COUNT
        res%count = n
    end subroutine

    ! ===================== renderer =====================

    module subroutine sql_render(res, unit)
        type(sql_result_t), intent(in) :: res
        integer,            intent(in) :: unit
        integer :: j, i
        integer, allocatable :: w(:)
        select case (res%kind)
        case (SQLRES_MSG)
            write(unit, '(a)') res%message
        case (SQLRES_COUNT)
            write(unit, '(i0,a)') res%count, ' row(s) affected'
        case (SQLRES_ROWS)
            if (res%ncols == 0) then
                write(unit, '(a)') '(no columns)'
                return
            end if
            allocate(w(res%ncols))
            do j = 1, res%ncols
                w(j) = len_trim(res%colnames(j))
                do i = 1, res%nrows
                    w(j) = max(w(j), len(res%cells(i, j)%text))
                end do
            end do
            ! header
            do j = 1, res%ncols
                call put_field(unit, trim(res%colnames(j)), w(j), j == res%ncols)
            end do
            write(unit, *)
            do j = 1, res%ncols
                call put_field(unit, repeat('-', w(j)), w(j), j == res%ncols)
            end do
            write(unit, *)
            do i = 1, res%nrows
                do j = 1, res%ncols
                    call put_field(unit, res%cells(i, j)%text, w(j), j == res%ncols)
                end do
                write(unit, *)
            end do
            write(unit, '(a,i0,a)') '(', res%nrows, ' row(s))'
        end select
    end subroutine

    subroutine put_field(unit, text, width, last)
        integer,          intent(in) :: unit
        character(len=*), intent(in) :: text
        integer,          intent(in) :: width
        logical,          intent(in) :: last
        character(len=:), allocatable :: pad
        integer :: gap
        gap = max(0, width - len(text))
        pad = repeat(' ', gap)
        if (last) then
            write(unit, '(a)', advance='no') text // pad
        else
            write(unit, '(a)', advance='no') text // pad // '  '
        end if
    end subroutine

end submodule sql_executor

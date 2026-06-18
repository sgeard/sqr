! Optional regex-search command for sqrsh — the `match <col> <regex>` action.
!
! This is a Make-only feature (`make sqrsh-regex` / `make test-regex`): it links
! the sibling tcl_re project's self-contained regex engine (../tcl_re), which
! fpm cannot build. The whole tcl_re dependency is confined to this directory,
! which lives outside the dirs fpm globs (app/, src/, test/, example/), so the
! shell and the default test suite stay fpm-buildable. app/sqrsh.f90 refers to
! act_match purely as an external procedure (an interface block under
! `#ifdef SQR_WITH_REGEX`), carrying no `use` of this code.

module sqrsh_match_ctx
    use sqr,     only: column_t
    use tcl_frx, only: regex_t
    implicit none
    private
    public :: match_ctx_t

    ! Context threaded through db_scan by the `match` command: a regex compiled
    ! once (Tcl ARE, the regex_t default), the target DT_CHAR column to test,
    ! and the running match count (ti is needed to print a row).
    type :: match_ctx_t
        type(regex_t)  :: rx
        type(column_t) :: col
        integer        :: ti    = 0
        integer        :: nrows = 0
    end type match_ctx_t
end module sqrsh_match_ctx


! db_scan callback for `match`: test the chosen DT_CHAR column of each live row
! against the pre-compiled regex and print the row on a hit.
subroutine match_cb(scan_db, row_id, buf, ctx, stop)
    use, intrinsic :: iso_fortran_env, only: int32
    use sqr,             only: db_t, row_get_char
    use sqrsh_actions,   only: print_row
    use sqrsh_match_ctx, only: match_ctx_t
    class(db_t),      intent(inout) :: scan_db
    integer(int32),   intent(in)    :: row_id
    character(len=*), intent(in)    :: buf
    class(*),         intent(inout) :: ctx
    logical,          intent(out)   :: stop
    character(len=:), allocatable :: val
    stop = .false.
    select type (ctx)
    type is (match_ctx_t)
        val = row_get_char(buf, ctx%col)
        call ctx%rx%apply(val)
        if (ctx%rx%matched()) then
            call print_row(ctx%ti, row_id, buf)
            ctx%nrows = ctx%nrows + 1
        end if
    end select
end subroutine match_cb


! match <col> <regex>: print every live row whose DT_CHAR <col> matches the
! (Tcl ARE) pattern. An O(n) full scan — regex cannot use an index — but the
! pattern is compiled once and reused for every row.
function act_match(args, ctx) result(rv)
    use cmdgraph
    use dlist
    use sqr
    use sqrsh_state
    use sqrsh_actions
    use sqrsh_match_ctx, only: match_ctx_t
    type(dlist_t),    intent(in) :: args
    character(len=*), intent(in) :: ctx
    type(action_result_t) :: rv
    interface
        subroutine match_cb(scan_db, row_id, buf, ctx, stop)
            use, intrinsic :: iso_fortran_env, only: int32
            use sqr, only: db_t
            class(db_t),      intent(inout) :: scan_db
            integer(int32),   intent(in)    :: row_id
            character(len=*), intent(in)    :: buf
            class(*),         intent(inout) :: ctx
            logical,          intent(out)   :: stop
        end subroutine match_cb
    end interface
    class(dlist_node_data_t), allocatable :: nc, np
    character(len=:), allocatable :: col, pat
    type(match_ctx_t) :: mctx
    integer :: ti, ci, rs, j
    ti = db_table_index(db, ctx); if (ti == 0) return
    if (args%size() /= 2) then
        write(*,'(a)') 'usage: match <col> <regex>'
        return
    end if
    nc = args%get(1); col = node_as_char(nc)
    ci = 0
    find_col: do j = 1, db%tables(ti)%ncols
        if (db%tables(ti)%cols(j)%name == trim(col)) then
            ci = j
            exit find_col
        end if
    end do find_col
    if (ci == 0) then
        write(*,'(a,a)') 'no such column: ', trim(col)
        rv%errored = .true.
        return
    end if
    if (db%tables(ti)%cols(ci)%dtype /= DT_CHAR) then
        write(*,'(a)') 'match is only supported on char columns'
        rv%errored = .true.
        return
    end if
    np = args%get(2); pat = node_as_char(np)
    call mctx%rx%compile(pat)
    if (.not. mctx%rx%is_compiled()) then
        write(*,'(a,a)') 'bad regex: ', trim(pat)
        rv%errored = .true.
        return
    end if
    mctx%col = db%tables(ti)%cols(ci)
    mctx%ti  = ti
    call print_header(ti)
    call db_scan(db, ctx, match_cb, mctx, rs)
    call mctx%rx%delete()
    if (rs /= SQR_OK) then
        write(*,'(a)') 'scan failed'
        rv%errored = .true.
        return
    end if
    if (mctx%nrows == 0) write(*,'(a)') '(no rows match)'
end function act_match

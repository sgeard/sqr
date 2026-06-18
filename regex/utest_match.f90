! Functional test for the optional DT_CHAR regex search (the `match` command
! in sqrsh). It drives the same path the command uses: a regex compiled once
! and applied, through a db_scan callback, to a DT_CHAR column pulled with
! row_get_char.
!
! Make-only test: built and run by `make test-regex`, which links the
! self-contained tcl_re engine (../tcl_re). It is filtered out of the default
! suite. Like the regex shell, it is not an fpm target (fpm cannot build
! tcl_re, and its scanner does not honour #ifdef).
program utest_match
    use, intrinsic :: iso_fortran_env, only: int32
    use sqr
    use clib_wrap, only: c_rmtree
    use tcl_frx,   only: regex_t
    implicit none

    ! Context threaded through db_scan: the compiled regex, the column to
    ! test, and the ids of matching rows (scan order) with their count.
    type :: match_ctx_t
        type(regex_t)  :: rx
        type(column_t) :: col
        integer        :: nrows   = 0
        integer(int32) :: hits(32) = 0_int32
    end type match_ctx_t

    integer :: pass = 0, fail = 0
    character(len=*), parameter :: TEST_DIR = 'utest_match_db'
    integer :: ios

    ios = c_rmtree(TEST_DIR)
    call test_match()
    ios = c_rmtree(TEST_DIR)

    print '(a,i0,a,i0,a)', 'match tests: ', pass, ' passed, ', fail, ' failed'
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
    end subroutine check

    ! db_scan callback: test the chosen DT_CHAR column against the compiled
    ! regex and record the row id on a hit (mirrors sqrsh's match_cb).
    subroutine match_cb(scan_db, rid, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        character(len=:), allocatable :: val
        stop = .false.
        select type (ctx)
        type is (match_ctx_t)
            val = row_get_char(buf, ctx%col)
            call ctx%rx%apply(val)
            if (ctx%rx%matched() .and. ctx%nrows < size(ctx%hits)) then
                ctx%nrows = ctx%nrows + 1
                ctx%hits(ctx%nrows) = rid
            end if
        end select
    end subroutine match_cb

    ! Compile `pattern`, scan `table`.`col` for matches, return the count and
    ! the matching row ids (scan order). compiled = .false. on a bad regex.
    subroutine scan_match(db, table, col, pattern, n, ids, compiled, rs)
        type(db_t),       intent(inout) :: db
        character(len=*), intent(in)    :: table, col, pattern
        integer,          intent(out)   :: n
        integer(int32),   intent(out)   :: ids(:)
        logical,          intent(out)   :: compiled
        integer,          intent(out)   :: rs
        type(match_ctx_t) :: ctx
        integer :: ti, j, m
        n = 0; ids = 0_int32; rs = SQR_OK; compiled = .false.
        ti = db_table_index(db, table)
        do j = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(j)%name == col) ctx%col = db%tables(ti)%cols(j)
        end do
        call ctx%rx%compile(pattern)
        compiled = ctx%rx%is_compiled()
        if (.not. compiled) return
        call db_scan(db, table, match_cb, ctx, rs)
        call ctx%rx%delete()
        n = ctx%nrows
        do m = 1, min(n, size(ids))
            ids(m) = ctx%hits(m)
        end do
    end subroutine scan_match

    subroutine test_match()
        type(db_t)     :: db
        type(column_t) :: c(2)
        integer        :: rs, n
        integer(int32) :: rid, ids(32)
        logical        :: ok
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        integer :: i
        character(len=*), parameter :: words(5) = &
            [character(len=8) :: 'apple', 'apricot', 'banana', 'cherry', 'Avocado']

        c(1)%name = 'id'   ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'word' ; c(2)%dtype = DT_CHAR ; c(2)%csize = 16
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_OK, 'open db')
        call db_create_table(db, 'fruit', c, rs, emsg)
        call check(rs == SQR_OK, 'create table fruit')

        call row_alloc(buf, db%tables(1)%record_size)
        do i = 1, size(words)
            call row_set_int (buf, db%tables(1)%cols(1), int(i, int32))
            call row_set_char(buf, db%tables(1)%cols(2), trim(words(i)))
            call db_insert(db, 'fruit', buf, rid, rs)
            call check(rs == SQR_OK, 'insert '//trim(words(i)))
        end do

        ! Case-sensitive anchor: apple(1), apricot(2) — not Avocado (capital A)
        ! nor banana.
        call scan_match(db, 'fruit', 'word', '^a', n, ids, ok, rs)
        call check(ok .and. rs == SQR_OK, 'compile ^a')
        call check(n == 2 .and. ids(1) == 1 .and. ids(2) == 2, '^a -> apple,apricot')

        ! Inline case-insensitive flag also catches Avocado(5).
        call scan_match(db, 'fruit', 'word', '(?i)^a', n, ids, ok, rs)
        call check(n == 3 .and. ids(3) == 5, '(?i)^a -> +Avocado')

        ! Unanchored substring: only cherry(4) has a double r.
        call scan_match(db, 'fruit', 'word', 'rr', n, ids, ok, rs)
        call check(n == 1 .and. ids(1) == 4, 'rr -> cherry')

        ! End anchor on the stored value (no trailing-pad surprises): only
        ! banana(3) ends in 'a'.
        call scan_match(db, 'fruit', 'word', 'a$', n, ids, ok, rs)
        call check(n == 1 .and. ids(1) == 3, 'a$ -> banana')

        ! No matches.
        call scan_match(db, 'fruit', 'word', '^z', n, ids, ok, rs)
        call check(n == 0, '^z -> none')

        ! A malformed pattern is reported, not silently treated as no-match.
        call scan_match(db, 'fruit', 'word', '[unterminated', n, ids, ok, rs)
        call check(.not. ok, 'bad regex rejected')

        call db_close(db)
    end subroutine test_match
end program utest_match

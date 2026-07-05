! proctest — the two-process crash/recovery test (review test-gap 2). A real
! child process opens the database, arms a hot journal (explicit transaction +
! insert), then _exit()s abruptly — a genuine crashed writer, not the in-process
! c_lock_release stand-in. The parent waits for it, reopens the database, and
! asserts recovery rolled the uncommitted transaction back.
!
! Single binary, two roles: invoked with argv(1)=='child' it plays the crasher;
! otherwise it is the parent, which spawns itself in child mode. POSIX-only
! (fork/exec/waitpid via test/procshim.c); built and run by `make proctest`.
program proctest
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    use sqr
    use clib_wrap, only: c_rmtree
    implicit none

    interface
        function sqr_test_spawn(path, arg1) bind(c) result(pid)
            import :: c_char, c_int64_t
            character(kind=c_char), intent(in) :: path(*), arg1(*)
            integer(c_int64_t) :: pid
        end function
        function sqr_test_wait(pid) bind(c) result(code)
            import :: c_int, c_int64_t
            integer(c_int64_t), value :: pid
            integer(c_int) :: code
        end function
        subroutine sqr_test_hard_exit(code) bind(c)
            import :: c_int
            integer(c_int), value :: code
        end subroutine
    end interface

    character(len=*), parameter :: PDIR = 'proctest_db'
    integer :: nargs, pass, fail
    character(len=16) :: mode

    nargs = command_argument_count()
    if (nargs >= 1) then
        call get_command_argument(1, mode)
        if (trim(mode) == 'child') call run_child()   ! never returns
    end if

    pass = 0; fail = 0
    call run_parent(pass, fail)
    print '(a,i0,a,i0,a)', 'sqr proc tests: ', pass, ' passed, ', fail, ' failed'
    if (fail > 0) error stop 1

contains

    ! Columns for the fixture table.
    pure function people_cols() result(c)
        type(column_t) :: c(3)
        c(1)%name = 'pid' ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age' ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
    end function

    subroutine ck(cond, label, pass, fail)
        logical,          intent(in)    :: cond
        character(len=*), intent(in)    :: label
        integer,          intent(inout) :: pass, fail
        if (cond) then
            pass = pass + 1
            print '(a,a)', '  OK   ', label
        else
            fail = fail + 1
            print '(a,a)', '  FAIL ', label
        end if
    end subroutine

    ! Child role: open, begin an explicit transaction, insert one row (base
    ! write + durable hot journal), then crash before commit/close.
    subroutine run_child()
        type(db_t) :: db
        integer :: rs, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        call db_open(db, PDIR, rs)
        if (rs /= SQR_OK) call sqr_test_hard_exit(10_c_int)
        call db_begin(db, rs)
        if (rs /= SQR_OK) call sqr_test_hard_exit(11_c_int)
        ti = db_table_index(db, 'people')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 999_int32)
        call row_set_int (buf, db%tables(ti)%cols(2), 42_int32)
        call row_set_char(buf, db%tables(ti)%cols(3), 'Ghost')
        call db_insert(db, 'people', buf, rid, rs)
        if (rs /= SQR_OK) call sqr_test_hard_exit(12_c_int)
        call sqr_test_hard_exit(0_c_int)      ! crash with the journal hot
    end subroutine

    ! Parent role: build a one-row fixture, spawn the crasher, wait, reopen and
    ! assert the uncommitted insert was rolled back by recovery.
    subroutine run_parent(pass, fail)
        integer, intent(inout) :: pass, fail
        type(db_t) :: db
        integer :: rs, ti, code
        integer(int64) :: pid
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=256) :: emsg, exepath

        rs = c_rmtree(PDIR)
        call db_open(db, PDIR, rs, emsg)
        call db_create_table(db, 'people', people_cols(), rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int (buf, db%tables(1)%cols(1), 1_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 33_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Alice')
        call db_insert(db, 'people', buf, rid, rs)
        call db_close(db)                     ! commit the fixture; no journal left

        ! Spawn ourselves in child mode: it crashes with a hot journal.
        call get_command_argument(0, exepath)
        pid = sqr_test_spawn(trim(exepath)//c_null_char, 'child'//c_null_char)
        call ck(pid > 0, 'proc: child process spawned', pass, fail)
        code = int(sqr_test_wait(pid))
        call ck(code == 0, 'proc: child armed journal then crashed (exit 0)', pass, fail)

        ! A second, independent process now recovers the crashed writer.
        call db_open(db, PDIR, rs, emsg)
        call ck(rs == SQR_OK, 'proc: parent reopens and recovers', pass, fail)
        ti = db_table_index(db, 'people')
        call ck(db%tables(ti)%live_count == 1, &
                'proc: uncommitted insert rolled back (live_count 1)', pass, fail)
        call db_get(db, 'people', 2_int32, buf, rs)
        call ck(rs == SQR_NOT_FOUND, 'proc: ghost row absent after recovery', pass, fail)
        call db_get(db, 'people', 1_int32, buf, rs)
        call ck(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 1_int32, &
                'proc: committed row intact', pass, fail)
        call db_verify(db, 'people', rs, emsg)
        call ck(rs == SQR_OK, 'proc: db_verify clean after cross-process recovery', pass, fail)
        call db_close(db)
        rs = c_rmtree(PDIR)
    end subroutine

end program proctest

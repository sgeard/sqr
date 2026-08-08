! hammer — drive every public sqr entry point over a (possibly corrupted)
! database directory.  Contract: NOTHING here may crash, hang or abort; every
! call must come back with a status, however bad the on-disk bytes are.
!
!   usage: hammer <dir> [ro]
!
! Exit codes:  0 survived (any stat values are fine)
!              3 runaway loop guard tripped (cursor / scan never terminates)
! Anything else (SIGSEGV, forrtl severe, abort) is the harness *finding* a bug.
module hammer_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use :: sqr
    implicit none

    integer, parameter :: RUNAWAY = 200000   ! cursor/scan iteration ceiling
    integer, parameter :: ROWCAP  = 4000     ! row-id probe ceiling

    type :: cnt_t
        integer :: n = 0
    end type

contains

    subroutine count_cb(db, row_id, buf, ctx, stop)
        class(db_t),      intent(inout) :: db
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        integer :: dummy
        dummy = row_id + len(buf) + db%ntables
        select type (ctx)
        type is (cnt_t)
            ctx%n = ctx%n + 1
            stop = ctx%n > RUNAWAY
        class default
            stop = .true.
        end select
    end subroutine

    subroutine say(what, st)
        character(len=*), intent(in) :: what
        integer,          intent(in), optional :: st
        if (present(st)) then
            write(output_unit, '(a,i0)') 'STEP ' // what // ' stat=', st
        else
            write(output_unit, '(a)') 'STEP ' // what
        end if
        flush(output_unit)
    end subroutine

end module hammer_mod

program hammer
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use :: sqr
    use :: hammer_mod
    implicit none

    type(db_t)                    :: db, db2
    type(db_cursor_t)             :: cur
    type(cnt_t)                   :: counter
    class(*), pointer             :: ctxp
    character(len=512)            :: dir, mode
    character(len=256)            :: emsg
    character(len=SQR_NAME_LEN), allocatable :: tnames(:)
    character(len=:), allocatable :: buf, txt, packf, updir, tn
    integer                       :: st, i, j, k, ti, n, nrows
    integer(int32)                :: rid
    logical                       :: ro, ok, wo
    type(cnt_t), target           :: ctx_t

    if (command_argument_count() < 1) stop 'usage: hammer <dir> [ro]'
    call get_command_argument(1, dir)
    mode = ''
    if (command_argument_count() >= 2) call get_command_argument(2, mode)
    ro = trim(mode) == 'ro'
    wo = trim(mode) == 'wo'      ! write-only: skip the read probes, go straight to mutation

    emsg = ''
    call say('open')
    call db_open(db, trim(dir), stat=st, errmsg=emsg, readonly=ro)
    call say('open.done', st)
    if (st /= SQR_OK) then
        write(output_unit, '(a)') 'OPEN-REJECTED: ' // trim(emsg)
        write(output_unit, '(a)') 'SURVIVED'
        stop 0
    end if

    call say('list_tables')
    call db_list_tables(db, tnames)
    if (.not. allocated(tnames)) allocate(tnames(0))
    write(output_unit, '(a,i0)') 'ntables=', size(tnames)

    tables: do i = 1, merge(0, size(tnames), wo)
        tn = trim(tnames(i))
        call say('verify ' // tn)
        emsg = ''
        call db_verify(db, tn, st, emsg)
        call say('verify.done ' // tn, st)

        ti = db_table_index(db, tn)
        if (ti == 0) cycle tables
        n = db_record_size(db, tn)
        write(output_unit, '(a,i0)') 'recsize=', n
        if (n < 1 .or. n > 1024*1024) cycle tables
        call row_alloc(buf, n)

        ! --- per-row read of every typed accessor -----------------------
        nrows = min(db%tables(ti)%next_id - 1, ROWCAP)
        call say('rows ' // tn)
        rows: do j = 1, max(nrows, 0)
            call db_get(db, tn, int(j, int32), buf, st)
            if (st /= SQR_OK) cycle rows
            cols: do k = 1, db%tables(ti)%ncols
                associate (c => db%tables(ti)%cols(k))
                    if (row_is_null(buf, c)) cycle cols
                    select case (c%dtype)
                    case (DT_INT)
                        call use_i(row_get_int(buf, c))
                    case (DT_REAL)
                        call use_r(row_get_real(buf, c))
                    case (DT_CHAR)
                        call use_c(row_get_char(buf, c))
                    case (DT_TEXT)
                        call db_get_text(db, tn, int(j, int32), trim(c%name), txt, st)
                        if (allocated(txt)) call use_c(txt)
                    end select
                end associate
            end do cols
        end do rows
        call say('rows.done ' // tn)

        ! --- scan -------------------------------------------------------
        call say('scan ' // tn)
        ctx_t%n = 0
        ctxp => ctx_t
        call db_scan(db, tn, count_cb, ctxp, st)
        call say('scan.done ' // tn, st)
        if (ctx_t%n > RUNAWAY) then
            write(output_unit, '(a)') 'RUNAWAY scan'
            stop 3
        end if

        ! --- index probes ----------------------------------------------
        probe: do k = 1, db%tables(ti)%ncols
            associate (c => db%tables(ti)%cols(k))
                select case (c%dtype)
                case (DT_INT)
                    call say('find_int ' // trim(c%name))
                    do j = -3, 40
                        call db_find_by_int(db, tn, trim(c%name), int(j, int32), rid, st)
                    end do
                    call db_find_by_int(db, tn, trim(c%name), huge(0_int32), rid, st)
                    call db_find_by_int(db, tn, trim(c%name), -huge(0_int32), rid, st)
                    call say('find_int.done', st)

                    call say('cursor ' // trim(c%name))
                    call db_open_cursor(db, tn, trim(c%name), cur, st)
                    if (st == SQR_OK) then
                        if (.not. drain(db, cur, buf)) stop 3
                    end if
                    call say('cursor.done', st)

                    call say('range ' // trim(c%name))
                    call db_find_range(db, tn, trim(c%name), -5_int32, 500_int32, cur, st)
                    if (st == SQR_OK) then
                        if (.not. drain(db, cur, buf)) stop 3
                    end if
                    call db_find_range(db, tn, trim(c%name), huge(0_int32), -huge(0_int32), cur, st)
                    if (st == SQR_OK) then
                        if (.not. drain(db, cur, buf)) stop 3
                    end if
                    call say('range.done', st)
                case (DT_REAL)
                    call say('find_real ' // trim(c%name))
                    call db_find_by_real(db, tn, trim(c%name), 1.5_real64, rid, st)
                    call db_find_by_real(db, tn, trim(c%name), 0.0_real64, rid, st)
                    call say('find_real.done', st)
                case (DT_CHAR)
                    call say('find_char ' // trim(c%name))
                    call db_find_by_char(db, tn, trim(c%name), 'name_1', rid, st)
                    call db_find_by_char(db, tn, trim(c%name), '', rid, st)
                    call db_find_by_char(db, tn, trim(c%name), repeat('z', 300), rid, st)
                    call db_find_range(db, tn, trim(c%name), '', repeat('z', 60), cur, st)
                    if (st == SQR_OK) then
                        if (.not. drain(db, cur, buf)) stop 3
                    end if
                    call say('find_char.done', st)
                end select
            end associate
        end do probe

        ! --- composite key lookup --------------------------------------
        if (db%tables(ti)%ncols >= 2) then
            call say('get_by_key ' // tn)
            call row_clear(buf)
            do k = 1, min(2, db%tables(ti)%ncols)
                associate (c => db%tables(ti)%cols(k))
                    select case (c%dtype)
                    case (DT_INT);  call row_set_int (buf, c, 3_int32)
                    case (DT_REAL); call row_set_real(buf, c, 3.0_real64)
                    case (DT_CHAR); call row_set_char(buf, c, 'g3')
                    end select
                end associate
            end do
            call db_get_by_key(db, tn, [db%tables(ti)%cols(1)%name], buf, buf, st)
            call say('get_by_key.done ' // tn, st)
        end if
    end do tables

    ! --- write workout ------------------------------------------------------
    if (.not. ro .and. size(tnames) > 0) then
        tn = trim(tnames(1))
        ti = db_table_index(db, tn)
        n = db_record_size(db, tn)
        if (ti > 0 .and. n > 0 .and. n <= 1024*1024) then
            call row_alloc(buf, n)
            call row_clear(buf)
            do k = 1, db%tables(ti)%ncols
                associate (c => db%tables(ti)%cols(k))
                    select case (c%dtype)
                    case (DT_INT);  call row_set_int (buf, c, 99001_int32)
                    case (DT_REAL); call row_set_real(buf, c, 42.25_real64)
                    case (DT_CHAR); call row_set_char(buf, c, 'hammer')
                    end select
                end associate
            end do

            call say('begin')
            call db_begin(db, st, label='hammer')
            call say('begin.done', st)

            call say('insert')
            call db_insert(db, tn, buf, rid, st)
            call say('insert.done', st)

            if (st == SQR_OK) then
                call say('set_text')
                call db_set_text(db, tn, rid, 'note', 'hammered', st)
                call say('set_text.done', st)
                call say('update')
                call db_update(db, tn, rid, buf, st)
                call say('update.done', st)
            end if

            call say('commit')
            call db_commit(db, st)
            call say('commit.done', st)

            call say('begin2')
            call db_begin(db, st)
            if (st == SQR_OK) then
                call db_insert(db, tn, buf, rid, st)
                call db_rollback(db, st)
            end if
            call say('rollback.done', st)

            call say('undo')
            if (db_can_undo(db)) call db_undo(db, st)
            call say('undo.done', st)
            call say('redo')
            if (db_can_redo(db)) call db_redo(db, st)
            call say('redo.done', st)

            call say('delete')
            call db_delete(db, tn, 1_int32, st)
            call say('delete.done', st)

            call say('create_index')
            call db_create_index(db, tn, trim(db%tables(ti)%cols(db%tables(ti)%ncols)%name), st)
            call say('create_index.done', st)

            call say('compact')
            call db_compact(db, tn, st)
            call say('compact.done', st)

            call say('add_column')
            block
                type(column_t) :: nc
                nc%name = 'extra'; nc%dtype = DT_INT; nc%csize = 4
                call db_add_column(db, tn, nc, st, emsg)
            end block
            call say('add_column.done', st)
            call say('drop_column')
            call db_drop_column(db, tn, 'extra', st, emsg)
            call say('drop_column.done', st)
        end if
    end if

    call say('close')
    call db_close(db, st)
    call say('close.done', st)

    ! --- pack / unpack round trip -------------------------------------------
    packf = trim(dir) // '.pack'
    updir = trim(dir) // '.unpacked'
    call say('pack')
    call db_pack(trim(dir), packf, st)
    call say('pack.done', st)
    if (st == SQR_OK) then
        call say('unpack')
        call db_unpack(packf, updir, st)
        call say('unpack.done', st)
        if (st == SQR_OK) then
            call say('reopen')
            call db_open(db2, updir, stat=st, errmsg=emsg)
            call say('reopen.done', st)
            if (st == SQR_OK) then
                call db_list_tables(db2, tnames)
                call db_close(db2, st)
            end if
        end if
    end if

    write(output_unit, '(a)') 'SURVIVED'

contains

    ! Drain a cursor with a runaway guard.  .false. => the cursor never ends.
    logical function drain(d, c, b) result(fine)
        type(db_t),        intent(inout) :: d
        type(db_cursor_t), intent(inout) :: c
        character(len=*),  intent(inout) :: b
        integer(int32) :: r
        logical        :: got
        integer        :: s, it
        fine = .true.
        it = 0
        pull: do
            call db_cursor_next(d, c, r, b, got, s)
            if (.not. got) exit pull
            it = it + 1
            if (it > RUNAWAY) then
                write(output_unit, '(a)') 'RUNAWAY cursor'
                fine = .false.
                exit pull
            end if
        end do pull
    end function

    subroutine use_i(v)
        integer(int32), intent(in) :: v
        if (v == huge(0_int32)) write(output_unit, '(a)') ''
    end subroutine

    subroutine use_r(v)
        real(real64), intent(in) :: v
        if (v == huge(0.0_real64)) write(output_unit, '(a)') ''
    end subroutine

    subroutine use_c(v)
        character(len=*), intent(in) :: v
        if (len(v) == -1) write(output_unit, '(a)') ''
    end subroutine

end program hammer

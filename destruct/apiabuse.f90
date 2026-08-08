! apiabuse — hostile *arguments* (rather than hostile bytes on disk) into the
! public API.  One case per process: `apiabuse <dir> <case>`.
! Prints DONE if the case returned; anything else means it did not.
module abuse_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use :: sqr
    implicit none
contains
    subroutine drop_cb(db, row_id, buf, ctx, stop)
        class(db_t),      intent(inout) :: db
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        integer :: s
        stop = .false.
        select type (ctx)
        type is (integer)
            ctx = ctx + row_id + len(buf)
            if (ctx > 0) then
                call db_drop_table(db, 'events', s)   ! forbidden mid-scan
                stop = .true.
            end if
        end select
    end subroutine
end module abuse_mod

program apiabuse
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use :: sqr
    use :: abuse_mod
    implicit none

    type(db_t)                    :: db
    type(db_cursor_t)             :: cur
    type(column_t)                :: c1, cols1(1)
    type(column_t), allocatable   :: bigcols(:)
    character(len=512)            :: dir, cs
    character(len=256)            :: emsg
    character(len=:), allocatable :: buf, small, txt
    character(len=SQR_NAME_LEN), allocatable :: tn(:)
    class(*), pointer             :: ctxp
    integer, target               :: icnt
    integer                       :: st, kase, ti, n, i
    integer(int32)                :: rid, rids(3)
    logical                       :: ok

    call get_command_argument(1, dir)
    call get_command_argument(2, cs)
    read(cs, *) kase
    emsg = ''

    ! Cases 1-4 abuse the handle itself and must not pre-open.
    select case (kase)
    case (1)
        call db_close(db, st)                    ! close a never-opened handle
        write(output_unit,'(a,i0)') 'stat=', st
    case (2)
        call db_open(db, '', stat=st, errmsg=emsg)          ! empty path
        write(output_unit,'(a,i0)') 'stat=', st
    case (3)
        call db_open(db, repeat('x', 4000), stat=st, errmsg=emsg)  ! huge path
        write(output_unit,'(a,i0)') 'stat=', st
    case (4)
        call db_open(db, trim(dir) // '/people.dat', stat=st, errmsg=emsg)  ! a file, not a dir
        write(output_unit,'(a,i0)') 'stat=', st
    case (5)
        call db_open(db, trim(dir), stat=st)
        call db_close(db, st)
        call db_close(db, st)                    ! double close
        write(output_unit,'(a,i0)') 'stat=', st
    case (6)
        call db_open(db, trim(dir), stat=st)
        call db_close(db, st)
        call row_alloc(buf, 58)
        call db_insert(db, 'people', buf, rid, st)   ! use after close
        write(output_unit,'(a,i0)') 'stat=', st
    case (7)
        call db_open(db, trim(dir), stat=st)
        call db_open_cursor(db, 'people', 'id', cur, st)
        call db_close(db, st)
        call row_alloc(buf, 58)
        call db_cursor_next(db, cur, rid, buf, ok, st)   ! cursor after close
        write(output_unit,'(a,i0)') 'stat=', st
    case default
        call db_open(db, trim(dir), stat=st, errmsg=emsg)
        if (st /= SQR_OK) then
            write(output_unit,'(a)') 'open failed'
            stop 0
        end if
        ti = db_table_index(db, 'people')
        n  = db_record_size(db, 'people')
        call row_alloc(buf, n)
        allocate(character(len=4) :: small)
        small = repeat(char(0), 4)

        select case (kase)
        case (10)   ! row_set_char into a buffer far shorter than the column offset
            call row_set_char(small, db%tables(ti)%cols(3), 'boom')
        case (11)   ! row_get_char out of a too-short buffer
            txt = row_get_char(small, db%tables(ti)%cols(3))
            write(output_unit,'(a,i0)') 'len=', len(txt)
        case (12)   ! row_set_int with a column whose offset exceeds the buffer
            call row_set_int(small, db%tables(ti)%cols(1), 7_int32)
        case (13)   ! row_status on a zero-length buffer
            call row_alloc(txt, 0)
            write(output_unit,'(a,i0)') 'status=', row_status(txt)
        case (14)   ! row_is_null with a null_bit past the bitmap
            c1 = db%tables(ti)%cols(1)
            c1%null_bit = 100000
            write(output_unit,'(a,l1)') 'isnull=', row_is_null(buf, c1)
        case (15)   ! row_set_null likewise
            c1 = db%tables(ti)%cols(1)
            c1%null_bit = 100000
            call row_set_null(buf, c1)
        case (16)   ! row_alloc with a negative size, then use it
            call row_alloc(txt, -5)
            write(output_unit,'(a,i0)') 'len=', len(txt)
        case (17)   ! db_get into a buffer shorter than the record
            call db_get(db, 'people', 1_int32, small, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (18)   ! db_insert with a short buffer
            call db_insert(db, 'people', small, rid, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (19)   ! db_cursor_next into a short buffer
            call db_open_cursor(db, 'people', 'id', cur, st)
            call db_cursor_next(db, cur, rid, small, ok, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (20)   ! db_get_by_key with a short key row
            call db_get_by_key(db, 'people', ['id'], small, buf, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (21)   ! oversize table name
            c1%name = 'a'; c1%dtype = DT_INT; c1%csize = 4
            cols1(1) = c1
            call db_create_table(db, repeat('n', 300), cols1, st, emsg)
            write(output_unit,'(a,i0)') 'stat=', st
        case (22)   ! path-ish table name
            c1%name = 'a'; c1%dtype = DT_INT; c1%csize = 4
            cols1(1) = c1
            call db_create_table(db, '../escape', cols1, st, emsg)
            write(output_unit,'(a,i0)') 'stat=', st
        case (23)   ! a schema far beyond SQR_MAX_RECORD
            allocate(bigcols(30000))
            do i = 1, size(bigcols)
                write(bigcols(i)%name, '(a,i0)') 'c', i
                bigcols(i)%dtype = DT_CHAR
                bigcols(i)%csize = 65536
            end do
            call db_create_table(db, 'huge', bigcols, st, emsg)
            write(output_unit,'(a,i0)') 'stat=', st
        case (24)   ! zero-length index member list
            block
                character(len=SQR_NAME_LEN) :: none(0)
                call db_create_index(db, 'people', none, st)
            end block
            write(output_unit,'(a,i0)') 'stat=', st
        case (25)   ! index a column list longer than the table has columns
            block
                character(len=SQR_NAME_LEN) :: many(40)
                many = 'id'
                call db_create_index(db, 'people', many, st)
            end block
            write(output_unit,'(a,i0)') 'stat=', st
        case (26)   ! very large TEXT value
            ! Filled through a substring assignment, not `repeat(...)`: the
            ! repeat result is a stack temporary at this size, and blowing the
            ! harness's own stack would read as an engine defect.
            allocate(character(len=8*1024*1024) :: txt)
            txt(:) = 'Z'
            call db_set_text(db, 'people', 1_int32, 'note', txt, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (27)   ! key far longer than the indexed column
            call db_find_by_char(db, 'people', 'name', repeat('q', 100000), rid, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (28)   ! structural change inside a scan (documented as forbidden)
            icnt = 0
            ctxp => icnt
            call db_scan(db, 'people', drop_cb, ctxp, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (29)   ! insert_many with mismatched array sizes
            block
                character(len=n) :: bufs(5)
                bufs = repeat(char(0), n)
                call db_insert_many(db, 'people', bufs, rids, st)
            end block
            write(output_unit,'(a,i0)') 'stat=', st
        case (30)   ! undo/redo with no history
            call db_undo(db, st)
            call db_redo(db, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (31)   ! pack onto its own directory / unpack over a live db
            call db_pack(trim(dir), trim(dir), st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (32)   ! unpack over an existing populated directory
            call db_unpack(trim(dir) // '/people.dat', trim(dir), st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (33)   ! row_id extremes
            call db_get(db, 'people', 0_int32, buf, st)
            call db_get(db, 'people', -1_int32, buf, st)
            call db_get(db, 'people', huge(0_int32), buf, st)
            call db_delete(db, 'people', huge(0_int32), st)
            call db_update(db, 'people', -5_int32, buf, st)
            call db_get_text(db, 'people', huge(0_int32), 'note', txt, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (34)   ! names with embedded NUL / spaces
            call db_get(db, 'peo' // char(0) // 'ple', 1_int32, buf, st)
            call db_get(db, '  people  ', 1_int32, buf, st)
            call db_find_by_int(db, 'people', 'i' // char(0) // 'd', 1_int32, rid, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case (35)   ! reopen into an already-open handle (documented caller error)
            call db_open(db, trim(dir), stat=st, errmsg=emsg)
            write(output_unit,'(a,i0)') 'stat=', st
        case (36)   ! drop a column that an index depends on, then use the index
            call db_drop_column(db, 'people', 'id', st, emsg)
            call db_find_by_int(db, 'people', 'id', 1_int32, rid, st)
            call db_list_tables(db, tn)
            write(output_unit,'(a,i0)') 'stat=', st
        case (37)   ! nested transactions / commit without begin
            call db_commit(db, st)
            call db_rollback(db, st)
            call db_begin(db, st)
            call db_begin(db, st)
            call db_commit(db, st)
            call db_commit(db, st)
            write(output_unit,'(a,i0)') 'stat=', st
        case default
            write(output_unit,'(a)') 'no such case'
        end select
        call db_close(db, st)
    end select

    write(output_unit,'(a)') 'DONE'
end program apiabuse

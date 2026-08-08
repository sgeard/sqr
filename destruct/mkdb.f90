! mkdb — build a pristine "victim" database for destruction testing.
!   usage: mkdb <dir>
! Rich enough to exercise every on-disk artefact: INT/REAL/CHAR/TEXT columns,
! NULLs, tombstones, single + composite + unique indices (multi-level B+-trees),
! a blob file and a second table.
program mkdb
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use :: sqr
    implicit none

    type(db_t)                    :: db
    type(column_t)                :: cols(5), ecols(3)
    character(len=:), allocatable :: buf
    character(len=256)            :: dir
    integer                       :: st, ti, i, nrow
    integer(int32)                :: rid

    if (command_argument_count() < 1) stop 'usage: mkdb <dir>'
    call get_command_argument(1, dir)
    nrow = 600

    call db_open(db, trim(dir), stat=st)
    call must(st, 'open')

    cols(1)%name = 'id';    cols(1)%dtype = DT_INT;  cols(1)%csize = 4
    cols(2)%name = 'score'; cols(2)%dtype = DT_REAL; cols(2)%csize = 8
    cols(3)%name = 'name';  cols(3)%dtype = DT_CHAR; cols(3)%csize = 24
    cols(4)%name = 'tag';   cols(4)%dtype = DT_CHAR; cols(4)%csize = 8
    cols(5)%name = 'note';  cols(5)%dtype = DT_TEXT; cols(5)%csize = SQR_TEXT_DESC
    call db_create_table(db, 'people', cols, st)
    call must(st, 'create people')

    ecols(1)%name = 'eid';  ecols(1)%dtype = DT_INT;  ecols(1)%csize = 4
    ecols(2)%name = 'who';  ecols(2)%dtype = DT_CHAR; ecols(2)%csize = 16
    ecols(3)%name = 'val';  ecols(3)%dtype = DT_REAL; ecols(3)%csize = 8
    call db_create_table(db, 'events', ecols, st)
    call must(st, 'create events')

    ti = db_table_index(db, 'people')
    call row_alloc(buf, db%tables(ti)%record_size)

    call db_begin(db, st)
    call must(st, 'begin')
    fill: do i = 1, nrow
        call row_clear(buf)
        call row_set_int (buf, db%tables(ti)%cols(1), int(i, int32))
        if (mod(i, 7) == 0) then
            call row_set_null(buf, db%tables(ti)%cols(2))       ! NULL score
        else
            call row_set_real(buf, db%tables(ti)%cols(2), real(i, real64) * 1.5_real64)
        end if
        call row_set_char(buf, db%tables(ti)%cols(3), 'name_' // itoa(i))
        call row_set_char(buf, db%tables(ti)%cols(4), 'g' // itoa(mod(i, 13)))
        call db_insert(db, 'people', buf, rid, st)
        call must(st, 'insert people')
    end do fill
    call db_commit(db, st)
    call must(st, 'commit')

    ! TEXT values on a sample of rows -> a populated blob file
    blobs: do i = 1, nrow, 17
        call db_set_text(db, 'people', int(i, int32), 'note', &
                         'note-' // itoa(i) // '-' // repeat('x', mod(i, 40)), st)
        call must(st, 'set_text')
    end do blobs

    ! Tombstones
    kills: do i = 5, nrow, 53
        call db_delete(db, 'people', int(i, int32), st)
        call must(st, 'delete')
    end do kills

    call db_create_index(db, 'people', 'id', st, unique=.true.)
    call must(st, 'index id')
    call db_create_index(db, 'people', 'name', st)
    call must(st, 'index name')
    call db_create_index(db, 'people', ['tag', 'id '], st)
    call must(st, 'index tag,id')

    ti = db_table_index(db, 'events')
    call row_alloc(buf, db%tables(ti)%record_size)
    ev: do i = 1, 120
        call row_clear(buf)
        call row_set_int (buf, db%tables(ti)%cols(1), int(i, int32))
        call row_set_char(buf, db%tables(ti)%cols(2), 'ev' // itoa(mod(i, 31)))
        call row_set_real(buf, db%tables(ti)%cols(3), real(i, real64))
        call db_insert(db, 'events', buf, rid, st)
        call must(st, 'insert events')
    end do ev
    call db_create_index(db, 'events', 'eid', st, unique=.true.)
    call must(st, 'index eid')

    call db_close(db, st)
    call must(st, 'close')
    print '(a)', 'mkdb: ok'

contains

    subroutine must(s, what)
        integer,          intent(in) :: s
        character(len=*), intent(in) :: what
        if (s /= SQR_OK) then
            print '(a,i0)', 'mkdb FAILED at ' // what // ' stat=', s
            stop 1
        end if
    end subroutine

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: t
        write(t, '(i0)') n
        s = trim(t)
    end function

end program mkdb

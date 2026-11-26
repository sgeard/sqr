! utest_fault — fault-injection sweep for sqr's low-level I/O failure
! branches. Built only by `make faulttest` / `make coverage` with
! FAULT=on (the sqr_fault_on submodule). For every high-level operation
! it counts the io_check events of a clean run, then sweeps the armed
! ordinal n = 1..count: each injected I/O must surface as a non-OK
! status (never a false success) and must not crash. This is what makes
! the otherwise-unreachable `if (ios /= 0)` clusters reachable.
!
! Only the operation itself is measured/injected: the fixture db_open and
! the trailing db_close are run disarmed (db_close in particular has no
! error channel, so injecting there could not surface anyway).
program utest_fault
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use sqr
    use sqr_fault, only: fault_arm, fault_disarm, fault_count
    use clib_wrap, only: c_rmtree, c_path_exists, c_remove
    implicit none

    integer :: pass = 0, fail = 0
    integer, parameter :: BIG = 100000000
    character(len=*), parameter :: DBDIR = 'utest_fault_db'
    character(len=*), parameter :: MGDIR = 'utest_fault_mig_db'

    ! Handle the measured operation acts on. Opened by the sweep (for
    ! act-ops) or by the op itself (for db_open under test); always
    ! closed disarmed after the op.
    type(db_t) :: gdb
    logical    :: gopen = .false.
    integer    :: fds0  = -1      ! open descriptors at program start (-1 = no /proc)

    abstract interface
        subroutine prep_i()
        end subroutine prep_i
        subroutine op_i(rs)
            integer, intent(out) :: rs
        end subroutine op_i
    end interface

    fds0 = open_fds()                 ! the baseline no_stranded_units compares to
    call sweep('open',         prep_fulldb,      op_open,    .true.)
    call sweep('insert',       prep_table,       op_insert,  .false.)
    call sweep('update',       prep_rows,        op_update,  .false.)
    call sweep('delete',       prep_rows,        op_delete,  .false.)
    call sweep('get',          prep_rows,        op_get,     .false.)
    call sweep('create_index', prep_rows,        op_index,   .false.)
    call sweep('find_by_int',  prep_indexed,     op_find,    .false.)
    call sweep('compact',      prep_compactable, op_compact, .false.)
    call sweep('reshape',      prep_indexed,     op_reshape, .false.)
    call reshape_atomicity()
    call sweep('add_text',     prep_plain,       op_addtext, .false.)
    call addtext_atomicity()
    call unpack_faults()
    call pack_faults()
    call sweep('set_text',     prep_textrow,     op_settext, .false.)
    call sweep('get_text',     prep_textset,     op_gettext, .false., expect=SQR_ERR)
    ! Every sweep closes its handle after each faulted run: an error path that
    ! strands a connected unit (presize once did, on the journal) goes unseen
    ! on a compiler that allows a file on two units, and breaks every later
    ! open of that file on one that refuses (flang).  Check on all of them.
    call check(no_stranded_units(), 'sweeps: no unit left connected after faulted runs')

    call compact_atomicity()
    call compact_index_recovery()
    call create_index_retry()
    call upgrade_recovery()
    call close_failure()
    call close_clean()

    ! Auto-commit atomicity: every row mutator brackets itself, so a mid-op
    ! fault must leave the indexed table in its exact pre-op state (live_count 3
    ! restored, db_verify clean).
    call mutator_atomicity('insert',  prep_indexed, op_insert,  3)
    call mutator_atomicity('update',  prep_indexed, op_update,  3)
    call mutator_atomicity('delete',  prep_indexed, op_delete,  3)
    call mutator_atomicity('settext', prep_indexed, op_settext, 3)
    ! Batched insert is also bracketed; its deferred packed reindex captures the
    ! whole index file up-front, so a fault during any row write or the rebuild
    ! rolls the entire batch back too.
    call mutator_atomicity('insertmany', prep_indexed, op_insert_many, 3)

    ! Pinpoint the Phase-2 commit durability barrier: db_commit fsyncs every
    ! modified base file and only then voids the journal (the commit point).
    call commit_durability()

    ! Q1: a fault swept across a LABELLED commit must never corrupt the history.
    call capture_fault()

    ! C4: an undo or redo is one journalled unit — a fault anywhere inside it
    ! leaves the gesture wholly in place or wholly gone, never half applied.
    call history_atomicity(redo=.false.)
    call history_atomicity(redo=.true.)

    ! D4: a mutator that fails part-way inside an explicit transaction.
    call doomed_by_fault()

    ! E2: a demotion that cannot save the counters stays writable.
    call readonly_by_fault()

    ! H1: a faulted undo replay must leave the journal hot, never voided.
    call recover_fault()

    ! C8: a first arm that fails after its hot header went out must still be
    ! voided by the rollback.
    call arm_fault_void()

    ! C7: a rollback that fails with the journal hot latches the handle
    ! against every write and keeps the journal for the next open.
    call rollback_latch()

    ! C9: a labelled commit that fails leaves both history stacks alone.
    call commit_keeps_redo()

    ! C11: a failed read of a partly covered record is an error, not zeros.
    call partial_record_fault()
    call check(no_stranded_units(), 'all: no unit left connected after faulted runs')

    call cleanup()
    print '(a,i0,a,i0,a)', 'sqr fault tests: ', pass, ' passed, ', fail, ' failed'
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

    ! Open file descriptors of this process (Linux /proc/self/fd), or -1 where
    ! there is no /proc.  A unit stranded by an error path keeps its
    ! descriptor, so the count after faulted runs exceeds the count before.
    ! Counted by descriptor because no Fortran INQUIRE finds such a unit
    ! portably: by FILE, gfortran matches by identity and the stranded file
    ! has since been deleted and recreated; by UNIT number, the runtimes'
    ! internal negative units make gfortran crash.
    integer function open_fds() result(n)
        character(len=32) :: p
        integer :: fd
        n = -1
        if (.not. c_path_exists('/proc/self/fd')) return
        n = 0
        do fd = 0, 4095
            write(p, '(a,i0)') '/proc/self/fd/', fd
            if (c_path_exists(trim(p))) n = n + 1
        end do
    end function open_fds

    ! No descriptor more than at program start (always true without /proc).
    logical function no_stranded_units() result(none)
        integer :: n
        n = open_fds()
        none = (fds0 < 0 .or. n <= fds0)
        if (.not. none) print '(a,i0,a,i0)', '  open descriptors: ', n, ', at start: ', fds0
    end function no_stranded_units

    subroutine cleanup()
        integer :: ios
        ios = c_rmtree(DBDIR)
        ios = c_rmtree(MGDIR)
    end subroutine cleanup

    pure function people_cols() result(c)
        type(column_t) :: c(4)
        c(1)%name = 'pid' ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age' ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR ; c(3)%csize = 16
        c(4)%name = 'bio' ; c(4)%dtype = DT_TEXT ; c(4)%csize = SQR_TEXT_DESC
    end function people_cols

    subroutine close_g()
        if (gopen) then
            call db_close(gdb)
            gopen = .false.
        end if
    end subroutine close_g

    ! Generic sweep. `op_opens` selects whether the operation under test
    ! is db_open itself (true: op opens gdb) or an act on an already-open
    ! gdb (false: the sweep opens gdb, unarmed, before measuring).
    subroutine sweep(label, prep, op, op_opens, expect)
        character(len=*),  intent(in) :: label
        procedure(prep_i)             :: prep
        procedure(op_i)               :: op
        logical,           intent(in) :: op_opens
        integer, optional, intent(in) :: expect  ! exact code every fault must give
        integer :: nops, n, rs, bad, miscoded

        call fault_disarm()
        call prep()
        if (.not. op_opens) then
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call check(rs == SQR_OK, label // ': fixture open ok')
        end if
        call fault_arm(BIG)              ! never triggers; just counts
        call op(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()
        call check(rs == SQR_OK,  label // ': clean run succeeds')
        call check(nops > 0,      label // ': I/O occurred (FAULT=on linked)')

        bad = 0; miscoded = 0
        sweep_loop: do n = 1, nops
            call prep()
            if (.not. op_opens) then
                call db_open(gdb, DBDIR, rs)
                gopen = rs == SQR_OK
            end if
            call fault_arm(n)
            call op(rs)
            call fault_disarm()
            call close_g()
            if (rs == SQR_OK) bad = bad + 1
            ! When the op has a single I/O failure class, every injected fault
            ! must report exactly that code — not, e.g., SQR_NOT_FOUND for a
            ! failed read (which would hide storage faults as a missing row).
            if (present(expect)) then
                if (rs /= SQR_OK .and. rs /= expect) miscoded = miscoded + 1
            end if
        end do sweep_loop
        call check(bad == 0, &
            label // ': every injected I/O surfaced as a non-OK status')
        if (present(expect)) &
            call check(miscoded == 0, &
                label // ': every injected I/O surfaced as the I/O-error code')
    end subroutine sweep

    ! ---- preconditions: build on-disk state via a local handle -------

    subroutine prep_table()
        type(db_t) :: db
        integer :: rs, ios
        ios = c_rmtree(DBDIR)
        call db_open(db, DBDIR, rs)
        call db_create_table(db, 'people', people_cols(), rs)
        call db_close(db)
    end subroutine prep_table

    subroutine insert_row(db, pid, age, nm)
        type(db_t),       intent(inout) :: db
        integer(int32),   intent(in)    :: pid, age
        character(len=*), intent(in)    :: nm
        character(len=:), allocatable :: buf
        integer(int32) :: rid
        integer :: rs, ti
        ti = db_table_index(db, 'people')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), pid)
        call row_set_int (buf, db%tables(ti)%cols(2), age)
        call row_set_char(buf, db%tables(ti)%cols(3), nm)
        call db_insert(db, 'people', buf, rid, rs)
    end subroutine insert_row

    subroutine prep_rows()
        type(db_t) :: db
        integer :: rs
        call prep_table()
        call db_open(db, DBDIR, rs)
        call insert_row(db, 1_int32, 33_int32, 'Alice')
        call insert_row(db, 2_int32, 45_int32, 'Bob')
        call insert_row(db, 3_int32, 21_int32, 'Carol')
        call db_close(db)
    end subroutine prep_rows

    subroutine prep_indexed()
        type(db_t) :: db
        integer :: rs
        call prep_rows()
        call db_open(db, DBDIR, rs)
        call db_create_index(db, 'people', 'age', rs)
        call db_close(db)
    end subroutine prep_indexed

    subroutine prep_compactable()
        type(db_t) :: db
        integer :: rs
        call prep_rows()
        call db_open(db, DBDIR, rs)
        call db_delete(db, 'people', 2_int32, rs)
        call db_close(db)
    end subroutine prep_compactable

    ! As prep_compactable, but with a live index on 'age' — so a compact must
    ! rebuild the index over the renumbered rows (the M4 path).
    subroutine prep_indexed_compactable()
        type(db_t) :: db
        integer :: rs
        call prep_rows()
        call db_open(db, DBDIR, rs)
        call db_create_index(db, 'people', 'age', rs)
        call db_delete(db, 'people', 2_int32, rs)
        call db_close(db)
    end subroutine prep_indexed_compactable

    subroutine prep_textrow()
        type(db_t) :: db
        integer :: rs
        call prep_table()
        call db_open(db, DBDIR, rs)
        call insert_row(db, 1_int32, 33_int32, 'Alice')
        call db_close(db)
    end subroutine prep_textrow

    subroutine prep_textset()
        type(db_t) :: db
        integer :: rs
        call prep_textrow()
        call db_open(db, DBDIR, rs)
        call db_set_text(db, 'people', 1_int32, 'bio', 'hello fault world', rs)
        call db_close(db)
    end subroutine prep_textset

    subroutine prep_fulldb()
        type(db_t) :: db
        integer :: rs
        call prep_indexed()
        call db_open(db, DBDIR, rs)
        call db_set_text(db, 'people', 1_int32, 'bio', 'hello fault world', rs)
        call db_close(db)
    end subroutine prep_fulldb

    ! ---- operations under test ---------------------------------------

    subroutine op_open(rs)
        integer, intent(out) :: rs
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
    end subroutine op_open

    subroutine op_insert(rs)
        integer, intent(out) :: rs
        character(len=:), allocatable :: buf
        integer(int32) :: rid
        integer :: ti
        ti = db_table_index(gdb, 'people')
        call row_alloc(buf, gdb%tables(ti)%record_size)
        call row_set_int (buf, gdb%tables(ti)%cols(1), 9_int32)
        call row_set_int (buf, gdb%tables(ti)%cols(2), 99_int32)
        call row_set_char(buf, gdb%tables(ti)%cols(3), 'Zoe')
        call db_insert(gdb, 'people', buf, rid, rs)
    end subroutine op_insert

    subroutine op_insert_many(rs)
        integer, intent(out) :: rs
        character(len=:), allocatable :: bufs(:)
        integer(int32) :: rids(3)
        integer :: ti, k, rsz
        ti = db_table_index(gdb, 'people')
        rsz = gdb%tables(ti)%record_size
        allocate(character(len=rsz) :: bufs(3))
        do k = 1, 3
            bufs(k) = repeat(char(0), rsz)
            call row_set_int (bufs(k), gdb%tables(ti)%cols(1), int(40 + k, int32))
            call row_set_int (bufs(k), gdb%tables(ti)%cols(2), int(70 + k, int32))
            call row_set_char(bufs(k), gdb%tables(ti)%cols(3), 'Bat')
        end do
        call db_insert_many(gdb, 'people', bufs, rids, rs)
    end subroutine op_insert_many

    subroutine op_update(rs)
        integer, intent(out) :: rs
        character(len=:), allocatable :: buf
        integer :: ti
        ti = db_table_index(gdb, 'people')
        call row_alloc(buf, gdb%tables(ti)%record_size)
        call row_set_int (buf, gdb%tables(ti)%cols(1), 2_int32)
        call row_set_int (buf, gdb%tables(ti)%cols(2), 46_int32)
        call row_set_char(buf, gdb%tables(ti)%cols(3), 'Bobby')
        call db_update(gdb, 'people', 2_int32, buf, rs)
    end subroutine op_update

    subroutine op_delete(rs)
        integer, intent(out) :: rs
        call db_delete(gdb, 'people', 2_int32, rs)
    end subroutine op_delete

    subroutine op_get(rs)
        integer, intent(out) :: rs
        character(len=:), allocatable :: buf
        integer :: ti
        ti = db_table_index(gdb, 'people')
        call row_alloc(buf, gdb%tables(ti)%record_size)
        call db_get(gdb, 'people', 2_int32, buf, rs)
    end subroutine op_get

    subroutine op_index(rs)
        integer, intent(out) :: rs
        call db_create_index(gdb, 'people', 'age', rs)
    end subroutine op_index

    subroutine op_find(rs)
        integer, intent(out) :: rs
        integer(int32) :: rid
        call db_find_by_int(gdb, 'people', 'age', 45_int32, rid, rs)
    end subroutine op_find

    ! Reshape `people` (pid, age, name, bio; age indexed): drop the indexed
    ! `age`, move `name` first, add `score` - every kind of change in one rewrite.
    subroutine op_reshape(rs)
        integer, intent(out) :: rs
        type(column_t) :: c(4)
        c(1)%name = 'name' ; c(1)%dtype = DT_CHAR ; c(1)%csize = 16
        c(2)%name = 'pid'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'bio'  ; c(3)%dtype = DT_TEXT ; c(3)%csize = SQR_TEXT_DESC
        c(4)%name = 'score'; c(4)%dtype = DT_INT  ; c(4)%csize = 4
        call db_reshape_table(gdb, 'people', c, rs)
    end subroutine op_reshape

    ! A reshape that fails at any I/O point leaves a database that reopens as
    ! EITHER the whole old table (4 columns, age second, Bob aged 45, the age
    ! index answering) OR the whole new one (name first, no age, Bob's name
    ! kept, score NULL) - never a mixture - and db_verify passes either way.
    ! (A failure past the commit point legitimately leaves the new layout.)
    subroutine reshape_atomicity()
        type(db_t) :: db
        integer :: nops, n, rs, bad, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: em
        logical :: old_ok, new_ok
        call fault_disarm()
        call prep_indexed()
        call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
        call fault_arm(BIG)
        call op_reshape(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()
        bad = 0
        sweep_loop: do n = 1, nops
            call prep_indexed()
            call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
            call fault_arm(n)
            call op_reshape(rs)
            call fault_disarm()
            call close_g()
            if (rs == SQR_OK) cycle sweep_loop
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                print '(a,i0,a,i0)', '  reshape fault ', n, ': reopen failed ', rs
                bad = bad + 1
                cycle sweep_loop
            end if
            ti = db_table_index(db, 'people')
            call row_alloc(buf, db%tables(ti)%record_size)
            call db_get(db, 'people', 2_int32, buf, rs)
            old_ok = .false.;  new_ok = .false.
            if (rs == SQR_OK .and. db%tables(ti)%ncols == 4) then
                if (trim(db%tables(ti)%cols(2)%name) == 'age') then
                    old_ok = row_get_int(buf, db%tables(ti)%cols(2)) == 45_int32
                    call db_find_by_int(db, 'people', 'age', 21_int32, rid, rs)
                    old_ok = old_ok .and. rs == SQR_OK .and. rid == 3_int32
                else if (trim(db%tables(ti)%cols(1)%name) == 'name') then
                    new_ok = trim(row_get_char(buf, db%tables(ti)%cols(1))) == 'Bob' .and. &
                             row_is_null(buf, db%tables(ti)%cols(4))
                end if
            end if
            em = ''
            call db_verify(db, 'people', rs, em)
            if (.not. (old_ok .or. new_ok) .or. rs /= SQR_OK) then
                print '(a,i0,a,l1,l1,a,i0,1x,a)', '  reshape fault ', n, ': old/new ', old_ok, new_ok, &
                    ' verify ', rs, trim(em)
                bad = bad + 1
            end if
            call db_close(db)
        end do sweep_loop
        call check(bad == 0, 'reshape: every failed reshape reopens wholly old or wholly new, verified')
    end subroutine reshape_atomicity

    ! `people` without its TEXT column: (pid, age, name), two rows, no blob file.
    subroutine prep_plain()
        type(db_t) :: db
        type(column_t) :: c(4)
        integer :: rs, ios
        ios = c_rmtree(DBDIR)
        c = people_cols()
        call db_open(db, DBDIR, rs)
        call db_create_table(db, 'people', c(1:3), rs)
        call insert_row(db, 1_int32, 33_int32, 'Alice')
        call insert_row(db, 2_int32, 45_int32, 'Bob')
        call db_close(db)
    end subroutine prep_plain

    ! Give `people` its first TEXT column.
    subroutine op_addtext(rs)
        integer, intent(out) :: rs
        type(column_t) :: c(4)
        c = people_cols()
        call db_add_column(gdb, 'people', c(4), rs)
    end subroutine op_addtext

    ! Adding the first TEXT column creates the blob file.  A failure at any
    ! I/O point leaves a database that reopens as EITHER the old table (three
    ! columns) OR the new one (four, and the TEXT column usable: its blob file
    ! is there) - never a schema naming a TEXT column with no blob file.
    subroutine addtext_atomicity()
        type(db_t) :: db
        integer :: nops, n, rs, bad, ti
        character(len=:), allocatable :: txt
        character(len=128) :: em
        logical :: old_ok, new_ok
        call fault_disarm()
        call prep_plain()
        call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
        call fault_arm(BIG)
        call op_addtext(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()
        call check(rs == SQR_OK .and. nops > 0, 'add_text: the unfaulted change succeeds')
        bad = 0
        sweep_loop: do n = 1, nops
            call prep_plain()
            call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
            call fault_arm(n)
            call op_addtext(rs)
            call fault_disarm()
            call close_g()
            if (rs == SQR_OK) cycle sweep_loop
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                print '(a,i0,a,i0)', '  add_text fault ', n, ': reopen failed ', rs
                bad = bad + 1
                cycle sweep_loop
            end if
            ti = db_table_index(db, 'people')
            old_ok = db%tables(ti)%ncols == 3
            new_ok = .false.
            if (db%tables(ti)%ncols == 4) then
                call db_set_text(db, 'people', 2_int32, 'bio', 'after the fault', rs)
                if (rs == SQR_OK) then
                    call db_get_text(db, 'people', 2_int32, 'bio', txt, rs)
                    if (rs == SQR_OK) new_ok = txt == 'after the fault'
                end if
            end if
            em = ''
            call db_verify(db, 'people', rs, em)
            if (.not. (old_ok .or. new_ok) .or. rs /= SQR_OK) then
                print '(a,i0,a,l1,l1,a,i0,1x,a)', '  add_text fault ', n, ': old/new ', old_ok, new_ok, &
                    ' verify ', rs, trim(em)
                bad = bad + 1
            end if
            call db_close(db)
        end do sweep_loop
        call check(bad == 0, 'add_text: every failed change reopens old, or new with a usable TEXT column')
    end subroutine addtext_atomicity

    ! db_unpack makes every extracted file, and the directory holding them,
    ! durable before the rename, and the rename after it.  A failure at any of
    ! those points is reported and leaves NOTHING: neither the database
    ! directory nor the temporary one beside it.  (Durability itself needs a
    ! power cut to observe; this covers the failure paths.)
    subroutine unpack_faults()
        character(len=*), parameter :: PK = 'utest_fault_unpack.sqr', OUT = 'utest_fault_unpack_db'
        type(db_t) :: db
        integer :: nops, n, rs, bad, left, ios
        character(len=128) :: em
        call fault_disarm()
        call prep_indexed()
        ios = c_remove(PK)
        ios = c_rmtree(OUT)
        call db_pack(DBDIR, PK, rs)
        call check(rs == SQR_OK, 'unpack: fixture packs')
        call fault_arm(BIG)
        call db_unpack(PK, OUT, rs)
        nops = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK .and. nops > 0, 'unpack: clean run succeeds, with I/O counted')
        call db_open(db, OUT, rs)
        em = ''
        if (rs == SQR_OK) call db_verify(db, 'people', rs, em)
        call check(rs == SQR_OK, 'unpack: the unpacked database opens and verifies')
        call db_close(db)
        call check(.not. c_path_exists(OUT // '.unpack-tmp'), 'unpack: no temporary directory left')
        bad = 0; left = 0
        sweep_loop: do n = 1, nops
            ios = c_rmtree(OUT)
            call fault_arm(n)
            call db_unpack(PK, OUT, rs)
            call fault_disarm()
            if (rs == SQR_OK) bad = bad + 1
            if (c_path_exists(OUT) .or. c_path_exists(OUT // '.unpack-tmp')) then
                print '(a,i0,a)', '  unpack fault ', n, ': a directory was left behind'
                left = left + 1
                ios = c_rmtree(OUT // '.unpack-tmp')
            end if
        end do sweep_loop
        call check(bad == 0,  'unpack: every injected I/O surfaced as a non-OK status')
        call check(left == 0, 'unpack: a failed unpack leaves neither directory')
        call db_unpack(PK, OUT, rs)
        call check(rs == SQR_OK, 'unpack: and the retry succeeds')
        ios = c_rmtree(OUT)
        ios = c_remove(PK)
    end subroutine unpack_faults

    ! A6: a failed pack never leaves its temporary file, and the target stays
    ! a container that unpacks to a database that verifies (old or new).
    subroutine pack_faults()
        character(len=*), parameter :: PK = 'utest_fault_pack.sqr', OUT = 'utest_fault_pack_db'
        type(db_t) :: db
        integer :: nops, n, rs, bad, left, broken, ios
        character(len=128) :: em
        call fault_disarm()
        call prep_indexed()
        ios = c_remove(PK)
        ios = c_remove(PK // '.tmp')
        ios = c_rmtree(OUT)
        call db_pack(DBDIR, PK, rs)
        call check(rs == SQR_OK, 'pack: a good container is in place')
        call fault_arm(BIG)
        call db_pack(DBDIR, PK, rs)
        nops = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK .and. nops > 0, 'pack: clean run succeeds, with I/O counted')
        bad = 0; left = 0; broken = 0
        sweep_loop: do n = 1, nops
            call fault_arm(n)
            call db_pack(DBDIR, PK, rs)
            call fault_disarm()
            if (rs == SQR_OK) bad = bad + 1
            if (c_path_exists(PK // '.tmp')) then
                print '(a,i0,a)', '  pack fault ', n, ': the temporary file was left behind'
                left = left + 1
                ios = c_remove(PK // '.tmp')
            end if
            ios = c_rmtree(OUT)
            call db_unpack(PK, OUT, rs)
            em = ''
            if (rs == SQR_OK) then
                call db_open(db, OUT, rs)
                if (rs == SQR_OK) call db_verify(db, 'people', rs, em)
                call db_close(db)
            end if
            if (rs /= SQR_OK) then
                print '(a,i0,a)', '  pack fault ', n, ': the target no longer unpacks to a sound database'
                broken = broken + 1
            end if
        end do sweep_loop
        call check(bad == 0,    'pack: every injected I/O surfaced as a non-OK status')
        call check(left == 0,   'pack: a failed pack leaves no temporary file')
        call check(broken == 0, 'pack: the target always unpacks to a database that verifies')
        ios = c_rmtree(OUT)
        ios = c_remove(PK)
    end subroutine pack_faults

    subroutine op_compact(rs)
        integer, intent(out) :: rs
        call db_compact(gdb, 'people', rs)
    end subroutine op_compact

    subroutine op_settext(rs)
        integer, intent(out) :: rs
        call db_set_text(gdb, 'people', 1_int32, 'bio', 'injected text value', rs)
    end subroutine op_settext

    subroutine op_gettext(rs)
        integer, intent(out) :: rs
        character(len=:), allocatable :: txt
        call db_get_text(gdb, 'people', 1_int32, 'bio', txt, rs)
    end subroutine op_gettext

    ! db_compact is build-then-swap: a failure mid-compact must leave the
    ! original data fully intact. Sweep every injection point and, after
    ! each failed compaction, reopen and assert the live row is still
    ! readable and correct.
    subroutine compact_atomicity()
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, n, nops, ti, bad
        integer(int32) :: age

        call fault_disarm()
        call prep_compactable()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call fault_arm(BIG)
        call op_compact(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()

        bad = 0
        atom_loop: do n = 1, nops
            call prep_compactable()            ! rows 1,3 live; row 2 deleted
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call fault_arm(n)
            call op_compact(rs)
            call fault_disarm()
            call close_g()
            if (rs == SQR_OK) cycle atom_loop  ! compaction completed: fine
            ! Failed compaction: the originals must be untouched.
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                bad = bad + 1
                cycle atom_loop
            end if
            ti = db_table_index(db, 'people')
            call row_alloc(buf, db%tables(ti)%record_size)
            call db_get(db, 'people', 1_int32, buf, rs)
            if (rs /= SQR_OK) then
                bad = bad + 1
            else
                age = row_get_int(buf, db%tables(ti)%cols(2))
                if (age /= 33_int32) bad = bad + 1
            end if
            call db_close(db)
        end do atom_loop
        call check(bad == 0, &
            'compact: original data intact after every mid-compact failure')
    end subroutine compact_atomicity

    ! M4: a compact that fails mid-reindex must never leave a truncated/empty
    ! index that later queries trust. Sweep every injection point of an indexed
    ! compact; after each failure reopen (unarmed) so the ".compacting" marker
    ! recovery finishes the compact and rebuilds the index, then db_verify —
    ! which walks the index against a full scan — must pass. An empty index over
    ! populated rows would fail it.
    subroutine compact_index_recovery()
        type(db_t) :: db
        integer :: rs, n, nops, bad
        character(len=128) :: emsg

        call fault_disarm()
        call prep_indexed_compactable()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call fault_arm(BIG)
        call op_compact(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()

        bad = 0
        atom_loop: do n = 1, nops
            call prep_indexed_compactable()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call fault_arm(n)
            call op_compact(rs)
            call fault_disarm()
            call close_g()
            ! Reopen unarmed: recovery (if a marker was left) rebuilds the index.
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                bad = bad + 1
                cycle atom_loop
            end if
            call db_verify(db, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            call db_close(db)
        end do atom_loop
        call check(bad == 0, &
            'compact: index consistent (db_verify) after every mid-compact failure')
    end subroutine compact_index_recovery

    ! Upgrade of a sqr 1.x database (schema version 1) by a read-write
    ! db_open: rebuild each table's indices, write its schema, then the
    ! catalog.  A fault at any I/O on the way must fail the open, and the next
    ! open, unarmed, must finish the job: every table verifies and the query
    ! that 1.x got wrong (a = 5 on the composite (a,b) index of t, with a NULL
    ! b) finds both rows.
    subroutine upgrade_recovery()
        character(len=*), parameter :: FIX = 'test/fixtures/v1_null.sqr'
        type(db_t) :: db
        type(db_cursor_t) :: cur
        integer :: rs, n, nops, bad, unhealed, ios, nrows
        integer(int32) :: rid
        logical :: ok, more
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        call fault_disarm()
        if (.not. c_path_exists(FIX)) then
            print '(a)', '  SKIP upgrade: sqr 1.x fixture not found (run from the project root)'
            return
        end if
        ios = c_rmtree(DBDIR)
        call db_unpack(FIX, DBDIR, rs)
        call check(rs == SQR_OK, 'upgrade: fixture unpacks')
        call fault_arm(BIG)
        call db_open(db, DBDIR, rs)
        nops = fault_count()
        call fault_disarm()
        if (rs == SQR_OK) call db_close(db)
        call check(rs == SQR_OK, 'upgrade: clean run succeeds')
        call check(nops > 0,     'upgrade: I/O occurred (FAULT=on linked)')

        bad = 0; unhealed = 0
        fault_loop: do n = 1, nops
            ios = c_rmtree(DBDIR)
            call db_unpack(FIX, DBDIR, rs)
            call fault_arm(n)
            call db_open(db, DBDIR, rs)
            call fault_disarm()
            if (rs == SQR_OK) then
                bad = bad + 1
                call db_close(db)
            end if
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                unhealed = unhealed + 1
                cycle fault_loop
            end if
            ok = .true.
            call db_verify(db, 't', rs, emsg); ok = ok .and. rs == SQR_OK
            call db_verify(db, 'u', rs, emsg); ok = ok .and. rs == SQR_OK
            call db_verify(db, 'e', rs, emsg); ok = ok .and. rs == SQR_OK
            call db_find_range(db, 't', 'a', 5_int32, 5_int32, cur, rs)
            ok = ok .and. rs == SQR_OK
            call row_alloc(buf, db_record_size(db, 't'))
            nrows = 0
            drain: do
                call db_cursor_next(db, cur, rid, buf, more, ios)
                if (.not. more) exit drain
                nrows = nrows + 1
            end do drain
            if (.not. ok .or. nrows /= 2) unhealed = unhealed + 1
            call db_close(db)
        end do fault_loop
        call check(bad == 0, 'upgrade: every injected I/O failed the open')
        call check(unhealed == 0, 'upgrade: the next open completes it after every failure')
        ios = c_rmtree(DBDIR)
    end subroutine upgrade_recovery

    ! M3: a create_index that fails at any point (including the final
    ! write_schema) must tear the half-built index back down, so a retry on the
    ! same open handle succeeds — never a spurious SQR_DUP for an index the
    ! caller was told failed — and the rebuilt index actually resolves lookups.
    subroutine create_index_retry()
        integer :: rs, n, nops, bad
        integer(int32) :: r

        call fault_disarm()
        call prep_rows()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call fault_arm(BIG)
        call db_create_index(gdb, 'people', 'age', rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()

        bad = 0
        retry_loop: do n = 1, nops
            call prep_rows()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call fault_arm(n)
            call db_create_index(gdb, 'people', 'age', rs)
            call fault_disarm()
            if (rs == SQR_OK) then
                call close_g()
                cycle retry_loop           ! created despite the fault: fine
            end if
            ! Failed create -> retry unarmed on the same handle must succeed.
            call db_create_index(gdb, 'people', 'age', rs)
            if (rs /= SQR_OK) then
                bad = bad + 1
            else
                call db_find_by_int(gdb, 'people', 'age', 33_int32, r, rs)
                if (rs /= SQR_OK .or. r /= 1_int32) bad = bad + 1
            end if
            call close_g()
        end do retry_loop
        call check(bad == 0, &
            'create_index: clean retry after every mid-create failure (no stale SQR_DUP)')
    end subroutine create_index_retry

    ! Review §5 gap 4: db_close's flush failures must surface through its
    ! stat (the generic sweep deliberately runs close disarmed), and the
    ! store must reopen verified afterwards — a failed close may lose the
    ! final counter flush but must never corrupt the store.  The insert after
    ! the index leaves the counters stale, so the close has a schema to write:
    ! a close with nothing moved rewrites nothing (see close_clean below).
    subroutine close_failure()
        integer :: rs, cs, n, nops, bad, silent

        call fault_disarm()
        call prep_rows()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call db_create_index(gdb, 'people', 'age', rs)  ! a writable tree to flush
        call insert_row(gdb, 4_int32, 52_int32, 'Dave')  ! counters now stale
        call fault_arm(BIG)
        call db_close(gdb, cs)
        nops = fault_count()
        call fault_disarm()
        gopen = .false.
        call check(cs == SQR_OK, 'close: clean close reports OK')
        call check(nops > 0,     'close: close performs I/O (FAULT=on linked)')

        bad = 0; silent = 0
        close_loop: do n = 1, nops
            call prep_rows()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call db_create_index(gdb, 'people', 'age', rs)
            call insert_row(gdb, 4_int32, 52_int32, 'Dave')
            call fault_arm(n)
            call db_close(gdb, cs)
            call fault_disarm()
            gopen = .false.
            if (cs == SQR_OK) silent = silent + 1
            ! Whatever the failed close managed to flush, the store must
            ! reopen and verify (open_data re-derives the counters).
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            if (rs /= SQR_OK) then
                bad = bad + 1
            else
                call db_verify(gdb, 'people', rs)
                if (rs /= SQR_OK) bad = bad + 1
            end if
            call close_g()
        end do close_loop
        call check(silent == 0, 'close: every injected flush failure surfaced in stat')
        call check(bad == 0,    'close: store reopens verified after every failed close')
    end subroutine close_failure

    ! A close with nothing moved since the schema was last written performs
    ! no checked I/O at all: the counters match the file's image, the catalog
    ! is current, so nothing is rewritten.  And a non-durable store performs
    ! no fsync anywhere — journal, commit barrier, or the schema/catalog
    ! replace — across create table, insert and close (the fsyncs are the
    ! injectable points a durable run of the same sequence does consume).
    subroutine close_clean()
        integer :: rs, cs, n_ro, n_durable, n_scratch
        character(len=*), parameter :: SDIR = 'fault_scratch_db'

        call fault_disarm()
        call prep_rows()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call fault_arm(BIG)
        call db_close(gdb, cs)
        n_ro = fault_count()
        call fault_disarm()
        gopen = .false.
        call check(cs == SQR_OK .and. n_ro == 0, 'close: an unchanged store is closed without a write')

        ! Same sequence durable and non-durable: the non-durable run must
        ! consume strictly fewer checked points (every fsync gone), and none
        ! of the points it does consume may be an fsync.
        n_durable = scratch_io_points(SDIR, durable=.true.)
        n_scratch = scratch_io_points(SDIR, durable=.false.)
        call check(n_durable > n_scratch, 'durable off: fewer checked I/O points than the durable run')
        call check(n_scratch > 0,         'durable off: the writes themselves are still checked')
        rs = c_rmtree(SDIR)
    end subroutine close_clean

    ! Checked I/O points consumed by create table + one insert + close on a
    ! fresh store opened with the given durability.
    integer function scratch_io_points(dir, durable) result(n)
        character(len=*), intent(in) :: dir
        logical,          intent(in) :: durable
        type(db_t) :: db
        integer :: st
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        st = c_rmtree(dir)
        call fault_arm(BIG)
        call db_open(db, dir, st, durable=durable)
        call db_create_table(db, 's', [column_t('v', DT_INT, 4)], st)
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int(buf, db%tables(1)%cols(1), 7_int32)
        call db_insert(db, 's', buf, rid, st)
        call db_close(db)
        n = fault_count()
        call fault_disarm()
    end function scratch_io_points

    ! Auto-commit rollback sweep. Each row mutator wraps its work in an
    ! implicit transaction (ac_begin/ac_end), so any injected I/O failure must
    ! roll the whole op back: on reopen the table is exactly as it was before
    ! the op (live_count == base_live) and db_verify — which walks every index
    ! against a full scan — passes. A torn row/index would fail one of these.
    subroutine mutator_atomicity(label, prep, op, base_live)
        character(len=*),  intent(in) :: label
        procedure(prep_i)             :: prep
        procedure(op_i)               :: op
        integer,           intent(in) :: base_live
        type(db_t) :: db
        integer :: rs, n, nops, ti, bad
        character(len=128) :: emsg

        call fault_disarm()
        call prep()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call fault_arm(BIG)
        call op(rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()

        bad = 0
        atom_loop: do n = 1, nops
            call prep()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call fault_arm(n)
            call op(rs)
            call fault_disarm()
            call close_g()
            if (rs == SQR_OK) cycle atom_loop      ! op completed: fine
            ! Failed op: the bracket must have rolled back to the pre-op state.
            call db_open(db, DBDIR, rs)
            if (rs /= SQR_OK) then
                bad = bad + 1
                cycle atom_loop
            end if
            ti = db_table_index(db, 'people')
            call db_verify(db, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            if (db%tables(ti)%live_count /= base_live) bad = bad + 1
            call db_close(db)
        end do atom_loop
        call check(bad == 0, &
            label // ': rolled back to pre-op state + db_verify clean after every mid-op fault')
    end subroutine mutator_atomicity

    ! Insert a fixed unique row into 'people' (helper for commit_durability).
    subroutine insert_zoe(db, rid, rs)
        type(db_t),     intent(inout) :: db
        integer(int32), intent(out)   :: rid
        integer,        intent(out)   :: rs
        character(len=:), allocatable :: buf
        integer :: ti
        ti = db_table_index(db, 'people')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 9_int32)
        call row_set_int (buf, db%tables(ti)%cols(2), 99_int32)
        call row_set_char(buf, db%tables(ti)%cols(3), 'Zoe')
        call db_insert(db, 'people', buf, rid, rs)
    end subroutine insert_zoe

    ! Commit durability barrier (Phase 2 / Option B).  An explicit transaction
    ! makes its base writes durable and only then voids the journal at commit.
    ! The auto-commit sweep covers these fsync ordinals en masse, but only a
    ! named test guards against the barrier being removed — which would
    ! silently drop those ordinals and the coverage with them.  Every io_check
    ! inside db_commit, when made to fail, must abort the commit and roll the
    ! transaction back: the row is absent on reopen and db_verify is clean —
    ! never a false success that a later crash would lose.
    subroutine commit_durability()
        integer :: rs, n_before, n_after, k, ti, base_live, bad
        integer(int32) :: rid
        character(len=128) :: emsg

        ! Find the io_check ordinals that fall inside db_commit on a clean run.
        ! prep_indexed rebuilds an identical fixture each time, so the count of
        ! io_checks consumed by db_begin + the insert (n_before) is reproducible.
        call prep_indexed()
        call db_open(gdb, DBDIR, rs)
        gopen = .true.
        ti = db_table_index(gdb, 'people')
        base_live = gdb%tables(ti)%live_count
        call fault_arm(BIG)                      ! never fires; just counts
        call db_begin(gdb, rs)
        call insert_zoe(gdb, rid, rs)
        n_before = fault_count()
        call db_commit(gdb, rs)
        n_after = fault_count()
        call fault_disarm()
        call close_g()
        call check(rs == SQR_OK,        'commit_durability: clean commit succeeds')
        call check(n_after > n_before,  'commit_durability: commit performs the fsync barrier')

        bad = 0
        barrier_sweep: do k = n_before + 1, n_after
            call prep_indexed()
            call db_open(gdb, DBDIR, rs)
            gopen = .true.
            call fault_arm(k)                    ! fires during db_commit (k > n_before)
            call db_begin(gdb, rs)
            call insert_zoe(gdb, rid, rs)        ! pre-commit work: must still succeed
            if (rs /= SQR_OK) bad = bad + 1
            call db_commit(gdb, rs)
            if (rs == SQR_OK) bad = bad + 1      ! a faulted commit must not claim success
            call fault_disarm()
            call close_g()
            ! Reopen: the whole transaction must be gone (rolled back).
            call db_open(gdb, DBDIR, rs)
            gopen = .true.
            ti = db_table_index(gdb, 'people')
            call db_verify(gdb, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            if (gdb%tables(ti)%live_count /= base_live) bad = bad + 1
            call close_g()
        end do barrier_sweep
        call check(bad == 0, &
            'commit_durability: every commit-barrier fault aborts and rolls back')
    end subroutine commit_durability

    ! Q1 history-capture failure policy.  capture_step runs inside db_commit
    ! BEFORE txn_commit; if its post-image reads (finish_delta) fail it must drop
    ! the session-only history (coherent-or-empty) while the commit's own
    ! durability stands.  Sweep a fault across a LABELLED commit: whatever the
    ! fault point, the db must stay coherent in the same session and any undo step
    ! that survives must replay cleanly (undo restores + verifies, redo re-applies
    ! + verifies) — a half-built step from a failed capture would break this.
    subroutine capture_fault()
        integer :: rs, n_before, n_after, k, bad
        integer(int32) :: rid
        character(len=128) :: emsg

        call prep_indexed()
        call db_open(gdb, DBDIR, rs)
        gopen = .true.
        call fault_arm(BIG)                      ! never fires; just counts
        call db_begin(gdb, rs, label='G')
        call insert_zoe(gdb, rid, rs)
        n_before = fault_count()
        call db_commit(gdb, rs)
        n_after = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK,         'capture_fault: clean labelled commit succeeds')
        call check(db_can_undo(gdb),     'capture_fault: clean labelled commit leaves one undo step')
        call close_g()

        bad = 0
        sweep: do k = n_before + 1, n_after
            call prep_indexed()
            call db_open(gdb, DBDIR, rs)
            gopen = .true.
            call fault_arm(k)
            call db_begin(gdb, rs, label='G')
            call insert_zoe(gdb, rid, rs)
            if (rs /= SQR_OK) bad = bad + 1              ! pre-commit work must still succeed
            call db_commit(gdb, rs)                      ! capture fault -> OK; barrier fault -> rollback
            call fault_disarm()
            ! Same session: the store must be coherent whatever the fault did.
            call db_verify(gdb, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            ! A surviving undo step must be replayable (undo then redo), never a
            ! half-captured step spliced onto the store.
            if (db_can_undo(gdb)) then
                call db_undo(gdb, rs)
                if (rs /= SQR_OK) bad = bad + 1
                call db_verify(gdb, 'people', rs, emsg)
                if (rs /= SQR_OK) bad = bad + 1
                call db_redo(gdb, rs)
                if (rs /= SQR_OK) bad = bad + 1
                call db_verify(gdb, 'people', rs, emsg)
                if (rs /= SQR_OK) bad = bad + 1
            end if
            call close_g()
        end do sweep
        call check(bad == 0, &
            'capture_fault: every labelled-commit fault leaves a coherent db + replayable history')
    end subroutine capture_fault

    ! C4 undo/redo atomicity.  The gesture inserts a row (the data file and
    ! the index grow, so undo truncates and redo re-extends) and deletes one
    ! (in-place regions).  Sweep a fault across every I/O of db_undo, or of
    ! db_redo after a clean undo.  A faulted apply must report an error, leave
    ! the store exactly as it was before the call — same session and after a
    ! reopen — and leave the step where it was, so that a retry completes.  A
    ! clean apply lands the other state.
    subroutine history_atomicity(redo)
        logical, intent(in) :: redo
        integer :: rs, k, nops, bad
        character(len=128) :: emsg
        character(len=:), allocatable :: what

        what = merge('redo', 'undo', redo)
        call hist_start(redo)
        call fault_arm(BIG)                      ! never fires; just counts
        call hist_apply(redo, rs)
        nops = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK, 'history_atomicity: clean ' // what // ' succeeds')
        call check(nops > 0,     'history_atomicity: ' // what // ' performs I/O')
        call check(hist_landed(redo, .true.), 'history_atomicity: clean ' // what // ' lands')
        call close_g()

        bad = 0
        sweep: do k = 1, nops
            call hist_start(redo)
            call fault_arm(k)
            call hist_apply(redo, rs)
            call fault_disarm()
            if (rs == SQR_OK) then
                if (.not. hist_landed(redo, .true.)) bad = bad + 1
            else
                if (.not. hist_landed(redo, .false.)) bad = bad + 1
                ! The step is still where it was: a retry goes through.
                call hist_apply(redo, rs)
                if (rs /= SQR_OK) bad = bad + 1
                if (.not. hist_landed(redo, .true.)) bad = bad + 1
            end if
            call close_g()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            if (rs /= SQR_OK) then
                bad = bad + 1
                cycle sweep
            end if
            call db_verify(gdb, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            if (hist_has_gesture() .neqv. redo) bad = bad + 1
            call close_g()
        end do sweep
        call check(bad == 0, 'history_atomicity: every mid-' // what // &
                             ' fault leaves the gesture wholly applied or wholly not')
    end subroutine history_atomicity

    ! Review finding D4.  Inside an explicit transaction a fault is injected at
    ! every I/O of an indexed insert.  The insert cannot be undone on its own,
    ! so once it has begun to write the transaction must be doomed: the next
    ! write refused, db_commit rolling back, and the store as it was.  A fault
    ! before the first write may leave the transaction usable; committing it
    ! must then leave a store that verifies, with no trace of the insert.
    subroutine doomed_by_fault()
        integer :: rs, k, nops, bad, ndoomed
        integer(int32) :: rid

        call prep_indexed()
        call db_open(gdb, DBDIR, rs)
        gopen = rs == SQR_OK
        call db_begin(gdb, rs)
        call fault_arm(BIG)                      ! never fires; just counts
        call insert_zoe(gdb, rid, rs)
        nops = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK .and. nops > 0, 'doomed: clean in-transaction insert performs I/O')
        call check(.not. db_txn_doomed(gdb), 'doomed: a clean insert does not doom')
        call db_rollback(gdb, rs)
        call close_g()

        bad = 0
        ndoomed = 0
        sweep: do k = 1, nops
            call prep_indexed()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            call db_begin(gdb, rs)
            call fault_arm(k)
            call insert_zoe(gdb, rid, rs)
            call fault_disarm()
            if (rs == SQR_OK) then               ! the fault was not on this path
                call db_rollback(gdb, rs)
                call close_g()
                cycle sweep
            end if
            if (db_txn_doomed(gdb)) then
                ndoomed = ndoomed + 1
                call db_delete(gdb, 'people', 2_int32, rs)
                if (rs /= SQR_INVALID) bad = bad + 1
                call db_commit(gdb, rs)
                if (rs /= SQR_INVALID) bad = bad + 1
            else
                call db_commit(gdb, rs)
                if (rs /= SQR_OK) bad = bad + 1
            end if
            if (db_in_txn(gdb)) bad = bad + 1
            if (.not. no_zoe_and_sound()) bad = bad + 1
            call close_g()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            if (rs /= SQR_OK) then
                bad = bad + 1
                cycle sweep
            end if
            if (.not. no_zoe_and_sound()) bad = bad + 1
            call close_g()
        end do sweep
        call check(ndoomed > 0, 'doomed: a fault after the first write dooms the transaction')
        call check(bad == 0, 'doomed: a part-applied insert is never committed, at any fault')
    end subroutine doomed_by_fault

    ! True when the open store verifies and holds the three fixture rows only.
    ! db_set_readonly saves the row counters before it gives up the right
    ! to write.  Every I/O fault in that save must leave the handle writable
    ! (so the ordinary close can still save them) and the database sound.
    subroutine readonly_by_fault()
        integer :: nops, n, rs, bad, st
        character(len=128) :: emsg
        call fault_disarm()
        call prep_rows()
        call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
        call db_delete(gdb, 'people', 2_int32, rs)
        call fault_arm(BIG)
        call db_set_readonly(gdb, rs)
        nops = fault_count()
        call fault_disarm()
        call close_g()
        call check(rs == SQR_OK .and. nops > 0, 'readonly: clean demotion does I/O')
        bad = 0
        fault_loop: do n = 1, nops
            call prep_rows()
            call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
            call db_delete(gdb, 'people', 2_int32, rs)
            call fault_arm(n)
            call db_set_readonly(gdb, rs)
            call fault_disarm()
            if (rs == SQR_OK .or. gdb%readonly) bad = bad + 1
            call close_g()
            call db_open(gdb, DBDIR, rs); gopen = rs == SQR_OK
            call db_verify(gdb, 'people', st, emsg)
            if (rs /= SQR_OK .or. st /= SQR_OK .or. db_row_count(gdb, 'people') /= 2) bad = bad + 1
            call close_g()
        end do fault_loop
        call check(bad == 0, 'readonly: a failed demotion stays writable and loses no count')
    end subroutine readonly_by_fault

    logical function no_zoe_and_sound() result(ok)
        integer :: st
        integer(int32) :: r
        character(len=128) :: emsg
        call db_verify(gdb, 'people', st, emsg)
        ok = st == SQR_OK
        call db_find_by_int(gdb, 'people', 'age', 99_int32, r, st)
        ok = ok .and. (st /= SQR_OK .or. r == 0)
        ok = ok .and. db_row_count(gdb, 'people') == 3
    end function no_zoe_and_sound

    ! Fresh fixture with one committed gesture on the undo stack (and, for the
    ! redo sweep, undone again so that it sits on the redo stack).
    subroutine hist_start(redo)
        logical, intent(in) :: redo
        integer :: st
        integer(int32) :: rid
        call prep_indexed()
        call db_open(gdb, DBDIR, st)
        gopen = st == SQR_OK
        call db_begin(gdb, st, label='G')
        call insert_zoe(gdb, rid, st)
        call db_delete(gdb, 'people', 2_int32, st)
        call db_commit(gdb, st)
        if (redo) call db_undo(gdb, st)
    end subroutine hist_start

    subroutine hist_apply(redo, st)
        logical, intent(in)  :: redo
        integer, intent(out) :: st
        if (redo) then
            call db_redo(gdb, st)
        else
            call db_undo(gdb, st)
        end if
    end subroutine hist_apply

    ! True when the gesture's effect is in the store: Zoe present, row 2 gone.
    logical function hist_has_gesture() result(yes)
        integer :: st, ti
        integer(int32) :: r
        logical :: has_zoe, has_bob
        character(len=:), allocatable :: buf
        ti = db_table_index(gdb, 'people')
        call db_find_by_int(gdb, 'people', 'age', 99_int32, r, st)
        has_zoe = st == SQR_OK .and. r /= 0
        call row_alloc(buf, gdb%tables(ti)%record_size)
        call db_get(gdb, 'people', 2_int32, buf, st)
        has_bob = st == SQR_OK
        yes = has_zoe .and. .not. has_bob
    end function hist_has_gesture

    ! True when the open store is coherent and stands where it should after an
    ! undo/redo that went through (`applied`) or was rolled back: the gesture
    ! in or out as a whole, and its step on the matching stack.
    logical function hist_landed(redo, applied) result(ok)
        logical, intent(in) :: redo, applied
        logical :: want
        integer :: st, ti
        character(len=128) :: emsg
        want = applied .eqv. redo
        ti = db_table_index(gdb, 'people')
        call db_verify(gdb, 'people', st, emsg)
        ok = st == SQR_OK
        ok = ok .and. (hist_has_gesture() .eqv. want)
        ok = ok .and. gdb%tables(ti)%live_count == 3
        ok = ok .and. (db_can_undo(gdb) .eqv. want) .and. (db_can_redo(gdb) .neqv. want)
    end function hist_landed

    ! H1 recovery-failure path.  A transient I/O error while replaying the undo
    ! set must NOT void the journal: it stays hot so the next open retries the
    ! (idempotent, absolute-write) undo rather than destroying the one record
    ! that can still repair a half-restored base file.  Sweep a fault across a
    ! genuine hot-journal recovery; every failed replay must leave the journal
    ! hot, and a disarmed retry must then complete and restore the pre-crash
    ! bytes.  (Recovery's undo write/fsync are the injectable io_check points;
    ! the header reads are not, which is exactly the "mid-replay" fault we want.)
    subroutine recover_fault()
        type(db_t) :: db
        character(len=*), parameter :: REL = 'jf.bin'
        character(len=:), allocatable :: full
        integer :: rs, n, nops, bad, r

        call fault_disarm()
        r = c_rmtree(DBDIR)
        call db_open(db, DBDIR, rs)
        full = trim(db%dir) // '/' // REL
        call arm_region(db, full, REL)
        call fault_arm(BIG)                       ! never fires; just counts
        call jrnl_recover(db, rs)
        nops = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK, 'recover_fault: clean recovery succeeds')
        call check(nops > 0,     'recover_fault: recovery performs undo I/O')
        call check(jread_file(full, 16) == 'AAAAAAAAAAAAAAAA', &
                   'recover_fault: clean recovery restores original')
        call db_close(db)

        ! Targeted H1 check: the undo write is the FIRST io_check in recovery
        ! (apply_undo runs before void_header), so arming n=1 fails the replay
        ! itself.  A failed replay must leave the journal HOT — the pre-fix code
        ! voided it unconditionally, abandoning the one record that repairs the
        ! base file.  A disarmed retry then completes and voids it cleanly.
        r = c_rmtree(DBDIR)
        call db_open(db, DBDIR, rs)
        call arm_region(db, full, REL)
        call fault_arm(1)
        call jrnl_recover(db, rs)
        call fault_disarm()
        call check(rs /= SQR_OK,  'recover_fault: faulted replay reports failure')
        call check(jrnl_hot(db),  'recover_fault: faulted replay leaves journal hot (not voided)')
        call jrnl_recover(db, rs)
        call check(rs == SQR_OK,  'recover_fault: disarmed retry completes')
        call check(.not. jrnl_hot(db), 'recover_fault: retry voids the journal')
        call check(jread_file(full, 16) == 'AAAAAAAAAAAAAAAA', &
                   'recover_fault: retry restores the original')
        call db_close(db)

        ! Broad safety net: wherever a fault lands during recovery, a disarmed
        ! retry must restore the original bytes and end with the journal voided.
        bad = 0
        sweep: do n = 1, nops
            r = c_rmtree(DBDIR)
            call db_open(db, DBDIR, rs)
            call arm_region(db, full, REL)
            call fault_arm(n)
            call jrnl_recover(db, rs)
            call fault_disarm()
            if (rs /= SQR_OK) then
                call jrnl_recover(db, rs)                 ! disarmed retry
                if (rs /= SQR_OK) bad = bad + 1
                if (jrnl_hot(db)) bad = bad + 1           ! retry must void it
                if (jread_file(full, 16) /= 'AAAAAAAAAAAAAAAA') bad = bad + 1
            end if
            call db_close(db)
        end do sweep
        call check(bad == 0, &
            'recover_fault: a disarmed retry restores after a fault at any point')
    end subroutine recover_fault

    ! C8.  txn_arm's `armed` latch says "a hot header may be on disk, so commit
    ! and rollback must void it".  It used to follow the status of the LAST
    ! step of the arm, so a first arm whose header was written and flushed but
    ! whose fsync then failed left the latch clear and the header hot: the
    ! rollback did not void it, and the hot journal outlived its transaction.
    ! Sweep a fault across every io_check of the first arm: the arm must fail,
    ! and after a clean rollback the journal must never be hot.
    subroutine arm_fault_void()
        type(db_t) :: db
        character(len=*), parameter :: REL = 'jf.bin'
        character(len=:), allocatable :: full
        integer :: rs, n0, n1, k, r, silent, hot, wrong

        call fault_disarm()
        r = c_rmtree(DBDIR)
        call db_open(db, DBDIR, rs)
        full = trim(db%dir) // '/' // REL
        call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
        call fault_arm(BIG)                       ! never fires; just counts
        call txn_begin(db, rs)
        n0 = fault_count()
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=rs)
        n1 = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK .and. jrnl_hot(db), 'arm_fault: clean first arm leaves the journal hot')
        call check(n1 > n0, 'arm_fault: the arm performs I/O')
        call txn_rollback(db, rs)
        call check(rs == SQR_OK .and. .not. jrnl_hot(db), 'arm_fault: clean rollback voids it')
        call db_close(db)

        silent = 0; hot = 0; wrong = 0
        sweep: do k = n0 + 1, n1
            r = c_rmtree(DBDIR)
            call db_open(db, DBDIR, rs)
            call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
            call fault_arm(k)                     ! fires inside the first arm
            call txn_begin(db, rs)
            call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=rs)
            call fault_disarm()
            if (rs == SQR_OK) silent = silent + 1
            ! The arm failed, so the base write it guarded is not made.
            call txn_rollback(db, rs)
            if (rs /= SQR_OK) wrong = wrong + 1
            if (jrnl_hot(db)) hot = hot + 1
            if (jread_file(full, 16) /= 'AAAAAAAAAAAAAAAA') wrong = wrong + 1
            call db_close(db)
        end do sweep
        call check(silent == 0, 'arm_fault: every fault in the arm fails it')
        call check(hot == 0,    'arm_fault: no failed arm leaves a hot journal behind its rollback')
        call check(wrong == 0,  'arm_fault: the rollback after a failed arm is clean')
    end subroutine arm_fault_void

    ! C7.  A rollback that cannot finish leaves the journal hot on purpose —
    ! it is the one record that can complete the repair.  The handle used to
    ! go idle all the same, so the next mutation's first arm restarted the
    ! journal at offset 0 and destroyed it, and db_close deleted the file.
    ! Sweep a fault across every io_check of db_rollback.  Wherever the
    ! rollback fails: row writes, structural operations and the read-only
    ! demotion are all refused (SQR_ERR), the journal file survives db_close,
    ! and the next open finishes the rollback — table verified, row count as
    ! before the transaction.
    subroutine rollback_latch()
        integer :: rs, n_before, n_after, k, ti, base_live
        integer :: silent, unrefused, lost, unrecovered, nfailed, nhot, unreported, code
        integer(int32) :: rid
        character(len=128) :: emsg
        character(len=:), allocatable :: msg

        call prep_indexed()
        call db_open(gdb, DBDIR, rs)
        gopen = .true.
        ti = db_table_index(gdb, 'people')
        base_live = gdb%tables(ti)%live_count
        call fault_arm(BIG)                      ! never fires; just counts
        call db_begin(gdb, rs)
        call insert_zoe(gdb, rid, rs)
        n_before = fault_count()
        call db_rollback(gdb, rs)
        n_after = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK,       'rollback_latch: clean rollback succeeds')
        call check(n_after > n_before, 'rollback_latch: rollback performs I/O')
        call insert_zoe(gdb, rid, rs)
        call check(rs == SQR_OK,       'rollback_latch: a clean rollback leaves the handle writable')
        call close_g()

        silent = 0; unrefused = 0; lost = 0; unrecovered = 0; nfailed = 0; nhot = 0
        unreported = 0
        sweep: do k = n_before + 1, n_after
            call prep_indexed()
            call db_open(gdb, DBDIR, rs)
            gopen = .true.
            call fault_arm(k)                    ! fires during db_rollback
            call db_begin(gdb, rs)
            call insert_zoe(gdb, rid, rs)
            emsg = ''
            call db_rollback(gdb, rs, emsg)
            call fault_disarm()
            if (rs == SQR_OK) then
                silent = silent + 1
                call close_g()
                cycle sweep
            end if
            nfailed = nfailed + 1
            ! C10: the failure reaches errmsg and the handle's last error.
            call db_last_error(gdb, code=code, msg=msg)
            if (len_trim(emsg) == 0 .or. code /= rs .or. len(msg) == 0) &
                unreported = unreported + 1
            if (jrnl_hot(gdb)) nhot = nhot + 1
            ! Every kind of write is now refused, and says what to do.
            call insert_zoe(gdb, rid, rs)
            if (rs /= SQR_ERR) unrefused = unrefused + 1
            call db_last_error(gdb, msg=msg)
            if (index(msg, 'close and reopen') == 0) unrefused = unrefused + 1
            call db_begin(gdb, rs)
            if (rs /= SQR_ERR) unrefused = unrefused + 1
            call db_create_index(gdb, 'people', 'name', rs)
            if (rs /= SQR_ERR) unrefused = unrefused + 1
            call db_compact(gdb, 'people', rs)
            if (rs /= SQR_ERR) unrefused = unrefused + 1
            call db_set_readonly(gdb, rs)
            if (rs /= SQR_ERR) unrefused = unrefused + 1
            ! The close keeps the journal...
            call close_g()
            if (.not. c_path_exists(DBDIR // '/_journal.dat')) lost = lost + 1
            ! ...and the next open finishes the rollback.
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            if (rs /= SQR_OK) then
                unrecovered = unrecovered + 1
                cycle sweep
            end if
            ti = db_table_index(gdb, 'people')
            if (gdb%tables(ti)%live_count /= base_live) unrecovered = unrecovered + 1
            call db_verify(gdb, 'people', rs, emsg)
            if (rs /= SQR_OK) unrecovered = unrecovered + 1
            if (jrnl_hot(gdb)) unrecovered = unrecovered + 1
            call insert_zoe(gdb, rid, rs)        ! and the reopened handle writes again
            if (rs /= SQR_OK) unrecovered = unrecovered + 1
            call close_g()
        end do sweep
        call check(silent == 0,      'rollback_latch: every fault in the rollback fails it')
        call check(unreported == 0,  'rollback_latch: a failed rollback sets errmsg and the last error')
        call check(nfailed > 0 .and. nhot > 0, &
                                     'rollback_latch: failed rollbacks leave the journal hot')
        call check(unrefused == 0,   'rollback_latch: every write is refused after a failed rollback')
        call check(lost == 0,        'rollback_latch: db_close keeps the journal')
        call check(unrecovered == 0, 'rollback_latch: the next open completes the rollback')
    end subroutine rollback_latch

    ! C9.  The history changes only when a gesture's commit succeeds.  Gesture
    ! A is committed and undone, so the redo stack holds it; then a fault is
    ! swept across the commit of gesture B.  A commit that fails was rolled
    ! back: A must still be redoable (the redo stack used to be cleared before
    ! the commit was attempted) and B must not be on the undo stack.  A commit
    ! that succeeds has forked the timeline, so nothing is redoable.
    subroutine commit_keeps_redo()
        integer :: rs, n_before, n_after, k, bad, nfailed, nkept
        integer(int32) :: rid
        character(len=128) :: emsg

        call redo_start()
        call fault_arm(BIG)                      ! never fires; just counts
        call db_begin(gdb, rs, label='B')
        call insert_zoe(gdb, rid, rs)
        n_before = fault_count()
        call db_commit(gdb, rs)
        n_after = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK, 'commit_keeps_redo: clean commit succeeds')
        call check(db_can_undo(gdb) .and. .not. db_can_redo(gdb), &
                   'commit_keeps_redo: a committed gesture forks the timeline')
        call close_g()

        bad = 0; nfailed = 0; nkept = 0
        sweep: do k = n_before + 1, n_after
            call redo_start()
            call fault_arm(k)                    ! fires during db_commit
            call db_begin(gdb, rs, label='B')
            call insert_zoe(gdb, rid, rs)
            call db_commit(gdb, rs)
            call fault_disarm()
            if (rs == SQR_OK) then
                if (db_can_redo(gdb)) bad = bad + 1
                call close_g()
                cycle sweep
            end if
            nfailed = nfailed + 1
            ! A rollback that itself failed latched the handle; its history
            ! is beside the point until the reopen.
            if (gdb%jrnl%needs_recovery) then
                call close_g()
                cycle sweep
            end if
            if (db_can_undo(gdb) .or. .not. db_can_redo(gdb)) then
                bad = bad + 1
                call close_g()
                cycle sweep
            end if
            nkept = nkept + 1
            call db_redo(gdb, rs)                ! A goes back in
            if (rs /= SQR_OK) bad = bad + 1
            call db_verify(gdb, 'people', rs, emsg)
            if (rs /= SQR_OK) bad = bad + 1
            call close_g()
        end do sweep
        call check(nfailed > 0 .and. nkept > 0, &
                   'commit_keeps_redo: some faults fail the commit with the handle usable')
        call check(bad == 0, &
                   'commit_keeps_redo: a failed commit leaves the redo step, and it replays')
    end subroutine commit_keeps_redo

    ! Fresh indexed db on gdb with one gesture committed and undone.
    subroutine redo_start()
        integer :: rs
        integer(int32) :: rid
        call prep_indexed()
        call db_open(gdb, DBDIR, rs)
        gopen = .true.
        call db_begin(gdb, rs, label='A')
        call insert_zoe(gdb, rid, rs)
        call db_commit(gdb, rs)
        call db_undo(gdb, rs)
    end subroutine redo_start

    ! C11.  A region that covers a record only partly is written back by
    ! reading the record, patching it and writing it whole.  A failed read
    ! used to be taken for "past the end of the file" and the record rebuilt
    ! from zeros, wiping the bytes outside the region.  Log two bytes in the
    ! middle of the second record, sweep a fault across the rollback, and
    ! compare every row with what it held before: a fault fails the rollback
    ! and, after the reopen, no row has changed.
    subroutine partial_record_fault()
        integer, parameter :: NROWS = 3
        character(len=:), allocatable :: want(:), got
        integer :: rs, n0, n1, k, i, ti, recl, silent, wiped
        integer(int64) :: off

        call prep_rows()
        call db_open(gdb, DBDIR, rs)
        gopen = .true.
        ti   = db_table_index(gdb, 'people')
        recl = gdb%tables(ti)%record_size
        off  = int(recl, int64) + 2_int64        ! bytes 2..3 of the second record
        allocate(character(len=recl) :: want(NROWS), got)
        do i = 1, NROWS
            call db_get(gdb, 'people', int(i, int32), want(i), rs)
        end do
        call fault_arm(BIG)                      ! never fires; just counts
        call db_begin(gdb, rs)
        call jrnl_log_region(gdb, 'people.dat', off, 2_int64, stat=rs)
        call check(rs == SQR_OK, 'partial_record: region logged')
        n0 = fault_count()
        call db_rollback(gdb, rs)
        n1 = fault_count()
        call fault_disarm()
        call check(rs == SQR_OK .and. n1 > n0, 'partial_record: clean rollback succeeds')
        wiped = 0
        do i = 1, NROWS
            call db_get(gdb, 'people', int(i, int32), got, rs)
            if (rs /= SQR_OK .or. got /= want(i)) wiped = wiped + 1
        end do
        call check(wiped == 0, 'partial_record: clean rollback leaves every row as it was')
        call close_g()

        silent = 0; wiped = 0
        sweep: do k = n0 + 1, n1
            call prep_rows()
            call db_open(gdb, DBDIR, rs)
            gopen = .true.
            call fault_arm(k)                    ! fires during db_rollback
            call db_begin(gdb, rs)
            call jrnl_log_region(gdb, 'people.dat', off, 2_int64, stat=rs)
            call db_rollback(gdb, rs)
            call fault_disarm()
            if (rs == SQR_OK) silent = silent + 1
            call close_g()
            call db_open(gdb, DBDIR, rs)
            gopen = rs == SQR_OK
            if (rs /= SQR_OK) then
                wiped = wiped + 1
                cycle sweep
            end if
            do i = 1, NROWS
                call db_get(gdb, 'people', int(i, int32), got, rs)
                if (rs /= SQR_OK .or. got /= want(i)) wiped = wiped + 1
            end do
            call close_g()
        end do sweep
        call check(silent == 0, 'partial_record: every fault in the rollback fails it')
        call check(wiped == 0,  'partial_record: no fault costs a row its bytes')
    end subroutine partial_record_fault

    ! Arm a hot journal capturing REL[5..8] and perform the uncommitted base
    ! overwrite, leaving the file hot for recovery to roll back to 'AAAA...'.
    subroutine arm_region(db, full, rel)
        type(db_t),       intent(inout) :: db
        character(len=*), intent(in)    :: full, rel
        integer :: rs
        call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
        call txn_begin(db, rs)
        call jrnl_log_region(db, rel, 5_int64, 4_int64, stat=rs)
        call txn_arm(db, rs)
        call jwrite_region(full, 5_int64, 'CCCC')
    end subroutine arm_region

    subroutine jwrite_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u, io
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', iostat=io)
        write(u) content
        close(u)
    end subroutine jwrite_file

    subroutine jwrite_region(path, off, content)
        character(len=*), intent(in) :: path, content
        integer(int64),   intent(in) :: off
        integer :: u, io
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=io)
        write(u, pos=off) content
        close(u)
    end subroutine jwrite_region

    function jread_file(path, n) result(s)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: n
        character(len=n) :: s
        integer :: u, io
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=io)
        read(u, pos=1) s
        close(u)
    end function jread_file

end program utest_fault

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
    use clib_wrap, only: c_rmtree
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

    abstract interface
        subroutine prep_i()
        end subroutine prep_i
        subroutine op_i(rs)
            integer, intent(out) :: rs
        end subroutine op_i
    end interface

    call sweep('open',         prep_fulldb,      op_open,    .true.)
    call sweep('insert',       prep_table,       op_insert,  .false.)
    call sweep('update',       prep_rows,        op_update,  .false.)
    call sweep('delete',       prep_rows,        op_delete,  .false.)
    call sweep('get',          prep_rows,        op_get,     .false.)
    call sweep('create_index', prep_rows,        op_index,   .false.)
    call sweep('find_by_int',  prep_indexed,     op_find,    .false.)
    call sweep('compact',      prep_compactable, op_compact, .false.)
    call sweep('set_text',     prep_textrow,     op_settext, .false.)
    call sweep('get_text',     prep_textset,     op_gettext, .false., expect=SQR_ERR)

    call compact_atomicity()

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

    ! H1: a faulted undo replay must leave the journal hot, never voided.
    call recover_fault()

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

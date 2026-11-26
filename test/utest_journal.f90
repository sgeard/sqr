! utest_journal — recovery against a torn / corrupt _journal.dat.
!
! The engine-armed journals are already exercised by utest_sqr (commit no-op,
! crash rollback, extend truncate).  This program covers the *review gap* the
! 2026-07-04 fault review named: journals the API never armed cleanly — a
! foreign magic, a mismatched checksum, a truncated payload, an implausible
! payload length.  In every case recovery must be SAFE: it never applies
! garbage over the base file and never aborts inside db_open.  A journal that
! is not hot (foreign magic) is ignored.  A HOT journal that fails its checks
! is damage to the one record that could take a half-applied transaction out,
! so the open is refused (SQR_ERR), the journal is left exactly as found, and
! the message names the way out: removing _journal.dat opens the database as
! it stands.
!
! Also here: the journal's payload ceiling (SQR_JRNL_MAX_PAYLOAD, lowered per
! handle through jrnl%max_payload so the test need not write 2 GiB).
!
! The fixture arms a real hot journal (region-capture + base overwrite) exactly
! as test_lifecycle does, releases the advisory lock to stand in for the dead
! writer, then perturbs the on-disk header before a fresh read-write open runs
! recovery.  Header layout (see txn_arm): magic[1..4] fmt[5..8] state[9..12]
! nrec[13..16] cksum[17..20] plen[21..28], payload at JHEADER+1 = 65.
program utest_journal
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use sqr
    use clib_wrap, only: c_rmtree, c_truncate, c_file_size, c_lock_release, c_remove, &
                         c_path_exists
    implicit none

    integer :: pass = 0, fail = 0
    character(len=*), parameter :: DIR   = 'utest_journal_db'
    character(len=*), parameter :: REL   = 'jtest.bin'
    character(len=*), parameter :: JPATH = DIR // '/_journal.dat'
    character(len=*), parameter :: FULL  = DIR // '/' // REL
    integer :: ios

    ios = c_rmtree(DIR)

    ! Positive control: an intact hot journal still rolls the base write back on
    ! the recovering open (guards the H1/H6 changes at the db_open level).
    call one_case('intact hot journal recovers (rolls back)', perturb_none, &
                  expect_rollback=.true.)

    ! E1 partial-append: a valid prefix (named by payload_len) followed by torn
    ! bytes beyond it — a crash mid-append of a NEW record.  Recovery reads only
    ! the durable prefix, its checksum matches, so it rolls the base write back.
    call one_case('trailing garbage past plen recovers', perturb_trailing_garbage, &
                  expect_rollback=.true.)

    ! Not hot: nothing to recover, the open succeeds and touches nothing.
    call one_case('foreign magic  -> no-op, safe',       perturb_magic,     .false.)
    ! Hot but damaged: the open is refused and the journal kept.
    call refused_case('bad checksum',       perturb_checksum)
    call refused_case('truncated payload',  perturb_truncate)
    call refused_case('huge plen (>int32)', perturb_plen_huge)
    call refused_case('negative plen',      perturb_plen_neg)

    ! Tail records: the bytes a truncation removes come back on recovery, at
    ! the point the file was cut to and nowhere else.
    call tail_case('tail: truncated file restored',            8_int64, .true.)
    call tail_case('tail: truncation never happened',         16_int64, .true.)
    call tail_case('tail: interrupted re-append completed',   12_int64, .true.)
    call tail_case('tail: file cut short of the record',       4_int64, .false.)
    call tail_rollback()

    ! The payload ceiling: raw journal calls, then through the row API.
    call payload_limit_raw()
    call payload_limit_api()

    ! A region that covers a record only partly, through the held data unit.
    call partial_record_rollback()

    ios = c_rmtree(DIR)
    print '(a,i0,a,i0,a)', 'sqr journal tests: ', pass, ' passed, ', fail, ' failed'
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

    ! Arm a hot journal that captured jtest.bin[5..8], perform the (uncommitted)
    ! base overwrite, then drop the advisory lock so the recovering open is not
    ! blocked by this same-process stand-in for the crashed writer.
    subroutine arm_hot()
        type(db_t), target :: db
        integer :: st, r
        r = c_rmtree(DIR)
        call db_open(db, DIR, stat=st)
        call jwrite_file(FULL, 'AAAAAAAAAAAAAAAA')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=st)
        call txn_arm(db, st)
        call jwrite_region(FULL, 5_int64, 'CCCC')     ! uncommitted base write
        call check(jrnl_hot(db), 'armed journal reads hot')
        call c_lock_release(db%lock_tok)              ! the crash: OS drops the lock
        call jrnl_close_unit(db)                      ! ...and every open file with it
    end subroutine arm_hot

    ! Arm, perturb the journal, recover via a fresh read-write open, assert
    ! safety.  expect_rollback selects the intact positive control (base bytes
    ! must be restored) from the torn cases (recovery must not corrupt).
    subroutine one_case(label, perturb, expect_rollback)
        character(len=*), intent(in) :: label
        interface
            subroutine perturb()
            end subroutine perturb
        end interface
        logical, intent(in) :: expect_rollback
        type(db_t), target :: db2
        integer :: st

        call arm_hot()
        call perturb()
        call db_open(db2, DIR, stat=st)
        call check(st == SQR_OK, label // ': recovering open succeeds')
        call check(.not. jrnl_hot(db2), label // ': journal not hot after recovery')
        if (expect_rollback) then
            call check(jread_file(FULL, 16) == 'AAAAAAAAAAAAAAAA', &
                       label // ': base rolled back to pre-txn bytes')
        else
            ! The base file must remain readable and its declared length intact
            ! (recovery neither applied garbage nor crashed part-way).
            call check(jfile_size(FULL) == 16_int64, label // ': base file intact')
        end if
        call db_close(db2)
    end subroutine one_case

    ! Arm, damage the hot journal, and open: the open must be refused with a
    ! message naming the way out, the journal must still be hot and the base
    ! file exactly as the "crash" left it.  Taking the way out (removing the
    ! journal) then opens the database as it stands.
    subroutine refused_case(label, perturb)
        character(len=*), intent(in) :: label
        interface
            subroutine perturb()
            end subroutine perturb
        end interface
        type(db_t), target :: db2
        integer :: st, r
        character(len=256) :: emsg

        call arm_hot()
        call perturb()
        emsg = ''
        call db_open(db2, DIR, stat=st, errmsg=emsg)
        call check(st == SQR_ERR, label // ': open refused (SQR_ERR)')
        call check(index(emsg, '_journal.dat') > 0, label // ': message names the journal file')
        call check(file_hot(JPATH), label // ': journal left hot, not voided')
        call check(jread_file(FULL, 16) == 'AAAACCCCAAAAAAAA', &
                   label // ': base file as the crash left it')
        r = c_remove(JPATH)
        call db_open(db2, DIR, stat=st)
        call check(st == SQR_OK, label // ': opens once the journal is removed')
        call check(jread_file(FULL, 16) == 'AAAACCCCAAAAAAAA', &
                   label // ': opened as it stands')
        call db_close(db2)
    end subroutine refused_case

    ! The ceiling on one transaction's undo payload, against the raw journal
    ! calls.  Each 4-byte region of jtest.bin serialises to 53 bytes (40 fixed
    ! + 9 of path + 4 of data), so a ceiling of 120 holds two and refuses the
    ! third — before the base write it would have guarded.  The in-memory
    ! rollback then undoes the two that were written, and the limit is per
    ! transaction: the next one starts from zero.
    subroutine payload_limit_raw()
        type(db_t), target :: db
        integer :: st, r
        character(len=:), allocatable :: msg
        r = c_rmtree(DIR)
        call db_open(db, DIR, stat=st)
        call jwrite_file(FULL, 'AAAAAAAAAAAAAAAA')
        db%jrnl%max_payload = 120_int64
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 1_int64, 4_int64, stat=st)
        call check(st == SQR_OK, 'limit: first region fits')
        call jwrite_region(FULL, 1_int64, 'BBBB')
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=st)
        call check(st == SQR_OK, 'limit: second region fits')
        call jwrite_region(FULL, 5_int64, 'CCCC')
        call jrnl_log_region(db, REL, 9_int64, 4_int64, stat=st)
        call check(st == SQR_FULL, 'limit: third region refused with SQR_FULL')
        call db_last_error(db, msg=msg)
        call check(index(msg, 'transaction too large') > 0, 'limit: message says too large')
        call check(jrnl_hot(db), 'limit: journal still hot with the two durable records')
        call txn_rollback(db, st)
        call check(st == SQR_OK .and. .not. jrnl_hot(db), 'limit: rollback succeeds, journal voided')
        call check(jread_file(FULL, 16) == 'AAAAAAAAAAAAAAAA', 'limit: rollback restores every byte')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 9_int64, 4_int64, stat=st)
        call check(st == SQR_OK, 'limit: a new transaction starts from zero')
        call txn_rollback(db, st)
        ! A ceiling set above the compiled one does not raise it.
        db%jrnl%max_payload = huge(0_int64)
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 1_int64, 4_int64, stat=st)
        call check(st == SQR_OK, 'limit: handle ceiling above the compiled one is harmless')
        call txn_rollback(db, st)
        call db_close(db)
    end subroutine payload_limit_raw

    ! The same ceiling through the row API.  In an explicit transaction the
    ! insert that hits it fails with SQR_FULL and dooms the transaction, the
    ! commit rolls everything back, and the table is intact afterwards.  An
    ! auto-committed insert that hits it rolls itself back.
    subroutine payload_limit_api()
        type(db_t), target :: db
        type(column_t) :: c(1)
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        integer, parameter :: NFIX = 2000
        integer :: st, r, i, nfit
        integer(int32) :: rid
        r = c_rmtree(DIR)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        ! durable=.false.: the fixture is 2000 auto-committed inserts and the
        ! ceiling does not depend on the fsyncs.
        call db_open(db, DIR, stat=st, durable=.false.)
        call db_create_table(db, 't', c, st)
        call db_create_index(db, 't', 'v', stat=st)
        call row_alloc(buf, db%tables(1)%record_size)
        ! Keys 10, 20, ... 20000: enough to spread the index over many leaf
        ! pages, each of which costs a transaction one page image the first
        ! time it is written.
        fixture: do i = 1, NFIX
            call row_set_int(buf, db%tables(1)%cols(1), int(10 * i, int32))
            call db_insert(db, 't', buf, rid, st)
            if (st /= SQR_OK) exit fixture
        end do fixture
        call check(st == SQR_OK, 'limit api: fixture rows inserted')

        ! Room for two page images, not three; the inserts below land in
        ! leaves far apart.
        db%jrnl%max_payload = 9000_int64
        call db_begin(db, st)
        nfit = 0
        fill: do i = 0, 19
            call row_set_int(buf, db%tables(1)%cols(1), int(5 + 1000 * i, int32))
            call db_insert(db, 't', buf, rid, st)
            if (st /= SQR_OK) exit fill
            nfit = nfit + 1
        end do fill
        call check(st == SQR_FULL, 'limit api: an insert is refused with SQR_FULL')
        call check(nfit > 0, 'limit api: earlier inserts in the transaction succeeded')
        call check(db_txn_doomed(db), 'limit api: the transaction is doomed')
        call db_commit(db, st)
        call check(st /= SQR_OK, 'limit api: commit of the doomed transaction refuses')
        call check(db%tables(1)%live_count == NFIX, 'limit api: every insert rolled back')
        call db_verify(db, 't', st, emsg)
        call check(st == SQR_OK, 'limit api: table verifies after the rollback')
        call check(.not. jrnl_hot(db), 'limit api: journal not hot')

        ! Auto-commit: one mutator too big for the ceiling undoes itself.
        db%jrnl%max_payload = 8_int64
        call row_set_int(buf, db%tables(1)%cols(1), 5001_int32)
        call db_insert(db, 't', buf, rid, st)
        call check(st == SQR_FULL, 'limit api: auto-committed insert refused with SQR_FULL')
        call check(db%tables(1)%live_count == NFIX, 'limit api: auto-committed insert left nothing')
        call db_verify(db, 't', st, emsg)
        call check(st == SQR_OK, 'limit api: table verifies after the refused insert')

        ! Back at the compiled ceiling the same insert goes in.
        db%jrnl%max_payload = SQR_JRNL_MAX_PAYLOAD
        call db_insert(db, 't', buf, rid, st)
        call check(st == SQR_OK .and. db%tables(1)%live_count == NFIX + 1, &
                   'limit api: the insert succeeds at the compiled ceiling')
        call db_close(db)
        call check(.not. c_path_exists(JPATH), 'limit api: clean close removes the journal')
    end subroutine payload_limit_api

    ! A region need not align to the data file's records.  Log two bytes in
    ! the middle of the second record, rewrite a row in the same transaction,
    ! and roll back: the partly covered record is read, patched and written
    ! whole, so every row comes back exactly as it was.
    subroutine partial_record_rollback()
        integer, parameter :: NROWS = 3
        type(db_t), target :: db
        type(column_t) :: c(1)
        character(len=:), allocatable :: buf, want(:), got
        character(len=128) :: emsg
        integer :: st, r, i, recl, wrong
        integer(int32) :: rid
        r = c_rmtree(DIR)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, DIR, stat=st)
        call db_create_table(db, 't', c, st)
        recl = db%tables(1)%record_size
        call row_alloc(buf, recl)
        allocate(character(len=recl) :: want(NROWS), got)
        do i = 1, NROWS
            call row_set_int(buf, db%tables(1)%cols(1), int(11 * i, int32))
            call db_insert(db, 't', buf, rid, st)
            call db_get(db, 't', rid, want(i), st)
        end do
        call db_begin(db, st)
        call jrnl_log_region(db, 't.dat', int(recl, int64) + 2_int64, 2_int64, stat=st)
        call check(st == SQR_OK, 'partial record: region logged')
        call row_set_int(buf, db%tables(1)%cols(1), 99_int32)
        do i = 1, NROWS
            call db_update(db, 't', int(i, int32), buf, st)
        end do
        call db_rollback(db, st)
        call check(st == SQR_OK .and. .not. jrnl_hot(db), 'partial record: rollback succeeds')
        wrong = 0
        do i = 1, NROWS
            call db_get(db, 't', int(i, int32), got, st)
            if (st /= SQR_OK .or. got /= want(i)) wrong = wrong + 1
        end do
        call check(wrong == 0, 'partial record: every row is as it was')
        call db_verify(db, 't', st, emsg)
        call check(st == SQR_OK, 'partial record: table verifies')
        call db_close(db)
    end subroutine partial_record_rollback

    ! Log the tail of a 16-byte file beyond byte 8, leave the file `left`
    ! bytes long (8 = the truncation the record guards; 16 = it never
    ! happened; 12 = a replay re-appended half and died; 4 = not a state the
    ! record describes), crash, and recover through a fresh open.
    subroutine tail_case(label, left, expect_whole)
        character(len=*), intent(in) :: label
        integer(int64),   intent(in) :: left
        logical,          intent(in) :: expect_whole
        type(db_t), target :: db, db2
        integer :: st, r
        r = c_rmtree(DIR)
        call db_open(db, DIR, stat=st)
        call jwrite_file(FULL, 'ABCDEFGHIJKLMNOP')
        call txn_begin(db, st)
        call jrnl_log_tail(db, REL, 8_int64, st)
        call check(st == SQR_OK .and. jrnl_hot(db), label // ': tail logged, journal hot')
        r = int(c_truncate(FULL, left))
        call c_lock_release(db%lock_tok)
        call jrnl_close_unit(db)
        call db_open(db2, DIR, stat=st)
        call check(st == SQR_OK .and. .not. jrnl_hot(db2), label // ': recovered, journal not hot')
        if (expect_whole) then
            call check(jfile_size(FULL) == 16_int64, label // ': length restored')
            call check(jread_file(FULL, 16) == 'ABCDEFGHIJKLMNOP', label // ': bytes restored')
        else
            call check(jfile_size(FULL) == left, label // ': left alone, no hole made')
        end if
        call db_close(db2)
    end subroutine tail_case

    ! The same record through an in-process rollback.
    subroutine tail_rollback()
        type(db_t), target :: db
        integer :: st, r
        r = c_rmtree(DIR)
        call db_open(db, DIR, stat=st)
        call jwrite_file(FULL, 'ABCDEFGHIJKLMNOP')
        call txn_begin(db, st)
        call jrnl_log_tail(db, REL, 8_int64, st)
        r = int(c_truncate(FULL, 8_int64))
        call txn_rollback(db, st)
        call check(st == SQR_OK .and. .not. jrnl_hot(db), 'tail: rollback succeeds, journal not hot')
        call check(jfile_size(FULL) == 16_int64, 'tail: rollback restores the length')
        call check(jread_file(FULL, 16) == 'ABCDEFGHIJKLMNOP', 'tail: rollback restores the bytes')
        call txn_begin(db, st)
        call jrnl_log_tail(db, REL, 16_int64, st)
        call check(st == SQR_OK .and. .not. jrnl_hot(db), 'tail: nothing beyond the cut logs nothing')
        call txn_rollback(db, st)
        call db_close(db)
    end subroutine tail_rollback

    ! ---- perturbations of the on-disk journal header ----

    subroutine perturb_none()
    end subroutine perturb_none

    subroutine perturb_magic()
        call jwrite_region(JPATH, 1_int64, 'XXXX')      ! foreign magic
    end subroutine perturb_magic

    subroutine perturb_trailing_garbage()
        ! arm_hot's one record occupies ~53 payload bytes from pos 65; the header
        ! payload_len names only those.  Write torn bytes well beyond them (still
        ! inside the 128 KiB pre-sized region) — recovery must never read past
        ! payload_len, so the valid prefix still recovers.
        call jwrite_region(JPATH, 400_int64, 'TORNTORNTORNTORN')
    end subroutine perturb_trailing_garbage

    subroutine perturb_checksum()
        call patch_i32(JPATH, 17_int64, 123456789)      ! wrong stored checksum
    end subroutine perturb_checksum

    subroutine perturb_truncate()
        integer :: r
        r = int(c_truncate(JPATH, 40_int64))            ! cut the payload region
    end subroutine perturb_truncate

    subroutine perturb_plen_huge()
        call patch_i64(JPATH, 21_int64, 9000000000_int64)   ! > huge(int32)
    end subroutine perturb_plen_huge

    subroutine perturb_plen_neg()
        call patch_i64(JPATH, 21_int64, -1_int64)
    end subroutine perturb_plen_neg

    ! ---- low-level file helpers (self-contained; mirror utest_sqr) ----

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

    subroutine patch_i32(path, off, val)
        character(len=*), intent(in) :: path
        integer(int64),   intent(in) :: off
        integer(int32),   intent(in) :: val
        integer :: u, io
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=io)
        write(u, pos=off) val
        close(u)
    end subroutine patch_i32

    subroutine patch_i64(path, off, val)
        character(len=*), intent(in) :: path
        integer(int64),   intent(in) :: off, val
        integer :: u, io
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=io)
        write(u, pos=off) val
        close(u)
    end subroutine patch_i64

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

    ! Does the journal file at `path` carry a hot header?  Read from the disk,
    ! with no handle (a refused open leaves none).
    function file_hot(path) result(hot)
        character(len=*), intent(in) :: path
        logical :: hot
        character(len=4) :: magic
        integer(int32) :: fmt, state
        integer :: u, io
        hot = .false.
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=io)
        if (io /= 0) return
        read(u, pos=1, iostat=io) magic, fmt, state
        close(u)
        hot = io == 0 .and. magic == 'SQRJ' .and. state == 1
    end function file_hot

    ! By stat, not inquire: a unit a stale (crash stand-in) handle still holds
    ! would make inquire answer from that connection, not from the disk.
    function jfile_size(path) result(n)
        character(len=*), intent(in) :: path
        integer(int64) :: n
        n = c_file_size(path)
    end function jfile_size

end program utest_journal

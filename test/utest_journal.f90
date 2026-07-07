! utest_journal — recovery against a torn / corrupt _journal.dat.
!
! The engine-armed journals are already exercised by utest_sqr (commit no-op,
! crash rollback, extend truncate).  This program covers the *review gap* the
! 2026-07-04 fault review named: journals the API never armed cleanly — a
! foreign magic, a mismatched checksum, a truncated payload, an implausible
! payload length.  In every case recovery must be SAFE: it returns SQR_OK,
! never applies garbage over the base file, never aborts inside db_open, and
! leaves the journal no longer hot.
!
! The fixture arms a real hot journal (region-capture + base overwrite) exactly
! as test_lifecycle does, releases the advisory lock to stand in for the dead
! writer, then perturbs the on-disk header before a fresh read-write open runs
! recovery.  Header layout (see txn_arm): magic[1..4] fmt[5..8] state[9..12]
! nrec[13..16] cksum[17..20] plen[21..28], payload at JHEADER+1 = 65.
program utest_journal
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use sqr
    use clib_wrap, only: c_rmtree, c_truncate, c_lock_release
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

    ! Torn corpus: recovery must be safe and leave the journal not hot.
    call one_case('foreign magic  -> no-op, safe',       perturb_magic,     .false.)
    call one_case('bad checksum   -> voided, safe',      perturb_checksum,  .false.)
    call one_case('truncated payload -> voided, safe',   perturb_truncate,  .false.)
    call one_case('huge plen (>int32) -> voided, safe',  perturb_plen_huge, .false.)
    call one_case('negative plen -> voided, safe',       perturb_plen_neg,  .false.)

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
        type(db_t) :: db
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
        type(db_t) :: db2
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

    function jfile_size(path) result(n)
        character(len=*), intent(in) :: path
        integer(int64) :: n
        integer :: io
        inquire(file=path, size=n, iostat=io)
        if (io /= 0) n = -1_int64
    end function jfile_size

end program utest_journal

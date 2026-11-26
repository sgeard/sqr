program utest_sqr
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use sqr
    use clib_wrap, only: c_rename, c_remove, c_mkdir, c_chmod, c_rmtree, c_path_exists, c_utf8_paths, &
                         c_fsync_path, c_fsync_dir, c_truncate, c_file_size, c_lock_release
    implicit none

    type :: scan_ctx_t
        integer :: count = 0
        integer :: sum   = 0
        type(column_t) :: age_col
    end type

    type :: scan_mut_ctx_t
        integer :: count = 0
        integer :: sum   = 0
        integer :: bad   = 0
        integer :: record_size = 0
        type(column_t) :: cols(2)
    end type

    type :: scan_text_ctx_t
        integer :: count = 0
        character(len=:), allocatable :: joined
    end type

    integer :: pass = 0, fail = 0, skipped = 0
    character(len=*), parameter :: TEST_DIR = 'utest_sqr_db'

    call cleanup_dir()

    call test_row_helpers()
    call test_empty_open_close()
    call test_create_table_and_insert()
    call test_get_and_scan()
    call test_delete()
    call test_persistence()
    call test_real_table_and_scan_stop()
    call test_index_int()
    call test_index_char()
    call test_index_real_nan()
    call test_range_cursor()
    call test_cursor_invalidation()
    call test_update()
    call test_api_error_paths()
    call test_validation()
    call test_nested_dir()
    call test_readonly()
    call test_drop_table()
    call test_drop_marker()
    call test_open_corruption_paths()
    call test_catalog_traversal_name()
    call test_crash_recovery()
    call test_durable_off()
    call test_version_check()
    call test_schema_corruption()
    call test_text()
    call test_high_findings()
    call test_large_blob()
    call test_compact()
    call test_compact_recovery()
    call test_deep_tree()
    call test_torn_journal()
    call test_schema_next_id()
    call test_corrupt_index()
    call test_pack()
    call test_pack_too_large()
    call test_quiesce()
    call test_resume_retry()
    call test_handle_misuse()
    call test_history_kept_on_refusal()
    call test_natural_keys()
    call test_null_columns()
    call test_null_in_keys()
    call test_format_upgrade()
    call test_leading_column()
    call test_find_by_leading()
    call test_drop_index()
    call test_db_verify()
    call test_verify_dup_split_by_dead()
    call test_list_tables_closed()
    call test_create_table_cleanup()
    call test_record_size()
    call test_strict_write_length()
    call test_row_id_exhaustion()
    call test_readonly_media_lock()
    call test_clean_close()
    call test_engine_errmsg()
    call test_wide_table()
    call test_scan_empty()
    call test_scan_mutation()
    call test_name_boundary()
    call test_index_reopen_after_removes()
    call test_idx_live_direct()
    call test_insert_many()
    call test_alter_column()
    call test_reshape_table()
    call test_alter_recovery()
    call test_alter_first_text()
    call test_oo_bindings()
    call test_coverage_paths()
    call test_clib_wrappers()
    call test_journal()
    call test_journal_adapter()
    call test_lifecycle()
    call test_txn_index_hooks()
    call test_txn_record_rollback()
    call test_txn_insert_many_rollback()
    call test_explicit_txn()
    call test_doomed_txn()
    call test_reserved_names()
    call test_table_name_rules()
    call test_close_rolls_back_open_txn()
    call test_txn_structural_block()
    call test_locking()
    call test_set_readonly()
    call test_set_readonly_counters()
    call test_accessors()
    call test_last_error()
    call test_real_text()

    call cleanup_dir()

    if (skipped > 0) then
        print '(a,i0,a,i0,a,i0,a)', 'sqr tests: ', pass, ' passed, ', fail, ' failed, ', skipped, ' skipped'
    else
        print '(a,i0,a,i0,a)', 'sqr tests: ', pass, ' passed, ', fail, ' failed'
    end if
    if (fail > 0) error stop 1

contains

    ! The in-process crash stand-in's file teardown: close every unit the
    ! handle holds (data, blob, index trees, journal) WITHOUT flushing state
    ! back through sqr, as a dead process's files are closed by the OS.  A
    ! recovering open in this same process must not meet a file still
    ! connected - a second connection is processor dependent (flang refuses).
    subroutine crash_units(db)
        type(db_t), intent(inout) :: db
        integer :: ti, j, ios
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                if (t%unit /= -1)      close(t%unit, iostat=ios)
                if (t%blob_unit /= -1) close(t%blob_unit, iostat=ios)
                t%unit = -1;  t%blob_unit = -1
                do j = 1, t%nindices
                    if (t%indices(j)%bt%unit /= -1) close(t%indices(j)%bt%unit, iostat=ios)
                    t%indices(j)%bt%unit = -1
                end do
            end associate
        end do
        call jrnl_close_unit(db)
        db%opened = .false.      ! the process is gone, and its handle with it
    end subroutine crash_units

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
    end subroutine

    ! Record a check that cannot be made on this platform.  It counts as
    ! neither a pass nor a failure and is named in the output and the total.
    subroutine skip(label, why)
        character(len=*), intent(in) :: label, why
        skipped = skipped + 1
        print '(a,a,a,a,a)', '  SKIP ', label, ' (', why, ')'
    end subroutine

    ! ---- Permission-based tests ------------------------------------------
    ! test_readonly_media_lock and test_clean_close imitate read-only media
    ! with chmod: 444 on the lock file, 555 on the database directory.  They
    ! are only meaningful where the operating system then refuses the write.
    ! It does not in two cases:
    !   * Windows (and so wine, where the cross-built utest_sqr.exe is run):
    !     a directory has no write-permission bit for chmod to clear, so
    !     files can still be created in a "555" directory;
    !   * a process running as root (the wine container runs as root):
    !     permission bits are not checked at all.
    ! The store is behaving correctly in both; it is the test's stand-in for
    ! read-only media that has no effect.  So each test probes, straight
    ! after its chmod, whether the restriction took hold, and skips the
    ! checks that depend on it when it did not.  First seen 2026-11-13:
    ! three failures in utest_sqr.exe under wine in podman.

    ! The probes go through the C library, as the store's own file
    ! operations do, and not through Fortran OPEN, whose treatment of a
    ! read-only file differs between compilers.

    ! Can `path` still be written?  Truncating a file to the length it
    ! already has changes nothing but needs write permission.
    function file_still_writable(path) result(yes)
        character(len=*), intent(in) :: path
        logical :: yes
        yes = c_truncate(path, c_file_size(path)) == 0
    end function

    ! Can a new entry still be created in directory `dir`?
    function dir_accepts_new_entry(dir) result(yes)
        character(len=*), intent(in) :: dir
        logical :: yes
        integer :: ios
        yes = c_mkdir(dir // '/_permprobe') == 0
        if (yes) ios = c_remove(dir // '/_permprobe')
    end function

    subroutine cleanup_dir()
        integer :: ios
        ios = c_rmtree(TEST_DIR)
    end subroutine

    ! Exercise the durability wrappers added for the Phase 2 journal:
    ! fsync a file and the cwd, truncate a file to a smaller length, and
    ! confirm a missing path is reported as a failure rather than a success.
    subroutine test_clib_wrappers()
        character(len=*), parameter :: f = 'utest_clib.tmp'
        character(len=100) :: buf
        integer            :: u, ios
        integer(int64)     :: sz
        buf = ''
        open(newunit=u, file=f, access='stream', form='unformatted', &
             status='replace', iostat=ios)
        write(u) buf                          ! 100 bytes on disk
        close(u)
        call check(c_fsync_path(f) == 0,                  'c_fsync_path on existing file')
        call check(c_fsync_dir('.') == 0,                 'c_fsync_dir on cwd')
        call check(c_truncate(f, 40_int64) == 0,          'c_truncate shrink to 40')
        inquire(file=f, size=sz)
        call check(sz == 40_int64,                        'file is 40 bytes after truncate')
        call check(c_file_size(f) == 40_int64,            'c_file_size agrees with inquire')
        call check(c_file_size('no_such_file.xyz') == -1_int64, 'c_file_size on missing file is -1')
        call check(c_fsync_path('no_such_file.xyz') /= 0, 'c_fsync_path on missing file fails')
        call check(c_truncate('no_such_file.xyz', 8_int64) /= 0, &
                                                          'c_truncate on missing file fails')
        ios = c_remove(f)
        ! c_rmtree on a plain file removes it, as on POSIX (A8, Windows branch)
        open(newunit=u, file=f, status='replace', iostat=ios)
        write(u, '(a)') 'x'
        close(u)
        call check(c_rmtree(f) == 0,                      'c_rmtree on a plain file returns 0')
        call check(.not. c_path_exists(f),                'c_rmtree removed the plain file')
    end subroutine

    ! Drive the rollback journal directly (no engine wiring yet): a committed
    ! transaction leaves its new bytes; an interrupted one is rolled back by
    ! recovery; an append is truncated away; duplicate regions log once.
    subroutine test_journal()
        type(db_t), target :: db
        character(len=*), parameter   :: REL = 'jtest.bin'
        character(len=:), allocatable :: full
        integer :: st
        call db_open(db, TEST_DIR, stat=st)
        call check(st == SQR_OK, 'journal: db_open')
        full = trim(db%dir) // '/' // REL

        ! Commit path: overwrite [5..8], commit, then recovery must be a no-op.
        call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=st)
        call txn_arm(db, st)
        call check(st == SQR_OK, 'journal: arm (commit path)')
        call jwrite_region(full, 5_int64, 'BBBB')
        call txn_commit(db, st)
        call jrnl_recover(db, st)
        call check(jread_file(full, 16) == 'AAAABBBBAAAAAAAA', 'journal: commit keeps new bytes')

        ! Crash path: overwrite, do NOT commit, recover -> original restored.
        call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=st)
        call txn_arm(db, st)
        call jwrite_region(full, 5_int64, 'CCCC')
        call jrnl_recover(db, st)
        call check(st == SQR_OK, 'journal: recover ok')
        call check(jread_file(full, 16) == 'AAAAAAAAAAAAAAAA', 'journal: recovery restores original')

        ! Extend rollback: grow the file, recovery truncates back.
        call jwrite_file(full, 'HELLO')
        call txn_begin(db, st)
        call jrnl_log_extend(db, REL, st)
        call txn_arm(db, st)
        call jwrite_region(full, 6_int64, 'WORLD')
        call jrnl_recover(db, st)
        call check(jfile_size(full) == 5_int64, 'journal: extend rollback truncates')

        ! Idempotency: an identical region is captured only once.
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 1_int64, 3_int64, stat=st)
        call jrnl_log_region(db, REL, 1_int64, 3_int64, stat=st)
        call check(db%jrnl%nrec == 1, 'journal: duplicate region logged once')
        call txn_rollback(db, st)

        ! Same-process rollback restores from the in-memory undo set.
        call jwrite_file(full, 'XXXXXXXXXX')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 1_int64, 5_int64, stat=st)
        call txn_arm(db, st)
        call jwrite_region(full, 1_int64, 'YYYYY')
        call txn_rollback(db, st)
        call check(jread_file(full, 10) == 'XXXXXXXXXX', 'journal: txn_rollback restores')

        ! Caller-supplied pre-image: the journal must store the bytes handed in
        ! (the caller's consistent view), not a re-read of the file.  The on-disk
        ! region is 'XXXXX' but we declare the pre-image 'QQQQQ'; the dummy length
        ! is ignored in favour of len(bytes).  Rollback must restore 'QQQQQ'.
        call jwrite_file(full, 'XXXXXXXXXX')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 1_int64, 0_int64, bytes='QQQQQ', stat=st)
        call check(st == SQR_OK, 'journal: log region with supplied bytes')
        call txn_arm(db, st)
        call jwrite_region(full, 1_int64, 'YYYYY')
        call txn_rollback(db, st)
        call check(jread_file(full, 10) == 'QQQQQXXXXX', 'journal: rollback uses supplied pre-image')

        ! A read-only handle refuses to begin a transaction.
        call db_close(db)
        call db_open(db, TEST_DIR, stat=st, readonly=.true.)
        call txn_begin(db, st)
        call check(st == SQR_READONLY, 'journal: txn_begin on read-only fails')
        call db_close(db)
    end subroutine

    ! bt_journal_adapter: the bridge a B+-tree's pre-write hook drives.  Exercise
    ! both branches it dispatches without a real tree — an in-place overwrite
    ! (is_new=.false., supplied pre-image) and a new page (is_new=.true.) — plus
    ! its refusal of a foreign context.
    subroutine test_journal_adapter()
        type(db_t), target :: db
        type(bt_jhook_ctx_t) :: cx
        character(len=*), parameter   :: REL = 'jadapt.bin'
        character(len=:), allocatable :: full
        integer :: st, bs, dummy
        call db_open(db, TEST_DIR, stat=st)
        call check(st == SQR_OK, 'adapter: db_open')
        full = trim(db%dir) // '/' // REL
        cx%db => db
        cx%rel = REL

        ! Region branch: the adapter stores the *supplied* pre-image ('aaaa' —
        ! the tree's own consistent view), not a re-read of the on-disk 'AAAA',
        ! so rollback restores those exact bytes at the page offset.
        call jwrite_file(full, 'AAAAAAAAAA')
        call txn_begin(db, st)
        call bt_journal_adapter(cx, 3_int64, 'aaaa', .false., bs)
        call check(bs == 0, 'adapter: region hook reports ok')
        call jwrite_region(full, 3_int64, 'bbbb')
        call txn_rollback(db, st)
        call check(jread_file(full, 10) == 'AAaaaaAAAA', 'adapter: region rollback uses pre-image')

        ! New-page branch: is_new logs an extend of the file's pre-growth length,
        ! and rollback truncates the appended bytes away.
        call jwrite_file(full, 'HELLO')
        call txn_begin(db, st)
        call bt_journal_adapter(cx, 0_int64, '', .true., bs)
        call check(bs == 0, 'adapter: extend hook reports ok')
        call jwrite_region(full, 6_int64, 'WORLD')
        call txn_rollback(db, st)
        call check(jfile_size(full) == 5_int64, 'adapter: extend rollback truncates')

        ! A context of the wrong type must abort the write rather than silently
        ! let an un-recorded overwrite through.
        dummy = 0
        call bt_journal_adapter(dummy, 0_int64, '', .false., bs)
        call check(bs /= 0, 'adapter: foreign context refused')
        call db_close(db)
    end subroutine

    ! db_open / db_close journal lifecycle: a hot journal left by a "crash"
    ! is rolled back by the next read-write open and refused by a read-only
    ! open; a clean close deletes the (voided) journal file.
    subroutine test_lifecycle()
        type(db_t), target :: db, db2
        character(len=*), parameter   :: REL  = 'lc.bin'
        character(len=*), parameter   :: JPATH = TEST_DIR // '/_journal.dat'
        character(len=:), allocatable :: full
        integer :: st

        ! Arm a journal that captured [5..8], perform the base write, then
        ! neither commit nor close: a crash leftover with a hot journal.
        call db_open(db, TEST_DIR, stat=st)
        full = trim(db%dir) // '/' // REL
        call jwrite_file(full, 'AAAAAAAAAAAAAAAA')
        call txn_begin(db, st)
        call jrnl_log_region(db, REL, 5_int64, 4_int64, stat=st)
        call txn_arm(db, st)
        call jwrite_region(full, 5_int64, 'ZZZZ')
        call check(jrnl_hot(db), 'lifecycle: armed journal reads hot')

        ! Simulate the crash faithfully: a dead process releases its advisory
        ! lock (the OS drops it), leaving only the hot journal on disk.  Drop
        ! the stale handle's lock so the recovering open below is not blocked
        ! by this same-process stand-in for the crashed writer.  A dead process
        ! also closes every file it held (crash_units).
        call c_lock_release(db%lock_tok)
        call crash_units(db)

        ! A read-only open refuses while the journal is hot.
        call db_open(db2, TEST_DIR, stat=st, readonly=.true.)
        call check(st == SQR_READONLY, 'lifecycle: read-only open refuses hot journal')

        ! A read-write open recovers: the region is restored, journal voided.
        call db_open(db2, TEST_DIR, stat=st)
        call check(st == SQR_OK, 'lifecycle: read-write open recovers')
        call check(jread_file(full, 16) == 'AAAAAAAAAAAAAAAA', &
                   'lifecycle: open restored pre-crash bytes')
        call check(.not. jrnl_hot(db2), 'lifecycle: journal no longer hot after recovery')

        ! A clean read-write close deletes the journal file.
        call db_close(db2)
        call check(jfile_size(JPATH) == -1_int64, 'lifecycle: clean close removes the journal')

        call db_close(db)   ! release the stale handle's units (journal already gone)
    end subroutine

    ! txn_begin installs the rollback-journal hook on every live index tree, so a
    ! transaction's B+-tree page writes feed the undo set; commit clears the
    ! hooks leaving the advanced index intact; rollback restores the index files
    ! and re-syncs the open tree handles (bt_reload) so they stop caching the
    ! rolled-forward state.  This drives the *index* dimension; the whole-row
    ! .dat/.blob rollback (and db_verify) is exercised in test_txn_record_rollback.
    subroutine test_txn_index_hooks()
        character(len=*), parameter :: HDIR = TEST_DIR // '_hooks'
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(HDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, HDIR, rs, emsg)
        call db_create_table(db, 'h', c, rs, emsg)
        ti = db_table_index(db, 'h')
        ! Baseline rows + an index over them, all outside any transaction.
        do i = 1, 40
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 'h', buf, rid, rs)
        end do
        call db_create_index(db, 'h', 'id', rs)
        call check(rs == SQR_OK, 'hooks: baseline table + index built')

        ! --- commit path: inserts inside a txn advance both index and journal ---
        call txn_begin(db, rs)
        call check(rs == SQR_OK .and. associated(db%tables(ti)%indices(1)%jctx), &
                   'hooks: txn_begin installs the index hook')
        do i = 101, 160
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 'h', buf, rid, rs)
        end do
        call check(db%jrnl%nrec > 0, 'hooks: index page writes captured undo records')
        call txn_commit(db, rs)
        call check(rs == SQR_OK .and. .not. db%jrnl%active, 'hooks: commit ends the txn')
        call check(.not. associated(db%tables(ti)%indices(1)%jctx), &
                   'hooks: commit clears the index hook context')
        call db_find_by_int(db, 'h', 'id', 130_int32, rid, rs)
        call check(rs == SQR_OK, 'hooks: committed key still indexed')

        ! --- rollback path: the index returns to its pre-txn entries ---
        call txn_begin(db, rs)
        do i = 201, 260
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 'h', buf, rid, rs)
        end do
        call txn_rollback(db, rs)
        call check(rs == SQR_OK, 'hooks: rollback succeeds')
        call check(.not. associated(db%tables(ti)%indices(1)%jctx), &
                   'hooks: rollback clears the index hook context')
        ! The reloaded handle is usable: keys that pre-date the rolled-back txn
        ! are still indexed; a key the txn added is gone from the index again.
        call db_find_by_int(db, 'h', 'id', 20_int32, rid, rs)
        call check(rs == SQR_OK, 'hooks: pre-txn baseline key survives rollback')
        call db_find_by_int(db, 'h', 'id', 130_int32, rid, rs)
        call check(rs == SQR_OK, 'hooks: earlier committed key survives rollback')
        call db_find_by_int(db, 'h', 'id', 230_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'hooks: rolled-back key absent from the index')

        call db_close(db)
        ios = c_rmtree(HDIR)
    end subroutine

    ! A transaction's row mutations — insert, in-place update, delete, and a
    ! TEXT write that appends to the blob — are all captured in the undo journal
    ! (records via region pre-images, appends via extend), so txn_rollback
    ! restores the .dat and .blob byte-for-byte AND reverts the in-memory
    ! counters (next_id/live_count/blob_next).  db_verify — which recounts live
    ! rows from the file and walks each index against it — then passes and every
    ! pre-txn value is back; a committed transaction keeps its changes.
    subroutine test_txn_record_rollback()
        character(len=*), parameter :: RDIR = TEST_DIR // '_recroll'
        type(db_t), target :: db
        type(column_t)     :: c(2)
        character(len=:), allocatable :: buf, txt
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(RDIR)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'note'; c(2)%dtype = DT_TEXT ; c(2)%csize = SQR_TEXT_DESC
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'r', c, rs, emsg)
        ti = db_table_index(db, 'r')
        ! Five baseline rows, each with blob text, indexed on id — all outside a
        ! transaction (so the journal is dormant and nothing is captured).
        do i = 1, 5
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 'r', buf, rid, rs)
            call db_set_text(db, 'r', rid, 'note', 'base', rs)
        end do
        call db_create_index(db, 'r', 'id', rs)
        call db_verify(db, 'r', rs, emsg)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == 5, &
                   'recroll: baseline of 5 rows verifies')

        ! --- rollback path: a mix of mutations, then unwind ---
        call txn_begin(db, rs)
        call row_alloc(buf, db%tables(ti)%record_size)         ! insert -> .dat extend
        call row_set_int(buf, db%tables(ti)%cols(1), 100_int32)
        call db_insert(db, 'r', buf, rid, rs)
        call check(rs == SQR_OK, 'recroll: in-txn insert ok')
        call row_alloc(buf, db%tables(ti)%record_size)         ! update -> record region
        call row_set_int(buf, db%tables(ti)%cols(1), 33_int32)
        call db_update(db, 'r', 3_int32, buf, rs)
        call check(rs == SQR_OK, 'recroll: in-txn update ok')
        call db_delete(db, 'r', 4_int32, rs)                   ! delete -> record region
        call check(rs == SQR_OK, 'recroll: in-txn delete ok')
        call db_set_text(db, 'r', 1_int32, 'note', 'a-much-longer-note', rs)  ! blob extend + region
        call check(rs == SQR_OK, 'recroll: in-txn set_text ok')
        call check(db%jrnl%nrec > 0, 'recroll: mutations captured undo records')
        call txn_rollback(db, rs)
        call check(rs == SQR_OK, 'recroll: rollback succeeds')

        ! Files restored byte-for-byte and counters reverted: db_verify proves
        ! the data file matches the (snapshot-restored) live_count.
        call db_verify(db, 'r', rs, emsg)
        call check(rs == SQR_OK, 'recroll: db_verify passes after rollback')
        call check(db%tables(ti)%live_count == 5, 'recroll: live_count restored to 5')
        call check(db%tables(ti)%next_id == 6, 'recroll: next_id restored to 6')
        call db_find_by_int(db, 'r', 'id', 100_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'recroll: inserted row gone from the index')
        call db_find_by_int(db, 'r', 'id', 33_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'recroll: updated key 33 reverted')
        call db_find_by_int(db, 'r', 'id', 3_int32, rid, rs)
        call check(rs == SQR_OK, 'recroll: original key 3 restored')
        call db_find_by_int(db, 'r', 'id', 4_int32, rid, rs)
        call check(rs == SQR_OK, 'recroll: deleted row 4 alive again')
        call db_get_text(db, 'r', 1_int32, 'note', txt, rs)
        call check(rs == SQR_OK .and. txt == 'base', 'recroll: row 1 text reverted')

        ! --- commit path: the same shape of change, this time kept ---
        call txn_begin(db, rs)
        call db_delete(db, 'r', 5_int32, rs)
        call db_set_text(db, 'r', 2_int32, 'note', 'committed-text', rs)
        call txn_commit(db, rs)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == 4, &
                   'recroll: commit keeps the delete (live_count 4)')
        call db_get_text(db, 'r', 2_int32, 'note', txt, rs)
        call check(rs == SQR_OK .and. txt == 'committed-text', &
                   'recroll: commit keeps the new text')
        call db_verify(db, 'r', rs, emsg)
        call check(rs == SQR_OK, 'recroll: db_verify passes after commit')

        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine

    ! db_insert_many maintains its indices with a deferred *packed* rebuild
    ! (rebuild_index: truncate + bulk-load), not the incremental hooked insert the
    ! single-row path uses.  Under a transaction that truncate would otherwise be
    ! invisible to the journal, so rebuild_index captures the whole index file
    ! up-front (extend + offset-0 region) before truncating.  This proves the
    ! capture works: a batch inside an explicit txn rolls back to leave the data
    ! file AND every index byte-exact (db_verify walks each index against a full
    ! scan), with the batched keys gone and the pre-txn keys intact.
    subroutine test_txn_insert_many_rollback()
        character(len=*), parameter :: MDIR = TEST_DIR // '_imroll'
        type(db_t), target :: db
        type(column_t)     :: c(2)
        character(len=:), allocatable :: bufs(:)
        integer        :: rs, ti, k, rsz, ios
        integer(int32) :: rid, rids(3)
        character(len=128) :: emsg
        ios = c_rmtree(MDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'sku'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, MDIR, rs, emsg)
        call db_create_table(db, 'm', c, rs, emsg)
        ti = db_table_index(db, 'm')
        call db_create_index(db, 'm', 'sku', rs, unique=.true.)
        rsz = db%tables(ti)%record_size

        ! Baseline batch of four rows, indexed on sku, outside any transaction.
        allocate(character(len=rsz) :: bufs(4))
        do k = 1, 4
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), int(100 + k, int32))
        end do
        block
            integer(int32) :: brids(4)
            call db_insert_many(db, 'm', bufs, brids, rs)
        end block
        call db_verify(db, 'm', rs, emsg)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == 4, &
                   'imroll: baseline batch of 4 verifies')
        deallocate(bufs)

        ! --- rollback path: a second batch inside an explicit txn, then unwind ---
        allocate(character(len=rsz) :: bufs(3))
        do k = 1, 3
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(200 + k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), int(200 + k, int32))
        end do
        call txn_begin(db, rs)
        call db_insert_many(db, 'm', bufs, rids, rs)
        call check(rs == SQR_OK, 'imroll: in-txn insert_many ok')
        call check(db%jrnl%nrec > 0, 'imroll: packed rebuild captured undo records')
        call db_find_by_int(db, 'm', 'sku', 202_int32, rid, rs)
        call check(rs == SQR_OK, 'imroll: batched key indexed mid-txn')
        call txn_rollback(db, rs)
        call check(rs == SQR_OK, 'imroll: rollback succeeds')

        ! Data file and every index restored byte-for-byte; counters reverted.
        call db_verify(db, 'm', rs, emsg)
        call check(rs == SQR_OK, 'imroll: db_verify passes after rollback')
        call check(db%tables(ti)%live_count == 4, 'imroll: live_count restored to 4')
        call check(db%tables(ti)%next_id == 5, 'imroll: next_id restored to 5')
        call db_find_by_int(db, 'm', 'sku', 202_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'imroll: batched key gone from the index')
        call db_find_by_int(db, 'm', 'sku', 103_int32, rid, rs)
        call check(rs == SQR_OK, 'imroll: pre-txn key 103 survives rollback')

        call db_close(db)
        ios = c_rmtree(MDIR)
    end subroutine

    ! The public explicit transaction façade (db_begin/commit/rollback, what SQL
    ! BEGIN/COMMIT/ROLLBACK will drive).  Checks the no-nesting guard, that
    ! commit/rollback without an open explicit txn are rejected, and that a
    ! multi-op explicit transaction is atomic both ways — committed changes kept,
    ! rolled-back changes fully reverted (files + counters via db_verify).
    ! A transaction doomed by db_doom_txn refuses writes, still reads, and is
    ! rolled back by db_commit (review findings Q1 / D4).
    subroutine test_doomed_txn()
        character(len=*), parameter :: XDIR = TEST_DIR // '_doomed'
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf, got, lbl
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        integer(int64) :: mark
        character(len=128) :: emsg
        ios = c_rmtree(XDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, XDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call db_create_index(db, 't', 'id', rs, unique=.true.)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_alloc(got, db%tables(ti)%record_size)
        do i = 1, 3
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do

        ! Outside a transaction there is nothing to doom.
        call db_doom_txn(db)
        call check(.not. db_txn_doomed(db), 'doomed: no effect outside a transaction')
        call check(db_txn_mark(db) == 0_int64, 'doomed: mark is 0 outside a transaction')

        ! A statement that fails before it writes does not doom.
        call db_begin(db, rs)
        mark = db_txn_mark(db)
        call row_set_int(buf, db%tables(ti)%cols(1), 2_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_DUP, 'doomed: a duplicate is refused')
        call db_doom_txn(db, mark)
        call check(.not. db_txn_doomed(db), 'doomed: a clean failure leaves the transaction usable')
        call row_set_int(buf, db%tables(ti)%cols(1), 4_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_OK, 'doomed: the transaction still accepts a write')
        call db_commit(db, rs)
        call check(rs == SQR_OK .and. db_row_count(db, 't') == 4, 'doomed: and commits')

        ! A statement that wrote and then failed dooms.
        call db_begin(db, rs)
        mark = db_txn_mark(db)
        call row_set_int(buf, db%tables(ti)%cols(1), 5_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(db_txn_mark(db) > mark, 'doomed: a write moves the mark')
        call db_doom_txn(db, mark)
        call check(db_txn_doomed(db), 'doomed: a failure after a write dooms')
        call row_set_int(buf, db%tables(ti)%cols(1), 6_int32)
        emsg = ''
        call db_insert(db, 't', buf, rid, rs, emsg)
        call check(rs == SQR_INVALID .and. rid == 0, 'doomed: insert refused')
        call check(index(emsg, 'roll it back') > 0, 'doomed: the refusal says why')
        call db_update(db, 't', 1_int32, buf, rs)
        call check(rs == SQR_INVALID, 'doomed: update refused')
        call db_delete(db, 't', 1_int32, rs)
        call check(rs == SQR_INVALID, 'doomed: delete refused')
        call db_get(db, 't', 1_int32, got, rs)
        call check(rs == SQR_OK, 'doomed: reads still work')
        call check(db_txn_doomed(db), 'doomed: a refused write leaves it doomed')
        emsg = ''
        call db_commit(db, rs, emsg)
        call check(rs == SQR_INVALID, 'doomed: commit is refused')
        call check(index(emsg, 'rolled back') > 0, 'doomed: commit says it rolled back')
        call check(.not. db_in_txn(db), 'doomed: the transaction is over')
        call check(db_row_count(db, 't') == 4, 'doomed: nothing it wrote survives')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'doomed: table verifies after the rollback')

        ! The next transaction starts clean, and ROLLBACK clears a doomed one.
        call db_begin(db, rs)
        call check(.not. db_txn_doomed(db), 'doomed: the next transaction is not doomed')
        call db_doom_txn(db)
        call check(db_txn_doomed(db), 'doomed: unconditional doom')
        call db_rollback(db, rs)
        call check(rs == SQR_OK .and. .not. db_txn_doomed(db), 'doomed: rollback ends it')

        ! A doomed gesture records no history step.
        call db_begin(db, rs, label='G')
        call row_set_int(buf, db%tables(ti)%cols(1), 7_int32)
        call db_insert(db, 't', buf, rid, rs)
        call db_doom_txn(db)
        call db_commit(db, rs)
        call check(rs == SQR_INVALID .and. .not. db_can_undo(db), &
                   'doomed: a doomed gesture leaves no undo step')
        call db_undo(db, rs, lbl)
        call check(rs == SQR_NO_UNDO, 'doomed: nothing to undo')
        call db_close(db)
        ios = c_rmtree(XDIR)
    end subroutine

    ! Review finding D1: a table may not take a name in the engine's namespace
    ! (a leading '_'), since its files would be the engine's own.  The rule is
    ! applied on creation only: an existing table with such a name still opens.
    subroutine test_reserved_names()
        character(len=*), parameter :: XDIR = TEST_DIR // '_reserved'
        character(len=*), parameter :: BAD(4) = [character(len=8) :: '_journal', '_catalog', '_lock', '_x']
        character(len=*), parameter :: EXT(2) = [character(len=7) :: '.dat', '.schema']
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf, cat
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        integer        :: rs, i, ios, u, p
        integer(int32) :: rid
        integer(int64) :: sz
        character(len=128) :: emsg
        ios = c_rmtree(XDIR)
        c(1)%name = '_id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, XDIR, rs, emsg)
        do i = 1, size(BAD)
            call db_create_table(db, trim(BAD(i)), c, rs, emsg)
            call check(rs == SQR_INVALID, 'reserved: "' // trim(BAD(i)) // '" refused')
            call check(.not. c_path_exists(XDIR // '/' // trim(BAD(i)) // '.schema'), &
                       'reserved: "' // trim(BAD(i)) // '" left no file')
        end do
        ! An underscore elsewhere, and in a column name, is ordinary.
        call db_create_table(db, 'notes_', c, rs, emsg)
        call check(rs == SQR_OK, 'reserved: a trailing underscore is allowed')
        call db_create_table(db, 'xnotes', c, rs, emsg)
        call check(rs == SQR_OK, 'reserved: a column may begin with an underscore')
        call row_alloc(buf, db_record_size(db, 'xnotes'))
        call row_set_int(buf, db%tables(db_table_index(db, 'xnotes'))%cols(1), 7_int32)
        call db_insert(db, 'xnotes', buf, rid, rs)
        call db_close(db, rs)
        call check(rs == SQR_OK, 'reserved: database closes cleanly after the refusals')
        call db_open(db, XDIR, rs, emsg)
        call check(rs == SQR_OK, 'reserved: and reopens')
        call db_verify(db, 'xnotes', rs, emsg)
        call check(rs == SQR_OK, 'reserved: and verifies')
        call db_close(db, rs)

        ! A database made before the rule: rename xnotes to _notes behind the
        ! engine's back (same length, so the catalog entry is patched in place).
        do i = 1, size(EXT)
            ios = c_rename(XDIR // '/xnotes' // trim(EXT(i)), XDIR // '/_notes' // trim(EXT(i)))
        end do
        inquire(file=XDIR // '/_catalog.dat', size=sz)
        allocate(character(len=int(sz)) :: cat)
        open(newunit=u, file=XDIR // '/_catalog.dat', access='stream', form='unformatted', &
             status='old', action='readwrite')
        read(u) cat
        p = index(cat, 'xnotes')
        call check(p > 0, 'reserved: catalog entry found')
        if (p > 0) write(u, pos=p) '_notes'
        close(u)
        call db_open(db, XDIR, rs, emsg)
        call check(rs == SQR_OK, 'reserved: an existing "_notes" table still opens')
        call db_list_tables(db, names)
        call check(any(names == '_notes'), 'reserved: and is listed')
        call db_get(db, '_notes', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'reserved: and is readable')
        call db_insert(db, '_notes', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'reserved: and writable')
        call db_close(db, rs)
        ios = c_rmtree(XDIR)
    end subroutine

    ! A new table's name: ASCII letters and digits, space and _ - ! + . , only,
    ! no space at either end, no dot at the end, and not the name of an
    ! existing table in another case.  An existing table is exempt.
    subroutine test_table_name_rules()
        character(len=*), parameter :: XDIR = TEST_DIR // '_names'
        ! UTF-8: e-acute (2 bytes), its capital, a CJK character (3), an emoji (4).
        character(len=*), parameter :: E_ACUTE = char(195) // char(169)
        character(len=*), parameter :: E_ACUTE_UC = char(195) // char(137)
        character(len=*), parameter :: CJK = char(230) // char(151) // char(165)
        character(len=*), parameter :: EMOJI = char(240) // char(159) // char(152) // char(128)
        character(len=*), parameter :: GOOD(8) = [character(len=12) :: &
            'Plain9', 'two words', 'a-b', 'a!b', 'a+b', 'a.b', 'a,b', '.hidden']
        ! Good too, wherever a UTF-8 file name reaches the OS as UTF-8; the
        ! last begins with the first character after the C1 controls.
        character(len=*), parameter :: GOOD_UTF8(5) = [character(len=12) :: &
            'caf' // E_ACUTE, 'CAF' // E_ACUTE_UC, CJK // CJK, 'x' // EMOJI, &
            'a' // char(194) // char(160)]
        character(len=*), parameter :: BAD(11) = [character(len=12) :: &
            'a:b', 'a*b', 'a?b', 'a"b', 'a<b', 'a>b', 'a|b', 'a#b', &
            ' lead', 'trail.', 'a..b']
        ! Not UTF-8: a Latin-1 byte, a cut-off sequence, a stray continuation
        ! byte, an overlong form, a surrogate, past U+10FFFF, an impossible lead.
        character(len=*), parameter :: NOT_UTF8(8) = [character(len=12) :: &
            'caf' // char(233), 'caf' // char(195), 'a' // char(169) // 'b', &
            char(192) // char(128), char(224) // char(128) // char(128), &
            char(237) // char(160) // char(128), &
            char(244) // char(144) // char(128) // char(128), char(245) // 'abc']
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf, cat
        integer        :: rs, i, ios, u, p
        integer(int32) :: rid
        integer(int64) :: sz
        logical        :: folds
        character(len=128) :: emsg
        ios = c_rmtree(XDIR)
        folds = .false.
        c(1)%name = 'a:b'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, XDIR, rs, emsg)
        do i = 1, size(GOOD)
            call db_create_table(db, trim(GOOD(i)), c, rs, emsg)
            call check(rs == SQR_OK, 'names: "' // trim(GOOD(i)) // '" accepted')
        end do
        ! Windows takes a file name in the process's code page: without the
        ! UTF-8 one (src/sqr_utf8.manifest) a non-ASCII name is refused.
        do i = 1, size(GOOD_UTF8)
            emsg = ''
            call db_create_table(db, trim(GOOD_UTF8(i)), c, rs, emsg)
            if (c_utf8_paths() .and. i == 2) then
                ! Windows and macOS hold "CAFÉ" to be the file "café": there
                ! the name is refused and the first table is left whole.
                if (folds) then
                    call check(rs == SQR_DUP .and. index(emsg, 'files already exist') > 0, &
                               'names: a name the file system folds onto another is refused')
                    call check(c_path_exists(XDIR // '/caf' // E_ACUTE // '.schema') .and. &
                               db_record_size(db, 'caf' // E_ACUTE) > 0, &
                               'names: and the table that owns the files is untouched')
                else
                    call check(rs == SQR_OK, 'names: "' // trim(GOOD_UTF8(i)) // '" accepted')
                end if
            else if (c_utf8_paths()) then
                call check(rs == SQR_OK, 'names: "' // trim(GOOD_UTF8(i)) // '" accepted')
                if (i == 1) folds = c_path_exists(XDIR // '/CAF' // E_ACUTE_UC // '.dat')
            else
                call check(rs == SQR_INVALID .and. index(emsg, 'UTF-8 code page') > 0 &
                           .and. db_table_index(db, trim(GOOD_UTF8(i))) == 0, &
                           'names: non-ASCII name refused outside the UTF-8 code page (case ' &
                           // achar(48 + i) // ')')
            end if
        end do
        do i = 1, size(BAD)
            emsg = ''
            call db_create_table(db, trim(BAD(i)), c, rs, emsg)
            call check(rs == SQR_INVALID .and. index(emsg, 'invalid table name') > 0, &
                       'names: "' // trim(BAD(i)) // '" refused')
        end do
        emsg = ''
        call db_create_table(db, 'a*b', c, rs, emsg)
        call check(index(emsg, 'only letters, digits') > 0, 'names: the refusal lists what is allowed')
        do i = 1, size(NOT_UTF8)
            emsg = ''
            call db_create_table(db, trim(NOT_UTF8(i)), c, rs, emsg)
            call check(rs == SQR_INVALID .and. index(emsg, 'not valid UTF-8') > 0, &
                       'names: ill-formed UTF-8 refused (case ' // achar(48 + i) // ')')
        end do
        emsg = ''
        call db_create_table(db, 'a' // char(194) // char(133), c, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'control characters') > 0, &
                   'names: a C1 control character is refused')
        call db_create_table(db, repeat(CJK, 11), c, rs, emsg)
        call check(rs == SQR_INVALID, 'names: the length limit counts bytes, not characters')
        if (.not. c_utf8_paths()) then
            call skip('names: the case rule folds ASCII letters only', 'not in the UTF-8 code page')
        else if (folds) then
            call skip('names: the case rule folds ASCII letters only', 'the file system folds more')
        else
            call check(db_table_index(db, 'caf' // E_ACUTE) > 0 .and. db_table_index(db, 'CAF' // E_ACUTE_UC) > 0, &
                       'names: the case rule folds ASCII letters only')
        end if

        ! Files already in the directory under the new table's name are not
        ! created over, whoever left them.
        open(newunit=u, file=XDIR // '/stray.schema', status='replace', action='write')
        write(u, '(a)') 'not a schema'
        close(u)
        emsg = ''
        call db_create_table(db, 'stray', c, rs, emsg)
        inquire(file=XDIR // '/stray.schema', size=sz)
        call check(rs == SQR_DUP .and. index(emsg, 'files already exist') > 0, &
                   'names: a table whose files already exist is refused')
        call check(sz > 0 .and. db_table_index(db, 'stray') == 0, &
                   'names: and the files are left as they were')
        ios = c_remove(XDIR // '/stray.schema')
        call db_create_table(db, 'stray', c, rs, emsg)
        call check(rs == SQR_OK, 'names: free once the files are gone')

        ! Case: the same name is a duplicate as before; another case is too.
        call db_create_table(db, 'Plain9', c, rs, emsg)
        call check(rs == SQR_DUP, 'names: an existing name is a duplicate')
        emsg = ''
        call db_create_table(db, 'PLAIN9', c, rs, emsg)
        call check(rs == SQR_DUP .and. index(emsg, 'differs only in case from "Plain9"') > 0, &
                   'names: a name differing only in case is refused, naming the other')
        call check(db_table_index(db, 'PLAIN9') == 0, 'names: lookup stays case-sensitive')
        call db_drop_table(db, 'Plain9', rs)
        call db_create_table(db, 'PLAIN9', c, rs, emsg)
        call check(rs == SQR_OK, 'names: free again once the other is dropped')

        call row_alloc(buf, db_record_size(db, 'a.b'))
        call row_set_int(buf, db%tables(db_table_index(db, 'a.b'))%cols(1), 7_int32)
        call db_insert(db, 'a.b', buf, rid, rs)
        call db_close(db, rs)

        ! A database from before the rules: 'a.b' becomes 'a:b' and 'a+b'
        ! becomes 'A,B' (so that 'a,b' and 'A,B' coexist) behind the engine's
        ! back.  Both must open and work.
        ios = c_rename(XDIR // '/a.b.dat',    XDIR // '/a:b.dat')
        ios = c_rename(XDIR // '/a.b.schema', XDIR // '/a:b.schema')
        ios = c_rename(XDIR // '/a+b.dat',    XDIR // '/A,B.dat')
        ios = c_rename(XDIR // '/a+b.schema', XDIR // '/A,B.schema')
        inquire(file=XDIR // '/_catalog.dat', size=sz)
        allocate(character(len=int(sz)) :: cat)
        open(newunit=u, file=XDIR // '/_catalog.dat', access='stream', form='unformatted', &
             status='old', action='readwrite')
        read(u) cat
        p = index(cat, 'a.b')
        if (p > 0) write(u, pos=p) 'a:b'
        i = index(cat, 'a+b')
        if (i > 0) write(u, pos=i) 'A,B'
        close(u)
        call check(p > 0 .and. i > 0, 'names: catalog entries found')
        call db_open(db, XDIR, rs, emsg)
        call check(rs == SQR_OK, 'names: a database with older names still opens')
        call db_get(db, 'a:b', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'names: a table with a now-refused character is readable')
        call db_insert(db, 'a:b', buf, rid, rs)
        call check(rs == SQR_OK, 'names: and writable')
        call check(db_table_index(db, 'a,b') > 0 .and. db_table_index(db, 'A,B') > 0, &
                   'names: two existing tables differing in case both open')
        call db_close(db, rs)
        ios = c_rmtree(XDIR)
    end subroutine

    subroutine test_explicit_txn()
        character(len=*), parameter :: XDIR = TEST_DIR // '_exptxn'
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(XDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, XDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call db_create_index(db, 't', 'id', rs)
        ti = db_table_index(db, 't')
        do i = 1, 3
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do

        ! Façade preconditions: no commit/rollback without an open explicit txn.
        call db_commit(db, rs)
        call check(rs == SQR_INVALID, 'exptxn: commit with no transaction rejected')
        call db_rollback(db, rs)
        call check(rs == SQR_INVALID, 'exptxn: rollback with no transaction rejected')

        ! Open one, and reject a nested begin (no nesting in v1).
        call db_begin(db, rs)
        call check(rs == SQR_OK .and. db%jrnl%active .and. db%jrnl%explicit, &
                   'exptxn: db_begin opens an explicit transaction')
        call db_begin(db, rs)
        call check(rs == SQR_INVALID, 'exptxn: nested db_begin rejected')

        ! --- rollback path: two inserts, then unwind via the façade ---
        do i = 10, 11
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do
        call db_rollback(db, rs)
        call check(rs == SQR_OK .and. .not. db%jrnl%active .and. .not. db%jrnl%explicit, &
                   'exptxn: db_rollback closes the transaction')
        call db_find_by_int(db, 't', 'id', 10_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'exptxn: rolled-back insert gone from index')
        call check(db%tables(ti)%live_count == 3, 'exptxn: live_count reverted to 3')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'exptxn: db_verify passes after rollback')

        ! --- commit path: the same shape of change, this time kept ---
        call db_begin(db, rs)
        do i = 20, 21
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do
        call db_commit(db, rs)
        call check(rs == SQR_OK .and. .not. db%jrnl%active, &
                   'exptxn: db_commit closes the transaction')
        call db_find_by_int(db, 't', 'id', 20_int32, rid, rs)
        call check(rs == SQR_OK, 'exptxn: committed insert kept in index')
        call check(db%tables(ti)%live_count == 5, 'exptxn: live_count now 5')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'exptxn: db_verify passes after commit')

        call db_close(db)
        ios = c_rmtree(XDIR)
    end subroutine

    ! H5: db_close with an explicit transaction still open must ROLL IT BACK, not
    ! silently persist it.  A begin+insert that reaches close without commit (e.g.
    ! an error path) must leave the store as if the transaction never happened —
    ! not durably commit it via the close-time schema flush.
    subroutine test_close_rolls_back_open_txn()
        character(len=*), parameter :: XDIR = TEST_DIR // '_closetxn'
        type(db_t), target :: db
        type(column_t)     :: c(1)
        character(len=:), allocatable :: buf
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(XDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, XDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call db_create_index(db, 't', 'id', rs)
        ti = db_table_index(db, 't')
        do i = 1, 3
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do

        ! Open a transaction, insert into it, then CLOSE without commit/rollback.
        call db_begin(db, rs)
        do i = 10, 12
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do
        call check(db%jrnl%active, 'closetxn: transaction open with uncommitted inserts')
        call db_close(db, rs)
        call check(rs == SQR_OK, 'closetxn: close (rolling the txn back) reports success')

        ! Reopen: the in-flight inserts must be gone, counters at the pre-txn
        ! value, and the store consistent — the close ABORTED the transaction.
        call db_open(db, XDIR, rs, emsg)
        ti = db_table_index(db, 't')
        call check(db%tables(ti)%live_count == 3, 'closetxn: live_count is the pre-txn 3 on reopen')
        call check(db%tables(ti)%next_id == 4,    'closetxn: next_id reverted to 4 on reopen')
        call db_find_by_int(db, 't', 'id', 11_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'closetxn: in-flight insert absent from index on reopen')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'closetxn: db_verify passes on reopen')
        call db_close(db)
        ios = c_rmtree(XDIR)
    end subroutine

    subroutine jwrite_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', iostat=ios)
        write(u) content
        close(u)
    end subroutine

    subroutine jwrite_region(path, off, content)
        character(len=*), intent(in) :: path, content
        integer(int64),   intent(in) :: off
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        write(u, pos=off) content
        close(u)
    end subroutine

    function jread_file(path, n) result(s)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: n
        character(len=n) :: s
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        read(u, pos=1) s
        close(u)
    end function

    ! By stat, not inquire: a unit a stale (crash stand-in) handle still holds
    ! would make inquire answer from that connection, not from the disk.
    function jfile_size(path) result(n)
        character(len=*), intent(in) :: path
        integer(int64) :: n
        n = c_file_size(path)
    end function

    pure function people_cols() result(c)
        type(column_t) :: c(3)
        c(1)%name = 'pid'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
    end function

    subroutine test_row_helpers()
        character(len=:), allocatable :: buf
        type(column_t) :: c_int, c_real, c_char
        ! Layout with the NULL bitmap: status byte (1) + 1 bitmap byte (3 cols)
        ! then the columns at offsets 3, 7, 15. null_bit is the 0-based ordinal.
        c_int%name = 'i'; c_int%dtype = DT_INT; c_int%csize = 4; c_int%offset = 3; c_int%null_bit = 0
        c_real%name = 'r'; c_real%dtype = DT_REAL; c_real%csize = 8; c_real%offset = 7; c_real%null_bit = 1
        c_char%name = 'c'; c_char%dtype = DT_CHAR; c_char%csize = 5; c_char%offset = 15; c_char%null_bit = 2

        call row_alloc(buf, 19)
        call row_set_status(buf, ROW_ALIVE)
        call row_set_int(buf, c_int, 17_int32)
        call row_set_real(buf, c_real, 2.5_real64)
        call row_set_char(buf, c_char, 'abc')

        call check(row_status(buf) == ROW_ALIVE, 'row_status helper')
        call check(row_get_int(buf, c_int) == 17, 'row int helper')
        call check(row_get_real(buf, c_real) == 2.5_real64, 'row real helper')
        call check(trim(row_get_char(buf, c_char)) == 'abc', 'row char helper')

        ! NULL bitmap: a value setter clears the bit; row_set_null sets it.
        call check(.not. row_is_null(buf, c_int), 'set value clears NULL bit')
        call row_set_null(buf, c_real)
        call check(row_is_null(buf, c_real), 'row_set_null marks NULL')
        call check(.not. row_is_null(buf, c_int), 'row_set_null is per-column')
        call row_set_real(buf, c_real, 9.0_real64)
        call check(.not. row_is_null(buf, c_real), 'setting value un-NULLs')

        call row_clear(buf)
        call check(row_status(buf) == 0_int8, 'row_clear clears status')
        call check(row_get_char(buf, c_char) == '', 'row_clear clears char')
    end subroutine

    subroutine test_empty_open_close()
        type(db_t), target :: db
        integer :: rs
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_OK, 'open empty db')
        call check(db%ntables == 0, 'empty db has 0 tables')
        call db_close(db)
        call check(.not. db%opened, 'db marked closed')
    end subroutine

    subroutine test_create_table_and_insert()
        type(db_t), target     :: db
        type(column_t) :: c(3)
        integer        :: rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        c = people_cols()
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_table(db, 'people', c, rs, emsg)
        call check(rs == SQR_OK, 'create_table people')
        call check(db%ntables == 1, 'ntables=1 after create')
        call check(db_table_index(db, 'people') == 1, 'table_index lookup')
        block
            character(len=SQR_NAME_LEN), allocatable :: names(:)
            call db_list_tables(db, names)
            call check(size(names) == 1 .and. trim(names(1)) == 'people', 'list_tables people')
        end block

        ! Insert 3 rows.
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int (buf, db%tables(1)%cols(1), 1_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 33_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Alice')
        call db_insert(db, 'people', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'insert row 1')

        call row_set_int (buf, db%tables(1)%cols(1), 2_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 45_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Bob')
        call db_insert(db, 'people', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'insert row 2')

        call row_set_int (buf, db%tables(1)%cols(1), 3_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 21_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Carol')
        call db_insert(db, 'people', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'insert row 3')

        call check(db%tables(1)%live_count == 3, 'live_count=3')
        call check(db%tables(1)%next_id == 4, 'next_id=4')

        call db_close(db)
    end subroutine

    subroutine test_real_table_and_scan_stop()
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        type(scan_ctx_t) :: ctx

        c(1)%name = 'id' ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'val'; c(2)%dtype = DT_REAL ; c(2)%csize = 8

        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_table(db, 'metrics', c, rs, emsg)
        call check(rs == SQR_OK, 'create real table')
        call row_alloc(buf, db%tables(db_table_index(db, 'metrics'))%record_size)

        call row_set_int(buf, db%tables(db_table_index(db, 'metrics'))%cols(1), 1_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'metrics'))%cols(2), 2.5_real64)
        call db_insert(db, 'metrics', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'insert real row 1')

        call row_set_int(buf, db%tables(db_table_index(db, 'metrics'))%cols(1), 2_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'metrics'))%cols(2), 1.5_real64)
        call db_insert(db, 'metrics', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'insert real row 2')

        call row_set_int(buf, db%tables(db_table_index(db, 'metrics'))%cols(1), 3_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'metrics'))%cols(2), 2.5_real64)
        call db_insert(db, 'metrics', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'insert duplicate real key row')

        call db_get(db, 'metrics', 2_int32, buf, rs)
        call check(rs == SQR_OK, 'get real row')
        call check(row_get_real(buf, db%tables(db_table_index(db, 'metrics'))%cols(2)) == 1.5_real64, &
                   'real value persisted in row')

        call db_create_index(db, 'metrics', 'val', rs)
        call check(rs == SQR_OK, 'create index on real column')
        call check(db%tables(db_table_index(db, 'metrics'))%indices(1)%nentries == 3, &
                   'real index has 3 entries')

        ! Exact-equality lookups on the real index.
        call db_find_by_real(db, 'metrics', 'val', 1.5_real64, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'find by val=1.5 -> row 2')
        call db_find_by_real(db, 'metrics', 'val', 2.5_real64, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'find by val=2.5 -> first live (row 1)')
        call db_find_by_real(db, 'metrics', 'val', 9.9_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find by val=9.9 (missing) -> NOT_FOUND')

        ! A value with no exact binary representation still round-trips
        ! bit-for-bit (insert goes through the live index too).
        call row_set_int (buf, db%tables(db_table_index(db, 'metrics'))%cols(1), 4_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'metrics'))%cols(2), 0.1_real64)
        call db_insert(db, 'metrics', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 4, 'insert real row 4 (0.1)')
        call db_find_by_real(db, 'metrics', 'val', 0.1_real64, rid, rs)
        call check(rs == SQR_OK .and. rid == 4, 'find by val=0.1 round-trips exactly')

        ctx%count = 0
        call db_scan(db, 'metrics', scan_stop_after_one, ctx, rs)
        call check(rs == SQR_OK .and. ctx%count == 1, 'scan callback can stop early')

        call db_drop_table(db, 'metrics', rs)
        call check(rs == SQR_OK, 'drop real table')
        call db_close(db)
    end subroutine

    ! A NaN has no place in the B+-tree's total order (it compares "equal" to
    ! every value), so it must be kept out of any real index. Exercise all
    ! three entry points: insert, update, and create-index over existing data.
    subroutine test_index_real_nan()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        real(real64) :: nan

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'val'; c(2)%dtype = DT_REAL ; c(2)%csize = 8

        call db_open(db, TEST_DIR, rs, emsg)

        ! --- Indexed table: NaN rejected at insert, store left unchanged ---
        call db_create_table(db, 'rnan', c, rs, emsg)
        call check(rs == SQR_OK, 'nan: create indexed real table')
        call row_alloc(buf, db%tables(db_table_index(db, 'rnan'))%record_size)
        call db_create_index(db, 'rnan', 'val', rs)
        call check(rs == SQR_OK, 'nan: create index on real column')

        call row_set_int (buf, db%tables(db_table_index(db, 'rnan'))%cols(1), 1_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'rnan'))%cols(2), 2.5_real64)
        call db_insert(db, 'rnan', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'nan: ordinary indexed insert OK')

        call row_set_int (buf, db%tables(db_table_index(db, 'rnan'))%cols(1), 2_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'rnan'))%cols(2), nan)
        call db_insert(db, 'rnan', buf, rid, rs)
        call check(rs == SQR_INVALID .and. rid == 0, 'nan: insert NaN into indexed real rejected')

        call db_find_by_real(db, 'rnan', 'val', 2.5_real64, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'nan: store unchanged after rejected insert')
        call check(db%tables(db_table_index(db, 'rnan'))%indices(1)%nentries == 1, &
                   'nan: index still has exactly 1 entry')

        ! --- db_update to NaN rejected, existing row preserved ---
        call row_set_int (buf, db%tables(db_table_index(db, 'rnan'))%cols(1), 1_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'rnan'))%cols(2), nan)
        call db_update(db, 'rnan', 1_int32, buf, rs)
        call check(rs == SQR_INVALID, 'nan: update indexed row to NaN rejected')
        call db_get(db, 'rnan', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. &
                   row_get_real(buf, db%tables(db_table_index(db, 'rnan'))%cols(2)) == 2.5_real64, &
                   'nan: row value preserved after rejected update')
        call db_drop_table(db, 'rnan', rs)

        ! --- create-index over a table that already holds a NaN value ---
        call db_create_table(db, 'rnan2', c, rs, emsg)
        call check(rs == SQR_OK, 'nan: create second real table')
        call row_alloc(buf, db%tables(db_table_index(db, 'rnan2'))%record_size)
        call row_set_int (buf, db%tables(db_table_index(db, 'rnan2'))%cols(1), 1_int32)
        call row_set_real(buf, db%tables(db_table_index(db, 'rnan2'))%cols(2), nan)
        call db_insert(db, 'rnan2', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'nan: NaN allowed in unindexed real column')
        call db_create_index(db, 'rnan2', 'val', rs)
        call check(rs == SQR_INVALID, 'nan: create index over existing NaN rejected')
        ! The failed build must tear the partial index back down.
        call check(db%tables(db_table_index(db, 'rnan2'))%nindices == 0, &
                   'nan: rejected index left no partial index behind')
        call db_drop_table(db, 'rnan2', rs)
        call db_close(db)
    end subroutine

    ! The type-bound spelling: every db_* operation is also reachable as
    ! db%op(...).  This drives a full round-trip purely through the bindings
    ! (a plain binding, the table_index function binding, and the create_index
    ! / find_range generic bindings) to prove they dispatch to the same code.
    subroutine test_oo_bindings()
        type(db_t), target        :: db
        type(column_t)    :: c(3)
        type(db_cursor_t) :: cur
        integer           :: rs, n
        integer(int32)    :: rid
        logical           :: ok
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        c = people_cols()
        call db%open(TEST_DIR, rs, emsg)
        call check(rs == SQR_OK, 'oo: db%open')
        call db%create_table('staff', c, rs, emsg)
        call check(rs == SQR_OK .and. db%table_index('staff') == db%ntables, &
                   'oo: db%create_table + db%table_index')

        block
            integer :: ti
            ti = db%table_index('staff')
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int (buf, db%tables(ti)%cols(1), 1_int32)
            call row_set_int (buf, db%tables(ti)%cols(2), 33_int32)
            call row_set_char(buf, db%tables(ti)%cols(3), 'Alice')
            call db%insert('staff', buf, rid, rs)
            call check(rs == SQR_OK .and. rid == 1, 'oo: db%insert')
            call row_set_int (buf, db%tables(ti)%cols(1), 2_int32)
            call row_set_int (buf, db%tables(ti)%cols(2), 45_int32)
            call row_set_char(buf, db%tables(ti)%cols(3), 'Bob')
            call db%insert('staff', buf, rid, rs)
        end block

        ! Generic binding -> create_index_1
        call db%create_index('staff', 'age', rs)
        call check(rs == SQR_OK, 'oo: db%create_index (generic)')

        call db%find_by_int('staff', 'age', 45_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'oo: db%find_by_int')

        ! Generic binding -> find_range_int, pulled via db%cursor_next
        n = 0
        call db%find_range('staff', 'age', 30_int32, 50_int32, cur, rs)
        do
            call db%cursor_next(cur, rid, buf, ok, rs)
            if (.not. ok) exit
            n = n + 1
        end do
        call check(rs == SQR_OK .and. n == 2, 'oo: db%find_range (generic) + db%cursor_next')

        call db%close(rs)
        call check(.not. db%opened, 'oo: db%close')
    end subroutine

    subroutine test_get_and_scan()
        type(db_t), target :: db
        integer    :: rs
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        type(scan_ctx_t) :: ctx
        call db_open(db, TEST_DIR, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)

        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'get row 1')
        call check(row_get_int(buf, db%tables(1)%cols(1)) == 1, 'row 1 pid=1')
        call check(row_get_int(buf, db%tables(1)%cols(2)) == 33, 'row 1 age=33')
        call check(trim(row_get_char(buf, db%tables(1)%cols(3))) == 'Alice', 'row 1 name=Alice')

        call db_get(db, 'people', 2_int32, buf, rs)
        call check(rs == SQR_OK, 'get row 2')
        call check(trim(row_get_char(buf, db%tables(1)%cols(3))) == 'Bob', 'row 2 name=Bob')

        ! Scan: count and sum ages.
        ctx%count   = 0
        ctx%sum     = 0
        ctx%age_col = db%tables(1)%cols(2)
        call db_scan(db, 'people', scan_count_and_sum, ctx, rs)
        call check(rs == SQR_OK, 'scan returns ok')
        call check(ctx%count == 3, 'scan visited 3 rows')
        call check(ctx%sum == 33 + 45 + 21, 'scan sum=99')
        call db_close(db)
    end subroutine

    subroutine test_delete()
        type(db_t), target :: db
        integer    :: rs
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        type(scan_ctx_t) :: ctx
        call db_open(db, TEST_DIR, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)

        call db_delete(db, 'people', 2_int32, rs)
        call check(rs == SQR_OK, 'delete row 2')
        call check(db%tables(1)%live_count == 2, 'live_count=2 after delete')

        call db_get(db, 'people', 2_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'get deleted row returns NOT_FOUND')

        ctx%count   = 0
        ctx%sum     = 0
        ctx%age_col = db%tables(1)%cols(2)
        call db_scan(db, 'people', scan_count_and_sum, ctx, rs)
        call check(ctx%count == 2, 'scan skips deleted (count=2)')
        call check(ctx%sum == 33 + 21, 'scan skips deleted (sum=54)')

        ! Restore: insert Bob back as row 4 so subsequent tests have him.
        call row_set_int (buf, db%tables(1)%cols(1), 2_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 45_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Bob')
        block
            integer(int32) :: rid
            call db_insert(db, 'people', buf, rid, rs)
            call check(rid == 4, 'reinsert Bob as row 4')
        end block

        call db_close(db)
    end subroutine

    subroutine test_persistence()
        type(db_t), target :: db
        integer    :: rs
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call check(db%ntables == 1, 'reopened: 1 table')
        call check(db%tables(1)%next_id == 5, 'reopened: next_id=5')
        call check(db%tables(1)%live_count == 3, 'reopened: live_count=3')
        call row_alloc(buf, db%tables(1)%record_size)
        call db_get(db, 'people', 4_int32, buf, rs)
        call check(rs == SQR_OK, 'reopened: get row 4')
        call check(trim(row_get_char(buf, db%tables(1)%cols(3))) == 'Bob', 'reopened: row 4 name=Bob')
        call db_close(db)
    end subroutine

    subroutine test_index_int()
        type(db_t), target :: db
        integer    :: rs
        integer(int32) :: rid
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_index(db, 'people', 'age', rs)
        call check(rs == SQR_OK, 'create index on age')
        call check(db%tables(1)%nindices == 1, 'nindices=1')
        call check(db%tables(1)%indices(1)%nentries == 3, 'index has 3 entries (skipping deleted)')

        call db_find_by_int(db, 'people', 'age', 33_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'find by age=33 -> row 1')

        call db_find_by_int(db, 'people', 'age', 21_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'find by age=21 -> row 3')

        call db_find_by_int(db, 'people', 'age', 99_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find by age=99 (missing) -> NOT_FOUND')

        call db_close(db)
    end subroutine

    subroutine test_index_char()
        type(db_t), target :: db
        integer    :: rs
        integer(int32) :: rid
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_index(db, 'people', 'name', rs)
        call check(rs == SQR_OK, 'create index on name')

        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'find by name=Alice -> row 1')

        call db_find_by_char(db, 'people', 'name', 'Carol', rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'find by name=Carol -> row 3')

        call db_find_by_char(db, 'people', 'name', 'Nobody', rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find by name=Nobody -> NOT_FOUND')

        ! A key longer than the char(32) column can never have been stored, so
        ! it must match nothing rather than silently truncating to 32 chars.
        call db_find_by_char(db, 'people', 'name', repeat('A', 33), rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find by 33-char name (>col) -> NOT_FOUND')

        call db_close(db)
    end subroutine

    ! Drain a cursor, collecting the id column (col 1) of every yielded row
    ! in order. Verifies both the ordering and that only live rows surface.
    subroutine drain_cursor(db, cur, ids, n)
        type(db_t),        intent(inout) :: db
        type(db_cursor_t), intent(inout) :: cur
        integer(int32),    intent(out)   :: ids(:)
        integer,           intent(out)   :: n
        integer :: rs, ti
        integer(int32) :: rid
        logical :: ok
        character(len=:), allocatable :: buf
        ti = cur%ti
        allocate(character(len=db%tables(ti)%record_size) :: buf)
        n = 0
        drain: do
            call db_cursor_next(db, cur, rid, buf, ok, rs)
            if (.not. ok) exit drain
            n = n + 1
            ids(n) = row_get_int(buf, db%tables(ti)%cols(1))
        end do drain
    end subroutine

    subroutine test_range_cursor()
        character(len=*), parameter :: RDIR = 'utest_sqr_range_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, n
        integer(int32) :: rid, ids(8)
        character(len=:), allocatable :: buf
        type(db_cursor_t) :: cur
        character(len=128) :: emsg

        ios = c_rmtree(RDIR)
        c(1)%name = 'id'   ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'score'; c(2)%dtype = DT_REAL ; c(2)%csize = 8
        c(3)%name = 'tag'  ; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'm', c, rs, emsg)
        ti = db_table_index(db, 'm')

        ! Insert in scrambled order so the index, not insertion, gives order.
        call ins_m(db, ti, 50_int32, 2.5_real64, 'delta',   buf)
        call ins_m(db, ti, 10_int32, 1.0_real64, 'alpha',   buf)
        call ins_m(db, ti, 30_int32, 3.0_real64, 'charlie', buf)
        call ins_m(db, ti, 20_int32, 1.5_real64, 'bravo',   buf)
        call ins_m(db, ti, 40_int32, 2.0_real64, 'echo',    buf)
        call db_create_index(db, 'm', 'id', rs)
        call db_create_index(db, 'm', 'score', rs)
        call db_create_index(db, 'm', 'tag', rs)
        call check(rs == SQR_OK, 'range: indices built')

        ! Whole-index ascending cursor.
        call db_open_cursor(db, 'm', 'id', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(rs == SQR_OK .and. n == 5 .and. all(ids(1:5) == &
                   [10_int32, 20_int32, 30_int32, 40_int32, 50_int32]), &
                   'range: open_cursor yields all rows in id order')

        ! Inclusive band [20,40].
        call db_find_range(db, 'm', 'id', 20_int32, 40_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [20_int32, 30_int32, 40_int32]), &
                   'range: id [20,40] inclusive -> 20,30,40')

        ! Single-value band (bounds equal).
        call db_find_range(db, 'm', 'id', 30_int32, 30_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 1 .and. ids(1) == 30_int32, 'range: id [30,30] -> 30')

        ! A value with no row.
        call db_find_range(db, 'm', 'id', 25_int32, 25_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 0, 'range: id [25,25] (absent) -> empty')

        ! lo > hi, and bands entirely outside the data.
        call db_find_range(db, 'm', 'id', 40_int32, 20_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 0, 'range: id lo>hi -> empty')
        call db_find_range(db, 'm', 'id', 60_int32, 70_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 0, 'range: id above all -> empty')

        ! Real band: tolerance lives here. [1.5,2.5] -> scores 1.5,2.0,2.5.
        call db_find_range(db, 'm', 'score', 1.5_real64, 2.5_real64, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [20_int32, 40_int32, 50_int32]), &
                   'range: score [1.5,2.5] -> ids 20,40,50 (score order)')

        ! Char band: NUL-padded. ['bravo','delta'] -> bravo,charlie,delta.
        call db_find_range(db, 'm', 'tag', 'bravo', 'delta', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [20_int32, 30_int32, 50_int32]), &
                   'range: tag [bravo,delta] -> ids 20,30,50 (tag order)')

        ! A bound longer than the char(8) column would be silently truncated,
        ! mis-sizing the band -> reject it as a malformed argument.
        call db_find_range(db, 'm', 'tag', 'aaaaaaaaa', 'zzz', cur, rs)
        call check(rs == SQR_INVALID, 'range: over-long char bound (>col) -> INVALID')

        ! Missing table / un-indexed column.
        call db_open_cursor(db, 'nosuch', 'id', cur, rs)
        call check(rs == SQR_NOT_FOUND, 'range: cursor on missing table -> NOT_FOUND')
        call db_find_range(db, 'm', 'id', 1_int32, 9_int32, cur, rs)
        call check(rs == SQR_OK, 'range: cursor on indexed column ok')

        ! Tombstoned rows are skipped. Delete id=30, re-range [20,40].
        call db_find_by_int(db, 'm', 'id', 30_int32, rid, rs)
        call db_delete(db, 'm', rid, rs)
        call check(rs == SQR_OK, 'range: delete id=30')
        call db_find_range(db, 'm', 'id', 20_int32, 40_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2 .and. all(ids(1:2) == [20_int32, 40_int32]), &
                   'range: [20,40] after delete skips tombstone -> 20,40')
        call db_open_cursor(db, 'm', 'id', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 4 .and. all(ids(1:4) == &
                   [10_int32, 20_int32, 40_int32, 50_int32]), &
                   'range: open_cursor after delete -> 10,20,40,50')

        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine

    ! A cursor caches a table slot, so a mutating call (which may shift or free
    ! slots) must invalidate it: db_cursor_next returns SQR_INVALID via the
    ! generation guard rather than reading a stale/out-of-range slot.
    subroutine test_cursor_invalidation()
        character(len=*), parameter :: CDIR = 'utest_sqr_curinv_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, k
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        type(db_cursor_t) :: cur
        character(len=128) :: emsg
        logical :: ok

        ios = c_rmtree(CDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, CDIR, rs, emsg)
        ! 'u' first so it occupies the earlier slot; dropping it later shifts
        ! 't' down — the exact slot-shift that would corrupt a live cursor.
        call db_create_table(db, 'u', c, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        fill: do k = 1, 3
            call row_alloc(buf, db%tables(db_table_index(db,'t'))%record_size)
            call row_set_int(buf, db%tables(db_table_index(db,'t'))%cols(1), int(k, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do fill
        call db_create_index(db, 't', 'id', rs)
        call row_alloc(buf, db%tables(db_table_index(db,'t'))%record_size)

        ! (a) a write to the table invalidates an open cursor.
        call db_open_cursor(db, 't', 'id', cur, rs)
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(ok .and. rs == SQR_OK, 'curinv: first pull ok')
        call db_insert(db, 't', buf, rid, rs)
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(.not. ok .and. rs == SQR_INVALID, &
                   'curinv: pull after insert -> SQR_INVALID')

        ! (b) dropping ANOTHER table (slot shift) also invalidates.
        call db_open_cursor(db, 't', 'id', cur, rs)
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(ok .and. rs == SQR_OK, 'curinv: re-opened cursor pulls')
        call db_drop_table(db, 'u', rs)
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(.not. ok .and. rs == SQR_INVALID, &
                   'curinv: pull after drop of other table -> SQR_INVALID')

        ! (c) a cursor used after db_close is rejected (handle not open); and
        ! db_close reports its flush status.
        call db_open_cursor(db, 't', 'id', cur, rs)
        call db_close(db, rs)
        call check(rs == SQR_OK, 'curinv: db_close stat -> SQR_OK')
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(.not. ok .and. rs == SQR_INVALID, &
                   'curinv: pull after close -> SQR_INVALID')
        ios = c_rmtree(CDIR)
    end subroutine

    ! Insert one (id, score, tag) row into table slot ti of m.
    subroutine ins_m(db, ti, id, score, tag, buf)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: id
        real(real64),     intent(in)    :: score
        character(len=*), intent(in)    :: tag
        character(len=:), allocatable, intent(inout) :: buf
        integer :: rs
        integer(int32) :: rid
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), id)
        call row_set_real(buf, db%tables(ti)%cols(2), score)
        call row_set_char(buf, db%tables(ti)%cols(3), tag)
        call db_insert(db, 'm', buf, rid, rs)
    end subroutine

    subroutine test_update()
        ! Runs after the age + name indices exist. Live people at this point:
        !   row 1 (pid 1, age 33, Alice), row 3 (pid 3, age 21, Carol),
        !   row 4 (pid 2, age 45, Bob); row 2 is a tombstone; next_id = 5.
        type(db_t), target :: db
        integer    :: rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        call db_open(db, TEST_DIR, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)

        ! --- Change a non-key + an indexed (age) column; name unchanged. ---
        call row_set_int (buf, db%tables(1)%cols(1), 1_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 50_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Alice')
        call db_update(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'update row 1 age 33 -> 50')
        call check(db%tables(1)%live_count == 3, 'update keeps live_count=3')
        call check(db%tables(1)%next_id == 5, 'update keeps next_id=5')

        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(1)%cols(2)) == 50, &
                   'row 1 age is 50 after update')

        ! Old age key gone from the index, new key resolves; others intact.
        call db_find_by_int(db, 'people', 'age', 33_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'old age key 33 removed from index')
        call db_find_by_int(db, 'people', 'age', 50_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'new age key 50 -> row 1')
        call db_find_by_int(db, 'people', 'age', 45_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 4, 'untouched age key 45 -> row 4')
        ! Name index entry was not rewritten (key unchanged) but still valid.
        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'unchanged name key still -> row 1')

        ! --- Change the char-indexed column; age now unchanged. ---
        call row_set_int (buf, db%tables(1)%cols(1), 1_int32)
        call row_set_int (buf, db%tables(1)%cols(2), 50_int32)
        call row_set_char(buf, db%tables(1)%cols(3), 'Alicia')
        call db_update(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'update row 1 name Alice -> Alicia')

        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_NOT_FOUND, 'old name key Alice removed from index')
        call db_find_by_char(db, 'people', 'name', 'Alicia', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'new name key Alicia -> row 1')
        call db_find_by_int(db, 'people', 'age', 50_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'unchanged age key still -> row 1')

        ! --- Error paths ---
        call db_update(db, 'missing', 1_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'update missing table -> NOT_FOUND')
        call db_update(db, 'people', 99_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'update out-of-range row -> NOT_FOUND')
        call db_update(db, 'people', 2_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'update tombstoned row -> NOT_FOUND')

        call db_close(db)

        ! --- Persistence of the rewritten row + maintained indices ---
        call db_open(db, TEST_DIR, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(1)%cols(2)) == 50 &
                   .and. trim(row_get_char(buf, db%tables(1)%cols(3))) == 'Alicia', &
                   'updated row 1 persisted across reopen')
        call db_find_by_int(db, 'people', 'age', 50_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'maintained age index persisted')
        call db_find_by_char(db, 'people', 'name', 'Alicia', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'maintained name index persisted')
        call db_close(db)
    end subroutine

    subroutine test_api_error_paths()
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        type(scan_ctx_t) :: ctx

        c = people_cols()
        call db_open(db, TEST_DIR, rs, emsg)
        call row_alloc(buf, db%tables(db_table_index(db, 'people'))%record_size)

        call db_create_table(db, 'people', c, rs, emsg)
        call check(rs == SQR_DUP, 'duplicate table rejected')

        call db_drop_table(db, 'missing', rs)
        call check(rs == SQR_NOT_FOUND, 'drop missing table')

        call db_insert(db, 'missing', buf, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'insert missing table')

        call db_get(db, 'missing', 1_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'get missing table')
        call db_get(db, 'people', 99_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'get missing row')

        call db_delete(db, 'missing', 1_int32, rs)
        call check(rs == SQR_NOT_FOUND, 'delete missing table')
        call db_delete(db, 'people', 99_int32, rs)
        call check(rs == SQR_NOT_FOUND, 'delete missing row')

        ctx%count = 0
        call db_scan(db, 'missing', scan_stop_after_one, ctx, rs)
        call check(rs == SQR_NOT_FOUND, 'scan missing table')

        call db_create_index(db, 'missing', 'age', rs)
        call check(rs == SQR_NOT_FOUND, 'index missing table')
        call db_create_index(db, 'people', 'missing', rs)
        call check(rs == SQR_NOT_FOUND, 'index missing column')
        call db_create_index(db, 'people', 'name', rs)
        call check(rs == SQR_DUP, 'duplicate index rejected')

        call db_find_by_int(db, 'missing', 'age', 1_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find int missing table')
        call db_find_by_int(db, 'people', 'pid', 1_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find int missing index')
        call db_find_by_real(db, 'missing', 'age', 1.0_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find real missing table')
        call db_find_by_real(db, 'people', 'age', 1.0_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find real missing index')
        call db_find_by_char(db, 'missing', 'name', 'Alice', rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find char missing table')
        call db_find_by_char(db, 'people', 'pid', '1', rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find char missing index')

        call db_close(db)
    end subroutine

    subroutine test_validation()
        type(db_t), target :: db
        type(column_t) :: c1(1), c2(2)
        integer :: rs
        character(len=128) :: emsg

        ! --- Bad db directory names: empty, '..' component, control char ---
        ! (the shell is gone, so '/' / spaces / quotes are now legal path
        ! characters; only traversal and unprintables are rejected.)
        call db_open(db, '', rs, emsg)
        call check(rs == SQR_INVALID, 'reject empty db dir')
        call db_open(db, '../escape', rs, emsg)
        call check(rs == SQR_INVALID, 'reject db dir with leading .. component')
        call db_open(db, 'a/../b', rs, emsg)
        call check(rs == SQR_INVALID, 'reject db dir with embedded .. component')
        call db_open(db, 'a' // char(9) // 'b', rs, emsg)
        call check(rs == SQR_INVALID, 'reject db dir with control char')
        ! A '\' is folded to '/' on entry (norm_seps), so a back-slash-delimited
        ! '..' component is caught by the traversal guard on every platform,
        ! exactly as the '/' form is.
        call db_open(db, '..' // char(92) // 'escape', rs, emsg)
        call check(rs == SQR_INVALID, 'reject db dir with leading ..\ component')
        call db_open(db, 'a' // char(92) // '..' // char(92) // 'b', rs, emsg)
        call check(rs == SQR_INVALID, 'reject db dir with embedded ..\ component')

        ! --- Accept ordinary path characters now the shell is gone ---
        block
            integer :: ios
            ios = c_rmtree('utest,comma,db')
            call db_open(db, 'utest,comma,db', rs, emsg)
            call check(rs == SQR_OK, 'accept db dir with comma')
            call db_close(db)
            ios = c_rmtree('utest,comma,db')
        end block

        call db_open(db, TEST_DIR, rs, emsg)

        ! --- Bad table names ---
        c1(1)%name = 'x' ; c1(1)%dtype = DT_INT ; c1(1)%csize = 4
        call db_create_table(db, '', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject empty table name')
        call db_create_table(db, '../escape', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject table name with ..')
        call db_create_table(db, 'a/b', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject table name with /')
        call db_create_table(db, 'bad'//char(9)//'tab', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject table name with control char')

        ! --- Bad column defs ---
        c1(1)%name = 'i' ; c1(1)%dtype = DT_INT ; c1(1)%csize = 2
        call db_create_table(db, 't_badint', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject DT_INT csize != 4')

        c1(1)%name = 'r' ; c1(1)%dtype = DT_REAL ; c1(1)%csize = 4
        call db_create_table(db, 't_badreal', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject DT_REAL csize != 8')

        c1(1)%name = 's' ; c1(1)%dtype = DT_CHAR ; c1(1)%csize = 0
        call db_create_table(db, 't_badchar', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject DT_CHAR csize <= 0')

        c1(1)%name = 'q' ; c1(1)%dtype = 999    ; c1(1)%csize = 4
        call db_create_table(db, 't_baddtype', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject unknown dtype')

        ! --- Bad column names ---
        c1(1)%name = '' ; c1(1)%dtype = DT_INT ; c1(1)%csize = 4
        call db_create_table(db, 't_emptycol', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject empty column name')

        c1(1)%name = '../x' ; c1(1)%dtype = DT_INT ; c1(1)%csize = 4
        call db_create_table(db, 't_escapecol', c1, rs, emsg)
        call check(rs == SQR_INVALID, 'reject column name with ..')

        ! --- Duplicate column names ---
        c2(1)%name = 'a' ; c2(1)%dtype = DT_INT ; c2(1)%csize = 4
        c2(2)%name = 'a' ; c2(2)%dtype = DT_INT ; c2(2)%csize = 4
        call db_create_table(db, 't_dupcol', c2, rs, emsg)
        call check(rs == SQR_INVALID, 'reject duplicate column names')

        ! --- Verify no stray files were created ---
        block
            logical :: ex
            inquire(file=TEST_DIR // '/t_badint.schema', exist=ex)
            call check(.not. ex, 'no schema file for rejected table')
        end block

        call db_close(db)
    end subroutine

    ! With the relaxed dir validation a database may live at a nested path;
    ! db_open must create the intermediate directories (mkdir -p semantics)
    ! and the store must round-trip through that path.
    subroutine test_nested_dir()
        type(db_t), target :: db
        integer :: rs, ios
        character(len=128) :: emsg
        type(column_t) :: c(1)
        character(len=:), allocatable :: buf
        integer(int32) :: rid
        character(len=*), parameter :: ND = 'utest_sqr_nest/level1/level2'

        ios = c_rmtree('utest_sqr_nest')
        call db_open(db, ND, rs, emsg)
        call check(rs == SQR_OK .and. db%opened, &
                   'nested: open creates intermediate dirs')
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'nd', c, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int(buf, db%tables(1)%cols(1), 42_int32)
        call db_insert(db, 'nd', buf, rid, rs)
        call db_close(db)

        call db_open(db, ND, rs, emsg)
        call check(rs == SQR_OK, 'nested: reopen from nested path')
        call row_alloc(buf, db%tables(1)%record_size)
        call db_get(db, 'nd', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. &
                   row_get_int(buf, db%tables(1)%cols(1)) == 42, &
                   'nested: row round-trips through nested db')
        call db_close(db)
        ios = c_rmtree('utest_sqr_nest')
    end subroutine

    subroutine test_readonly()
        character(len=*), parameter :: RO_DIR = 'utest_sqr_ro_db'
        type(db_t), target :: db_rw, db_ro
        type(column_t) :: c(3)
        integer :: rs, ios
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        type(scan_ctx_t) :: ctx

        ios = c_rmtree(RO_DIR)

        ! --- readonly on non-existent dir -> SQR_NOT_FOUND ---
        call db_open(db_ro, 'no_such_sqr_db', rs, emsg, readonly=.true.)
        call check(rs == SQR_NOT_FOUND, 'readonly rejects missing db')
        call check(.not. db_ro%opened, 'readonly missing leaves db closed')

        ! Set up a real db with one table + a row, via a writer.
        c = people_cols()
        call db_open(db_rw, RO_DIR, rs, emsg)
        call db_create_table(db_rw, 'people', c, rs, emsg)
        call check(rs == SQR_OK, 'rw create_table for ro test')
        call row_alloc(buf, db_rw%tables(1)%record_size)
        call row_set_int (buf, db_rw%tables(1)%cols(1), 42_int32)
        call row_set_int (buf, db_rw%tables(1)%cols(2), 30_int32)
        call row_set_char(buf, db_rw%tables(1)%cols(3), 'Eve')
        call db_insert(db_rw, 'people', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'rw insert for ro test')
        call db_close(db_rw)

        ! Open the same dir read-only.
        call db_open(db_ro, RO_DIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_OK, 'readonly open existing db')
        call check(db_ro%readonly, 'readonly flag set')
        call check(db_ro%ntables == 1, 'readonly sees 1 table')

        ! Reads work.
        call db_get(db_ro, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'readonly db_get works')
        call check(row_get_int(buf, db_ro%tables(1)%cols(1)) == 42_int32, 'readonly value preserved')

        ctx%count = 0; ctx%sum = 0
        ctx%age_col = db_ro%tables(1)%cols(2)
        call db_scan(db_ro, 'people', scan_count_and_sum, ctx, rs)
        call check(rs == SQR_OK .and. ctx%count == 1, 'readonly db_scan works')

        ! Writes all return SQR_READONLY.
        call db_insert(db_ro, 'people', buf, rid, rs)
        call check(rs == SQR_READONLY .and. rid == 0, 'readonly db_insert blocked')

        call db_delete(db_ro, 'people', 1_int32, rs)
        call check(rs == SQR_READONLY, 'readonly db_delete blocked')

        call db_update(db_ro, 'people', 1_int32, buf, rs)
        call check(rs == SQR_READONLY, 'readonly db_update blocked')

        call db_create_table(db_ro, 'extra', c, rs, emsg)
        call check(rs == SQR_READONLY, 'readonly db_create_table blocked')

        call db_drop_table(db_ro, 'people', rs)
        call check(rs == SQR_READONLY, 'readonly db_drop_table blocked')

        call db_create_index(db_ro, 'people', 'age', rs)
        call check(rs == SQR_READONLY, 'readonly db_create_index blocked')

        ! Row 1 still alive after the blocked delete.
        call db_get(db_ro, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'row survived blocked delete')

        call db_close(db_ro)

        ! Re-open RW and confirm everything is intact.
        call db_open(db_rw, RO_DIR, rs, emsg)
        call check(db_rw%ntables == 1, 'reopen rw: still 1 table')
        call check(db_rw%tables(1)%live_count == 1, 'reopen rw: still 1 live row')
        call db_close(db_rw)

        ios = c_rmtree(RO_DIR)
    end subroutine

    subroutine test_drop_table()
        type(db_t), target :: db
        integer    :: rs
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call db_drop_table(db, 'people', rs)
        call check(rs == SQR_OK, 'drop people')
        call check(db%ntables == 0, 'ntables=0 after drop')
        call db_close(db)

        ! Re-open to verify drop persisted.
        call db_open(db, TEST_DIR, rs, emsg)
        call check(db%ntables == 0, 'drop persisted')
        call db_close(db)
    end subroutine

    subroutine test_open_corruption_paths()
        type(db_t), target :: db
        integer :: rs, u, ios
        character(len=128) :: emsg
        character(len=4) :: magic
        type(column_t) :: c(1)
        character(len=:), allocatable :: buf
        integer(int32) :: rid

        call cleanup_dir()
        ios = c_mkdir(TEST_DIR)

        magic = 'BAD!'
        open(newunit=u, file=TEST_DIR // '/_catalog.dat', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) SQR_SCHEMA_VERSION, 0
        close(u)
        emsg = ''
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_ERR .and. .not. db%opened, 'open rejects bad catalog magic')

        magic = 'SQRC'
        open(newunit=u, file=TEST_DIR // '/_catalog.dat', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) SQR_BOM
        write(u) 99, 0
        close(u)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_VERSION .and. .not. db%opened, 'open rejects bad catalog version')

        ! Byte-order mark mismatch: a catalog written on a host of the opposite
        ! endianness (the mark reads back byte-swapped) is rejected, not
        ! silently misread. A garbage mark is likewise refused.
        magic = 'SQRC'
        open(newunit=u, file=TEST_DIR // '/_catalog.dat', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) int(z'04030201', int32)   ! SQR_BOM byte-swapped
        write(u) SQR_SCHEMA_VERSION, 0
        close(u)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_VERSION .and. .not. db%opened, &
                   'open rejects opposite-endian catalog (swapped BOM)')

        open(newunit=u, file=TEST_DIR // '/_catalog.dat', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) 1234567_int32              ! neither SQR_BOM nor its swap
        write(u) SQR_SCHEMA_VERSION, 0
        close(u)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_VERSION .and. .not. db%opened, &
                   'open rejects garbage catalog BOM')

        call cleanup_dir()
        c(1)%name = 'x'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_table(db, 'badschema', c, rs, emsg)
        call db_close(db)
        magic = 'BAD!'
        open(newunit=u, file=TEST_DIR // '/badschema.schema', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        close(u)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_ERR .and. .not. db%opened, 'open rejects bad schema magic')

        call cleanup_dir()
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_table(db, 'missingdata', c, rs, emsg)
        call db_close(db)
        ios = c_remove(TEST_DIR // '/missingdata.dat')
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_ERR .and. .not. db%opened, 'open rejects missing data file')

        call cleanup_dir()
        call db_open(db, TEST_DIR, rs, emsg)
        call db_create_table(db, 'missingindex', c, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int(buf, db%tables(1)%cols(1), 1_int32)
        call db_insert(db, 'missingindex', buf, rid, rs)
        call db_create_index(db, 'missingindex', 'x', rs)
        call db_close(db)
        ios = c_remove(TEST_DIR // '/missingindex__i1.idx')
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_ERR .and. .not. db%opened, 'open rejects missing index file')

        call cleanup_dir()
    end subroutine

    ! A table name read back from the catalog is the only on-disk string that
    ! is joined into a filesystem path. A crafted/corrupt entry containing a
    ! path-traversal sequence must be rejected as corruption on open, never
    ! followed out of the database directory (read_catalog re-validates it).
    subroutine test_catalog_traversal_name()
        type(db_t), target :: db
        integer :: rs, u, ios
        character(len=128) :: emsg
        character(len=4) :: magic
        character(len=SQR_NAME_LEN) :: nm

        call cleanup_dir()
        ios = c_mkdir(TEST_DIR)
        magic = 'SQRC'
        open(newunit=u, file=TEST_DIR // '/_catalog.dat', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) SQR_BOM
        write(u) SQR_SCHEMA_VERSION, 1
        nm = '../escape'
        write(u) nm
        close(u)
        emsg = ''
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID .and. .not. db%opened, &
                   'open rejects catalog name with path traversal')
        call cleanup_dir()
    end subroutine

    ! Unclean-shutdown recovery: next_id is only persisted to the schema at
    ! db_close, so a crash after inserts leaves it stale. Force-close the raw
    ! units without db_close to reproduce that exact state, then confirm
    ! open_data recovers next_id from the .dat high-water mark — the
    ! crash-window rows stay visible and the next insert does not overwrite a
    ! live row (the pre-fix silent corruption).
    subroutine test_crash_recovery()
        type(db_t), target :: db
        integer :: rs, j
        character(len=128) :: emsg
        type(column_t) :: c(1)
        character(len=:), allocatable :: buf
        integer(int32) :: rid, first_val

        call cleanup_dir()
        call db_open(db, TEST_DIR, rs, emsg)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'crash', c, rs, emsg)
        call db_create_index(db, 'crash', 'v', rs)
        insert_loop: do j = 1, 5
            call row_alloc(buf, db%tables(1)%record_size)
            call row_set_int(buf, db%tables(1)%cols(1), int(100 + j, int32))
            call db_insert(db, 'crash', buf, rid, rs)
        end do insert_loop
        call check(rs == SQR_OK .and. rid == 5, 'crash: five rows inserted (last id 5)')

        ! Force-close every unit WITHOUT db_close (crash_units): the schema keeps
        ! the stale next_id=1 written back at db_create_table.
        call crash_units(db)
        ! A dead process also drops its advisory lock; release it here so the
        ! recovering open is not blocked by this same-process crash stand-in.
        call c_lock_release(db%lock_tok)

        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_OK .and. db%opened, 'crash: reopened after unclean shutdown')
        ! Recovery recounts both stale counters, not just next_id: all five
        ! crash-window rows are live, so live_count must read 5 (was the stale
        ! schema 0 before the fix), and next_id 6.
        call check(db%tables(1)%next_id == 6, 'crash: next_id recovered to 6')
        call check(db%tables(1)%live_count == 5, 'crash: live_count recovered to 5')

        call row_alloc(buf, db%tables(1)%record_size)
        call db_get(db, 'crash', 5_int32, buf, rs)
        call check(rs == SQR_OK, 'crash: db_get sees crash-window row 5')

        ! Capture row 1's value, then insert a new row.
        call db_get(db, 'crash', 1_int32, buf, rs)
        first_val = row_get_int(buf, db%tables(1)%cols(1))
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int(buf, db%tables(1)%cols(1), 999_int32)
        call db_insert(db, 'crash', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 6, &
                   'crash: next insert appends at id 6, no overwrite')

        call db_get(db, 'crash', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(1)%cols(1)) == first_val, &
                   'crash: live row 1 not overwritten')
        call check(db%tables(1)%live_count == 6, 'crash: live_count tracks new insert (6)')
        call db_verify(db, 'crash', rs)
        call check(rs == SQR_OK, 'crash: db_verify passes after recovery')

        ! Recovered counters persist correctly through a clean close/reopen.
        call db_close(db)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(db%tables(1)%live_count == 6 .and. db%tables(1)%next_id == 7, &
                   'crash: recovered counts survive clean reopen')
        call db_close(db)
        call cleanup_dir()
    end subroutine

    subroutine test_version_check()
        ! Manually create a schema file with a bad version, then try to open.
        type(db_t), target :: db
        integer    :: rs, u, ios
        character(len=128) :: emsg
        character(len=4) :: magic
        character(len=SQR_NAME_LEN) :: nm
        ! Need a table in the catalog first; create then corrupt its schema.
        type(column_t) :: c(1)
        call db_open(db, TEST_DIR, rs, emsg)
        c(1)%name = 'x'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'corrupt', c, rs, emsg)
        call db_close(db)

        ! Rewrite the schema header with a wrong version.
        magic = SQR_MAGIC
        open(newunit=u, file=TEST_DIR // '/corrupt.schema', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) SQR_BOM
        write(u) 99, 1, 5, 1, 0, 0      ! schema_version=99 (bogus)
        nm = 'x'
        write(u) nm
        write(u) DT_INT, 4, 2
        close(u)

        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_VERSION, 'open rejects bad schema version')
        call check(.not. db%opened, 'failed open leaves db closed')
        call db_close(db)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_VERSION, 'close after failed open does not rewrite catalog')
    end subroutine

    ! A schema file is untrusted on-disk data. Craft headers that are
    ! well-formed enough to pass the magic/version gate but internally
    ! inconsistent, and confirm read_schema rejects each with SQR_INVALID
    ! rather than allocating from a bad count or indexing tbl%cols(0).
    subroutine test_schema_corruption()
        type(db_t), target      :: db
        type(column_t)  :: c(1)
        integer         :: rs
        character(len=128) :: emsg

        call cleanup_dir()
        call db_open(db, TEST_DIR, rs, emsg)
        c(1)%name = 'x'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'csch', c, rs, emsg)
        call db_close(db)

        ! Layout for a single DT_INT column: status byte + 1 NULL-bitmap byte
        ! (ncols=1) + 4 data bytes, so the column sits at offset 3 and the
        ! record is 6 bytes. Each negative case below perturbs ONE field.
        ! ncols < 1
        call put_schema(1, 0, 6, 1, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID .and. .not. db%opened, 'reject schema ncols<1')

        ! ncols absurdly large (would drive a huge allocate)
        call put_schema(1, 9000000, 6, 1, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema ncols too large')

        ! next_id < 1
        call put_schema(1, 1, 6, 0, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema next_id<1')

        ! live_count < 0
        call put_schema(1, 1, 6, 1, -1, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema live_count<0')

        ! nindices absurdly large (would drive a huge allocate). nindices is no
        ! longer bounded by ncols — composite indices and dropped-but-tombstoned
        ! slots both push it higher — so only an implausible count is rejected.
        call put_schema(1, 1, 6, 1, 0, 9000000, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema nindices too large')

        ! record_size inconsistent with the column layout (valid is 6)
        call put_schema(1, 1, 7, 1, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema record_size mismatch')

        ! unknown column dtype
        call put_schema(1, 1, 6, 1, 0, 0, 99, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema bad column dtype')

        ! index naming a column that does not exist -> col_idx would be 0
        call put_schema(1, 1, 6, 1, 0, 1, DT_INT, 4, 3, .true., 'zzz', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema index unknown column')

        ! index key_size disagrees with the indexed column
        call put_schema(1, 1, 6, 1, 0, 1, DT_INT, 4, 3, .true., 'x', 8)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema index key geometry')

        ! D3: fixed-width sizes follow the creation rules, the layout still
        ! adding up.  An INT cut to 1 byte (record 6 -> 3), a REAL cut to 4,
        ! a CHAR above SQR_MAX_CHAR.
        emsg = ''
        call put_schema(1, 1, 3, 1, 0, 0, DT_INT, 1, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'corrupt column layout') > 0, &
                   'reject schema INT column of 1 byte')
        call put_schema(1, 1, 6, 1, 0, 0, DT_REAL, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema REAL column of 4 bytes')
        call put_schema(1, 1, SQR_MAX_CHAR + 3, 1, 0, 0, DT_CHAR, SQR_MAX_CHAR + 1, 3, &
                        .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema CHAR column above SQR_MAX_CHAR')

        ! D5: counts below the ceiling that cannot fit in the file
        emsg = ''
        call put_schema(1, 1000000, 6, 1, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'corrupt schema header') > 0, &
                   'reject schema claiming 1,000,000 columns in a small file')
        call put_schema(1, 1, 6, 1, 0, 1000000, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_INVALID, 'reject schema claiming 1,000,000 indexes in a small file')

        ! a faithfully written schema still opens (guards against the checks
        ! being too strict and rejecting valid files)
        call put_schema(1, 1, 6, 1, 0, 0, DT_INT, 4, 3, .false., 'x', 4)
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_OK .and. db%opened, 'valid schema still accepted')
        call db_close(db)

        call cleanup_dir()
    end subroutine

    subroutine put_schema(sv, nc, rsz, nid, lc, ni, cdtype, ccsize, coff, &
                          has_idx, idx_col, idx_ks)
        integer,          intent(in) :: sv, nc, rsz, nid, lc, ni
        integer,          intent(in) :: cdtype, ccsize, coff, idx_ks
        logical,          intent(in) :: has_idx
        character(len=*), intent(in) :: idx_col
        integer :: u, ios
        character(len=4)            :: magic
        character(len=SQR_NAME_LEN) :: nm
        magic = SQR_MAGIC
        open(newunit=u, file=TEST_DIR // '/csch.schema', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) magic
        write(u) SQR_BOM
        write(u) sv, nc, rsz, nid, lc, ni
        if (nc >= 1) then
            nm = 'x'
            write(u) nm
            write(u) cdtype, ccsize, coff
        end if
        if (has_idx) then
            write(u) 1                 ! index arity (single member)
            nm = idx_col
            write(u) nm
            write(u) idx_ks, 0         ! key_size, unique flag
        end if
        close(u)
    end subroutine

    subroutine test_compact()
        character(len=*), parameter :: CDIR = 'utest_sqr_compact_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, bsz_before, bsz_after
        integer(int32) :: rid
        character(len=:), allocatable :: buf, txt
        character(len=128) :: emsg

        ios = c_rmtree(CDIR)

        c(1)%name = 'id'   ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'label'; c(2)%dtype = DT_CHAR ; c(2)%csize = 4
        c(3)%name = 'body' ; c(3)%dtype = DT_TEXT ; c(3)%csize = SQR_TEXT_DESC
        call db_open(db, CDIR, rs, emsg)
        call db_create_table(db, 'box', c, rs, emsg)
        call check(rs == SQR_OK, 'compact: create table')

        ti = db_table_index(db, 'box')
        block
            integer :: k
            integer(int32) :: r
            character(len=2) :: lab
            character(len=16) :: bodies(6)
            bodies = ['body-one-xxxxxxx', 'body-two-xxxxxxx', 'body-three-xxxxx', &
                      'body-four-xxxxxx', 'body-five-xxxxxx', 'body-six-xxxxxxx']
            do k = 1, 6
                call row_alloc(buf, db%tables(ti)%record_size)
                call row_set_int(buf, db%tables(ti)%cols(1), int(k, int32))
                write(lab, '(a1,i1)') 'r', k
                call row_set_char(buf, db%tables(ti)%cols(2), lab)
                call db_insert(db, 'box', buf, r, rs)
                call db_set_text(db, 'box', r, 'body', trim(bodies(k)), rs)
            end do
        end block

        call db_create_index(db, 'box', 'id', rs)
        call check(rs == SQR_OK, 'compact: create id index')
        call db_create_index(db, 'box', 'label', rs)
        call check(rs == SQR_OK, 'compact: create label index')

        ! Tombstone rows 2, 4, 5 -> survivors are old rows 1, 3, 6.
        call db_delete(db, 'box', 2_int32, rs)
        call db_delete(db, 'box', 4_int32, rs)
        call db_delete(db, 'box', 5_int32, rs)
        call check(db%tables(ti)%live_count == 3, 'compact: live_count 3 pre-compact')
        call check(db%tables(ti)%next_id == 7, 'compact: next_id 7 pre-compact')

        ! Rewrite a TEXT value so the original bytes are orphaned in the blob.
        call db_set_text(db, 'box', 1_int32, 'body', 'x', rs)

        inquire(file=CDIR // '/box.blob', size=bsz_before)

        call db_compact(db, 'box', rs)
        call check(rs == SQR_OK, 'compact: db_compact OK')
        call check(db%tables(ti)%live_count == 3, 'compact: live_count 3 post-compact')
        call check(db%tables(ti)%next_id == 4, 'compact: next_id renumbered to 4')

        ! Survivors are renumbered 1,2,3 in original scan order
        ! (old row 1 -> id 1, old row 3 -> id 3, old row 6 -> id 6).
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 'box', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 1, &
                   'compact: new row 1 is old id 1')
        call db_get(db, 'box', 2_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 3, &
                   'compact: new row 2 is old id 3')
        call db_get(db, 'box', 3_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 6, &
                   'compact: new row 3 is old id 6')
        call db_get(db, 'box', 4_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'compact: old tombstone slot gone')

        ! TEXT travels with the renumbered rows; the rewritten value wins.
        call db_get_text(db, 'box', 1_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'x', 'compact: row 1 text is rewritten value')
        call db_get_text(db, 'box', 2_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'body-three-xxxxx', 'compact: row 2 text intact')
        call db_get_text(db, 'box', 3_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'body-six-xxxxxxx', 'compact: row 3 text intact')

        ! Orphaned blob bytes reclaimed.
        inquire(file=CDIR // '/box.blob', size=bsz_after)
        call check(bsz_after < bsz_before, 'compact: blob file shrank')

        ! Indices rebuilt against the renumbered rows.
        call db_find_by_int(db, 'box', 'id', 6_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'compact: id index -> renumbered row')
        call db_find_by_int(db, 'box', 'id', 3_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'compact: id index second survivor')
        call db_find_by_int(db, 'box', 'id', 2_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'compact: deleted key absent from index')
        call db_find_by_char(db, 'box', 'label', 'r6', rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'compact: label index rebuilt')

        ! Idempotent: compacting an already-clean table is a safe no-op.
        call db_compact(db, 'box', rs)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == 3 .and. &
                   db%tables(ti)%next_id == 4, 'compact: idempotent on clean table')

        ! Error path: unknown table.
        call db_compact(db, 'nope', rs)
        call check(rs == SQR_NOT_FOUND, 'compact: unknown table -> NOT_FOUND')

        call db_close(db)

        ! Persistence across reopen.
        call db_open(db, CDIR, rs, emsg)
        ti = db_table_index(db, 'box')
        call check(db%tables(ti)%live_count == 3 .and. db%tables(ti)%next_id == 4, &
                   'compact: counters persisted across reopen')
        call db_find_by_int(db, 'box', 'id', 6_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'compact: index persisted across reopen')
        call db_get_text(db, 'box', 1_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'x', 'compact: text persisted across reopen')
        call db_close(db)

        ! Error path: readonly db rejects compaction.
        call db_open(db, CDIR, rs, emsg, readonly=.true.)
        call db_compact(db, 'box', rs)
        call check(rs == SQR_READONLY, 'compact: readonly db -> READONLY')
        call db_close(db)

        ios = c_rmtree(CDIR)
    end subroutine

    ! Byte-copy a whole file (helper for the compact-recovery fixture).
    subroutine copy_file(src, dst, ok)
        character(len=*), intent(in)  :: src, dst
        logical,          intent(out) :: ok
        integer :: us, ud, ios
        integer(int64) :: sz
        character(len=:), allocatable :: b
        ok = .false.
        open(newunit=us, file=src, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) return
        inquire(unit=us, size=sz)
        allocate(character(len=int(sz)) :: b)
        read(us, iostat=ios) b
        close(us)
        if (ios /= 0) return
        open(newunit=ud, file=dst, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        write(ud, iostat=ios) b
        close(ud)
        ok = (ios == 0)
    end subroutine

    ! Write `text` as the whole content of `path` (fabricated markers).
    subroutine put_text_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) return
        write(u, iostat=ios) text
        close(u)
    end subroutine

    ! E11: an interrupted db_drop_table (the `_dropping` marker) is finished
    ! by the next read-write open, whatever it had got through.
    subroutine test_drop_marker()
        character(len=*), parameter :: DDIR = 'utest_sqr_dropmark_db'
        character(len=*), parameter :: VICTIM = 'utest_sqr_dropx_victim'
        type(db_t), target :: db
        integer :: rs, ios
        logical :: ok
        character(len=128) :: emsg

        ! ---- marker + catalog still naming the table + all its files
        call build_pair(DDIR)
        call put_text_file(DDIR // '/_dropping', drop_marker_text('gone'))
        call reopen_and_expect_gone(DDIR, 'drop marker, table still catalogued')

        ! ---- marker + catalog without the table + the data file alone
        call build_pair(DDIR)
        call save_gone(DDIR)
        call db_open(db, DDIR, rs, emsg)
        call db_drop_table(db, 'gone', rs, emsg)
        call db_close(db)
        call copy_file(DDIR // '/gone.dat.keep', DDIR // '/gone.dat', ok)
        call put_text_file(DDIR // '/_dropping', drop_marker_text('gone'))
        call reopen_and_expect_gone(DDIR, 'drop marker, only the data file left')

        ! ---- marker + catalog without the table + schema and one index
        call build_pair(DDIR)
        call save_gone(DDIR)
        call db_open(db, DDIR, rs, emsg)
        call db_drop_table(db, 'gone', rs, emsg)
        call db_close(db)
        call copy_file(DDIR // '/gone.schema.keep', DDIR // '/gone.schema', ok)
        call copy_file(DDIR // '/gone__i1.idx.keep', DDIR // '/gone__i1.idx', ok)
        call put_text_file(DDIR // '/_dropping', drop_marker_text('gone'))
        call reopen_and_expect_gone(DDIR, 'drop marker, schema and an index left')

        ! ---- marker alone
        call build_pair(DDIR)
        call db_open(db, DDIR, rs, emsg)
        call db_drop_table(db, 'gone', rs, emsg)
        call db_close(db)
        call put_text_file(DDIR // '/_dropping', drop_marker_text('gone'))
        call reopen_and_expect_gone(DDIR, 'drop marker alone')

        ! ---- a read-only open is refused
        call build_pair(DDIR)
        call put_text_file(DDIR // '/_dropping', drop_marker_text('gone'))
        call db_open(db, DDIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_READONLY .and. .not. db%opened, 'drop marker: read-only open refused')
        call db_close(db)

        ! ---- an untrusted name is refused before anything is touched
        call build_pair(DDIR)
        call put_text_file(VICTIM // '.dat', 'x')
        call put_text_file(VICTIM // '.schema', 'x')
        call put_text_file(DDIR // '/_dropping', drop_marker_text('../' // VICTIM))
        emsg = ''
        call db_open(db, DDIR, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'corrupt drop marker') > 0, &
                   'drop marker: a name with ../ is SQR_INVALID')
        call check(c_path_exists(VICTIM // '.dat') .and. c_path_exists(VICTIM // '.schema'), &
                   'drop marker: no file outside the directory touched')
        call db_close(db)
        ios = c_remove(VICTIM // '.dat'); ios = c_remove(VICTIM // '.schema')

        ! ---- an incomplete marker (power lost before its contents were
        ! durable, or a torn write) means no drop began: it is discarded
        call expect_marker_discarded(DDIR, '', 'empty drop marker')
        call expect_marker_discarded(DDIR, 'DROPgone' // repeat(achar(0), 4), &
                                     'drop marker with a wrong check value')
        call expect_marker_discarded(DDIR, repeat(achar(0), 20), 'zero-filled drop marker')
        ios = c_rmtree(DDIR)
    end subroutine

    ! The `_dropping` marker db_drop_table writes for `name`: `DROP`, the
    ! name, then the 4-byte checksum of the two - the engine's polynomial fold
    ! (multiplier 31, modulus 2^31-1), written out here independently.
    function drop_marker_text(name) result(t)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: t
        integer(int64) :: acc
        integer :: i
        t = 'DROP' // name
        acc = 0
        do i = 1, len(t)
            acc = mod(acc * 31_int64 + iachar(t(i:i)), 2147483647_int64)
        end do
        t = t // transfer(int(acc, int32), 'abcd')
    end function

    ! An incomplete marker `text` beside the build_pair tables: a read-only
    ! open succeeds and leaves it; a writable open succeeds, removes it and
    ! touches nothing else.
    subroutine expect_marker_discarded(dir, text, label)
        character(len=*), intent(in) :: dir, text, label
        type(db_t), target :: db
        integer :: rs, st1, st2
        logical :: intact
        character(len=128) :: emsg
        call build_pair(dir)
        call put_text_file(dir // '/_dropping', text)
        emsg = ''
        call db_open(db, dir, rs, emsg, readonly=.true.)
        call check(rs == SQR_OK .and. db_table_index(db, 'gone') > 0, &
                   label // ': a read-only open succeeds')
        call db_close(db)
        call check(c_path_exists(dir // '/_dropping'), label // ': the read-only open leaves it')
        emsg = ''
        call db_open(db, dir, rs, emsg)
        call check(rs == SQR_OK, label // ': a writable open succeeds')
        call check(.not. c_path_exists(dir // '/_dropping'), label // ': the marker is gone')
        intact = .false.
        if (rs == SQR_OK) then
            call db_verify(db, 'keep', st1, emsg)
            call db_verify(db, 'gone', st2, emsg)
            intact = st1 == SQR_OK .and. st2 == SQR_OK .and. db_row_count(db, 'keep') == 1 &
                     .and. db_row_count(db, 'gone') == 1
        end if
        call check(intact, label // ': every table intact')
        call db_close(db)
    end subroutine

    ! test_drop_marker fixture: tables `keep` (one row) and `gone` (a row, a
    ! TEXT value and an index on id, so it has all four kinds of file).
    subroutine build_pair(dir)
        character(len=*), intent(in) :: dir
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        ios = c_rmtree(dir)
        call db_open(db, dir, rs)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'keep', c(1:1), rs)
        c(2)%name = 'body'; c(2)%dtype = DT_TEXT; c(2)%csize = SQR_TEXT_DESC
        call db_create_table(db, 'gone', c, rs)
        ti = db_table_index(db, 'keep')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), 1_int32)
        call db_insert(db, 'keep', buf, rid, rs)
        ti = db_table_index(db, 'gone')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), 7_int32)
        call db_insert(db, 'gone', buf, rid, rs)
        call db_set_text(db, 'gone', rid, 'body', 'some text', rs)
        call db_create_index(db, 'gone', 'id', rs)
        call db_close(db)
    end subroutine

    ! Copies of `gone`'s files, to be put back after a real drop.
    subroutine save_gone(dir)
        character(len=*), intent(in) :: dir
        character(len=*), parameter :: F(4) = [character(len=16) :: &
            'gone.dat', 'gone.schema', 'gone.blob', 'gone__i1.idx']
        integer :: k
        logical :: ok
        do k = 1, size(F)
            call copy_file(dir // '/' // trim(F(k)), dir // '/' // trim(F(k)) // '.keep', ok)
        end do
    end subroutine

    ! Open read-write and expect the interrupted drop of `gone` finished.
    subroutine reopen_and_expect_gone(dir, label)
        character(len=*), intent(in) :: dir, label
        character(len=*), parameter :: F(4) = [character(len=16) :: &
            'gone.dat', 'gone.schema', 'gone.blob', 'gone__i1.idx']
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, k, st
        logical :: left
        character(len=128) :: emsg
        emsg = ''
        call db_open(db, dir, rs, emsg)
        call check(rs == SQR_OK, label // ': opens')
        if (rs /= SQR_OK) return
        call check(db_table_index(db, 'gone') == 0 .and. db_table_index(db, 'keep') > 0, &
                   label // ': the table is gone, the other kept')
        left = .false.
        do k = 1, size(F)
            if (c_path_exists(dir // '/' // trim(F(k)))) left = .true.
        end do
        call check(.not. left, label // ': none of its files remain')
        call check(.not. c_path_exists(dir // '/_dropping'), label // ': the marker is gone')
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_create_table(db, 'gone', c, rs, emsg)
        call check(rs == SQR_OK, label // ': the name can be created again')
        call db_verify(db, 'keep', st, emsg)
        call check(st == SQR_OK .and. db_row_count(db, 'keep') == 1, label // ': the database verifies')
        call db_close(db)
    end subroutine

    subroutine touch_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios == 0) close(u)
    end subroutine

    ! H2 — db_open must finish an interrupted db_compact detected via the
    ! ".compacting" marker: roll a surviving ".dat.compact" temp forward, then
    ! rederive next_id / live_count and rebuild the indices from the renumbered
    ! file. Two windows: (B) marker + temp present (rename-forward), and (A)
    ! marker present with the temp already consumed (idempotent re-run).
    subroutine test_compact_recovery()
        character(len=*), parameter :: RDIR = 'utest_sqr_reco_db'
        character(len=*), parameter :: REF  = 'utest_sqr_reco_ref'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        logical :: ok, exists

        ios = c_rmtree(RDIR)
        ios = c_rmtree(REF)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'tag' ; c(2)%dtype = DT_CHAR ; c(2)%csize = 4

        ! Main db: five rows, tombstone 2 and 4 -> survivors old rows 1,3,5
        ! (id columns 10,30,50). Left un-compacted on disk.
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'reco', c, rs, emsg)
        block
            integer :: k
            character(len=2) :: tg
            do k = 1, 5
                call row_alloc(buf, db%tables(1)%record_size)
                call row_set_int (buf, db%tables(1)%cols(1), int(10*k, int32))
                write(tg, '(a1,i1)') 't', k
                call row_set_char(buf, db%tables(1)%cols(2), tg)
                call db_insert(db, 'reco', buf, rid, rs)
            end do
        end block
        call db_create_index(db, 'reco', 'id', rs)
        call db_create_index(db, 'reco', 'tag', rs)
        call db_delete(db, 'reco', 2_int32, rs)
        call db_delete(db, 'reco', 4_int32, rs)
        call db_close(db)

        ! Reference db: the same survivors inserted fresh in scan order — its
        ! data file is byte-identical to what a completed compact produces
        ! (compact preserves column bytes and status; only the record slot is
        ! renumbered), so it stands in for the ".dat.compact" temp.
        call db_open(db, REF, rs, emsg)
        call db_create_table(db, 'reco', c, rs, emsg)
        block
            integer :: k
            character(len=2) :: tg
            do k = 1, 5, 2
                call row_alloc(buf, db%tables(1)%record_size)
                call row_set_int (buf, db%tables(1)%cols(1), int(10*k, int32))
                write(tg, '(a1,i1)') 't', k
                call row_set_char(buf, db%tables(1)%cols(2), tg)
                call db_insert(db, 'reco', buf, rid, rs)
            end do
        end block
        call db_close(db)

        ! ---- Window B: marker + surviving temp -> rename forward. ----
        call copy_file(REF // '/reco.dat', RDIR // '/reco.dat.compact', ok)
        call check(ok, 'reco: fabricate .dat.compact temp')
        call touch_file(RDIR // '/reco.compacting')

        call db_open(db, RDIR, rs, emsg)
        call check(rs == SQR_OK, 'reco: reopen finishes interrupted compact')
        ti = db_table_index(db, 'reco')
        call check(db%tables(ti)%live_count == 3, 'reco: live_count rederived to 3')
        call check(db%tables(ti)%next_id == 4,    'reco: next_id rederived to 4')

        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 'reco', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 10, &
                   'reco: renumbered row 1 is old id 10')
        call db_get(db, 'reco', 3_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 50, &
                   'reco: renumbered row 3 is old id 50')
        call db_get(db, 'reco', 4_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'reco: old tombstone slot gone')

        ! Indices rebuilt against the renumbered rows.
        call db_find_by_int(db, 'reco', 'id', 30_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'reco: id index -> renumbered row 2')
        call db_find_by_char(db, 'reco', 'tag', 't5', rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'reco: tag index -> renumbered row 3')

        call db_verify(db, 'reco', rs, emsg)
        call check(rs == SQR_OK, 'reco: db_verify clean after recovery')
        call db_close(db)

        ! Marker and temp are retired.
        inquire(file=RDIR // '/reco.compacting', exist=exists)
        call check(.not. exists, 'reco: marker cleared after recovery')
        inquire(file=RDIR // '/reco.dat.compact', exist=exists)
        call check(.not. exists, 'reco: temp consumed by rename')

        ! ---- Window A: marker present, temp already gone -> idempotent. ----
        call touch_file(RDIR // '/reco.compacting')
        call db_open(db, RDIR, rs, emsg)
        call check(rs == SQR_OK, 'reco: idempotent re-run with no temp')
        ti = db_table_index(db, 'reco')
        call check(db%tables(ti)%live_count == 3, 'reco: live_count still 3')
        call db_verify(db, 'reco', rs, emsg)
        call check(rs == SQR_OK, 'reco: db_verify clean after idempotent re-run')
        call db_close(db)
        inquire(file=RDIR // '/reco.compacting', exist=exists)
        call check(.not. exists, 'reco: marker cleared on idempotent re-run')

        ! ---- Read-only handle refuses to run recovery. ----
        call touch_file(RDIR // '/reco.compacting')
        call db_open(db, RDIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_READONLY, 'reco: read-only open refuses recovery')
        ios = c_remove(RDIR // '/reco.compacting')

        ios = c_rmtree(RDIR)
        ios = c_rmtree(REF)
    end subroutine

    ! Test gap 1: drive a multi-level B+-tree entirely through the public API
    ! (the largest indexed table elsewhere fits in one leaf). Insert thousands
    ! of keys in batches to force splits and internal nodes, exercise point and
    ! range lookups across them, then split inside a transaction and roll back.
    subroutine test_deep_tree()
        character(len=*), parameter :: DDIR = 'utest_sqr_deep_db'
        integer, parameter :: NROW = 5000, BATCH = 500
        type(db_t), target :: db
        type(db_cursor_t) :: cur
        type(column_t) :: c(2)
        integer :: rs, ios, ti, k, b, base, cnt
        integer(int32) :: rid, rids(BATCH)
        character(len=:), allocatable :: bufs(:), buf
        character(len=128) :: emsg
        logical :: ok

        ios = c_rmtree(DDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'k' ; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, DDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call db_create_index(db, 't', 'k', rs)
        allocate(character(len=db%tables(ti)%record_size) :: bufs(BATCH))

        ! Keys k = 2*id (all even), inserted 500 at a time.
        base = 0
        fill: do b = 1, NROW / BATCH
            do k = 1, BATCH
                bufs(k) = repeat(char(0), db%tables(ti)%record_size)
                call row_set_int(bufs(k), db%tables(ti)%cols(1), int(base + k, int32))
                call row_set_int(bufs(k), db%tables(ti)%cols(2), int(2*(base + k), int32))
            end do
            call db_insert_many(db, 't', bufs, rids, rs)
            if (rs /= SQR_OK) exit fill
            base = base + BATCH
        end do fill
        call check(rs == SQR_OK .and. base == NROW, 'deep: 5000 rows inserted in batches')
        call check(db%tables(ti)%live_count == NROW, 'deep: live_count 5000')

        ! Point lookups at both ends and a guaranteed-absent odd key.
        call db_find_by_int(db, 't', 'k', 2_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'deep: first key resolves')
        call db_find_by_int(db, 't', 'k', int(2*NROW, int32), rid, rs)
        call check(rs == SQR_OK .and. rid == NROW, 'deep: last key resolves')
        call db_find_by_int(db, 't', 'k', 3_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'deep: absent odd key -> NOT_FOUND')

        ! Range scan over [1000,2000] inclusive: even keys 1000..2000 = 501.
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_find_range(db, 't', 'k', 1000_int32, 2000_int32, cur, rs)
        cnt = 0
        scan: do
            call db_cursor_next(db, cur, rid, buf, ok, rs)
            if (.not. ok) exit scan
            cnt = cnt + 1
        end do scan
        call check(rs == SQR_OK .and. cnt == 501, 'deep: range scan spans internal nodes')

        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'deep: db_verify clean over multi-level tree')

        ! Split inside a transaction, then roll back: the index must return to
        ! its exact pre-txn shape (db_verify walks it against a full scan).
        call db_begin(db, rs)
        do k = 1, BATCH
            bufs(k) = repeat(char(0), db%tables(ti)%record_size)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(NROW + k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), int(2*NROW + 2*k, int32))
        end do
        call db_insert_many(db, 't', bufs, rids, rs)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == NROW + BATCH, &
                   'deep: txn insert grows the tree')
        call db_rollback(db, rs)
        call check(rs == SQR_OK .and. db%tables(ti)%live_count == NROW, &
                   'deep: rollback restores live_count')
        call db_find_by_int(db, 't', 'k', int(2*NROW + 2, int32), rid, rs)
        call check(rs == SQR_NOT_FOUND, 'deep: rolled-back key absent from index')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'deep: db_verify clean after split-in-txn rollback')

        call db_close(db)
        ios = c_rmtree(DDIR)
    end subroutine

    ! Write a hot journal header + payload directly (test gap 3 helper). The
    ! on-disk layout is magic|fmt|state|nrec|cksum|plen then payload at
    ! JHEADER+1; these constants mirror sqr_journal's format.
    subroutine write_hot_journal(path, nrec, cksum, payload)
        character(len=*), intent(in) :: path, payload
        integer,          intent(in) :: nrec, cksum
        integer, parameter :: JFMT = 1, JSTATE_HOT = 1
        integer(int64), parameter :: JHEADER = 64_int64
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        write(u, pos=1) 'SQRJ', JFMT, JSTATE_HOT, nrec, cksum, int(len(payload), int64)
        if (len(payload) > 0) write(u, pos=JHEADER + 1) payload
        close(u)
    end subroutine

    ! Write a hot journal claiming an arbitrary payload length with no payload
    ! (test gap 3 helper for the length-guard path).
    subroutine write_hot_journal_plen(path, plen)
        character(len=*), intent(in) :: path
        integer(int64),   intent(in) :: plen
        integer, parameter :: JFMT = 1, JSTATE_HOT = 1
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        write(u, pos=1) 'SQRJ', JFMT, JSTATE_HOT, 1, 0, plen
        close(u)
    end subroutine

    ! Test gap 3: recovery is otherwise only tested against journals the API
    ! itself armed. Construct corrupt/torn _journal.dat files by hand and prove
    ! the open path discards them gracefully (never replays garbage) and leaves
    ! the committed data intact — the checksum/length/short-header guards where
    ! H1/H6 live had zero coverage.
    subroutine test_torn_journal()
        character(len=*), parameter :: JDIR = 'utest_sqr_torn_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, u
        integer(int32) :: rid
        character(len=:), allocatable :: buf, jpath
        character(len=128) :: emsg

        ios = c_rmtree(JDIR)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, JDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        call row_set_int(buf, db%tables(1)%cols(1), 7_int32)
        call db_insert(db, 't', buf, rid, rs)
        call db_close(db)                       ! one committed row, journal gone
        jpath = JDIR // '/_journal.dat'

        ! A HOT journal that fails its checks is never replayed and never
        ! voided: it is damage to the record of an interrupted transaction, so
        ! the open is refused and the file kept (torn_refused checks that,
        ! then takes the documented way out and proves the committed row).

        ! Case 1: hot header + garbage payload with a mismatched checksum.
        call write_hot_journal(jpath, 1, 123456, 'XXXXXXXX')
        call torn_refused(JDIR, jpath, 'corrupt-payload journal')

        ! Case 2: hot header claiming an absurd payload length — must be rejected
        ! by the length guard, not trusted into a huge allocate.
        call write_hot_journal_plen(jpath, huge(0_int64))
        call torn_refused(JDIR, jpath, 'absurd-length journal')

        ! Case 3: truncated file (magic only) — not a hot header at all, so it
        ! is treated as foreign/absent and the open succeeds.
        open(newunit=u, file=jpath, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        write(u) 'SQRJ'
        close(u)
        call db_open(db, JDIR, rs, emsg)
        call check(rs == SQR_OK, 'torn: open ignores truncated journal header')
        call db_get(db, 't', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'torn: row still readable after truncated-journal open')
        call db_close(db)

        ! Case 4: payload_len exactly huge(int32). The old ceiling was
        ! "> huge(0)", so this value slipped through and drove a 2 GiB allocate
        ! whose checksum fold then ran off the end. The file's own size is the
        ! real bound: a journal cannot hold more payload than it is long.
        call write_hot_journal_plen(jpath, int(huge(0), int64))
        call torn_refused(JDIR, jpath, 'plen of huge(int32)')

        ! Case 5: an absurd record count with an empty payload. nrec sized an
        ! unguarded allocate, so this aborted db_open with "insufficient
        ! virtual memory" rather than reporting anything. A record cannot be
        ! shorter than 40 bytes, so the payload length caps the count.
        call write_hot_journal(jpath, huge(0), 0, '')
        call torn_refused(JDIR, jpath, 'absurd record count')

        ! Case 6: a record whose pathlen is near huge(int32). The guard used to
        ! read `p + pathlen - 1 > len(buf)`, which overflows to a negative
        ! number and passes; the next field was then cut at a negative index
        ! (SIGSEGV in an optimised build). Bounding against the bytes REMAINING
        ! cannot wrap. The payload carries its own checksum so it reaches the
        ! decoder rather than being rejected earlier.
        wild_pathlen: block
            character(len=:), allocatable :: pl
            pl = i32b(1) // i32b(huge(0)) // 'x' // repeat(char(0), 32)
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
        end block wild_pathlen
        call torn_refused(JDIR, jpath, 'wild pathlen')

        ! Cases 7-10: well-formed records (valid checksum, parseable fields)
        ! carrying hostile *meanings*. Each must leave t.dat exactly as it is:
        ! an undo restores bytes a file already held, so replaying one can
        ! never make a file longer.
        undo_bounds: block
            integer(int64) :: sz0, sz1
            character(len=:), allocatable :: dpath, pl
            dpath = JDIR // '/t.dat'
            inquire(file=dpath, size=sz0)

            ! 7: a region a gigabyte past the end. Applied blind this punches a
            ! sparse hole and leaves a data file whose length no longer matches
            ! its record geometry.
            pl = jrec_bytes(1, 't.dat', sz0, 1000000000_int64, 4_int64, 'ABCD')
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
            call db_open(db, JDIR, rs, emsg)
            call check(rs == SQR_OK, 'torn: open survives a region past EOF')
            call db_close(db)
            inquire(file=dpath, size=sz1)
            call check(sz1 == sz0, 'torn: region past EOF did not grow the file')

            ! 8: the same, at an offset that would overflow a length sum.
            pl = jrec_bytes(1, 't.dat', sz0, 9223372036854775000_int64, 4_int64, 'ABCD')
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
            call db_open(db, JDIR, rs, emsg)
            call check(rs == SQR_OK, 'torn: open survives a wild region offset')
            call db_close(db)
            inquire(file=dpath, size=sz1)
            call check(sz1 == sz0, 'torn: wild region offset did not grow the file')

            ! 9: an EXTEND undo whose captured length exceeds the file. Truncate
            ! grows as readily as it shrinks; an undo must only ever shrink.
            pl = jrec_bytes(2, 't.dat', sz0 + 1000000_int64, 0_int64, 0_int64, '')
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
            call db_open(db, JDIR, rs, emsg)
            call check(rs == SQR_OK, 'torn: open survives an oversized extend undo')
            call db_close(db)
            inquire(file=dpath, size=sz1)
            call check(sz1 == sz0, 'torn: oversized extend undo did not grow the file')

            ! 10: a path that escapes the database directory, and an unknown
            ! record kind. Both are structural, so nothing of the set is
            ! applied: the open is refused, and the committed row is still
            ! there once the journal is removed.
            pl = jrec_bytes(1, '../escaped', 0_int64, 1_int64, 4_int64, 'PWND')
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
            call torn_refused(JDIR, jpath, 'journal naming a path outside the db')
            call check(.not. c_path_exists(JDIR // '/../escaped'), &
                       'torn: no file written outside the database directory')

            pl = jrec_bytes(99, 't.dat', sz0, 1_int64, 4_int64, 'ABCD')
            call write_hot_journal(jpath, 1, cksum_of(pl), pl)
            call torn_refused(JDIR, jpath, 'unknown record kind')
            inquire(file=dpath, size=sz1)
            call check(sz1 == sz0, 'torn: t.dat unchanged by the hostile-record cases')
        end block undo_bounds

        ios = c_rmtree(JDIR)

    end subroutine

    ! The journal just written at `jpath` is hot and unusable: the open must
    ! be refused with the way out in the message and the journal kept.  Then
    ! take the way out and check the one committed row (v = 7) is as it was.
    subroutine torn_refused(jdir, jpath, what)
        character(len=*), intent(in) :: jdir, jpath, what
        type(db_t), target :: db
        character(len=:), allocatable :: buf
        character(len=256) :: why
        integer :: rs, r
        why = ''
        call db_open(db, jdir, rs, why)
        call check(rs == SQR_ERR, 'torn: open refuses a ' // what)
        call check(index(why, '_journal.dat') > 0, &
                   'torn: refusal of a ' // what // ' names the journal file')
        call check(c_path_exists(jpath), 'torn: ' // what // ' is kept, not removed')
        r = c_remove(jpath)
        call db_open(db, jdir, rs, why)
        call check(rs == SQR_OK, 'torn: opens as it stands without the ' // what)
        call check(db%tables(1)%live_count == 1, 'torn: committed row survives a ' // what)
        call row_alloc(buf, db%tables(1)%record_size)
        call db_get(db, 't', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(1)%cols(1)) == 7, &
                   'torn: committed row value intact after a ' // what)
        call db_verify(db, 't', rs, why)
        call check(rs == SQR_OK, 'torn: db_verify clean after a ' // what)
        call db_close(db)
    end subroutine torn_refused

    ! One serialised undo record, field order per sqr_journal::put_rec:
    ! kind | pathlen | path | orig_len | offset | length | byteslen | bytes.
    function jrec_bytes(kind, path, orig_len, offset, length, bytes) result(s)
        integer,          intent(in) :: kind
        character(len=*), intent(in) :: path, bytes
        integer(int64),   intent(in) :: orig_len, offset, length
        character(len=:), allocatable :: s
        s = i32b(kind) // i32b(len(path)) // path // i64b(orig_len) // &
            i64b(offset) // i64b(length) // i64b(int(len(bytes), int64)) // bytes
    end function

    ! int64 -> its 8 native bytes, for hand-built journal payloads.
    function i64b(v) result(s)
        integer(int64), intent(in) :: v
        character(len=8) :: s
        s = transfer(v, s)
    end function

    ! int32 -> its 4 native bytes, for hand-built journal payloads.
    function i32b(v) result(s)
        integer, intent(in) :: v
        character(len=4) :: s
        s = transfer(int(v, int32), s)
    end function

    ! The engine's rolling payload checksum (sqr_base::checksum), duplicated
    ! here so a hand-built payload can carry a valid one and so reach the
    ! record decoder instead of being turned away by the checksum guard.
    function cksum_of(buf) result(c)
        character(len=*), intent(in) :: buf
        integer :: c, i
        integer(int64) :: acc
        acc = 0_int64
        do i = 1, len(buf)
            acc = mod(acc * 31_int64 + iachar(buf(i:i)), 2147483647_int64)
        end do
        c = int(acc)
    end function

    ! A schema's next_id is the high-water row id; the .dat file is the truth.
    ! A corrupt value ABOVE what the file can justify used to survive open
    ! (open_data only ever repaired upward), after which one insert extended
    ! the data file to tens of GB and the undo/redo history sized a read region
    ! from the gap — a wild allocate reached from nothing but a reopen.
    subroutine test_schema_next_id()
        character(len=*), parameter :: NDIR = 'utest_sqr_nextid_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, u, k
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        ios = c_rmtree(NDIR)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, NDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call row_alloc(buf, db%tables(1)%record_size)
        do k = 1, 5
            call row_set_int(buf, db%tables(1)%cols(1), int(k, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do
        call db_close(db)

        ! next_id lives at schema byte 21 (magic 4, BOM 4, version/ncols/
        ! record_size 12).
        open(newunit=u, file=NDIR // '/t.schema', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=21) 1000000000_int32
        close(u)
        call db_open(db, NDIR, rs, emsg)
        call check(rs == SQR_INVALID, 'nextid: value beyond the data file rejected')
        call db_close(db)

        ! One past the true high-water is still corruption, not staleness:
        ! staleness only ever runs the other way (a crash leaves next_id LOW).
        open(newunit=u, file=NDIR // '/t.schema', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=21) 7_int32
        close(u)
        call db_open(db, NDIR, rs, emsg)
        call check(rs == SQR_INVALID, 'nextid: one past the high-water rejected')
        call db_close(db)

        ! The legitimate value still opens, and a LOW one is still repaired
        ! upward from the file (the crash-recovery path).
        open(newunit=u, file=NDIR // '/t.schema', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=21) 6_int32
        close(u)
        call db_open(db, NDIR, rs, emsg)
        call check(rs == SQR_OK, 'nextid: correct value opens')
        call db_close(db)

        open(newunit=u, file=NDIR // '/t.schema', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=21) 2_int32
        close(u)
        call db_open(db, NDIR, rs, emsg)
        call check(rs == SQR_OK .and. db%tables(1)%next_id == 6, &
                   'nextid: stale-low value still recovered from the file')
        call db_close(db)

        ios = c_rmtree(NDIR)
    end subroutine

    ! The engine's view of a corrupt index: every entry point that touches one
    ! must come back with SQR_INVALID rather than crash, and db_verify — the
    ! routine a user reaches for BECAUSE they suspect corruption — must return
    ! at all on a leaf chain that cycles.
    subroutine test_corrupt_index()
        character(len=*), parameter :: CDIR = 'utest_sqr_badidx_db'
        type(db_t), target :: db
        type(db_cursor_t) :: cur
        type(column_t) :: c(1)
        integer :: rs, ios, k, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        logical :: ok

        ios = c_rmtree(CDIR)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, CDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        do k = 1, 600
            call row_set_int(buf, db%tables(ti)%cols(1), int(k, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do
        call db_create_index(db, 't', 'v', rs)
        call check(rs == SQR_OK, 'badidx: 600 rows indexed')
        call db_close(db)

        ! nkeys of the first leaf (page 2, byte 2 within it) far past capacity.
        call poke_index('utest_sqr_badidx_db/t__i1.idx', 4096 + 2, 100000_int32)
        call db_open(db, CDIR, rs, emsg)
        call check(rs == SQR_OK, 'badidx: open succeeds (the schema is fine)')
        call db_find_by_int(db, 't', 'v', 3_int32, rid, rs)
        call check(rs == SQR_INVALID, 'badidx: find_by_int reports corruption')
        call db_verify(db, 't', rs, emsg)
        call check(rs /= SQR_OK, 'badidx: verify reports rather than crashes')
        ! Opening a cursor reads no page (bt_first only positions it), so the
        ! corruption surfaces on the first pull — which is the contract that
        ! matters: the caller gets a status, not a crash.
        call db_open_cursor(db, 't', 'v', cur, rs)
        call check(rs == SQR_OK, 'badidx: cursor opens without touching a page')
        call db_cursor_next(db, cur, rid, buf, ok, rs)
        call check(rs == SQR_INVALID, 'badidx: first cursor pull reports corruption')
        ! A LOW key: the index-maintenance descent then routes into the damaged
        ! leaf (page 2 holds the low keys) rather than the intact one, which is
        ! the path that used to write past the page buffer.
        call row_clear(buf)
        call row_set_int(buf, db%tables(1)%cols(1), 1_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_INVALID, 'badidx: insert reports corruption')
        call db_close(db)

        ! A leaf chain that cycles: db_verify walks it entry by entry, so
        ! before the cursor's hop ceiling this never returned at all.
        call poke_index('utest_sqr_badidx_db/t__i1.idx', 4096 + 2, 294_int32)
        call poke_index('utest_sqr_badidx_db/t__i1.idx', 4096 + 6, 2_int32)
        call db_open(db, CDIR, rs, emsg)
        call db_verify(db, 't', rs, emsg)
        call check(rs /= SQR_OK, 'badidx: verify terminates on a cyclic leaf chain')
        call db_close(db)

        ios = c_rmtree(CDIR)
    end subroutine

    ! Overwrite one native int32 at a 1-based byte position of an index file.
    subroutine poke_index(path, pos, v)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: pos
        integer(int32),   intent(in) :: v
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='write', iostat=ios)
        if (ios /= 0) return
        write(u, pos=pos) v
        close(u)
    end subroutine

    ! Step 7: pack a database directory into a single .sqr container and unpack
    ! it back. Round-trip (open + verify + query match), plus the guards:
    ! overwrite refusal, truncated-container rejection, hot-journal refusal, and
    ! path-traversal rejection of a crafted archive name.
    ! Review finding A1: the pack holds each file, and the whole payload, in
    ! one string, so sizes beyond a default integer are refused, not wrapped.
    ! The big files are sparse (one byte written far out), so this is quick.
    subroutine test_pack_too_large()
        character(len=*), parameter :: PDIR  = 'utest_sqr_packbig_db'
        character(len=*), parameter :: PFILE = 'utest_sqr_packbig.sqr'
        integer(int64),   parameter :: GIB = 1073741824_int64
        type(db_t), target :: db
        type(column_t) :: c(1)
        character(len=128) :: emsg
        integer :: rs, ios
        ios = c_rmtree(PDIR)
        ios = c_remove(PFILE)
        ! TEXT tables: it is the blob files that are grown, because db_open
        ! reads the whole of a data file that has grown, to recount its rows.
        c(1)%name = 'v'; c(1)%dtype = DT_TEXT; c(1)%csize = SQR_TEXT_DESC
        call db_open(db, PDIR, rs)
        call db_create_table(db, 'big', c, rs)
        call db_create_table(db, 'other', c, rs)
        call db_close(db, rs)

        ! Each file under the limit, the two together over it.
        call stretch(PDIR // '/big.blob',   GIB + GIB / 2)
        call stretch(PDIR // '/other.blob', GIB + GIB / 2)
        emsg = ''
        call db_pack(PDIR, PFILE, rs, emsg)
        call check(rs == SQR_FULL .and. index(emsg, 'larger than 2 GiB in total') > 0, &
                   'pack big: a total over 2 GiB is refused, and says so')
        call check(.not. c_path_exists(PFILE) .and. .not. c_path_exists(PFILE // '.tmp'), &
                   'pack big: nothing was written')

        ! The open-handle pack shares the path, and the handle survives it.
        call db_open(db, PDIR, rs)
        call check(rs == SQR_OK, 'pack big: fixture handle open over the large files')
        emsg = ''
        call db_pack_live(db, PFILE, rs, emsg)
        call check(rs == SQR_FULL .and. index(emsg, 'larger than 2 GiB in total') > 0, &
                   'pack big: db_pack_live refuses the same way')
        call check(db_row_count(db, 'other') == 0 .and. .not. c_path_exists(PFILE), &
                   'pack big: the handle is still usable and nothing was written')
        call db_close(db, rs)

        ! One file of 5 GiB: the size that used to pack "successfully", cut to 1 GiB.
        call stretch(PDIR // '/big.blob', 5 * GIB)
        emsg = ''
        call db_pack(PDIR, PFILE, rs, emsg)
        call check(rs == SQR_FULL .and. index(emsg, '"big.blob" is larger than 2 GiB') > 0, &
                   'pack big: a file over 2 GiB is refused, by name')
        call check(.not. c_path_exists(PFILE), 'pack big: no container for the oversized file')
        ios = c_rmtree(PDIR)
    end subroutine

    ! Grow `path` to `nbytes` by writing its last byte only (a sparse file).
    subroutine stretch(path, nbytes)
        character(len=*), intent(in) :: path
        integer(int64),   intent(in) :: nbytes
        integer :: u
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite')
        write(u, pos=nbytes) char(0)
        close(u)
    end subroutine

    subroutine test_pack()
        character(len=*), parameter :: PDIR  = 'utest_sqr_pack_db'
        character(len=*), parameter :: PDIR2 = 'utest_sqr_pack_db2'
        character(len=*), parameter :: EVIL  = 'utest_sqr_pack_evil'
        character(len=*), parameter :: PFILE = 'utest_sqr_pack.sqr'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, u, k
        integer(int32) :: rid
        integer(int64) :: fsz
        character(len=:), allocatable :: buf, txt
        character(len=16) :: body
        character(len=128) :: emsg
        logical :: ex

        ios = c_rmtree(PDIR); ios = c_rmtree(PDIR2); ios = c_rmtree(EVIL)
        ios = c_remove(PFILE)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'tag' ; c(2)%dtype = DT_CHAR ; c(2)%csize = 4
        c(3)%name = 'body'; c(3)%dtype = DT_TEXT ; c(3)%csize = SQR_TEXT_DESC
        call db_open(db, PDIR, rs, emsg)
        call db_create_table(db, 'items', c, rs, emsg)
        ti = db_table_index(db, 'items')
        do k = 1, 3
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int (buf, db%tables(ti)%cols(1), int(10*k, int32))
            write(body, '(a1,i1)') 't', k
            call row_set_char(buf, db%tables(ti)%cols(2), trim(body))
            call db_insert(db, 'items', buf, rid, rs)
            write(body, '(a5,i1)') 'body-', k
            call db_set_text(db, 'items', rid, 'body', trim(body), rs)
        end do
        call db_create_index(db, 'items', 'id', rs)
        call db_close(db)

        ! Round-trip.
        call db_pack(PDIR, PFILE, rs)
        call check(rs == SQR_OK, 'pack: db_pack succeeds')
        inquire(file=PFILE, exist=ex)
        call check(ex, 'pack: container file created')
        ! A temporary directory left by an abandoned attempt is replaced, and
        ! none is left by this one.
        ios = c_mkdir(PDIR2 // '.unpack-tmp')
        call put_bytes(PDIR2 // '.unpack-tmp/stale.dat', 'left by an abandoned unpack')
        call db_unpack(PFILE, PDIR2, rs)
        call check(rs == SQR_OK, 'pack: db_unpack succeeds')
        call check(.not. c_path_exists(PDIR2 // '.unpack-tmp') .and. &
                   .not. c_path_exists(PDIR2 // '/stale.dat'), &
                   'pack: a stale temporary directory is replaced, and none is left')

        call db_open(db, PDIR2, rs, emsg)
        call check(rs == SQR_OK, 'pack: unpacked db opens')
        ti = db_table_index(db, 'items')
        call check(db%tables(ti)%live_count == 3, 'pack: row count preserved')
        call db_find_by_int(db, 'items', 'id', 20_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'pack: index resolves in unpacked db')
        call db_get_text(db, 'items', 2_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'body-2', 'pack: TEXT/blob preserved')
        call db_verify(db, 'items', rs, emsg)
        call check(rs == SQR_OK, 'pack: db_verify clean on unpacked db')
        call db_close(db)

        ! Unpack refuses to overwrite an existing directory.
        call db_unpack(PFILE, PDIR2, rs)
        call check(rs == SQR_DUP, 'pack: unpack refuses existing dir')

        ! Truncated container rejected; no directory left behind.
        ios = c_rmtree(PDIR2)
        inquire(file=PFILE, size=fsz)
        ios = c_truncate(PFILE, fsz - 8_int64)
        call db_unpack(PFILE, PDIR2, rs)
        call check(rs == SQR_ERR, 'pack: truncated container rejected')
        call check(.not. c_path_exists(PDIR2), 'pack: no dir left after a failed unpack')

        ! Hot journal refused by pack (a consistent snapshot only).
        call write_hot_journal(PDIR // '/_journal.dat', 1, 0, '')
        call db_pack(PDIR, 'utest_sqr_pack_hot.sqr', rs)
        call check(rs == SQR_READONLY, 'pack: hot journal refused')
        ios = c_remove(PDIR // '/_journal.dat')
        ios = c_remove('utest_sqr_pack_hot.sqr')

        ! A crafted container whose archived name escapes the directory is
        ! rejected before any file is written.
        open(newunit=u, file='utest_sqr_pack_evil.sqr', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) 'SQRP', 1_int32, SQR_BOM, 1_int32, 0_int32
        write(u) 7_int32, '../evil', 1_int64, 0_int64
        write(u) 'x'
        close(u)
        call db_unpack('utest_sqr_pack_evil.sqr', EVIL, rs)
        call check(rs == SQR_INVALID, 'pack: path-traversal archive name rejected')
        call check(.not. c_path_exists(EVIL), 'pack: nothing written for a malicious archive')

        ! A3: a trailing separator on the target names the same directory.
        ! (PFILE was truncated above: pack it afresh.)
        call db_pack(PDIR, PFILE, rs)
        ios = c_rmtree(PDIR2)
        call db_unpack(PFILE, PDIR2 // '/', rs)
        call check(rs == SQR_OK, 'pack: unpack to "<dir>/" succeeds')
        call check(.not. c_path_exists(PDIR2 // '/.unpack-tmp') .and. &
                   .not. c_path_exists(PDIR2 // '.unpack-tmp'), &
                   'pack: unpack to "<dir>/" leaves no temporary inside or beside it')
        call db_open(db, PDIR2, rs, emsg)
        call check(rs == SQR_OK .and. db_table_index(db, 'items') > 0, &
                   'pack: the database unpacked to "<dir>/" opens with its table')
        call db_close(db)
        call db_unpack(PFILE, PDIR2 // '/', rs)
        call check(rs == SQR_DUP, 'pack: a second unpack to "<dir>/" is refused as existing')
        call db_unpack(PFILE, '', rs)
        call check(rs == SQR_INVALID, 'pack: unpack to an empty name is refused')
        ios = c_rmtree(PDIR2)

        ! A5: the checksum covers the table of contents.  Damage one name into
        ! another acceptable one ("items.schema" -> "itemz.schema").
        call get_bytes(PFILE, buf)
        call check(buf(5:8) == i32b(2), 'pack: db_pack writes container version 2')
        k = index(buf, 'items.schema')
        if (k > 0) buf(k+4:k+4) = 'z'
        call put_bytes('utest_sqr_pack_toc.sqr', buf)
        call db_unpack('utest_sqr_pack_toc.sqr', PDIR2, rs)
        call check(k > 0 .and. rs == SQR_ERR, 'pack: a damaged name in the TOC is refused')
        call check(.not. c_path_exists(PDIR2), 'pack: nothing left after a damaged TOC')
        ios = c_remove('utest_sqr_pack_toc.sqr')

        ! A4: only the names db_pack writes are accepted.
        call craft_refused(EVIL, '_journal.dat', SQR_INVALID)
        call craft_refused(EVIL, '_JOURNAL.DAT', SQR_INVALID)
        call craft_refused(EVIL, '_lock',        SQR_INVALID)
        call craft_refused(EVIL, 't.alter',      SQR_INVALID)
        call craft_refused(EVIL, 'notes.txt',    SQR_INVALID)
        call craft_refused(EVIL, '.dat',         SQR_INVALID)
        call craft_refused(EVIL, 't.dat|t.dat',  SQR_ERR)
        ! A7: a name ending in a blank is not a database file, and is not
        ! taken for the trimmed name.
        call craft_refused(EVIL, 't.dat |_catalog.dat', SQR_INVALID)
        call check(.not. c_path_exists(EVIL // '/t.dat'), 'pack: "t.dat " is not unpacked as "t.dat"')
        call craft_pack('utest_sqr_pack_evil.sqr', 1, 'notes.txt')
        emsg = ''
        call db_unpack('utest_sqr_pack_evil.sqr', EVIL, rs, emsg)
        call check(index(emsg, 'cannot unpack: "notes.txt" is not a database file') == 1, &
                   'pack: the refusal names the file')

        ! A7: sizes are bounded by the container file before anything is
        ! allocated from them.
        open(newunit=u, file='utest_sqr_pack_evil.sqr', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) 'SQRP', 2_int32, SQR_BOM, 1000000_int32, 0_int32
        write(u) 5_int32, 't.dat', 1_int64, 0_int64
        write(u) 'x'
        close(u)
        call db_unpack('utest_sqr_pack_evil.sqr', EVIL, rs)
        call check(rs == SQR_ERR .and. .not. c_path_exists(EVIL), &
                   'pack: a file count the container cannot hold is refused')
        open(newunit=u, file='utest_sqr_pack_evil.sqr', access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        write(u) 'SQRP', 2_int32, SQR_BOM, 1_int32, 0_int32
        write(u) 5_int32, 't.dat', 1000000000_int64, 0_int64
        write(u) 'x'
        close(u)
        call db_unpack('utest_sqr_pack_evil.sqr', EVIL, rs)
        call check(rs == SQR_ERR .and. .not. c_path_exists(EVIL), &
                   'pack: a file size larger than the container is refused')

        ios = c_rmtree(PDIR); ios = c_rmtree(PDIR2); ios = c_rmtree(EVIL)
        ios = c_remove(PFILE); ios = c_remove('utest_sqr_pack_evil.sqr')
    end subroutine

    ! Unpack a crafted version-1 container (valid checksum) holding the
    ! '|'-separated names into `dir`: refused with `expect`, nothing left.
    subroutine craft_refused(dir, spec, expect)
        character(len=*), intent(in) :: dir, spec
        integer,          intent(in) :: expect
        integer :: rs, ios
        ios = c_rmtree(dir)
        call craft_pack('utest_sqr_pack_evil.sqr', 1, spec)
        call db_unpack('utest_sqr_pack_evil.sqr', dir, rs)
        call check(rs == expect .and. .not. c_path_exists(dir) .and. &
                   .not. c_path_exists(dir // '.unpack-tmp'), &
                   'pack: crafted container "' // spec // '" refused, nothing left')
    end subroutine

    ! Write a container of version `ver` holding the '|'-separated names of
    ! `spec`, each a one-byte file 'x', with a valid checksum for its version.
    subroutine craft_pack(path, ver, spec)
        character(len=*), intent(in) :: path, spec
        integer,          intent(in) :: ver
        character(len=:), allocatable :: toc, payload, nm
        integer :: p, q, n, cks
        toc = ''
        payload = ''
        n = 0
        p = 1
        names: do
            q = index(spec(p:), '|')
            if (q == 0) then
                nm = spec(p:)
            else
                nm = spec(p:p+q-2)
            end if
            toc = toc // i32b(len(nm)) // nm // i64b(1_int64) // i64b(int(n, int64))
            payload = payload // 'x'
            n = n + 1
            if (q == 0) exit names
            p = p + q
        end do names
        if (ver >= 2) then
            cks = cksum_of(toc // payload)
        else
            cks = cksum_of(payload)
        end if
        call put_bytes(path, 'SQRP' // i32b(ver) // i32b(SQR_BOM) // i32b(n) // &
                       i32b(cks) // toc // payload)
    end subroutine

    ! Set sku/region/qty into a freshly-cleared row buffer.
    subroutine nk_row(db, ti, buf, sku, reg, qty)
        type(db_t),       intent(in)    :: db
        integer,          intent(in)    :: ti
        character(len=:), allocatable, intent(inout) :: buf
        character(len=*), intent(in)    :: sku
        integer(int32),   intent(in)    :: reg, qty
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_char(buf, db%tables(ti)%cols(1), sku)
        call row_set_int (buf, db%tables(ti)%cols(2), reg)
        call row_set_int (buf, db%tables(ti)%cols(3), qty)
    end subroutine

    ! Step 7b: quiesce/resume and the live pack built on them.  Quiescing
    ! flushes and drops the file units while the handle and its exclusive lock
    ! stay held, so another reader — db_pack's stream units — can read the
    ! directory without breaking the one-file-one-unit rule.
    ! E9: a db_resume that failed part-way can be retried; the retry opens
    ! only what is still closed.
    subroutine test_resume_retry()
        character(len=*), parameter :: RDIR = 'utest_sqr_resume_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti, dunit
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(RDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'tag'; c(2)%dtype = DT_CHAR ; c(2)%csize = 4
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'items', c, rs, emsg)
        ti = db_table_index(db, 'items')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 10_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), 't1')
        call db_insert(db, 'items', buf, rid, rs)
        call db_create_index(db, 'items', 'id', rs)
        call db_quiesce(db, rs, emsg)
        ios = c_rename(RDIR // '/items__i1.idx', RDIR // '/items__i1.idx.away')
        call db_resume(db, rs, emsg)
        call check(rs /= SQR_OK .and. db%quiesced, 'resume retry: a missing index fails, still quiesced')
        dunit = db%tables(ti)%unit
        call check(dunit /= -1, 'resume retry: the data file was reopened before the failure')
        ios = c_rename(RDIR // '/items__i1.idx.away', RDIR // '/items__i1.idx')
        call db_resume(db, rs, emsg)
        call check(rs == SQR_OK .and. .not. db%quiesced, 'resume retry: the second resume succeeds')
        call check(db%tables(ti)%unit == dunit, 'resume retry: the data unit is not reopened')
        call db_verify(db, 'items', rs, emsg)
        call check(rs == SQR_OK, 'resume retry: verify passes')
        call row_set_int (buf, db%tables(ti)%cols(1), 20_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), 't2')
        call db_insert(db, 'items', buf, rid, rs)
        call check(rs == SQR_OK, 'resume retry: an insert works')
        call db_find_by_int(db, 'items', 'id', 20_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'resume retry: an indexed find works')
        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine

    ! E10: undo/redo on a closed handle; db_open on an open one.
    subroutine test_handle_misuse()
        character(len=*), parameter :: MDIR = 'utest_sqr_misuse_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(MDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, MDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), 1_int32)
        call db_begin(db, rs, label='first')
        call db_insert(db, 't', buf, rid, rs)
        call db_commit(db, rs)
        call db_undo(db, rs)                   ! leaves one redo step
        call db_redo(db, rs)                   ! and back: one undo step
        call check(db_can_undo(db), 'misuse: a gesture can be undone')
        call db_close(db)
        call check(.not. db_can_undo(db) .and. .not. db_can_redo(db), &
                   'misuse: a closed handle has no undo or redo')
        call db_undo(db, rs)
        call check(rs == SQR_INVALID, 'misuse: undo on a closed handle is SQR_INVALID')
        call db_redo(db, rs)
        call check(rs == SQR_INVALID, 'misuse: redo on a closed handle is SQR_INVALID')

        call db_open(db, MDIR, rs, emsg)
        ti = db_table_index(db, 't')
        call row_set_int(buf, db%tables(ti)%cols(1), 2_int32)
        call db_insert(db, 't', buf, rid, rs)
        emsg = ''
        call db_open(db, MDIR, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'already open') > 0, &
                   'misuse: db_open on an open handle is SQR_INVALID')
        call check(db%opened .and. db_row_count(db, 't') == 2, 'misuse: the open handle is untouched')
        call db_get(db, 't', rid, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 2_int32, &
                   'misuse: its row is still readable')
        call row_set_int(buf, db%tables(ti)%cols(1), 3_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_OK, 'misuse: and it still takes an insert')
        call db_close(db, rs)
        call check(rs == SQR_OK, 'misuse: it closes')
        call db_open(db, MDIR, rs, emsg)
        call check(rs == SQR_OK .and. db_row_count(db, 't') == 3, 'misuse: and opens again')
        call db_close(db)
        ios = c_rmtree(MDIR)
    end subroutine

    ! E12: a structural operation refused for a bad argument keeps the history.
    subroutine test_history_kept_on_refusal()
        character(len=*), parameter :: HDIR = 'utest_sqr_histkeep_db'
        type(db_t), target :: db
        type(column_t) :: c(2), extra
        integer :: rs, ios, ti, ep, bad
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(HDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'n' ; c(2)%dtype = DT_INT; c(2)%csize = 4
        extra%name = 'x'; extra%dtype = DT_INT; extra%csize = 4
        call db_open(db, HDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call db_create_index(db, 't', 'id', rs)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), 1_int32)
        call db_begin(db, rs, label='g')
        call db_insert(db, 't', buf, rid, rs)
        call db_commit(db, rs)
        call check(db_can_undo(db), 'history kept: the gesture is undoable')
        ep = db%hist%epoch
        bad = 0
        call db_drop_table(db, 'nosuch', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_compact(db, 'nosuch', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_add_column(db, 'nosuch', extra, rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_drop_column(db, 'nosuch', 'n', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_drop_column(db, 't', 'nosuch', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_create_index(db, 'nosuch', 'n', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_create_index(db, 't', 'nosuch', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_create_index(db, 't', 'id', rs)
        if (rs == SQR_OK) bad = bad + 1   ! exists already
        call db_drop_index(db, 'nosuch', 'id', rs)
        if (rs == SQR_OK) bad = bad + 1
        call db_drop_index(db, 't', 'n', rs)
        if (rs == SQR_OK) bad = bad + 1   ! no such index
        call check(bad == 0, 'history kept: each bad request is refused')
        call check(db_can_undo(db) .and. db%hist%epoch == ep, &
                   'history kept: refusals leave the undo step and the epoch')
        call db_add_column(db, 't', extra, rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), &
                   'history kept: a valid structural change empties it')
        call db_close(db)
        ios = c_rmtree(HDIR)
    end subroutine

    subroutine test_quiesce()
        character(len=*), parameter :: QDIR  = 'utest_sqr_quiesce_db'
        character(len=*), parameter :: QDIR2 = 'utest_sqr_quiesce_db2'
        character(len=*), parameter :: QFILE = 'utest_sqr_quiesce.sqr'
        type(db_t), target :: db, restored
        type(column_t) :: c(3)
        integer :: rs, ios, ti, k
        integer(int32) :: rid
        character(len=:), allocatable :: buf, txt
        character(len=16) :: body
        character(len=128) :: emsg

        ios = c_rmtree(QDIR); ios = c_rmtree(QDIR2); ios = c_remove(QFILE)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'tag' ; c(2)%dtype = DT_CHAR ; c(2)%csize = 4
        c(3)%name = 'body'; c(3)%dtype = DT_TEXT ; c(3)%csize = SQR_TEXT_DESC
        call db_open(db, QDIR, rs, emsg)
        call db_create_table(db, 'items', c, rs, emsg)
        ti = db_table_index(db, 'items')
        do k = 1, 3
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int (buf, db%tables(ti)%cols(1), int(10*k, int32))
            write(body, '(a1,i1)') 't', k
            call row_set_char(buf, db%tables(ti)%cols(2), trim(body))
            call db_insert(db, 'items', buf, rid, rs)
            write(body, '(a5,i1)') 'body-', k
            call db_set_text(db, 'items', rid, 'body', trim(body), rs)
        end do
        call db_create_index(db, 'items', 'id', rs)

        ! ---- quiesce / resume as a pair ----
        call db_quiesce(db, rs, emsg)
        call check(rs == SQR_OK .and. db%quiesced, 'quiesce: succeeds and marks the handle')
        ti = db_table_index(db, 'items')
        call check(db%tables(ti)%unit == -1 .and. db%tables(ti)%blob_unit == -1, &
                   'quiesce: data and blob units released')
        call check(db%opened, 'quiesce: the handle stays open')
        call db_quiesce(db, rs, emsg)
        call check(rs == SQR_INVALID, 'quiesce: refused on an already-quiesced handle')
        ! Why db_pack_live exists: flock treats a second open of the lock file
        ! as an independent holder, so the offline packer is denied by this
        ! handle's own exclusive lock even from inside the same process.
        call db_pack(QDIR, QFILE, rs)
        call check(rs == SQR_LOCKED, 'quiesce: offline db_pack is still locked out by this handle')

        call db_resume(db, rs, emsg)
        call check(rs == SQR_OK .and. .not. db%quiesced, 'resume: succeeds and clears the mark')
        call db_resume(db, rs, emsg)
        call check(rs == SQR_INVALID, 'resume: refused when not quiesced')
        call db_find_by_int(db, 'items', 'id', 20_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'resume: reopened index still resolves')
        call db_get_text(db, 'items', 2_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'body-2', 'resume: reopened blob still reads')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 40_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), 't4')
        call db_insert(db, 'items', buf, rid, rs)
        call check(rs == SQR_OK, 'resume: reopened data file still accepts writes')

        ! ---- the live pack ----
        call db_pack_live(db, QFILE, rs, emsg)
        call check(rs == SQR_OK, 'pack_live: packs an open database')
        call check(c_path_exists(QFILE), 'pack_live: container created')
        call db_unpack(QFILE, QDIR2, rs)
        call check(rs == SQR_OK, 'pack_live: the container unpacks')
        call db_open(restored, QDIR2, rs, emsg)
        call check(rs == SQR_OK, 'pack_live: the unpacked database opens')
        call check(db_row_count(restored, 'items') == 4, 'pack_live: every live row is in the archive')
        call db_find_by_int(restored, 'items', 'id', 40_int32, rid, rs)
        call check(rs == SQR_OK, 'pack_live: the archived index resolves the newest row')
        call db_get_text(restored, 'items', 2_int32, 'body', txt, rs)
        call check(rs == SQR_OK .and. txt == 'body-2', 'pack_live: TEXT/blob survives the live pack')
        call db_verify(restored, 'items', rs, emsg)
        call check(rs == SQR_OK, 'pack_live: db_verify clean on the archive')
        call db_close(restored)
        ! The source handle is still usable — that is the whole point.
        call db_find_by_int(db, 'items', 'id', 30_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'pack_live: the source handle survives')

        ! A transaction in flight is not a committed state: refuse both.
        call db_begin(db, rs)
        call db_quiesce(db, rs, emsg)
        call check(rs == SQR_INVALID, 'quiesce: refused while a transaction is open')
        call db_pack_live(db, QFILE, rs, emsg)
        call check(rs == SQR_INVALID, 'pack_live: refused while a transaction is open')
        call db_rollback(db, rs)

        call db_close(db)
        ios = c_rmtree(QDIR); ios = c_rmtree(QDIR2); ios = c_remove(QFILE)
    end subroutine

    subroutine test_natural_keys()
        character(len=*), parameter :: NK = 'utest_sqr_nk_db'
        character(len=6), parameter :: KEYCOLS(2) = [character(len=6) :: 'sku', 'region']
        type(db_t), target :: db
        type(column_t) :: c(3), d(1)
        integer :: rs, ios, ti, td
        integer(int32) :: rid
        character(len=:), allocatable :: buf, krow
        character(len=128) :: emsg

        ios = c_rmtree(NK)
        c(1)%name = 'sku'   ; c(1)%dtype = DT_CHAR; c(1)%csize = 8
        c(2)%name = 'region'; c(2)%dtype = DT_INT ; c(2)%csize = 4
        c(3)%name = 'qty'   ; c(3)%dtype = DT_INT ; c(3)%csize = 4
        call db_open(db, NK, rs, emsg)
        call db_create_table(db, 'inv', c, rs, emsg)
        call check(rs == SQR_OK, 'nk: create table')
        ti = db_table_index(db, 'inv')

        call db_create_index(db, 'inv', KEYCOLS, rs, unique=.true.)
        call check(rs == SQR_OK, 'nk: composite unique index')
        call db_create_index(db, 'inv', 'qty', rs)
        call check(rs == SQR_OK, 'nk: single-col non-unique index (regression)')

        call nk_row(db, ti, buf, 'AAA', 1_int32, 10_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_OK, 'nk: insert AAA/1')
        call nk_row(db, ti, buf, 'AAA', 2_int32, 20_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_OK, 'nk: insert AAA/2 (same sku, different region)')
        call nk_row(db, ti, buf, 'BBB', 1_int32, 30_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_OK, 'nk: insert BBB/1')

        ! Duplicate composite key rejected, row not added.
        call nk_row(db, ti, buf, 'AAA', 1_int32, 99_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_DUP .and. rid == 0, 'nk: duplicate composite key -> SQR_DUP')
        call check(db%tables(ti)%live_count == 3, 'nk: rejected dup did not add a row')

        ! by-key get resolves the right row.
        call nk_row(db, ti, krow, 'BBB', 1_int32, 0_int32)
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(3)) == 30, &
                   'nk: get_by_key BBB/1 -> qty 30')

        ! Update by key (change qty); key columns unchanged.
        call nk_row(db, ti, buf, 'BBB', 1_int32, 77_int32)
        call db_update_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_OK, 'nk: update_by_key BBB/1 qty -> 77')
        call db_get_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(3)) == 77, &
                   'nk: update_by_key persisted')

        ! Update that would collide with another row's composite key.
        call db_get_by_key(db, 'inv', KEYCOLS, krow, buf, rs)   ! BBB/1 row
        call row_set_char(buf, db%tables(ti)%cols(1), 'AAA')
        call row_set_int (buf, db%tables(ti)%cols(2), 1_int32)  ! -> AAA/1, taken
        call db_update_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_DUP, 'nk: update into existing composite key -> SQR_DUP')

        ! by-key delete.
        call nk_row(db, ti, krow, 'AAA', 2_int32, 0_int32)
        call db_delete_by_key(db, 'inv', KEYCOLS, krow, rs)
        call check(rs == SQR_OK, 'nk: delete_by_key AAA/2')
        call db_get_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'nk: deleted key no longer found')

        ! The natural key is stable across a compaction (row_ids are not).
        call db_compact(db, 'inv', rs)
        call check(rs == SQR_OK, 'nk: compact OK')
        call nk_row(db, ti, krow, 'BBB', 1_int32, 0_int32)
        call db_get_by_key(db, 'inv', KEYCOLS, krow, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(3)) == 77, &
                   'nk: by-key survives compaction')

        ! Reinserting a freed key after the delete is allowed.
        call nk_row(db, ti, buf, 'AAA', 2_int32, 55_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_OK, 'nk: reinsert freed composite key')

        call db_close(db)

        ! Persistence: composite unique index reloads and still enforces.
        call db_open(db, NK, rs, emsg)
        ti = db_table_index(db, 'inv')
        call check(db%tables(ti)%indices(1)%ncols == 2 .and. &
                   db%tables(ti)%indices(1)%unique, 'nk: composite unique persisted')
        call nk_row(db, ti, buf, 'BBB', 1_int32, 1_int32)
        call db_insert(db, 'inv', buf, rid, rs)
        call check(rs == SQR_DUP, 'nk: uniqueness still enforced after reopen')

        ! A unique index must not be built over duplicate live data.
        d(1)%name = 'v'; d(1)%dtype = DT_INT; d(1)%csize = 4
        call db_create_table(db, 'dup', d, rs, emsg)
        td = db_table_index(db, 'dup')
        call row_alloc(buf, db%tables(td)%record_size)
        call row_set_int(buf, db%tables(td)%cols(1), 5_int32)
        call db_insert(db, 'dup', buf, rid, rs)
        call db_insert(db, 'dup', buf, rid, rs)
        call db_create_index(db, 'dup', 'v', rs, unique=.true.)
        call check(rs == SQR_DUP, 'nk: unique index on duplicate data -> SQR_DUP')
        call check(db%tables(td)%nindices == 0, 'nk: failed unique index rolled back')
        ! Non-unique index over the same data is fine.
        call db_create_index(db, 'dup', 'v', rs)
        call check(rs == SQR_OK .and. db%tables(td)%nindices == 1, &
                   'nk: non-unique index over duplicates OK')

        ! A key-bearing buffer shorter than a record: extract_key slices the
        ! member columns at their record offsets, so this used to read past the
        ! buffer's end (undefined, and an ASan overflow in an -O3 build). All
        ! three by-key entry points share resolve_by_key, so all three are
        ! covered by the one check.
        short_keyrow: block
            character(len=:), allocatable :: shortk, obuf
            ti = db_table_index(db, 'inv')
            call row_alloc(obuf, db%tables(ti)%record_size)
            call row_alloc(shortk, 4)          ! well short of a record
            call db_get_by_key(db, 'inv', KEYCOLS, shortk, obuf, rs)
            call check(rs == SQR_INVALID, 'nk: get_by_key with a short keyrow rejected')
            call db_update_by_key(db, 'inv', KEYCOLS, shortk, obuf, rs)
            call check(rs == SQR_INVALID, 'nk: update_by_key with a short keyrow rejected')
            call db_delete_by_key(db, 'inv', KEYCOLS, shortk, rs)
            call check(rs == SQR_INVALID, 'nk: delete_by_key with a short keyrow rejected')
            call check(db%tables(ti)%live_count == 3, 'nk: rejected by-key ops changed nothing')
        end block short_keyrow

        call db_close(db)
        ios = c_rmtree(NK)
    end subroutine

    ! Exercises validation / diagnostic paths the feature tests skip:
    ! empty column set, oversized record, invalid column name, and the
    ! raise() no-stat branch (library-must-not-crash contract).
    subroutine test_coverage_paths()
        character(len=*), parameter :: CV = 'utest_sqr_cov_db'
        type(db_t), target :: db
        type(column_t), allocatable :: zc(:), big(:)
        type(column_t) :: bad(1)
        integer :: rs, ios, i
        character(len=128) :: emsg

        ios = c_rmtree(CV)
        call db_open(db, CV, rs, emsg)

        ! Zero-column table.
        allocate(zc(0))
        call db_create_table(db, 'empty', zc, rs, emsg)
        call check(rs == SQR_INVALID, 'cov: zero-column table rejected')

        ! Record larger than SQR_MAX_RECORD (17 x 65536 > 1 MiB).
        allocate(big(17))
        do i = 1, 17
            write(big(i)%name, '(a,i0)') 'c', i
            big(i)%dtype = DT_CHAR
            big(i)%csize = 65536
        end do
        call db_create_table(db, 'huge', big, rs, emsg)
        call check(rs == SQR_INVALID, 'cov: oversized record rejected')

        ! One CHAR column a byte wider than SQR_MAX_CHAR; the message quotes
        ! the limit.
        char_width: block
            type(column_t) :: wide(1)
            wide(1)%name  = 'c'
            wide(1)%dtype = DT_CHAR
            wide(1)%csize = SQR_MAX_CHAR + 1
            emsg = ''
            call db_create_table(db, 'wide', wide, rs, emsg)
            call check(rs == SQR_INVALID, 'cov: CHAR wider than SQR_MAX_CHAR rejected')
            call check(index(emsg, '1..65536') > 0, 'cov: CHAR width message quotes the limit')
            wide(1)%csize = SQR_MAX_CHAR
            call db_create_table(db, 'widest', wide, rs, emsg)
            call check(rs == SQR_OK, 'cov: CHAR of exactly SQR_MAX_CHAR accepted')
        end block char_width

        ! A negative buffer size: repeat()'s ncopies must be non-negative, so
        ! this was a runtime abort rather than a buffer that plainly cannot
        ! hold a row (which the write paths then reject on length).
        neg_alloc: block
            character(len=:), allocatable :: nbuf
            call row_alloc(nbuf, -5)
            call check(allocated(nbuf) .and. len(nbuf) == 0, &
                       'cov: row_alloc with a negative size gives an empty buffer')
        end block neg_alloc

        ! Invalid column name exercises valid_name's '/' and '..' scans.
        bad(1)%name = 'a/b'; bad(1)%dtype = DT_INT; bad(1)%csize = 4
        call db_create_table(db, 'badcol', bad, rs, emsg)
        call check(rs == SQR_INVALID, 'cov: invalid column name rejected')

        call db_close(db)

        ! A failing public call made WITHOUT stat must route through raise()
        ! to error_unit and RETURN (not error stop). Reaching the next line
        ! is the assertion. ('..' component is rejected before any fs access.)
        call db_open(db, '../badesc')
        call check(.not. db%opened, 'cov: raise no-stat path returns, no crash')

        ios = c_rmtree(CV)
    end subroutine

    ! A multi-MB blob must travel through db_get_text and db_compact without
    ! a stack-automatic buffer sized by the stored length (those are now heap
    ! allocatables). 1 MiB is well past any sane stack frame yet quick.
    subroutine test_large_blob()
        character(len=*), parameter :: LB_DIR = 'utest_sqr_blob_db'
        integer, parameter :: NBIG = 1048576          ! 1 MiB
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios
        integer(int32) :: rid
        character(len=:), allocatable :: buf, got, big
        character(len=128) :: emsg_blob

        ios = c_rmtree(LB_DIR)
        big = repeat('Z', NBIG)
        c(1)%name = 'id'   ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'body' ; c(2)%dtype = DT_TEXT ; c(2)%csize = SQR_TEXT_DESC

        call db_open(db, LB_DIR, rs, emsg_blob)
        call db_create_table(db, 'big', c, rs, emsg_blob)
        call check(rs == SQR_OK, 'blob: create text table')
        call row_alloc(buf, db%tables(db_table_index(db, 'big'))%record_size)
        call row_set_int(buf, db%tables(db_table_index(db, 'big'))%cols(1), 1_int32)
        call db_insert(db, 'big', buf, rid, rs)
        call db_set_text(db, 'big', 1_int32, 'body', big, rs)
        call check(rs == SQR_OK, 'blob: set 1 MiB text')

        ! Heap read buffer in db_get_text.
        call db_get_text(db, 'big', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. len(got) == NBIG .and. got == big, &
                   'blob: 1 MiB text round-trips (heap read buffer)')

        ! Heap transfer buffer in db_compact.
        call db_compact(db, 'big', rs)
        call check(rs == SQR_OK, 'blob: compact table holding 1 MiB blob')
        call db_get_text(db, 'big', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. len(got) == NBIG .and. got == big, &
                   'blob: 1 MiB text intact after compact')

        call db_close(db)
        ios = c_rmtree(LB_DIR)
    end subroutine

    subroutine test_text()
        character(len=*), parameter :: TXT_DIR = 'utest_sqr_text_db'
        type(db_t), target :: db
        type(column_t) :: c(2), cbad(1)
        integer :: rs, ios
        integer(int32) :: rid
        character(len=:), allocatable :: buf, got
        character(len=128) :: emsg
        character(len=5000) :: LONG

        LONG = repeat('A', 5000)
        ios = c_rmtree(TXT_DIR)

        ! --- DT_TEXT column validation ---
        cbad(1)%name = 'b'; cbad(1)%dtype = DT_TEXT; cbad(1)%csize = 5
        call db_open(db, TXT_DIR, rs, emsg)
        call db_create_table(db, 't_badtext', cbad, rs, emsg)
        call check(rs == SQR_INVALID, 'reject DT_TEXT csize /= SQR_TEXT_DESC')

        c(1)%name = 'id'   ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'body' ; c(2)%dtype = DT_TEXT ; c(2)%csize = SQR_TEXT_DESC
        call db_create_table(db, 'docs', c, rs, emsg)
        call check(rs == SQR_OK, 'create table with text column')

        call row_alloc(buf, db%tables(db_table_index(db, 'docs'))%record_size)
        call row_set_int(buf, db%tables(db_table_index(db, 'docs'))%cols(1), 1_int32)
        call db_insert(db, 'docs', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'insert text row 1')

        ! Unset text reads as empty.
        call db_get_text(db, 'docs', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == '', 'unset text is empty')

        call db_set_text(db, 'docs', 1_int32, 'body', 'hello world', rs)
        call check(rs == SQR_OK, 'set_text short')
        call db_get_text(db, 'docs', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == 'hello world', 'get_text short round-trips')

        ! Overwrite with empty string.
        call db_set_text(db, 'docs', 1_int32, 'body', '', rs)
        call db_get_text(db, 'docs', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. len(got) == 0, 'set_text empty then get is empty')

        ! Long string in a fresh row.
        call row_set_int(buf, db%tables(db_table_index(db, 'docs'))%cols(1), 2_int32)
        call db_insert(db, 'docs', buf, rid, rs)
        call check(rid == 2, 'insert text row 2')
        call db_set_text(db, 'docs', 2_int32, 'body', LONG, rs)
        call db_get_text(db, 'docs', 2_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. len(got) == 5000 .and. got == LONG, &
                   'long text (5000) round-trips')

        ! Row 1 still independent.
        call db_get_text(db, 'docs', 1_int32, 'body', got, rs)
        call check(got == '', 'row 1 text unaffected by row 2')

        ! db_update preserves the existing TEXT (blob) descriptor: changing a
        ! non-text column must not lose the row's text.
        call row_alloc(buf, db%tables(db_table_index(db, 'docs'))%record_size)
        call row_set_int(buf, db%tables(db_table_index(db, 'docs'))%cols(1), 99_int32)
        call db_update(db, 'docs', 2_int32, buf, rs)
        call check(rs == SQR_OK, 'update text-table row (non-text column)')
        call db_get(db, 'docs', 2_int32, buf, rs)
        call check(rs == SQR_OK .and. &
                   row_get_int(buf, db%tables(db_table_index(db, 'docs'))%cols(1)) == 99, &
                   'updated id visible on text-table row')
        call db_get_text(db, 'docs', 2_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == LONG, 'TEXT preserved across db_update')

        ! Indexing a text column is rejected.
        call db_create_index(db, 'docs', 'body', rs)
        call check(rs == SQR_INVALID, 'create_index on text column rejected')

        call db_close(db)

        ! --- Persistence: blob_next derived from file size on reopen ---
        call db_open(db, TXT_DIR, rs, emsg)
        call check(rs == SQR_OK, 'reopen db with text/blob')
        call db_get_text(db, 'docs', 2_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == LONG, 'long text persisted across reopen')
        call db_set_text(db, 'docs', 1_int32, 'body', 'after reopen', rs)
        call db_get_text(db, 'docs', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == 'after reopen', 'append after reopen works')
        call db_get_text(db, 'docs', 2_int32, 'body', got, rs)
        call check(got == LONG, 'row 2 intact after post-reopen append')
        call db_close(db)

        ! --- Read-only blocks db_set_text ---
        call db_open(db, TXT_DIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_OK, 'reopen text db read-only')
        call db_get_text(db, 'docs', 2_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == LONG, 'readonly db_get_text works')
        call db_set_text(db, 'docs', 1_int32, 'body', 'nope', rs)
        call check(rs == SQR_READONLY, 'readonly db_set_text blocked')
        call db_close(db)

        ! --- TEXT resolved inside a db_scan callback (db now passed to cb) ---
        call db_open(db, TXT_DIR, rs, emsg)
        block
            type(scan_text_ctx_t) :: tctx
            call db_scan(db, 'docs', scan_concat_text, tctx, rs)
            call check(rs == SQR_OK .and. tctx%count == 2, &
                       'db_scan callback visited both text rows')
            call check(tctx%joined == 'after reopen|' // trim(LONG) // '|', &
                       'db_scan callback resolved TEXT via the db handle')
        end block
        call db_close(db)

        ! --- Drop removes the blob file and persists ---
        call db_open(db, TXT_DIR, rs, emsg)
        call db_drop_table(db, 'docs', rs)
        call check(rs == SQR_OK, 'drop text table')
        block
            logical :: ex
            inquire(file=TXT_DIR // '/docs.blob', exist=ex)
            call check(.not. ex, 'blob file deleted on drop')
        end block
        call db_close(db)
        call db_open(db, TXT_DIR, rs, emsg)
        call check(db%ntables == 0, 'text table drop persisted')
        call db_close(db)

        ios = c_rmtree(TXT_DIR)
    end subroutine

    ! Regression cover for the 2026-06-16 High findings:
    !   #1 TEXT/NULL must agree — a column set NULL reads as absent, never the
    !      old blob; setting text clears the NULL bit; compact drops NULL blobs.
    !   #2 a NaN real lookup/range bound matches nothing rather than a wrong row.
    !   #3 an equality overload against an index of the wrong dtype is rejected.
    subroutine test_high_findings()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        character(len=*), parameter :: HF_DIR = 'utest_sqr_hf_db'
        type(db_t), target :: db
        type(column_t) :: c(4)
        type(db_cursor_t) :: cur
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf, got
        character(len=128) :: emsg
        real(real64) :: nan

        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        ios = c_rmtree(HF_DIR)

        c(1)%name = 'i'    ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'r'    ; c(2)%dtype = DT_REAL ; c(2)%csize = 8
        c(3)%name = 's'    ; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
        c(4)%name = 'body' ; c(4)%dtype = DT_TEXT ; c(4)%csize = SQR_TEXT_DESC
        call db_open(db, HF_DIR, rs, emsg)
        call db_create_table(db, 'mix', c, rs, emsg)
        call check(rs == SQR_OK, 'hf: create mixed table')
        ti = db_table_index(db, 'mix')
        call db_create_index(db, 'mix', 'i', rs)
        call db_create_index(db, 'mix', 'r', rs)
        call db_create_index(db, 'mix', 's', rs)

        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 7_int32)
        call row_set_real(buf, db%tables(ti)%cols(2), 2.5_real64)
        call row_set_char(buf, db%tables(ti)%cols(3), 'alpha')
        call db_insert(db, 'mix', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'hf: insert row')
        call db_set_text(db, 'mix', 1_int32, 'body', 'hello', rs)
        call check(rs == SQR_OK, 'hf: set text')

        ! --- #1 set TEXT to NULL via db_update, must not return stale text ---
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 7_int32)
        call row_set_real(buf, db%tables(ti)%cols(2), 2.5_real64)
        call row_set_char(buf, db%tables(ti)%cols(3), 'alpha')
        call row_set_null(buf, db%tables(ti)%cols(4))   ! body := NULL
        call db_update(db, 'mix', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'hf: update body to NULL')
        call db_get_text(db, 'mix', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == '', 'hf: NULL text reads as absent (not stale)')
        call db_get(db, 'mix', 1_int32, buf, rs)
        call check(row_is_null(buf, db%tables(ti)%cols(4)), 'hf: stored row marks body NULL')

        ! --- #1 db_set_text on a NULL column clears the NULL bit ---
        call db_set_text(db, 'mix', 1_int32, 'body', 'again', rs)
        call check(rs == SQR_OK, 'hf: set_text on NULL column')
        call db_get_text(db, 'mix', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == 'again', 'hf: text restored after set on NULL')
        call db_get(db, 'mix', 1_int32, buf, rs)
        call check(.not. row_is_null(buf, db%tables(ti)%cols(4)), 'hf: set_text cleared NULL bit')

        ! --- #1 compact drops a NULL column's blob and keeps semantics ---
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), 7_int32)
        call row_set_real(buf, db%tables(ti)%cols(2), 2.5_real64)
        call row_set_char(buf, db%tables(ti)%cols(3), 'alpha')
        call row_set_null(buf, db%tables(ti)%cols(4))
        call db_update(db, 'mix', 1_int32, buf, rs)
        call db_compact(db, 'mix', rs)
        call check(rs == SQR_OK, 'hf: compact with a NULL text column')
        call db_get_text(db, 'mix', 1_int32, 'body', got, rs)
        call check(rs == SQR_OK .and. got == '', 'hf: NULL text still absent after compact')

        ! --- #3 equality overload against a wrong-dtype index is rejected ---
        call db_find_by_real(db, 'mix', 'i', 2.5_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'hf: find_by_real on int index rejected')
        call db_find_by_int(db, 'mix', 'r', 7_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'hf: find_by_int on real index rejected')
        call db_find_by_char(db, 'mix', 'i', 'alpha', rid, rs)
        call check(rs == SQR_NOT_FOUND, 'hf: find_by_char on int index rejected')
        ! Correct overloads still resolve.
        call db_find_by_int(db, 'mix', 'i', 7_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'hf: matching int overload still works')

        ! --- #2 NaN equality lookup matches nothing, not a wrong row ---
        call db_find_by_real(db, 'mix', 'r', nan, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'hf: NaN real lookup matches nothing')
        ! --- #2 NaN range bound is rejected ---
        call db_find_range(db, 'mix', 'r', nan, 9.9_real64, cur, rs)
        call check(rs == SQR_INVALID, 'hf: NaN range low bound rejected')
        call db_find_range(db, 'mix', 'r', 0.0_real64, nan, cur, rs)
        call check(rs == SQR_INVALID, 'hf: NaN range high bound rejected')

        call db_close(db)
        ios = c_rmtree(HF_DIR)
    end subroutine

    ! Scan callback: count rows and sum ages.
    subroutine scan_count_and_sum(scan_db, rid, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        stop = .false.
        select type (ctx)
        type is (scan_ctx_t)
            ctx%count = ctx%count + 1
            ctx%sum   = ctx%sum   + row_get_int(buf, ctx%age_col)
        end select
    end subroutine

    subroutine scan_stop_after_one(scan_db, rid, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        select type (ctx)
        type is (scan_ctx_t)
            ctx%count = ctx%count + 1
        end select
        stop = .true.
    end subroutine

    ! Resolves a DT_TEXT column for each row *inside* the scan, via the db
    ! handle now threaded to the callback — concatenates all bodies in scan
    ! order. Exercises the TEXT-in-db_scan API.
    subroutine scan_concat_text(scan_db, rid, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        character(len=:), allocatable :: val
        integer :: rs
        stop = .false.
        select type (ctx)
        type is (scan_text_ctx_t)
            call db_get_text(scan_db, 'docs', rid, 'body', val, rs)
            if (rs == SQR_OK) then
                if (.not. allocated(ctx%joined)) ctx%joined = ''
                ctx%joined = ctx%joined // trim(val) // '|'
                ctx%count  = ctx%count + 1
            end if
        end select
    end subroutine

    ! ===== #6 NULL columns =====

    subroutine ins_null(db, ti, id, val, valnull, nm, buf)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: id, val
        logical,          intent(in)    :: valnull
        character(len=*), intent(in)    :: nm
        character(len=:), allocatable, intent(inout) :: buf
        integer :: rs
        integer(int32) :: rid
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), id)
        if (valnull) then
            call row_set_null(buf, db%tables(ti)%cols(2))
        else
            call row_set_int(buf, db%tables(ti)%cols(2), val)
        end if
        call row_set_char(buf, db%tables(ti)%cols(3), nm)
        call db_insert(db, 't', buf, rid, rs)
    end subroutine

    subroutine upd_null(db, ti, rid, val, valnull, nm, buf)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: rid, val
        logical,          intent(in)    :: valnull
        character(len=*), intent(in)    :: nm
        character(len=:), allocatable, intent(inout) :: buf
        integer :: rs
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), rid)
        if (valnull) then
            call row_set_null(buf, db%tables(ti)%cols(2))
        else
            call row_set_int(buf, db%tables(ti)%cols(2), val)
        end if
        call row_set_char(buf, db%tables(ti)%cols(3), nm)
        call db_update(db, 't', rid, buf, rs)
    end subroutine

    subroutine drain_count(db, cur, n)
        type(db_t),        intent(inout) :: db
        type(db_cursor_t), intent(inout) :: cur
        integer,           intent(out)   :: n
        integer :: rs, ti
        integer(int32) :: rid
        logical :: ok
        character(len=:), allocatable :: buf
        ti = cur%ti
        allocate(character(len=db%tables(ti)%record_size) :: buf)
        n = 0
        do
            call db_cursor_next(db, cur, rid, buf, ok, rs)
            if (.not. ok) exit
            n = n + 1
        end do
    end subroutine

    subroutine test_null_columns()
        character(len=*), parameter :: NDIR = 'utest_sqr_null_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, n, rsz
        integer(int32) :: rid, ids(8)
        character(len=:), allocatable :: buf
        type(db_cursor_t) :: cur
        character(len=128) :: emsg

        ios = c_rmtree(NDIR)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'val' ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
        call db_open(db, NDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')

        call ins_null(db, ti, 1_int32, 10_int32, .false., 'a', buf)
        call ins_null(db, ti, 2_int32,  0_int32, .true. , 'b', buf)
        call ins_null(db, ti, 3_int32, 20_int32, .false., 'c', buf)
        call ins_null(db, ti, 4_int32,  0_int32, .true. , 'd', buf)
        call db_create_index(db, 't', 'val', rs)
        call check(rs == SQR_OK, 'null: index over data with NULLs built')

        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 't', 2_int32, buf, rs)
        call check(rs == SQR_OK .and. row_is_null(buf, db%tables(ti)%cols(2)), &
                   'null: stored NULL reads back as NULL')
        call db_get(db, 't', 1_int32, buf, rs)
        call check(.not. row_is_null(buf, db%tables(ti)%cols(2)), &
                   'null: stored value is not NULL')

        call db_find_by_int(db, 't', 'val', 10_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'null: find by value hits the valued row')

        call db_find_range(db, 't', 'val', -100_int32, 100_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2 .and. all(ids(1:2) == [1_int32, 3_int32]), &
                   'null: range excludes NULL-member rows -> 1,3')
        call db_open_cursor(db, 't', 'val', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 4 .and. all(ids(1:4) == [2_int32, 4_int32, 1_int32, 3_int32]), &
                   'null: open_cursor yields NULL rows first -> 2,4,1,3')

        ! update: NULL row -> valued is found by its value; valued row -> NULL no longer is
        call upd_null(db, ti, 2_int32, 15_int32, .false., 'b', buf)
        call db_find_by_int(db, 't', 'val', 15_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'null: update NULL->value is found by value')
        call upd_null(db, ti, 1_int32, 0_int32, .true., 'a', buf)
        call db_find_by_int(db, 't', 'val', 10_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'null: update value->NULL matches no value')

        ! a unique index permits any number of NULL-member rows
        call upd_null(db, ti, 3_int32, 0_int32, .true., 'c', buf)   ! now 1,3 NULL; 2 has 15; 4 NULL
        call db_drop_index(db, 't', 'val', rs)
        call db_create_index(db, 't', 'val', rs, unique=.true.)
        call check(rs == SQR_OK, 'null: unique index builds over multiple NULLs')

        ! record_size and NULLs survive a close/reopen
        rsz = db%tables(ti)%record_size
        call db_close(db)
        call db_open(db, NDIR, rs, emsg)
        ti = db_table_index(db, 't')
        call check(db%tables(ti)%record_size == rsz, 'null: record_size persists')
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 't', 1_int32, buf, rs)
        call check(row_is_null(buf, db%tables(ti)%cols(2)), 'null: NULL survives reopen')

        call db_close(db)
        ios = c_rmtree(NDIR)
    end subroutine

    ! ===== NULL members in index keys (schema version 2; review E1, E3) =====

    ! Row buffer for a table (id INT, a INT, b INT); a and b may be NULL.
    subroutine row3(db, ti, id, a, anull, b, bnull, buf)
        type(db_t),       intent(in)    :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: id, a, b
        logical,          intent(in)    :: anull, bnull
        character(len=:), allocatable, intent(inout) :: buf
        associate (c => db%tables(ti)%cols)
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, c(1), id)
            if (anull) then
                call row_set_null(buf, c(2))
            else
                call row_set_int(buf, c(2), a)
            end if
            if (bnull) then
                call row_set_null(buf, c(3))
            else
                call row_set_int(buf, c(3), b)
            end if
        end associate
    end subroutine

    subroutine test_null_in_keys()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        character(len=*), parameter :: KDIR = 'utest_sqr_nullkey_db'
        character(len=8), parameter :: AB(2) = [character(len=8) :: 'a', 'b']
        character(len=8), parameter :: XY(2) = [character(len=8) :: 'x', 'y']
        logical, parameter :: NUL = .true., SET = .false.
        type(db_t), target :: db
        type(column_t) :: c(3), cr(3)
        integer :: rs, ios, ti, tr, n
        integer(int32) :: rid, ids(16), rids(2)
        character(len=:), allocatable :: buf, got, bufs(:)
        type(db_cursor_t) :: cur
        character(len=128) :: emsg
        logical :: ok

        ios = c_rmtree(KDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'a' ; c(2)%dtype = DT_INT; c(2)%csize = 4
        c(3)%name = 'b' ; c(3)%dtype = DT_INT; c(3)%csize = 4
        call db_open(db, KDIR, rs, emsg)
        call db_create_table(db, 'k', c, rs, emsg)
        ti = db_table_index(db, 'k')
        call row3(db, ti, 1_int32, 5_int32, SET, 0_int32, NUL, buf); call db_insert(db, 'k', buf, rid, rs)
        call row3(db, ti, 2_int32, 5_int32, SET, 1_int32, SET, buf); call db_insert(db, 'k', buf, rid, rs)
        call row3(db, ti, 3_int32, 6_int32, SET, 0_int32, NUL, buf); call db_insert(db, 'k', buf, rid, rs)
        call row3(db, ti, 4_int32, 0_int32, NUL, 7_int32, SET, buf); call db_insert(db, 'k', buf, rid, rs)

        ! The answers a single-column index on a gives ...
        call db_create_index(db, 'k', 'a', rs)
        call db_find_range(db, 'k', 'a', 5_int32, 5_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2 .and. all(ids(1:2) == [1_int32, 2_int32]), &
                   'nullkey: single-column index, a = 5 -> 1,2')
        call db_drop_index(db, 'k', 'a', rs)

        ! ... are the answers a composite index on (a, b) gives (review E1:
        ! in 1.x the rows whose b is NULL vanished once this index existed).
        call db_create_index(db, 'k', AB, rs)
        call check(rs == SQR_OK, 'nullkey: composite (a,b) built over NULL members')
        call db_find_range(db, 'k', 'a', 5_int32, 5_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2 .and. all(ids(1:2) == [1_int32, 2_int32]), &
                   'nullkey: composite index, a = 5 -> 1,2 (NULL trailing member found)')
        call db_find_range(db, 'k', 'a', 5_int32, 6_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [1_int32, 2_int32, 3_int32]), &
                   'nullkey: composite index, a in [5,6] -> 1,2,3')
        call db_find_by_int(db, 'k', 'a', 6_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'nullkey: find a = 6 reaches the (6,NULL) row')
        call db_find_by_int(db, 'k', 'a', 5_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'nullkey: find a = 5 -> first in key order, (5,NULL)')

        ! A NULL leading member is outside every value range, however wide ...
        call db_find_range(db, 'k', 'a', -huge(0_int32) - 1_int32, huge(0_int32), cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [1_int32, 2_int32, 3_int32]), &
                   'nullkey: full int range excludes the NULL leading member')
        ! ... and first in the whole-index order.
        call db_open_cursor(db, 'k', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 4 .and. all(ids(1:4) == [4_int32, 1_int32, 2_int32, 3_int32]), &
                   'nullkey: open_cursor yields every row, NULL first -> 4,1,2,3')
        call db_verify(db, 'k', rs, emsg)
        call check(rs == SQR_OK, 'nullkey: verify passes on NULL-bearing indexed data')

        ! Unique: a key with a NULL member equals no other key.
        call row3(db, ti, 5_int32, 5_int32, SET, 0_int32, NUL, buf); call db_insert(db, 'k', buf, rid, rs)
        call db_drop_index(db, 'k', AB, rs)
        call db_create_index(db, 'k', AB, rs, unique=.true.)
        call check(rs == SQR_OK, 'nullkey: unique index builds over two (5,NULL) rows')
        call row3(db, ti, 6_int32, 6_int32, SET, 0_int32, NUL, buf); call db_insert(db, 'k', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 6, 'nullkey: unique index accepts a second (6,NULL)')
        call row3(db, ti, 9_int32, 5_int32, SET, 1_int32, SET, buf); call db_insert(db, 'k', buf, rid, rs)
        call check(rs == SQR_DUP, 'nullkey: unique index refuses a second (5,1)')
        call row3(db, ti, 6_int32, 5_int32, SET, 1_int32, SET, buf); call db_update(db, 'k', 6_int32, buf, rs)
        call check(rs == SQR_DUP, 'nullkey: update NULL -> value refused when the key is taken')
        call row3(db, ti, 6_int32, 0_int32, NUL, 7_int32, SET, buf); call db_update(db, 'k', 6_int32, buf, rs)
        call check(rs == SQR_OK, 'nullkey: update to a second (NULL,7) accepted')
        call db_open_cursor(db, 'k', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 6 .and. all(ids(1:6) == [4_int32, 6_int32, 1_int32, 5_int32, 2_int32, 3_int32]), &
                   'nullkey: order is NULL first per member, ties by row id -> 4,6,1,5,2,3')
        call row3(db, ti, 6_int32, 5_int32, SET, 2_int32, SET, buf); call db_update(db, 'k', 6_int32, buf, rs)
        call db_open_cursor(db, 'k', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(rs == SQR_OK .and. n == 6 .and. &
                   all(ids(1:6) == [4_int32, 1_int32, 5_int32, 2_int32, 6_int32, 3_int32]), &
                   'nullkey: update NULL -> value moves the entry -> 4,1,5,2,6,3')
        call db_verify(db, 'k', rs, emsg)
        call check(rs == SQR_OK, 'nullkey: verify passes with repeated NULL keys in a unique index')

        ! By-key: a NULL key member names no row (review E3: it used to act
        ! on the row whose member was zero).
        call row3(db, ti, 7_int32, 0_int32, SET, 0_int32, SET, buf); call db_insert(db, 'k', buf, rid, rs)
        call row_alloc(got, db%tables(ti)%record_size)
        call row3(db, ti, 0_int32, 0_int32, NUL, 0_int32, SET, buf)
        call db_get_by_key(db, 'k', AB, buf, got, rs)
        call check(rs == SQR_NOT_FOUND, 'nullkey: get_by_key (NULL,0) -> NOT_FOUND')
        call db_delete_by_key(db, 'k', AB, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'nullkey: delete_by_key (NULL,0) -> NOT_FOUND')
        call row3(db, ti, 0_int32, 0_int32, SET, 0_int32, NUL, buf)
        call db_get_by_key(db, 'k', AB, buf, got, rs)
        call check(rs == SQR_NOT_FOUND, 'nullkey: get_by_key (0,NULL) -> NOT_FOUND')
        call row3(db, ti, 0_int32, 5_int32, SET, 0_int32, NUL, buf)
        call db_get_by_key(db, 'k', AB, buf, got, rs)
        call check(rs == SQR_NOT_FOUND, 'nullkey: get_by_key (5,NULL) -> NOT_FOUND though two rows hold it')
        call row3(db, ti, 0_int32, 0_int32, SET, 0_int32, SET, buf)
        call db_get_by_key(db, 'k', AB, buf, got, rs, row_id=rid)
        call check(rs == SQR_OK .and. rid == 7, 'nullkey: the (0,0) row is untouched and found by its key')

        ! Rollback and undo/redo carry the entry with the row.
        call db_begin(db, rs)
        call row3(db, ti, 3_int32, 8_int32, SET, 0_int32, NUL, buf); call db_update(db, 'k', 3_int32, buf, rs)
        call db_find_by_int(db, 'k', 'a', 8_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'nullkey: in txn, (6,NULL) -> (8,NULL) found at a = 8')
        call db_rollback(db, rs)
        call db_find_by_int(db, 'k', 'a', 8_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'nullkey: rollback removes the a = 8 entry')
        call db_find_by_int(db, 'k', 'a', 6_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'nullkey: rollback restores the (6,NULL) entry')
        call db_begin(db, rs, label='move')
        call row3(db, ti, 3_int32, 9_int32, SET, 4_int32, SET, buf); call db_update(db, 'k', 3_int32, buf, rs)
        call db_commit(db, rs)
        call db_undo(db, rs)
        call db_find_by_int(db, 'k', 'a', 9_int32, rid, ios)
        call check(rs == SQR_OK .and. ios == SQR_NOT_FOUND, 'nullkey: undo removes the (9,4) entry')
        call db_find_by_int(db, 'k', 'a', 6_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'nullkey: undo restores the (6,NULL) entry')
        call db_redo(db, rs)
        call db_find_by_int(db, 'k', 'a', 9_int32, rid, ios)
        call check(rs == SQR_OK .and. ios == SQR_OK .and. rid == 3, 'nullkey: redo brings (9,4) back')
        call db_verify(db, 'k', rs, emsg)
        call check(rs == SQR_OK, 'nullkey: verify passes after rollback, undo and redo')

        ! Delete, reopen, compact.
        call db_delete(db, 'k', 1_int32, rs)
        call db_find_range(db, 'k', 'a', 5_int32, 5_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 3 .and. all(ids(1:3) == [5_int32, 2_int32, 6_int32]), &
                   'nullkey: deleted NULL-bearing row is skipped -> 5,2,6')
        call db_close(db)
        call db_open(db, KDIR, rs, emsg)
        ti = db_table_index(db, 'k')
        call db_open_cursor(db, 'k', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 6 .and. all(ids(1:6) == [4_int32, 7_int32, 5_int32, 2_int32, 6_int32, 3_int32]), &
                   'nullkey: order survives reopen -> 4,7,5,2,6,3')
        call db_compact(db, 'k', rs)
        call db_open_cursor(db, 'k', 'a', cur, ios)
        call drain_cursor(db, cur, ids, n)
        call check(rs == SQR_OK .and. n == 6 .and. &
                   all(ids(1:6) == [4_int32, 7_int32, 5_int32, 2_int32, 6_int32, 3_int32]), &
                   'nullkey: order survives compact')
        call db_verify(db, 'k', rs, emsg)
        call check(rs == SQR_OK, 'nullkey: verify passes after delete, reopen and compact')

        ! Batch insert: validated by the same rule, indexed by the rebuild.
        allocate(character(len=db%tables(ti)%record_size) :: bufs(2))
        call row3(db, ti, 10_int32, 7_int32, SET, 0_int32, NUL, buf); bufs(1) = buf
        call row3(db, ti, 11_int32, 7_int32, SET, 0_int32, NUL, buf); bufs(2) = buf
        call db_insert_many(db, 'k', bufs, rids, rs)
        call check(rs == SQR_OK, 'nullkey: insert_many accepts two (7,NULL) under a unique index')
        call row3(db, ti, 12_int32, 7_int32, SET, 3_int32, SET, buf); bufs(1) = buf
        call row3(db, ti, 13_int32, 7_int32, SET, 3_int32, SET, buf); bufs(2) = buf
        call db_insert_many(db, 'k', bufs, rids, rs)
        call check(rs == SQR_DUP, 'nullkey: insert_many refuses two (7,3)')
        call db_find_range(db, 'k', 'a', 7_int32, 7_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2 .and. all(ids(1:2) == [10_int32, 11_int32]), &
                   'nullkey: the batch rows are indexed -> 10,11')

        ! A NULL member does not excuse a NaN in another: the row is indexed.
        cr(1)%name = 'id'; cr(1)%dtype = DT_INT ; cr(1)%csize = 4
        cr(2)%name = 'x' ; cr(2)%dtype = DT_INT ; cr(2)%csize = 4
        cr(3)%name = 'y' ; cr(3)%dtype = DT_REAL; cr(3)%csize = 8
        call db_create_table(db, 'r', cr, rs, emsg)
        tr = db_table_index(db, 'r')
        ti = db_table_index(db, 'k')
        call db_create_index(db, 'r', XY, rs)
        call row_alloc(buf, db%tables(tr)%record_size)
        call row_set_int(buf, db%tables(tr)%cols(1), 1_int32)
        call row_set_null(buf, db%tables(tr)%cols(2))
        call row_set_real(buf, db%tables(tr)%cols(3), ieee_value(0.0_real64, ieee_quiet_nan))
        call db_insert(db, 'r', buf, rid, rs)
        call check(rs == SQR_INVALID, 'nullkey: (NULL, NaN) refused - NaN has no place in the order')
        call row_set_real(buf, db%tables(tr)%cols(3), 1.5_real64)
        call db_insert(db, 'r', buf, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'nullkey: (NULL, 1.5) accepted')
        call db_verify(db, 'r', rs, emsg)
        call check(rs == SQR_OK, 'nullkey: verify passes on (x,y) with a NULL leading member')

        ! db_verify counts one entry per live row, so an index that is one
        ! short is caught: put back the index file as it was a row ago.
        call db_close(db)
        call copy_file(KDIR // '/r__i1.idx', KDIR // '/r__i1.saved', ok)
        call db_open(db, KDIR, rs, emsg)
        tr = db_table_index(db, 'r')
        call row_alloc(buf, db%tables(tr)%record_size)
        call row_set_int(buf, db%tables(tr)%cols(1), 2_int32)
        call row_set_null(buf, db%tables(tr)%cols(2))
        call row_set_null(buf, db%tables(tr)%cols(3))
        call db_insert(db, 'r', buf, rid, rs)
        call db_close(db)
        call copy_file(KDIR // '/r__i1.saved', KDIR // '/r__i1.idx', ok)
        call db_open(db, KDIR, rs, emsg)
        call db_verify(db, 'r', ios, emsg)
        call check(ok .and. rs == SQR_OK .and. ios == SQR_INVALID, &
                   'nullkey: verify catches an index missing a NULL-keyed row')
        call db_close(db)
        ios = c_rmtree(KDIR)
    end subroutine

    ! The schema version stored in a database's catalog (0 if unreadable).
    function catalog_version(dir) result(ver)
        character(len=*), intent(in) :: dir
        integer :: ver, u, ios
        character(len=4) :: magic
        integer(int32) :: bom, v
        ver = 0
        open(newunit=u, file=dir // '/_catalog.dat', access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(u, iostat=ios) magic, bom, v
        if (ios == 0) ver = int(v)
        close(u)
    end function

    ! Upgrade of a sqr 1.x database (schema version 1: index keys without
    ! NULL flags, NULL-member rows left out).  The fixture was written by
    ! sqr 1.8.2 (see test/fixtures/README).
    subroutine test_format_upgrade()
        character(len=*), parameter :: FIX   = 'test/fixtures/v1_null.sqr'
        character(len=*), parameter :: UDIR  = 'utest_sqr_upgrade_db'
        character(len=*), parameter :: UDIR2 = 'utest_sqr_upgrade2_db'
        type(db_t), target :: db
        integer :: rs, ios, ti, n, u
        integer(int32) :: rid, ids(16)
        integer(int64) :: isz
        character(len=:), allocatable :: buf
        type(db_cursor_t) :: cur
        character(len=256) :: emsg
        logical :: ok

        ios = c_rmtree(UDIR); ios = c_rmtree(UDIR2)
        if (.not. c_path_exists(FIX)) then
            call skip('upgrade: sqr 1.x fixture', 'run from the project root to find ' // FIX)
            return
        end if
        call db_unpack(FIX, UDIR, rs, emsg)
        call check(rs == SQR_OK .and. catalog_version(UDIR) == 1, 'upgrade: fixture unpacks as schema version 1')
        isz = c_file_size(UDIR // '/t__i1.idx')

        ! A read-only handle cannot rebuild, and is refused untouched.
        emsg = ''
        call db_open(db, UDIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_VERSION .and. .not. db%opened, 'upgrade: read-only open of a 1.x database refused')
        call check(index(emsg, '1.x format') > 0 .and. index(emsg, 'read-write') > 0, &
                   'upgrade: the refusal says what to do')
        call check(catalog_version(UDIR) == 1 .and. c_file_size(UDIR // '/t__i1.idx') == isz, &
                   'upgrade: the refused open changed nothing')

        ! A read-write open upgrades in place.
        call db_open(db, UDIR, rs, emsg)
        call check(rs == SQR_OK .and. db%opened, 'upgrade: read-write open of a 1.x database succeeds')
        call check(catalog_version(UDIR) == SQR_SCHEMA_VERSION .and. &
                   all(db%tables(1:db%ntables)%schema_version == SQR_SCHEMA_VERSION), &
                   'upgrade: catalog and every table are at the current version')
        call db_verify(db, 't', rs, emsg)
        call db_verify(db, 'u', ios, emsg)
        call db_verify(db, 'e', n, emsg)
        call check(rs == SQR_OK .and. ios == SQR_OK .and. n == SQR_OK, 'upgrade: every table verifies')
        ! t(a,b,c,r): (5,NULL) (5,1) (6,NULL) (NULL,7) (7,2), index (a,b).
        call db_find_range(db, 't', 'a', 5_int32, 5_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 2, 'upgrade: a = 5 finds both rows (1.x found one)')
        call db_find_range(db, 't', 'a', 5_int32, huge(0_int32), cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 4 .and. all(ids(1:4) == [5_int32, 5_int32, 6_int32, 7_int32]), &
                   'upgrade: a >= 5 finds four rows (1.x found two)')
        call db_open_cursor(db, 't', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 5, 'upgrade: the cursor yields all five live rows')
        call db_find_by_char(db, 't', 'c', 'y', rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'upgrade: single-column CHAR index still answers')
        ! u(k,v): unique index on k over 0, 1, NULL, NULL.
        ti = db_table_index(db, 'u')
        call db_find_by_int(db, 'u', 'k', 1_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'upgrade: unique index answers')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), 0_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), 'dup')
        call db_insert(db, 'u', buf, rid, rs)
        call check(rs == SQR_DUP, 'upgrade: unique index still enforced')
        call row_set_null(buf, db%tables(ti)%cols(1))
        call db_insert(db, 'u', buf, rid, rs)
        call check(rs == SQR_OK, 'upgrade: a third NULL key is accepted')
        call db_close(db)

        ! Upgraded, so a read-only handle is now served.
        call db_open(db, UDIR, rs, emsg, readonly=.true.)
        call db_find_by_int(db, 'u', 'k', 1_int32, rid, ios)
        call check(rs == SQR_OK .and. ios == SQR_OK .and. rid == 2, 'upgrade: read-only open works afterwards')
        call db_close(db)

        ! Interrupted after the tables, before the catalog: only the catalog
        ! still says 1.
        open(newunit=u, file=UDIR // '/_catalog.dat', access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        write(u, pos=9, iostat=ios) 1_int32
        close(u)
        call db_open(db, UDIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_VERSION, 'upgrade: half-upgraded database refused read-only')
        call db_open(db, UDIR, rs, emsg)
        call check(rs == SQR_OK .and. catalog_version(UDIR) == SQR_SCHEMA_VERSION, &
                   'upgrade: a read-write open completes the catalog')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'upgrade: verify passes after completing the catalog')
        call db_close(db)

        ! Interrupted after a table's indices were rebuilt, before its schema
        ! was written: a version-1 schema over a current-layout index file.
        call db_unpack(FIX, UDIR2, rs, emsg)
        call copy_file(UDIR2 // '/u.schema', UDIR2 // '/u.schema.v1', ok)
        call db_open(db, UDIR2, rs, emsg)
        call db_close(db)
        call copy_file(UDIR2 // '/u.schema.v1', UDIR2 // '/u.schema', ok)
        call db_open(db, UDIR2, rs, emsg, readonly=.true.)
        call check(rs == SQR_VERSION .and. .not. db%opened, &
                   'upgrade: a version-1 table under a current catalog is refused read-only')
        call db_open(db, UDIR2, rs, emsg)
        ti = db_table_index(db, 'u')
        call check(ok .and. rs == SQR_OK, 'upgrade: version-1 schema over a rebuilt index opens')
        call check(db%tables(ti)%schema_version == SQR_SCHEMA_VERSION, 'upgrade: that table is upgraded again')
        call db_verify(db, 'u', rs, emsg)
        call db_find_by_int(db, 'u', 'k', 1_int32, rid, ios)
        call check(rs == SQR_OK .and. ios == SQR_OK .and. rid == 2, 'upgrade: and verifies and answers')
        call db_close(db)

        ios = c_rmtree(UDIR); ios = c_rmtree(UDIR2)
    end subroutine

    ! ===== 5.2 leading-column range/cursor on a composite index =====

    subroutine test_leading_column()
        character(len=*), parameter :: LDIR = 'utest_sqr_lead_db'
        type(db_t), target :: db
        type(column_t) :: c(3), c2(2)
        integer :: rs, ios, ti, n
        integer(int32) :: ids(16)
        character(len=:), allocatable :: buf
        type(db_cursor_t) :: cur
        character(len=128) :: emsg

        ios = c_rmtree(LDIR)
        ! Composite (a int, b real, c char): range/scan on the LEADING int member
        ! exercises typed min/max fill of the trailing real and char members.
        c(1)%name = 'a'; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'b'; c(2)%dtype = DT_REAL ; c(2)%csize = 8
        c(3)%name = 'c'; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
        call db_open(db, LDIR, rs, emsg)
        call db_create_table(db, 'm', c, rs, emsg)
        ti = db_table_index(db, 'm')
        call ins_m(db, ti, 10_int32, 1.0_real64, 'p', buf)
        call ins_m(db, ti, 10_int32, 2.0_real64, 'q', buf)
        call ins_m(db, ti, 20_int32, 1.0_real64, 'r', buf)
        call ins_m(db, ti, 30_int32, 5.0_real64, 's', buf)
        call ins_m(db, ti, 20_int32, 3.0_real64, 't', buf)
        call db_create_index(db, 'm', [character(len=8) :: 'a', 'b', 'c'], rs)
        call check(rs == SQR_OK, 'lead: composite (a,b,c) index built')

        ! Range on leading 'a' [10,20] -> every row with a in [10,20], any b/c.
        call db_find_range(db, 'm', 'a', 10_int32, 20_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(rs == SQR_OK .and. n == 4 .and. &
                   all(ids(1:4) == [10_int32, 10_int32, 20_int32, 20_int32]), &
                   'lead: range on leading int member -> 10,10,20,20')

        ! Whole-index cursor ordered by the leading member.
        call db_open_cursor(db, 'm', 'a', cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 5 .and. all(ids(1:5) == &
                   [10_int32, 10_int32, 20_int32, 20_int32, 30_int32]), &
                   'lead: open_cursor on leading member -> a order')

        ! Single value on the leading member.
        call db_find_range(db, 'm', 'a', 30_int32, 30_int32, cur, rs)
        call drain_cursor(db, cur, ids, n)
        call check(n == 1 .and. ids(1) == 30_int32, 'lead: range a [30,30] -> 30')

        ! Wrong-typed overload for this index's leading member -> NOT_FOUND.
        call db_find_range(db, 'm', 'a', 1.0_real64, 9.0_real64, cur, rs)
        call check(rs == SQR_NOT_FOUND, 'lead: real range on int leading member -> NOT_FOUND')

        call db_close(db)
        ios = c_rmtree(LDIR)

        ! Composite (region char, sku int): leading char member exercises typed
        ! min/max fill of the trailing int member.
        ios = c_rmtree(LDIR)
        c2(1)%name = 'region'; c2(1)%dtype = DT_CHAR; c2(1)%csize = 4
        c2(2)%name = 'sku'   ; c2(2)%dtype = DT_INT ; c2(2)%csize = 4
        call db_open(db, LDIR, rs, emsg)
        call db_create_table(db, 'r2', c2, rs, emsg)
        ti = db_table_index(db, 'r2')
        call ins_ci(db, 'r2', ti, 'eu', 1_int32, buf)
        call ins_ci(db, 'r2', ti, 'us', 2_int32, buf)
        call ins_ci(db, 'r2', ti, 'eu', 3_int32, buf)
        call ins_ci(db, 'r2', ti, 'as', 4_int32, buf)
        call db_create_index(db, 'r2', [character(len=8) :: 'region', 'sku'], rs)
        call check(rs == SQR_OK, 'lead: composite (region,sku) index built')

        call db_find_range(db, 'r2', 'region', 'eu', 'us', cur, rs)
        call drain_count(db, cur, n)
        call check(rs == SQR_OK .and. n == 3, 'lead: char range [eu,us] -> 3 (as excluded)')
        call db_find_range(db, 'r2', 'region', 'eu', 'eu', cur, rs)
        call drain_count(db, cur, n)
        call check(n == 2, 'lead: char range [eu,eu] -> 2')
        call db_open_cursor(db, 'r2', 'region', cur, rs)
        call drain_count(db, cur, n)
        call check(n == 4, 'lead: open_cursor on leading char -> all 4')

        call db_close(db)
        ios = c_rmtree(LDIR)
    end subroutine

    ! Review §3: db_find_by_* can use a composite index's LEADING member
    ! (parity with db_find_range/db_open_cursor).  Several rows may share
    ! the leading value: the one returned is the first in key order.
    subroutine test_find_by_leading()
        character(len=*), parameter :: LDIR = 'utest_sqr_findlead_db'
        type(db_t), target :: db
        type(column_t) :: c(3), c2(2)
        integer :: rs, ios, ti
        integer(int32) :: rid, r_lo, r_hi, r_eu1, r_eu3
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        ios = c_rmtree(LDIR)
        c(1)%name = 'a'; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'b'; c(2)%dtype = DT_REAL ; c(2)%csize = 8
        c(3)%name = 'c'; c(3)%dtype = DT_CHAR ; c(3)%csize = 8
        call db_open(db, LDIR, rs, emsg)
        call db_create_table(db, 'm', c, rs, emsg)
        ti = db_table_index(db, 'm')
        ! Insert the higher trailing member FIRST so first-in-key-order is
        ! distinguishable from first-inserted.
        call row_alloc(buf, db_record_size(db, 'm'))
        call row_set_int (buf, db%tables(ti)%cols(1), 20_int32)
        call row_set_real(buf, db%tables(ti)%cols(2), 3.0_real64)
        call row_set_char(buf, db%tables(ti)%cols(3), 't')
        call db_insert(db, 'm', buf, r_hi, rs)
        call row_set_real(buf, db%tables(ti)%cols(2), 1.0_real64)
        call row_set_char(buf, db%tables(ti)%cols(3), 'r')
        call db_insert(db, 'm', buf, r_lo, rs)
        call row_set_int (buf, db%tables(ti)%cols(1), 10_int32)
        call db_insert(db, 'm', buf, rid, rs)
        call db_create_index(db, 'm', [character(len=8) :: 'a', 'b', 'c'], rs)

        call db_find_by_int(db, 'm', 'a', 20_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == r_lo, &
                   'find lead: composite leading int found, first in key order')
        call db_find_by_int(db, 'm', 'a', 99_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND .and. rid == 0, 'find lead: absent key -> NOT_FOUND')
        call db_find_by_real(db, 'm', 'a', 20.0_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find lead: wrong-typed overload -> NOT_FOUND')
        call db_find_by_real(db, 'm', 'b', 1.0_real64, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find lead: trailing member is not served')
        call db_close(db)
        ios = c_rmtree(LDIR)

        ! Leading char member, and liveness within the run: after deleting
        ! the first-in-key-order row the next live one is returned.
        c2(1)%name = 'region'; c2(1)%dtype = DT_CHAR; c2(1)%csize = 4
        c2(2)%name = 'sku'   ; c2(2)%dtype = DT_INT ; c2(2)%csize = 4
        call db_open(db, LDIR, rs, emsg)
        call db_create_table(db, 'r2', c2, rs, emsg)
        ti = db_table_index(db, 'r2')
        call row_alloc(buf, db_record_size(db, 'r2'))
        call row_set_char(buf, db%tables(ti)%cols(1), 'eu')
        call row_set_int (buf, db%tables(ti)%cols(2), 3_int32)
        call db_insert(db, 'r2', buf, r_eu3, rs)
        call row_set_int (buf, db%tables(ti)%cols(2), 1_int32)
        call db_insert(db, 'r2', buf, r_eu1, rs)
        call row_set_char(buf, db%tables(ti)%cols(1), 'us')
        call db_insert(db, 'r2', buf, rid, rs)
        call db_create_index(db, 'r2', [character(len=8) :: 'region', 'sku'], rs)

        call db_find_by_char(db, 'r2', 'region', 'eu', rid, rs)
        call check(rs == SQR_OK .and. rid == r_eu1, 'find lead: leading char, lowest sku first')
        call db_delete(db, 'r2', r_eu1, rs)
        call db_find_by_char(db, 'r2', 'region', 'eu', rid, rs)
        call check(rs == SQR_OK .and. rid == r_eu3, 'find lead: dead first hit skipped')
        call db_find_by_char(db, 'r2', 'region', 'europe', rid, rs)
        call check(rs == SQR_NOT_FOUND, 'find lead: key wider than leading column -> NOT_FOUND')
        call db_close(db)
        ios = c_rmtree(LDIR)
    end subroutine

    subroutine ins_ci(db, tname, ti, region, sku, buf)
        type(db_t),       intent(inout) :: db
        character(len=*), intent(in)    :: tname
        integer,          intent(in)    :: ti
        character(len=*), intent(in)    :: region
        integer(int32),   intent(in)    :: sku
        character(len=:), allocatable, intent(inout) :: buf
        integer :: rs
        integer(int32) :: rid
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_char(buf, db%tables(ti)%cols(1), region)
        call row_set_int (buf, db%tables(ti)%cols(2), sku)
        call db_insert(db, tname, buf, rid, rs)
    end subroutine

    ! ===== 5.3 db_drop_index =====

    subroutine test_drop_index()
        character(len=*), parameter :: DDIR = 'utest_sqr_dropidx_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        ios = c_rmtree(DDIR)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'age' ; c(2)%dtype = DT_INT ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR; c(3)%csize = 8
        call db_open(db, DDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        ti = db_table_index(db, 'p')
        call ins_ic3(db, 'p', ti, 1_int32, 30_int32, 'x', buf)
        call ins_ic3(db, 'p', ti, 2_int32, 40_int32, 'y', buf)
        call db_create_index(db, 'p', 'age', rs)
        call db_create_index(db, 'p', 'name', rs)
        call check(rs == SQR_OK, 'dropidx: two indices built')

        call db_find_by_int(db, 'p', 'age', 30_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'dropidx: find by age works')
        call db_drop_index(db, 'p', 'age', rs)
        call check(rs == SQR_OK, 'dropidx: drop age index')
        call db_find_by_int(db, 'p', 'age', 30_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'dropidx: dropped index no longer resolves')
        call db_find_by_char(db, 'p', 'name', 'x', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'dropidx: surviving index still works')
        call db_drop_index(db, 'p', 'age', rs)
        call check(rs == SQR_NOT_FOUND, 'dropidx: re-drop -> NOT_FOUND')

        ! Reopen: the tombstoned slot is skipped, db opens cleanly.
        call db_close(db)
        call db_open(db, DDIR, rs, emsg)
        call check(rs == SQR_OK, 'dropidx: db reopens after drop')
        call db_find_by_char(db, 'p', 'name', 'y', rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'dropidx: surviving index works after reopen')
        call db_find_by_int(db, 'p', 'age', 30_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'dropidx: dropped index still gone after reopen')

        ! A new index over the same column appends a fresh slot.
        call db_create_index(db, 'p', 'age', rs)
        call check(rs == SQR_OK, 'dropidx: recreate index on dropped column')
        call db_find_by_int(db, 'p', 'age', 40_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'dropidx: recreated index works')
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_OK, 'dropidx: verify OK after drop+recreate')

        call db_close(db)
        ios = c_rmtree(DDIR)
    end subroutine

    subroutine ins_ic3(db, tname, ti, id, n2, nm, buf)
        type(db_t),       intent(inout) :: db
        character(len=*), intent(in)    :: tname
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: id, n2
        character(len=*), intent(in)    :: nm
        character(len=:), allocatable, intent(inout) :: buf
        integer :: rs
        integer(int32) :: rid
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int (buf, db%tables(ti)%cols(1), id)
        call row_set_int (buf, db%tables(ti)%cols(2), n2)
        call row_set_char(buf, db%tables(ti)%cols(3), nm)
        call db_insert(db, tname, buf, rid, rs)
    end subroutine

    ! Look a column up by name and return its descriptor, for building/reading
    ! row buffers in the schema-evolution test (positions shift as columns are
    ! added and dropped, so referencing cols(k) by a fixed index will not do).
    function col_of(db, tname, cname) result(c)
        type(db_t),       intent(in) :: db
        character(len=*), intent(in) :: tname, cname
        type(column_t) :: c
        integer :: ti, k
        ti = db_table_index(db, tname)
        find: do k = 1, db%tables(ti)%ncols
            if (trim(db%tables(ti)%cols(k)%name) == cname) then
                c = db%tables(ti)%cols(k)
                return
            end if
        end do find
    end function

    ! ===== #7 schema evolution: add / drop column =====

    ! db_reshape_table: one rewrite to a declared column list - drop, reorder
    ! and add together, matched by name, row ids and survivors' values kept.
    subroutine test_reshape_table()
        character(len=*), parameter :: RDIR = 'utest_sqr_reshape_db'
        type(db_t), target :: db
        type(column_t) :: c(4), t2(4), bad(2)
        integer :: rs, ios, ti, gen0
        integer(int32) :: rid
        character(len=:), allocatable :: buf, got
        character(len=128) :: emsg

        ios = c_rmtree(RDIR)
        c(1)%name = 'pid'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
        c(4)%name = 'bio'  ; c(4)%dtype = DT_TEXT ; c(4)%csize = SQR_TEXT_DESC
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'people', c, rs, emsg)
        ti = db_table_index(db, 'people')
        call db_create_index(db, 'people', 'age', rs)    ! cascades with age
        call db_create_index(db, 'people', 'name', rs)   ! survives, moves to slot 1
        call ins_ic3(db, 'people', ti, 1_int32, 30_int32, 'Alice', buf)
        call ins_ic3(db, 'people', ti, 2_int32, 40_int32, 'Bob',   buf)
        call ins_ic3(db, 'people', ti, 3_int32, 50_int32, 'Carol', buf)
        call db_set_text(db, 'people', 3_int32, 'bio', 'carol bio', rs)
        call db_delete(db, 'people', 2_int32, rs)
        call check(rs == SQR_OK, 'reshape: setup (2 indices, text, 3 rows, 1 deleted)')

        ! --- equal layout: a no-op (no rewrite, cursors not invalidated) ---
        gen0 = db%generation
        call db_reshape_table(db, 'people', c, rs, emsg)
        call check(rs == SQR_OK .and. db%generation == gen0, 'reshape: equal layout is a no-op')

        ! --- drop age (indexed), name first, keep pid and bio, add score ---
        t2(1)%name = 'name' ; t2(1)%dtype = DT_CHAR ; t2(1)%csize = 32
        t2(2)%name = 'pid'  ; t2(2)%dtype = DT_INT  ; t2(2)%csize = 4
        t2(3)%name = 'bio'  ; t2(3)%dtype = DT_TEXT ; t2(3)%csize = SQR_TEXT_DESC
        t2(4)%name = 'score'; t2(4)%dtype = DT_INT  ; t2(4)%csize = 4
        call db_reshape_table(db, 'people', t2, rs, emsg)
        call check(rs == SQR_OK, 'reshape: drop + reorder + add in one call')
        ti = db_table_index(db, 'people')
        call check(db%tables(ti)%ncols == 4 .and. trim(db%tables(ti)%cols(1)%name) == 'name' .and. &
                   trim(db%tables(ti)%cols(4)%name) == 'score', 'reshape: stored layout is the target order')
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. trim(row_get_char(buf, db%tables(ti)%cols(1))) == 'Alice' .and. &
                   row_get_int(buf, db%tables(ti)%cols(2)) == 1, 'reshape: row 1 values follow their names')
        call check(row_is_null(buf, db%tables(ti)%cols(4)), 'reshape: the added column reads NULL')
        call db_get(db, 'people', 2_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'reshape: the tombstoned row id stays tombstoned')
        call db_get(db, 'people', 3_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(2)) == 3, &
                   'reshape: row 3 keeps its row id')
        call db_get_text(db, 'people', 3_int32, 'bio', got, rs)
        call check(rs == SQR_OK .and. got == 'carol bio', 'reshape: text survives a moved column')
        call db_find_by_int(db, 'people', 'age', 30_int32, rid, rs)
        call check(rs /= SQR_OK, 'reshape: CASCADE removed the age index')
        call db_find_by_char(db, 'people', 'name', 'Carol', rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'reshape: the name index survives its column moving')
        call db_verify(db, 'people', rs, emsg)
        call check(rs == SQR_OK, 'reshape: db_verify passes')

        ! --- persistence ---
        call db_close(db)
        call db_open(db, RDIR, rs, emsg)
        ti = db_table_index(db, 'people')
        call check(rs == SQR_OK .and. trim(db%tables(ti)%cols(1)%name) == 'name', 'reshape: layout persists')
        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'reshape: index persists')

        ! --- refusals ---
        bad(1) = t2(1); bad(2) = t2(2); bad(2)%csize = 8   ! pid 4 -> 8 bytes
        call db_reshape_table(db, 'people', bad, rs, emsg)
        call check(rs == SQR_INVALID, 'reshape: a size change is refused')
        bad(2) = t2(2); bad(2)%dtype = DT_REAL; bad(2)%csize = 8
        call db_reshape_table(db, 'people', bad, rs, emsg)
        call check(rs == SQR_INVALID, 'reshape: a type change is refused')
        ti = db_table_index(db, 'people')
        call check(db%tables(ti)%ncols == 4, 'reshape: a refused reshape changes nothing')
        bad(1) = t2(1); bad(2) = t2(1)
        call db_reshape_table(db, 'people', bad, rs, emsg)
        call check(rs == SQR_INVALID, 'reshape: a duplicate name is refused')
        call db_reshape_table(db, 'people', t2(1:0), rs, emsg)
        call check(rs == SQR_INVALID, 'reshape: an empty column list is refused')
        call db_reshape_table(db, 'nosuch', t2, rs, emsg)
        call check(rs == SQR_NOT_FOUND, 'reshape: a missing table')
        call db_begin(db, rs)
        call db_reshape_table(db, 'people', c(1:3), rs, emsg)
        call check(rs /= SQR_OK, 'reshape: refused inside a transaction')
        call db_rollback(db, rs)
        call db_close(db)
        call db_open(db, RDIR, rs, emsg, readonly=.true.)
        call db_reshape_table(db, 'people', c(1:3), rs, emsg)
        call check(rs == SQR_READONLY, 'reshape: refused read-only')
        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine test_reshape_table

    ! An interrupted layout change is rolled forward on open.  The interrupted
    ! states are built by hand: a twin database is reshaped for real, and its
    ! data and schema are placed beside the original as the staged `.alter`
    ! files, with the `.altering` marker - (a) nothing renamed yet, (b) the data
    ! already renamed.  A read-only open refuses; a read-write open finishes the
    ! swap and the table reads in the new layout.
    subroutine test_alter_recovery()
        character(len=*), parameter :: ODIR = 'utest_sqr_altrec_db', TDIR = 'utest_sqr_altrec_twin'
        type(db_t), target :: db
        type(column_t) :: c(3), t2(2)
        integer :: rs, ios, ti, k
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        character(len=32) :: stage
        logical :: okc
        c(1)%name = 'pid'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
        t2(1) = c(3);  t2(2) = c(1)
        do k = 1, 2
            stage = merge('before any rename    ', 'after the data rename', k == 1)
            ios = c_rmtree(ODIR);  ios = c_rmtree(TDIR)
            call altrec_people(ODIR)
            call altrec_people(TDIR)
            call db_open(db, TDIR, rs, emsg)
            call db_reshape_table(db, 'people', t2, rs, emsg)
            call db_close(db)
            if (k == 1) then
                call copy_file(TDIR // '/people.dat', ODIR // '/people.dat.alter', okc)
            else
                call copy_file(TDIR // '/people.dat', ODIR // '/people.dat', okc)
            end if
            call copy_file(TDIR // '/people.schema', ODIR // '/people.schema.alter', okc)
            call copy_file(TDIR // '/people.schema', ODIR // '/people.altering', okc)   ! any bytes: presence counts

            call db_open(db, ODIR, rs, emsg, readonly=.true.)
            call check(rs == SQR_READONLY, 'altrec (' // trim(stage) // '): read-only open refuses')
            call db_open(db, ODIR, rs, emsg)
            call check(rs == SQR_OK, 'altrec (' // trim(stage) // '): read-write open recovers')
            ti = db_table_index(db, 'people')
            call check(db%tables(ti)%ncols == 2 .and. trim(db%tables(ti)%cols(1)%name) == 'name', &
                       'altrec (' // trim(stage) // '): the staged layout is in force')
            call row_alloc(buf, db%tables(ti)%record_size)
            call db_get(db, 'people', 2_int32, buf, rs)
            call check(rs == SQR_OK .and. trim(row_get_char(buf, db%tables(ti)%cols(1))) == 'Bob' .and. &
                       row_get_int(buf, db%tables(ti)%cols(2)) == 2, &
                       'altrec (' // trim(stage) // '): rows read in the new layout')
            call db_verify(db, 'people', rs, emsg)
            call check(rs == SQR_OK, 'altrec (' // trim(stage) // '): db_verify passes')
            call db_close(db)
            call check(.not. c_path_exists(ODIR // '/people.altering') .and. &
                       .not. c_path_exists(ODIR // '/people.dat.alter') .and. &
                       .not. c_path_exists(ODIR // '/people.schema.alter'), &
                       'altrec (' // trim(stage) // '): marker and staged files gone')
        end do
        ios = c_rmtree(ODIR);  ios = c_rmtree(TDIR)
    end subroutine test_alter_recovery

    ! The first TEXT column's blob file is part of the staged set.  (1) A
    ! change interrupted after its commit marker - (a) nothing renamed, (b) the
    ! blob file already renamed, (c) nothing renamed and an orphan blob file
    ! in the way - is finished by the next open with the blob file in place.
    ! (2) The change itself, made over an orphan blob file, starts the TEXT
    ! column from an empty blob file and survives a reopen.
    subroutine test_alter_first_text()
        character(len=*), parameter :: ODIR = 'utest_sqr_alttext_db', TDIR = 'utest_sqr_alttext_twin'
        character(len=*), parameter :: STAGES(3) = [character(len=24) :: 'nothing renamed', &
            'blob file renamed', 'orphan blob file present']
        type(db_t), target :: db
        type(column_t) :: bio
        integer :: rs, ios, ti, k
        character(len=:), allocatable :: txt, tag
        character(len=128) :: emsg
        logical :: okc
        bio%name = 'bio' ; bio%dtype = DT_TEXT ; bio%csize = SQR_TEXT_DESC
        recover: do k = 1, 3
            tag = 'alttext (' // trim(STAGES(k)) // ')'
            ios = c_rmtree(ODIR);  ios = c_rmtree(TDIR)
            call altrec_people(ODIR)
            call altrec_people(TDIR)
            call db_open(db, TDIR, rs, emsg)
            call db_add_column(db, 'people', bio, rs, emsg)
            call db_close(db)
            call copy_file(TDIR // '/people.dat',    ODIR // '/people.dat.alter', okc)
            call copy_file(TDIR // '/people.schema', ODIR // '/people.schema.alter', okc)
            call copy_file(TDIR // '/people.schema', ODIR // '/people.altering', okc)
            select case (k)
            case (1);  call put_bytes(ODIR // '/people.blob.alter', '')
            case (2);  call put_bytes(ODIR // '/people.blob', '')
            case (3);  call put_bytes(ODIR // '/people.blob.alter', '')
                       call put_bytes(ODIR // '/people.blob', 'orphaned bytes from an earlier table')
            end select

            call db_open(db, ODIR, rs, emsg)
            call check(rs == SQR_OK, tag // ': read-write open recovers')
            if (rs /= SQR_OK) cycle recover
            ti = db_table_index(db, 'people')
            call check(db%tables(ti)%ncols == 4 .and. db%tables(ti)%blob_next == 1_int64, &
                       tag // ': four columns over an empty blob file')
            call db_set_text(db, 'people', 2_int32, 'bio', 'written after recovery', rs)
            call check(rs == SQR_OK, tag // ': the TEXT column takes a value')
            call db_verify(db, 'people', rs, emsg)
            call check(rs == SQR_OK, tag // ': db_verify passes')
            call db_close(db)
            call check(.not. c_path_exists(ODIR // '/people.altering') .and. &
                       .not. c_path_exists(ODIR // '/people.blob.alter') .and. &
                       c_path_exists(ODIR // '/people.blob'), &
                       tag // ': marker and staged blob gone, blob file in place')
            call db_open(db, ODIR, rs, emsg)
            txt = ''
            if (rs == SQR_OK) call db_get_text(db, 'people', 2_int32, 'bio', txt, rs)
            call check(rs == SQR_OK .and. txt == 'written after recovery', &
                       tag // ': the value reads back after a reopen')
            call db_close(db)
        end do recover

        ios = c_rmtree(ODIR)
        call altrec_people(ODIR)
        call put_bytes(ODIR // '/people.blob', 'orphaned bytes from an earlier table')
        call db_open(db, ODIR, rs, emsg)
        call db_add_column(db, 'people', bio, rs, emsg)
        ti = db_table_index(db, 'people')
        call check(rs == SQR_OK .and. db%tables(ti)%blob_next == 1_int64, &
                   'alttext: first TEXT column added over an orphan blob file starts empty')
        call check(.not. c_path_exists(ODIR // '/people.blob.alter') .and. &
                   c_path_exists(ODIR // '/people.blob'), &
                   'alttext: the staged blob file became the blob file')
        call db_set_text(db, 'people', 1_int32, 'bio', 'first text', rs)
        call check(rs == SQR_OK, 'alttext: the new column takes a value')
        call db_close(db)
        call db_open(db, ODIR, rs, emsg)
        txt = ''
        if (rs == SQR_OK) call db_get_text(db, 'people', 1_int32, 'bio', txt, rs)
        call check(rs == SQR_OK .and. txt == 'first text', 'alttext: and it reads back after a reopen')
        call db_verify(db, 'people', rs, emsg)
        call check(rs == SQR_OK, 'alttext: db_verify passes after the reopen')
        call db_close(db)
        ios = c_rmtree(ODIR);  ios = c_rmtree(TDIR)
    end subroutine test_alter_first_text

    ! Write `bytes` (possibly none) as the whole of `path`.
    subroutine put_bytes(path, bytes)
        character(len=*), intent(in) :: path, bytes
        integer :: u
        open(newunit=u, file=path, access='stream', form='unformatted', status='replace', action='write')
        if (len(bytes) > 0) write(u) bytes
        close(u)
    end subroutine put_bytes

    ! A whole file as a byte string.
    subroutine get_bytes(path, bytes)
        character(len=*),              intent(in)  :: path
        character(len=:), allocatable, intent(out) :: bytes
        integer :: u
        integer(int64) :: sz
        open(newunit=u, file=path, access='stream', form='unformatted', status='old', action='read')
        inquire(unit=u, size=sz)
        allocate(character(len=int(sz)) :: bytes)
        if (sz > 0_int64) read(u) bytes
        close(u)
    end subroutine get_bytes

    ! A two-row people(pid, age, name) table with a name index (altrec fixture).
    subroutine altrec_people(dir)
        character(len=*), intent(in) :: dir
        type(db_t), target :: d
        type(column_t) :: c(3)
        integer :: r, tj
        character(len=:), allocatable :: b
        c(1)%name = 'pid'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
        call db_open(d, dir, r)
        call db_create_table(d, 'people', c, r)
        tj = db_table_index(d, 'people')
        call db_create_index(d, 'people', 'name', r)
        call ins_ic3(d, 'people', tj, 1_int32, 30_int32, 'Alice', b)
        call ins_ic3(d, 'people', tj, 2_int32, 40_int32, 'Bob',   b)
        call db_close(d)
    end subroutine altrec_people

    subroutine test_alter_column()
        character(len=*), parameter :: ADIR = 'utest_sqr_alter_db'
        type(db_t), target :: db
        type(column_t) :: c(3), nc
        integer :: rs, ios, ti, rsz0, rsz1
        integer(int32) :: rid
        character(len=:), allocatable :: buf, got
        character(len=128) :: emsg
        logical :: ex

        ios = c_rmtree(ADIR)
        c(1)%name = 'pid'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'age'  ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'name' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
        call db_open(db, ADIR, rs, emsg)
        call db_create_table(db, 'people', c, rs, emsg)
        ti = db_table_index(db, 'people')
        rsz0 = db%tables(ti)%record_size
        ! age index (will be CASCADE-dropped); name index (a survivor whose
        ! column ordinal shifts when 'age' is later dropped — exercises col_idx
        ! re-resolution).
        call db_create_index(db, 'people', 'age', rs)
        call db_create_index(db, 'people', 'name', rs)
        call ins_ic3(db, 'people', ti, 1_int32, 30_int32, 'Alice', buf)
        call ins_ic3(db, 'people', ti, 2_int32, 40_int32, 'Bob',   buf)
        call ins_ic3(db, 'people', ti, 3_int32, 50_int32, 'Carol', buf)
        call db_delete(db, 'people', 2_int32, rs)   ! tombstone the middle row
        call check(rs == SQR_OK, 'alter: setup (2 indices, 3 rows, 1 deleted)')

        ! --- ADD a DT_INT column ---
        nc%name = 'score'; nc%dtype = DT_INT; nc%csize = 4
        call db_add_column(db, 'people', nc, rs, emsg)
        call check(rs == SQR_OK, 'alter: add column score')
        ti = db_table_index(db, 'people')
        rsz1 = db%tables(ti)%record_size
        call check(db%tables(ti)%ncols == 4 .and. rsz1 > rsz0, 'alter: ncols and record grew')

        ! Existing rows intact and at their original row_ids (row 2 still gone).
        call row_alloc(buf, rsz1)
        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. &
                   row_get_int(buf, col_of(db,'people','pid')) == 1 .and. &
                   row_get_int(buf, col_of(db,'people','age')) == 30 .and. &
                   trim(row_get_char(buf, col_of(db,'people','name'))) == 'Alice', &
                   'alter: row 1 values survive add')
        call check(row_is_null(buf, col_of(db,'people','score')), &
                   'alter: added column reads NULL on existing row')
        call db_get(db, 'people', 2_int32, buf, rs)
        call check(rs == SQR_NOT_FOUND, 'alter: tombstoned row_id preserved across add')
        call db_get(db, 'people', 3_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, col_of(db,'people','pid')) == 3, &
                   'alter: row 3 (row_id 3) preserved across add')

        ! Both indices still resolve (no rebuild needed; keys/row_ids unchanged).
        call db_find_by_int(db, 'people', 'age', 50_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'alter: age index intact after add')
        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'alter: name index intact after add')

        ! Write the new column on an existing row.
        call db_get(db, 'people', 1_int32, buf, rs)
        call row_set_int(buf, col_of(db,'people','score'), 99_int32)
        call db_update(db, 'people', 1_int32, buf, rs)
        call db_get(db, 'people', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. .not. row_is_null(buf, col_of(db,'people','score')) .and. &
                   row_get_int(buf, col_of(db,'people','score')) == 99, &
                   'alter: new column writable after add')

        ! --- ADD a DT_TEXT column to a table that had no text (creates blob) ---
        nc%name = 'bio'; nc%dtype = DT_TEXT; nc%csize = SQR_TEXT_DESC
        call db_add_column(db, 'people', nc, rs, emsg)
        call check(rs == SQR_OK, 'alter: add DT_TEXT column')
        inquire(file=ADIR // '/people.blob', exist=ex)
        call check(ex, 'alter: blob file created for first text column')
        call db_set_text(db, 'people', 1_int32, 'bio', 'hello bio', rs)
        call db_get_text(db, 'people', 1_int32, 'bio', got, rs)
        call check(rs == SQR_OK .and. got == 'hello bio', 'alter: text on added column round-trips')
        call db_verify(db, 'people', rs, emsg)
        call check(rs == SQR_OK, 'alter: db_verify passes after adds')

        ! --- Error paths for add ---
        call db_add_column(db, 'nosuch', nc, rs, emsg)
        call check(rs == SQR_NOT_FOUND, 'alter: add to missing table')
        nc%name = 'age'; nc%dtype = DT_INT; nc%csize = 4   ! duplicate name
        call db_add_column(db, 'people', nc, rs, emsg)
        call check(rs == SQR_INVALID, 'alter: add duplicate column name rejected')
        nc%name = 'bad'; nc%dtype = DT_INT; nc%csize = 8   ! wrong csize
        call db_add_column(db, 'people', nc, rs, emsg)
        call check(rs == SQR_INVALID, 'alter: add malformed column rejected')

        ! --- Persistence round-trip ---
        call db_close(db)
        call db_open(db, ADIR, rs, emsg)
        ti = db_table_index(db, 'people')
        call check(rs == SQR_OK .and. db%tables(ti)%ncols == 5, 'alter: added columns persist')
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 'people', 3_int32, buf, rs)
        call check(row_is_null(buf, col_of(db,'people','score')), 'alter: NULL column persists')
        call db_get_text(db, 'people', 1_int32, 'bio', got, rs)
        call check(got == 'hello bio', 'alter: text persists across reopen')
        call db_find_by_char(db, 'people', 'name', 'Carol', rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'alter: index persists across reopen')

        ! --- DROP a non-indexed column ---
        call db_drop_column(db, 'people', 'score', rs, emsg)
        call check(rs == SQR_OK, 'alter: drop column score')
        ti = db_table_index(db, 'people')
        nc = col_of(db, 'people', 'score')   ! csize 0 ⇒ no such column
        call check(db%tables(ti)%ncols == 4 .and. nc%csize == 0, &
                   'alter: score gone after drop')
        call db_get_text(db, 'people', 1_int32, 'bio', got, rs)
        call check(got == 'hello bio', 'alter: text survives a non-text column drop')
        call db_find_by_int(db, 'people', 'age', 30_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'alter: age index survives unrelated drop')

        ! --- DROP an indexed column: CASCADE drops its index; survivor stands ---
        call db_drop_column(db, 'people', 'age', rs, emsg)
        call check(rs == SQR_OK, 'alter: drop indexed column age')
        call db_find_by_int(db, 'people', 'age', 30_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'alter: CASCADE removed the age index')
        call db_find_by_char(db, 'people', 'name', 'Alice', rid, rs)
        call check(rs == SQR_OK .and. rid == 1, 'alter: name index survives (col_idx re-resolved)')
        call db_get_text(db, 'people', 1_int32, 'bio', got, rs)
        call check(got == 'hello bio', 'alter: text survives an indexed-column drop')
        call db_verify(db, 'people', rs, emsg)
        call check(rs == SQR_OK, 'alter: db_verify passes after drops')

        ! --- DROP the last DT_TEXT column removes the blob file ---
        call db_drop_column(db, 'people', 'bio', rs, emsg)
        call check(rs == SQR_OK, 'alter: drop last text column')
        inquire(file=ADIR // '/people.blob', exist=ex)
        call check(.not. ex, 'alter: blob deleted with last text column')
        call row_alloc(buf, db%tables(db_table_index(db,'people'))%record_size)
        call db_get(db, 'people', 3_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, col_of(db,'people','pid')) == 3, &
                   'alter: row_id 3 still pid 3 after all drops')

        ! --- Cannot drop the only remaining column; missing-name/table paths ---
        call db_drop_column(db, 'people', 'name', rs, emsg)
        call check(rs == SQR_OK, 'alter: drop name (down to one column)')
        call db_drop_column(db, 'people', 'pid', rs, emsg)
        call check(rs == SQR_INVALID, 'alter: cannot drop the only column')
        call db_drop_column(db, 'people', 'zzz', rs, emsg)
        call check(rs == SQR_NOT_FOUND, 'alter: drop missing column')
        call db_drop_column(db, 'nosuch', 'pid', rs, emsg)
        call check(rs == SQR_NOT_FOUND, 'alter: drop from missing table')

        call db_close(db)
        ios = c_rmtree(ADIR)
    end subroutine

    ! ===== 5.3 db_verify =====

    subroutine test_db_verify()
        character(len=*), parameter :: VDIR = 'utest_sqr_verify_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, rsz, u
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        ios = c_rmtree(VDIR)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'age' ; c(2)%dtype = DT_INT ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR; c(3)%csize = 8
        call db_open(db, VDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        ti = db_table_index(db, 'p')
        call ins_ic3(db, 'p', ti, 1_int32, 30_int32, 'x', buf)
        call ins_ic3(db, 'p', ti, 2_int32, 40_int32, 'y', buf)
        call ins_ic3(db, 'p', ti, 3_int32, 50_int32, 'z', buf)
        call db_create_index(db, 'p', 'age', rs, unique=.true.)
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_OK, 'verify: clean table+index passes')
        call db_verify(db, 'nosuch', rs, emsg)
        call check(rs == SQR_NOT_FOUND, 'verify: missing table -> NOT_FOUND')

        ! Tombstone row 2's status byte directly, bypassing db_delete's
        ! live_count decrement, so the recount disagrees with the header.
        rsz = db%tables(ti)%record_size
        call db_close(db)
        open(newunit=u, file=VDIR // '/p.dat', access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        write(u, pos=int(2 - 1, kind=8)*rsz + 1) ROW_TOMBSTONE
        close(u)
        call db_open(db, VDIR, rs, emsg)
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_INVALID, 'verify: live_count tamper detected')
        call db_close(db)
        ios = c_rmtree(VDIR)

        ! Stale index entry: rewrite a row's indexed bytes under the index so the
        ! entry key disagrees with the row.
        ios = c_rmtree(VDIR)
        call db_open(db, VDIR, rs, emsg)
        call db_create_table(db, 'q', c, rs, emsg)
        ti = db_table_index(db, 'q')
        call ins_ic3(db, 'q', ti, 1_int32, 30_int32, 'x', buf)
        call ins_ic3(db, 'q', ti, 2_int32, 40_int32, 'y', buf)
        call db_create_index(db, 'q', 'age', rs)
        block
            integer :: aoff
            character(len=4) :: newbytes
            aoff = db%tables(ti)%cols(2)%offset
            rsz  = db%tables(ti)%record_size
            call db_close(db)
            ! overwrite row 1's age field (now 99) without touching the index
            newbytes = transfer(99_int32, newbytes)
            open(newunit=u, file=VDIR // '/q.dat', access='stream', form='unformatted', &
                 status='old', action='readwrite', iostat=ios)
            write(u, pos=int(1 - 1, kind=8)*rsz + aoff) newbytes
            close(u)
        end block
        call db_open(db, VDIR, rs, emsg)
        call db_verify(db, 'q', rs, emsg)
        call check(rs == SQR_INVALID, 'verify: stale index entry detected')

        call db_close(db)
        ios = c_rmtree(VDIR)
    end subroutine

    ! Review §3: has_dup_live_keys must catch (k,live)(k,dead)(k,live) —
    ! adjacent-pair comparison missed a duplicate split by a dead entry.
    subroutine test_verify_dup_split_by_dead()
        character(len=*), parameter :: VDIR = 'utest_sqr_dupdead_db'
        type(db_t), target :: db
        type(column_t) :: c(3)
        integer :: rs, ios, ti, rsz, u
        character(len=:), allocatable :: buf
        character(len=128) :: emsg

        ios = c_rmtree(VDIR)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'age' ; c(2)%dtype = DT_INT ; c(2)%csize = 4
        c(3)%name = 'name'; c(3)%dtype = DT_CHAR; c(3)%csize = 8
        call db_open(db, VDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        call db_create_index(db, 'p', 'age', rs, unique=.true.)
        ti = db_table_index(db, 'p')
        ! Three same-key inserts with deletes in between are legal under a
        ! unique index (dead rows don't count); deletes never touch the
        ! index, so it keeps all three key-50 entries in rid order.
        call ins_ic3(db, 'p', ti, 1_int32, 50_int32, 'a', buf)
        call db_delete(db, 'p', 1_int32, rs)
        call ins_ic3(db, 'p', ti, 2_int32, 50_int32, 'b', buf)
        call db_delete(db, 'p', 2_int32, rs)
        call ins_ic3(db, 'p', ti, 3_int32, 50_int32, 'c', buf)
        call ins_ic3(db, 'p', ti, 4_int32, 70_int32, 'd', buf)
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_OK, 'dup-by-dead: legal history verifies clean')

        ! Corrupt: resurrect rid1 and tombstone rid4. Live-row recount and
        ! per-index matched-entry counts still balance, so only the
        ! duplicate-key walk can see the fault: the key-50 entries are now
        ! (50,rid1)live (50,rid2)dead (50,rid3)live.
        rsz = db%tables(ti)%record_size
        call db_close(db)
        open(newunit=u, file=VDIR // '/p.dat', access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        write(u, pos=int(1 - 1, kind=8)*rsz + 1) ROW_ALIVE
        write(u, pos=int(4 - 1, kind=8)*rsz + 1) ROW_TOMBSTONE
        close(u)
        call db_open(db, VDIR, rs, emsg)
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_INVALID, 'dup-by-dead: duplicate split by dead entry detected')
        call db_close(db)
        ios = c_rmtree(VDIR)
    end subroutine

    ! Review §3: db_list_tables on a handle that was never opened must
    ! return an empty list, not reference an unallocated array.
    subroutine test_list_tables_closed()
        type(db_t), target :: db
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        call db_list_tables(db, names)
        call check(size(names) == 0, 'list_tables: closed handle -> empty list')
    end subroutine

    ! Review §3: a mid-failure in db_create_table must leave no trace —
    ! no leaked unit, no orphan .dat/.schema files, no phantom table.
    subroutine test_create_table_cleanup()
        character(len=*), parameter :: CDIR = 'utest_sqr_ctfail_db'
        type(db_t), target :: db
        type(column_t) :: c(2), ct(2)
        integer :: rs, ios, u
        character(len=128) :: emsg

        ios = c_rmtree(CDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'nm' ; c(2)%dtype = DT_CHAR; c(2)%csize = 8
        ct(1) = c(1)
        ct(2)%name = 'note'; ct(2)%dtype = DT_TEXT; ct(2)%csize = SQR_TEXT_DESC
        call db_open(db, CDIR, rs, emsg)

        ! (a) a blocking file where the fresh '.dat' must go.  (Since the
        ! files-already-exist check this is refused before anything is
        ! written; the failure tails themselves now need an I/O fault.)
        open(newunit=u, file=CDIR // '/t.dat', status='new', iostat=ios)
        close(u)
        call db_create_table(db, 't', c, rs, emsg)
        call check(rs /= SQR_OK, 'create cleanup: blocked .dat fails')
        call check(.not. c_path_exists(CDIR // '/t.schema'), &
                   'create cleanup: schema removed after data-open failure')
        call check(db_table_index(db, 't') == 0, 'create cleanup: no phantom table (a)')
        ios = c_remove(CDIR // '/t.dat')
        call db_create_table(db, 't', c, rs, emsg)
        call check(rs == SQR_OK, 'create cleanup: succeeds once unblocked')

        ! (b) a directory squatting on the '.blob' path: refused likewise.
        ios = c_mkdir(CDIR // '/u.blob')
        call db_create_table(db, 'u', ct, rs, emsg)
        call check(rs /= SQR_OK, 'create cleanup: blocked .blob fails')
        call check(.not. c_path_exists(CDIR // '/u.dat'), &
                   'create cleanup: data file removed after blob failure')
        call check(.not. c_path_exists(CDIR // '/u.schema'), &
                   'create cleanup: schema removed after blob failure')
        call check(db_table_index(db, 'u') == 0, 'create cleanup: no phantom table (b)')
        ios = c_rmtree(CDIR // '/u.blob')
        call db_create_table(db, 'u', ct, rs, emsg)
        call check(rs == SQR_OK, 'create cleanup: text table succeeds once unblocked')

        call db_close(db)
        ios = c_rmtree(CDIR)
    end subroutine

    ! Slice B: the db_record_size enquiry apps size their buffers with.
    subroutine test_record_size()
        character(len=*), parameter :: RDIR = 'utest_sqr_rsz_db'
        type(db_t), target :: db, never_opened
        type(column_t) :: c(2)
        integer :: rs, ios, ti
        character(len=128) :: emsg
        ios = c_rmtree(RDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'nm'; c(2)%dtype = DT_CHAR; c(2)%csize = 8
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call check(db_record_size(db, 't') == db%tables(ti)%record_size, &
                   'record_size: matches the table layout')
        call check(db_record_size(db, 't') > 0, 'record_size: positive for a real table')
        call check(db_record_size(db, 'nosuch') == 0, 'record_size: unknown table -> 0')
        call check(db_record_size(never_opened, 't') == 0, 'record_size: closed handle -> 0')
        call check(db%record_size('t') == db_record_size(db, 't'), 'record_size: type-bound form')
        ! D5: 40,000 CHAR(65536) columns sum past huge(0); refused, not wrapped
        wide: block
            type(column_t), allocatable :: w(:)
            integer :: k
            allocate(w(40000))
            do k = 1, size(w)
                write(w(k)%name, '(a,i0)') 'c', k
                w(k)%dtype = DT_CHAR; w(k)%csize = SQR_MAX_CHAR
            end do
            emsg = ''
            call db_create_table(db, 'wide', w, rs, emsg)
            call check(rs == SQR_INVALID .and. index(emsg, 'record size too large') > 0 .and. &
                       db_table_index(db, 'wide') == 0, &
                       'record_size: 40,000 CHAR(65536) columns refused')
        end block wide
        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine

    ! Slice B: writes reject a wrong-length buffer with SQR_INVALID;
    ! reads stay tolerant of the caller's buffer length.
    subroutine test_strict_write_length()
        character(len=*), parameter :: WDIR = 'utest_sqr_strict_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti, rsz
        integer(int32) :: rid, rids(2)
        character(len=:), allocatable :: buf, short, long
        character(len=8)   :: sbuf
        character(len=300) :: lbuf
        character(len=128) :: emsg
        ios = c_rmtree(WDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'nm'; c(2)%dtype = DT_CHAR; c(2)%csize = 8
        call db_open(db, WDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        rsz = db_record_size(db, 't')

        short = repeat(char(0), rsz - 1)
        long  = repeat(char(0), rsz + 1)
        call db_insert(db, 't', short, rid, rs)
        call check(rs == SQR_INVALID .and. rid == 0, 'strict: short insert rejected')
        call db_insert(db, 't', long, rid, rs)
        call check(rs == SQR_INVALID, 'strict: long insert rejected')

        call row_alloc(buf, rsz)
        call row_set_int (buf, db%tables(ti)%cols(1), 7_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), 'seven')
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_OK, 'strict: exact-length insert accepted')

        call db_update(db, 't', rid, short, rs)
        call check(rs == SQR_INVALID, 'strict: short update rejected')
        call db_update(db, 't', rid, long, rs)
        call check(rs == SQR_INVALID, 'strict: long update rejected')

        call db_insert_many(db, 't', [short, short], rids, rs)
        call check(rs == SQR_INVALID, 'strict: wrong-length batch rejected')

        ! Reads fill whatever they are given: short truncates, long blank-pads.
        sbuf = ''
        call db_get(db, 't', rid, sbuf, rs)
        call check(rs == SQR_OK, 'tolerant: short read buffer accepted')
        lbuf = ''
        call db_get(db, 't', rid, lbuf, rs)
        call check(rs == SQR_OK, 'tolerant: long read buffer accepted')
        call check(row_get_int(lbuf, db%tables(ti)%cols(1)) == 7_int32, &
                   'tolerant: long read returns the record')
        call db_close(db)
        ios = c_rmtree(WDIR)
    end subroutine

    ! Slice B: the int32 row-id space is guarded (SQR_FULL), not wrapped.
    subroutine test_row_id_exhaustion()
        character(len=*), parameter :: FDIR = 'utest_sqr_full_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, ti
        integer(int32) :: rid, rids(2)
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(FDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, FDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db_record_size(db, 't'))
        call row_set_int(buf, db%tables(ti)%cols(1), 1_int32)
        ! Simulate an exhausted table by advancing the id high-water mark
        ! directly (2^31 real inserts are not a unit test).  The guard
        ! fires before anything touches disk.
        db%tables(ti)%next_id = huge(0_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_FULL .and. rid == 0, 'full: insert refused at id exhaustion')
        db%tables(ti)%next_id = huge(0_int32) - 1
        call db_insert_many(db, 't', [buf, buf], rids, rs)
        call check(rs == SQR_FULL, 'full: batch overrunning the id space refused')
        call db_close(db)
        ios = c_rmtree(FDIR)
    end subroutine

    ! Slice B: a read-only open works when the lock file itself cannot be
    ! opened for write (read-only media) — the C shim falls back to
    ! O_RDONLY, which still takes a shared lock.  A read-write open on
    ! the same database must still be refused.
    subroutine test_readonly_media_lock()
        character(len=*), parameter :: MDIR = 'utest_sqr_romedia_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, ti
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(MDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, MDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db_record_size(db, 't'))
        call row_set_int(buf, db%tables(ti)%cols(1), 42_int32)
        call db_insert(db, 't', buf, rid, rs)
        call db_close(db)

        ! Approximate read-only media at the exact failure point: the lock
        ! file no longer opens read-write.
        ios = c_chmod(MDIR // '/_lock', int(o'444'))
        call check(ios == 0, 'ro media: chmod _lock read-only')
        if (file_still_writable(MDIR // '/_lock')) then
            ! See "Permission-based tests" above the probes.
            call skip('ro media: read-write open refused', 'chmod 444 is not enforced here')
            call skip('ro media: read-only open succeeds', 'chmod 444 is not enforced here')
            call skip('ro media: row readable',            'chmod 444 is not enforced here')
            ios = c_chmod(MDIR // '/_lock', int(o'644'))
            ios = c_rmtree(MDIR)
            return
        end if
        call db_open(db, MDIR, rs, emsg)
        call check(rs /= SQR_OK, 'ro media: read-write open refused')
        emsg = ''
        call db_open(db, MDIR, rs, emsg, readonly=.true.)
        call check(rs == SQR_OK, 'ro media: read-only open succeeds')
        if (rs == SQR_OK) then
            ti = db_table_index(db, 't')
            call db_get(db, 't', rid, buf, rs)
            call check(rs == SQR_OK .and. row_get_int(buf, db%tables(ti)%cols(1)) == 42_int32, &
                       'ro media: row readable')
            call db_close(db)
        end if
        ios = c_chmod(MDIR // '/_lock', int(o'644'))
        ios = c_rmtree(MDIR)
    end subroutine

    ! A read-write open followed by a close with nothing changed writes
    ! NOTHING: the schema counters match the file's image and the catalog is
    ! current, so neither is rewritten.  Proved black-box by making the
    ! directory unwritable between open and close — a rewrite goes through a
    ! temp file + rename, which the unwritable directory refuses — and
    ! requiring a clean close.  The mirror case (a row inserted, so the
    ! counters moved) must then FAIL its close, showing the probe can see a
    ! rewrite when there is one.  Also the journal: the working unit is held
    ! for the session, and a clean close must still delete the file.
    subroutine test_clean_close()
        character(len=*), parameter :: CDIR = 'utest_sqr_cleanclose_db'
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios, ti
        logical :: enforced
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        ios = c_rmtree(CDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, CDIR, rs)
        call db_create_table(db, 't', c, rs)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db_record_size(db, 't'))
        call row_set_int(buf, db%tables(ti)%cols(1), 42_int32)
        call db_insert(db, 't', buf, rid, rs)
        call db_close(db, rs)
        call check(rs == SQR_OK, 'clean close: seed store closes')
        call check(.not. c_path_exists(CDIR // '/_journal.dat'), &
                   'clean close: the journal file is gone after an autocommit session')

        ! Nothing changed: an unwritable directory must not trouble the close.
        call db_open(db, CDIR, rs)
        call check(rs == SQR_OK, 'clean close: reopen read-write')
        call db_get(db, 't', rid, buf, rs)
        ios = c_chmod(CDIR, int(o'555'))
        call check(ios == 0, 'clean close: chmod dir read-only')
        enforced = .not. dir_accepts_new_entry(CDIR)
        call db_close(db, rs)
        ios = c_chmod(CDIR, int(o'755'))
        call check(rs == SQR_OK, 'clean close: unchanged store closes without writing')

        ! Counters moved: the same probe must now see the schema rewrite fail.
        call db_open(db, CDIR, rs)
        call row_set_int(buf, db%tables(ti)%cols(1), 43_int32)
        call db_insert(db, 't', buf, rid, rs)
        call check(rs == SQR_OK, 'clean close: insert on the writable dir')
        ios = c_chmod(CDIR, int(o'555'))
        call db_close(db, rs)
        ios = c_chmod(CDIR, int(o'755'))
        if (enforced) then
            call check(rs /= SQR_OK, 'clean close: a moved counter needs the rewrite (probe sees it)')
        else
            ! See "Permission-based tests" above the probes.
            call skip('clean close: a moved counter needs the rewrite (probe sees it)', &
                      'chmod 555 on a directory is not enforced here')
        end if

        ! The store is still whole: the row count is re-derived from the data file.
        call db_open(db, CDIR, rs)
        call check(rs == SQR_OK .and. db_row_count(db, 't') == 2, &
                   'clean close: store reopens with both rows')
        call db_close(db)
        ios = c_rmtree(CDIR)
    end subroutine

    ! Review §5.4: engine-level errmsg CONTENT (substring matches so the
    ! wording can evolve).
    subroutine test_engine_errmsg()
        character(len=*), parameter :: EDIR = 'utest_sqr_errmsg_db'
        type(db_t), target :: db
        type(column_t) :: c(2), big(16)
        integer :: rs, ios, i
        character(len=256) :: emsg
        ios = c_rmtree(EDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'nm'; c(2)%dtype = DT_CHAR; c(2)%csize = 8

        emsg = ''
        call db_open(db, 'no_such_db_dir', rs, emsg, readonly=.true.)
        call check(index(emsg, 'database not found') > 0, 'errmsg: missing db named')

        call db_open(db, EDIR, rs, emsg)
        emsg = ''
        call db_create_table(db, 'bad/name', c, rs, emsg)
        call check(index(emsg, 'invalid table name') > 0, 'errmsg: invalid name quoted')
        emsg = ''
        call db_create_table(db, '_journal', c, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'reserved table name: "_journal"') > 0, &
                   'errmsg: reserved name refused and quoted')
        call db_create_table(db, 't', c, rs, emsg)
        emsg = ''
        call db_create_table(db, 't', c, rs, emsg)
        call check(index(emsg, 'already exists') > 0, 'errmsg: duplicate table named')

        ! 16 x CHAR(65536) = exactly SQR_MAX_RECORD before the status byte
        ! and NULL bitmap push it over — every csize individually valid.
        do i = 1, size(big)
            write(big(i)%name, '(a,i0)') 'c', i
            big(i)%dtype = DT_CHAR; big(i)%csize = 65536
        end do
        emsg = ''
        call db_create_table(db, 'wide', big, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'record size too large') > 0, &
                   'errmsg: over-cap record size described')
        emsg = ''
        call db_verify(db, 'nosuch', rs, emsg)
        call check(index(emsg, 'no such table') > 0, 'errmsg: verify on missing table')
        call db_close(db)
        ios = c_rmtree(EDIR)
    end subroutine

    ! Review §5.5: a record sized close to SQR_MAX_RECORD must round-trip.
    subroutine test_wide_table()
        character(len=*), parameter :: WDIR = 'utest_sqr_wide_db'
        type(db_t), target :: db
        type(column_t) :: c(16)
        integer :: rs, ios, ti, i, rsz
        integer(int32) :: rid
        character(len=:), allocatable :: buf, back
        character(len=128) :: emsg
        ios = c_rmtree(WDIR)
        ! 15 x CHAR(65536) (the per-column maximum) + an int: ~983 KB of
        ! the 1 MiB record cap.
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        wide_cols: do i = 2, 16
            write(c(i)%name, '(a,i0)') 'blk', i
            c(i)%dtype = DT_CHAR; c(i)%csize = 65536
        end do wide_cols
        call db_open(db, WDIR, rs, emsg)
        call db_create_table(db, 'w', c, rs, emsg)
        call check(rs == SQR_OK, 'wide: ~1MB record accepted')
        ti = db_table_index(db, 'w')
        rsz = db_record_size(db, 'w')
        call check(rsz > 900000 .and. rsz <= SQR_MAX_RECORD, 'wide: record size near the cap')
        call row_alloc(buf, rsz)
        call row_set_int (buf, db%tables(ti)%cols(1), 7_int32)
        call row_set_char(buf, db%tables(ti)%cols(2), repeat('x', 65536))
        call row_set_char(buf, db%tables(ti)%cols(16), repeat('y', 65536))
        call db_insert(db, 'w', buf, rid, rs)
        call check(rs == SQR_OK, 'wide: insert ok')
        call row_alloc(back, rsz)
        call db_get(db, 'w', rid, back, rs)
        call check(rs == SQR_OK .and. &
                   row_get_int(back, db%tables(ti)%cols(1)) == 7_int32 .and. &
                   row_get_char(back, db%tables(ti)%cols(2)) == repeat('x', 65536) .and. &
                   row_get_char(back, db%tables(ti)%cols(16)) == repeat('y', 65536), &
                   'wide: round-trip intact')
        call db_close(db)
        ios = c_rmtree(WDIR)
    end subroutine

    ! Review §5.5: db_scan over a zero-row table — the callback never
    ! fires and the scan reports SQR_OK.
    subroutine test_scan_empty()
        character(len=*), parameter :: SDIR = 'utest_sqr_scan0_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        type(scan_ctx_t) :: ctx
        integer :: rs, ios, ti
        character(len=128) :: emsg
        ios = c_rmtree(SDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'age'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, SDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        ti = db_table_index(db, 'p')
        ctx%age_col = db%tables(ti)%cols(2)
        call db_scan(db, 'p', scan_count_and_sum, ctx, rs)
        call check(rs == SQR_OK .and. ctx%count == 0, 'scan: zero-row table -> OK, no callbacks')
        call db_close(db)
        ios = c_rmtree(SDIR)
    end subroutine

    ! scan_cb's contract (LOWS G6): the callback may insert, update and
    ! delete.  The scan visits the row ids that existed when it began: a row
    ! updated ahead of it is seen as updated, one deleted ahead of it is
    ! skipped, one inserted is not visited.  An index is kept through it.
    subroutine test_scan_mutation()
        character(len=*), parameter :: SDIR = 'utest_sqr_scanmut_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        type(scan_mut_ctx_t) :: ctx
        character(len=:), allocatable :: buf
        integer :: rs, ios, ti, k, st
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(SDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'age'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, SDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        call db_create_index(db, 'p', 'age', rs)
        ti = db_table_index(db, 'p')
        ctx%cols = db%tables(ti)%cols(1:2)
        ctx%record_size = db%tables(ti)%record_size
        call row_alloc(buf, ctx%record_size)
        fill: do k = 1, 4
            call row_set_int(buf, ctx%cols(1), int(k, int32))
            call row_set_int(buf, ctx%cols(2), int(10 * k, int32))
            call db_insert(db, 'p', buf, rid, rs)
        end do fill
        call db_scan(db, 'p', scan_and_mutate, ctx, rs)
        call check(rs == SQR_OK .and. ctx%bad == 0, 'scan mutation: every change from the callback succeeded')
        call check(ctx%count == 3 .and. ctx%sum == 10 + 20 + 33, &
                   'scan mutation: updated row seen updated, deleted row skipped, inserted row not visited')
        emsg = ''
        call db_verify(db, 'p', st, emsg)
        call check(st == SQR_OK .and. db_row_count(db, 'p') == 4, 'scan mutation: table and index verify, 4 rows')
        call db_find_by_int(db, 'p', 'age', 50_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 5, 'scan mutation: the inserted row is found by its index')
        call db_close(db)
        ios = c_rmtree(SDIR)
    end subroutine

    ! At row 1: update row 3 (age 33), delete row 4, insert a row (age 50).
    subroutine scan_and_mutate(scan_db, rid, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        character(len=:), allocatable :: nb
        integer(int32) :: newid
        integer :: rs
        stop = .false.
        select type (ctx)
        type is (scan_mut_ctx_t)
            ctx%count = ctx%count + 1
            ctx%sum   = ctx%sum + row_get_int(buf, ctx%cols(2))
            if (rid /= 1) return
            call row_alloc(nb, ctx%record_size)
            call row_set_int(nb, ctx%cols(1), 3_int32)
            call row_set_int(nb, ctx%cols(2), 33_int32)
            call db_update(scan_db, 'p', 3_int32, nb, rs)
            if (rs /= SQR_OK) ctx%bad = ctx%bad + 1
            call db_delete(scan_db, 'p', 4_int32, rs)
            if (rs /= SQR_OK) ctx%bad = ctx%bad + 1
            call row_set_int(nb, ctx%cols(1), 5_int32)
            call row_set_int(nb, ctx%cols(2), 50_int32)
            call db_insert(scan_db, 'p', nb, newid, rs)
            if (rs /= SQR_OK .or. newid /= 5) ctx%bad = ctx%bad + 1
        end select
    end subroutine

    ! Review §5.5: SQR_NAME_LEN boundary — a 32-char table name works and
    ! survives a reopen; 33 chars is rejected.
    subroutine test_name_boundary()
        character(len=*), parameter :: NDIR = 'utest_sqr_name_db'
        character(len=32), parameter :: N32 = repeat('abcd', 8)   ! exactly SQR_NAME_LEN
        type(db_t), target :: db
        type(column_t) :: c(1)
        integer :: rs, ios
        character(len=128) :: emsg
        ios = c_rmtree(NDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, NDIR, rs, emsg)
        call db_create_table(db, N32, c, rs, emsg)
        call check(rs == SQR_OK, 'names: 32-char table name accepted')
        call db_create_table(db, N32 // 'x', c, rs, emsg)
        call check(rs == SQR_INVALID, 'names: 33-char table name rejected')
        call db_close(db)
        call db_open(db, NDIR, rs, emsg)
        call check(db_table_index(db, N32) > 0, 'names: 32-char name survives reopen')
        call db_close(db)
        ios = c_rmtree(NDIR)
    end subroutine

    ! Review §5.5: reopen a tree after removes — updates that move keys
    ! drive bt_remove; the reopened index must serve the new keys and
    ! miss the old ones, and verify clean.
    subroutine test_index_reopen_after_removes()
        character(len=*), parameter :: RDIR = 'utest_sqr_reopen_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti, i
        integer(int32) :: rid, r
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        ios = c_rmtree(RDIR)
        c(1)%name = 'id'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'k' ; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, RDIR, rs, emsg)
        call db_create_table(db, 'p', c, rs, emsg)
        call db_create_index(db, 'p', 'k', rs)
        ti = db_table_index(db, 'p')
        call row_alloc(buf, db_record_size(db, 'p'))
        fill: do i = 1, 50
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 'p', buf, rid, rs)
        end do fill
        ! Move half the keys: each update removes the old entry (bt_remove)
        ! and inserts the new one.
        move: do i = 2, 50, 2
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i + 1000, int32))
            call db_update(db, 'p', int(i, int32), buf, rs)
        end do move
        call db_close(db)

        call db_open(db, RDIR, rs, emsg)
        call db_verify(db, 'p', rs, emsg)
        call check(rs == SQR_OK, 'reopen: tree verifies after removes')
        call db_find_by_int(db, 'p', 'k', 1002_int32, r, rs)
        call check(rs == SQR_OK .and. r == 2_int32, 'reopen: moved key found')
        call db_find_by_int(db, 'p', 'k', 2_int32, r, rs)
        call check(rs == SQR_NOT_FOUND, 'reopen: old key gone')
        call db_find_by_int(db, 'p', 'k', 3_int32, r, rs)
        call check(rs == SQR_OK .and. r == 3_int32, 'reopen: unmoved key still found')
        call db_close(db)
        ios = c_rmtree(RDIR)
    end subroutine

    ! Review §5.5: idx_live asserted directly around create/drop.
    subroutine test_idx_live_direct()
        character(len=*), parameter :: LDIR = 'utest_sqr_idxlive_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti
        character(len=128) :: emsg
        ios = c_rmtree(LDIR)
        c(1)%name = 'a'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'b'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, LDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call db_create_index(db, 't', 'a', rs)
        call check(idx_live(db%tables(ti)%indices(1)), 'idx_live: fresh index live')
        call db_drop_index(db, 't', 'a', rs)
        call check(.not. idx_live(db%tables(ti)%indices(1)), 'idx_live: dropped slot dead')
        call db_create_index(db, 't', 'b', rs)
        call check(idx_live(db%tables(ti)%indices(db%tables(ti)%nindices)), &
                   'idx_live: later index live in its own slot')
        call db_close(db)
        ios = c_rmtree(LDIR)
    end subroutine

    ! ===== 5.3 db_insert_many =====

    subroutine test_insert_many()
        character(len=*), parameter :: BDIR = 'utest_sqr_batch_db'
        type(db_t), target :: db
        type(column_t) :: c(2)
        integer :: rs, ios, ti, rsz, k, nid0
        integer(int32) :: rid, rids(4)
        character(len=:), allocatable :: bufs(:)
        character(len=128) :: emsg

        ios = c_rmtree(BDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'sku'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, BDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        ti = db_table_index(db, 't')
        call db_create_index(db, 't', 'sku', rs, unique=.true.)
        rsz = db%tables(ti)%record_size
        allocate(character(len=rsz) :: bufs(4))

        ! Happy path: four distinct rows.
        do k = 1, 4
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), int(100 + k, int32))
        end do
        call db_insert_many(db, 't', bufs, rids, rs)
        call check(rs == SQR_OK .and. all(rids == [1_int32, 2_int32, 3_int32, 4_int32]), &
                   'batch: insert_many assigns sequential ids')
        call db_find_by_int(db, 't', 'sku', 103_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 3, 'batch: rows indexed after insert_many')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'batch: verify OK after insert_many')

        ! Intra-batch duplicate key -> whole batch rejected, nothing written.
        nid0 = db%tables(ti)%next_id
        do k = 1, 4
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(10 + k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), 300_int32)   ! all duplicate
        end do
        call db_insert_many(db, 't', bufs, rids, rs)
        call check(rs == SQR_DUP .and. all(rids == 0), 'batch: intra-batch dup -> DUP')
        call check(db%tables(ti)%next_id == nid0, 'batch: rejected batch wrote nothing')

        ! Collision with an existing row -> DUP.
        do k = 1, 4
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(20 + k, int32))
            call row_set_int(bufs(k), db%tables(ti)%cols(2), int(400 + k, int32))
        end do
        call row_set_int(bufs(2), db%tables(ti)%cols(2), 101_int32)   ! already present
        call db_insert_many(db, 't', bufs, rids, rs)
        call check(rs == SQR_DUP, 'batch: collision with existing row -> DUP')

        ! Multiple NULL keys in a unique index are allowed.
        do k = 1, 2
            bufs(k) = repeat(char(0), rsz)
            call row_set_int(bufs(k), db%tables(ti)%cols(1), int(50 + k, int32))
            call row_set_null(bufs(k), db%tables(ti)%cols(2))
        end do
        call db_insert_many(db, 't', bufs(1:2), rids(1:2), rs)
        call check(rs == SQR_OK, 'batch: multiple NULL unique keys allowed')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'batch: verify OK after NULL batch')

        call db_close(db)
        ios = c_rmtree(BDIR)
    end subroutine

    ! Structural / whole-table operations must be refused inside an explicit
    ! transaction: the rollback journal cannot undo their un-journalled file
    ! renames/creates/deletes, and drop-table shifts the snapshot's table slots.
    ! Each call below is one that would succeed outside a transaction, so the
    ! SQR_INVALID is the txn_block guard firing — and the transaction stays open
    ! and intact, committing cleanly afterwards with the table untouched.
    subroutine test_txn_structural_block()
        character(len=*), parameter :: SDIR = TEST_DIR // '_txnblock'
        type(db_t), target :: db
        type(column_t)     :: c(2), extra
        character(len=:), allocatable :: buf
        integer        :: rs, ti, i, ios
        integer(int32) :: rid
        character(len=128) :: emsg
        ios = c_rmtree(SDIR)
        c(1)%name = 'id';  c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'val'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, SDIR, rs, emsg)
        call db_create_table(db, 't', c, rs, emsg)
        call db_create_index(db, 't', 'id', rs)
        ti = db_table_index(db, 't')
        do i = 1, 3
            call row_alloc(buf, db%tables(ti)%record_size)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(10 * i, int32))
            call db_insert(db, 't', buf, rid, rs)
        end do

        call db_begin(db, rs)
        call check(rs == SQR_OK .and. db%jrnl%active, 'txnblock: begin')

        extra%name = 'extra'; extra%dtype = DT_INT; extra%csize = 4

        call db_create_table(db, 't2', c, rs, emsg)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: create_table refused')
        call db_drop_table(db, 't', rs)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: drop_table refused')
        call db_compact(db, 't', rs)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: compact refused')
        call db_add_column(db, 't', extra, rs, emsg)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: add_column refused')
        call db_drop_column(db, 't', 'val', rs, emsg)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: drop_column refused')
        call db_create_index(db, 't', 'val', rs)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: create_index refused')
        call db_drop_index(db, 't', 'id', rs)
        call check(rs == SQR_INVALID .and. db%jrnl%active, 'txnblock: drop_index refused')

        call db_commit(db, rs)
        call check(rs == SQR_OK .and. .not. db%jrnl%active, 'txnblock: commit after rejections')
        ti = db_table_index(db, 't')
        call check(db%tables(ti)%ncols == 2, 'txnblock: table still has 2 columns')
        call check(db_table_index(db, 't2') == 0, 'txnblock: no t2 created')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'txnblock: verify OK after rejections')

        call db_close(db)
        ios = c_rmtree(SDIR)
    end subroutine

    ! Advisory locking (Phase 3 isolation). flock/LockFileEx locks are held by
    ! the open file description, so two independent opens in this one process
    ! contend exactly as two processes would — letting us exercise the matrix
    ! without a subprocess.
    subroutine test_locking()
        character(len=*), parameter :: LDIR = 'utest_sqr_lock_db'
        type(db_t), target :: a, b, c
        type(column_t) :: cols(1)
        integer :: rs, ios
        ios = c_rmtree(LDIR)
        cols(1)%name = 'id'; cols(1)%dtype = DT_INT; cols(1)%csize = 4

        ! Create the database so a read-only open has something to attach to.
        ! It ends table-free: every handle opens its tables' files at db_open,
        ! and two handles in ONE process would then connect the same file to two
        ! units, which is processor dependent (flang refuses).  The lock matrix
        ! is a property of the lock file alone.  A create + drop writes the
        ! catalog a read-only open requires.
        call db_open(a, LDIR, stat=rs)
        call check(rs == SQR_OK, 'lock: initial rw open succeeds')
        call db_create_table(a, 't', cols, rs)
        call db_drop_table(a, 't', rs)
        call check(rs == SQR_OK, 'lock: initialised, table-free')

        ! A second writer is refused while the first holds the exclusive lock.
        call db_open(b, LDIR, stat=rs)
        call check(rs == SQR_LOCKED, 'lock: second writer blocked (SQR_LOCKED)')
        call check(.not. b%opened, 'lock: blocked writer left closed')

        ! A reader is likewise refused while a writer holds the lock.
        call db_open(b, LDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_LOCKED, 'lock: reader blocked by writer')
        call db_close(a)

        ! With no writer, several readers may share the lock.
        call db_open(a, LDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_OK, 'lock: first reader attaches')
        call db_open(b, LDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_OK, 'lock: second reader shares the lock')

        ! A writer cannot break in while readers hold the shared lock.
        call db_open(c, LDIR, stat=rs)
        call check(rs == SQR_LOCKED, 'lock: writer blocked by readers')
        call db_close(a)
        call db_close(b)

        ! Lock fully released: a writer can open again.
        call db_open(a, LDIR, stat=rs)
        call check(rs == SQR_OK, 'lock: writer reattaches after readers leave')
        call db_close(a)

        ios = c_rmtree(LDIR)
    end subroutine

    ! db_set_readonly downgrades a live writer to a shared reader so other
    ! read-only connections may attach, and makes the handle refuse writes.
    subroutine test_set_readonly()
        character(len=*), parameter :: SDIR = 'utest_sqr_setro_db'
        type(db_t), target :: a, b
        type(column_t) :: cols(1)
        integer :: rs, ios
        ios = c_rmtree(SDIR)
        cols(1)%name = 'id'; cols(1)%dtype = DT_INT; cols(1)%csize = 4

        ! Table-free for the reason test_locking gives: the reader below attaches
        ! in this same process, so no file may already be held by `a`.
        call db_open(a, SDIR, stat=rs)
        call db_create_table(a, 't', cols, rs)
        call db_drop_table(a, 't', rs)
        call check(rs == SQR_OK, 'setro: rw open, initialised, table-free')

        ! Before the downgrade, a concurrent reader is blocked by the writer.
        call db_open(b, SDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_LOCKED, 'setro: reader blocked before downgrade')

        ! Demote the writer; it must now refuse writes.
        call db_set_readonly(a, rs)
        call check(rs == SQR_OK, 'setro: set_readonly succeeds')
        call check(a%readonly, 'setro: handle now read-only')
        call db_create_table(a, 't', cols, rs)
        call check(rs == SQR_READONLY, 'setro: write refused after downgrade')

        ! With the lock downgraded to shared, a reader can now attach.
        call db_open(b, SDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_OK, 'setro: reader attaches after downgrade')
        call db_close(b)

        ! Idempotent on an already read-only handle; rejected on a closed one.
        call db_set_readonly(a, rs)
        call check(rs == SQR_OK, 'setro: idempotent when already read-only')
        call db_close(a)
        call db_set_readonly(a, rs)
        call check(rs == SQR_INVALID, 'setro: rejected on a closed handle')

        ios = c_rmtree(SDIR)
    end subroutine

    ! Review finding E2: a read-only handle writes nothing at close, so the
    ! demotion itself must save the row counters a writer keeps in memory.
    subroutine test_set_readonly_counters()
        character(len=*), parameter :: SDIR = 'utest_sqr_setroc_db'
        type(db_t), target :: a
        type(column_t) :: cols(1)
        character(len=:), allocatable :: buf
        character(len=128) :: emsg
        integer(int32) :: rid
        integer :: rs, ios, i
        ios = c_rmtree(SDIR)
        cols(1)%name = 'v'; cols(1)%dtype = DT_INT; cols(1)%csize = 4
        call db_open(a, SDIR, stat=rs)
        call db_create_table(a, 't', cols, rs)
        call row_alloc(buf, db_record_size(a, 't'))
        do i = 1, 3
            call row_set_int(buf, a%tables(1)%cols(1), int(i, int32))
            call db_insert(a, 't', buf, rid, rs)
        end do
        call db_close(a, rs)

        ! Delete, demote, close: the delete must still be counted afterwards.
        call db_open(a, SDIR, stat=rs)
        call db_delete(a, 't', 2_int32, rs)
        call db_begin(a, rs)
        emsg = ''
        call db_set_readonly(a, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'transaction is open') > 0, &
                   'setro counters: refused in a transaction, and says why')
        call db_rollback(a, rs)
        call check(c_path_exists(SDIR // '/_journal.dat'), 'setro counters: a voided journal is on disk')
        call db_set_readonly(a, rs)
        call check(rs == SQR_OK .and. a%readonly, 'setro counters: demoted')
        call check(.not. c_path_exists(SDIR // '/_journal.dat'), 'setro counters: demotion removed the journal')
        call db_close(a, rs)
        call check(rs == SQR_OK, 'setro counters: close after demotion ok')
        call db_open(a, SDIR, stat=rs)
        call check(db_row_count(a, 't') == 2, 'setro counters: the delete is counted after reopen')
        call db_verify(a, 't', rs, emsg)
        call check(rs == SQR_OK, 'setro counters: verify clean after delete + demote')

        ! The same for an insert: next_id and the count both survive.
        call row_set_int(buf, a%tables(1)%cols(1), 9_int32)
        call db_insert(a, 't', buf, rid, rs)
        call db_set_readonly(a, rs)
        call db_close(a, rs)
        call db_open(a, SDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_OK .and. db_row_count(a, 't') == 3, &
                   'setro counters: the insert is counted by a read-only reopen')
        call db_verify(a, 't', rs, emsg)
        call check(rs == SQR_OK, 'setro counters: verify clean after insert + demote')
        call db_close(a, rs)

        emsg = ''
        call db_set_readonly(a, rs, emsg)
        call check(rs == SQR_INVALID .and. index(emsg, 'not open') > 0, &
                   'setro counters: a closed handle is refused, and says why')
        ios = c_rmtree(SDIR)
    end subroutine

    ! The procedural metadata surface: db_describe / db_row_count / db_in_txn.
    ! R15, R16: one text form for a real, the same from every compiler - the
    ! E always shown, a third exponent digit only when needed, zero unsigned.
    subroutine test_real_text()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
                                                 ieee_positive_inf, ieee_negative_inf
        print '(a)', 'real_text:'
        call check(real_text(0.0_real64)    == '0.00000000E+00',   'real_text 0.0')
        call check(real_text(-0.0_real64)   == '0.00000000E+00',   'real_text -0.0 unsigned')
        call check(real_text(1.5_real64)    == '1.50000000E+00',   'real_text 1.5')
        call check(real_text(1e-300_real64) == '1.00000000E-300',  'real_text 1e-300 keeps its E')
        call check(real_text(-1e300_real64) == '-1.00000000E+300', 'real_text -1e300 keeps its E')
        call check(real_text(1e99_real64)   == '1.00000000E+99',   'real_text 1e99 two digits')
        call check(real_text(1e100_real64)  == '1.00000000E+100',  'real_text 1e100 three digits')
        call check(real_text(1e-99_real64)  == '1.00000000E-99',   'real_text 1e-99 two digits')
        call check(real_text(1e-100_real64) == '1.00000000E-100',  'real_text 1e-100 three digits')
        call check(real_text(-2.5e7_real64) == '-2.50000000E+07',  'real_text -2.5e7')
        call check(real_text(ieee_value(1.0_real64, ieee_quiet_nan))    == 'NaN',       'real_text NaN')
        call check(real_text(ieee_value(1.0_real64, ieee_positive_inf)) == 'Infinity',  'real_text +Inf')
        call check(real_text(ieee_value(1.0_real64, ieee_negative_inf)) == '-Infinity', 'real_text -Inf')
    end subroutine

    subroutine test_accessors()
        character(len=*), parameter :: ADIR = 'utest_sqr_acc_db'
        type(db_t), target :: a
        type(column_t) :: cols(2)
        type(column_t), allocatable :: got(:)
        character(len=:), allocatable :: buf
        integer :: rs
        integer(int32) :: rid
        integer :: ios
        ios = c_rmtree(ADIR)
        cols(1)%name = 'id';   cols(1)%dtype = DT_INT;  cols(1)%csize = 4
        cols(2)%name = 'name'; cols(2)%dtype = DT_CHAR; cols(2)%csize = 8

        call db_open(a, ADIR, stat=rs)
        call db_create_table(a, 't', cols, rs)
        call check(rs == SQR_OK, 'acc: open + table')

        call db_describe(a, 't', got, rs)
        call check(rs == SQR_OK .and. size(got) == 2, 'acc: describe returns 2 columns')
        call check(trim(got(1)%name) == 'id' .and. got(1)%dtype == DT_INT, &
                   'acc: describe column 1 metadata')
        call check(trim(got(2)%name) == 'name' .and. got(2)%csize == 8, &
                   'acc: describe column 2 metadata')
        call db_describe(a, 'zzz', got, rs)
        call check(rs == SQR_NOT_FOUND .and. size(got) == 0, 'acc: describe missing table')

        call check(db_row_count(a, 't') == 0, 'acc: row_count empty table')
        call db_describe(a, 't', got)               ! re-fetch: got was left zero-length above
        call row_alloc(buf, db_record_size(a, 't'))
        call row_set_int(buf, got(1), 7_int32)      ! described columns carry the layout
        call row_set_char(buf, got(2), 'seven')
        call db_insert(a, 't', buf, rid, rs)
        call check(db_row_count(a, 't') == 1, 'acc: row_count after insert')
        call db_delete(a, 't', rid, rs)
        call check(db_row_count(a, 't') == 0, 'acc: row_count after delete')
        call check(db_row_count(a, 'zzz') == 0, 'acc: row_count missing table')

        call check(.not. db_in_txn(a), 'acc: in_txn false outside txn')
        call db_begin(a, rs)
        call check(db_in_txn(a), 'acc: in_txn true after begin')
        call db_commit(a, rs)
        call check(.not. a%in_txn(), 'acc: in_txn false after commit (OO spelling)')

        call db_close(a)
        ios = c_rmtree(ADIR)
    end subroutine

    ! Sticky error state (db_last_error) and the uniform errmsg argument.
    subroutine test_last_error()
        character(len=*), parameter :: EDIR = 'utest_sqr_lerr_db'
        type(db_t), target :: a
        type(column_t) :: cols(1)
        type(column_t), allocatable :: laid(:)
        character(len=:), allocatable :: buf, msg
        character(len=96) :: em
        integer :: rs, code
        integer(int32) :: rid
        integer :: ios
        ios = c_rmtree(EDIR)
        cols(1)%name = 'id'; cols(1)%dtype = DT_INT; cols(1)%csize = 4

        call db_open(a, EDIR, stat=rs)
        call db_create_table(a, 't', cols, rs)
        call db_create_index(a, 't', 'id', rs, unique=.true.)
        call check(rs == SQR_OK, 'lerr: open + table + unique index')
        call db_describe(a, 't', laid)              ! layout-carrying columns

        ! A failure records code + detail; the errmsg argument gets the same.
        em = ''
        call row_alloc(buf, db_record_size(a, 't'))
        call row_set_int(buf, laid(1), 1_int32)
        call db_insert(a, 'zzz', buf, rid, rs, em)
        call check(rs == SQR_NOT_FOUND, 'lerr: insert into missing table fails')
        call check(index(em, 'zzz') > 0, 'lerr: errmsg carries the table name')
        call a%last_error(code, msg)
        call check(code == SQR_NOT_FOUND, 'lerr: last_error code')
        call check(index(msg, 'no such table: zzz') > 0, 'lerr: last_error detail')

        ! Success clears the sticky state.
        call db_insert(a, 't', buf, rid, rs)
        call db_last_error(a, code, msg)
        call check(rs == SQR_OK .and. code == SQR_OK .and. len(msg) == 0, &
                   'lerr: success clears sticky state')

        ! A duplicate key names the violated index.
        call db_insert(a, 't', buf, rid, rs)
        call db_last_error(a, code, msg)
        call check(rs == SQR_DUP .and. code == SQR_DUP, 'lerr: duplicate key code')
        call check(index(msg, 'unique-key') > 0 .and. index(msg, 'id') > 0, &
                   'lerr: duplicate-key detail names the index')

        ! With no site-recorded detail the canonical text is the fallback.
        call db_undo(a, rs)
        call db_last_error(a, code, msg)
        call check(rs == SQR_NO_UNDO .and. msg == sqr_errstr(SQR_NO_UNDO), &
                   'lerr: canonical fallback text')

        ! Guard failures record too: a read-only handle refuses a write.
        call db_set_readonly(a, rs)
        em = ''
        call db_insert(a, 't', buf, rid, rs, em)
        call db_last_error(a, code)
        call check(rs == SQR_READONLY .and. code == SQR_READONLY .and. &
                   em == sqr_errstr(SQR_READONLY), 'lerr: readonly guard records')

        call db_close(a)
        ios = c_rmtree(EDIR)
    end subroutine

    ! durable=.false. (v1.4.0): the journal fsyncs are skipped - writes and
    ! flushes are kept, so txns, rollback and reopen behave identically; only
    ! crash-durability is traded away (for scratch stores a host can rebuild).
    subroutine test_durable_off()
        type(db_t), target :: a
        character(len=*), parameter :: DDIR = 'utest_sqr_dur'
        character(len=:), allocatable :: buf
        integer :: rs, i, got, ios
        integer(int32) :: rid
        ios = c_rmtree(DDIR)
        call db_open(a, DDIR, rs, durable=.false.)
        call check(rs == SQR_OK .and. .not. a%durable, 'durable off: opens, flag held')
        call db_create_table(a, 't', [column_t('v', DT_INT, 4)], rs)
        call check(rs == SQR_OK, 'durable off: create table')
        call row_alloc(buf, a%tables(1)%record_size)
        call db_begin(a, rs)
        do i = 1, 3
            call row_set_int(buf, a%tables(1)%cols(1), int(10 * i, int32))
            call db_insert(a, 't', buf, rid, rs)
        end do
        call db_commit(a, rs)
        call check(rs == SQR_OK, 'durable off: a txn commits')
        call db_begin(a, rs)
        call row_set_int(buf, a%tables(1)%cols(1), 99_int32)
        call db_insert(a, 't', buf, rid, rs)
        call db_rollback(a, rs)
        call check(rs == SQR_OK, 'durable off: a rollback still works')
        call db_get(a, 't', 2_int32, buf, rs)
        got = row_get_int(buf, a%tables(1)%cols(1))
        call check(rs == SQR_OK .and. got == 20, 'durable off: committed rows read back')
        call db_get(a, 't', 4_int32, buf, rs)
        call check(rs /= SQR_OK, 'durable off: the rolled-back row is gone')
        call db_close(a, rs)
        call db_open(a, DDIR, rs)                  ! reopen with the default
        call check(rs == SQR_OK .and. a%durable, 'durable off: reopen defaults durable')
        call db_get(a, 't', 3_int32, buf, rs)
        got = row_get_int(buf, a%tables(1)%cols(1))
        call check(rs == SQR_OK .and. got == 30, 'durable off: data survives reopen')
        call db_close(a)
        ios = c_rmtree(DDIR)
    end subroutine

end program utest_sqr

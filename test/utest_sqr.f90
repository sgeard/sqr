program utest_sqr
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use sqr
    use clib_wrap, only: c_rename, c_remove, c_mkdir, c_rmtree, c_path_exists, &
                         c_fsync_path, c_fsync_dir, c_truncate, c_lock_release
    implicit none

    type :: scan_ctx_t
        integer :: count = 0
        integer :: sum   = 0
        type(column_t) :: age_col
    end type

    type :: scan_text_ctx_t
        integer :: count = 0
        character(len=:), allocatable :: joined
    end type

    integer :: pass = 0, fail = 0
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
    call test_open_corruption_paths()
    call test_catalog_traversal_name()
    call test_crash_recovery()
    call test_version_check()
    call test_schema_corruption()
    call test_text()
    call test_high_findings()
    call test_large_blob()
    call test_compact()
    call test_compact_recovery()
    call test_deep_tree()
    call test_torn_journal()
    call test_pack()
    call test_natural_keys()
    call test_null_columns()
    call test_leading_column()
    call test_drop_index()
    call test_db_verify()
    call test_verify_dup_split_by_dead()
    call test_list_tables_closed()
    call test_create_table_cleanup()
    call test_insert_many()
    call test_alter_column()
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
    call test_close_rolls_back_open_txn()
    call test_txn_structural_block()
    call test_locking()
    call test_set_readonly()

    call cleanup_dir()

    print '(a,i0,a,i0,a)', 'sqr tests: ', pass, ' passed, ', fail, ' failed'
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
    end subroutine

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
        call check(c_fsync_path('no_such_file.xyz') /= 0, 'c_fsync_path on missing file fails')
        call check(c_truncate('no_such_file.xyz', 8_int64) /= 0, &
                                                          'c_truncate on missing file fails')
        ios = c_remove(f)
    end subroutine

    ! Drive the rollback journal directly (no engine wiring yet): a committed
    ! transaction leaves its new bytes; an interrupted one is rolled back by
    ! recovery; an append is truncated away; duplicate regions log once.
    subroutine test_journal()
        type(db_t) :: db
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
        type(db_t) :: db, db2
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
        ! by this same-process stand-in for the crashed writer.
        call c_lock_release(db%lock_tok)

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

    function jfile_size(path) result(n)
        character(len=*), intent(in) :: path
        integer(int64) :: n
        integer :: ios
        inquire(file=path, size=n, iostat=ios)
        if (ios /= 0) n = -1_int64
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
        type(db_t) :: db
        integer :: rs
        character(len=128) :: emsg
        call db_open(db, TEST_DIR, rs, emsg)
        call check(rs == SQR_OK, 'open empty db')
        call check(db%ntables == 0, 'empty db has 0 tables')
        call db_close(db)
        call check(.not. db%opened, 'db marked closed')
    end subroutine

    subroutine test_create_table_and_insert()
        type(db_t)     :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t)        :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db_rw, db_ro
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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

        ! Force-close the data + index units WITHOUT db_close: the schema keeps
        ! the stale next_id=1 written back at db_create_table.
        if (db%tables(1)%unit /= -1) close(db%tables(1)%unit)
        close_idx: do j = 1, db%tables(1)%nindices
            if (db%tables(1)%indices(j)%bt%unit /= -1) &
                close(db%tables(1)%indices(j)%bt%unit)
        end do close_idx
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
        type(db_t) :: db
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
        type(db_t)      :: db
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
        type(db_t) :: db
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

    ! Create a zero-content file (used to fabricate the compact marker).
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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

        ! Case 1: hot header + garbage payload with a mismatched checksum. The
        ! payload is either checksum-rejected or fails to deserialise; either
        ! way it must be voided, never replayed, and the committed row survives.
        call write_hot_journal(jpath, 1, 123456, 'XXXXXXXX')
        call db_open(db, JDIR, rs, emsg)
        call check(rs == SQR_OK, 'torn: open recovers past corrupt-payload journal')
        call check(db%tables(1)%live_count == 1, 'torn: committed row survives')
        call db_get(db, 't', 1_int32, buf, rs)
        call check(rs == SQR_OK .and. row_get_int(buf, db%tables(1)%cols(1)) == 7, &
                   'torn: committed row value intact')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK, 'torn: db_verify clean after corrupt-payload recovery')
        call db_close(db)

        ! Case 2: hot header claiming an absurd payload length — must be rejected
        ! by the length guard, not trusted into a huge allocate.
        call write_hot_journal_plen(jpath, huge(0_int64))
        call db_open(db, JDIR, rs, emsg)
        call check(rs == SQR_OK, 'torn: open voids absurd-length journal')
        call db_close(db)

        ! Case 3: truncated file (magic only) — treated as foreign/absent.
        open(newunit=u, file=jpath, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        write(u) 'SQRJ'
        close(u)
        call db_open(db, JDIR, rs, emsg)
        call check(rs == SQR_OK, 'torn: open ignores truncated journal header')
        call db_get(db, 't', 1_int32, buf, rs)
        call check(rs == SQR_OK, 'torn: row still readable after truncated-journal open')
        call db_close(db)

        ios = c_rmtree(JDIR)
    end subroutine

    ! Step 7: pack a database directory into a single .sqr container and unpack
    ! it back. Round-trip (open + verify + query match), plus the guards:
    ! overwrite refusal, truncated-container rejection, hot-journal refusal, and
    ! path-traversal rejection of a crafted archive name.
    subroutine test_pack()
        character(len=*), parameter :: PDIR  = 'utest_sqr_pack_db'
        character(len=*), parameter :: PDIR2 = 'utest_sqr_pack_db2'
        character(len=*), parameter :: EVIL  = 'utest_sqr_pack_evil'
        character(len=*), parameter :: PFILE = 'utest_sqr_pack.sqr'
        type(db_t) :: db
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
        call db_unpack(PFILE, PDIR2, rs)
        call check(rs == SQR_OK, 'pack: db_unpack succeeds')

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

        ios = c_rmtree(PDIR); ios = c_rmtree(PDIR2); ios = c_rmtree(EVIL)
        ios = c_remove(PFILE); ios = c_remove('utest_sqr_pack_evil.sqr')
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

    subroutine test_natural_keys()
        character(len=*), parameter :: NK = 'utest_sqr_nk_db'
        character(len=6), parameter :: KEYCOLS(2) = [character(len=6) :: 'sku', 'region']
        type(db_t) :: db
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

        call db_close(db)
        ios = c_rmtree(NK)
    end subroutine

    ! Exercises validation / diagnostic paths the feature tests skip:
    ! empty column set, oversized record, invalid column name, and the
    ! raise() no-stat branch (library-must-not-crash contract).
    subroutine test_coverage_paths()
        character(len=*), parameter :: CV = 'utest_sqr_cov_db'
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        call check(n == 2, 'null: open_cursor excludes NULL rows')

        ! update: NULL row -> valued enters the index; valued row -> NULL leaves it
        call upd_null(db, ti, 2_int32, 15_int32, .false., 'b', buf)
        call db_find_by_int(db, 't', 'val', 15_int32, rid, rs)
        call check(rs == SQR_OK .and. rid == 2, 'null: update NULL->value enters index')
        call upd_null(db, ti, 1_int32, 0_int32, .true., 'a', buf)
        call db_find_by_int(db, 't', 'val', 10_int32, rid, rs)
        call check(rs == SQR_NOT_FOUND, 'null: update value->NULL leaves index')

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

    ! ===== 5.2 leading-column range/cursor on a composite index =====

    subroutine test_leading_column()
        character(len=*), parameter :: LDIR = 'utest_sqr_lead_db'
        type(db_t) :: db
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
        type(db_t) :: db
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

    subroutine test_alter_column()
        character(len=*), parameter :: ADIR = 'utest_sqr_alter_db'
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
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
        type(db_t) :: db
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        call db_list_tables(db, names)
        call check(size(names) == 0, 'list_tables: closed handle -> empty list')
    end subroutine

    ! Review §3: a mid-failure in db_create_table must leave no trace —
    ! no leaked unit, no orphan .dat/.schema files, no phantom table.
    subroutine test_create_table_cleanup()
        character(len=*), parameter :: CDIR = 'utest_sqr_ctfail_db'
        type(db_t) :: db
        type(column_t) :: c(2), ct(2)
        integer :: rs, ios, u
        character(len=128) :: emsg

        ios = c_rmtree(CDIR)
        c(1)%name = 'id' ; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'nm' ; c(2)%dtype = DT_CHAR; c(2)%csize = 8
        ct(1) = c(1)
        ct(2)%name = 'note'; ct(2)%dtype = DT_TEXT; ct(2)%csize = SQR_TEXT_DESC
        call db_open(db, CDIR, rs, emsg)

        ! (a) open_data fails: a blocking file where the fresh '.dat' must go.
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

        ! (b) open_blob fails: a directory squatting on the '.blob' path.
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

    ! ===== 5.3 db_insert_many =====

    subroutine test_insert_many()
        character(len=*), parameter :: BDIR = 'utest_sqr_batch_db'
        type(db_t) :: db
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
        type(db_t) :: a, b, c
        type(column_t) :: cols(1)
        integer :: rs, ios
        ios = c_rmtree(LDIR)
        cols(1)%name = 'id'; cols(1)%dtype = DT_INT; cols(1)%csize = 4

        ! Create the database so a read-only open has something to attach to.
        call db_open(a, LDIR, stat=rs)
        call check(rs == SQR_OK, 'lock: initial rw open succeeds')
        call db_create_table(a, 't', cols, rs)
        call check(rs == SQR_OK, 'lock: create table')

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
        type(db_t) :: a, b
        type(column_t) :: cols(1)
        character(len=:), allocatable :: buf
        integer :: rs, ios
        integer(int32) :: rid
        ios = c_rmtree(SDIR)
        cols(1)%name = 'id'; cols(1)%dtype = DT_INT; cols(1)%csize = 4

        call db_open(a, SDIR, stat=rs)
        call db_create_table(a, 't', cols, rs)
        call check(rs == SQR_OK, 'setro: rw open + table')

        ! Before the downgrade, a concurrent reader is blocked by the writer.
        call db_open(b, SDIR, stat=rs, readonly=.true.)
        call check(rs == SQR_LOCKED, 'setro: reader blocked before downgrade')

        ! Demote the writer; it must now refuse writes.
        call db_set_readonly(a, rs)
        call check(rs == SQR_OK, 'setro: set_readonly succeeds')
        call check(a%readonly, 'setro: handle now read-only')
        call row_alloc(buf, a%tables(1)%record_size)
        call row_set_int(buf, a%tables(1)%cols(1), 1_int32)
        call db_insert(a, 't', buf, rid, rs)
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

end program utest_sqr

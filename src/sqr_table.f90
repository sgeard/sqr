! sqr_table — table and database lifecycle for the sqr module.
!
! Descendant of `sqr_base`: the catalog/schema codecs, file-open helpers,
! validation, layout and B+-tree rebuild it relies on all come from the parent
! submodule by host association.  Holds the public lifecycle API — opening and
! closing a database, creating/dropping/compacting tables, and the table
! lookup helpers.

submodule (sqr:sqr_base) sqr_table
    use :: clib_wrap, only: c_remove, c_lock_try, c_lock_share, c_utf8_paths   ! c_rename/c_fsync_dir/c_file_size host-associated from sqr_base
    implicit none
contains

    module subroutine db_open(db, dir, stat, errmsg, readonly, durable)
        class(db_t),       intent(inout)           :: db
        character(len=*), intent(in)              :: dir
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        logical,          intent(in),   optional  :: readonly
        logical,          intent(in),   optional  :: durable
        integer :: rs, i, j, n, catver
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        character(len=:), allocatable :: ndir
        character(len=SQR_ERRMSG_LEN) :: em
        ! Said when a read-only handle meets a database whose index keys are
        ! still in the sqr 1.x layout (schema version 1): it cannot rebuild
        ! them, and this build carries no reader for that layout.
        character(len=*), parameter :: NEEDS_UPGRADE = &
            'indices are in the sqr 1.x format; open the database read-write' // &
            ' once to upgrade it, or read it with sqr 1.8.x: "'

        ! An open handle is refused and left as it is: wiping it would leak its
        ! units and its lock, and the lock would then refuse the new open.
        if (db%opened) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'handle is already open; close it first')
            return
        end if
        call wipe_handle(db)       ! the default state, as intent(out) gave it
        rs = SQR_OK
        ! Fold any '\' to '/' so the engine reasons about a single separator on
        ! every platform (Windows accepts both); see norm_seps.
        ndir = norm_seps(dir)
        open_seq: block
            if (.not. valid_dir_name(ndir)) then
                rs = SQR_INVALID
                call raise_db(db, rs, stat, errmsg, &
                           'invalid database directory name: "' // trim(dir) // '"')
                exit open_seq
            end if

            db%dir = trim(ndir)
            db%ntables  = 0
            allocate(db%tables(0))
            db%opened   = .false.
            db%readonly = .false.
            if (present(readonly)) db%readonly = readonly
            db%durable  = .true.
            if (present(durable)) db%durable = durable

            ! Read-only opens require an initialised database (catalog file
            ! must exist); read-write opens create the directory if needed.
            ! We probe the catalog file rather than the directory itself
            ! because inquire on directories is unreliable across compilers
            ! (ifx returns .false.).
            if (db%readonly) then
                if (.not. file_exists(catalog_path(db))) then
                    rs = SQR_NOT_FOUND
                    call raise_db(db, rs, stat, errmsg, &
                               'database not found: "' // trim(db%dir) // '"')
                    exit open_seq
                end if
            else
                ! Create the directory and any missing parents (a database may
                ! now be nested or absolute). An already-present directory is
                ! fine (idempotent); a genuine failure to create is fatal.
                if (.not. mkdir_p(db%dir)) then
                    rs = SQR_ERR
                    call raise_db(db, rs, stat, errmsg, &
                               'cannot create database directory: "' // trim(db%dir) // '"')
                    exit open_seq
                end if
            end if

            ! Concurrency: take an advisory lock on a sentinel file in the
            ! database directory before touching any content.  A read-write
            ! open needs an exclusive lock (sole writer); a read-only open
            ! takes a shared lock so several readers may coexist but no writer
            ! can.  This must precede recovery, which writes to disk.  The
            ! lock is released by db_close, or by the OS if the process dies.
            lock_db: block
                integer :: lerr
                call c_lock_try(lock_path(db), .not. db%readonly, db%lock_tok, lerr)
                if (lerr == 1) then
                    rs = SQR_LOCKED
                    call raise_db(db, rs, stat, errmsg, &
                               'database is locked by another connection: "' &
                               // trim(db%dir) // '"')
                    exit open_seq
                else if (lerr /= 0) then
                    rs = SQR_ERR
                    call raise_db(db, rs, stat, errmsg, &
                               'cannot create lock file in: "' // trim(db%dir) // '"')
                    exit open_seq
                end if
            end block lock_db

            ! Crash recovery: a hot journal means a previous run died
            ! mid-transaction.  A read-write open rolls it back to the
            ! pre-transaction state before reading any table; a read-only open
            ! cannot write the recovery, so it refuses rather than serve a torn
            ! database.  Absent/voided journal -> nothing to do.
            if (db%readonly) then
                if (jrnl_hot(db)) then
                    rs = SQR_READONLY
                    call raise_db(db, rs, stat, errmsg, &
                               'database needs recovery; reopen read-write: "' &
                               // trim(db%dir) // '"')
                    exit open_seq
                end if
            else
                call jrnl_recover(db, rs)
                if (rs /= SQR_OK) then
                    ! A refused (damaged) journal says why and what to do;
                    ! copied out first, since raise_db writes the same buffer.
                    why_block: block
                        character(len=:), allocatable :: why
                        why = 'journal recovery failed'
                        if (db%last_errlen > 0) &
                            why = 'journal recovery failed: ' // db%last_errmsg(1:db%last_errlen)
                        call raise_db(db, rs, stat, errmsg, why)
                    end block why_block
                    exit open_seq
                end if
            end if

            ! An interrupted db_drop_table (a complete `_dropping` marker) is
            ! finished below, once the tables are loaded; a read-only handle
            ! cannot, so it refuses, as for the other markers.  An incomplete
            ! marker means no drop began: a read-only handle ignores it.
            if (db%readonly) then
                ro_marker: block
                    character(len=:), allocatable :: mname
                    logical :: exists, complete
                    call read_drop_marker(db, exists, complete, mname, rs)
                    if (rs /= SQR_OK) then
                        call raise_db(db, rs, stat, errmsg, 'cannot read the drop marker')
                        exit open_seq
                    end if
                    if (complete) then
                        rs = SQR_READONLY
                        call raise_db(db, rs, stat, errmsg, &
                            'database needs drop-table recovery; reopen read-write: "' &
                            // trim(db%dir) // '"')
                        exit open_seq
                    end if
                end block ro_marker
            end if

            call read_catalog(db, names, n, rs, catver)
            if (rs /= SQR_OK) then
                call raise_db(db, rs, stat, errmsg, 'cannot read catalog')
                exit open_seq
            end if
            ! Upgrade from schema version 1 (index keys without NULL flags,
            ! rows with a NULL member left out).  Indices are derived data, so
            ! the upgrade is a rebuild, table by table, below: rebuild the
            ! table's indices in the current layout, then write its schema at
            ! the current version; the catalog follows once every table is
            ! done.  Each step is durable before the next, and a table still at
            ! version 1 is simply rebuilt again, so a crash anywhere leaves a
            ! database the next read-write open completes.  A read-only handle
            ! cannot do any of it and is refused.
            if (catver == SCHEMA_VERSION_UNFLAGGED .and. db%readonly) then
                rs = SQR_VERSION
                call raise_db(db, rs, stat, errmsg, NEEDS_UPGRADE // trim(db%dir) // '"')
                exit open_seq
            end if

            if (n > 0) then
                deallocate(db%tables)
                allocate(db%tables(n))
                tables_open: do i = 1, n
                    associate (t => db%tables(i))
                        ! read_schema writes its own detailed message (bad
                        ! magic / version mismatch / ...); thread it through a
                        ! local so it reaches the sticky state even when the
                        ! caller passed no errmsg.
                        ! An interrupted column-layout change (see
                        ! apply_layout_change) is rolled forward BEFORE the
                        ! schema is read: its staged schema is the truth.
                        altering: block
                            if (file_exists(alter_marker_path(db, trim(names(i))))) then
                                if (db%readonly) then
                                    rs = SQR_READONLY
                                    call raise_db(db, rs, stat, errmsg, &
                                        'database needs layout-change recovery; reopen read-write: "' &
                                        // trim(db%dir) // '"')
                                    exit open_seq
                                end if
                                call complete_alter_swap(db, trim(names(i)), rs)
                                if (rs /= SQR_OK) then
                                    call raise_db(db, rs, stat, errmsg, &
                                        'cannot finish interrupted layout change for ' // trim(names(i)))
                                    exit open_seq
                                end if
                            end if
                        end block altering
                        em = ''
                        call read_schema(db, trim(names(i)), t, rs, em)
                        if (rs /= SQR_OK) then
                            call raise_db(db, rs, stat, errmsg, trim(em))
                            exit open_seq
                        end if

                        ! An interrupted db_compact leaves a marker (see
                        ! compact_marker_path). A read-only handle cannot write
                        ! the completion, so it refuses — as with a hot journal.
                        recovering: block
                            logical :: interrupted, upgrade
                            upgrade = t%schema_version == SCHEMA_VERSION_UNFLAGGED
                            if (upgrade .and. db%readonly) then
                                rs = SQR_VERSION
                                call raise_db(db, rs, stat, errmsg, &
                                    NEEDS_UPGRADE // trim(db%dir) // '"')
                                exit open_seq
                            end if
                            interrupted = file_exists(compact_marker_path(db, trim(names(i))))
                            if (interrupted .and. db%readonly) then
                                rs = SQR_READONLY
                                call raise_db(db, rs, stat, errmsg, &
                                    'database needs compaction recovery; reopen read-write: "' &
                                    // trim(db%dir) // '"')
                                exit open_seq
                            end if
                            if (interrupted) then
                                call complete_compact_swap(db, t, rs)
                                if (rs /= SQR_OK) then
                                    call raise_db(db, rs, stat, errmsg, &
                                        'cannot finish interrupted compact for ' // trim(names(i)))
                                    exit open_seq
                                end if
                                ! The renumbered, tombstone-free file is now the
                                ! truth for next_id; the pre-compact schema is
                                ! stale HIGH, which open_data rejects as
                                ! corruption (a schema cannot legitimately claim
                                ! ids past the end of its data file). Take the
                                ! count from the swapped-in file first, so the
                                ! handle open_data validates already describes
                                ! it. live_count still needs the open unit and
                                ! is recounted below.
                                stale_id: block
                                    integer(int64) :: fsize
                                    fsize = c_file_size(data_path(db, t%name))
                                    t%next_id = int(fsize / t%record_size, int32) + 1_int32
                                end block stale_id
                            end if

                            call open_data(db, t, 'old', rs)
                            if (rs /= SQR_OK) then
                                call raise_db(db, rs, stat, errmsg, &
                                           'cannot open data file for ' // trim(names(i)))
                                exit open_seq
                            end if

                            ! The rebuilds below size their buffers from
                            ! live_count, so it has to be right first.
                            if (interrupted .or. upgrade) then
                                recount: block
                                    integer :: ios
                                    call recount_live(t%unit, t, ios)
                                    if (ios /= 0) then
                                        rs = SQR_ERR
                                        call raise_db(db, rs, stat, errmsg, &
                                            'cannot recount rows for ' // trim(names(i)))
                                        exit open_seq
                                    end if
                                end block recount
                            end if

                            if (table_has_text(t)) then
                                call open_blob(db, t, 'old', rs)
                                if (rs /= SQR_OK) then
                                    call raise_db(db, rs, stat, errmsg, &
                                               'cannot open blob file for ' // trim(names(i)))
                                    exit open_seq
                                end if
                            end if

                            if (interrupted .or. upgrade) then
                                ! After an interrupted compact the indices map
                                ! keys to pre-compact row ids; on an upgrade
                                ! they hold version-1 keys.  Either way rebuild
                                ! them (bt_open truncate-creates, whatever the
                                ! file held), persist the schema - fixed
                                ! counters, current version - then retire the
                                ! compact marker if there was one.
                                rebuild_all: do j = 1, t%nindices
                                    if (.not. idx_live(t%indices(j))) cycle rebuild_all
                                    call rebuild_index(db, i, j, rs)
                                    if (rs /= SQR_OK) then
                                        call raise_db(db, rs, stat, errmsg, &
                                            'cannot rebuild index for ' // trim(names(i)))
                                        exit open_seq
                                    end if
                                end do rebuild_all
                                call write_schema(db, t, rs)
                                if (rs == SQR_OK .and. interrupted) then
                                    retire: block
                                        logical :: gone
                                        character(len=1) :: none(0)
                                        call retract_marker(db, compact_marker_path(db, trim(names(i))), &
                                                            none, gone)
                                        rs = merge(SQR_OK, SQR_ERR, gone)
                                    end block retire
                                end if
                                if (rs /= SQR_OK) then
                                    if (interrupted) then
                                        call raise_db(db, rs, stat, errmsg, &
                                            'cannot finalise compact recovery for ' // trim(names(i)))
                                    else
                                        call raise_db(db, rs, stat, errmsg, &
                                            'cannot finish the index upgrade for ' // trim(names(i)))
                                    end if
                                    exit open_seq
                                end if
                                t%schema_version = SQR_SCHEMA_VERSION
                            else
                                indices_open: do j = 1, t%nindices
                                    if (.not. idx_live(t%indices(j))) cycle indices_open
                                    call open_index(db, t, t%indices(j), j, 'old', rs)
                                    if (rs /= SQR_OK) then
                                        call raise_db(db, rs, stat, errmsg, &
                                                   'cannot open index file for ' // trim(names(i)))
                                        exit open_seq
                                    end if
                                end do indices_open
                            end if
                        end block recovering
                    end associate
                end do tables_open
                db%ntables = n
            end if

            if (.not. db%readonly) then
                finish_marker: block
                    character(len=:), allocatable :: why
                    call finish_drop(db, rs, why)
                    if (rs /= SQR_OK) then
                        call raise_db(db, rs, stat, errmsg, why)
                        exit open_seq
                    end if
                end block finish_marker
            end if

            ! Every table is at the current version now; say so in the
            ! catalog.  Last, so that it is the one thing an interrupted
            ! upgrade can leave behind, and the next open lands here again.
            if (catver == SCHEMA_VERSION_UNFLAGGED) then
                call write_catalog(db, rs)
                if (rs /= SQR_OK) then
                    call raise_db(db, rs, stat, errmsg, 'cannot finish the format upgrade')
                    exit open_seq
                end if
            end if

            db%opened = .true.
        end block open_seq

        if (rs /= SQR_OK) then
            call abandon_open(db)
        else if (present(stat)) then
            stat = SQR_OK
        end if
    end subroutine

    module subroutine db_close(db, stat, errmsg)
        class(db_t), intent(inout)         :: db
        integer,    intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: i, j, rs, first, cs
        call clear_last_err(db)
        first = SQR_OK
        call report(db, SQR_OK, stat, errmsg)
        if (.not. db%opened) return
        ! A still-open explicit transaction must be UNDONE at close, not silently
        ! persisted.  Otherwise write_schema below flushes the in-flight counters,
        ! the guarded base writes stay on disk, and the hot journal is then deleted
        ! — so a close would durably COMMIT a transaction the caller never committed
        ! (e.g. an error path that reached close without db_commit/db_rollback).
        ! Roll it back first so a close means abort, not commit.  Read-only handles
        ! never hold an active transaction.
        if (db%jrnl%active .and. .not. db%readonly) then
            call txn_rollback(db, rs)
            if (rs /= SQR_OK .and. first == SQR_OK) first = rs
        end if
        ! A drop left half done in this session (its `_dropping` marker is
        ! still there) is finished before the catalog is settled.
        if (.not. db%readonly) then
            finish_marker: block
                character(len=:), allocatable :: why
                call finish_drop(db, rs, why)
                if (rs /= SQR_OK .and. first == SQR_OK) then
                    first = rs
                    call set_last_err(db, rs, why)
                end if
            end block finish_marker
        end if
        ! Schema counters (next_id/live_count) and a catalog whose last write
        ! failed reach the disk here, so capture the first write failure for
        ! the caller — but keep closing everything so units are not leaked.
        if (.not. db%readonly) then
            call settle_meta(db, rs)
            if (rs /= SQR_OK .and. first == SQR_OK) first = rs
        end if
        close_tables: do i = 1, db%ntables
            associate (t => db%tables(i))
                ! A close performs the final flush; capture (never let it abort)
                ! an ENOSPC/EIO there so it surfaces as a stat, not error stop.
                if (t%unit /= -1) then
                    close(t%unit, iostat=cs)
                    if (cs /= 0 .and. first == SQR_OK) first = SQR_ERR
                end if
                t%unit = -1
                if (t%blob_unit /= -1) then
                    close(t%blob_unit, iostat=cs)
                    if (cs /= 0 .and. first == SQR_OK) first = SQR_ERR
                end if
                t%blob_unit = -1
                close_indices: do j = 1, t%nindices
                    if (idx_live(t%indices(j))) then
                        ! bt_close rewrites the tree meta on a writable unit —
                        ! capture its failure like every other flush here.
                        call bt_close(t%indices(j)%bt, cs)
                        if (cs /= BT_OK .and. first == SQR_OK) first = SQR_ERR
                    end if
                    ! Free any journal-hook context left by an unclosed txn so
                    ! the heap target does not leak when the slot is deallocated.
                    if (associated(t%indices(j)%jctx)) deallocate(t%indices(j)%jctx)
                end do close_indices
            end associate
        end do close_tables
        if (.not. db%readonly .and. .not. db%jrnl%needs_recovery) then
            ! Any in-flight transaction was rolled back above, so a journal on
            ! disk is now a voided leftover: delete it so the next open does zero
            ! recovery work.  (Read-only opens never write, so they leave it.)
            ! Not when a rollback FAILED, in this close or earlier in the
            ! session: the journal is then still hot and is what the next open
            ! needs to finish the repair.
            call remove_idle_journal(db, rs)
            if (rs /= SQR_OK .and. first == SQR_OK) first = rs
        end if
        call jrnl_close_unit(db)      ! a read-only handle never opened one; harmless
        db%jrnl%needs_recovery = .false.   ! the latch belongs to the session just closed
        call db_reset_history(db)     ! a closed handle has nothing to undo or redo
        if (allocated(db%tables)) deallocate(db%tables)
        if (allocated(db%dir))    deallocate(db%dir)
        ! Release the advisory lock (closing its descriptor/handle).
        call c_lock_release(db%lock_tok)
        db%ntables  = 0
        db%opened   = .false.
        db%readonly = .false.
        db%quiesced = .false.
        call report(db, first, stat, errmsg)
    end subroutine

    module subroutine db_quiesce(db, stat, errmsg)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: i, j, first, rs, cs
        call clear_last_err(db)
        if (.not. db%opened) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'database is not open')
            return
        end if
        if (db%quiesced) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'database is already quiesced')
            return
        end if
        ! Same rule as db_set_readonly: uncommitted state must not be stranded
        ! behind closed units, and a hot journal is not a snapshot.
        if (db%jrnl%active) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                'cannot quiesce while a transaction is open')
            return
        end if
        first = SQR_OK
        ! Flush-and-close exactly what db_close does, minus the teardown: the
        ! tables array, the directory and the advisory lock all stay, so
        ! db_resume can reopen from the same in-memory schemas.
        if (.not. db%readonly) then
            call settle_meta(db, rs)           ! next_id/live_count live only in memory
            if (rs /= SQR_OK) first = rs
        end if
        quiesce_tables: do i = 1, db%ntables
            associate (t => db%tables(i))
                if (t%unit /= -1) then
                    close(t%unit, iostat=cs)
                    if (cs /= 0 .and. first == SQR_OK) first = SQR_ERR
                end if
                t%unit = -1
                if (t%blob_unit /= -1) then
                    close(t%blob_unit, iostat=cs)
                    if (cs /= 0 .and. first == SQR_OK) first = SQR_ERR
                end if
                t%blob_unit = -1
                quiesce_indices: do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle quiesce_indices
                    call bt_close(t%indices(j)%bt, cs)  ! rewrites the tree meta
                    if (cs /= BT_OK .and. first == SQR_OK) first = SQR_ERR
                end do quiesce_indices
            end associate
        end do quiesce_tables
        call jrnl_close_unit(db)     ! reopened lazily by the next arm or void
        ! Quiesced even on a partial failure: some units are already closed, so
        ! db_resume (or db_close) is the only safe continuation either way.
        db%quiesced = .true.
        call report(db, first, stat, errmsg)
    end subroutine

    module subroutine db_resume(db, stat, errmsg)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: i, j, first, rs
        call clear_last_err(db)
        if (.not. db%quiesced) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'database is not quiesced')
            return
        end if
        first = SQR_OK
        ! Reopen from the on-disk state db_quiesce flushed.  Every failure is
        ! recorded but the loop runs to the end: a handle with some units open
        ! and some not is what db_close expects to tidy up.  Only what is still
        ! closed is opened, so a retry after a partial failure does not open a
        ! file twice (each open leaves its unit at -1 when it fails).
        resume_tables: do i = 1, db%ntables
            associate (t => db%tables(i))
                if (t%wedged) cycle resume_tables   ! stays closed until a reopen
                if (t%unit == -1) then
                    call open_data(db, t, 'old', rs)
                    if (rs /= SQR_OK .and. first == SQR_OK) then
                        first = rs
                        call set_last_err(db, rs, 'cannot reopen data file for ' // trim(t%name))
                    end if
                end if
                if (table_has_text(t) .and. t%blob_unit == -1) then
                    call open_blob(db, t, 'old', rs)
                    if (rs /= SQR_OK .and. first == SQR_OK) then
                        first = rs
                        call set_last_err(db, rs, 'cannot reopen blob file for ' // trim(t%name))
                    end if
                end if
                resume_indices: do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle resume_indices
                    if (t%indices(j)%bt%unit /= -1) cycle resume_indices
                    call open_index(db, t, t%indices(j), j, 'old', rs)
                    if (rs /= SQR_OK .and. first == SQR_OK) then
                        first = rs
                        call set_last_err(db, rs, 'cannot reopen index file for ' // trim(t%name))
                    end if
                end do resume_indices
            end associate
        end do resume_tables
        ! Only a clean reopen lifts the flag: a wedged handle stays quiesced so
        ! that db_resume can be called again; it reopens only what is still
        ! closed.
        if (first == SQR_OK) db%quiesced = .false.
        call report(db, first, stat, errmsg)
    end subroutine

    module subroutine db_set_readonly(db, stat, errmsg)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: rs
        call clear_last_err(db)
        call report(db, SQR_OK, stat, errmsg)
        if (.not. db%opened) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'database is not open')
            return
        end if
        if (db%readonly) return            ! already read-only: nothing to do
        ! A live transaction owns uncommitted state; demoting now would strand
        ! it.  The caller must commit or roll back first.
        if (db%jrnl%active) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                'cannot set read-only while a transaction is open')
            return
        end if
        ! After a failed rollback the journal is hot and must survive to the
        ! next open; the demotion below would delete it.
        if (recovery_block(db, stat, errmsg)) return
        ! Bring the disk level with memory while the lock is still exclusive:
        ! a read-only handle never writes again, not even at db_close, so the
        ! row counters (and a catalog whose last write failed) would otherwise
        ! be lost.  The voided journal goes too, as at a writable close.  On a
        ! failure the handle stays writable and exclusive: nothing is lost, and
        ! the caller may retry or close.
        call settle_meta(db, rs)
        if (rs == SQR_OK) call remove_idle_journal(db, rs)
        if (rs /= SQR_OK) then
            call raise_db(db, rs, stat, errmsg, &
                'cannot set read-only: the schema counters could not be saved')
            return
        end if
        db%readonly = .true.               ! mutators now refuse via readonly_block
        ! Drop the exclusive lock to shared so other read-only connections may
        ! attach. The downgrade is not atomic on every platform (Windows drops
        ! then retakes the byte range), so a failed retake leaves the handle
        ! holding NO lock — it would then serve stale reads from its open units
        ! while a writer mutates underneath. Force it shut instead so it cannot,
        ! and report the failure; the caller must reopen.
        if (c_lock_share(db%lock_tok) /= 0) then
            call abandon_open(db)
            call raise_db(db, SQR_ERR, stat, errmsg, &
                'the lock could not be shared; the database has been closed')
        end if
    end subroutine

    module subroutine db_create_table(db, name, cols, stat, errmsg)
        class(db_t),       intent(inout)           :: db
        character(len=*), intent(in)              :: name
        type(column_t),   intent(in)              :: cols(:)
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        type(table_t), allocatable :: new_tables(:)
        type(table_t) :: tbl
        integer :: rs, other
        logical :: in_place
        character(len=SQR_ERRMSG_LEN) :: em
        character(len=:), allocatable :: fault

        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        db%generation = db%generation + 1   ! structural change: invalidate cursors

        if (.not. valid_name(name)) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                       'invalid table name: "' // trim(name) // '"')
            return
        end if
        ! A table's files are named after it, and the engine's own files in the
        ! same directory (_catalog.dat, _journal.dat, _lock, ...) all begin
        ! with '_'.  The prefix is therefore the engine's: a table called
        ! _journal would BE the journal.  Checked on creation only - a table
        ! of an existing database is still opened under whatever name it has.
        fault = new_name_fault(name)
        if (len(fault) > 0) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                       'invalid table name: "' // trim(name) // '" (' // fault // ')')
            return
        end if
        ! The name becomes file names.  Where the operating system would not
        ! take those bytes as UTF-8 (Windows, unless the process runs in the
        ! UTF-8 code page) the files would be created under other names, or
        ! not at all: refuse instead.  (Past new_name_fault, a character
        ! outside NAME_CHARS is a non-ASCII one.)
        if (verify(trim(name), NAME_CHARS) > 0) then
            if (.not. c_utf8_paths()) then
                call raise_db(db, SQR_INVALID, stat, errmsg, &
                           'invalid table name: "' // trim(name) // &
                           '" (non-ASCII characters need the UTF-8 code page on Windows)')
                return
            end if
        end if
        if (reserved_name(name)) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                       'reserved table name: "' // trim(name) // &
                       '" (names beginning with "_" belong to the engine)')
            return
        end if

        ! validate_columns writes its own detailed message; thread it through
        ! a local so the detail reaches the sticky state even when the caller
        ! passed no errmsg.
        em = ''
        call validate_columns(cols, rs, em)
        if (rs /= SQR_OK) then
            call raise_db(db, rs, stat, errmsg, trim(em))
            return
        end if

        if (db_table_index(db, name) > 0) then
            call raise_db(db, SQR_DUP, stat, errmsg, &
                       'table already exists: ' // trim(name))
            return
        end if
        ! Two tables whose names differ only in case would be one file on a
        ! file system that ignores case, and are a trap for readers anywhere.
        other = table_index_nocase(db, name)
        if (other > 0) then
            call raise_db(db, SQR_DUP, stat, errmsg, &
                       'table name "' // trim(name) // '" differs only in case from "' // &
                       trim(db%tables(other)%name) // '"')
            return
        end if
        ! The file system has the last word on whether two names are one
        ! file: Windows and macOS ignore case in every script, not only ASCII,
        ! and macOS also treats the two spellings of an accented letter as
        ! one.  Creating over another table's files would overwrite its
        ! schema, and the failure tail would then delete it.
        if (table_files_exist(db, name)) then
            call raise_db(db, SQR_DUP, stat, errmsg, &
                       'cannot create table "' // trim(name) // '": its files already exist ' // &
                       '(the name may differ only in case from another table''s)')
            return
        end if

        tbl%name = name
        tbl%ncols = size(cols)
        allocate(tbl%cols(tbl%ncols))
        tbl%cols = cols
        call layout_columns(tbl%cols, tbl%record_size)
        tbl%next_id        = 1
        tbl%live_count     = 0
        tbl%schema_version = SQR_SCHEMA_VERSION
        tbl%nindices       = 0
        allocate(tbl%indices(0))

        call write_schema(db, tbl, rs)
        if (rs /= SQR_OK) then
            ! The schema may be in place all the same (its rename done, the
            ! directory sync after it failed): remove it, or the name is taken.
            call discard_new_table(db, tbl)
            call report(db, rs, stat, errmsg)
            return
        end if
        call open_data(db, tbl, 'new', rs)
        if (rs /= SQR_OK) then
            call discard_new_table(db, tbl)   ! remove the schema file
            call report(db, rs, stat, errmsg)
            return
        end if
        if (table_has_text(tbl)) then
            call open_blob(db, tbl, 'replace', rs)
            if (rs /= SQR_OK) then
                call discard_new_table(db, tbl)   ! close+delete .dat, remove schema
                call report(db, rs, stat, errmsg)
                return
            end if
        end if

        allocate(new_tables(db%ntables + 1))
        new_tables(1:db%ntables) = db%tables(1:db%ntables)
        new_tables(db%ntables + 1) = tbl
        call move_alloc(new_tables, db%tables)
        db%ntables = db%ntables + 1
        db%catalog_dirty = .true.

        call write_catalog(db, rs, in_place)
        ! A catalog that reached its place names the table, so the table
        ! stays even if the directory sync after the rename failed: removing
        ! its files then would leave a database that cannot be opened.  The
        ! create has succeeded; catalog_dirty is still set, so the close
        ! writes the catalog again.
        if (rs /= SQR_OK .and. .not. in_place) then
            ! The on-disk catalog does not name the table: take it back out
            ! of memory (the slot beyond ntables is never referenced) and
            ! remove its files so no orphans remain.
            db%ntables = db%ntables - 1
            db%catalog_dirty = .false.      ! memory matches the unchanged file again
            call discard_new_table(db, tbl)
            call report(db, rs, stat, errmsg)
            return
        end if

        call report(db, SQR_OK, stat, errmsg)
    end subroutine

    !! Is any file a table called `name` would own already in the database
    !! directory - under that name as the file system compares names?
    function table_files_exist(db, name) result(yes)
        type(db_t),       intent(in) :: db
        character(len=*), intent(in) :: name
        logical :: yes
        yes = c_path_exists(schema_path(db, name))
        if (.not. yes) yes = c_path_exists(data_path(db, name))
        if (.not. yes) yes = c_path_exists(blob_path(db, name))
    end function

    ! The default state of a handle, as `intent(out)` on db_open's dummy gave
    ! it before db_open began refusing an open handle.
    subroutine wipe_handle(h)
        type(db_t), intent(out) :: h
    end subroutine

    !! Undo a half-created table: close its open units (deleting the files
    !! they are connected to) and remove the schema file.  Used only by the
    !! db_create_table failure tails, where the contract is "no trace left"
    !! rather than leaked units and orphan files.
    subroutine discard_new_table(db, tbl)
        type(db_t),    intent(in)    :: db
        type(table_t), intent(inout) :: tbl
        integer :: ios
        if (tbl%unit /= -1) then
            close(tbl%unit, status='delete', iostat=ios)
            tbl%unit = -1
        end if
        if (tbl%blob_unit /= -1) then
            close(tbl%blob_unit, status='delete', iostat=ios)
            tbl%blob_unit = -1
        end if
        call remove_file(schema_path(db, trim(tbl%name)))
    end subroutine

    module subroutine db_drop_table(db, name, stat, errmsg)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: name
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: rs, idx
        logical :: gone
        character(len=1) :: none(0)
        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        idx = db_table_index(db, name)
        if (idx == 0) then
            call report(db, SQR_NOT_FOUND, stat, errmsg)
            return
        end if
        db%generation = db%generation + 1   ! shifts table slots: invalidate cursors
        call db_reset_history(db)           ! slot shift ⇒ captured undo/redo steps can't replay

        ! 1. The commit point: a durable `_dropping` marker naming the table.
        ! A failed write may still have left the marker on disk: it is
        ! withdrawn, and if that cannot be confirmed the table is wedged, so
        ! nothing is written that a drop finished later would throw away.
        ! From a written marker on, a crash or a failure below is finished by
        ! the next writable open or by db_close (finish_drop): the table's
        ! name is never left blocked by orphaned files.
        call write_drop_marker(db, db%tables(idx)%name, rs)
        if (rs /= SQR_OK) then
            call retract_marker(db, drop_marker_path(db), none, gone)
            if (.not. gone) then
                call wedge_table(db, db%tables(idx))
                rs = SQR_ERR
            end if
            call report(db, rs, stat, errmsg)
            return
        end if
        ! 2. to 4.: out of memory and the catalog, files removed, marker cleared.
        call drop_loaded_table(db, idx, rs)
        call report(db, rs, stat, errmsg)
    end subroutine

    ! Steps 2 to 4 of a drop (db_drop_table, finish_drop) for table `idx`,
    ! which is loaded: close its units (a failed close is ignored: the files
    ! are about to be deleted), take it out of db%tables, write the catalog,
    ! then remove its files and clear the marker.  Catalog first: if the
    ! catalog still named a table whose schema was gone, db_open would fail on
    ! the whole database.  A failure returns with the marker in place.
    subroutine drop_loaded_table(db, idx, stat)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: idx
        integer,    intent(out)   :: stat
        type(table_t), allocatable :: nt(:)
        character(len=SQR_NAME_LEN) :: nm
        integer :: j, ni, ios
        nm = db%tables(idx)%name
        ni = db%tables(idx)%nindices
        if (db%tables(idx)%unit /= -1) close(db%tables(idx)%unit, iostat=ios)
        if (db%tables(idx)%blob_unit /= -1) close(db%tables(idx)%blob_unit, iostat=ios)
        close_indices: do j = 1, ni
            associate (ix => db%tables(idx)%indices(j))
                call bt_discard(ix%bt)
                if (associated(ix%jctx)) deallocate(ix%jctx)
            end associate
        end do close_indices

        ! Shrink db%tables — remove element at idx — then persist the catalog.
        allocate(nt(db%ntables - 1))
        nt(1:idx-1)          = db%tables(1:idx-1)        ! section copies, no
        nt(idx:db%ntables-1) = db%tables(idx+1:db%ntables)  ! constructor temp
        call move_alloc(nt, db%tables)
        db%ntables = db%ntables - 1
        db%catalog_dirty = .true.

        call write_catalog(db, stat)
        if (stat /= SQR_OK) return
        call remove_dropped_files(db, trim(nm), ni, stat)
    end subroutine

    ! Steps 3 and 4 of a drop, once the catalog no longer names `name`: remove
    ! its index files (`ni` slots), blob and data file, the schema LAST (so a
    ! schema still present means the others may be too), then clear the
    ! marker.  Each step tolerates a file already gone, so it can be re-run.
    subroutine remove_dropped_files(db, name, ni, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        integer,          intent(in)  :: ni
        integer,          intent(out) :: stat
        integer :: j, ios
        logical :: gone
        character(len=1) :: none(0)
        ios = 0
        del_indices: do j = 1, ni
            call remove_existing(index_path(db, name, j), ios)
            if (ios /= 0) exit del_indices
        end do del_indices
        if (ios == 0) call remove_existing(blob_path(db, name), ios)
        if (ios == 0) call remove_existing(data_path(db, name), ios)
        if (ios == 0) call remove_existing(schema_path(db, name), ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        call retract_marker(db, drop_marker_path(db), none, gone)
        stat = merge(SQR_OK, SQR_ERR, gone)
    end subroutine

    ! Delete `path` if it exists; ios is the failure, 0 when there was nothing.
    subroutine remove_existing(path, ios)
        character(len=*), intent(in)  :: path
        integer,          intent(out) :: ios
        ios = 0
        if (.not. c_path_exists(path)) return
        ios = c_remove(path)
        call io_check(ios)
    end subroutine

    ! Write the drop marker naming `name` and make it durable: `DROP`, the
    ! name, then checksum(`DROP` // name) as a 4-byte integer (as the journal
    ! stores its own), in one write.  read_drop_marker takes anything else -
    ! an empty or zero-filled file left by power loss - for a write that
    ! never finished.
    subroutine write_drop_marker(db, name, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=drop_marker_path(db), access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        write(u, iostat=ios) 'DROP' // trim(name), checksum('DROP' // trim(name))
        call io_check(ios)
        if (ios /= 0) then
            close(u, iostat=ios)
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)
        call io_check(ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_path(drop_marker_path(db))
        call io_check(ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_dir(db%dir)   ! the new marker must be durable
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! The `_dropping` marker as finish_drop and a read-only db_open see it.
    ! `exists`: the file is there.  `complete`: it holds `DROP`, a name of 1
    ! to SQR_NAME_LEN bytes and the checksum write_drop_marker stores; `nm` is
    ! then that name, still untrusted.  An incomplete marker is a write that
    ! never finished, so no drop began.  stat is SQR_ERR only when the file
    ! is there but cannot be read.
    subroutine read_drop_marker(db, exists, complete, nm, stat)
        type(db_t),                    intent(in)  :: db
        logical,                       intent(out) :: exists, complete
        character(len=:), allocatable, intent(out) :: nm
        integer,                       intent(out) :: stat
        character(len=:), allocatable :: buf
        integer :: u, ios, cs, n
        integer(int64) :: sz
        nm       = ''
        complete = .false.
        stat     = SQR_OK
        exists   = c_path_exists(drop_marker_path(db))
        if (.not. exists) return
        open(newunit=u, file=drop_marker_path(db), access='stream', &
             form='unformatted', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        inquire(unit=u, size=sz)
        if (sz < 9 .or. sz > SQR_NAME_LEN + 8) then
            close(u, iostat=cs)   ! read only: a failed close is ignored
            return
        end if
        n = int(sz)
        allocate(character(len=n) :: buf)
        read(u, iostat=ios) buf
        call io_check(ios)
        close(u, iostat=cs)       ! read only: a failed close is ignored
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (buf(1:4) /= 'DROP') return
        if (transfer(buf(n-3:n), 0) /= checksum(buf(1:n-4))) return
        complete = .true.
        nm = buf(5:n-4)
    end subroutine

    ! Finish a drop that a crash or a failure left half done (the `_dropping`
    ! marker exists).  Called by db_open on a writable handle once the tables
    ! are loaded and the journal recovered, and by db_close on a writable
    ! handle before it settles the catalog.  An incomplete marker (see
    ! read_drop_marker) means no drop began: it is withdrawn and nothing else
    ! is touched.  A complete marker's name is untrusted: it must pass the
    ! checks a catalog name passes, or nothing is touched (SQR_INVALID,
    ! "corrupt drop marker").  A table still loaded is dropped
    ! by the ordinary path; otherwise the catalog is brought level if it is
    ! behind, its files are removed (the index slot count from its schema if
    ! that still reads) and the marker cleared.  Idempotent.
    subroutine finish_drop(db, stat, why)
        type(db_t),       intent(inout) :: db
        integer,          intent(out)   :: stat
        character(len=:), allocatable, intent(out) :: why
        character(len=:), allocatable :: nm
        integer :: idx, ni
        logical :: exists, complete, gone
        character(len=1) :: none(0)
        why  = ''
        call read_drop_marker(db, exists, complete, nm, stat)
        if (.not. exists) return
        if (stat /= SQR_OK) then
            why  = 'cannot read the drop marker'
            return
        end if
        if (.not. complete) then
            call retract_marker(db, drop_marker_path(db), none, gone)
            if (.not. gone) then
                stat = SQR_ERR
                why  = 'cannot finish an interrupted drop table'
            end if
            return
        end if
        if (.not. valid_name(nm)) then
            stat = SQR_INVALID
            why  = 'corrupt drop marker'
            return
        end if
        idx = db_table_index(db, nm)
        if (idx > 0) then
            call drop_loaded_table(db, idx, stat)
        else
            ! Step 2 may have taken the table out of memory without the
            ! catalog write succeeding: the catalog first, as in step 2.
            if (db%catalog_dirty) then
                call write_catalog(db, stat)
                if (stat /= SQR_OK) then
                    why = 'cannot finish an interrupted drop table'
                    return
                end if
            end if
            ni = 0
            schema_count: block
                type(table_t) :: gone
                integer :: rs
                if (.not. c_path_exists(schema_path(db, nm))) exit schema_count
                call read_schema(db, nm, gone, rs)
                if (rs == SQR_OK) ni = gone%nindices
            end block schema_count
            call remove_dropped_files(db, nm, ni, stat)
        end if
        if (stat /= SQR_OK) why = 'cannot finish an interrupted drop table'
    end subroutine

    module subroutine db_compact(db, table_name, stat, errmsg)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: idx, ud, ub, ios, rs, j, ci
        integer(int32) :: rid, new_rid, length
        integer(int64) :: off, newpos
        logical :: has_text, gone
        character(len=:), allocatable :: rbuf, dpath, dtmp, bpath, btmp
        character(len=1) :: none(0)
        call clear_last_err(db)

        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            call report(db, SQR_NOT_FOUND, stat, errmsg)
            return
        end if
        db%generation = db%generation + 1   ! renumbers rows: invalidate cursors
        call db_reset_history(db)           ! row renumber ⇒ captured undo/redo steps can't replay
        ud = -1; ub = -1
        rs = SQR_OK
        associate (t => db%tables(idx))
            has_text = table_has_text(t)
            dpath = data_path(db, t%name)
            dtmp  = dpath // '.compact'
            bpath = blob_path(db, t%name)
            btmp  = bpath // '.compact'

            ! Phase A — build the compacted files alongside the originals.
            ! The original data/blob units stay open and untouched here, so
            ! any failure in this phase leaves the table fully intact; we
            ! just delete the temp files and return the error.
            build: block
                ! A crash on a previous attempt can leave a stale temp file;
                ! drop it before recreating.
                call remove_file(dtmp)
                if (has_text) call remove_file(btmp)

                open(newunit=ud, file=dtmp, access='direct', &
                     form='unformatted', recl=t%record_size, &
                     status='replace', action='readwrite', iostat=ios)
                if (ios /= 0) then
                    rs = SQR_ERR
                    exit build
                end if
                if (has_text) then
                    open(newunit=ub, file=btmp, access='stream', &
                         form='unformatted', status='replace', &
                         action='readwrite', iostat=ios)
                    if (ios /= 0) then
                        rs = SQR_ERR
                        exit build
                    end if
                end if

                allocate(character(len=t%record_size) :: rbuf)
                new_rid = 0
                newpos  = 1_int64
                copy_rows: do rid = 1, t%next_id - 1
                    read(t%unit, rec=rid, iostat=ios) rbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        rs = SQR_ERR
                        exit build
                    end if
                    if (row_status(rbuf) /= ROW_ALIVE) cycle copy_rows
                    new_rid = new_rid + 1
                    text_cols: do ci = 1, t%ncols
                        if (t%cols(ci)%dtype /= DT_TEXT) cycle text_cols
                        if (row_is_null(rbuf, t%cols(ci))) then
                            ! Logically-NULL text carries no blob; drop any stale
                            ! descriptor rather than copying orphaned bytes forward.
                            call row_set_text_desc(rbuf, t%cols(ci), 0_int64, 0_int32)
                            cycle text_cols
                        end if
                        call row_get_text_desc(rbuf, t%cols(ci), off, length)
                        if (length > 0) then
                            move_text: block
                                ! Allocatable (heap) transfer buffer — a large
                                ! blob length must not be an automatic stack
                                ! array. Bound the descriptor against the blob
                                ! file and guard the allocate so a corrupt
                                ! length is diagnosed, not an abort.
                                character(len=:), allocatable :: tb
                                integer(int64) :: bsize
                                inquire(unit=t%blob_unit, size=bsize)
                                if (off < 1 .or. &
                                    off - 1 + int(length, int64) > bsize) then
                                    rs = SQR_INVALID
                                    exit build
                                end if
                                allocate(character(len=length) :: tb, stat=ios)
                                if (ios /= 0) then
                                    rs = SQR_ERR
                                    exit build
                                end if
                                read(t%blob_unit, pos=off, iostat=ios) tb
                                call io_check(ios)
                                if (ios == 0) write(ub, pos=newpos, iostat=ios) tb
                                if (ios /= 0) then
                                    rs = SQR_ERR
                                    exit build
                                end if
                            end block move_text
                            call row_set_text_desc(rbuf, t%cols(ci), newpos, length)
                            newpos = newpos + length
                        else
                            call row_set_text_desc(rbuf, t%cols(ci), 0_int64, 0_int32)
                        end if
                    end do text_cols
                    write(ud, rec=new_rid, iostat=ios) rbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        rs = SQR_ERR
                        exit build
                    end if
                end do copy_rows
            end block build

            if (rs /= SQR_OK) then
                ! Originals untouched — discard the partial temp files (a
                ! failed close of a temp being deleted is ignored).
                if (ud /= -1) close(ud, status='delete', iostat=ios)
                if (ub /= -1) close(ub, status='delete', iostat=ios)
                call report(db, rs, stat, errmsg)
                return
            end if
            ! Guard the temp closes: a failed final flush here means the temp is
            ! not fully written, so abort before the swap rather than error stop.
            close(ud, iostat=ios)
            if (has_text) then
                close(ub, iostat=rs)            ! closed even when ud's close failed
                if (ios == 0) ios = rs
                rs = SQR_OK
            end if
            if (ios /= 0) then
                call report(db, SQR_ERR, stat, errmsg)
                return
            end if

            ! Make the compacted temps durable BEFORE they become authoritative.
            ! A rename is only as safe as the bytes behind it: without this,
            ! power loss just after a "successful" compact could leave the new
            ! data file empty/partial while the original is already gone.
            ! A non-durable handle (durable=.false.) skips every fsync.
            if (db%durable) then
                ios = c_fsync_path(dtmp)
                call io_check(ios)
                if (ios == 0 .and. has_text) then
                    ios = c_fsync_path(btmp)
                    call io_check(ios)
                end if
                if (ios == 0) then
                    ios = c_fsync_dir(db%dir)
                    call io_check(ios)
                end if
            end if
            if (ios /= 0) then
                call report(db, SQR_ERR, stat, errmsg)
                return
            end if

            ! Phase B — swap in the compacted files, then rebuild derived state.
            ! The two renames + reindex + schema rewrite are not one atomic step,
            ! so drop a durable marker first: if a crash interrupts any of them,
            ! the next open sees the marker and finishes the compact (the temps
            ! are already durable, so completion always rolls forward). rename(2)
            ! atomically replaces the destination, so no separate delete needed.
            ! A failed marker write may still have left the marker on disk: it
            ! and the temps are withdrawn (an ordinary failure, the table
            ! untouched), and if that cannot be confirmed the table is wedged,
            ! so no later write is rolled over by the next open.
            call write_compact_marker(db, t%name, rs)
            if (rs /= SQR_OK) then
                if (has_text) then
                    call retract_marker(db, compact_marker_path(db, t%name), &
                        [character(len=max(len(dtmp), len(btmp))) :: dtmp, btmp], gone)
                else
                    call retract_marker(db, compact_marker_path(db, t%name), [dtmp], gone)
                end if
                if (gone) then
                    call report(db, rs, stat, errmsg)
                else
                    call wedge_table(db, t)
                    call report(db, SQR_ERR, stat, errmsg)
                end if
                return
            end if
            ! From here every failure wedges the table (wedge_table): the
            ! marker is the truth and the next open finishes the compact; the
            ! session must not go on with indexes holding pre-compact row ids.
            past_marker: block
                close(t%unit, iostat=ios); t%unit = -1   ! superseded: ignored
                ios = c_rename(dtmp, dpath)
                if (ios /= 0) exit past_marker
                if (has_text) then
                    close(t%blob_unit, iostat=ios); t%blob_unit = -1   ! superseded: ignored
                    ios = c_rename(btmp, bpath)
                    if (ios /= 0) exit past_marker
                end if
                if (db%durable) then
                    ios = c_fsync_dir(db%dir)   ! the renames themselves must be durable
                    call io_check(ios)
                    if (ios /= 0) exit past_marker
                end if

                ! Adopt the compacted counters BEFORE reopening. The renamed file
                ! is the shorter one now, and open_data cross-checks next_id
                ! against its size — a schema claiming ids past the end of the
                ! data file is corruption. Carrying the pre-compact next_id
                ! across the reopen would trip that check on a table we have
                ! just rewritten.
                t%next_id    = new_rid + 1
                t%live_count = new_rid

                call open_data(db, t, 'old', rs)
                if (rs /= SQR_OK) exit past_marker
                if (has_text) then
                    call open_blob(db, t, 'old', rs)
                    if (rs /= SQR_OK) exit past_marker
                end if

                reindex: do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle reindex
                    call rebuild_index(db, idx, j, rs)
                    if (rs /= SQR_OK) exit past_marker
                end do reindex

                call write_schema(db, t, rs)
                if (rs /= SQR_OK) exit past_marker
                ! Fully durable: data+blob renamed, indices rebuilt, schema
                ! written atomically. Retire the marker so the next open does no
                ! recovery. A retirement that cannot be confirmed wedges the
                ! table: the marker may reappear after power loss, and a later
                ! compact in this session would write new temps under it.
                call retract_marker(db, compact_marker_path(db, t%name), none, gone)
                if (.not. gone) exit past_marker
                call report(db, SQR_OK, stat, errmsg)
                return
            end block past_marker
            call wedge_table(db, t)
            call report(db, SQR_ERR, stat, errmsg)
            return
        end associate
        call report(db, SQR_OK, stat, errmsg)
    end subroutine

    ! Drop the "compact in progress" marker (see compact_marker_path). Written
    ! after the compacted temps are durable and cleared once the swap is fully
    ! committed, so its presence on open means an interrupted compact to finish.
    subroutine write_compact_marker(db, name, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=compact_marker_path(db, name), access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_path(compact_marker_path(db, name))
        call io_check(ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_dir(db%dir)   ! the new marker must be durable
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! Withdraw a marker: remove the marker FIRST, then each `staged` file (a
    ! crash in between leaves staged files without a marker, which every open
    ! ignores), then, on a durable handle, sync the directory.  Each removal
    ! is an io_check point and a file already gone is not a failure.  Every
    ! step is attempted whatever an earlier one returned, so as little as
    ! possible is left; `gone` is .true. only if every step succeeded.  Used
    ! after a marker write that failed (the marker may be on disk) and to
    ! retire a marker once its operation is complete (no staged files).  A
    ! non-durable handle takes a removal that returned success as gone.
    subroutine retract_marker(db, marker, staged, gone)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: marker
        character(len=*), intent(in)  :: staged(:)
        logical,          intent(out) :: gone
        integer :: k, ios
        call remove_existing(marker, ios)
        gone = ios == 0
        each_staged: do k = 1, size(staged)
            call remove_existing(trim(staged(k)), ios)
            if (ios /= 0) gone = .false.
        end do each_staged
        if (db%durable) then
            ios = c_fsync_dir(db%dir)   ! the removals must be durable
            call io_check(ios)
            if (ios /= 0) gone = .false.
        end if
    end subroutine

    ! Finish an interrupted compact detected on open: roll each surviving
    ! .compact temp forward over its target (the temps were made durable before
    ! the marker was dropped, so rolling forward is always the complete result),
    ! then let the caller rederive next_id/live_count and the indices from the
    ! renumbered data file. Idempotent — safe to re-run if it too is interrupted.
    subroutine complete_compact_swap(db, tbl, stat)
        type(db_t),    intent(in)  :: db
        type(table_t), intent(in)  :: tbl
        integer,       intent(out) :: stat
        integer :: ios
        character(len=:), allocatable :: dpath, dtmp, bpath, btmp
        ios = 0
        dpath = data_path(db, tbl%name)
        dtmp  = dpath // '.compact'
        if (c_path_exists(dtmp)) ios = c_rename(dtmp, dpath)
        if (ios == 0 .and. table_has_text(tbl)) then
            bpath = blob_path(db, tbl%name)
            btmp  = bpath // '.compact'
            if (c_path_exists(btmp)) ios = c_rename(btmp, bpath)
        end if
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        ios = c_fsync_dir(db%dir)
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    module subroutine db_list_tables(db, names)
        class(db_t),                               intent(in)  :: db
        character(len=SQR_NAME_LEN), allocatable, intent(out) :: names(:)
        ! A handle that was never opened has db%tables unallocated — even a
        ! zero-length section of it is not referenceable.
        if (.not. allocated(db%tables)) then
            allocate(names(0))
            return
        end if
        names = db%tables(1:db%ntables)%name
    end subroutine

    pure module function db_table_index(db, name) result(idx)
        class(db_t),       intent(in) :: db
        character(len=*), intent(in) :: name
        integer :: idx
        ! Linear scan over the contiguous tables array; see col_index — indexing
        ! %name per element avoids findloc's strided component-section copy.
        do idx = 1, db%ntables
            if (db%tables(idx)%name == name) return
        end do
        idx = 0
    end function

    pure module function db_record_size(db, name) result(sz)
        class(db_t),       intent(in) :: db
        character(len=*), intent(in) :: name
        integer                      :: sz
        integer :: ti
        sz = 0
        ti = db_table_index(db, name)
        if (ti /= 0) sz = db%tables(ti)%record_size
    end function

    pure module subroutine db_describe(db, table_name, cols, stat)
        class(db_t),      intent(in)             :: db
        character(len=*), intent(in)             :: table_name
        type(column_t), allocatable, intent(out) :: cols(:)
        integer,          intent(out), optional  :: stat
        integer :: ti
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            allocate(cols(0))
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        cols = db%tables(ti)%cols(1:db%tables(ti)%ncols)
        if (present(stat)) stat = SQR_OK
    end subroutine

    pure module function db_row_count(db, table_name) result(n)
        class(db_t),      intent(in) :: db
        character(len=*), intent(in) :: table_name
        integer(int32)               :: n
        integer :: ti
        n = 0
        ti = db_table_index(db, table_name)
        if (ti /= 0) n = db%tables(ti)%live_count
    end function

    pure module function db_in_txn(db) result(active)
        class(db_t), intent(in) :: db
        logical                 :: active
        active = db%jrnl%active .and. db%jrnl%explicit
    end function

    pure module function db_txn_mark(db) result(mark)
        class(db_t), intent(in) :: db
        integer(int64)          :: mark
        mark = merge(db%jrnl%nwrites, 0_int64, db%jrnl%active)
    end function

    module subroutine db_doom_txn(db, since)
        class(db_t),    intent(inout)        :: db
        integer(int64), intent(in), optional :: since
        if (.not. (db%jrnl%active .and. db%jrnl%explicit)) return
        if (present(since)) then
            if (db%jrnl%nwrites <= since) return
        end if
        db%jrnl%doomed = .true.
    end subroutine

    pure module function db_txn_doomed(db) result(doomed)
        class(db_t), intent(in) :: db
        logical                 :: doomed
        doomed = db%jrnl%active .and. db%jrnl%explicit .and. db%jrnl%doomed
    end function

    ! ===== Schema evolution: add / drop column =====

    module subroutine db_add_column(db, table_name, col, stat, errmsg)
        class(db_t),      intent(inout)           :: db
        character(len=*), intent(in)              :: table_name
        type(column_t),   intent(in)              :: col
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, rs, nold, k, new_rs
        type(column_t), allocatable :: newcols(:)
        integer, allocatable :: src(:), cascade(:)
        character(len=SQR_ERRMSG_LEN) :: em

        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call raise_db(db, SQR_NOT_FOUND, stat, errmsg, 'no such table: ' // trim(table_name))
            return
        end if
        nold = db%tables(ti)%ncols

        ! Candidate set = existing columns + the new one. validate_columns
        ! re-checks the whole set: the new name/dtype/csize, a name already in
        ! the table (its duplicate-name pass), and the widened record bound. It
        ! writes its own detailed message — exactly as db_create_table.
        allocate(newcols(nold + 1))
        newcols(1:nold)   = db%tables(ti)%cols(1:nold)
        newcols(nold + 1) = col
        em = ''
        call validate_columns(newcols, rs, em)
        if (rs /= SQR_OK) then
            call raise_db(db, rs, stat, errmsg, trim(em))
            return
        end if
        call layout_columns(newcols, new_rs)

        ! Each existing column maps to itself; the appended column has no old
        ! source (0 ⇒ written NULL). An ADD never drops an index.
        allocate(src(nold + 1))
        src(1:nold)   = [(k, k = 1, nold)]
        src(nold + 1) = 0
        allocate(cascade(0))

        db%generation = db%generation + 1   ! structural change: invalidate cursors
        call db_reset_history(db)           ! record_size change ⇒ captured undo/redo steps can't replay
        call apply_layout_change(db, ti, newcols, new_rs, src, cascade, rs)
        call report(db, rs, stat, errmsg)
    end subroutine

    module subroutine db_drop_column(db, table_name, col_name, stat, errmsg)
        class(db_t),      intent(inout)           :: db
        character(len=*), intent(in)              :: table_name
        character(len=*), intent(in)              :: col_name
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, p, nold, nj, nc, new_rs, rs
        type(column_t), allocatable :: newcols(:)
        integer, allocatable :: src(:), cascade(:)

        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call raise_db(db, SQR_NOT_FOUND, stat, errmsg, 'no such table: ' // trim(table_name))
            return
        end if
        associate (t => db%tables(ti))
            p = col_index(t, col_name)
            if (p == 0) then
                call raise_db(db, SQR_NOT_FOUND, stat, errmsg, &
                           'no such column: "' // trim(col_name) // '"')
                return
            end if
            if (t%ncols == 1) then
                call raise_db(db, SQR_INVALID, stat, errmsg, &
                           'cannot drop the only column of "' // trim(table_name) // '"')
                return
            end if
            nold = t%ncols

            ! New set = every column but position p, order preserved; src maps
            ! each surviving column to its old ordinal.
            allocate(newcols(nold - 1), src(nold - 1))
            nc = 0
            keep: do nj = 1, nold
                if (nj == p) cycle keep
                nc = nc + 1
                newcols(nc) = t%cols(nj)
                src(nc)     = nj
            end do keep
            call layout_columns(newcols, new_rs)

            ! CASCADE: every live index that has this column as a member.
            call cascade_indices(t, col_name, cascade)
        end associate

        db%generation = db%generation + 1   ! structural change: invalidate cursors
        call db_reset_history(db)           ! record_size change ⇒ captured undo/redo steps can't replay
        call apply_layout_change(db, ti, newcols, new_rs, src, cascade, rs)
        call report(db, rs, stat, errmsg)
    end subroutine

    module subroutine db_reshape_table(db, table_name, cols, stat, errmsg)
        class(db_t),      intent(inout)           :: db
        character(len=*), intent(in)              :: table_name
        type(column_t),   intent(in)              :: cols(:)
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, nj, new_rs, rs
        type(column_t), allocatable :: newcols(:)
        integer, allocatable :: src(:), cascade(:)
        character(len=SQR_ERRMSG_LEN) :: em

        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call raise_db(db, SQR_NOT_FOUND, stat, errmsg, 'no such table: ' // trim(table_name))
            return
        end if
        if (size(cols) == 0) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                       'a table must keep at least one column: "' // trim(table_name) // '"')
            return
        end if

        ! The target set on its own terms first (names, types, sizes, duplicate
        ! names, record bound) - exactly what db_create_table would accept.
        allocate(newcols(size(cols)))
        do nj = 1, size(cols)
            newcols(nj)%name  = cols(nj)%name
            newcols(nj)%dtype = cols(nj)%dtype
            newcols(nj)%csize = cols(nj)%csize
        end do
        em = ''
        call validate_columns(newcols, rs, em)
        if (rs /= SQR_OK) then
            call raise_db(db, rs, stat, errmsg, trim(em))
            return
        end if

        associate (t => db%tables(ti))
            ! src(nj): the stored column feeding target nj, by name (0 = new).
            allocate(src(size(newcols)))
            do nj = 1, size(newcols)
                src(nj) = col_index(t, newcols(nj)%name)
                if (src(nj) == 0) cycle
                if (t%cols(src(nj))%dtype /= newcols(nj)%dtype .or. &
                    t%cols(src(nj))%csize /= newcols(nj)%csize) then
                    call raise_db(db, SQR_INVALID, stat, errmsg, 'column "' // &
                               trim(newcols(nj)%name) // '" would change type or size')
                    return
                end if
            end do
            ! Already the target layout: nothing to rewrite, history kept.
            if (size(newcols) == t%ncols) then
                if (all(src == [(nj, nj = 1, t%ncols)])) then
                    call report(db, SQR_OK, stat, errmsg)
                    return
                end if
            end if
            call cascade_absent(t, newcols, cascade)
        end associate

        db%generation = db%generation + 1   ! structural change: invalidate cursors
        call db_reset_history(db)           ! record layout change: steps can't replay
        call layout_columns(newcols, new_rs)
        call apply_layout_change(db, ti, newcols, new_rs, src, cascade, rs)
        call report(db, rs, stat, errmsg)
    end subroutine

    ! Slots of every live index of `t` with a member column absent from
    ! `keep` - the indices db_reshape_table CASCADE-drops.
    subroutine cascade_absent(t, keep, slots)
        type(table_t),        intent(in)  :: t
        type(column_t),       intent(in)  :: keep(:)
        integer, allocatable, intent(out) :: slots(:)
        integer :: j, m, k, n
        logical :: found
        integer :: tmp(t%nindices)
        n = 0
        scan_idx: do j = 1, t%nindices
            if (.not. idx_live(t%indices(j))) cycle scan_idx
            members: do m = 1, t%indices(j)%ncols
                found = .false.
                do k = 1, size(keep)
                    if (trim(keep(k)%name) == trim(t%indices(j)%columns(m))) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    n = n + 1
                    tmp(n) = j
                    exit members
                end if
            end do members
        end do scan_idx
        slots = tmp(1:n)
    end subroutine

    ! Slots of every live index of `t` that has `col_name` as a member — the
    ! indices db_drop_column CASCADE-drops because their key would lose a column.
    subroutine cascade_indices(t, col_name, slots)
        type(table_t),        intent(in)  :: t
        character(len=*),     intent(in)  :: col_name
        integer, allocatable, intent(out) :: slots(:)
        integer :: j, m, n
        integer :: tmp(t%nindices)
        n = 0
        scan_idx: do j = 1, t%nindices
            if (.not. idx_live(t%indices(j))) cycle scan_idx
            has_member: do m = 1, t%indices(j)%ncols
                if (trim(t%indices(j)%columns(m)) == trim(col_name)) then
                    n = n + 1
                    tmp(n) = j
                    exit has_member
                end if
            end do has_member
        end do scan_idx
        slots = tmp(1:n)
    end subroutine

    ! Build one record in the new layout from a record in the old layout. The
    ! status byte is copied; new column nj is filled from old column src(nj)
    ! (data bytes + NULL state), or written NULL when src(nj) == 0 (a brand-new
    ! column has no value yet). nbuf is the exact new record size and is
    ! zero-filled first, so the wider NULL bitmap and any added column's data
    ! start clean.
    pure subroutine transform_record(obuf, nbuf, oldcols, newcols, src)
        character(len=*), intent(in)  :: obuf
        character(len=*), intent(out) :: nbuf
        type(column_t),   intent(in)  :: oldcols(:), newcols(:)
        integer,          intent(in)  :: src(:)
        integer :: nj, oj
        nbuf = repeat(CH_NUL, len(nbuf))
        nbuf(ROW_STATUS_OFF:ROW_STATUS_OFF) = obuf(ROW_STATUS_OFF:ROW_STATUS_OFF)
        cols: do nj = 1, size(newcols)
            oj = src(nj)
            if (oj == 0) then
                call row_set_null(nbuf, newcols(nj))    ! brand-new column
                cycle cols
            end if
            associate (nc => newcols(nj), oc => oldcols(oj))
                nbuf(nc%offset : nc%offset + nc%csize - 1) = &
                    obuf(oc%offset : oc%offset + oc%csize - 1)
                if (row_is_null(obuf, oc)) call row_set_null(nbuf, nc)
            end associate
        end do cols
    end subroutine

    ! Rewrite every record of table ti into `newcols` (already laid out, record
    ! size `new_rs`); `src(nj)` is the old column feeding new column nj (0 =
    ! brand-new, written NULL); `cascade` lists the index slots to drop (an
    ! index with a member that no longer exists).  Shared by db_add_column,
    ! db_drop_column and db_reshape_table.
    !
    ! row_ids are PRESERVED: alive and tombstoned slots alike are rewritten at
    ! the same record number, and next_id / live_count are unchanged.
    ! Surviving indices keep their B+-trees untouched - member values and
    ! row_ids are unchanged and key_off / key_size do not move - so only the
    ! in-memory col_idx is re-resolved by name.  No index is rebuilt.
    !
    ! Atomicity, the db_compact pattern:
    !   1. STAGE - every fallible write happens beside the originals: the
    !      rewritten data file (`<t>.dat.alter`), the new schema
    !      (`<t>.schema.alter`) and, when the change gives the table its first
    !      DT_TEXT column, an empty blob file (`<t>.blob.alter`), each made
    !      durable.  A failure here deletes the temps; the table is untouched.
    !   2. COMMIT - a durable marker (`<t>.altering`).  From here the staged
    !      files are the truth.
    !   3. SWAP - renames only, then the marker is cleared.  An interruption
    !      anywhere after the marker is rolled forward on the next open
    !      (complete_alter_swap), before the schema is read.
    ! A failure past the commit returns an error but leaves the database
    ! consistent on disk in the NEW layout (a reopen sees it whole); the
    ! table is wedged for the rest of the session (wedge_table).
    subroutine apply_layout_change(db, ti, newcols, new_rs, src, cascade, stat)
        type(db_t),     intent(inout) :: db
        integer,        intent(in)    :: ti
        type(column_t), intent(in)    :: newcols(:)
        integer,        intent(in)    :: new_rs
        integer,        intent(in)    :: src(:)
        integer,        intent(in)    :: cascade(:)
        integer,        intent(out)   :: stat
        integer :: ud, ios, rs, j, m, k, old_rs
        integer(int32) :: rid
        logical :: had_text, has_text_new, gone
        character(len=:), allocatable :: rbuf, nbuf, dpath, dtmp, spath, stmp, btmp
        type(column_t), allocatable :: oldcols(:)
        type(table_t) :: nt

        stat = SQR_OK
        ud   = -1
        associate (t => db%tables(ti))
            old_rs   = t%record_size
            oldcols  = t%cols(1:t%ncols)
            had_text = table_has_text(t)
            dpath = data_path(db, t%name)
            dtmp  = dpath // '.alter'
            spath = schema_path(db, t%name)
            stmp  = spath // '.alter'
            btmp  = blob_path(db, t%name) // '.alter'
            has_text_new = any(newcols%dtype == DT_TEXT)

            ! ---- 1. STAGE: the rewritten data file beside the original.  The
            ! original data unit stays open and untouched, so any failure here
            ! just drops the temps and returns with nothing committed.
            build: block
                call remove_file(dtmp)   ! stale temps from an abandoned attempt
                call remove_file(stmp)
                call remove_file(btmp)
                open(newunit=ud, file=dtmp, access='direct', form='unformatted', &
                     recl=new_rs, status='replace', action='readwrite', iostat=ios)
                if (ios /= 0) then
                    rs = SQR_ERR
                    exit build
                end if
                allocate(character(len=old_rs) :: rbuf)
                allocate(character(len=new_rs) :: nbuf)
                copy_rows: do rid = 1, t%next_id - 1
                    read(t%unit, rec=rid, iostat=ios) rbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        rs = SQR_ERR
                        exit build
                    end if
                    call transform_record(rbuf, nbuf, oldcols, newcols, src)
                    write(ud, rec=rid, iostat=ios) nbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        rs = SQR_ERR
                        exit build
                    end if
                end do copy_rows
                rs = SQR_OK
            end block build
            if (rs /= SQR_OK) then
                if (ud /= -1) close(ud, status='delete', iostat=ios)   ! a temp: ignored
                stat = rs
                return
            end if
            close(ud, iostat=ios)                     ! the final flush
            if (ios == 0 .and. db%durable) then
                ios = c_fsync_path(dtmp)
                call io_check(ios)
            end if
            if (ios /= 0) then
                call remove_file(dtmp)
                stat = SQR_ERR
                return
            end if

            ! ...the blob file of a table gaining its first DT_TEXT column.
            ! Staged like the rest, so the swap (and an open that finishes an
            ! interrupted one) puts it in place with the schema that needs it.
            if (has_text_new .and. .not. had_text) then
                call stage_empty_file(db, btmp, rs)
                if (rs /= SQR_OK) then
                    call remove_file(dtmp)
                    call remove_file(btmp)
                    stat = rs
                    return
                end if
            end if

            ! ...and the schema describing it: a copy of the table in the new
            ! layout with the cascaded slots tombstoned, staged durably.
            nt = t
            nt%cols        = newcols
            nt%ncols       = size(newcols)
            nt%record_size = new_rs
            do k = 1, size(cascade)
                call tombstone_slot(nt%indices(cascade(k)))
            end do
            call stage_schema(db, nt, stmp, rs)
            if (rs == SQR_OK .and. db%durable) then
                ios = c_fsync_dir(db%dir)
                call io_check(ios)
                if (ios /= 0) rs = SQR_ERR
            end if
            if (rs /= SQR_OK) then
                call remove_file(dtmp)
                call remove_file(stmp)
                call remove_file(btmp)
                stat = rs
                return
            end if

            ! ---- 2. COMMIT: the marker makes the staged pair authoritative.
            ! A failed marker write may still have left the marker on disk: it
            ! and the staged files are withdrawn (an ordinary failure, the
            ! table untouched), and if that cannot be confirmed the table is
            ! wedged, so the next open never swaps a stale layout over later
            ! writes.
            call write_alter_marker(db, t%name, rs)
            if (rs /= SQR_OK) then
                call retract_marker(db, alter_marker_path(db, t%name), &
                    [character(len=max(len(dtmp), len(stmp), len(btmp))) :: dtmp, stmp, btmp], gone)
                if (gone) then
                    stat = rs
                else
                    call wedge_table(db, t)
                    stat = SQR_ERR
                end if
                return
            end if

            ! ---- 3. SWAP: renames only; the next open finishes an interrupted one.
            ! From here every failure wedges the table (wedge_table): the
            ! marker on disk is the truth, and the session neither adopts the
            ! new layout over a swap that may not have happened nor writes the
            ! old one over a swap that did.
            close(t%unit, iostat=ios); t%unit = -1   ! superseded: a failed close is ignored
            call complete_alter_swap(db, t%name, rs)
            if (rs /= SQR_OK) then
                call wedge_table(db, t)
                stat = SQR_ERR
                return
            end if

            ! In-memory table now takes the new layout (the staged schema's).
            t%cols        = newcols
            t%ncols       = size(newcols)
            t%record_size = new_rs
            t%disk_next_id    = t%next_id
            t%disk_live_count = t%live_count

            ! CASCADE: tombstone each dropped-member index (slot kept stable so
            ! survivors' __i<slot> file names are undisturbed) and close its tree.
            drop_cascade: do k = 1, size(cascade)
                call bt_discard(t%indices(cascade(k))%bt)
                call tombstone_slot(t%indices(cascade(k)))
            end do drop_cascade

            ! Surviving indices: re-resolve member ordinals against the new columns.
            fix_idx: do j = 1, t%nindices
                if (.not. idx_live(t%indices(j))) cycle fix_idx
                members: do m = 1, t%indices(j)%ncols
                    t%indices(j)%col_idx(m) = col_index(t, t%indices(j)%columns(m))
                end do members
            end do fix_idx

            ! ---- Past the swap: reopen data, adjust the blob, drop cascade files.
            call open_data(db, t, 'old', rs)
            if (rs /= SQR_OK) then
                call wedge_table(db, t)
                stat = SQR_ERR
                return
            end if

            if (has_text_new .and. t%blob_unit == -1) then
                ! First DT_TEXT column on a previously text-less table: the
                ! swap has just moved its staged blob file into place.
                call open_blob(db, t, 'old', rs)
                if (rs /= SQR_OK) then
                    call wedge_table(db, t)
                    stat = SQR_ERR
                    return
                end if
            else if (had_text .and. .not. has_text_new) then
                ! Last DT_TEXT column gone: the blob file is now orphaned.
                if (t%blob_unit /= -1) then
                    close(t%blob_unit, iostat=ios)   ! about to be deleted: ignored
                    t%blob_unit = -1
                end if
                call remove_file(blob_path(db, t%name))
                t%blob_next = 1_int64
            end if

            del_cascade: do k = 1, size(cascade)
                call remove_file(index_path(db, t%name, cascade(k)))
            end do del_cascade
        end associate
    end subroutine

    ! Drop the layout-change commit marker (alter_marker_path) durably.
    subroutine write_alter_marker(db, name, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=alter_marker_path(db, name), access='stream', &
             form='unformatted', status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_path(alter_marker_path(db, name))
        call io_check(ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_dir(db%dir)
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! Create `path` empty, replacing anything already there, and make its
    ! contents durable.  The caller syncs the directory.
    subroutine stage_empty_file(db, path, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: path
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)
        call io_check(ios)
        if (ios == 0 .and. db%durable) ios = c_fsync_path(path)
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! Roll a committed layout change forward: move each staged file still
    ! present over its target, then clear the marker.  Idempotent - every step
    ! is a rename or a removal of something that may already be gone - so it
    ! is safe to re-run when it is itself interrupted.  Used by the change
    ! itself and by db_open when it finds the marker.
    subroutine complete_alter_swap(db, name, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        integer,          intent(out) :: stat
        integer :: ios
        logical :: gone
        character(len=1) :: none(0)
        character(len=:), allocatable :: dpath, spath, bpath
        ios = 0
        dpath = data_path(db, name)
        spath = schema_path(db, name)
        bpath = blob_path(db, name)
        ! The blob file first: once the schema names a DT_TEXT column the file
        ! it needs is already there.
        if (c_path_exists(bpath // '.alter')) ios = c_rename(bpath // '.alter', bpath)
        if (ios == 0) then
            if (c_path_exists(dpath // '.alter')) ios = c_rename(dpath // '.alter', dpath)
        end if
        if (ios == 0 .and. c_path_exists(spath // '.alter')) &
            ios = c_rename(spath // '.alter', spath)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (db%durable) then
            ios = c_fsync_dir(db%dir)            ! the renames must be durable
            call io_check(ios)                   ! before the marker goes
            if (ios /= 0) then
                stat = SQR_ERR
                return
            end if
        end if
        call retract_marker(db, alter_marker_path(db, name), none, gone)
        stat = merge(SQR_OK, SQR_ERR, gone)
    end subroutine

end submodule sqr_table

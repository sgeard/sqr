! sqr_table — table and database lifecycle for the sqr module.
!
! Descendant of `sqr_base`: the catalog/schema codecs, file-open helpers,
! validation, layout and B+-tree rebuild it relies on all come from the parent
! submodule by host association.  Holds the public lifecycle API — opening and
! closing a database, creating/dropping/compacting tables, and the table
! lookup helpers.

submodule (sqr:sqr_base) sqr_table
    use :: clib_wrap, only: c_rename, c_remove, c_lock_try, c_lock_share
    implicit none
contains

    module subroutine db_open(db, dir, stat, errmsg, readonly)
        class(db_t),       intent(out)             :: db
        character(len=*), intent(in)              :: dir
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        logical,          intent(in),   optional  :: readonly
        integer :: rs, i, j, n
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        character(len=:), allocatable :: ndir

        rs = SQR_OK
        ! Fold any '\' to '/' so the engine reasons about a single separator on
        ! every platform (Windows accepts both); see norm_seps.
        ndir = norm_seps(dir)
        open_seq: block
            if (.not. valid_dir_name(ndir)) then
                rs = SQR_INVALID
                call raise(rs, stat, errmsg, &
                           'invalid database directory name: "' // trim(dir) // '"')
                exit open_seq
            end if

            db%dir = trim(ndir)
            db%ntables  = 0
            allocate(db%tables(0))
            db%opened   = .false.
            db%readonly = .false.
            if (present(readonly)) db%readonly = readonly

            ! Read-only opens require an initialised database (catalog file
            ! must exist); read-write opens create the directory if needed.
            ! We probe the catalog file rather than the directory itself
            ! because inquire on directories is unreliable across compilers
            ! (ifx returns .false.).
            if (db%readonly) then
                if (.not. file_exists(catalog_path(db))) then
                    rs = SQR_NOT_FOUND
                    call raise(rs, stat, errmsg, &
                               'database not found: "' // trim(db%dir) // '"')
                    exit open_seq
                end if
            else
                ! Create the directory and any missing parents (a database may
                ! now be nested or absolute). An already-present directory is
                ! fine (idempotent); a genuine failure to create is fatal.
                if (.not. mkdir_p(db%dir)) then
                    rs = SQR_ERR
                    call raise(rs, stat, errmsg, &
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
                    call raise(rs, stat, errmsg, &
                               'database is locked by another connection: "' &
                               // trim(db%dir) // '"')
                    exit open_seq
                else if (lerr /= 0) then
                    rs = SQR_ERR
                    call raise(rs, stat, errmsg, &
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
                    call raise(rs, stat, errmsg, &
                               'database needs recovery; reopen read-write: "' &
                               // trim(db%dir) // '"')
                    exit open_seq
                end if
            else
                call jrnl_recover(db, rs)
                if (rs /= SQR_OK) then
                    call raise(rs, stat, errmsg, 'journal recovery failed')
                    exit open_seq
                end if
            end if

            call read_catalog(db, names, n, rs)
            if (rs /= SQR_OK) then
                call raise(rs, stat, errmsg, 'cannot read catalog')
                exit open_seq
            end if

            if (n > 0) then
                deallocate(db%tables)
                allocate(db%tables(n))
                tables_open: do i = 1, n
                    associate (t => db%tables(i))
                        ! read_schema writes its own detailed errmsg
                        ! (bad magic / version mismatch / ...).
                        call read_schema(db, trim(names(i)), t, rs, errmsg)
                        if (rs /= SQR_OK) then
                            call raise(rs, stat, errmsg)
                            exit open_seq
                        end if
                        call open_data(db, t, 'old', rs)
                        if (rs /= SQR_OK) then
                            call raise(rs, stat, errmsg, &
                                       'cannot open data file for ' // trim(names(i)))
                            exit open_seq
                        end if
                        indices_open: do j = 1, t%nindices
                            if (.not. idx_live(t%indices(j))) cycle indices_open
                            call open_index(db, t, t%indices(j), j, 'old', rs)
                            if (rs /= SQR_OK) then
                                call raise(rs, stat, errmsg, &
                                           'cannot open index file for ' // trim(names(i)))
                                exit open_seq
                            end if
                        end do indices_open
                        if (table_has_text(t)) then
                            call open_blob(db, t, 'old', rs)
                            if (rs /= SQR_OK) then
                                call raise(rs, stat, errmsg, &
                                           'cannot open blob file for ' // trim(names(i)))
                                exit open_seq
                            end if
                        end if
                    end associate
                end do tables_open
                db%ntables = n
            end if

            db%opened = .true.
        end block open_seq

        if (rs /= SQR_OK) then
            call abandon_open(db)
        else if (present(stat)) then
            stat = SQR_OK
        end if
    end subroutine

    module subroutine db_close(db, stat)
        class(db_t), intent(inout)         :: db
        integer,    intent(out), optional :: stat
        integer :: i, j, rs, first, cs
        first = SQR_OK
        if (present(stat)) stat = SQR_OK
        if (.not. db%opened) return
        close_tables: do i = 1, db%ntables
            associate (t => db%tables(i))
                ! Schema counters (next_id/live_count) are flushed only here,
                ! so capture the first write failure for the caller — but keep
                ! closing everything so units are not leaked.
                if (.not. db%readonly) then
                    call write_schema(db, t, rs)
                    if (rs /= SQR_OK .and. first == SQR_OK) first = rs
                end if
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
                    if (idx_live(t%indices(j))) call bt_close(t%indices(j)%bt)
                    ! Free any journal-hook context left by an unclosed txn so
                    ! the heap target does not leak when the slot is deallocated.
                    if (associated(t%indices(j)%jctx)) deallocate(t%indices(j)%jctx)
                end do close_indices
            end associate
        end do close_tables
        if (.not. db%readonly) then
            call write_catalog(db, rs)
            if (rs /= SQR_OK .and. first == SQR_OK) first = rs
            ! A clean close guarantees no transaction is live, so any journal on
            ! disk is a voided leftover: delete it so the next open does zero
            ! recovery work.  (Read-only opens never write, so they leave it.)
            del_journal: block
                character(len=:), allocatable :: jpath
                jpath = pathjoin(db%dir, '_journal.dat')
                if (c_path_exists(jpath)) then
                    if (c_remove(jpath) /= 0 .and. first == SQR_OK) first = SQR_ERR
                end if
            end block del_journal
        end if
        if (allocated(db%tables)) deallocate(db%tables)
        if (allocated(db%dir))    deallocate(db%dir)
        ! Release the advisory lock (closing its descriptor/handle).
        call c_lock_release(db%lock_tok)
        db%ntables  = 0
        db%opened   = .false.
        db%readonly = .false.
        if (present(stat)) stat = first
    end subroutine

    module subroutine db_set_readonly(db, stat)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        if (present(stat)) stat = SQR_OK
        if (.not. db%opened) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        if (db%readonly) return            ! already read-only: nothing to do
        ! A live transaction owns uncommitted state; demoting now would strand
        ! it.  The caller must commit or roll back first.
        if (db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        db%readonly = .true.               ! mutators now refuse via readonly_block
        ! Drop the exclusive lock to shared so other read-only connections may
        ! attach.  A failed downgrade still leaves the handle safely read-only.
        if (c_lock_share(db%lock_tok) /= 0 .and. present(stat)) stat = SQR_ERR
    end subroutine

    module subroutine db_create_table(db, name, cols, stat, errmsg)
        class(db_t),       intent(inout)           :: db
        character(len=*), intent(in)              :: name
        type(column_t),   intent(in)              :: cols(:)
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        type(table_t), allocatable :: new_tables(:)
        type(table_t) :: tbl
        integer :: rs

        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        db%generation = db%generation + 1   ! structural change: invalidate cursors

        if (.not. valid_name(name)) then
            call raise(SQR_INVALID, stat, errmsg, &
                       'invalid table name: "' // trim(name) // '"')
            return
        end if

        ! validate_columns writes its own detailed errmsg; just route stat.
        call validate_columns(cols, rs, errmsg)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if

        if (db_table_index(db, name) > 0) then
            call raise(SQR_DUP, stat, errmsg, &
                       'table already exists: ' // trim(name))
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
            if (present(stat)) stat = rs
            return
        end if
        call open_data(db, tbl, 'new', rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        if (table_has_text(tbl)) then
            call open_blob(db, tbl, 'replace', rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
        end if

        allocate(new_tables(db%ntables + 1))
        new_tables(1:db%ntables) = db%tables(1:db%ntables)
        new_tables(db%ntables + 1) = tbl
        call move_alloc(new_tables, db%tables)
        db%ntables = db%ntables + 1

        call write_catalog(db, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if

        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_drop_table(db, name, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: name
        integer,          intent(out), optional :: stat
        type(table_t), allocatable :: nt(:)
        integer :: j, rs, idx, u, ios, ni
        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        db%generation = db%generation + 1   ! shifts table slots: invalidate cursors
        idx = db_table_index(db, name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if

        ! Order matters: drop the table from the catalog *before* deleting any
        ! files. If we deleted first and the catalog write then failed (or the
        ! process died in between), the catalog would still name a table whose
        ! schema file is gone, and db_open would hard-fail the whole database.
        ! With catalog-first the worst case is orphaned files on disk (benign,
        ! re-creatable) rather than an unopenable store.

        ! Close all of this table's units while db%tables(idx) is still live.
        ni = db%tables(idx)%nindices
        if (db%tables(idx)%unit /= -1) close(db%tables(idx)%unit)
        if (db%tables(idx)%blob_unit /= -1) close(db%tables(idx)%blob_unit)
        close_indices: do j = 1, ni
            associate (ix => db%tables(idx)%indices(j))
                if (ix%bt%unit /= -1) then
                    close(ix%bt%unit)
                    ix%bt%unit = -1
                end if
            end associate
        end do close_indices

        ! Shrink db%tables — remove element at idx — then persist the catalog.
        allocate(nt(db%ntables - 1))
        nt(1:idx-1)          = db%tables(1:idx-1)        ! section copies, no
        nt(idx:db%ntables-1) = db%tables(idx+1:db%ntables)  ! constructor temp
        call move_alloc(nt, db%tables)
        db%ntables = db%ntables - 1

        call write_catalog(db, rs)
        if (rs /= SQR_OK) then
            ! Catalog not updated: leave the files in place so the table is
            ! still recoverable rather than orphaning a half-dropped table.
            if (present(stat)) stat = rs
            return
        end if

        ! Catalog no longer references the table — now reclaim its files.
        ! Any failure here leaves harmless orphans, not a broken database.
        open(newunit=u, file=data_path(db, name), status='old', iostat=ios)
        if (ios == 0) close(u, status='delete')
        open(newunit=u, file=blob_path(db, name), status='old', iostat=ios)
        if (ios == 0) close(u, status='delete')
        del_indices: do j = 1, ni
            open(newunit=u, file=index_path(db, name, j), &
                 status='old', iostat=ios)
            if (ios == 0) close(u, status='delete')
        end do del_indices
        open(newunit=u, file=schema_path(db, name), status='old', iostat=ios)
        if (ios == 0) close(u, status='delete')

        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_compact(db, table_name, stat)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        integer,          intent(out), optional :: stat
        integer :: idx, ud, ub, ios, rs, j, ci, u
        integer(int32) :: rid, new_rid, length
        integer(int64) :: off, newpos
        logical :: has_text
        character(len=:), allocatable :: rbuf, dpath, dtmp, bpath, btmp

        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        db%generation = db%generation + 1   ! renumbers rows: invalidate cursors
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
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
                open(newunit=u, file=dtmp, status='old', iostat=ios)
                if (ios == 0) close(u, status='delete')
                if (has_text) then
                    open(newunit=u, file=btmp, status='old', iostat=ios)
                    if (ios == 0) close(u, status='delete')
                end if

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
                ! Originals untouched — discard the partial temp files.
                if (ud /= -1) close(ud, status='delete')
                if (ub /= -1) close(ub, status='delete')
                if (present(stat)) stat = rs
                return
            end if
            ! Guard the temp closes: a failed final flush here means the temp is
            ! not fully written, so abort before the swap rather than error stop.
            close(ud, iostat=ios)
            if (ios == 0 .and. has_text) close(ub, iostat=ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if

            ! Phase B — swap in the compacted files, then rebuild derived
            ! state. rename(2) atomically replaces the destination, so no
            ! separate delete is needed. A crash between the two renames or
            ! during the index rebuild is the documented residual window
            ! (no journaling — transactions are deferred); indices are
            ! derivable, so re-running compact recovers.
            close(t%unit); t%unit = -1
            if (c_rename(dtmp, dpath) /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (has_text) then
                close(t%blob_unit); t%blob_unit = -1
                if (c_rename(btmp, bpath) /= 0) then
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
            end if

            call open_data(db, t, 'old', rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
            if (has_text) then
                call open_blob(db, t, 'old', rs)
                if (rs /= SQR_OK) then
                    if (present(stat)) stat = rs
                    return
                end if
            end if

            t%next_id    = new_rid + 1
            t%live_count = new_rid

            reindex: do j = 1, t%nindices
                if (.not. idx_live(t%indices(j))) cycle reindex
                call rebuild_index(db, idx, j, rs)
                if (rs /= SQR_OK) then
                    if (present(stat)) stat = rs
                    return
                end if
            end do reindex

            call write_schema(db, t, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_list_tables(db, names)
        class(db_t),                               intent(in)  :: db
        character(len=SQR_NAME_LEN), allocatable, intent(out) :: names(:)
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

        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        db%generation = db%generation + 1   ! structural change: invalidate cursors
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call raise(SQR_NOT_FOUND, stat, errmsg, 'no such table: ' // trim(table_name))
            return
        end if
        nold = db%tables(ti)%ncols

        ! Candidate set = existing columns + the new one. validate_columns
        ! re-checks the whole set: the new name/dtype/csize, a name already in
        ! the table (its duplicate-name pass), and the widened record bound. It
        ! writes its own errmsg, so just route stat — exactly as db_create_table.
        allocate(newcols(nold + 1))
        newcols(1:nold)   = db%tables(ti)%cols(1:nold)
        newcols(nold + 1) = col
        call validate_columns(newcols, rs, errmsg)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        call layout_columns(newcols, new_rs)

        ! Each existing column maps to itself; the appended column has no old
        ! source (0 ⇒ written NULL). An ADD never drops an index.
        allocate(src(nold + 1))
        src(1:nold)   = [(k, k = 1, nold)]
        src(nold + 1) = 0
        allocate(cascade(0))

        call apply_layout_change(db, ti, newcols, new_rs, src, cascade, stat)
    end subroutine

    module subroutine db_drop_column(db, table_name, col_name, stat, errmsg)
        class(db_t),      intent(inout)           :: db
        character(len=*), intent(in)              :: table_name
        character(len=*), intent(in)              :: col_name
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, p, nold, nj, nc, new_rs
        type(column_t), allocatable :: newcols(:)
        integer, allocatable :: src(:), cascade(:)

        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        db%generation = db%generation + 1   ! structural change: invalidate cursors
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call raise(SQR_NOT_FOUND, stat, errmsg, 'no such table: ' // trim(table_name))
            return
        end if
        associate (t => db%tables(ti))
            p = col_index(t, col_name)
            if (p == 0) then
                call raise(SQR_NOT_FOUND, stat, errmsg, &
                           'no such column: "' // trim(col_name) // '"')
                return
            end if
            if (t%ncols == 1) then
                call raise(SQR_INVALID, stat, errmsg, &
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

        call apply_layout_change(db, ti, newcols, new_rs, src, cascade, stat)
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
        nbuf = repeat(char(0), len(nbuf))
        nbuf(1:1) = obuf(1:1)                       ! status byte
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

    ! Rewrite every record of table ti from its current layout into `newcols`
    ! (already laid out, record size `new_rs`); `src(nj)` is the old column
    ! index supplying new column nj (0 = brand-new, written NULL). Shared by
    ! db_add_column and db_drop_column.
    !
    ! row_ids are PRESERVED: alive and tombstoned slots alike are rewritten at
    ! the same record number, and next_id / live_count are unchanged (unlike
    ! db_compact, which renumbers). `cascade` lists secondary-index slots to
    ! drop (the indices that referenced a dropped column). Surviving indices
    ! keep their on-disk B+-trees untouched — the column VALUES and row_ids in
    ! every entry are unchanged, and key_off / key_size do not move, so the only
    ! thing that shifts is the in-memory col_idx (a drop renumbers later
    ! columns), which is simply re-resolved by name. No index is rebuilt.
    !
    ! Durability mirrors db_compact: build a temp data file alongside the
    ! original (failure there is clean — originals untouched), then commit by
    ! renaming it in and rewriting the schema back to back. A hard crash
    ! strictly between those two is the documented pre-journal residual window
    ! (the on-disk data would then be the new layout while the schema still
    ! describes the old one); the Phase-2 journal closes it.
    subroutine apply_layout_change(db, ti, newcols, new_rs, src, cascade, stat)
        type(db_t),     intent(inout) :: db
        integer,        intent(in)    :: ti
        type(column_t), intent(in)    :: newcols(:)
        integer,        intent(in)    :: new_rs
        integer,        intent(in)    :: src(:)
        integer,        intent(in)    :: cascade(:)
        integer,        intent(out)   :: stat
        integer :: ud, ios, rs, u, j, m, k, old_rs, nnew
        integer(int32) :: rid
        logical :: had_text, has_text_new
        character(len=:), allocatable :: rbuf, nbuf, dpath, dtmp
        type(column_t), allocatable :: oldcols(:)

        stat = SQR_OK
        ud   = -1
        nnew = size(newcols)
        associate (t => db%tables(ti))
            old_rs   = t%record_size
            oldcols  = t%cols(1:t%ncols)
            had_text = table_has_text(t)
            dpath = data_path(db, t%name)
            dtmp  = dpath // '.alter'

            ! ---- Phase A: build the rewritten data file beside the original.
            ! The original data unit stays open and untouched, so any failure
            ! here just drops the temp file and returns with nothing committed.
            build: block
                open(newunit=u, file=dtmp, status='old', iostat=ios)
                if (ios == 0) close(u, status='delete')   ! stale temp from a crash
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
                if (ud /= -1) close(ud, status='delete')
                stat = rs
                return
            end if
            ! Guard the temp close (final flush) before committing the swap.
            close(ud, iostat=ios)
            if (ios /= 0) then
                stat = SQR_ERR
                return
            end if

            ! ---- Commit: swap in the rewritten data, then persist the new
            ! schema. These two are kept adjacent — the residual crash window.
            close(t%unit, iostat=ios); t%unit = -1
            if (c_rename(dtmp, dpath) /= 0) then
                stat = SQR_ERR
                return
            end if

            ! In-memory table now takes the new layout.
            t%cols        = newcols
            t%ncols       = nnew
            t%record_size = new_rs

            ! CASCADE: tombstone each dropped-member index (slot kept stable so
            ! survivors' __i<slot> file names are undisturbed) and close its
            ! tree; the file is deleted after the schema commit.
            drop_cascade: do k = 1, size(cascade)
                associate (ix => t%indices(cascade(k)))
                    if (ix%bt%unit /= -1) then
                        close(ix%bt%unit)
                        ix%bt%unit = -1
                    end if
                    ix%ncols    = 0
                    ix%key_size = 0
                    ix%nentries = 0
                    ix%unique   = .false.
                    if (allocated(ix%columns)) deallocate(ix%columns)
                    if (allocated(ix%col_idx)) deallocate(ix%col_idx)
                    if (allocated(ix%key_off)) deallocate(ix%key_off)
                end associate
            end do drop_cascade

            ! Surviving indices: re-resolve member ordinals against the new
            ! column array (a drop shifts later columns down). key_off /
            ! key_size and the tree itself are unchanged — same keys, same
            ! row_ids — so nothing is rebuilt.
            fix_idx: do j = 1, t%nindices
                if (.not. idx_live(t%indices(j))) cycle fix_idx
                members: do m = 1, t%indices(j)%ncols
                    t%indices(j)%col_idx(m) = col_index(t, t%indices(j)%columns(m))
                end do members
            end do fix_idx

            call write_schema(db, t, rs)
            if (rs /= SQR_OK) then
                stat = rs
                return
            end if

            ! ---- Past the commit: reopen data, adjust the blob, drop cascade files.
            call open_data(db, t, 'old', rs)
            if (rs /= SQR_OK) then
                stat = rs
                return
            end if

            has_text_new = table_has_text(t)
            if (has_text_new .and. t%blob_unit == -1) then
                ! First DT_TEXT column on a previously text-less table.
                call open_blob(db, t, 'replace', rs)
                if (rs /= SQR_OK) then
                    stat = rs
                    return
                end if
            else if (had_text .and. .not. has_text_new) then
                ! Last DT_TEXT column gone: the blob file is now orphaned.
                if (t%blob_unit /= -1) then
                    close(t%blob_unit)
                    t%blob_unit = -1
                end if
                open(newunit=u, file=blob_path(db, t%name), status='old', iostat=ios)
                if (ios == 0) close(u, status='delete')
                t%blob_next = 1_int64
            end if

            del_cascade: do k = 1, size(cascade)
                open(newunit=u, file=index_path(db, t%name, cascade(k)), &
                     status='old', iostat=ios)
                if (ios == 0) close(u, status='delete')
            end do del_cascade
        end associate
        stat = SQR_OK
    end subroutine

end submodule sqr_table

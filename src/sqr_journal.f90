! sqr_journal — the rollback (undo) journal for crash-safe atomicity.
!
! Descendant of `sqr_base`: it inherits `int32`/`int64`, `io_check`, `pathjoin`
! and the basic filesystem shims by host association, and adds the durability
! primitives `c_fsync_path` / `c_fsync_dir` / `c_truncate` (not pulled in by
! the parent) with its own `use`.
!
! Design: a physical *undo* journal with strict write-ahead ordering.  Before
! a transaction overwrites any region of a base file, the original bytes
! (REGION) or the original file length (EXTEND, for appends/growth) are
! captured.  Capture happens through jrnl_log_region/jrnl_log_extend, which
! every base-write site calls *before* its write — and each of those calls
! re-serialises the undo set to the hot journal and fsyncs it before
! returning.  So the undo image of a region is always durable before that
! region's new value can reach disk: a crash at any instant is recoverable by
! replaying the undo records in reverse (jrnl_recover).  Commit makes the new
! base data durable (fsync of every modified file) and only then zeroes the
! header — the single durable commit point; recovery after that sees nothing
! to do.  A commit that cannot make the base data durable rolls the whole
! transaction back rather than reporting a false success.
!
! On-disk format of `<db>/_journal.dat` (native byte order — a journal is a
! transient same-machine sidecar, never carried between hosts):
!   header @ pos 1 (28 bytes used, 64 reserved):
!     magic 'SQRJ' (4) | fmt int32 | state int32 | nrec int32
!                      | checksum int32 | payload_len int64
!   records @ pos 65, repeated nrec times:
!     kind int32 | pathlen int32 | path(pathlen) | orig_len int64
!                | offset int64 | length int64 | byteslen int64 | bytes(byteslen)
submodule (sqr:sqr_base) sqr_journal
    use :: clib_wrap, only: c_fsync_path, c_fsync_dir, c_truncate, c_remove
    implicit none

    character(len=4), parameter :: JMAGIC      = 'SQRJ'
    integer,          parameter :: JFMT        = 1        ! journal format version
    integer,          parameter :: JSTATE_VOID = 0        ! empty / invalidated
    integer,          parameter :: JSTATE_HOT  = 1        ! valid undo set present
    integer,          parameter :: UNDO_REGION = 1        ! original bytes captured
    integer,          parameter :: UNDO_EXTEND = 2        ! original length captured
    integer(int64),   parameter :: JHEADER     = 64_int64 ! reserved header size
    integer(int64),   parameter :: JPRESIZE    = 131072_int64  ! 128 KiB pre-allocation

contains

    ! ---- explicit transaction façade ----
    ! db_begin/commit/rollback wrap the txn_* primitives for callers (and the
    ! coming SQL layer).  The only state they add over the primitives is the
    ! `explicit` latch: it distinguishes a user-owned transaction from the
    ! auto-commit bracket a single mutator opens around itself, and it is what
    ! makes nesting detectable.  v1 has no nested transactions.

    module subroutine db_begin(db, stat)
        class(db_t), intent(inout), target :: db
        integer,    intent(out), optional  :: stat
        integer :: st
        if (present(stat)) stat = SQR_OK
        if (db%jrnl%active) then          ! no nesting in v1
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        call txn_begin(db, st)
        if (st /= SQR_OK) then
            if (present(stat)) stat = st
            return
        end if
        db%jrnl%explicit = .true.
    end subroutine

    module subroutine db_commit(db, stat)
        class(db_t), intent(inout) :: db
        integer,    intent(out), optional :: stat
        if (present(stat)) stat = SQR_OK
        if (.not. (db%jrnl%active .and. db%jrnl%explicit)) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        call txn_commit(db, stat)         ! also clears the explicit latch
    end subroutine

    module subroutine db_rollback(db, stat)
        class(db_t), intent(inout) :: db
        integer,    intent(out), optional :: stat
        if (present(stat)) stat = SQR_OK
        if (.not. (db%jrnl%active .and. db%jrnl%explicit)) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        call txn_rollback(db, stat)       ! also clears the explicit latch
    end subroutine

    ! ---- public transaction API ----

    module subroutine txn_begin(db, stat)
        class(db_t), intent(inout), target :: db
        integer,    intent(out), optional  :: stat
        if (present(stat)) stat = SQR_OK
        if (db%readonly) then
            if (present(stat)) stat = SQR_READONLY
            return
        end if
        if (.not. allocated(db%jrnl%path)) &
            db%jrnl%path = pathjoin(db%dir, '_journal.dat')
        ! Fresh in-memory undo set; the file (if any) stays invalid until arm.
        if (allocated(db%jrnl%recs)) deallocate(db%jrnl%recs)
        allocate(db%jrnl%recs(0))
        db%jrnl%nrec   = 0
        db%jrnl%active = .true.
        db%jrnl%armed  = .false.
        ! Push every buffered base-file write to disk first: the journal captures
        ! file extents (and, un-armed, bytes) by reading disk, so a write still
        ! sitting in a unit buffer would make an EXTEND's recorded length too
        ! short and corrupt the rollback.
        call flush_base_files(db)
        ! Snapshot the in-memory row counters so rollback can revert them: the
        ! journal restores files, but next_id/live_count/blob_next live only in
        ! memory between header writes.
        call snapshot_counters(db)
        ! From here, every index-tree page write funnels its pre-image into the
        ! undo set via the installed hook.
        call install_index_hooks(db)
    end subroutine

    module subroutine jrnl_log_region(db, path, offset, length, bytes, stat)
        class(db_t),       intent(inout)         :: db
        character(len=*),  intent(in)            :: path
        integer(int64),    intent(in)            :: offset, length
        character(len=*),  intent(in),  optional :: bytes
        integer,           intent(out), optional :: stat
        type(undo_rec_t) :: r
        integer          :: ios
        integer(int64)   :: rlen
        if (present(stat)) stat = SQR_OK
        if (.not. db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        ! A caller-supplied pre-image dictates the region length; otherwise the
        ! requested length governs both the idempotency key and the re-read.
        if (present(bytes)) then
            rlen = int(len(bytes), int64)
        else
            rlen = length
        end if
        ! Idempotent: an identical region already captured this txn is a no-op.
        if (have_rec(db%jrnl, UNDO_REGION, trim(path), offset, rlen)) return
        r%kind   = UNDO_REGION
        r%path   = trim(path)
        r%offset = offset
        r%length = rlen
        if (present(bytes)) then
            r%bytes = bytes
        else
            call read_region(pathjoin(db%dir, trim(path)), offset, rlen, r%bytes, ios)
            call io_check(ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if
        end if
        r%orig_len = file_len(pathjoin(db%dir, trim(path)))
        call append_rec(db%jrnl, r)
        ! Write-ahead: make this undo image durable before the caller performs
        ! the base write it is about to do.  A failure aborts the write.
        call txn_arm(db, stat)
    end subroutine

    module subroutine jrnl_log_extend(db, path, stat)
        class(db_t),      intent(inout)         :: db
        character(len=*), intent(in)            :: path
        integer,          intent(out), optional :: stat
        type(undo_rec_t) :: r
        if (present(stat)) stat = SQR_OK
        if (.not. db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        if (have_rec(db%jrnl, UNDO_EXTEND, trim(path), 0_int64, 0_int64)) return
        r%kind     = UNDO_EXTEND
        r%path     = trim(path)
        r%orig_len = file_len(pathjoin(db%dir, trim(path)))
        r%bytes    = ''
        call append_rec(db%jrnl, r)
        ! Write-ahead: persist the captured length before the caller grows the
        ! file, so a crash can truncate the appended bytes away.
        call txn_arm(db, stat)
    end subroutine

    module subroutine txn_arm(db, stat)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        character(len=:), allocatable :: payload
        integer                       :: u, ios, i, st
        logical                       :: fresh
        st = SQR_OK
        if (.not. db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        ! Lazily create + pre-size the journal on the first txn of the session.
        fresh = .false.
        if (.not. db%jrnl%sized) then
            call presize(db, st)
            if (st /= SQR_OK) then
                if (present(stat)) stat = st
                return
            end if
            fresh = .true.
        end if
        ! Serialise the undo set into one buffer (so the checksum and the
        ! single positioned write stay consistent).
        payload = ''
        do i = 1, db%jrnl%nrec
            call append_serialised(payload, db%jrnl%recs(i))
        end do
        open(newunit=u, file=db%jrnl%path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        call io_check(ios)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        if (len(payload) > 0) then
            write(u, pos=JHEADER + 1, iostat=ios) payload
            call io_check(ios)
        end if
        if (ios == 0) &
            write(u, pos=1, iostat=ios) JMAGIC, JFMT, JSTATE_HOT, db%jrnl%nrec, &
                                        checksum(payload), int(len(payload), int64)
        call io_check(ios)
        if (ios == 0) flush(u)
        close(u)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        ios = c_fsync_path(db%jrnl%path)
        call io_check(ios)
        if (ios /= 0) st = SQR_ERR
        if (fresh) then
            ios = c_fsync_dir(db%dir)        ! make the new file durable
            call io_check(ios)
            if (ios /= 0) st = SQR_ERR
        end if
        ! Latch armed once a hot header has been written: re-arm is called
        ! before every base write, and a later re-arm failure does not un-hot
        ! the file, so clean commit/rollback must still void it.  (Crash
        ! recovery reads the disk, not this flag — see jrnl_recover.)
        if (ios == 0) db%jrnl%armed = .true.
        if (present(stat)) stat = st
    end subroutine

    module subroutine txn_commit(db, stat)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        integer :: st, rst
        if (.not. db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        st = SQR_OK
        ! Clear hooks first: the committed files are the new truth, so the cached
        ! tree state already matches disk — no reload needed.
        call clear_index_hooks(db, reload=.false., stat=st)
        if (db%jrnl%armed) then
            ! Durability barrier: force every modified base file to stable
            ! storage *before* the commit point, so a crash just after the
            ! header is voided cannot lose committed data.
            call fsync_base_files(db, st)
            ! The commit point is voiding the journal header: until that lands,
            ! the transaction is not committed.  Voiding only after the data is
            ! durable means a crash before this point recovers (rolls back) and
            ! after it the data is safe.
            if (st == SQR_OK) call void_header(db, st)
            if (st /= SQR_OK) then
                ! Either the data could not be made durable or the commit point
                ! could not be reached: roll the whole transaction back rather
                ! than report a false success.  The journal is still hot here,
                ! so even if this in-process unwind cannot finish, db_open will
                ! recover to the pre-txn state.
                rst = SQR_OK
                call apply_rollback(db, rst)
            end if
        end if
        call clear_txn_state(db)
        if (present(stat)) stat = st
    end subroutine

    module subroutine txn_rollback(db, stat)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        integer :: st
        if (.not. db%jrnl%active) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        st = SQR_OK
        call apply_rollback(db, st)
        call clear_txn_state(db)
        if (present(stat)) stat = st
    end subroutine

    ! Undo every captured region/extend in reverse, revert the in-memory
    ! counters, resync the open base units, reload the index trees from the
    ! restored files, and void the (hot) journal header.  Shared by
    ! txn_rollback and by a commit that fails its durability barrier; does not
    ! clear the transaction-active state — the caller does that.
    subroutine apply_rollback(db, st)
        class(db_t), intent(inout) :: db
        integer,     intent(inout) :: st
        integer :: i
        ! Drain any dirty buffer in the open data/blob units to disk first.  Two
        ! reasons: apply_undo overwrites disk through its own units, and the
        ! later resync closes these units — on a compiler that buffers record
        ! writes that close would otherwise flush the rolled-forward bytes back,
        ! clobbering the bytes apply_undo had just restored.
        call flush_base_files(db)
        ! Reverse order so an earlier capture of a region wins over a later one.
        do i = db%jrnl%nrec, 1, -1
            call apply_undo(db, db%jrnl%recs(i), st)
        end do
        ! The base files are back to their pre-txn bytes; revert the in-memory
        ! row counters to match before any later operation trusts them.
        call restore_counters(db)
        ! ...then reopen the data/blob units so no buffer still serves the
        ! rolled-forward bytes the undo has just removed from disk (the record
        ! analogue of the bt_reload below).  Must follow restore_counters:
        ! open_data only ever raises next_id, so the snapshot has to be in place.
        call resync_base_files(db, st)
        ! The index files are back to their pre-txn bytes; the open tree handles
        ! still cache the rolled-forward meta, so reload each before clearing.
        call clear_index_hooks(db, reload=.true., stat=st)
        if (db%jrnl%armed) call void_header(db, st)
    end subroutine

    ! Tear down the in-memory transaction state after a commit or rollback.
    subroutine clear_txn_state(db)
        class(db_t), intent(inout) :: db
        db%jrnl%active   = .false.
        db%jrnl%explicit = .false.
        db%jrnl%armed    = .false.
        db%jrnl%nrec     = 0
        if (allocated(db%jrnl%recs))  deallocate(db%jrnl%recs)
        if (allocated(db%jrnl%snaps)) deallocate(db%jrnl%snaps)
    end subroutine

    module subroutine jrnl_recover(db, stat)
        class(db_t), intent(inout)        :: db
        integer,    intent(out), optional :: stat
        type(undo_rec_t), allocatable :: recs(:)
        character(len=:), allocatable :: payload
        character(len=4) :: magic
        integer          :: u, ios, fmt, state, nrec, cksum, i, st
        integer(int64)   :: plen
        st = SQR_OK
        if (.not. allocated(db%jrnl%path)) &
            db%jrnl%path = pathjoin(db%dir, '_journal.dat')
        if (.not. c_path_exists(db%jrnl%path)) then
            if (present(stat)) stat = SQR_OK
            return
        end if
        open(newunit=u, file=db%jrnl%path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        read(u, pos=1, iostat=ios) magic, fmt, state, nrec, cksum, plen
        if (ios /= 0 .or. magic /= JMAGIC .or. fmt /= JFMT &
                     .or. state /= JSTATE_HOT) then
            close(u)                       ! absent/void/foreign -> nothing to do
            if (present(stat)) stat = SQR_OK
            return
        end if
        allocate(character(len=int(plen)) :: payload)
        if (plen > 0) read(u, pos=JHEADER + 1, iostat=ios) payload
        close(u)
        ! A torn or mismatched payload means arming never completed, so no base
        ! file was touched: discard the journal rather than replay garbage.
        if (ios /= 0 .or. checksum(payload) /= cksum) then
            call void_header(db, st)
            if (present(stat)) stat = st
            return
        end if
        call deserialise(payload, nrec, recs, ios)
        if (ios /= 0) then
            call void_header(db, st)
            if (present(stat)) stat = st
            return
        end if
        do i = nrec, 1, -1               ! replay undo in reverse
            call apply_undo(db, recs(i), st)
        end do
        call void_header(db, st)         ! invalidate once the base files are sound
        if (present(stat)) stat = st
    end subroutine

    module function jrnl_hot(db) result(hot)
        class(db_t), intent(in) :: db
        logical                 :: hot
        character(len=:), allocatable :: jpath
        character(len=4) :: magic
        integer          :: u, ios, fmt, state
        hot = .false.
        ! Probe only — never mutates db (db%jrnl%path may not be set yet).
        if (allocated(db%jrnl%path)) then
            jpath = db%jrnl%path
        else
            jpath = pathjoin(db%dir, '_journal.dat')
        end if
        if (.not. c_path_exists(jpath)) return
        open(newunit=u, file=jpath, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(u, pos=1, iostat=ios) magic, fmt, state
        close(u)
        hot = (ios == 0 .and. magic == JMAGIC .and. fmt == JFMT &
                         .and. state == JSTATE_HOT)
    end function

    ! ---- base-file flush (txn lifecycle) ----

    ! Flush every open data and blob unit so the on-disk base files reflect all
    ! committed writes before the journal captures pre-images and extents.  An
    ! open unit's sentinel is -1 (newunit hands out negative units, so a "> 0"
    ! test would be wrong).
    subroutine flush_base_files(db)
        class(db_t), intent(in) :: db
        integer :: ti
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                if (t%unit      /= -1) flush(t%unit)
                if (t%blob_unit /= -1) flush(t%blob_unit)
            end associate
        end do
    end subroutine

    ! Force every base file this transaction modified to stable storage — the
    ! commit durability barrier.  First drain each open unit's buffer to the OS
    ! (data and blob units, then every live index tree via bt_sync), then fsync
    ! each distinct base file named by the undo records.  The first failure is
    ! reported so commit rolls back rather than voiding the journal over data
    ! that never reached the disk.
    subroutine fsync_base_files(db, st)
        class(db_t), intent(inout) :: db
        integer,     intent(inout) :: st
        integer :: i, k, ios, ti, j, bs
        logical :: dup
        call flush_base_files(db)
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (idx_live(t%indices(j))) call bt_sync(t%indices(j)%bt, bs)
                end do
            end associate
        end do
        do i = 1, db%jrnl%nrec
            dup = .false.
            dedup: do k = 1, i - 1
                if (db%jrnl%recs(k)%path == db%jrnl%recs(i)%path) then
                    dup = .true.
                    exit dedup
                end if
            end do dedup
            if (dup) cycle
            ios = c_fsync_path(pathjoin(db%dir, db%jrnl%recs(i)%path))
            call io_check(ios)
            if (ios /= 0) st = SQR_ERR
        end do
    end subroutine

    ! Reopen each table's data and blob units after a rollback so no unit buffer
    ! still holds rolled-forward bytes that apply_undo has just removed from
    ! disk.  Reusing open_data/open_blob also re-derives blob_next from the
    ! restored blob size; next_id/live_count come from the snapshot, since
    ! open_data only ever raises next_id (recovered == next_id here, so its
    ! crash-recovery branch is a no-op).
    subroutine resync_base_files(db, stat)
        class(db_t), intent(inout) :: db
        integer,     intent(inout) :: stat
        integer :: ti, st
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                if (t%unit /= -1) then
                    close(t%unit)
                    t%unit = -1
                    call open_data(db, t, 'old', st)
                    if (st /= SQR_OK .and. stat == SQR_OK) stat = st
                end if
                if (t%blob_unit /= -1) then
                    close(t%blob_unit)
                    t%blob_unit = -1
                    call open_blob(db, t, 'old', st)
                    if (st /= SQR_OK .and. stat == SQR_OK) stat = st
                end if
            end associate
        end do
    end subroutine

    ! ---- per-table counter snapshot / restore (txn lifecycle) ----

    ! Record each table's in-memory row counters at txn_begin, indexed by table
    ! position.  v1 forbids table create/drop inside a transaction, so the
    ! position mapping holds for the txn's life.
    subroutine snapshot_counters(db)
        class(db_t), intent(inout) :: db
        integer :: ti
        if (allocated(db%jrnl%snaps)) deallocate(db%jrnl%snaps)
        allocate(db%jrnl%snaps(db%ntables))
        do ti = 1, db%ntables
            associate (t => db%tables(ti), s => db%jrnl%snaps(ti))
                s%next_id    = t%next_id
                s%live_count = t%live_count
                s%blob_next  = t%blob_next
            end associate
        end do
    end subroutine

    ! Roll the counters back to their snapshot after the files are restored.
    subroutine restore_counters(db)
        class(db_t), intent(inout) :: db
        integer :: ti
        if (.not. allocated(db%jrnl%snaps)) return
        do ti = 1, min(db%ntables, size(db%jrnl%snaps))
            associate (t => db%tables(ti), s => db%jrnl%snaps(ti))
                t%next_id    = s%next_id
                t%live_count = s%live_count
                t%blob_next  = s%blob_next
            end associate
        end do
    end subroutine

    ! ---- index-tree hook install / clear (txn lifecycle) ----

    ! Install the rollback-journal pre-write hook on every live index tree of an
    ! open writable db so b_tree page writes during the txn funnel pre-images
    ! into the undo set.  Each tree gets its own heap-allocated bt_jhook_ctx_t,
    ! owned by the index slot (freed in clear_index_hooks), naming this db and
    ! the tree file relative to db%dir.  db is TARGET so the context can hold a
    ! lasting pointer back to it once txn_begin returns.
    subroutine install_index_hooks(db)
        class(db_t), intent(inout), target :: db
        integer           :: ti, j
        class(*), pointer :: cp
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle
                    if (associated(t%indices(j)%jctx)) deallocate(t%indices(j)%jctx)
                    allocate(bt_jhook_ctx_t :: t%indices(j)%jctx)
                    select type (c => t%indices(j)%jctx)
                    type is (bt_jhook_ctx_t)
                        c%db  => db
                        c%rel =  index_relpath(t%name, j)
                    end select
                    cp => t%indices(j)%jctx
                    call bt_set_journal_hook(t%indices(j)%bt, bt_journal_adapter, cp)
                end do
            end associate
        end do
    end subroutine

    ! Remove the hook from every live index tree and free its context.  On
    ! rollback (reload=.true.) the on-disk index files have just been restored,
    ! so each open tree handle is re-synced from its meta page before the hook
    ! goes — a tree left caching the rolled-forward root/high-water would operate
    ! on phantom pages.  A bt_reload failure is reported but never aborts the
    ! unwind (the disk state is already correct).
    subroutine clear_index_hooks(db, reload, stat)
        class(db_t), intent(inout) :: db
        logical,     intent(in)    :: reload
        integer,     intent(inout) :: stat
        integer :: ti, j, rst
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle
                    call bt_set_journal_hook(t%indices(j)%bt)   ! clears hook + ctx ptr
                    if (reload) then
                        call bt_reload(t%indices(j)%bt, rst)
                        if (rst == BT_OK) then
                            t%indices(j)%nentries = int(t%indices(j)%bt%nentries)
                        else if (stat == SQR_OK) then
                            stat = SQR_ERR
                        end if
                    end if
                    if (associated(t%indices(j)%jctx)) deallocate(t%indices(j)%jctx)
                end do
            end associate
        end do
    end subroutine

    ! Bridge a b_tree pre-write hook to the rollback journal.  `ctx` carries the
    ! db whose journal records the undo and the tree's path relative to db%dir.
    ! A new page logs an extend (rollback truncates it away); an in-place
    ! overwrite logs the region using the tree's own pre-image bytes, so a write
    ! still buffered in the tree's unit is captured correctly rather than
    ! re-read.  Any journal failure (or a foreign context) returns non-zero,
    ! which aborts the page write before an un-recorded overwrite can land.
    module subroutine bt_journal_adapter(ctx, offset, old_bytes, is_new, stat)
        class(*),         intent(in)  :: ctx
        integer(int64),   intent(in)  :: offset
        character(len=*), intent(in)  :: old_bytes
        logical,          intent(in)  :: is_new
        integer,          intent(out) :: stat
        integer :: sst
        stat = 0
        select type (ctx)
        type is (bt_jhook_ctx_t)
            if (is_new) then
                call jrnl_log_extend(ctx%db, ctx%rel, sst)
            else
                call jrnl_log_region(ctx%db, ctx%rel, offset, 0_int64, &
                                     bytes=old_bytes, stat=sst)
            end if
            if (sst /= SQR_OK) stat = 1
        class default
            stat = 1   ! wrong context type wired in — refuse the write
        end select
    end subroutine

    ! ---- private helpers ----

    ! Restore one undo record onto its base file and fsync it.
    subroutine apply_undo(db, r, st)
        class(db_t),      intent(in)    :: db
        type(undo_rec_t), intent(in)    :: r
        integer,          intent(inout) :: st
        character(len=:), allocatable :: full
        integer :: ios
        full = pathjoin(db%dir, r%path)
        select case (r%kind)
        case (UNDO_REGION)
            call write_region(full, r%offset, r%bytes, ios)
            call io_check(ios)
            if (ios /= 0) st = SQR_ERR
        case (UNDO_EXTEND)
            ios = c_truncate(full, r%orig_len)
            call io_check(ios)
            if (ios /= 0) st = SQR_ERR
        end select
        ios = c_fsync_path(full)
        call io_check(ios)
        if (ios /= 0) st = SQR_ERR
    end subroutine

    ! Create the journal file and pre-write it to JPRESIZE zero bytes, so a
    ! bounded transaction never changes the file's length (commit needs only a
    ! data fsync, never a size-change + directory fsync).
    subroutine presize(db, st)
        class(db_t), intent(inout) :: db
        integer,     intent(inout) :: st
        character(len=:), allocatable :: zeros
        integer :: u, ios
        allocate(character(len=int(JPRESIZE)) :: zeros)
        zeros = repeat(char(0), int(JPRESIZE))
        open(newunit=u, file=db%jrnl%path, access='stream', form='unformatted', &
             status='replace', action='readwrite', iostat=ios)
        call io_check(ios)
        if (ios /= 0) then
            st = SQR_ERR
            return
        end if
        write(u, pos=1, iostat=ios) zeros
        call io_check(ios)
        if (ios == 0) flush(u)
        close(u)
        if (ios /= 0) then
            st = SQR_ERR
            return
        end if
        db%jrnl%capacity = JPRESIZE
        db%jrnl%sized    = .true.
    end subroutine

    ! Write a JSTATE_VOID header + fsync — the durable invalidation used at
    ! commit, on rollback, and after recovery.
    subroutine void_header(db, st)
        class(db_t), intent(in)    :: db
        integer,     intent(inout) :: st
        integer :: u, ios
        open(newunit=u, file=db%jrnl%path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        call io_check(ios)
        if (ios /= 0) then
            st = SQR_ERR
            return
        end if
        write(u, pos=1, iostat=ios) JMAGIC, JFMT, JSTATE_VOID, 0, 0, 0_int64
        call io_check(ios)
        if (ios == 0) flush(u)
        close(u)
        if (ios /= 0) then
            st = SQR_ERR
            return
        end if
        ios = c_fsync_path(db%jrnl%path)
        call io_check(ios)
        if (ios /= 0) st = SQR_ERR
    end subroutine

    ! True if an equal undo record was already captured this transaction.
    pure logical function have_rec(j, kind, path, offset, length) result(yes)
        type(journal_t), intent(in) :: j
        integer,         intent(in) :: kind
        character(len=*),intent(in) :: path
        integer(int64),  intent(in) :: offset, length
        integer :: i
        yes = .false.
        do i = 1, j%nrec
            if (j%recs(i)%kind == kind .and. j%recs(i)%path == path .and. &
                j%recs(i)%offset == offset .and. j%recs(i)%length == length) then
                yes = .true.
                return
            end if
        end do
    end function

    ! Append one undo record, growing the backing array as needed.
    subroutine append_rec(j, r)
        type(journal_t), intent(inout) :: j
        type(undo_rec_t),intent(in)    :: r
        type(undo_rec_t), allocatable :: tmp(:)
        if (j%nrec == size(j%recs)) then
            allocate(tmp(max(4, 2 * j%nrec)))
            tmp(1:j%nrec) = j%recs(1:j%nrec)
            call move_alloc(tmp, j%recs)
        end if
        j%nrec = j%nrec + 1
        j%recs(j%nrec) = r
    end subroutine

    ! Serialise one record onto the payload buffer.
    subroutine append_serialised(buf, r)
        character(len=:), allocatable, intent(inout) :: buf
        type(undo_rec_t),              intent(in)    :: r
        integer(int64) :: blen
        blen = 0_int64
        if (r%kind == UNDO_REGION) blen = int(len(r%bytes), int64)
        buf = buf // i32b(r%kind) // i32b(len(r%path)) // r%path // &
              i64b(r%orig_len) // i64b(r%offset) // i64b(r%length) // i64b(blen)
        if (blen > 0) buf = buf // r%bytes
    end subroutine

    ! Parse nrec records back out of a payload buffer.
    subroutine deserialise(buf, nrec, recs, ios)
        character(len=*),              intent(in)  :: buf
        integer,                       intent(in)  :: nrec
        type(undo_rec_t), allocatable, intent(out) :: recs(:)
        integer,                       intent(out) :: ios
        integer        :: i, p, pathlen
        integer(int64) :: blen
        ios = 0
        allocate(recs(nrec))
        p = 1
        do i = 1, nrec
            if (p + 7 > len(buf)) then; ios = 1; return; end if
            recs(i)%kind = b32(buf(p:p+3));     p = p + 4
            pathlen      = b32(buf(p:p+3));      p = p + 4
            if (pathlen < 0 .or. p + pathlen - 1 > len(buf)) then; ios = 1; return; end if
            recs(i)%path = buf(p:p+pathlen-1);   p = p + pathlen
            if (p + 23 > len(buf)) then; ios = 1; return; end if
            recs(i)%orig_len = b64(buf(p:p+7));  p = p + 8
            recs(i)%offset   = b64(buf(p:p+7));  p = p + 8
            recs(i)%length   = b64(buf(p:p+7));  p = p + 8
            blen             = b64(buf(p:p+7));  p = p + 8
            if (blen < 0 .or. p + blen - 1 > len(buf)) then; ios = 1; return; end if
            if (blen > 0) then
                recs(i)%bytes = buf(p:p+int(blen)-1); p = p + int(blen)
            else
                recs(i)%bytes = ''
            end if
        end do
    end subroutine

    ! ---- byte (de)serialisation of scalars (native order) ----

    pure function i32b(i) result(s)
        integer, intent(in) :: i
        character(len=4) :: s
        s = transfer(int(i, int32), s)
    end function

    pure function i64b(i) result(s)
        integer(int64), intent(in) :: i
        character(len=8) :: s
        s = transfer(i, s)
    end function

    pure integer function b32(s) result(i)
        character(len=*), intent(in) :: s
        integer(int32) :: v
        v = transfer(s(1:4), v)
        i = int(v)
    end function

    pure integer(int64) function b64(s) result(i)
        character(len=*), intent(in) :: s
        i = transfer(s(1:8), i)
    end function

    ! Order-sensitive rolling checksum over the record payload.
    pure integer function checksum(buf) result(c)
        character(len=*), intent(in) :: buf
        integer(int64) :: acc
        integer        :: i
        acc = 0_int64
        do i = 1, len(buf)
            acc = mod(acc * 31_int64 + iachar(buf(i:i)), 2147483647_int64)
        end do
        c = int(acc)
    end function

    ! ---- raw byte file IO ----

    integer(int64) function file_len(path) result(n)
        character(len=*), intent(in) :: path
        integer :: ios
        inquire(file=path, size=n, iostat=ios)
        if (ios /= 0) n = -1_int64
    end function

    subroutine read_region(path, offset, length, bytes, ios)
        character(len=*),              intent(in)  :: path
        integer(int64),                intent(in)  :: offset, length
        character(len=:), allocatable, intent(out) :: bytes
        integer,                       intent(out) :: ios
        integer :: u
        allocate(character(len=int(length)) :: bytes)
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(u, pos=offset, iostat=ios) bytes
        close(u)
    end subroutine

    subroutine write_region(path, offset, bytes, ios)
        character(len=*), intent(in)  :: path
        integer(int64),   intent(in)  :: offset
        character(len=*), intent(in)  :: bytes
        integer,          intent(out) :: ios
        integer :: u
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        if (ios /= 0) return
        write(u, pos=offset, iostat=ios) bytes
        if (ios == 0) flush(u)
        close(u)
    end subroutine

end submodule sqr_journal

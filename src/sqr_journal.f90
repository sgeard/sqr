! sqr_journal — the rollback (undo) journal for crash-safe atomicity, and the
! in-memory Undo/Redo history built on its capture (see the history section near
! the end of the file).
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
    use :: clib_wrap, only: c_truncate, c_remove   ! c_fsync_path/c_fsync_dir host-associated from sqr_base
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

    module subroutine db_begin(db, stat, label)
        class(db_t), intent(inout), target :: db
        integer,    intent(out), optional  :: stat
        character(len=*), intent(in), optional :: label
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
        ! A labelled begin is a user gesture: latch the intent so the matching
        ! commit records one Undo/Redo step.  txn_begin has just snapshotted the
        ! pre-gesture counters into jrnl%snaps (the step's `before` image).
        db%hist%capturing = present(label)
        if (present(label)) db%hist%pending_label = label
    end subroutine

    module subroutine db_commit(db, stat)
        class(db_t), intent(inout) :: db
        integer,    intent(out), optional :: stat
        integer :: cst
        if (present(stat)) stat = SQR_OK
        if (.not. (db%jrnl%active .and. db%jrnl%explicit)) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        ! Snapshot this gesture as one Undo step BEFORE txn_commit discards the
        ! in-memory undo set (jrnl%recs) and counters.  A no-op gesture (no
        ! recs) records nothing, so an empty bracket stays invisible to Undo.
        ! A capture failure only costs the (session-only) history — capture_step
        ! discards it and resets to empty; the commit's own durability stands, so
        ! db_commit still returns txn_commit's status.
        if (db%hist%capturing) call capture_step(db, cst)
        db%hist%capturing = .false.
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
        call reset_jrnl_arm_state(db%jrnl)   ! dedup hash + incremental-arm trio
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
        character(len=:), allocatable :: newbytes
        integer                       :: u, ios, i, st, ck
        integer(int64)                :: newplen
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
        ! Incremental arm: serialise ONLY the records not yet durable.  The file
        ! already holds the first plen_durable bytes (an immutable prefix — records
        ! never change once appended), so we append the new suffix beyond them.
        call serialise_range(db%jrnl, db%jrnl%nrec_durable + 1, db%jrnl%nrec, newbytes)
        newplen = db%jrnl%plen_durable + int(len(newbytes), int64)
        ck      = checksum_fold(db%jrnl%cksum_acc, newbytes)   ! extends the prefix's checksum
        open(newunit=u, file=db%jrnl%path, access='stream', form='unformatted', &
             status='old', action='readwrite', iostat=ios)
        call io_check(ios)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        ! Phase 1 — make the appended payload durable BEFORE the header names it.
        ! Appends land STRICTLY BEYOND plen_durable and the prefix is never
        ! rewritten, so a crash after this fsync but before the header lands leaves
        ! the OLD header (old nrec/plen/cksum) over an untouched valid prefix:
        ! recovery reads only that prefix, its checksum matches, every earlier undo
        ! record survives.  ("torn payload => arming never completed", every arm.)
        if (len(newbytes) > 0) then
            write(u, pos=JHEADER + 1 + db%jrnl%plen_durable, iostat=ios) newbytes
            call io_check(ios)
        end if
        if (ios == 0) flush(u, iostat=ios)
        call io_check(ios)
        if (ios == 0) then
            ios = c_fsync_path(db%jrnl%path)     ! payload durable
            call io_check(ios)
        end if
        if (ios /= 0) then
            close(u, iostat=i)
            if (present(stat)) stat = SQR_ERR
            return
        end if
        ! Phase 2 — the header may now safely point at the durable payload.
        write(u, pos=1, iostat=ios) JMAGIC, JFMT, JSTATE_HOT, db%jrnl%nrec, &
                                    ck, newplen
        call io_check(ios)
        if (ios == 0) flush(u, iostat=ios)
        call io_check(ios)
        close(u, iostat=i)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        ios = c_fsync_path(db%jrnl%path)         ! header durable
        call io_check(ios)
        if (ios /= 0) st = SQR_ERR
        ! The payload+header bytes are now durable, so the trio may advance to name
        ! them (the dir fsync below only affects the fresh file's directory entry,
        ! not these bytes).  Reached only on a clean header fsync; any earlier
        ! failure took an early return with the trio untouched, so the next arm
        ! re-serialises from nrec_durable+1 and rewrites the same immutable bytes.
        if (ios == 0) then
            db%jrnl%plen_durable = newplen
            db%jrnl%nrec_durable = db%jrnl%nrec
            db%jrnl%cksum_acc    = ck
        end if
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
                ! Surface a failed unwind rather than masking it as a clean
                ! commit failure: if the rollback could not finish, the journal
                ! is left hot (apply_rollback only voids on success) and db_open
                ! will recover, but the caller must still see the error.
                if (rst /= SQR_OK) st = rst
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
        call flush_base_files(db, st)
        ! Same for the index trees: clear_index_hooks below reopens each by
        ! close+open, so any page write still buffered in a tree's unit must
        ! reach disk before apply_undo restores the file — otherwise that close
        ! flushes the rolled-forward pages back over the restored bytes (surfaces
        ! only on a compiler that buffers direct-access writes, and only when the
        ! txn split a tree).
        call flush_index_trees(db, st)
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
        ! Void only on a clean unwind.  A failed undo/resync (transient EIO,
        ! ENOSPC) must leave the journal hot so the next open re-applies the
        ! (idempotent) undo records rather than serving a half-restored file.
        if (db%jrnl%armed .and. st == SQR_OK) call void_header(db, st)
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
        call reset_jrnl_arm_state(db%jrnl)
    end subroutine

    ! Reset the per-txn dedup hash (E2) and the incremental-arm trio (E1) — the
    ! trio must describe fsynced bytes only, so it starts at zero for every txn
    ! (and after presize recreates the file).
    subroutine reset_jrnl_arm_state(jrnl)
        type(journal_t), intent(inout) :: jrnl
        if (allocated(jrnl%hslot)) deallocate(jrnl%hslot)
        jrnl%hcount       = 0
        jrnl%plen_durable = 0
        jrnl%nrec_durable = 0
        jrnl%cksum_acc    = 0
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
        ! Guard the payload length before trusting it: it is not covered by the
        ! checksum, int(plen) would truncate a >2 GiB value, and a corrupt huge
        ! plen must not abort db_open inside the allocate.  An implausible or
        ! unallocatable length means the header is corrupt -> void and move on.
        if (plen < 0_int64 .or. plen > int(huge(0), int64)) then
            close(u)
            call void_header(db, st)
            if (present(stat)) stat = st
            return
        end if
        allocate(character(len=int(plen)) :: payload, stat=ios)
        if (ios /= 0) then
            close(u)
            call void_header(db, st)
            if (present(stat)) stat = st
            return
        end if
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
        ! Void only if every undo landed.  A transient I/O error mid-replay
        ! leaves the base file half-restored; keeping the journal hot lets the
        ! next open retry the (idempotent, absolute-write) undo records instead
        ! of destroying the one record that can still repair the database.
        if (st == SQR_OK) call void_header(db, st)
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
    subroutine flush_base_files(db, st)
        class(db_t), intent(in)              :: db
        integer,     intent(inout), optional :: st
        integer :: ti, ios
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                if (t%unit /= -1) then
                    flush(t%unit, iostat=ios)
                    if (ios /= 0 .and. present(st)) st = SQR_ERR
                end if
                if (t%blob_unit /= -1) then
                    flush(t%blob_unit, iostat=ios)
                    if (ios /= 0 .and. present(st)) st = SQR_ERR
                end if
            end associate
        end do
    end subroutine

    ! Drain every open index tree's write buffer to disk — the index analogue of
    ! flush_base_files, used before a rollback restores the files and reopens the
    ! trees (see apply_rollback).
    subroutine flush_index_trees(db, st)
        class(db_t), intent(in)              :: db
        integer,     intent(inout), optional :: st
        integer :: ti, j, bs
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle
                    call bt_sync(t%indices(j)%bt, bs)
                    if (bs /= BT_OK .and. present(st)) st = SQR_ERR
                end do
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
        integer :: i, ios, ti, j, bs
        logical :: dup
        call flush_base_files(db, st)
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (idx_live(t%indices(j))) call bt_sync(t%indices(j)%bt, bs)
                end do
            end associate
        end do
        ! Fsync each distinct base file this txn touched, once.  Compare each
        ! record's path only against the DISTINCT paths already seen (first(:),
        ! bounded by tables*2 + indices) — O(nrec * ndistinct), not the old
        ! all-pairs O(nrec^2).  First-occurrence order and error handling unchanged.
        dedup_block: block
            integer, allocatable :: first(:), tmp(:)
            integer :: nseen, m
            nseen = 0
            allocate(first(8))
            do i = 1, db%jrnl%nrec
                dup = .false.
                seen: do m = 1, nseen
                    if (db%jrnl%recs(first(m))%path == db%jrnl%recs(i)%path) then
                        dup = .true.
                        exit seen
                    end if
                end do seen
                if (dup) cycle
                if (nseen == size(first)) then
                    allocate(tmp(2 * size(first)))
                    tmp(1:nseen) = first(1:nseen)
                    call move_alloc(tmp, first)
                end if
                nseen = nseen + 1
                first(nseen) = i
                ios = c_fsync_path(pathjoin(db%dir, db%jrnl%recs(i)%path))
                call io_check(ios)
                if (ios /= 0) st = SQR_ERR
            end do
        end block dedup_block
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
                    if (reload) then
                        ! The on-disk index was just restored through a different
                        ! unit, so this tree's open unit may cache stale pages or
                        ! a stale EOF — and after a rollback that split the tree,
                        ! its cached root/high-water point past the restored file,
                        ! so a later seek on the same handle walks phantom pages.
                        ! bt_reload re-reads the meta page through that same stale
                        ! unit, so it is not enough (see resync_index_trees); a
                        ! clean close + reopen discards the buffer. bt_open resets
                        ! the handle, which also clears the journal hook.
                        if (t%indices(j)%bt%unit /= -1) then
                            close(t%indices(j)%bt%unit)
                            t%indices(j)%bt%unit = -1
                        end if
                        call open_index(db, t, t%indices(j), j, 'old', rst)
                        if (rst /= SQR_OK .and. stat == SQR_OK) stat = rst
                    else
                        call bt_set_journal_hook(t%indices(j)%bt)   ! clears hook + ctx ptr
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
        if (ios == 0) flush(u, iostat=ios)
        close(u)
        if (ios /= 0) then
            st = SQR_ERR
            return
        end if
        ! The file was just recreated (zeroed), so no payload bytes are durable:
        ! the incremental-arm trio starts from zero.  (The dedup hash is NOT
        ! touched — records already captured before this first arm stay indexed.)
        db%jrnl%plen_durable = 0
        db%jrnl%nrec_durable = 0
        db%jrnl%cksum_acc    = 0
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
        if (ios == 0) flush(u, iostat=ios)
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
    ! Hash of a record's idempotency tuple, masked into [0, mask] (mask is a
    ! power-of-two size minus one).  Bounded polynomial fold (mod 2^31-1) so the
    ! int64 accumulator never overflows — no reliance on wraparound.
    pure integer function rec_hash(kind, path, offset, length, mask) result(h)
        integer,          intent(in) :: kind
        character(len=*), intent(in) :: path
        integer(int64),   intent(in) :: offset, length
        integer,          intent(in) :: mask
        integer(int64), parameter :: M = 2147483647_int64   ! 2^31-1 (Mersenne prime)
        integer(int64) :: acc
        integer :: i
        acc = 2166136261_int64
        do i = 1, len(path)
            acc = modulo(acc * 16777619_int64 + int(iachar(path(i:i)), int64), M)
        end do
        acc = modulo(acc * 16777619_int64 + int(kind, int64),   M)
        acc = modulo(acc * 16777619_int64 + modulo(offset, M),  M)
        acc = modulo(acc * 16777619_int64 + modulo(length, M),  M)
        h = int(iand(acc, int(mask, int64)))
    end function

    ! Open-addressed dedup probe (E2): O(1) replacement for the O(nrec) linear
    ! scan.  Same idempotency tuple (kind, path, offset, length), same "first
    ! capture wins" semantics.  Stays pure — reads intent(in) state only.
    pure logical function have_rec(j, kind, path, offset, length) result(yes)
        type(journal_t), intent(in) :: j
        integer,         intent(in) :: kind
        character(len=*),intent(in) :: path
        integer(int64),  intent(in) :: offset, length
        integer :: h, i, slot, cap
        yes = .false.
        if (.not. allocated(j%hslot)) return          ! no records captured yet
        cap = size(j%hslot)
        h = rec_hash(kind, path, offset, length, cap - 1)
        probe: do i = 0, cap - 1
            slot = iand(h + i, cap - 1) + 1            ! linear probe, 1-based
            if (j%hslot(slot) == 0) return            ! empty run ⇒ not present
            associate (r => j%recs(j%hslot(slot)))
                if (r%kind == kind .and. r%path == path .and. &
                    r%offset == offset .and. r%length == length) then
                    yes = .true.
                    return
                end if
            end associate
        end do probe
    end function

    ! Insert record index `idx` into the dedup hash (caller has ensured the tuple
    ! is not already present — dedup runs before append).
    subroutine hslot_insert(j, idx)
        type(journal_t), intent(inout) :: j
        integer,         intent(in)    :: idx
        integer :: h, i, slot, cap
        cap = size(j%hslot)
        associate (r => j%recs(idx))
            h = rec_hash(r%kind, r%path, r%offset, r%length, cap - 1)
        end associate
        do i = 0, cap - 1
            slot = iand(h + i, cap - 1) + 1
            if (j%hslot(slot) == 0) then
                j%hslot(slot) = idx
                j%hcount = j%hcount + 1
                return
            end if
        end do
    end subroutine

    ! (Re)build the dedup hash at `newcap` slots (power of two) from the existing
    ! records recs(1:nrec-1) — the just-appended recs(nrec) is inserted by the
    ! caller afterwards.  Doubles as the lazy first allocation (nrec == 1).
    subroutine rehash(j, newcap)
        type(journal_t), intent(inout) :: j
        integer,         intent(in)    :: newcap
        integer :: idx
        if (allocated(j%hslot)) deallocate(j%hslot)
        allocate(j%hslot(newcap))
        j%hslot  = 0
        j%hcount = 0
        do idx = 1, j%nrec - 1
            call hslot_insert(j, idx)
        end do
    end subroutine

    ! Append one undo record, growing the backing array and dedup hash as needed.
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
        ! Keep the dedup hash at load < 1/2 (first size 64), then index the row.
        if (.not. allocated(j%hslot)) then
            call rehash(j, 64)
        else if (j%hcount + 1 > size(j%hslot) / 2) then
            call rehash(j, size(j%hslot) * 2)
        end if
        call hslot_insert(j, j%nrec)
    end subroutine

    ! Serialised byte length of one record: the fixed 40-byte header
    ! (kind + pathlen + orig_len + offset + length + byteslen) plus path + bytes.
    pure integer(int64) function rec_serial_len(r) result(n)
        type(undo_rec_t), intent(in) :: r
        integer(int64) :: blen
        blen = 0_int64
        if (r%kind == UNDO_REGION) blen = int(len(r%bytes), int64)
        n = 40_int64 + int(len(r%path), int64) + blen
    end function

    ! Write one serialised record into buf starting at 1-based position p,
    ! advancing p past it.  Field order mirrors deserialise exactly.
    subroutine put_rec(buf, p, r)
        character(len=*), intent(inout) :: buf
        integer,          intent(inout) :: p
        type(undo_rec_t), intent(in)    :: r
        integer(int64) :: blen
        integer :: plen
        blen = 0_int64
        if (r%kind == UNDO_REGION) blen = int(len(r%bytes), int64)
        plen = len(r%path)
        buf(p:p+3) = i32b(r%kind);                     p = p + 4
        buf(p:p+3) = i32b(plen);                       p = p + 4
        if (plen > 0) then; buf(p:p+plen-1) = r%path;  p = p + plen; end if
        buf(p:p+7) = i64b(r%orig_len);                 p = p + 8
        buf(p:p+7) = i64b(r%offset);                   p = p + 8
        buf(p:p+7) = i64b(r%length);                   p = p + 8
        buf(p:p+7) = i64b(blen);                       p = p + 8
        if (blen > 0) then; buf(p:p+int(blen)-1) = r%bytes; p = p + int(blen); end if
    end subroutine

    ! Serialise records recs(lo:hi) into a single freshly-allocated buffer (one
    ! allocation, positioned copies — no per-record concat).  Empty when lo > hi.
    subroutine serialise_range(j, lo, hi, buf)
        type(journal_t),               intent(in)  :: j
        integer,                       intent(in)  :: lo, hi
        character(len=:), allocatable, intent(out) :: buf
        integer(int64) :: total
        integer :: i, p
        total = 0_int64
        do i = lo, hi
            total = total + rec_serial_len(j%recs(i))
        end do
        allocate(character(len=int(total)) :: buf)
        p = 1
        do i = lo, hi
            call put_rec(buf, p, j%recs(i))
        end do
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
    ! checksum() lives in the parent sqr_base (host-associated here) so the
    ! pack/unpack codec can share the one definition.

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
        if (ios == 0) flush(u, iostat=ios)
        close(u)
    end subroutine

    ! ===== in-memory Undo/Redo history (built on the journal's capture) =====
    ! A committed gesture's undo set (jrnl%recs) is exactly the inverse image an
    ! Undo needs.  capture_step turns it (plus the post-gesture bytes read back
    ! from the flushed files) into one bidirectional hist_step_t; db_undo/db_redo
    ! write the chosen side's bytes and reuse the rollback path's coherence dance
    ! (restore counters, reopen units, reload index trees) to bring the in-memory
    ! db back into step.  Session-only: never serialised, never crash-durable.

    module subroutine db_undo(db, stat, label)
        class(db_t), intent(inout) :: db
        integer,     intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: label
        type(hist_step_t) :: step
        integer :: st
        st = SQR_OK
        if (present(label)) label = ''
        if (db%readonly) then
            if (present(stat)) stat = SQR_READONLY
            return
        end if
        if (.not. db_can_undo(db)) then
            if (present(stat)) stat = SQR_NO_UNDO
            return
        end if
        call pop_step(db%hist%undo, step)
        call apply_step(db, step, redo=.false., st=st)
        if (present(label) .and. allocated(step%label)) label = step%label
        call push_step(db%hist%redo, step, db%hist%cap)   ! empties step
        if (present(stat)) stat = st
    end subroutine

    module subroutine db_redo(db, stat, label)
        class(db_t), intent(inout) :: db
        integer,     intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: label
        type(hist_step_t) :: step
        integer :: st
        st = SQR_OK
        if (present(label)) label = ''
        if (db%readonly) then
            if (present(stat)) stat = SQR_READONLY
            return
        end if
        if (.not. db_can_redo(db)) then
            if (present(stat)) stat = SQR_NO_UNDO
            return
        end if
        call pop_step(db%hist%redo, step)
        call apply_step(db, step, redo=.true., st=st)
        if (present(label) .and. allocated(step%label)) label = step%label
        call push_step(db%hist%undo, step, db%hist%cap)   ! empties step
        if (present(stat)) stat = st
    end subroutine

    pure module function db_can_undo(db) result(yes)
        class(db_t), intent(in) :: db
        logical :: yes
        yes = db%hist%undo%count > 0
    end function

    pure module function db_can_redo(db) result(yes)
        class(db_t), intent(in) :: db
        logical :: yes
        yes = db%hist%redo%count > 0
    end function

    module subroutine db_reset_history(db)
        class(db_t), intent(inout) :: db
        call reset_stack(db%hist%undo)
        call reset_stack(db%hist%redo)
        if (allocated(db%hist%pending_label)) deallocate(db%hist%pending_label)
        db%hist%capturing = .false.
    end subroutine

    ! Snapshot the committed gesture as one bidirectional history step and push
    ! it onto the undo stack (clearing the redo stack — a new gesture forks the
    ! timeline).  Must run BEFORE txn_commit discards jrnl%recs/snaps.  Reads the
    ! post-gesture ("redo") bytes from disk, so flush every base unit AND sync
    ! the index trees first (fsync_base_files does the tree sync at commit, but
    ! that is after this point).
    subroutine capture_step(db, stat)
        class(db_t), intent(inout) :: db
        integer,     intent(out)   :: stat
        type(hist_step_t) :: step
        integer :: ti, j, bs
        stat = SQR_OK
        if (db%jrnl%nrec == 0) return          ! a gesture that changed nothing
        call flush_base_files(db)
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (idx_live(t%indices(j))) call bt_sync(t%indices(j)%bt, bs)
                end do
            end associate
        end do
        call build_deltas(db, step%deltas, stat)
        if (stat /= SQR_OK) then
            ! The gesture is durable, but its post-images could not be read back,
            ! so this step would be incomplete AND the steps already stacked can
            ! no longer be trusted against the new on-disk state.  A wrong Undo is
            ! worse than none: drop history entirely (coherent-or-empty invariant).
            call db_reset_history(db)
            return
        end if
        if (allocated(db%jrnl%snaps)) step%before = db%jrnl%snaps
        step%after = snapshot_now(db)
        if (allocated(db%hist%pending_label)) then
            step%label = db%hist%pending_label
        else
            step%label = ''
        end if
        call reset_stack(db%hist%redo)         ! a new gesture forks the timeline
        call push_step(db%hist%undo, step, db%hist%cap)
    end subroutine

    ! Group the undo set by base file into per-path deltas, each holding both the
    ! pre-gesture (undo) and post-gesture (redo) image.  An EXTEND record fixes a
    ! path's pre-gesture length (the single growth point); REGION records give
    ! the pre-image of each in-place overwrite.  finish_delta fills the post
    ! images and the grown tail from the (now flushed) files.
    subroutine build_deltas(db, deltas, stat)
        class(db_t),                     intent(in)  :: db
        type(path_delta_t), allocatable, intent(out) :: deltas(:)
        integer,                         intent(out) :: stat
        integer :: i, di
        stat = SQR_OK
        allocate(deltas(0))
        do i = 1, db%jrnl%nrec
            associate (r => db%jrnl%recs(i))
                di = find_delta(deltas, r%path)
                if (di == 0) di = add_delta(deltas, r%path)
                select case (r%kind)
                case (UNDO_EXTEND)
                    deltas(di)%undo_len = r%orig_len   ! length before the growth
                case (UNDO_REGION)
                    call add_region(deltas(di), r%offset, r%bytes)
                end select
            end associate
        end do
        do di = 1, size(deltas)
            call finish_delta(db, deltas(di), stat)
            if (stat /= SQR_OK) return
        end do
    end subroutine

    ! Complete one delta: read the current length and the post-gesture image of
    ! every in-place range, then capture the grown tail (bytes that did not exist
    ! before the gesture; undo truncates them away, redo re-appends them).
    subroutine finish_delta(db, d, stat)
        class(db_t),        intent(in)    :: db
        type(path_delta_t), intent(inout) :: d
        integer,            intent(out)   :: stat
        character(len=:), allocatable :: full
        integer :: k, ios
        stat = SQR_OK
        full = pathjoin(db%dir, d%path)
        d%redo_len = file_len(full)
        if (d%undo_len < 0) d%undo_len = d%redo_len      ! no growth this gesture
        do k = 1, size(d%ranges)
            call read_region(full, d%ranges(k)%offset, &
                 int(len(d%ranges(k)%undo_bytes), int64), d%ranges(k)%redo_bytes, ios)
            call io_check(ios)
            if (ios /= 0) then
                stat = SQR_ERR
                return
            end if
        end do
        if (d%redo_len > d%undo_len) then
            call add_region(d, d%undo_len + 1, '')       ! pure tail: empty pre-image
            k = size(d%ranges)
            call read_region(full, d%undo_len + 1, d%redo_len - d%undo_len, &
                 d%ranges(k)%redo_bytes, ios)
            call io_check(ios)
            if (ios /= 0) then
                stat = SQR_ERR
                return
            end if
        end if
    end subroutine

    ! Write one history step's bytes in the chosen direction, then restore the
    ! matching counters and resync the open units + index trees from the bytes
    ! just written — the same coherence sequence apply_rollback runs, but driven
    ! by a stored step rather than the live undo set, and with no active txn.
    subroutine apply_step(db, step, redo, st)
        class(db_t),       intent(inout) :: db
        type(hist_step_t), intent(in)    :: step
        logical,           intent(in)    :: redo
        integer,           intent(inout) :: st
        character(len=:), allocatable :: full, img
        integer :: di, k, ios
        call flush_base_files(db)
        do di = 1, size(step%deltas)
            associate (d => step%deltas(di))
                full = pathjoin(db%dir, d%path)
                do k = 1, size(d%ranges)
                    if (redo) then
                        img = d%ranges(k)%redo_bytes
                    else
                        img = d%ranges(k)%undo_bytes
                    end if
                    if (len(img) > 0) then
                        call write_region(full, d%ranges(k)%offset, img, ios)
                        call io_check(ios)
                        if (ios /= 0) st = SQR_ERR
                    end if
                end do
                ! Undo shrinks a grown file back to its pre-gesture length; redo's
                ! tail write has already grown it, so no truncate is needed there.
                if (.not. redo .and. d%redo_len > d%undo_len) then
                    ios = c_truncate(full, d%undo_len)
                    call io_check(ios)
                    if (ios /= 0) st = SQR_ERR
                end if
            end associate
        end do
        if (redo) then
            call restore_counters_from(db, step%after)
        else
            call restore_counters_from(db, step%before)
        end if
        call resync_base_files(db, st)
        call resync_index_trees(db, st)
        db%generation = db%generation + 1      ! invalidate any open cursors
    end subroutine

    ! Reopen every live index tree from its (just-restored) file — the index
    ! analogue of resync_base_files.  bt_reload alone is not enough here: unlike
    ! a rollback, a history apply is followed by reads (the app re-broadcasts the
    ! model), so the tree's open unit may hold a stale read-buffer of a page we
    ! have since overwritten through a different unit, and on a redo the file may
    ! have GROWN past that unit's cached end-of-file.  A clean close + reopen
    ! discards both.  The unit carries only pending reads since the gesture
    ! committed (its writes were flushed then), so a raw close flushes nothing
    ! back; the on-disk bytes — meta page included — are already authoritative.
    subroutine resync_index_trees(db, stat)
        class(db_t), intent(inout) :: db
        integer,     intent(inout) :: stat
        integer :: ti, j, st
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle
                    if (t%indices(j)%bt%unit /= -1) then
                        close(t%indices(j)%bt%unit)
                        t%indices(j)%bt%unit = -1
                    end if
                    call open_index(db, t, t%indices(j), j, 'old', st)
                    if (st /= SQR_OK .and. stat == SQR_OK) stat = st
                end do
            end associate
        end do
    end subroutine

    ! Snapshot every table's current in-memory counters (the post-gesture image).
    function snapshot_now(db) result(snaps)
        class(db_t), intent(in) :: db
        type(tbl_snap_t), allocatable :: snaps(:)
        integer :: ti
        allocate(snaps(db%ntables))
        do ti = 1, db%ntables
            associate (t => db%tables(ti))
                snaps(ti)%next_id    = t%next_id
                snaps(ti)%live_count = t%live_count
                snaps(ti)%blob_next  = t%blob_next
            end associate
        end do
    end function

    ! Restore each table's counters from a stored snapshot (history analogue of
    ! restore_counters, which reads the live txn snapshot instead).
    subroutine restore_counters_from(db, snaps)
        class(db_t),      intent(inout) :: db
        type(tbl_snap_t), intent(in)    :: snaps(:)
        integer :: ti
        do ti = 1, min(db%ntables, size(snaps))
            associate (t => db%tables(ti))
                t%next_id    = snaps(ti)%next_id
                t%live_count = snaps(ti)%live_count
                t%blob_next  = snaps(ti)%blob_next
            end associate
        end do
    end subroutine

    ! ---- history stack + delta helpers ----

    ! Move one step's allocatable images from src to dst in O(1) (no byte copy),
    ! leaving src empty.  The nested allocatables inside deltas move with the
    ! array component.
    subroutine hist_move(src, dst)
        type(hist_step_t), intent(inout) :: src, dst
        call move_alloc(src%deltas, dst%deltas)
        call move_alloc(src%before, dst%before)
        call move_alloc(src%after,  dst%after)
        call move_alloc(src%label,  dst%label)
    end subroutine

    ! Empty a stack: drop the ring and mark it unused.
    subroutine reset_stack(stack)
        type(hist_stack_t), intent(inout) :: stack
        if (allocated(stack%steps)) deallocate(stack%steps)
        stack%head  = 0
        stack%count = 0
    end subroutine

    ! Push a step onto the ring, retaining at most `cap` newest: once full the
    ! advancing head overwrites the oldest slot.  Never copies the whole stack —
    ! the step is MOVED into its slot, so `step` is emptied on return.
    subroutine push_step(stack, step, cap)
        type(hist_stack_t), intent(inout) :: stack
        type(hist_step_t),  intent(inout) :: step
        integer,            intent(in)    :: cap
        if (.not. allocated(stack%steps)) allocate(stack%steps(max(cap, 1)))
        stack%head = mod(stack%head, size(stack%steps)) + 1
        call hist_move(step, stack%steps(stack%head))   ! overwrites the oldest slot when full
        if (stack%count < size(stack%steps)) stack%count = stack%count + 1
    end subroutine

    ! Pop the newest (top) step off a non-empty ring, moving it out into `step`.
    subroutine pop_step(stack, step)
        type(hist_stack_t), intent(inout) :: stack
        type(hist_step_t),  intent(out)   :: step
        if (stack%count == 0) return                    ! empty (callers guard)
        call hist_move(stack%steps(stack%head), step)
        stack%head  = stack%head - 1
        if (stack%head == 0) stack%head = size(stack%steps)   ! wrap to newest-below
        stack%count = stack%count - 1
        if (stack%count == 0) stack%head = 0            ! empty sentinel
    end subroutine

    ! Index of the delta for `path`, or 0 if none yet.
    integer function find_delta(deltas, path) result(di)
        type(path_delta_t), intent(in) :: deltas(:)
        character(len=*),   intent(in) :: path
        integer :: i
        di = 0
        do i = 1, size(deltas)
            if (deltas(i)%path == path) then
                di = i
                return
            end if
        end do
    end function

    ! Append a fresh delta for `path` (undo_len = -1: no growth seen yet).
    integer function add_delta(deltas, path) result(di)
        type(path_delta_t), allocatable, intent(inout) :: deltas(:)
        character(len=*),                intent(in)    :: path
        type(path_delta_t), allocatable :: tmp(:)
        integer :: n
        n = size(deltas)
        allocate(tmp(n+1))
        if (n > 0) tmp(1:n) = deltas(1:n)
        tmp(n+1)%path     = path
        tmp(n+1)%undo_len = -1_int64
        allocate(tmp(n+1)%ranges(0))
        call move_alloc(tmp, deltas)
        di = n + 1
    end function

    ! Append one changed range (redo image filled later by finish_delta).
    subroutine add_region(d, offset, bytes)
        type(path_delta_t), intent(inout) :: d
        integer(int64),     intent(in)    :: offset
        character(len=*),   intent(in)    :: bytes
        type(byterange_t), allocatable :: tmp(:)
        integer :: n
        n = size(d%ranges)
        allocate(tmp(n+1))
        if (n > 0) tmp(1:n) = d%ranges(1:n)
        tmp(n+1)%offset     = offset
        tmp(n+1)%undo_bytes = bytes
        tmp(n+1)%redo_bytes = ''
        call move_alloc(tmp, d%ranges)
    end subroutine

end submodule sqr_journal

! sqr_record — row-level data operations for the sqr module.
!
! Descendant of `sqr_base`: key extraction/compare, uniqueness checking, the
! NULL-bitmap and text-descriptor helpers, and the file handles all come from
! the parent submodule by host association.  Holds the public row API — insert,
! get, update, delete, scan, and the variable-length DT_TEXT accessors — plus
! the per-row secondary-index maintenance those mutations drive.

submodule (sqr:sqr_base) sqr_record
    implicit none

    ! Old/new index-key bytes for one index during an update: extracted once
    ! (db_update needs them for the pre-write uniqueness/NaN check and again
    ! for the post-write maintenance) so the two passes cannot drift.
    type :: keypair_t
        character(len=:), allocatable :: okey  ! key bytes from the old row image
        character(len=:), allocatable :: nkey  ! key bytes from the new row image
        logical :: oldin = .false.  ! old image is in this index (no NULL member)
        logical :: newin = .false.  ! new image is in this index
    end type

contains

    ! ===== Row operations =====

    ! Auto-commit bracket: each row mutator wraps its core in an implicit
    ! transaction so a mid-op failure rolls back cleanly (no torn row/index).
    ! See ac_begin/ac_end in sqr_base.  When an explicit transaction is already
    ! in flight the bracket is a no-op and the explicit commit/rollback decides.
    module subroutine db_insert(db, table_name, buf, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        character(len=*), intent(in)           :: buf
        integer(int32),   intent(out)          :: row_id
        integer,          intent(out), optional :: stat
        integer :: rs
        logical :: owns
        call ac_begin(db, owns, rs)
        if (rs == SQR_OK) call ins_core(db, table_name, buf, row_id, rs)
        call ac_end(db, owns, rs)
        if (rs /= SQR_OK) row_id = 0   ! atomic: a rolled-back insert leaves no row
        if (present(stat)) stat = rs
    end subroutine

    subroutine ins_core(db, table_name, buf, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        character(len=*), intent(in)           :: buf
        integer(int32),   intent(out)          :: row_id
        integer,          intent(out), optional :: stat
        integer :: idx, rs, ci, ios
        character(len=:), allocatable :: wbuf
        if (readonly_block(db, stat)) then
            row_id = 0
            return
        end if
        db%generation = db%generation + 1   ! write: invalidate cursors
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            row_id = 0
            return
        end if
        associate (t => db%tables(idx))
            row_id = t%next_id
            allocate(character(len=t%record_size) :: wbuf)
            wbuf = buf(1:min(len(buf), t%record_size))
            if (len(buf) < t%record_size) wbuf(len(buf)+1:) = char(0)
            call row_set_status(wbuf, ROW_ALIVE)
            ! Fresh rows start with empty text; db_set_text fills them later.
            zero_text: do ci = 1, t%ncols
                if (t%cols(ci)%dtype == DT_TEXT) &
                    call row_set_text_desc(wbuf, t%cols(ci), 0_int64, 0_int32)
            end do zero_text

            ! Per-index pre-write checks. Nothing on disk is touched yet, so a
            ! rejection here leaves the table unchanged. Reject NaN keys (no
            ! place in the index's total order) for every index, then enforce
            ! uniqueness for unique indices.
            precheck: do ci = 1, t%nindices
                associate (ix => t%indices(ci))
                    if (.not. idx_live(ix)) cycle precheck
                    ! A row with any NULL index member is not in that index, so
                    ! it cannot violate uniqueness and carries no NaN key there.
                    if (key_has_null(t, ix, wbuf)) cycle precheck
                    check_one: block
                        character(len=:), allocatable :: key
                        logical :: viol
                        allocate(character(len=ix%key_size) :: key)
                        call extract_key(t, ix, wbuf, key)
                        if (key_has_nan(t, ix, key)) then
                            if (present(stat)) stat = SQR_INVALID
                            row_id = 0
                            return
                        end if
                        if (.not. ix%unique) exit check_one
                        call unique_violation(db, idx, ci, key, 0_int32, viol, rs)
                        if (rs /= SQR_OK) then
                            if (present(stat)) stat = rs
                            row_id = 0
                            return
                        end if
                        if (viol) then
                            if (present(stat)) stat = SQR_DUP
                            row_id = 0
                            return
                        end if
                    end block check_one
                end associate
            end do precheck

            ! A fresh row is written at next_id, the data file's high-water, so it
            ! always grows the file by one record at the end: an EXTEND undo whose
            ! rollback truncates every row this txn appended.
            if (db%jrnl%active) then
                call jrnl_log_extend(db, data_relpath(t%name), rs)
                if (rs /= SQR_OK) then
                    if (present(stat)) stat = rs
                    row_id = 0
                    return
                end if
            end if
            write(t%unit, rec=row_id, iostat=ios) wbuf
            call io_check(ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                row_id = 0          ! contract: 0 on failure (no row written)
                return
            end if
            t%next_id = t%next_id + 1
            t%live_count = t%live_count + 1
            call update_indices_on_insert(db, idx, row_id, wbuf, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                ! Index update failed: the bracket rolls the whole op back
                ! (row write + any index changes), so the table is left
                ! exactly as it was — db_insert zeroes row_id on failure.
                return
            end if
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_get(db, table_name, row_id, buf, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        character(len=*), intent(out)          :: buf
        integer,          intent(out), optional :: stat
        integer :: idx, ios
        character(len=:), allocatable :: rbuf
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            if (row_id < 1 .or. row_id >= t%next_id) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            allocate(character(len=t%record_size) :: rbuf)
            read(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then          ! I/O failure is not "row absent"
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (row_status(rbuf) /= ROW_ALIVE) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            buf = rbuf
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    ! Rewrite an existing live row at the same record slot. Records are
    ! fixed-size, so there is no "size changed -> delete+insert" case here:
    ! the slot is always reused. The only non-trivial work is keeping any
    ! secondary index consistent — an index entry whose key has changed must
    ! be removed and reinserted, or a later lookup by the *old* key would
    ! resolve to this still-live row (index_find trusts the index and does not
    ! re-check the column value).
    module subroutine db_update(db, table_name, row_id, buf, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        character(len=*), intent(in)           :: buf
        integer,          intent(out), optional :: stat
        integer :: rs
        logical :: owns
        call ac_begin(db, owns, rs)
        if (rs == SQR_OK) call upd_core(db, table_name, row_id, buf, rs)
        call ac_end(db, owns, rs)
        if (present(stat)) stat = rs
    end subroutine

    subroutine upd_core(db, table_name, row_id, buf, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        character(len=*), intent(in)           :: buf
        integer,          intent(out), optional :: stat
        integer :: idx, ios, ci, j, rs
        integer(int64) :: toff
        integer(int32) :: tlen
        character(len=:), allocatable :: rbuf, wbuf
        type(keypair_t), allocatable :: kp(:)   ! per-index old/new key bytes
        if (readonly_block(db, stat)) return
        db%generation = db%generation + 1   ! write: invalidate cursors
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            if (row_id < 1 .or. row_id >= t%next_id) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            allocate(character(len=t%record_size) :: rbuf)
            read(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then          ! I/O failure is not "row absent"
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (row_status(rbuf) /= ROW_ALIVE) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if

            ! Build the new record image from the caller buffer.
            allocate(character(len=t%record_size) :: wbuf)
            wbuf = buf(1:min(len(buf), t%record_size))
            if (len(buf) < t%record_size) wbuf(len(buf)+1:) = char(0)
            call row_set_status(wbuf, ROW_ALIVE)

            ! TEXT is blob-backed and changed via db_set_text; the caller's
            ! buffer cannot carry a valid descriptor. db_update is a full-row
            ! replace, so the NULL bit comes from the caller's buffer (wbuf):
            !   - caller marks the column NULL  -> drop the descriptor; any old
            !     blob is orphaned and reclaimed by db_compact (the NULL bit set
            !     in wbuf and a zero descriptor must agree, else db_get_text can
            !     return stale text from a logically-NULL column).
            !   - column stays non-NULL and had stored text -> carry the stored
            !     descriptor forward.
            !   - column becomes non-NULL from NULL -> start empty; the caller
            !     supplies the bytes via db_set_text after this update.
            keep_text: do ci = 1, t%ncols
                associate (c => t%cols(ci))
                    if (c%dtype /= DT_TEXT) cycle keep_text
                    if (row_is_null(wbuf, c) .or. row_is_null(rbuf, c)) then
                        call row_set_text_desc(wbuf, c, 0_int64, 0_int32)
                    else
                        call row_get_text_desc(rbuf, c, toff, tlen)
                        call row_set_text_desc(wbuf, c, toff, tlen)
                    end if
                end associate
            end do keep_text

            ! Extract each index's old/new key bytes ONCE: the pre-write check
            ! below and the post-write maintenance both need them, and computing
            ! them in one place keeps the two passes from drifting apart.
            allocate(kp(t%nindices))
            extract_keys: do j = 1, t%nindices
                associate (ix => t%indices(j))
                    if (.not. idx_live(ix)) cycle extract_keys
                    allocate(character(len=ix%key_size) :: kp(j)%okey, kp(j)%nkey)
                    call extract_key(t, ix, rbuf, kp(j)%okey)
                    call extract_key(t, ix, wbuf, kp(j)%nkey)
                    ! A NULL index member keeps the row out of that index
                    ! (partial-index semantics), so maintenance below is driven
                    ! by whether each image is in the index, not just key change.
                    kp(j)%oldin = .not. key_has_null(t, ix, rbuf)
                    kp(j)%newin = .not. key_has_null(t, ix, wbuf)
                end associate
            end do extract_keys

            ! Pre-write index checks BEFORE any write: reject NaN keys (no place
            ! in the index's total order) for every index, and for every unique
            ! index whose composite key changed, a different live row must not
            ! already carry the new key (the row itself is exempt).
            uniq_loop: do j = 1, t%nindices
                associate (ix => t%indices(j))
                    if (.not. idx_live(ix)) cycle uniq_loop
                    if (.not. kp(j)%newin) cycle uniq_loop   ! new image not indexed
                    uniq_one: block
                        logical :: viol
                        if (key_has_nan(t, ix, kp(j)%nkey)) then
                            if (present(stat)) stat = SQR_INVALID
                            return
                        end if
                        ! Check uniqueness only if the row is entering the index
                        ! (was NULL/out) or its key changed within it.
                        if (ix%unique .and. (.not. kp(j)%oldin .or. kp(j)%okey /= kp(j)%nkey)) then
                            call unique_violation(db, idx, j, kp(j)%nkey, row_id, viol, rs)
                            if (rs /= SQR_OK) then
                                if (present(stat)) stat = rs
                                return
                            end if
                            if (viol) then
                                if (present(stat)) stat = SQR_DUP
                                return
                            end if
                        end if
                    end block uniq_one
                end associate
            end do uniq_loop

            ! Write the row image first. If this fails the secondary indices
            ! have not been touched yet, so the store stays consistent (a row
            ! write that fails after index rewrites would leave indices ahead
            ! of durable state). rbuf/wbuf still hold the old/new key bytes the
            ! index maintenance below needs.
            ! rbuf is the pristine on-disk image; capture it as the record's
            ! pre-image before overwriting in place so a rollback restores it.
            call journal_record(db, t, row_id, rbuf, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
            write(t%unit, rec=row_id, iostat=ios) wbuf
            call io_check(ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if

            ! Maintain each index (keys/membership reused from the single
            ! extraction above): remove the old entry if the old image was in
            ! the index and either the key changed or the row is leaving it;
            ! insert the new entry if the new image belongs and either the key
            ! changed or the row is entering it.
            idx_loop: do j = 1, t%nindices
                associate (ix => t%indices(j))
                    if (.not. idx_live(ix)) cycle idx_loop
                    maint: block
                        logical :: changed
                        changed = kp(j)%okey /= kp(j)%nkey
                        if (kp(j)%oldin .and. (.not. kp(j)%newin .or. changed)) then
                            call index_remove(db, idx, j, row_id, kp(j)%okey, rs)
                            if (rs /= SQR_OK) then
                                if (present(stat)) stat = rs
                                return
                            end if
                        end if
                        if (kp(j)%newin .and. (.not. kp(j)%oldin .or. changed)) then
                            call index_insert(db, idx, j, row_id, wbuf, rs)
                            if (rs /= SQR_OK) then
                                if (present(stat)) stat = rs
                                return
                            end if
                        end if
                    end block maint
                end associate
            end do idx_loop

        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_delete(db, table_name, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        integer,          intent(out), optional :: stat
        integer :: rs
        logical :: owns
        call ac_begin(db, owns, rs)
        if (rs == SQR_OK) call del_core(db, table_name, row_id, rs)
        call ac_end(db, owns, rs)
        if (present(stat)) stat = rs
    end subroutine

    subroutine del_core(db, table_name, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        integer,          intent(out), optional :: stat
        integer :: idx, ios, rs
        character(len=:), allocatable :: rbuf
        if (readonly_block(db, stat)) return
        db%generation = db%generation + 1   ! write: invalidate cursors
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            if (row_id < 1 .or. row_id >= t%next_id) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            allocate(character(len=t%record_size) :: rbuf)
            read(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then          ! I/O failure is not "row absent"
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (row_status(rbuf) /= ROW_ALIVE) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            ! Capture the live image before tombstoning it in place (rbuf is
            ! about to be mutated), so a rollback brings the row back alive.
            call journal_record(db, t, row_id, rbuf, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
            call row_set_status(rbuf, ROW_TOMBSTONE)
            write(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if
            t%live_count = t%live_count - 1
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_scan(db, table_name, cb, ctx, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        procedure(scan_cb)                     :: cb
        class(*),         intent(inout)        :: ctx
        integer,          intent(out), optional :: stat
        integer :: idx, ios
        integer(int32) :: rid
        logical :: stop_flag
        character(len=:), allocatable :: rbuf
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            allocate(character(len=t%record_size) :: rbuf)
            scan_loop: do rid = 1, t%next_id - 1
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then          ! stop, don't silently omit rows
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
                if (row_status(rbuf) /= ROW_ALIVE) cycle scan_loop
                call cb(db, rid, rbuf, ctx, stop_flag)
                if (stop_flag) exit scan_loop
            end do scan_loop
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    ! ===== Variable-length text (blob-backed) =====

    module subroutine db_set_text(db, table_name, row_id, col_name, text, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        character(len=*), intent(in)           :: col_name
        character(len=*), intent(in)           :: text
        integer,          intent(out), optional :: stat
        integer :: rs
        logical :: owns
        call ac_begin(db, owns, rs)
        if (rs == SQR_OK) call set_text_core(db, table_name, row_id, col_name, text, rs)
        call ac_end(db, owns, rs)
        if (present(stat)) stat = rs
    end subroutine

    subroutine set_text_core(db, table_name, row_id, col_name, text, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        integer(int32),   intent(in)           :: row_id
        character(len=*), intent(in)           :: col_name
        character(len=*), intent(in)           :: text
        integer,          intent(out), optional :: stat
        integer :: idx, ci, ios, rs
        integer(int64) :: off
        character(len=:), allocatable :: rbuf
        if (readonly_block(db, stat)) return
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            if (row_id < 1 .or. row_id >= t%next_id) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            ci = col_index(t, col_name)
            if (ci == 0) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            if (t%cols(ci)%dtype /= DT_TEXT) then
                if (present(stat)) stat = SQR_INVALID
                return
            end if
            allocate(character(len=t%record_size) :: rbuf)
            read(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then          ! I/O failure is not "row absent"
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (row_status(rbuf) /= ROW_ALIVE) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            off = t%blob_next
            if (len(text) > 0) then
                ! The text is appended at blob_next, the blob file's end, so the
                ! write only ever grows it: an EXTEND undo truncating this txn's
                ! appended bytes on rollback.
                if (db%jrnl%active) then
                    call jrnl_log_extend(db, blob_relpath(t%name), rs)
                    if (rs /= SQR_OK) then
                        if (present(stat)) stat = rs
                        return
                    end if
                end if
                write(t%blob_unit, pos=off, iostat=ios) text
                call io_check(ios)
                if (ios /= 0) then
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
                t%blob_next = off + len(text)
            end if
            ! Capture the record's pre-image before stamping the new descriptor
            ! (rbuf is the on-disk image until the next two calls mutate it).
            call journal_record(db, t, row_id, rbuf, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
            call row_set_text_desc(rbuf, t%cols(ci), off, int(len(text), int32))
            call row_clear_null(rbuf, t%cols(ci))   ! a stored value is not NULL
            ! Blob bytes are already durable; a failure here leaves them as
            ! orphans (benign — db_compact reclaims) and the row unchanged.
            write(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then
                if (present(stat)) stat = SQR_ERR
                return
            end if
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    module subroutine db_get_text(db, table_name, row_id, col_name, text, stat)
        class(db_t),       intent(inout)            :: db
        character(len=*), intent(in)               :: table_name
        integer(int32),   intent(in)               :: row_id
        character(len=*), intent(in)               :: col_name
        character(len=:), allocatable, intent(out) :: text
        integer,          intent(out), optional    :: stat
        integer :: idx, ci, ios
        integer(int64) :: off
        integer(int32) :: length
        character(len=:), allocatable :: rbuf
        text = ''
        idx = db_table_index(db, table_name)
        if (idx == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(idx))
            if (row_id < 1 .or. row_id >= t%next_id) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            ci = col_index(t, col_name)
            if (ci == 0) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            if (t%cols(ci)%dtype /= DT_TEXT) then
                if (present(stat)) stat = SQR_INVALID
                return
            end if
            allocate(character(len=t%record_size) :: rbuf)
            read(t%unit, rec=row_id, iostat=ios) rbuf
            call io_check(ios)
            if (ios /= 0) then          ! I/O failure is not "row absent"
                if (present(stat)) stat = SQR_ERR
                return
            end if
            if (row_status(rbuf) /= ROW_ALIVE) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            if (row_is_null(rbuf, t%cols(ci))) then   ! NULL reads as absent
                if (present(stat)) stat = SQR_OK
                return                                 ! text already ''
            end if
            call row_get_text_desc(rbuf, t%cols(ci), off, length)
            if (length > 0) then
                ! Bound the stored descriptor against the actual blob file
                ! before trusting it. A corrupt length near huge(int32) (~2 GiB)
                ! would otherwise drive a wild allocate; check it against the
                ! blob size so the diagnosis is "corrupt", not allocation
                ! pressure. off is 1-based stream position.
                bound_desc: block
                    integer(int64) :: bsize
                    inquire(unit=t%blob_unit, size=bsize)
                    if (off < 1 .or. off - 1 + int(length, int64) > bsize) then
                        if (present(stat)) stat = SQR_INVALID
                        return
                    end if
                end block bound_desc
                ! Read straight into the allocatable (heap) result, sized by
                ! the stored descriptor. An automatic buffer here would put a
                ! multi-MB blob — or a corrupt/huge length — on the stack.
                deallocate(text)
                allocate(character(len=length) :: text, stat=ios)
                if (ios /= 0) then
                    text = ''
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
                read(t%blob_unit, pos=off, iostat=ios) text
                call io_check(ios)
                if (ios /= 0) then
                    text = ''
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
            end if
        end associate
        if (present(stat)) stat = SQR_OK
    end subroutine

    ! ===== Per-row index maintenance =====

    subroutine update_indices_on_insert(db, ti, row_id, buf, stat)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        integer,          intent(out)   :: stat
        integer :: j
        stat = SQR_OK
        update_loop: do j = 1, db%tables(ti)%nindices
            if (.not. idx_live(db%tables(ti)%indices(j))) cycle update_loop
            call index_insert(db, ti, j, row_id, buf, stat)
            if (stat /= SQR_OK) return
        end do update_loop
    end subroutine

    ! Insert (key, row_id) into index j of table ti. O(log N) via the
    ! B+-tree; duplicates are ordered (key, row_id) so each entry is
    ! uniquely addressable for removal.
    subroutine index_insert(db, ti, j, row_id, buf, stat)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti, j
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        integer,          intent(out)   :: stat
        character(len=:), allocatable :: key
        type(kc_ctx_t) :: cx
        integer :: bs
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            stat = SQR_OK
            if (key_has_null(t, ix, buf)) return   ! NULL member ⇒ not indexed
            allocate(character(len=ix%key_size) :: key)
            call extract_key(t, ix, buf, key)
            cx = make_kc_ctx(t, ix)
            call bt_insert(ix%bt, key, row_id, bt_key_cmp, cx, bs)
            stat = sqr_of_bt(bs)
            if (stat == SQR_OK) ix%nentries = int(ix%bt%nentries)
        end associate
    end subroutine

    ! Remove the (key, row_id) entry from index j of table ti. O(log N)
    ! via the B+-tree (lazy delete). SQR_NOT_FOUND if no such entry.
    subroutine index_remove(db, ti, j, row_id, key, stat)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti, j
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: key
        integer,          intent(out)   :: stat
        type(kc_ctx_t) :: cx
        integer :: bs
        logical :: found
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            cx = make_kc_ctx(t, ix)
            call bt_remove(ix%bt, key, row_id, bt_key_cmp, cx, found, bs)
            stat = sqr_of_bt(bs)
            if (stat == SQR_OK) then
                ix%nentries = int(ix%bt%nentries)
                if (.not. found) stat = SQR_NOT_FOUND
            end if
        end associate
    end subroutine

    ! Capture a record's current bytes into the rollback journal before it is
    ! overwritten in place.  A no-op outside a transaction.  old_bytes are the
    ! record's pristine on-disk image, which every caller already holds, passed
    ! straight to jrnl_log_region so a write still buffered in t%unit cannot be
    ! mis-read; the journal dedups a region captured twice in one txn, so the
    ! first (pre-txn) image wins.  A direct-access record of t%record_size bytes
    ! starts at the 1-based stream position (row_id-1)*record_size + 1.
    subroutine journal_record(db, t, row_id, old_bytes, stat)
        class(db_t),      intent(inout) :: db
        type(table_t),    intent(in)    :: t
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: old_bytes
        integer,          intent(out)   :: stat
        integer(int64) :: off
        stat = SQR_OK
        if (.not. db%jrnl%active) return
        off = (int(row_id, int64) - 1) * int(t%record_size, int64) + 1
        call jrnl_log_region(db, data_relpath(t%name), off, 0_int64, &
                             bytes=old_bytes, stat=stat)
    end subroutine

end submodule sqr_record

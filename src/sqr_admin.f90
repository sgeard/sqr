! sqr_admin — table maintenance operations for the sqr module.
!
! Descendant of `sqr_base`: it inherits the storage/engine core (key compare
! and extraction, uniqueness and duplicate-key walks, the B+-tree bulk
! rebuild, schema I/O) by host association and carries no `use` of its own.
! These are the heavier whole-table operations that sit beside the per-row
! API in sqr_record and the lookup API in sqr_index: dropping a secondary
! index (db_drop_index_1/m), batched all-or-nothing insert with one packed
! reindex per index (db_insert_many), and the consistency checker that walks
! every index against the data file (db_verify, verify_one_index).

submodule (sqr:sqr_base) sqr_admin
    implicit none

contains

    ! ===== Drop index / batch insert / verify =====

    module subroutine db_drop_index_1(db, table_name, col_name, stat, errmsg)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_name
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=len(col_name)) :: one(1)   ! named 1-elt array: no constructor temp
        call clear_last_err(db)
        one(1) = col_name
        call db_drop_index_m(db, table_name, one, stat, errmsg)
    end subroutine

    module subroutine db_drop_index_m(db, table_name, col_names, stat, errmsg)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, j, rs
        call clear_last_err(db)
        if (readonly_block(db, stat, errmsg)) return
        if (txn_block(db, stat, errmsg)) return
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call set_last_err(db, SQR_NOT_FOUND, 'no such table: ' // trim(table_name))
            call report(db, SQR_NOT_FOUND, stat, errmsg)
            return
        end if
        associate (t => db%tables(ti))
            j = index_for_columns(t, col_names)
            if (j == 0) then
                call report(db, SQR_NOT_FOUND, stat, errmsg)
                return
            end if
            ! Nothing irreversible before the schema write succeeds.  The slot
            ! is tombstoned (ncols = 0) rather than removed, so the __i<slot>
            ! file names of surviving indices stay valid; its fields are saved
            ! first and its tree stays open, so a failed write puts the index
            ! back exactly as it was - live in memory, in the schema and on disk,
            ! with the history and generation untouched.
            associate (ix => t%indices(j))
                saved: block
                    integer :: ncols, key_size, nentries
                    logical :: unique
                    character(len=SQR_NAME_LEN), allocatable :: columns(:)
                    integer, allocatable :: col_idx(:), key_off(:)
                    ncols = ix%ncols; key_size = ix%key_size
                    nentries = ix%nentries; unique = ix%unique
                    call move_alloc(ix%columns, columns)
                    call move_alloc(ix%col_idx, col_idx)
                    if (allocated(ix%key_off)) call move_alloc(ix%key_off, key_off)
                    call tombstone_slot(ix)
                    ! Persist the tombstone BEFORE deleting the file
                    ! (catalog-first discipline, as in db_drop_table): a crash
                    ! after it leaves an orphaned file the dead slot ignores, not
                    ! a live slot with no file.
                    call write_schema(db, t, rs)
                    if (rs /= SQR_OK) then
                        ix%ncols = ncols; ix%key_size = key_size
                        ix%nentries = nentries; ix%unique = unique
                        call move_alloc(columns, ix%columns)
                        call move_alloc(col_idx, ix%col_idx)
                        if (allocated(key_off)) call move_alloc(key_off, ix%key_off)
                        call report(db, rs, stat, errmsg)
                        return
                    end if
                end block saved
                db%generation = db%generation + 1   ! structural change: invalidate cursors
                call db_reset_history(db)           ! index-set change ⇒ captured undo/redo steps can't replay
                call bt_discard(ix%bt)              ! file is deleted next
            end associate
            call remove_file(index_path(db, t%name, j))
        end associate
        call report(db, SQR_OK, stat, errmsg)
    end subroutine

    ! Auto-commit bracket: the whole batch is one implicit transaction, so a
    ! mid-batch I/O failure or a failed packed reindex rolls every row back —
    ! the table returns to its exact pre-call state rather than keeping the rows
    ! written so far.  A no-op when an explicit transaction is already in flight
    ! (that scope's commit/rollback then decides) or on a read-only handle (the
    ! core reports SQR_READONLY).
    module subroutine db_insert_many(db, table_name, bufs, row_ids, stat, errmsg)
        class(db_t),      intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: bufs(:)
        integer(int32),   intent(out)           :: row_ids(:)
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: rs
        logical :: owns
        call clear_last_err(db)
        call ac_begin(db, owns, rs)
        if (rs == SQR_OK) call insert_many_core(db, table_name, bufs, row_ids, rs)
        call ac_end(db, owns, rs)
        if (rs /= SQR_OK) row_ids = 0   ! atomic: a rolled-back batch leaves no rows
        call report(db, rs, stat, errmsg)
    end subroutine

    subroutine insert_many_core(db, table_name, bufs, row_ids, stat)
        class(db_t),      intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: bufs(:)
        integer(int32),   intent(out)           :: row_ids(:)
        integer,          intent(out)          :: stat
        integer :: ti, n, k, j, ci, ios, rs
        integer(int32) :: rid
        character(len=:), allocatable :: wrows(:)
        stat = SQR_OK
        n = size(bufs)
        row_ids = 0
        if (readonly_block(db, stat)) return
        db%generation = db%generation + 1   ! write: invalidate cursors
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call set_last_err(db, SQR_NOT_FOUND, 'no such table: ' // trim(table_name))
            stat = SQR_NOT_FOUND
            return
        end if
        if (size(row_ids) < n) then
            stat = SQR_INVALID
            return
        end if
        if (n == 0) return
        associate (t => db%tables(ti))
            ! Strict on write — see ins_core (bufs is an array of one
            ! length, so a single check covers every row).
            if (len(bufs) /= t%record_size) then
                stat = SQR_INVALID
                return
            end if
            ! Refuse the whole batch if it would exhaust the int32 id space.
            if (t%next_id > huge(0_int32) - n) then
                stat = SQR_FULL
                return
            end if
            ! Build the row images once (status alive, text descriptors
            ! zeroed), mirroring db_insert's per-row preparation.
            allocate(character(len=t%record_size) :: wrows(n))
            build: do k = 1, n
                wrows(k) = bufs(k)
                call row_set_status(wrows(k), ROW_ALIVE)
                do ci = 1, t%ncols
                    if (t%cols(ci)%dtype == DT_TEXT) &
                        call row_set_text_desc(wrows(k), t%cols(ci), 0_int64, 0_int32)
                end do
            end do build

            ! Validate the WHOLE batch before writing anything: reject NaN keys
            ! and, for unique indices, any key that collides with the existing
            ! index or with an earlier batch row. A failure here leaves the table
            ! untouched (all-or-nothing on validation). A key with a NULL member
            ! equals no other key, so it does not constrain uniqueness; it is
            ! indexed all the same, so its other members may not be NaN.
            validate: do j = 1, t%nindices
                if (.not. idx_live(t%indices(j))) cycle validate
                associate (ix => t%indices(j))
                    check_index: block
                        character(len=:), allocatable :: bkeys(:)
                        logical, allocatable :: bnull(:)
                        logical :: viol
                        integer :: p
                        allocate(character(len=ix%key_size) :: bkeys(n))
                        allocate(bnull(n))
                        keys_pass: do k = 1, n
                            call extract_key(t, ix, wrows(k), bkeys(k))
                            bnull(k) = key_null_any(ix, bkeys(k))
                            if (key_has_nan(t, ix, bkeys(k))) then
                                stat = SQR_INVALID
                                return
                            end if
                        end do keys_pass
                        if (ix%unique) then
                            uniq_pass: do k = 1, n
                                if (bnull(k)) cycle uniq_pass
                                call unique_violation(db, ti, j, bkeys(k), 0_int32, viol, rs)
                                if (rs /= SQR_OK) then
                                    stat = rs
                                    return
                                end if
                                if (viol) then
                                    call set_last_err(db, SQR_DUP, 'unique-key violation on index over "' &
                                        // trim(ix%columns(1)) // '"')
                                    stat = SQR_DUP
                                    return
                                end if
                                ! Intra-batch: an earlier non-NULL row, same key.
                                batch_dup: do p = 1, k - 1
                                    if (bnull(p)) cycle batch_dup
                                    if (key_cmp_ix(t, ix, bkeys(p), bkeys(k)) == 0) then
                                        call set_last_err(db, SQR_DUP, 'unique-key violation on index over "' &
                                            // trim(ix%columns(1)) // '"')
                                        stat = SQR_DUP
                                        return
                                    end if
                                end do batch_dup
                            end do uniq_pass
                        end if
                    end block check_index
                end associate
            end do validate

            ! Every row appends at next_id (the data file's high-water), so the
            ! batch only ever grows the file: one EXTEND undo whose rollback
            ! truncates away every row this txn appended.  Logged once before the
            ! loop (jrnl_log_extend is idempotent per path anyway).
            if (db%jrnl%active) then
                call jrnl_log_extend(db, data_relpath(t%name), rs)
                if (rs /= SQR_OK) then
                    stat = rs
                    return
                end if
            end if

            ! Write every row.  Under the auto-commit bracket (or an explicit
            ! txn) the EXTEND above makes a mid-batch I/O failure roll the whole
            ! batch back; bare and un-journalled it degrades to the old weak
            ! guarantee (rows written so far stay).
            write_rows: do k = 1, n
                rid = t%next_id
                write(t%unit, rec=rid, iostat=ios) wrows(k)
                call io_check(ios)
                if (ios /= 0) then
                    stat = SQR_ERR
                    return
                end if
                row_ids(k)   = rid
                t%next_id    = t%next_id + 1
                t%live_count = t%live_count + 1
            end do write_rows

            ! Deferred index maintenance: one packed rebuild per live index over
            ! the now-complete data file, instead of a per-row tree insert.
            reindex: do j = 1, t%nindices
                if (.not. idx_live(t%indices(j))) cycle reindex
                call rebuild_index(db, ti, j, rs)
                if (rs /= SQR_OK) then
                    stat = rs
                    return
                end if
            end do reindex
        end associate
    end subroutine

    module subroutine db_verify(db, table_name, stat, errmsg)
        class(db_t),       intent(inout)           :: db
        character(len=*), intent(in)              :: table_name
        integer,          intent(out),  optional  :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer :: ti, j, ios, recount, vrs
        integer, allocatable :: expected(:)
        integer(int32) :: rid
        integer(int64) :: fsize
        character(len=:), allocatable :: rbuf
        character(len=SQR_ERRMSG_LEN) :: detail
        call clear_last_err(db)
        vrs = SQR_OK
        detail = ''
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            call set_last_err(db, SQR_NOT_FOUND, 'no such table: ' // trim(table_name))
            call report(db, SQR_NOT_FOUND, stat, errmsg)
            return
        end if
        verify: block
            associate (t => db%tables(ti))
                allocate(character(len=t%record_size) :: rbuf)
                ! (1) Live-row recount, data-file extent, and each live index's
                ! expected entry count — all folded into this single data scan
                ! so verify_one_index need not re-read the whole file per index.
                recount = 0
                allocate(expected(t%nindices))
                expected = 0
                scan_rows: do rid = 1, t%next_id - 1
                    read(t%unit, rec=rid, iostat=ios) rbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        vrs = SQR_ERR; detail = 'cannot read data record'
                        exit verify
                    end if
                    if (row_status(rbuf) /= ROW_ALIVE) cycle scan_rows
                    recount = recount + 1
                end do scan_rows
                if (recount /= t%live_count) then
                    vrs = SQR_INVALID; detail = 'live_count disagrees with row recount'
                    exit verify
                end if
                expected = recount   ! every live index holds one entry per live row
                inquire(unit=t%unit, size=fsize)
                if (fsize < int(t%next_id - 1, int64) * int(t%record_size, int64)) then
                    vrs = SQR_INVALID; detail = 'data file shorter than next_id implies'
                    exit verify
                end if
                ! (2) Each live index agrees with the data: every entry over a
                ! live row carries that row's current key (no stale entry), and
                ! the count of such entries equals the live rows to be indexed.
                verify_idx: do j = 1, t%nindices
                    if (.not. idx_live(t%indices(j))) cycle verify_idx
                    call verify_one_index(db, ti, j, rbuf, expected(j), vrs, detail)
                    if (vrs /= SQR_OK) exit verify
                end do verify_idx
            end associate
        end block verify
        if (vrs /= SQR_OK) call set_last_err(db, vrs, trim(detail))
        call report(db, vrs, stat, errmsg)
    end subroutine

    ! Check one index against the table data: walk the tree, and for every entry
    ! pointing at a LIVE row confirm the row's extracted key equals the entry key
    ! (catches a stale entry left by an interrupted update or a crash-overwrite),
    ! and confirm the number of such matched entries equals the live rows that
    ! ought to be indexed (catches a missing entry). Unique indices additionally
    ! must have no duplicate live keys. rs is SQR_OK / SQR_INVALID / SQR_ERR.
    subroutine verify_one_index(db, ti, j, rbuf, expected_in, rs, detail)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti, j
        character(len=*), intent(inout) :: rbuf
        integer,          intent(in)    :: expected_in   ! live rows to be indexed, from db_verify's scan
        integer,          intent(out)   :: rs
        character(len=*), intent(inout) :: detail
        integer :: bs, ios, matched
        integer(int32) :: rid
        logical :: ok, dup
        character(len=:), allocatable :: ckey, rkey
        type(bt_cursor_t) :: cur
        rs = SQR_OK
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            ! Walk the index; check live-row entries against the stored row.
            allocate(character(len=ix%key_size) :: ckey, rkey)
            call bt_first(ix%bt, cur, bs)
            if (bs /= BT_OK) then
                rs = sqr_of_bt(bs); detail = idx_detail(bs)
                return
            end if
            matched = 0
            walk: do
                ! A cyclic leaf chain used to make this walk run forever: every
                ! bt_next returned an entry, so the corruption showed up as a
                ! hang in the very routine a user runs to detect corruption.
                ! The cursor now counts its leaf hops and reports instead.
                call bt_next(ix%bt, cur, ckey, rid, ok, bs)
                if (bs /= BT_OK) then
                    rs = sqr_of_bt(bs); detail = idx_detail(bs)
                    return
                end if
                if (.not. ok) exit walk
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then
                    rs = SQR_ERR; detail = 'cannot read data record'
                    return
                end if
                if (row_status(rbuf) /= ROW_ALIVE) cycle walk   ! lazy-delete leftover
                call extract_key(t, ix, rbuf, rkey)
                if (key_cmp_ix(t, ix, rkey, ckey) /= 0) then
                    rs = SQR_INVALID; detail = 'stale index entry: key disagrees with row'
                    return
                end if
                matched = matched + 1
            end do walk
            if (matched /= expected_in) then
                rs = SQR_INVALID; detail = 'index entry count disagrees with live rows'
                return
            end if
            if (ix%unique) then
                dup = .true.
                call has_dup_live_keys(db, ti, j, dup, ios)
                if (ios /= SQR_OK) then
                    rs = ios; detail = 'cannot walk index for duplicate check'
                    return
                end if
                if (dup) then
                    rs = SQR_INVALID; detail = 'duplicate live keys in a unique index'
                    return
                end if
            end if
        end associate
    end subroutine

    ! db_verify's wording for a failed index read: BT_CORRUPT means the tree's
    ! own bytes are wrong (a bad page header, a pointer out of range, a leaf
    ! chain that cycles), which is a different report from a filesystem error.
    pure function idx_detail(bs) result(d)
        integer, intent(in) :: bs
        character(len=:), allocatable :: d
        if (bs == BT_CORRUPT) then
            d = 'corrupt index structure'
        else
            d = 'cannot read index'
        end if
    end function

end submodule sqr_admin

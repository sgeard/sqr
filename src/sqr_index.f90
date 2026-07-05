! sqr_index — secondary-index query and maintenance for the sqr module.
!
! Descendant of `sqr_base`: it inherits the storage/engine core — key compare
! and extraction, the B+-tree bulk rebuild, the kc_ctx_t comparator context,
! and the file-open helpers — by host association, so it carries no `use` of
! its own beyond the IEEE infinities that bound a range scan.  This submodule
! is the index read/seek side: building an index and its in-memory geometry
! (db_create_index_*), equality lookup (db_find_by_*), ordered cursors and
! leading-column range scans (db_open_cursor, db_find_range_*, db_cursor_next),
! and the natural-key by-unique-index operations (db_get/update/delete_by_key).

submodule (sqr:sqr_base) sqr_index
    use, intrinsic :: ieee_arithmetic, only: ieee_value, &
                                             ieee_positive_inf, ieee_negative_inf
    ! ieee_is_nan is host-associated from sqr_base (used by key_has_nan).
    implicit none

contains

    ! ===== Index support =====

    module subroutine db_create_index_1(db, table_name, col_name, stat, unique)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_name
        integer,          intent(out), optional :: stat
        logical,          intent(in),  optional :: unique
        character(len=len(col_name)) :: one(1)   ! named 1-elt array: no constructor temp
        one(1) = col_name
        call create_index_impl(db, table_name, one, stat, unique)
    end subroutine

    module subroutine db_create_index_m(db, table_name, col_names, stat, unique)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        integer,          intent(out), optional :: stat
        logical,          intent(in),  optional :: unique
        call create_index_impl(db, table_name, col_names, stat, unique)
    end subroutine

    ! Build a (possibly composite, possibly unique) secondary index over
    ! col_names in the given order. Rejects: unknown table/column, a TEXT
    ! member, a repeated member, an index already covering exactly these
    ! columns, and — when unique — pre-existing duplicate live keys.
    subroutine create_index_impl(db, table_name, col_names, stat, unique)
        type(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        integer,          intent(out), optional :: stat
        logical,          intent(in),  optional :: unique
        integer :: ti, rs, m, p, nc, koff
        logical :: uniq
        type(index_t), allocatable :: new_idx(:)
        type(index_t) :: ix
        if (readonly_block(db, stat)) return
        if (txn_block(db, stat)) return
        ! A freshly built index is not covered by any earlier gesture's captured
        ! deltas, so a later db_undo would restore the rows but leave this tree
        ! reflecting the post-gesture keys — a silent index/row disagreement.
        ! Drop the history (create_index does not shift rows, hence no gen bump).
        call db_reset_history(db)
        uniq = .false.
        if (present(unique)) uniq = unique
        nc = size(col_names)
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(ti))
            if (nc < 1) then
                if (present(stat)) stat = SQR_INVALID
                return
            end if
            if (index_for_columns(t, col_names) > 0) then
                if (present(stat)) stat = SQR_DUP
                return
            end if
            ix%ncols = nc
            allocate(ix%columns(nc), ix%col_idx(nc), ix%key_off(nc))
            koff = 1
            members: do m = 1, nc
                ix%columns(m) = col_names(m)
                ix%col_idx(m) = col_index(t, col_names(m))
                if (ix%col_idx(m) == 0) then
                    if (present(stat)) stat = SQR_NOT_FOUND
                    return
                end if
                if (t%cols(ix%col_idx(m))%dtype == DT_TEXT) then
                    if (present(stat)) stat = SQR_INVALID
                    return
                end if
                dup_member: do p = 1, m - 1
                    if (ix%col_idx(p) == ix%col_idx(m)) then
                        if (present(stat)) stat = SQR_INVALID
                        return
                    end if
                end do dup_member
                ix%key_off(m) = koff
                koff = koff + t%cols(ix%col_idx(m))%csize
            end do members
            ix%key_size = koff - 1
            ix%nentries = 0
            ix%unique   = uniq

            allocate(new_idx(t%nindices + 1))
            new_idx(1:t%nindices) = t%indices(1:t%nindices)
            new_idx(t%nindices + 1) = ix
            call move_alloc(new_idx, t%indices)
            t%nindices = t%nindices + 1

            call rebuild_index(db, ti, t%nindices, rs)
            if (rs /= SQR_OK) then
                call drop_last_index(db, ti)
                if (present(stat)) stat = rs
                return
            end if

            ! A unique index must not be built over data that already has
            ! duplicate live keys; tear it back down and report SQR_DUP.
            if (uniq) then
                call has_dup_live_keys(db, ti, t%nindices, uniq, rs)
                if (rs /= SQR_OK) then
                    call drop_last_index(db, ti)
                    if (present(stat)) stat = rs
                    return
                end if
                if (uniq) then   ! reused as the "violation found" out-flag
                    call drop_last_index(db, ti)
                    if (present(stat)) stat = SQR_DUP
                    return
                end if
            end if

            call write_schema(db, t, rs)
            if (rs /= SQR_OK) then
                if (present(stat)) stat = rs
                return
            end if
            if (present(stat)) stat = SQR_OK
        end associate
    end subroutine

    ! Tear down the most recently appended (still in-memory, not yet
    ! schema-persisted) index of table ti: close + delete its file and
    ! shrink the index array. Used to roll back a failed create.
    subroutine drop_last_index(db, ti)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: ti
        type(index_t), allocatable :: keep(:)
        integer :: s, u, ios
        associate (t => db%tables(ti))
            s = t%nindices
            ! File is deleted next — close without flushing meta.
            if (t%indices(s)%bt%unit /= -1) then
                close(t%indices(s)%bt%unit)
                t%indices(s)%bt%unit = -1
            end if
            open(newunit=u, file=index_path(db, t%name, s), status='old', iostat=ios)
            if (ios == 0) close(u, status='delete')
            allocate(keep(s - 1))
            keep(1:s-1) = t%indices(1:s-1)
            call move_alloc(keep, t%indices)
            t%nindices = s - 1
        end associate
    end subroutine

    ! First live row whose indexed key equals `key` (B+-tree lower-bound
    ! seek, then forward over the equal-key run skipping dead rows).
    ! row_id = 0 / SQR_NOT_FOUND if none.
    subroutine index_find(db, ti, j, key, row_id, stat)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti, j
        character(len=*), intent(in)    :: key
        integer(int32),   intent(out)   :: row_id
        integer,          intent(out)   :: stat
        integer :: bs, ios
        integer(int32) :: rid
        logical :: ok
        character(len=:), allocatable :: ckey, rbuf
        type(kc_ctx_t) :: cx
        type(bt_cursor_t) :: cur
        row_id = 0
        stat   = SQR_NOT_FOUND
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            allocate(character(len=ix%key_size) :: ckey)
            allocate(character(len=t%record_size) :: rbuf)
            cx = make_kc_ctx(t, ix)
            call bt_seek(ix%bt, key, bt_key_cmp, cx, cur, bs)
            if (bs /= BT_OK) then
                stat = SQR_ERR
                return
            end if
            scan: do
                call bt_next(ix%bt, cur, ckey, rid, ok, bs)
                if (bs /= BT_OK) then
                    stat = SQR_ERR
                    return
                end if
                if (.not. ok) exit scan
                if (key_cmp_ix(t, ix, ckey, key) /= 0) exit scan
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios == 0 .and. row_status(rbuf) == ROW_ALIVE) then
                    row_id = rid
                    stat = SQR_OK
                    return
                end if
            end do scan
        end associate
    end subroutine

    module subroutine db_find_by_int(db, table_name, col_name, key, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        character(len=*), intent(in)           :: col_name
        integer(int32),   intent(in)           :: key
        integer(int32),   intent(out)          :: row_id
        integer,          intent(out), optional :: stat
        integer :: ti, j, rs
        character(len=4) :: kbuf
        row_id = 0
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        j = index_index(db%tables(ti), col_name)
        if (j == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        if (leading_dtype(db%tables(ti), db%tables(ti)%indices(j)) /= DT_INT) then
            if (present(stat)) stat = SQR_NOT_FOUND   ! wrong overload for this index
            return
        end if
        kbuf = transfer(key, kbuf)
        call index_find(db, ti, j, kbuf, row_id, rs)
        if (present(stat)) stat = rs
    end subroutine

    ! Exact bit-for-bit equality — see the interface comment in sqr.f90.
    module subroutine db_find_by_real(db, table_name, col_name, key, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        character(len=*), intent(in)           :: col_name
        real(real64),     intent(in)           :: key
        integer(int32),   intent(out)          :: row_id
        integer,          intent(out), optional :: stat
        integer :: ti, j, rs
        character(len=8) :: kbuf
        row_id = 0
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        j = index_index(db%tables(ti), col_name)
        if (j == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        if (leading_dtype(db%tables(ti), db%tables(ti)%indices(j)) /= DT_REAL) then
            if (present(stat)) stat = SQR_NOT_FOUND   ! wrong overload for this index
            return
        end if
        ! A NaN key is never stored (rejected on write) and key_cmp's </>
        ! comparison would treat it as equal to every stored real, so it could
        ! return an unrelated row. It can match nothing — say so.
        if (ieee_is_nan(key)) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        kbuf = transfer(key, kbuf)
        call index_find(db, ti, j, kbuf, row_id, rs)
        if (present(stat)) stat = rs
    end subroutine

    module subroutine db_find_by_char(db, table_name, col_name, key, row_id, stat)
        class(db_t),       intent(inout)        :: db
        character(len=*), intent(in)           :: table_name
        character(len=*), intent(in)           :: col_name
        character(len=*), intent(in)           :: key
        integer(int32),   intent(out)          :: row_id
        integer,          intent(out), optional :: stat
        integer :: ti, j, rs, ks, nc
        character(len=:), allocatable :: kbuf
        row_id = 0
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        j = index_index(db%tables(ti), col_name)
        if (j == 0) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        if (leading_dtype(db%tables(ti), db%tables(ti)%indices(j)) /= DT_CHAR) then
            if (present(stat)) stat = SQR_NOT_FOUND   ! wrong overload for this index
            return
        end if
        ks = db%tables(ti)%indices(j)%key_size
        ! A key longer than the column can hold could never have been stored
        ! (row_set_char would truncate it), so it matches nothing — say so
        ! rather than silently truncating the search key to ks and matching a
        ! shorter stored value. Trailing blanks are insignificant padding.
        if (len_trim(key) > ks) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        nc = min(ks, len(key))
        allocate(character(len=ks) :: kbuf)
        kbuf              = repeat(char(0), ks)
        kbuf(1:nc) = key(1:nc)
        call index_find(db, ti, j, kbuf, row_id, rs)
        if (present(stat)) stat = rs
    end subroutine

    ! ===== Ordered cursor / range queries =====

    ! Resolve (table, column) to an index that can order/range by col_name:
    ! an exact single-column index if one exists (unchanged behaviour), else
    ! the first index whose LEADING member is col_name. A composite index's
    ! B+-tree key order is primarily by its leading member, so a range or scan
    ! over that member is a prefix scan needing no redundant single-column
    ! index (review item 5.2). SQR_NOT_FOUND if neither exists.
    subroutine find_leading_index(db, table_name, col_name, ti, j, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: table_name, col_name
        integer,          intent(out) :: ti, j
        integer,          intent(out) :: stat
        integer :: k
        j  = 0
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            stat = SQR_NOT_FOUND
            return
        end if
        j = index_index(db%tables(ti), col_name)   ! exact single-column first
        if (j == 0) then
            associate (t => db%tables(ti))
                scan: do k = 1, t%nindices
                    if (.not. idx_live(t%indices(k))) cycle scan
                    if (trim(t%indices(k)%columns(1)) == trim(col_name)) then
                        j = k
                        exit scan
                    end if
                end do scan
            end associate
        end if
        stat = merge(SQR_OK, SQR_NOT_FOUND, j /= 0)
    end subroutine

    pure function leading_dtype(t, ix) result(dt)
        type(table_t), intent(in) :: t
        type(index_t), intent(in) :: ix
        integer :: dt
        dt = t%cols(ix%col_idx(1))%dtype
    end function

    ! Byte image of the minimum / maximum value of a key member of the given
    ! dtype and width, used to fill the trailing members of a leading-column
    ! range bound so the full-key band [lokey,hikey] spans "any value" for those
    ! members in the index's member-by-member order (key_cmp): signed int32,
    ! real64 (-inf / +inf; NaN is already excluded from the index), or
    ! byte-lexicographic DT_CHAR. Not pure (ieee_value is not guaranteed pure),
    ! but only ever called from the non-pure range setup.
    function member_min(dtype, width) result(bytes)
        integer, intent(in)  :: dtype, width
        character(len=width)  :: bytes
        integer(int32) :: iv
        real(real64)   :: rv
        select case (dtype)
        case (DT_INT)
            iv = -huge(0_int32) - 1_int32          ! int32 minimum
            bytes = transfer(iv, bytes)
        case (DT_REAL)
            rv = ieee_value(rv, ieee_negative_inf)
            bytes = transfer(rv, bytes)
        case default                               ! DT_CHAR
            bytes = repeat(char(0), width)
        end select
    end function

    function member_max(dtype, width) result(bytes)
        integer, intent(in)  :: dtype, width
        character(len=width)  :: bytes
        integer(int32) :: iv
        real(real64)   :: rv
        select case (dtype)
        case (DT_INT)
            iv = huge(0_int32)                     ! int32 maximum
            bytes = transfer(iv, bytes)
        case (DT_REAL)
            rv = ieee_value(rv, ieee_positive_inf)
            bytes = transfer(rv, bytes)
        case default                               ! DT_CHAR
            bytes = repeat(char(255), width)
        end select
    end function

    ! Compose full-width lo/hi key bounds for a range on the LEADING member of
    ! index ix: the caller's leading-member bytes go in slot 1; every trailing
    ! member is filled with its typed minimum (lo) / maximum (hi) so the full-key
    ! band [lokey,hikey] is exactly {rows : lo <= leading <= hi}, whatever the
    ! trailing members hold. Reuses the existing full-key cursor machinery.
    subroutine build_leading_bounds(t, ix, lobytes, hibytes, lokey, hikey)
        type(table_t),                 intent(in)  :: t
        type(index_t),                 intent(in)  :: ix
        character(len=*),              intent(in)  :: lobytes, hibytes
        character(len=:), allocatable, intent(out) :: lokey, hikey
        integer :: m, lo, hi, w, dt
        allocate(character(len=ix%key_size) :: lokey, hikey)
        w = t%cols(ix%col_idx(1))%csize            ! leading member at key offset 1
        lokey(1:w) = lobytes(1:w)
        hikey(1:w) = hibytes(1:w)
        fill: do m = 2, ix%ncols
            lo = ix%key_off(m)
            w  = t%cols(ix%col_idx(m))%csize
            hi = lo + w - 1
            dt = t%cols(ix%col_idx(m))%dtype
            lokey(lo:hi) = member_min(dt, w)
            hikey(lo:hi) = member_max(dt, w)
        end do fill
    end subroutine

    module subroutine db_open_cursor(db, table_name, col_name, cur, stat)
        class(db_t),        intent(inout)         :: db
        character(len=*),  intent(in)            :: table_name, col_name
        type(db_cursor_t), intent(out)           :: cur
        integer,           intent(out), optional :: stat
        integer :: ti, j, rs, bs
        call find_leading_index(db, table_name, col_name, ti, j, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        cur%ti      = ti
        cur%j       = j
        cur%bounded = .false.
        cur%gen     = db%generation
        call bt_first(db%tables(ti)%indices(j)%bt, cur%bt, bs)
        cur%active = bs == BT_OK
        if (present(stat)) stat = sqr_of_bt(bs)
    end subroutine

    ! Shared opener for the typed db_find_range_* wrappers. lokey/hikey are
    ! the index key bytes for the inclusive [lo,hi] bounds; seeks to the first
    ! key >= lokey and records hikey so db_cursor_next stops once a yielded
    ! key orders after it. lo > hi simply yields nothing (the first key >= lo
    ! already orders after hi).
    subroutine open_range(db, ti, j, lokey, hikey, cur, stat)
        type(db_t),        intent(inout) :: db
        integer,           intent(in)    :: ti, j
        character(len=*),  intent(in)    :: lokey, hikey
        type(db_cursor_t), intent(out)   :: cur
        integer,           intent(out)   :: stat
        integer :: bs
        type(kc_ctx_t) :: cx
        cur%ti      = ti
        cur%j       = j
        cur%bounded = .true.
        cur%hikey   = hikey
        cur%gen     = db%generation
        cx = make_kc_ctx(db%tables(ti), db%tables(ti)%indices(j))
        call bt_seek(db%tables(ti)%indices(j)%bt, lokey, bt_key_cmp, cx, cur%bt, bs)
        cur%active = bs == BT_OK
        stat = sqr_of_bt(bs)
    end subroutine

    module subroutine db_find_range_int(db, table_name, col_name, lo, hi, cur, stat)
        class(db_t),        intent(inout)         :: db
        character(len=*),  intent(in)            :: table_name, col_name
        integer(int32),    intent(in)            :: lo, hi
        type(db_cursor_t), intent(out)           :: cur
        integer,           intent(out), optional :: stat
        integer :: ti, j, rs
        character(len=4) :: lb, hb
        character(len=:), allocatable :: lk, hk
        call find_leading_index(db, table_name, col_name, ti, j, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            if (leading_dtype(t, ix) /= DT_INT) then   ! wrong overload for this index
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            lb = transfer(lo, lb)
            hb = transfer(hi, hb)
            call build_leading_bounds(t, ix, lb, hb, lk, hk)
        end associate
        call open_range(db, ti, j, lk, hk, cur, rs)
        if (present(stat)) stat = rs
    end subroutine

    module subroutine db_find_range_real(db, table_name, col_name, lo, hi, cur, stat)
        class(db_t),        intent(inout)         :: db
        character(len=*),  intent(in)            :: table_name, col_name
        real(real64),      intent(in)            :: lo, hi
        type(db_cursor_t), intent(out)           :: cur
        integer,           intent(out), optional :: stat
        integer :: ti, j, rs
        character(len=8) :: lb, hb
        character(len=:), allocatable :: lk, hk
        call find_leading_index(db, table_name, col_name, ti, j, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            if (leading_dtype(t, ix) /= DT_REAL) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            ! A NaN bound is unordered against every stored real (key_cmp's </>
            ! both read false), so it cannot define a band. Reject it rather than
            ! seek on a nonsensical range.
            if (ieee_is_nan(lo) .or. ieee_is_nan(hi)) then
                if (present(stat)) stat = SQR_INVALID
                return
            end if
            lb = transfer(lo, lb)
            hb = transfer(hi, hb)
            call build_leading_bounds(t, ix, lb, hb, lk, hk)
        end associate
        call open_range(db, ti, j, lk, hk, cur, rs)
        if (present(stat)) stat = rs
    end subroutine

    module subroutine db_find_range_char(db, table_name, col_name, lo, hi, cur, stat)
        class(db_t),        intent(inout)         :: db
        character(len=*),  intent(in)            :: table_name, col_name
        character(len=*),  intent(in)            :: lo, hi
        type(db_cursor_t), intent(out)           :: cur
        integer,           intent(out), optional :: stat
        integer :: ti, j, rs, lw
        character(len=:), allocatable :: lb, hb, lk, hk
        call find_leading_index(db, table_name, col_name, ti, j, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            if (leading_dtype(t, ix) /= DT_CHAR) then
                if (present(stat)) stat = SQR_NOT_FOUND
                return
            end if
            lw = t%cols(ix%col_idx(1))%csize       ! leading char member width
            ! A bound longer than the column would be silently truncated,
            ! narrowing or widening the band to something the caller did not ask
            ! for; reject it instead. Trailing blanks are insignificant padding.
            if (len_trim(lo) > lw .or. len_trim(hi) > lw) then
                if (present(stat)) stat = SQR_INVALID
                return
            end if
            allocate(character(len=lw) :: lb, hb)
            lb = repeat(char(0), lw)
            hb = repeat(char(0), lw)
            lb(1:min(lw, len(lo))) = lo(1:min(lw, len(lo)))
            hb(1:min(lw, len(hi))) = hi(1:min(lw, len(hi)))
            call build_leading_bounds(t, ix, lb, hb, lk, hk)
        end associate
        call open_range(db, ti, j, lk, hk, cur, rs)
        if (present(stat)) stat = rs
    end subroutine

    ! Pull the next live row at/after the cursor in ascending key order,
    ! skipping tombstoned rows (lazy delete leaves their index entries) and
    ! stopping at the band's upper bound. The same seek+forward idiom as
    ! index_find, but yielding one row per call rather than the first match.
    module subroutine db_cursor_next(db, cur, row_id, buf, ok, stat)
        class(db_t),        intent(inout)         :: db
        type(db_cursor_t), intent(inout)         :: cur
        integer(int32),    intent(out)           :: row_id
        character(len=*),  intent(out)           :: buf
        logical,           intent(out)           :: ok
        integer,           intent(out), optional :: stat
        integer :: bs, ios
        integer(int32) :: rid
        logical :: got
        character(len=:), allocatable :: ckey, rbuf
        row_id = 0
        ok     = .false.
        ! A mutating call (or close) since this cursor was opened may have
        ! shifted or freed table slots; cur%ti/cur%j would then address the
        ! wrong table or run off the array. Detect it instead of risking UB.
        if (.not. db%opened) then
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        if (cur%gen /= db%generation) then
            cur%active = .false.
            if (present(stat)) stat = SQR_INVALID
            return
        end if
        if (.not. cur%active) then
            if (present(stat)) stat = SQR_OK
            return
        end if
        associate (t => db%tables(cur%ti), ix => db%tables(cur%ti)%indices(cur%j))
            allocate(character(len=ix%key_size)  :: ckey)
            allocate(character(len=t%record_size) :: rbuf)
            scan: do
                call bt_next(ix%bt, cur%bt, ckey, rid, got, bs)
                if (bs /= BT_OK) then
                    cur%active = .false.
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
                if (.not. got) then
                    cur%active = .false.
                    if (present(stat)) stat = SQR_OK
                    return
                end if
                if (cur%bounded) then
                    if (key_cmp_ix(t, ix, ckey, cur%hikey) > 0) then
                        cur%active = .false.
                        if (present(stat)) stat = SQR_OK
                        return
                    end if
                end if
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then
                    cur%active = .false.
                    if (present(stat)) stat = SQR_ERR
                    return
                end if
                if (row_status(rbuf) == ROW_ALIVE) then
                    row_id = rid
                    buf    = rbuf
                    ok     = .true.
                    if (present(stat)) stat = SQR_OK
                    return
                end if
            end do scan
        end associate
    end subroutine

    ! ===== Natural-key (by unique composite index) operations =====

    ! Resolve (table, member columns, key-bearing row buffer) to a live
    ! row_id via the matching UNIQUE index. SQR_NOT_FOUND if the table or a
    ! matching index is absent or no live row carries the key; SQR_INVALID
    ! if the matching index is not unique (a by-key op needs a single row).
    subroutine resolve_by_key(db, table_name, col_names, keyrow, ti, row_id, stat)
        type(db_t),       intent(inout) :: db
        character(len=*), intent(in)    :: table_name
        character(len=*), intent(in)    :: col_names(:)
        character(len=*), intent(in)    :: keyrow
        integer,          intent(out)   :: ti
        integer(int32),   intent(out)   :: row_id
        integer,          intent(out)   :: stat
        integer :: j
        character(len=:), allocatable :: key
        row_id = 0
        ti = db_table_index(db, table_name)
        if (ti == 0) then
            stat = SQR_NOT_FOUND
            return
        end if
        associate (t => db%tables(ti))
            j = index_for_columns(t, col_names)
            if (j == 0) then
                stat = SQR_NOT_FOUND
                return
            end if
            if (.not. t%indices(j)%unique) then
                stat = SQR_INVALID
                return
            end if
            allocate(character(len=t%indices(j)%key_size) :: key)
            call extract_key(t, t%indices(j), keyrow, key)
            call index_find(db, ti, j, key, row_id, stat)
        end associate
    end subroutine

    module subroutine db_get_by_key(db, table_name, col_names, keyrow, buf, stat, row_id)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        character(len=*), intent(in)            :: keyrow
        character(len=*), intent(out)           :: buf
        integer,          intent(out), optional :: stat
        integer(int32),   intent(out), optional :: row_id
        integer :: ti, rs
        integer(int32) :: rid
        if (present(row_id)) row_id = 0
        call resolve_by_key(db, table_name, col_names, keyrow, ti, rid, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        call db_get(db, table_name, rid, buf, stat)
        if (present(row_id)) row_id = rid
    end subroutine

    module subroutine db_update_by_key(db, table_name, col_names, keyrow, newrow, stat)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        character(len=*), intent(in)            :: keyrow
        character(len=*), intent(in)            :: newrow
        integer,          intent(out), optional :: stat
        integer :: ti, rs
        integer(int32) :: rid
        if (readonly_block(db, stat)) return
        call resolve_by_key(db, table_name, col_names, keyrow, ti, rid, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        call db_update(db, table_name, rid, newrow, stat)
    end subroutine

    module subroutine db_delete_by_key(db, table_name, col_names, keyrow, stat)
        class(db_t),       intent(inout)         :: db
        character(len=*), intent(in)            :: table_name
        character(len=*), intent(in)            :: col_names(:)
        character(len=*), intent(in)            :: keyrow
        integer,          intent(out), optional :: stat
        integer :: ti, rs
        integer(int32) :: rid
        if (readonly_block(db, stat)) return
        call resolve_by_key(db, table_name, col_names, keyrow, ti, rid, rs)
        if (rs /= SQR_OK) then
            if (present(stat)) stat = rs
            return
        end if
        call db_delete(db, table_name, rid, stat)
    end subroutine

end submodule sqr_index

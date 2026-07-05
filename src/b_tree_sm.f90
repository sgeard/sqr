! b_tree_sm — implementation of the generic on-disk B+-tree.
!
! Paged file (<path>, direct-access, recl = page_size):
!   page 1            meta  : magic "BTRE", byte-order mark (BT_BOM, a
!                             native int32 written so a file opened on a
!                             different-endian host fails the check rather
!                             than silently misreading every scalar),
!                             format version, page_size, key_len, root,
!                             free_head, npages, first_leaf,
!                             nentries(int64). Written LAST.
!   leaf  page        : kind=1, nkeys, next-leaf id, then nkeys entries,
!                       each (key[key_len], payload int32). Leaves are
!                       singly chained left->right for iteration.
!   internal page     : kind=2, nkeys, then (MAXK+2) child ids, then
!                       (MAXK+1) separators each (key[key_len], payload
!                       int32). The areas are widened by one past the
!                       stable capacity MAXK so the transient over-full
!                       node built in place during a split (MAXK+1 seps /
!                       MAXK+2 children) still fits. Child/sep areas are
!                       fixed-width so byte offsets do not depend on the
!                       live count.
!
! Order is the total order on (key, payload): the caller's pure
! comparator on keys, ties broken by ascending int32 payload. Every
! duplicate key is therefore uniquely addressable. Delete is lazy: an
! emptied leaf is left chained in place; bt_bulk_load repacks.
!
! The free_head field is reserved on disk for the crash-safety journal
! layer; this implementation never frees pages incrementally (lazy
! delete) and bt_bulk_load rewrites the file wholesale, so it stays 0.

submodule (b_tree) b_tree_sm
    use, intrinsic :: iso_fortran_env, only: int8  ! int32/int64 via host association from b_tree
    implicit none

    character(len=4), parameter :: BT_MAGIC = 'BTRE'
    integer(int8),    parameter :: K_LEAF = 1_int8
    integer(int8),    parameter :: K_INT  = 2_int8

    ! Byte-order mark: an asymmetric int32 stored native in the meta page.
    ! On read it equals BT_BOM iff the file was written with this host's
    ! byte order; BT_BOM_SWAP signals the opposite endianness (a clean
    ! BT_VERSION reject); anything else is corruption.
    integer(int32), parameter :: BT_BOM      = int(z'01020304', int32)
    integer(int32), parameter :: BT_BOM_SWAP = int(z'04030201', int32)

contains

    ! ===== Scalar <-> byte helpers (1-based offsets) =====

    pure function get_i32(pg, off) result(v)
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: off
        integer(int32) :: v
        v = transfer(pg(off:off+3), v)
    end function

    pure subroutine put_i32(pg, off, v)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: off
        integer(int32),   intent(in)    :: v
        pg(off:off+3) = transfer(v, pg(off:off+3))   ! mold supplies type/len only
    end subroutine

    pure function get_i64(pg, off) result(v)
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: off
        integer(int64) :: v
        v = transfer(pg(off:off+7), v)
    end function

    pure subroutine put_i64(pg, off, v)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: off
        integer(int64),   intent(in)    :: v
        pg(off:off+7) = transfer(v, pg(off:off+7))   ! mold supplies type/len only
    end subroutine

    ! ===== Geometry (derived from key_len, stored in the meta page) =====

    ! Stable max separators per internal node == max entries per leaf,
    ! using one MAXK for both with the tighter (internal) bound. A split
    ! builds the over-full node *in place* before dividing it, so a node
    ! transiently holds one more than MAXK: a leaf MAXK+1 entries; an
    ! internal node MAXK+1 separators (key_len+4 each) AND MAXK+2 child
    ! pointers (4 each). The page is sized for that peak, so the binding
    ! (internal) constraint is
    !     page_size >= 5 + (MAXK+2)*4 + (MAXK+1)*(key_len+4),
    ! which inverts to the MAXK below. The leaf peak
    ! (9 + (MAXK+1)*(key_len+4)) is looser and always fits.
    pure function maxk(bt) result(k)
        type(btree_t), intent(in) :: bt
        integer :: k
        k = (bt%page_size - bt%key_len - 17) / (bt%key_len + 8)
    end function

    pure function leaf_entry_base(bt, j) result(b)   ! 1-based, entry j (1..)
        type(btree_t), intent(in) :: bt
        integer,       intent(in) :: j
        integer :: b
        b = 10 + (j - 1) * (bt%key_len + 4)
    end function

    pure function child_off(j) result(o)             ! 1-based, child j (1..)
        integer, intent(in) :: j
        integer :: o
        o = 6 + (j - 1) * 4
    end function

    pure function sep_base(bt, j) result(b)          ! 1-based, separator j (1..)
        type(btree_t), intent(in) :: bt
        integer,       intent(in) :: j
        integer :: b
        b = 6 + (maxk(bt) + 2) * 4 + (j - 1) * (bt%key_len + 4)
    end function

    ! ===== Raw page I/O =====

    subroutine read_page(bt, pid, pg, stat)
        type(btree_t),    intent(in)  :: bt
        integer,          intent(in)  :: pid
        character(len=*), intent(out) :: pg
        integer,          intent(out) :: stat
        integer :: ios
        read(bt%unit, rec=pid, iostat=ios) pg
        stat = merge(BT_ERR, BT_OK, ios /= 0)
    end subroutine

    subroutine write_page(bt, pid, pg, stat)
        type(btree_t),    intent(in)  :: bt
        integer,          intent(in)  :: pid
        character(len=*), intent(in)  :: pg
        integer,          intent(out) :: stat
        integer :: ios
        integer(int64) :: off
        character(len=:), allocatable :: old
        ! A journal hook (if installed) must capture an undo image before the
        ! overwrite: the page's current bytes (in-place overwrite) or just the
        ! file's pre-growth length (a page newly allocated this txn -> rollback
        ! truncates).  A non-zero hook stat aborts the write so nothing slips
        ! past un-recorded.  The pre-image is read from our own unit, the one
        ! consistent view of the page.
        if (associated(bt%jhook)) then
            off = int(pid - 1, int64) * int(bt%page_size, int64) + 1_int64
            if (pid > bt%jbase) then
                call bt%jhook(bt%jctx, off, '', .true., stat)
            else
                allocate(character(len=bt%page_size) :: old)
                read(bt%unit, rec=pid, iostat=ios) old
                if (ios /= 0) then
                    stat = BT_ERR
                    return
                end if
                call bt%jhook(bt%jctx, off, old, .false., stat)
            end if
            if (stat /= BT_OK) return
        end if
        write(bt%unit, rec=pid, iostat=ios) pg
        stat = merge(BT_ERR, BT_OK, ios /= 0)
    end subroutine

    ! Install or clear the pre-write journal hook (interface in the parent).
    module subroutine bt_set_journal_hook(bt, hook, ctx)
        type(btree_t),    intent(inout)         :: bt
        procedure(bt_journal_hook),    optional :: hook
        class(*), pointer, intent(in), optional :: ctx
        if (present(hook)) then
            bt%jhook => hook
            bt%jbase =  bt%npages   ! pages above this are new this transaction
            if (present(ctx)) then
                bt%jctx => ctx
            else
                bt%jctx => null()
            end if
        else
            bt%jhook => null()
            bt%jctx  => null()
        end if
    end subroutine

    ! Meta page is page 1, written last so a torn write never corrupts a
    ! reachable tree (orphan pages are reclaimed by the next bulk_load).
    subroutine write_meta(bt, stat)
        type(btree_t), intent(in)  :: bt
        integer,       intent(out) :: stat
        character(len=:), allocatable :: pg
        allocate(character(len=bt%page_size) :: pg)
        pg = repeat(char(0), bt%page_size)
        pg(1:4) = BT_MAGIC
        call put_i32(pg, 5,  BT_BOM)
        call put_i32(pg, 9,  int(BT_FORMAT_VERSION, int32))
        call put_i32(pg, 13, int(bt%page_size,  int32))
        call put_i32(pg, 17, int(bt%key_len,    int32))
        call put_i32(pg, 21, int(bt%root,       int32))
        call put_i32(pg, 25, int(bt%free_head,  int32))
        call put_i32(pg, 29, int(bt%npages,     int32))
        call put_i32(pg, 33, int(bt%first_leaf, int32))
        call put_i64(pg, 37, bt%nentries)
        call write_page(bt, 1, pg, stat)
    end subroutine

    ! Allocate a fresh page id. Lazy delete never frees pages and
    ! bulk_load rewrites wholesale, so this only ever extends the file.
    subroutine alloc_page(bt, pid)
        type(btree_t), intent(inout) :: bt
        integer,       intent(out)   :: pid
        bt%npages = bt%npages + 1
        pid = bt%npages
    end subroutine

    ! ===== Node accessors =====

    pure function node_kind(pg) result(k)
        character(len=*), intent(in) :: pg
        integer(int8) :: k
        k = int(iachar(pg(1:1)), int8)
    end function

    pure subroutine set_node(pg, k, nkeys)
        character(len=*), intent(inout) :: pg
        integer(int8),    intent(in)    :: k
        integer(int32),   intent(in)    :: nkeys
        pg(1:1) = achar(int(k))
        call put_i32(pg, 2, nkeys)
    end subroutine

    pure function n_keys(pg) result(n)
        character(len=*), intent(in) :: pg
        integer :: n
        n = int(get_i32(pg, 2))
    end function

    ! ----- leaf -----

    pure function leaf_next(pg) result(nx)
        character(len=*), intent(in) :: pg
        integer :: nx
        nx = int(get_i32(pg, 6))
    end function

    pure subroutine set_leaf_next(pg, nx)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: nx
        call put_i32(pg, 6, int(nx, int32))
    end subroutine

    pure function leaf_key(bt, pg, j) result(k)
        type(btree_t),    intent(in)  :: bt
        character(len=*), intent(in)  :: pg
        integer,          intent(in)  :: j
        character(len=bt%key_len) :: k
        integer :: b
        b = leaf_entry_base(bt, j)
        k = pg(b : b + bt%key_len - 1)
    end function

    pure function leaf_pay(bt, pg, j) result(p)
        type(btree_t),    intent(in) :: bt
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: j
        integer(int32) :: p
        p = get_i32(pg, leaf_entry_base(bt, j) + bt%key_len)
    end function

    pure subroutine set_leaf_entry(bt, pg, j, key, pay)
        type(btree_t),    intent(in)    :: bt
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: j
        character(len=*), intent(in)    :: key
        integer(int32),   intent(in)    :: pay
        integer :: b
        b = leaf_entry_base(bt, j)
        pg(b : b + bt%key_len - 1) = key(1:bt%key_len)
        call put_i32(pg, b + bt%key_len, pay)
    end subroutine

    ! ----- internal -----

    pure function int_child(pg, j) result(c)
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: j
        integer :: c
        c = int(get_i32(pg, child_off(j)))
    end function

    pure subroutine set_int_child(pg, j, c)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: j, c
        call put_i32(pg, child_off(j), int(c, int32))
    end subroutine

    pure function int_key(bt, pg, j) result(k)
        type(btree_t),    intent(in) :: bt
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: j
        character(len=bt%key_len) :: k
        integer :: b
        b = sep_base(bt, j)
        k = pg(b : b + bt%key_len - 1)
    end function

    pure function int_pay(bt, pg, j) result(p)
        type(btree_t),    intent(in) :: bt
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: j
        integer(int32) :: p
        p = get_i32(pg, sep_base(bt, j) + bt%key_len)
    end function

    pure subroutine set_int_sep(bt, pg, j, key, pay)
        type(btree_t),    intent(in)    :: bt
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: j
        character(len=*), intent(in)    :: key
        integer(int32),   intent(in)    :: pay
        integer :: b
        b = sep_base(bt, j)
        pg(b : b + bt%key_len - 1) = key(1:bt%key_len)
        call put_i32(pg, b + bt%key_len, pay)
    end subroutine

    ! ===== (key,payload) total order =====

    ! .true. iff (ak,ap) strictly precedes (bk,bp).
    pure function pair_lt(ak, ap, bk, bp, cmp, ctx) result(lt)
        character(len=*),      intent(in) :: ak, bk
        integer(int32),        intent(in) :: ap, bp
        procedure(bt_compare)             :: cmp
        class(*),              intent(in) :: ctx
        logical :: lt
        integer :: c
        c = cmp(ak, bk, ctx)
        if (c /= 0) then
            lt = c < 0
        else
            lt = ap < bp
        end if
    end function

    ! First slot in [1 .. n_keys] whose stored (key,pay) is strictly greater
    ! than the target pair (target precedes it), or n_keys+1 if every entry is
    ! <= target.  Binary search on the (key,pay) total order, which the per-page
    ! entries are stored in; works on both leaf and internal pages.
    pure function pair_upper_bound(bt, pg, key, pay, cmp, ctx) result(ub)
        type(btree_t),         intent(in) :: bt
        character(len=*),      intent(in) :: pg
        character(len=*),      intent(in) :: key
        integer(int32),        intent(in) :: pay
        procedure(bt_compare)             :: cmp
        class(*),              intent(in) :: ctx
        integer :: ub, lo, hi, mid, nk
        logical :: leaf
        character(len=bt%key_len) :: kmid
        integer(int32) :: pmid
        nk   = n_keys(pg)
        leaf = node_kind(pg) == K_LEAF
        lo   = 1
        hi   = nk + 1
        narrow: do while (lo < hi)
            mid = lo + (hi - lo) / 2
            if (leaf) then
                kmid = leaf_key(bt, pg, mid)
                pmid = leaf_pay(bt, pg, mid)
            else
                kmid = int_key(bt, pg, mid)
                pmid = int_pay(bt, pg, mid)
            end if
            if (pair_lt(key, pay, kmid, pmid, cmp, ctx)) then
                hi = mid          ! target < stored_mid -> answer is <= mid
            else
                lo = mid + 1      ! stored_mid <= target -> answer is > mid
            end if
        end do narrow
        ub = lo
    end function

    ! First slot in [1 .. n_keys] whose key compares >= target (key only),
    ! or n_keys+1 if every key is strictly less.  Binary search: the per-page
    ! keys are stored in non-decreasing key order, so a lower bound on the
    ! key alone is well defined on both leaf and internal pages.
    pure function key_lower_bound(bt, pg, key, cmp, ctx) result(lb)
        type(btree_t),         intent(in) :: bt
        character(len=*),      intent(in) :: pg
        character(len=*),      intent(in) :: key
        procedure(bt_compare)             :: cmp
        class(*),              intent(in) :: ctx
        integer :: lb, lo, hi, mid, nk
        logical :: leaf
        character(len=bt%key_len) :: kmid
        nk   = n_keys(pg)
        leaf = node_kind(pg) == K_LEAF
        lo   = 1
        hi   = nk + 1
        narrow: do while (lo < hi)
            mid = lo + (hi - lo) / 2
            if (leaf) then
                kmid = leaf_key(bt, pg, mid)
            else
                kmid = int_key(bt, pg, mid)
            end if
            if (cmp(kmid, key, ctx) < 0) then
                lo = mid + 1
            else
                hi = mid
            end if
        end do narrow
        lb = lo
    end function

    ! Child slot (1..nk+1) to descend into for (key,pay): first separator
    ! the pair precedes, else the last child.
    pure function route(bt, pg, key, pay, cmp, ctx) result(ci)
        type(btree_t),         intent(in) :: bt
        character(len=*),      intent(in) :: pg
        character(len=*),      intent(in) :: key
        integer(int32),        intent(in) :: pay
        procedure(bt_compare)             :: cmp
        class(*),              intent(in) :: ctx
        integer :: ci
        ci = pair_upper_bound(bt, pg, key, pay, cmp, ctx)
    end function

    ! ===== Open / close =====

    module subroutine bt_open(bt, path, key_len, writable, create, stat)
        type(btree_t),    intent(out) :: bt
        character(len=*), intent(in)  :: path
        integer,          intent(in)  :: key_len
        logical,          intent(in)  :: writable
        logical,          intent(in)  :: create
        integer,          intent(out) :: stat
        integer :: u, ios, ps, entry, need
        character(len=9) :: act
        character(len=:), allocatable :: pg

        if (key_len < 1) then
            stat = BT_CORRUPT
            return
        end if
        act = merge('readwrite', 'read     ', writable)

        if (create) then
            ! Creating a tree means writing its initial pages, so a read-only
            ! create is contradictory — reject it rather than silently handing
            ! back a writable tree (the create branch always opens readwrite).
            if (.not. writable) then
                stat = BT_ERR
                return
            end if
            ! Page must hold a healthy fan-out even for very large keys;
            ! size so MAXK >= 32 under the split-aware geometry above
            ! (page_size - key_len - 17 >= 32*(key_len+8)), rounded up to
            ! a 512-byte multiple, never below 4096.
            entry = key_len + 8
            need  = 33 * entry + 9
            ps    = max(4096, ((need + 511) / 512) * 512)
            open(newunit=u, file=path, access='direct', form='unformatted', &
                 recl=ps, status='replace', action='readwrite', iostat=ios)
            if (ios /= 0) then
                stat = BT_ERR
                return
            end if
            bt%unit       = u
            bt%page_size  = ps
            bt%key_len    = key_len
            bt%free_head  = 0
            bt%npages     = 1
            bt%nentries   = 0_int64
            bt%writable   = .true.
            ! One empty leaf as the root.
            call alloc_page(bt, bt%root)
            bt%first_leaf = bt%root
            allocate(character(len=ps) :: pg)
            pg = repeat(char(0), ps)
            call set_node(pg, K_LEAF, 0_int32)
            call set_leaf_next(pg, 0)
            call write_page(bt, bt%root, pg, stat)
            if (stat /= BT_OK) return
            call write_meta(bt, stat)
            return
        end if

        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            stat = BT_ERR
            return
        end if
        read_meta: block
            character(len=44) :: hdr
            integer(int32)    :: bom
            read(u, iostat=ios) hdr
            close(u)
            if (ios /= 0) then
                stat = BT_ERR
                return
            end if
            if (hdr(1:4) /= BT_MAGIC) then
                stat = BT_CORRUPT
                return
            end if
            ! Byte-order mark: checked before any other scalar, since a
            ! wrong-endian file misreads every int field below.
            bom = get_i32(hdr, 5)
            if (bom /= BT_BOM) then
                stat = merge(BT_VERSION, BT_CORRUPT, bom == BT_BOM_SWAP)
                return
            end if
            if (get_i32(hdr, 9) /= BT_FORMAT_VERSION) then
                stat = BT_VERSION
                return
            end if
            bt%page_size  = int(get_i32(hdr, 13))
            bt%key_len    = int(get_i32(hdr, 17))
            bt%root       = int(get_i32(hdr, 21))
            bt%free_head  = int(get_i32(hdr, 25))
            bt%npages     = int(get_i32(hdr, 29))
            bt%first_leaf = int(get_i32(hdr, 33))
            bt%nentries   = get_i64(hdr, 37)
        end block read_meta
        ! Geometry must be self-consistent, not merely individually plausible.
        ! page_size >= 64 is not enough: a page too small for the (matching)
        ! key_len drives maxk <= 0, after which the first split writes slots
        ! past the page buffer — the silent out-of-bounds class the 2026-06-08
        ! CRITICAL fix closed, reachable again via a corrupt meta page. Mirror
        ! the create-time bound (MAXK >= 32). first_leaf <= npages and
        ! nentries >= 0 are free corruption catches while we are here.
        if (bt%key_len /= key_len .or. bt%page_size < 64 .or.        &
            bt%root < 2 .or. bt%npages < bt%root .or.               &
            bt%first_leaf < 2 .or. bt%first_leaf > bt%npages .or.   &
            maxk(bt) < 32 .or. bt%nentries < 0) then
            stat = BT_CORRUPT
            return
        end if
        open(newunit=u, file=path, access='direct', form='unformatted', &
             recl=bt%page_size, status='old', action=trim(act), iostat=ios)
        if (ios /= 0) then
            stat = BT_ERR
            return
        end if
        bt%unit     = u
        bt%writable = writable
        stat = BT_OK
    end subroutine

    module subroutine bt_close(bt, stat)
        type(btree_t), intent(inout)         :: bt
        integer,       intent(out), optional :: stat
        integer :: rs
        rs = BT_OK
        if (bt%unit /= -1) then
            if (bt%writable) call write_meta(bt, rs)
            close(bt%unit)
            bt%unit = -1
        end if
        if (present(stat)) stat = rs
    end subroutine

    ! Flush a writable tree's buffered writes to the OS (interface in the
    ! parent).  Durability (the actual fsync) is the journal layer's job, done
    ! by path; this only drains the unit so that fsync sees every page write.
    module subroutine bt_sync(bt, stat)
        type(btree_t), intent(in)            :: bt
        integer,       intent(out), optional :: stat
        integer :: ios
        if (present(stat)) stat = BT_OK
        if (bt%unit /= -1 .and. bt%writable) then
            flush(bt%unit, iostat=ios)
            if (ios /= 0 .and. present(stat)) stat = BT_ERR
        end if
    end subroutine

    ! Re-sync the cached meta fields from the on-disk meta page (page 1)
    ! through the already-open unit.  Used after a journal rollback has
    ! restored the file: the in-memory root/npages/etc. then describe the
    ! pre-rollback tree and must be reloaded.  page_size/key_len are fixed at
    ! open; a stored mismatch means the meta page was corrupted underneath us.
    module subroutine bt_reload(bt, stat)
        type(btree_t), intent(inout) :: bt
        integer,       intent(out)   :: stat
        character(len=:), allocatable :: pg
        if (bt%unit == -1) then
            stat = BT_ERR
            return
        end if
        allocate(character(len=bt%page_size) :: pg)
        call read_page(bt, 1, pg, stat)
        if (stat /= BT_OK) return
        if (pg(1:4) /= BT_MAGIC .or. get_i32(pg, 5) /= BT_BOM .or. &
            get_i32(pg, 9) /= int(BT_FORMAT_VERSION, int32) .or.   &
            int(get_i32(pg, 13)) /= bt%page_size .or.              &
            int(get_i32(pg, 17)) /= bt%key_len) then
            stat = BT_CORRUPT
            return
        end if
        bt%root       = int(get_i32(pg, 21))
        bt%free_head  = int(get_i32(pg, 25))
        bt%npages     = int(get_i32(pg, 29))
        bt%first_leaf = int(get_i32(pg, 33))
        bt%nentries   = get_i64(pg, 37)
        ! Same self-consistency bound bt_open enforces on a freshly read tree.
        if (bt%root < 2 .or. bt%npages < bt%root .or.            &
            bt%first_leaf < 2 .or. bt%first_leaf > bt%npages .or. &
            maxk(bt) < 32 .or. bt%nentries < 0) then
            stat = BT_CORRUPT
            return
        end if
        stat = BT_OK
    end subroutine

    ! ===== Insert =====

    ! Recursive descent. On return split=.true. means this node was split:
    ! its right half is page `right_pid` and (up_key,up_pay) is the
    ! separator the parent must adopt (the right half's first entry).
    recursive subroutine ins(bt, pid, key, pay, cmp, ctx, &
                             split, up_key, up_pay, right_pid, stat)
        type(btree_t),         intent(inout) :: bt
        integer,               intent(in)    :: pid
        character(len=*),      intent(in)    :: key
        integer(int32),        intent(in)    :: pay
        procedure(bt_compare)                :: cmp
        class(*),              intent(in)    :: ctx
        logical,               intent(out)   :: split
        character(len=*),      intent(inout) :: up_key
        integer(int32),        intent(out)   :: up_pay
        integer,               intent(out)   :: right_pid
        integer,               intent(out)   :: stat
        character(len=:), allocatable :: pg, rpg
        integer :: nk, j, k, mk, ln, rn, ci, mid
        logical :: csplit
        character(len=:), allocatable :: ckey
        integer(int32) :: cpay

        split = .false.
        right_pid = 0
        up_pay = 0_int32
        allocate(character(len=bt%page_size) :: pg)
        call read_page(bt, pid, pg, stat)
        if (stat /= BT_OK) return
        mk = maxk(bt)

        if (node_kind(pg) == K_LEAF) then
            nk = n_keys(pg)
            ! Find slot: first entry the new pair precedes (binary search).
            j = pair_upper_bound(bt, pg, key, pay, cmp, ctx)
            ! Shift [j..nk] up by one, drop the new entry in at j.
            shift: do k = nk, j, -1
                call set_leaf_entry(bt, pg, k + 1, &
                                    leaf_key(bt, pg, k), leaf_pay(bt, pg, k))
            end do shift
            call set_leaf_entry(bt, pg, j, key, pay)
            nk = nk + 1
            if (nk <= mk) then
                call set_node(pg, K_LEAF, int(nk, int32))
                call write_page(bt, pid, pg, stat)
                if (stat /= BT_OK) return   ! nothing landed: don't count it
                bt%nentries = bt%nentries + 1_int64
                return
            end if
            ! Overflow: split into pid (lower) + a new right leaf.
            ln = nk / 2
            rn = nk - ln
            call alloc_page(bt, right_pid)
            allocate(character(len=bt%page_size) :: rpg)
            rpg = repeat(char(0), bt%page_size)
            move_r: do k = 1, rn
                call set_leaf_entry(bt, rpg, k, &
                     leaf_key(bt, pg, ln + k), leaf_pay(bt, pg, ln + k))
            end do move_r
            call set_node(rpg, K_LEAF, int(rn, int32))
            call set_leaf_next(rpg, leaf_next(pg))
            call set_node(pg, K_LEAF, int(ln, int32))
            call set_leaf_next(pg, right_pid)
            up_key(1:bt%key_len) = leaf_key(bt, rpg, 1)
            up_pay = leaf_pay(bt, rpg, 1)
            ! Write the new right leaf BEFORE the left: the left's next-pointer
            ! already refers to right_pid, so a crash between the two writes must
            ! leave right durable (else the leaf chain would dangle). Reversed,
            ! a crash leaves right an unreferenced orphan and left unchanged —
            ! the coherent-tree guarantee (b_tree.f90:12-14).
            call write_page(bt, right_pid, rpg, stat)
            if (stat /= BT_OK) return
            call write_page(bt, pid, pg, stat)
            if (stat /= BT_OK) return
            bt%nentries = bt%nentries + 1_int64
            split = .true.
            return
        end if

        ! Internal node: descend, then absorb a child split if it happened.
        ci = route(bt, pg, key, pay, cmp, ctx)
        allocate(character(len=bt%key_len) :: ckey)
        call ins(bt, int_child(pg, ci), key, pay, cmp, ctx, &
                 csplit, ckey, cpay, right_pid, stat)
        if (stat /= BT_OK) return
        if (.not. csplit) return

        nk = n_keys(pg)
        ! Insert separator (ckey,cpay) at position ci and the new child
        ! at ci+1: shift separators [ci..nk] and children [ci+1..nk+1] up.
        sshift: do k = nk, ci, -1
            call set_int_sep(bt, pg, k + 1, int_key(bt, pg, k), int_pay(bt, pg, k))
        end do sshift
        cshift: do k = nk + 1, ci + 1, -1
            call set_int_child(pg, k + 1, int_child(pg, k))
        end do cshift
        call set_int_sep(bt, pg, ci, ckey, cpay)
        call set_int_child(pg, ci + 1, right_pid)
        nk = nk + 1
        if (nk <= mk) then
            call set_node(pg, K_INT, int(nk, int32))
            call write_page(bt, pid, pg, stat)
            split = .false.
            return
        end if
        ! Internal overflow: middle separator moves up; children split
        ! with it (left keeps 1..mid children, right gets the rest).
        mid = nk / 2 + 1
        up_key(1:bt%key_len) = int_key(bt, pg, mid)
        up_pay = int_pay(bt, pg, mid)
        call alloc_page(bt, right_pid)
        allocate(character(len=bt%page_size) :: rpg)
        rpg = repeat(char(0), bt%page_size)
        rn = nk - mid
        rsep: do k = 1, rn
            call set_int_sep(bt, rpg, k, &
                 int_key(bt, pg, mid + k), int_pay(bt, pg, mid + k))
        end do rsep
        rchild: do k = 1, rn + 1
            call set_int_child(rpg, k, int_child(pg, mid + k))
        end do rchild
        call set_node(rpg, K_INT, int(rn, int32))
        call set_node(pg,  K_INT, int(mid - 1, int32))
        ! Right before left: truncating the left node first would, on a crash
        ! before the right write, orphan the upper-half child subtrees (dropped
        ! by left, not yet held by right). Written this way the moved children
        ! stay reachable via the still-intact left node until the parent adopts
        ! right; right is at worst an unreferenced orphan.
        call write_page(bt, right_pid, rpg, stat)
        if (stat /= BT_OK) return
        call write_page(bt, pid, pg, stat)
        if (stat /= BT_OK) return
        split = .true.
    end subroutine

    module subroutine bt_insert(bt, key, payload, cmp, ctx, stat)
        type(btree_t),    intent(inout) :: bt
        character(len=*), intent(in)    :: key
        integer(int32),   intent(in)    :: payload
        procedure(bt_compare)           :: cmp
        class(*),         intent(in)    :: ctx
        integer,          intent(out)   :: stat
        logical :: split
        integer :: right_pid, newroot
        integer(int32) :: up_pay
        character(len=:), allocatable :: up_key, pg
        allocate(character(len=bt%key_len) :: up_key)
        call ins(bt, bt%root, key, payload, cmp, ctx, &
                 split, up_key, up_pay, right_pid, stat)
        if (stat /= BT_OK) return
        if (split) then
            ! Tree grew: new root over the old root and its new sibling.
            call alloc_page(bt, newroot)
            allocate(character(len=bt%page_size) :: pg)
            pg = repeat(char(0), bt%page_size)
            call set_node(pg, K_INT, 1_int32)
            call set_int_child(pg, 1, bt%root)
            call set_int_child(pg, 2, right_pid)
            call set_int_sep(bt, pg, 1, up_key, up_pay)
            call write_page(bt, newroot, pg, stat)
            if (stat /= BT_OK) return
            bt%root = newroot
        end if
        call write_meta(bt, stat)
    end subroutine

    ! ===== Remove (lazy) =====

    module subroutine bt_remove(bt, key, payload, cmp, ctx, found, stat)
        type(btree_t),    intent(inout) :: bt
        character(len=*), intent(in)    :: key
        integer(int32),   intent(in)    :: payload
        procedure(bt_compare)           :: cmp
        class(*),         intent(in)    :: ctx
        logical,          intent(out)   :: found
        integer,          intent(out)   :: stat
        character(len=:), allocatable :: pg
        integer :: pid, nk, j, k

        found = .false.
        allocate(character(len=bt%page_size) :: pg)
        pid = bt%root
        descend: do
            call read_page(bt, pid, pg, stat)
            if (stat /= BT_OK) return
            if (node_kind(pg) == K_LEAF) exit descend
            pid = int_child(pg, route(bt, pg, key, payload, cmp, ctx))
        end do descend

        ! The exact pair, if present, is the entry just before the first one
        ! strictly greater than it (entries are in (key,pay) order).
        nk = n_keys(pg)
        j  = pair_upper_bound(bt, pg, key, payload, cmp, ctx) - 1
        if (j >= 1) then
            if (cmp(leaf_key(bt, pg, j), key, ctx) == 0 .and. &
                leaf_pay(bt, pg, j) == payload) then
                shift: do k = j, nk - 1
                    call set_leaf_entry(bt, pg, k, &
                         leaf_key(bt, pg, k + 1), leaf_pay(bt, pg, k + 1))
                end do shift
                call set_node(pg, K_LEAF, int(nk - 1, int32))
                call write_page(bt, pid, pg, stat)
                if (stat /= BT_OK) return
                bt%nentries = bt%nentries - 1_int64
                found = .true.
                call write_meta(bt, stat)
                return
            end if
        end if
        stat = BT_OK
    end subroutine

    ! ===== Cursors =====

    module subroutine bt_first(bt, cur, stat)
        type(btree_t),     intent(in)  :: bt
        type(bt_cursor_t), intent(out) :: cur
        integer,           intent(out) :: stat
        cur%leaf  = bt%first_leaf
        cur%slot  = 0
        cur%valid = .true.
        stat = BT_OK
    end subroutine

    module subroutine bt_seek(bt, key, cmp, ctx, cur, stat)
        type(btree_t),     intent(in)  :: bt
        character(len=*),  intent(in)  :: key
        procedure(bt_compare)          :: cmp
        class(*),          intent(in)  :: ctx
        type(bt_cursor_t), intent(out) :: cur
        integer,           intent(out) :: stat
        character(len=:), allocatable :: pg
        integer :: pid, slot

        cur%valid = .false.
        cur%leaf  = 0
        cur%slot  = 0
        allocate(character(len=bt%page_size) :: pg)
        pid = bt%root
        ! Lower bound on key alone: at each internal node take the first
        ! child whose separator key is not less than the target, so we
        ! never start past the first matching entry.
        descend: do
            call read_page(bt, pid, pg, stat)
            if (stat /= BT_OK) return
            if (node_kind(pg) == K_LEAF) exit descend
            pid = int_child(pg, key_lower_bound(bt, pg, key, cmp, ctx))
        end do descend

        ! First leaf slot whose key is >= target; cursor sits one before it
        ! so bt_next yields it (slot = n_keys -> past end, rolls to next leaf).
        slot = key_lower_bound(bt, pg, key, cmp, ctx) - 1
        cur%leaf  = pid
        cur%slot  = slot
        cur%valid = .true.
        stat = BT_OK
    end subroutine

    module subroutine bt_next(bt, cur, key, payload, ok, stat)
        type(btree_t),     intent(in)    :: bt
        type(bt_cursor_t), intent(inout) :: cur
        character(len=*),  intent(out)   :: key
        integer(int32),    intent(out)   :: payload
        logical,           intent(out)   :: ok
        integer,           intent(out)   :: stat
        integer :: nk

        ok      = .false.
        payload = 0_int32
        stat    = BT_OK
        if (.not. cur%valid .or. cur%leaf == 0) return
        advance: do
            ! Serve the current leaf from the cursor's one-page cache: a range
            ! scan yields many keys from the same leaf, so this reads that page
            ! once rather than on every step. Moving to leaf_next (or a fresh
            ! cursor, which is intent(out) so cpg auto-deallocates) refreshes it.
            if (cur%cpid /= cur%leaf .or. .not. allocated(cur%cpg)) then
                if (.not. allocated(cur%cpg)) &
                    allocate(character(len=bt%page_size) :: cur%cpg)
                call read_page(bt, cur%leaf, cur%cpg, stat)
                if (stat /= BT_OK) return
                cur%cpid = cur%leaf
            end if
            nk = n_keys(cur%cpg)
            if (cur%slot < nk) then
                key(1:bt%key_len) = leaf_key(bt, cur%cpg, cur%slot + 1)
                payload = leaf_pay(bt, cur%cpg, cur%slot + 1)
                cur%slot = cur%slot + 1
                ok = .true.
                return
            end if
            cur%leaf = leaf_next(cur%cpg)
            cur%slot = 0
            if (cur%leaf == 0) then
                cur%valid = .false.
                return
            end if
        end do advance
    end subroutine

    ! ===== Bulk load (O(N log N), perfectly packed) =====

    module subroutine bt_bulk_load(bt, keys, payloads, cmp, ctx, stat)
        type(btree_t),    intent(inout) :: bt
        character(len=*), intent(in)    :: keys(:)
        integer(int32),   intent(in)    :: payloads(:)
        procedure(bt_compare)           :: cmp
        class(*),         intent(in)    :: ctx
        integer,          intent(out)   :: stat
        integer :: n, mk, i, k, nleaf, fill, rem, base, e, cnt
        integer :: nlev, gi, parent, c0, c1, child
        integer, allocatable :: perm(:)
        integer, allocatable :: lev_id(:), nxt_id(:)
        character(len=:), allocatable :: pg
        character(len=:), allocatable :: lev_lk(:), nxt_lk(:)
        integer(int32), allocatable   :: lev_lp(:), nxt_lp(:)

        n  = size(keys)
        mk = maxk(bt)
        ! Reset to just the meta page; everything is rewritten below.
        bt%npages   = 1
        bt%nentries = int(n, int64)
        allocate(character(len=bt%page_size) :: pg)

        if (n == 0) then
            call alloc_page(bt, bt%root)
            bt%first_leaf = bt%root
            pg = repeat(char(0), bt%page_size)
            call set_node(pg, K_LEAF, 0_int32)
            call set_leaf_next(pg, 0)
            call write_page(bt, bt%root, pg, stat)
            if (stat /= BT_OK) return
            call write_meta(bt, stat)
            return
        end if

        ! Sort a permutation by the (key,payload) total order (heapsort:
        ! O(N log N) worst case, no recursion).
        allocate(perm(n))
        do i = 1, n              ! identity permutation; loop avoids the
            perm(i) = i          ! implied-do constructor temporary
        end do
        call heapsort(perm, keys, payloads, cmp, ctx)

        ! --- leaf level: pack mk entries per leaf ---
        nleaf = (n + mk - 1) / mk
        allocate(lev_id(nleaf))
        allocate(character(len=bt%key_len) :: lev_lk(nleaf))
        allocate(lev_lp(nleaf))
        ! Even out the last two leaves so none is left tiny.
        fill = n / nleaf
        rem  = n - fill * nleaf
        base = 0
        leaves: do i = 1, nleaf
            cnt = fill
            if (i <= rem) cnt = cnt + 1
            call alloc_page(bt, lev_id(i))
            if (i == 1) bt%first_leaf = lev_id(1)  ! first leaf id, known as soon as it is allocated
            pg = repeat(char(0), bt%page_size)
            pack: do k = 1, cnt
                e = perm(base + k)
                call set_leaf_entry(bt, pg, k, keys(e), payloads(e))
            end do pack
            call set_node(pg, K_LEAF, int(cnt, int32))
            if (i < nleaf) then
                call set_leaf_next(pg, lev_id(i) + 1)  ! next leaf is the next page
            else
                call set_leaf_next(pg, 0)
            end if
            call write_page(bt, lev_id(i), pg, stat)
            if (stat /= BT_OK) return
            lev_lk(i)(1:bt%key_len) = keys(perm(base + 1))
            lev_lp(i) = payloads(perm(base + 1))
            base = base + cnt
        end do leaves

        ! --- internal levels: group up to mk+1 children per node ---
        build: do
            if (size(lev_id) == 1) then
                bt%root = lev_id(1)
                exit build
            end if
            nlev = (size(lev_id) + mk) / (mk + 1)
            allocate(nxt_id(nlev))
            allocate(character(len=bt%key_len) :: nxt_lk(nlev))
            allocate(nxt_lp(nlev))
            c0 = 1
            groups: do gi = 1, nlev
                c1 = min(c0 + mk, size(lev_id))      ! up to mk+1 children
                call alloc_page(bt, parent)
                pg = repeat(char(0), bt%page_size)
                call set_node(pg, K_INT, int(c1 - c0, int32))
                child = 0
                kids: do k = c0, c1
                    child = child + 1
                    call set_int_child(pg, child, lev_id(k))
                    if (k > c0) call set_int_sep(bt, pg, child - 1, &
                                                 lev_lk(k), lev_lp(k))
                end do kids
                call write_page(bt, parent, pg, stat)
                if (stat /= BT_OK) return
                nxt_id(gi)            = parent
                nxt_lk(gi)(1:bt%key_len) = lev_lk(c0)
                nxt_lp(gi)            = lev_lp(c0)
                c0 = c1 + 1
            end do groups
            call move_alloc(nxt_id, lev_id)
            call move_alloc(nxt_lk, lev_lk)
            call move_alloc(nxt_lp, lev_lp)
        end do build

        call write_meta(bt, stat)
    end subroutine

    ! In-place heapsort of perm so that
    ! (keys(perm(i)),payloads(perm(i))) is ascending in the total order.
    subroutine heapsort(perm, keys, payloads, cmp, ctx)
        integer,          intent(inout) :: perm(:)
        character(len=*), intent(in)    :: keys(:)
        integer(int32),   intent(in)    :: payloads(:)
        procedure(bt_compare)           :: cmp
        class(*),         intent(in)    :: ctx
        integer :: n, i, tmp
        n = size(perm)
        heapify: do i = n / 2, 1, -1
            call sift(perm, keys, payloads, cmp, ctx, i, n)
        end do heapify
        sortdown: do i = n, 2, -1
            tmp = perm(1); perm(1) = perm(i); perm(i) = tmp
            call sift(perm, keys, payloads, cmp, ctx, 1, i - 1)
        end do sortdown
    end subroutine

    subroutine sift(perm, keys, payloads, cmp, ctx, lo, hi)
        integer,          intent(inout) :: perm(:)
        character(len=*), intent(in)    :: keys(:)
        integer(int32),   intent(in)    :: payloads(:)
        procedure(bt_compare)           :: cmp
        class(*),         intent(in)    :: ctx
        integer,          intent(in)    :: lo, hi
        integer :: root, child, tmp
        root = lo
        descend: do
            child = 2 * root
            if (child > hi) exit descend
            if (child < hi) then
                if (pair_lt(keys(perm(child)),   payloads(perm(child)),   &
                            keys(perm(child+1)), payloads(perm(child+1)), &
                            cmp, ctx)) child = child + 1
            end if
            if (.not. pair_lt(keys(perm(root)),  payloads(perm(root)), &
                              keys(perm(child)), payloads(perm(child)), &
                              cmp, ctx)) exit descend
            tmp = perm(root); perm(root) = perm(child); perm(child) = tmp
            root = child
        end do descend
    end subroutine

end submodule b_tree_sm

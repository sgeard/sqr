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
! On-disk invariants. Every scalar above is untrusted data read back from a
! file that a crash, a bad disk or a hostile edit may have changed, so each is
! checked before it can steer an access:
!   file length    == page_size * npages                     (bt_open)
!   root, first_leaf in 2..npages, maxk >= MIN_FANOUT,
!   key_len match                                            (bt_open)
!   node kind      == K_LEAF or K_INT                        (check_node)
!   nkeys          in 0..MAXK                                (check_node)
!   leaf next id   == 0 or in 2..npages                      (check_node)
!   child id       in 2..npages                              (child_pid)
!   descent depth  <= npages, cursor leaf hops <= npages     (loop ceilings)
! MAXK is the stable per-node bound: a node is split whenever it would exceed
! it (ins) and bt_bulk_load never packs more, so a page this library wrote is
! always within it -- the transient MAXK+1 exists only in memory during a
! split. Anything outside these ranges is BT_CORRUPT; before they were
! enforced, one wrong nkeys int32 turned an ordinary insert into a heap write
! past the page buffer.
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

    ! ----- Scalar widths (bytes) -----
    integer, parameter :: I32_BYTES     = storage_size(0_int32) / 8
    integer, parameter :: I64_BYTES     = storage_size(0_int64) / 8
    integer, parameter :: KIND_BYTES    = 1           ! node-kind tag (K_LEAF / K_INT)
    integer, parameter :: PID_BYTES     = I32_BYTES   ! a page id: child pointer, leaf chain
    integer, parameter :: PAYLOAD_BYTES = I32_BYTES   ! the payload stored beside each key

    ! ----- Page ids -----
    integer, parameter :: META_PID       = 1   ! the meta page
    integer, parameter :: FIRST_BODY_PID = 2   ! lowest leaf / internal page id
    integer, parameter :: NO_PAGE        = 0   ! "none": end of leaf chain, empty free list

    ! ----- Meta page layout (1-based byte offsets; each field follows the last) -----
    integer, parameter :: META_MAGIC_OFF      = 1
    integer, parameter :: META_BOM_OFF        = META_MAGIC_OFF      + len(BT_MAGIC)
    integer, parameter :: META_VERSION_OFF    = META_BOM_OFF        + I32_BYTES
    integer, parameter :: META_PAGE_SIZE_OFF  = META_VERSION_OFF    + I32_BYTES
    integer, parameter :: META_KEY_LEN_OFF    = META_PAGE_SIZE_OFF  + I32_BYTES
    integer, parameter :: META_ROOT_OFF       = META_KEY_LEN_OFF    + I32_BYTES
    integer, parameter :: META_FREE_HEAD_OFF  = META_ROOT_OFF       + I32_BYTES
    integer, parameter :: META_NPAGES_OFF     = META_FREE_HEAD_OFF  + I32_BYTES
    integer, parameter :: META_FIRST_LEAF_OFF = META_NPAGES_OFF     + I32_BYTES
    integer, parameter :: META_NENTRIES_OFF   = META_FIRST_LEAF_OFF + I32_BYTES
    integer, parameter :: META_BYTES          = META_NENTRIES_OFF   + I64_BYTES - 1

    ! ----- Node page layout -----
    ! Every node starts (kind, nkeys); a leaf follows that with its next-leaf
    ! id, an internal node with its child-id area.
    integer, parameter :: KIND_OFF       = 1
    integer, parameter :: NKEYS_OFF      = KIND_OFF + KIND_BYTES
    integer, parameter :: NODE_HDR_BYTES = KIND_BYTES + I32_BYTES
    integer, parameter :: LEAF_NEXT_OFF  = NODE_HDR_BYTES + 1
    integer, parameter :: LEAF_HDR_BYTES = NODE_HDR_BYTES + PID_BYTES

    ! A split builds the over-full node in place before dividing it, so every
    ! slot area is this many slots wider than the stable capacity MAXK.
    integer, parameter :: SPLIT_SLACK = 1

    ! ----- Page sizing -----
    integer, parameter :: MIN_FANOUT    = 32     ! smallest MAXK a tree may have
    integer, parameter :: MIN_PAGE_SIZE = 4096   ! page size for all but very large keys
    integer, parameter :: PAGE_ALIGN    = 512    ! page size is a multiple of this
    ! Coarse floor on a stored page_size, cheap to test first; the binding
    ! bound is MAXK >= MIN_FANOUT.
    integer, parameter :: PAGE_SIZE_FLOOR = 64

contains

    ! ===== Scalar <-> byte helpers (1-based offsets) =====

    pure function get_i32(pg, off) result(v)
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: off
        integer(int32) :: v
        v = transfer(pg(off : off + I32_BYTES - 1), v)
    end function

    pure subroutine put_i32(pg, off, v)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: off
        integer(int32),   intent(in)    :: v
        associate (hi => off + I32_BYTES - 1)
            pg(off:hi) = transfer(v, pg(off:hi))   ! mold supplies type/len only
        end associate
    end subroutine

    pure function get_i64(pg, off) result(v)
        character(len=*), intent(in) :: pg
        integer,          intent(in) :: off
        integer(int64) :: v
        v = transfer(pg(off : off + I64_BYTES - 1), v)
    end function

    pure subroutine put_i64(pg, off, v)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: off
        integer(int64),   intent(in)    :: v
        associate (hi => off + I64_BYTES - 1)
            pg(off:hi) = transfer(v, pg(off:hi))   ! mold supplies type/len only
        end associate
    end subroutine

    ! ===== Geometry (derived from key_len, stored in the meta page) =====

    ! Bytes of one stored entry: a leaf entry or an internal separator.
    pure function entry_bytes(key_len) result(n)
        integer, intent(in) :: key_len
        integer :: n
        n = key_len + PAYLOAD_BYTES
    end function

    ! Page bytes that do not scale with MAXK in an internal node: the header
    ! plus the slots that exist whatever the fan-out. A node of K separators
    ! has K+1 children, and the split slack adds SPLIT_SLACK more of each.
    pure function fixed_bytes(key_len) result(n)
        integer, intent(in) :: key_len
        integer :: n
        n = NODE_HDR_BYTES + (1 + SPLIT_SLACK) * PID_BYTES &
            + SPLIT_SLACK * entry_bytes(key_len)
    end function

    ! Page bytes each unit of MAXK costs an internal node: one separator and
    ! one child id.
    pure function per_key_bytes(key_len) result(n)
        integer, intent(in) :: key_len
        integer :: n
        n = entry_bytes(key_len) + PID_BYTES
    end function

    ! Stable max separators per internal node == max entries per leaf,
    ! using one MAXK for both with the tighter (internal) bound. A split
    ! builds the over-full node *in place* before dividing it, so a node
    ! transiently holds SPLIT_SLACK more than MAXK: a leaf MAXK+1 entries; an
    ! internal node MAXK+1 separators AND MAXK+2 child ids. The page is sized
    ! for that peak, so the binding (internal) constraint is
    !     page_size >= NODE_HDR_BYTES + (MAXK+2)*PID_BYTES
    !                                 + (MAXK+1)*entry_bytes
    !               == fixed_bytes + MAXK*per_key_bytes,
    ! which inverts to the MAXK below. The leaf peak
    ! (LEAF_HDR_BYTES + (MAXK+1)*entry_bytes) is looser and always fits.
    pure function maxk(bt) result(k)
        type(btree_t), intent(in) :: bt
        integer :: k
        k = (bt%page_size - fixed_bytes(bt%key_len)) / per_key_bytes(bt%key_len)
    end function

    ! Smallest page that gives MAXK >= MIN_FANOUT: the constraint above, taken
    ! at MAXK == MIN_FANOUT.
    pure function min_page_bytes(key_len) result(n)
        integer, intent(in) :: key_len
        integer :: n
        n = fixed_bytes(key_len) + MIN_FANOUT * per_key_bytes(key_len)
    end function

    pure function leaf_entry_base(bt, j) result(b)   ! 1-based, entry j (1..)
        type(btree_t), intent(in) :: bt
        integer,       intent(in) :: j
        integer :: b
        b = LEAF_HDR_BYTES + 1 + (j - 1) * entry_bytes(bt%key_len)
    end function

    pure function child_off(j) result(o)             ! 1-based, child j (1..)
        integer, intent(in) :: j
        integer :: o
        o = NODE_HDR_BYTES + 1 + (j - 1) * PID_BYTES
    end function

    pure function sep_base(bt, j) result(b)          ! 1-based, separator j (1..)
        type(btree_t), intent(in) :: bt
        integer,       intent(in) :: j
        integer :: b
        b = NODE_HDR_BYTES + 1 + (maxk(bt) + 1 + SPLIT_SLACK) * PID_BYTES &
            + (j - 1) * entry_bytes(bt%key_len)
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

    ! Read a leaf/internal page and validate its header before anyone indexes
    ! by it. Every body-page read goes through here; read_page stays raw for
    ! the meta page (bt_reload) and for write_page's own pre-image read, which
    ! copies bytes without interpreting them.
    subroutine read_node(bt, pid, pg, stat)
        type(btree_t),    intent(in)  :: bt
        integer,          intent(in)  :: pid
        character(len=*), intent(out) :: pg
        integer,          intent(out) :: stat
        call read_page(bt, pid, pg, stat)
        if (stat /= BT_OK) return
        call check_node(bt, pg, stat)
    end subroutine

    ! The on-disk page invariants (see the header comment). O(1): only the
    ! fields that steer an access are checked, and the child ids are bounded
    ! individually by child_pid at the point each one is followed, so a page
    ! read never costs a sweep over its slots.
    subroutine check_node(bt, pg, stat)
        type(btree_t),    intent(in)  :: bt
        character(len=*), intent(in)  :: pg
        integer,          intent(out) :: stat
        integer :: nk, nx
        stat = BT_CORRUPT
        associate (nkind => node_kind(pg))
            if (nkind /= K_LEAF .and. nkind /= K_INT) return
            nk = n_keys(pg)
            if (nk < 0 .or. nk > maxk(bt)) return
            if (nkind == K_LEAF) then
                nx = leaf_next(pg)                 ! NO_PAGE terminates the chain
                if (nx /= NO_PAGE .and. (nx < FIRST_BODY_PID .or. nx > bt%npages)) return
            end if
        end associate
        stat = BT_OK
    end subroutine

    ! Child id in slot j of an internal page, bounded before it is followed:
    ! page 1 is the meta page, so a body page id is 2..npages.
    subroutine child_pid(bt, pg, j, pid, stat)
        type(btree_t),    intent(in)  :: bt
        character(len=*), intent(in)  :: pg
        integer,          intent(in)  :: j
        integer,          intent(out) :: pid
        integer,          intent(out) :: stat
        pid  = int_child(pg, j)
        stat = merge(BT_OK, BT_CORRUPT, pid >= FIRST_BODY_PID .and. pid <= bt%npages)
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
        pg(META_MAGIC_OFF : META_BOM_OFF - 1) = BT_MAGIC
        call put_i32(pg, META_BOM_OFF,        BT_BOM)
        call put_i32(pg, META_VERSION_OFF,    int(BT_FORMAT_VERSION, int32))
        call put_i32(pg, META_PAGE_SIZE_OFF,  int(bt%page_size,  int32))
        call put_i32(pg, META_KEY_LEN_OFF,    int(bt%key_len,    int32))
        call put_i32(pg, META_ROOT_OFF,       int(bt%root,       int32))
        call put_i32(pg, META_FREE_HEAD_OFF,  int(bt%free_head,  int32))
        call put_i32(pg, META_NPAGES_OFF,     int(bt%npages,     int32))
        call put_i32(pg, META_FIRST_LEAF_OFF, int(bt%first_leaf, int32))
        call put_i64(pg, META_NENTRIES_OFF,   bt%nentries)
        call write_page(bt, META_PID, pg, stat)
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
        k = int(iachar(pg(KIND_OFF:KIND_OFF)), int8)
    end function

    pure subroutine set_node(pg, k, nkeys)
        character(len=*), intent(inout) :: pg
        integer(int8),    intent(in)    :: k
        integer(int32),   intent(in)    :: nkeys
        pg(KIND_OFF:KIND_OFF) = achar(int(k))
        call put_i32(pg, NKEYS_OFF, nkeys)
    end subroutine

    pure function n_keys(pg) result(n)
        character(len=*), intent(in) :: pg
        integer :: n
        n = int(get_i32(pg, NKEYS_OFF))
    end function

    ! ----- leaf -----

    pure function leaf_next(pg) result(nx)
        character(len=*), intent(in) :: pg
        integer :: nx
        nx = int(get_i32(pg, LEAF_NEXT_OFF))
    end function

    pure subroutine set_leaf_next(pg, nx)
        character(len=*), intent(inout) :: pg
        integer,          intent(in)    :: nx
        call put_i32(pg, LEAF_NEXT_OFF, int(nx, int32))
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
        integer :: u, ios, ps
        integer(int64) :: fsize
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
            ! size so MAXK >= MIN_FANOUT under the split-aware geometry above
            ! (min_page_bytes), rounded up to a PAGE_ALIGN multiple, never
            ! below MIN_PAGE_SIZE.
            associate (need => min_page_bytes(key_len))
                ps = max(MIN_PAGE_SIZE, ((need + PAGE_ALIGN - 1) / PAGE_ALIGN) * PAGE_ALIGN)
            end associate
            open(newunit=u, file=path, access='direct', form='unformatted', &
                 recl=ps, status='replace', action='readwrite', iostat=ios)
            if (ios /= 0) then
                stat = BT_ERR
                return
            end if
            bt%unit       = u
            bt%page_size  = ps
            bt%key_len    = key_len
            bt%free_head  = NO_PAGE
            bt%npages     = META_PID
            bt%nentries   = 0_int64
            bt%writable   = .true.
            ! One empty leaf as the root.
            call alloc_page(bt, bt%root)
            bt%first_leaf = bt%root
            allocate(character(len=ps) :: pg)
            pg = repeat(char(0), ps)
            call set_node(pg, K_LEAF, 0_int32)
            call set_leaf_next(pg, NO_PAGE)
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
            character(len=META_BYTES) :: hdr
            integer(int32)    :: bom
            read(u, iostat=ios) hdr
            inquire(unit=u, size=fsize)
            close(u)
            if (ios /= 0) then
                stat = BT_ERR
                return
            end if
            if (hdr(META_MAGIC_OFF : META_BOM_OFF - 1) /= BT_MAGIC) then
                stat = BT_CORRUPT
                return
            end if
            ! Byte-order mark: checked before any other scalar, since a
            ! wrong-endian file misreads every int field below.
            bom = get_i32(hdr, META_BOM_OFF)
            if (bom /= BT_BOM) then
                stat = merge(BT_VERSION, BT_CORRUPT, bom == BT_BOM_SWAP)
                return
            end if
            if (get_i32(hdr, META_VERSION_OFF) /= BT_FORMAT_VERSION) then
                stat = BT_VERSION
                return
            end if
            bt%page_size  = int(get_i32(hdr, META_PAGE_SIZE_OFF))
            bt%key_len    = int(get_i32(hdr, META_KEY_LEN_OFF))
            bt%root       = int(get_i32(hdr, META_ROOT_OFF))
            bt%free_head  = int(get_i32(hdr, META_FREE_HEAD_OFF))
            bt%npages     = int(get_i32(hdr, META_NPAGES_OFF))
            bt%first_leaf = int(get_i32(hdr, META_FIRST_LEAF_OFF))
            bt%nentries   = get_i64(hdr, META_NENTRIES_OFF)
        end block read_meta
        ! Geometry must be self-consistent, not merely individually plausible.
        ! page_size >= PAGE_SIZE_FLOOR is not enough: a page too small for the (matching)
        ! key_len drives maxk <= 0, after which the first split writes slots
        ! past the page buffer — the silent out-of-bounds class the 2026-06-08
        ! CRITICAL fix closed, reachable again via a corrupt meta page. Mirror
        ! the create-time bound (MAXK >= MIN_FANOUT). first_leaf <= npages and
        ! nentries >= 0 are free corruption catches while we are here.
        !
        ! key_len is tested on its own first. maxk divides by a quantity
        ! derived from it, and .or. does not short-circuit, so a corrupt
        ! stored key_len must be out of the way before maxk is evaluated;
        ! once it equals the caller's key_len (>= 1, checked on entry) the
        ! divisor is positive.
        if (bt%key_len /= key_len) then
            stat = BT_CORRUPT
            return
        end if
        if (bt%page_size < PAGE_SIZE_FLOOR .or.                                &
            bt%root < FIRST_BODY_PID .or. bt%npages < bt%root .or.             &
            bt%first_leaf < FIRST_BODY_PID .or. bt%first_leaf > bt%npages .or. &
            maxk(bt) < MIN_FANOUT .or. bt%nentries < 0) then
            stat = BT_CORRUPT
            return
        end if
        ! The stored page_size becomes the direct-access recl below, so it must
        ! agree with the file itself: off by even one byte, every page read
        ! lands mid-record and nkeys/pointers come back from the middle of key
        ! data. alloc_page/write_page always leave the file at exactly
        ! npages*page_size, and jrnl_recover has already repaired any torn
        ! file by the time db_open reaches an index, so demand equality.
        if (fsize /= int(bt%page_size, int64) * int(bt%npages, int64)) then
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

    ! Close without touching the meta page (interface in the parent).  The
    ! counterpart of bt_close for paths that must leave the on-disk image
    ! exactly as it is.
    module subroutine bt_discard(bt)
        type(btree_t), intent(inout) :: bt
        if (bt%unit /= -1) then
            close(bt%unit)
            bt%unit = -1
        end if
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
        call read_page(bt, META_PID, pg, stat)
        if (stat /= BT_OK) return
        if (pg(META_MAGIC_OFF : META_BOM_OFF - 1) /= BT_MAGIC .or.              &
            get_i32(pg, META_BOM_OFF) /= BT_BOM .or.                            &
            get_i32(pg, META_VERSION_OFF) /= int(BT_FORMAT_VERSION, int32) .or. &
            int(get_i32(pg, META_PAGE_SIZE_OFF)) /= bt%page_size .or.           &
            int(get_i32(pg, META_KEY_LEN_OFF)) /= bt%key_len) then
            stat = BT_CORRUPT
            return
        end if
        bt%root       = int(get_i32(pg, META_ROOT_OFF))
        bt%free_head  = int(get_i32(pg, META_FREE_HEAD_OFF))
        bt%npages     = int(get_i32(pg, META_NPAGES_OFF))
        bt%first_leaf = int(get_i32(pg, META_FIRST_LEAF_OFF))
        bt%nentries   = get_i64(pg, META_NENTRIES_OFF)
        ! Same self-consistency bound bt_open enforces on a freshly read tree.
        if (bt%root < FIRST_BODY_PID .or. bt%npages < bt%root .or.             &
            bt%first_leaf < FIRST_BODY_PID .or. bt%first_leaf > bt%npages .or. &
            maxk(bt) < MIN_FANOUT .or. bt%nentries < 0) then
            stat = BT_CORRUPT
            return
        end if
        stat = BT_OK
    end subroutine

    ! ===== Insert =====

    ! Recursive descent. On return split=.true. means this node was split:
    ! its right half is page `right_pid` and (up_key,up_pay) is the
    ! separator the parent must adopt (the right half's first entry).
    recursive subroutine ins(bt, pid, depth, key, pay, cmp, ctx, &
                             split, up_key, up_pay, right_pid, stat)
        type(btree_t),         intent(inout) :: bt
        integer,               intent(in)    :: pid
        integer,               intent(in)    :: depth
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
        integer :: nk, j, k, mk, ln, rn, ci, mid, cpid
        logical :: csplit
        character(len=:), allocatable :: ckey
        integer(int32) :: cpay

        split = .false.
        right_pid = NO_PAGE
        up_pay = 0_int32
        ! A child id cycling back up the tree would otherwise recurse until the
        ! stack ran out; no path from the root can be longer than the page count.
        if (depth > bt%npages) then
            stat = BT_CORRUPT
            return
        end if
        allocate(character(len=bt%page_size) :: pg)
        call read_node(bt, pid, pg, stat)
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
        call child_pid(bt, pg, ci, cpid, stat)
        if (stat /= BT_OK) return
        allocate(character(len=bt%key_len) :: ckey)
        call ins(bt, cpid, depth + 1, key, pay, cmp, ctx, &
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
        call ins(bt, bt%root, 1, key, payload, cmp, ctx, &
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
        integer :: pid, nk, j, k, hops

        found = .false.
        allocate(character(len=bt%page_size) :: pg)
        pid  = bt%root
        hops = 0
        descend: do
            call read_node(bt, pid, pg, stat)
            if (stat /= BT_OK) return
            if (node_kind(pg) == K_LEAF) exit descend
            ! No root-to-leaf path can visit more pages than the file holds:
            ! a longer walk means the child ids cycle.
            hops = hops + 1
            if (hops > bt%npages) then
                stat = BT_CORRUPT
                return
            end if
            call child_pid(bt, pg, route(bt, pg, key, payload, cmp, ctx), pid, stat)
            if (stat /= BT_OK) return
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
        integer :: pid, slot, hops

        cur%valid = .false.
        cur%leaf  = NO_PAGE
        cur%slot  = 0
        allocate(character(len=bt%page_size) :: pg)
        pid  = bt%root
        hops = 0
        ! Lower bound on key alone: at each internal node take the first
        ! child whose separator key is not less than the target, so we
        ! never start past the first matching entry.
        descend: do
            call read_node(bt, pid, pg, stat)
            if (stat /= BT_OK) return
            if (node_kind(pg) == K_LEAF) exit descend
            hops = hops + 1                      ! cycle guard, as in del
            if (hops > bt%npages) then
                stat = BT_CORRUPT
                return
            end if
            call child_pid(bt, pg, key_lower_bound(bt, pg, key, cmp, ctx), pid, stat)
            if (stat /= BT_OK) return
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
        if (.not. cur%valid .or. cur%leaf == NO_PAGE) return
        advance: do
            ! Serve the current leaf from the cursor's one-page cache: a range
            ! scan yields many keys from the same leaf, so this reads that page
            ! once rather than on every step. Moving to leaf_next (or a fresh
            ! cursor, which is intent(out) so cpg auto-deallocates) refreshes it.
            if (cur%cpid /= cur%leaf .or. .not. allocated(cur%cpg)) then
                if (.not. allocated(cur%cpg)) &
                    allocate(character(len=bt%page_size) :: cur%cpg)
                call read_node(bt, cur%leaf, cur%cpg, stat)
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
            if (cur%leaf == NO_PAGE) then
                cur%valid = .false.
                return
            end if
            ! Leaf-chain cycle guard. It has to be carried in the cursor, not
            ! counted per call: a cycle of non-empty leaves yields an entry on
            ! every call, so each call returns normally while the caller's own
            ! walk (db_verify, index_find) never ends. A full left-to-right
            ! scan makes at most npages-1 transitions.
            cur%hops = cur%hops + 1
            if (cur%hops > bt%npages) then
                cur%valid = .false.
                stat = BT_CORRUPT
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
        bt%npages   = META_PID
        bt%nentries = int(n, int64)
        allocate(character(len=bt%page_size) :: pg)

        if (n == 0) then
            call alloc_page(bt, bt%root)
            bt%first_leaf = bt%root
            pg = repeat(char(0), bt%page_size)
            call set_node(pg, K_LEAF, 0_int32)
            call set_leaf_next(pg, NO_PAGE)
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
                call set_leaf_next(pg, NO_PAGE)
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

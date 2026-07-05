!! Generic on-disk B+-tree.
!!
!! Standalone and reusable
!! Stores opaque fixed-length byte keys mapped to an
!! `int32` payload, ordered by a caller-supplied **pure comparator**
!! passed via the `bt_compare` abstract interface together with an opaque
!! `class(*)` context threaded through unchanged (the same idiom as a
!! callback context).  Duplicate keys are permitted; the tree imposes a
!! total order on `(key, payload)` internally so equal keys are stable and
!! a specific entry can be removed by its payload.
!!
!! On-disk: one fixed-size page per direct-access record.  Page 1 is the
!! meta page and is always written **last** so a crash leaves a coherent
!! tree (at worst some unreferenced pages, reclaimable by `bt_bulk_load`).
!! Freed pages are kept on a free list rooted in the meta page.  The page
!! geometry is derived from the key length at create time and stored in
!! the meta page, so arbitrarily large keys are supported without overflow
!! pages.  The stable page ids + free list + commit-last meta page are the
!! hooks a journal layers onto without restructuring the tree.
!!
!! Performance: O(log N) incremental insert / lookup / remove and
!! O(N log N) perfectly-packed bottom-up bulk build.  Leaves are chained
!! left-to-right for ascending iteration and range scans.  Delete is
!! *lazy*: the entry is removed but underfull leaves are tolerated (no
!! merge/redistribute); space is reclaimed by a `bt_bulk_load` rebuild.

module b_tree
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none
    private

    ! --- Status codes ---
    integer, parameter, public :: BT_OK      = 0  !! Success
    integer, parameter, public :: BT_ERR     = 1  !! I/O / filesystem failure
    integer, parameter, public :: BT_CORRUPT = 2  !! Corrupt on-disk metadata
    integer, parameter, public :: BT_VERSION = 3  !! Unsupported on-disk format

    !! On-disk format version of the paged file. Bumped 1->2 when the
    !! page geometry was widened to provision the transient over-full
    !! node a split builds in place (child area MAXK+2, sep area MAXK+1).
    !! Bumped 2->3 when a byte-order mark was added to the meta page (so a
    !! tree written on a different-endian host is rejected, not silently
    !! misread). Earlier versions use the old offsets and are rejected with
    !! BT_VERSION; an index is derived data, so rebuild it.
    integer, parameter, public :: BT_FORMAT_VERSION = 3

    abstract interface
        !! Optional pre-write journal hook.  Invoked by every page write
        !! *before* the page is overwritten, so a transaction layer can
        !! capture an undo image.  `offset` is the page's 1-based byte
        !! position in the file.  For an in-place overwrite `is_new` is
        !! `.false.` and `old_bytes` is the page's current `page_size`-byte
        !! pre-image; for a freshly allocated page `is_new` is `.true.`,
        !! `old_bytes` is empty, and the layer should record the file's
        !! pre-growth length instead.  A non-zero `stat` aborts the write,
        !! so a journalling failure never lets an un-recorded overwrite
        !! through.  `ctx` is the caller's opaque context, threaded
        !! unchanged.
        subroutine bt_journal_hook(ctx, offset, old_bytes, is_new, stat)
            import :: int64
            class(*),         intent(in)  :: ctx       !! Opaque caller context
            integer(int64),   intent(in)  :: offset    !! 1-based byte position of the page
            character(len=*), intent(in)  :: old_bytes !! Page pre-image (empty if `is_new`)
            logical,          intent(in)  :: is_new    !! Page is newly allocated this txn
            integer,          intent(out) :: stat      !! `0` = OK; non-zero aborts the write
        end subroutine
    end interface
    public :: bt_journal_hook

    !! An open B+-tree.  Pure data plus the open unit — the comparator and
    !! its context are stateless and supplied per call, so a handle can be
    !! closed and reopened freely and carries nothing un-persistable.
    type, public :: btree_t
        integer        :: unit       = -1        !! Open Fortran unit, -1 if closed
        integer        :: page_size  = 0         !! Bytes per page (derived from `key_len`)
        integer        :: key_len    = 0         !! Fixed key length in bytes
        integer        :: root       = 0         !! Root page id
        integer        :: free_head  = 0         !! Head of the free-page list (0 = none)
        integer        :: npages     = 0         !! Highest page id ever allocated
        integer        :: first_leaf = 0         !! Leftmost leaf page id (iteration start)
        integer(int64) :: nentries   = 0_int64   !! Number of live `(key,payload)` entries
        logical        :: writable   = .false.   !! Opened read-write (`.false.` = read-only)
        ! Optional journal hook (off by default).  When `jhook` is associated
        ! every page write first calls it; `jbase` is the page high-water at
        ! install time, so a write to `pid > jbase` is a new page this txn.
        procedure(bt_journal_hook), pointer, nopass :: jhook => null()  !! Pre-write undo hook
        class(*),                   pointer         :: jctx  => null()  !! Hook context
        integer :: jbase = 0  !! Page high-water when the hook was installed
    end type

    !! A forward cursor over entries in ascending `(key,payload)` order.
    !! Obtained from `bt_first` (whole tree) or `bt_seek` (lower bound on a
    !! key); advanced and read with `bt_next`.
    type, public :: bt_cursor_t
        integer :: leaf  = 0        !! Current leaf page id (0 = exhausted)
        integer :: slot  = 0        !! 0-based index of the next entry to yield
        logical :: valid = .false.  !! `.true.` while the cursor may yield more
        integer :: cpid  = 0        !! Page id currently held in `cpg` (0 = none)
        character(len=:), allocatable :: cpg  !! One-leaf read cache: a range
                                    !! scan yields many keys from one leaf, so
                                    !! `bt_next` reads it once instead of per key
    end type

    abstract interface
        !! Total order on keys.  Returns `<0`, `0`, `>0` for `a` ordering
        !! before / equal to / after `b`.  Must be pure; `a` and `b` are
        !! exactly `key_len` bytes.  `ctx` is the caller's opaque context,
        !! threaded through every comparison unchanged.
        pure function bt_compare(a, b, ctx) result(c)
            character(len=*), intent(in) :: a
            character(len=*), intent(in) :: b
            class(*),         intent(in) :: ctx
            integer :: c
        end function
    end interface
    public :: bt_compare

    interface
        !! Open an existing tree (`create=.false.`) or create a fresh empty
        !! one (`create=.true.`, file truncated).  `writable=.false.` opens
        !! read-only.  On a non-create open `key_len` must match the value
        !! stored in the file or `BT_CORRUPT` is returned.
        module subroutine bt_open(bt, path, key_len, writable, create, stat)
            type(btree_t),    intent(out) :: bt    !! Tree handle (overwritten)
            character(len=*), intent(in)  :: path  !! Paged-file path
            integer,          intent(in)  :: key_len  !! Fixed key length in bytes
            logical,          intent(in)  :: writable  !! Open read-write
            logical,          intent(in)  :: create  !! Truncate + initialise empty
            integer,          intent(out) :: stat  !! `BT_OK` or an error code
        end subroutine

        !! Flush the meta page (read-write opens) and close the unit.  Safe
        !! to call on an already-closed handle.
        module subroutine bt_close(bt, stat)
            type(btree_t), intent(inout)         :: bt    !! Tree handle
            integer,       intent(out), optional :: stat  !! `BT_OK` or an error code
        end subroutine

        !! Re-read the mutable meta fields (`root`, `free_head`, `npages`,
        !! `first_leaf`, `nentries`) from the on-disk meta page into the open
        !! handle, discarding the cached in-memory copies.  This re-syncs a
        !! tree whose file was changed underneath it — specifically after a
        !! journal rollback restores the meta page, the cached fields are
        !! stale and must be reloaded before the tree is touched again.  The
        !! unit stays open; `page_size`/`key_len` are immutable and a mismatch
        !! (or a failed geometry self-check) is reported as `BT_CORRUPT`.
        module subroutine bt_reload(bt, stat)
            type(btree_t), intent(inout) :: bt    !! Open tree handle, re-synced in place
            integer,       intent(out)   :: stat  !! `BT_OK` or an error code
        end subroutine

        !! Push a writable tree's buffered page writes out to the operating
        !! system so a subsequent fsync of the file makes them durable.  Every
        !! mutator already writes the meta page last, so the on-disk image is
        !! coherent; this only drains the open unit's buffer and performs no
        !! fsync itself (the journal layer owns durability, by path).  A no-op
        !! on a closed or read-only handle.
        module subroutine bt_sync(bt, stat)
            type(btree_t), intent(in)            :: bt    !! Open tree handle
            integer,       intent(out), optional :: stat  !! `BT_OK`
        end subroutine

        !! Insert `(key, payload)`.  Duplicate keys are allowed; the pair
        !! is ordered by key then payload so the entry is uniquely
        !! addressable for `bt_remove`.
        module subroutine bt_insert(bt, key, payload, cmp, ctx, stat)
            type(btree_t),    intent(inout) :: bt       !! Tree handle
            character(len=*), intent(in)    :: key      !! Key bytes (`key_len`)
            integer(int32),   intent(in)    :: payload  !! Associated payload
            procedure(bt_compare)           :: cmp      !! Key order
            class(*),         intent(in)    :: ctx      !! Opaque comparator context
            integer,          intent(out)   :: stat     !! `BT_OK` or an error code
        end subroutine

        !! Remove the entry matching `(key, payload)` exactly.  `found`
        !! is `.false.` (with `stat == BT_OK`) if no such entry exists.
        !! Lazy: an emptied leaf is left in place, not merged.
        module subroutine bt_remove(bt, key, payload, cmp, ctx, found, stat)
            type(btree_t),    intent(inout) :: bt       !! Tree handle
            character(len=*), intent(in)    :: key      !! Key bytes (`key_len`)
            integer(int32),   intent(in)    :: payload  !! Payload identifying the entry
            procedure(bt_compare)           :: cmp      !! Key order
            class(*),         intent(in)    :: ctx      !! Opaque comparator context
            logical,          intent(out)   :: found    !! `.true.` if an entry was removed
            integer,          intent(out)   :: stat     !! `BT_OK` or an error code
        end subroutine

        !! Rebuild the whole tree from `keys`/`payloads`: sort `(key,payload)`
        !! then write perfectly-packed leaves and the internal levels
        !! bottom-up.  O(N log N) — the proper replacement for per-row
        !! reinsertion.  `keys` is a rank-1 array of `key_len` byte strings,
        !! `payloads(i)` the payload for `keys(i)`.
        !!
        !! Note: this resets the logical page count and rewrites from page 2,
        !! but does NOT shrink the underlying file — pages above the new high
        !! water remain allocated on disk (harmless; never read).  To actually
        !! reclaim space (e.g. repacking after many lazy deletes), recreate the
        !! file with `bt_open(create=.true.)` and load into the fresh tree.
        module subroutine bt_bulk_load(bt, keys, payloads, cmp, ctx, stat)
            type(btree_t),    intent(inout) :: bt           !! Tree handle
            character(len=*), intent(in)    :: keys(:)      !! Keys (each `key_len` bytes)
            integer(int32),   intent(in)    :: payloads(:)  !! Payload per key
            procedure(bt_compare)           :: cmp          !! Key order
            class(*),         intent(in)    :: ctx          !! Opaque comparator context
            integer,          intent(out)   :: stat         !! `BT_OK` or an error code
        end subroutine

        !! Position `cur` at the first entry whose key is not ordered
        !! before `key` (lower bound).  Callers iterate with `bt_next` and
        !! stop themselves once the yielded key compares greater.
        module subroutine bt_seek(bt, key, cmp, ctx, cur, stat)
            type(btree_t),    intent(in)  :: bt   !! Tree handle
            character(len=*), intent(in)  :: key  !! Lower-bound key (`key_len`)
            procedure(bt_compare)         :: cmp  !! Key order
            class(*),         intent(in)  :: ctx  !! Opaque comparator context
            type(bt_cursor_t), intent(out) :: cur  !! Positioned cursor
            integer,          intent(out) :: stat !! `BT_OK` or an error code
        end subroutine

        !! Position `cur` at the leftmost entry (whole-tree ascending
        !! iteration).  The cursor is exhausted immediately for an empty
        !! tree.
        module subroutine bt_first(bt, cur, stat)
            type(btree_t),     intent(in)  :: bt   !! Tree handle
            type(bt_cursor_t), intent(out) :: cur  !! Positioned cursor
            integer,           intent(out) :: stat !! `BT_OK` or an error code
        end subroutine

        !! Yield the entry at the cursor and advance.  `ok` is `.false.`
        !! when the cursor is exhausted (no entry returned).
        !! `key` must be exactly `key_len` bytes: only `key(1:key_len)` is
        !! assigned, so a longer buffer keeps undefined trailing bytes.
        module subroutine bt_next(bt, cur, key, payload, ok, stat)
            type(btree_t),     intent(in)    :: bt       !! Tree handle
            type(bt_cursor_t), intent(inout) :: cur      !! Cursor (advanced)
            character(len=*),  intent(out)   :: key      !! Receives the key (must be exactly `key_len`)
            integer(int32),    intent(out)   :: payload  !! Receives the payload
            logical,           intent(out)   :: ok       !! `.true.` if an entry was yielded
            integer,           intent(out)   :: stat     !! `BT_OK` or an error code
        end subroutine

        !! Install (or, called with no `hook`, remove) the pre-write journal
        !! hook on an open writable tree.  Installing records the current page
        !! high-water as the new-page boundary for the transaction about to
        !! run; clearing it returns the tree to un-journalled writes.  `ctx`
        !! must be supplied whenever `hook` is, and must out-live the tree.
        module subroutine bt_set_journal_hook(bt, hook, ctx)
            type(btree_t),    intent(inout)         :: bt    !! Tree handle
            procedure(bt_journal_hook),    optional :: hook  !! Hook (absent = clear)
            class(*), pointer, intent(in), optional :: ctx   !! Opaque hook context
        end subroutine
    end interface

    public :: bt_open, bt_close, bt_reload, bt_sync, bt_insert, bt_remove, bt_bulk_load
    public :: bt_seek, bt_first, bt_next, bt_set_journal_hook

contains

end module b_tree

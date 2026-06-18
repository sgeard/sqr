!! Lightweight pure-Fortran relational store.
!!
!! A database is a directory.  Tables are stored as a pair of files:
!!
!!   * `<name>.dat` — direct-access binary, fixed-size records
!!     (`recl = record_size`)
!!   * `<name>.schema` — stream-access binary, schema header + column
!!     definitions
!!
!! Each data record is `record_size` bytes: byte 1 is the status
!! (`ROW_ALIVE` / `ROW_TOMBSTONE`); the next `(ncols+7)/8` bytes are a
!! per-column NULL bitmap; the remaining bytes are column data packed at
!! fixed offsets per the schema.  A column whose NULL bit is set reads back
!! as absent (`row_is_null`) and is omitted from any index it belongs to.
!! Each secondary index is an on-disk
!! B+-tree (one paged file per index) in `<name>__i<slot>.idx`, mapping
!! the composite key bytes to the `int32` row id — see the generic
!! `b_tree` module.
!!
!! A fixed magic + version is written into every `.schema` file; a
!! mismatch on open returns `SQR_VERSION`.  Errors are reported via
!! optional `stat`/`errmsg` out arguments — there is no `error stop` in
!! library code.

module sqr
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real64
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use :: b_tree, only: btree_t, bt_cursor_t
    implicit none
    private

    ! --- Column data types ---
    integer, parameter, public :: DT_INT  = 1  !! 32-bit integer column (4 B)
    integer, parameter, public :: DT_REAL = 2  !! 64-bit real column (8 B)
    !! Fixed-width character column (1..65536 B).  Stored NUL-padded and
    !! read back up to the first NUL (see `row_set_char` / `row_get_char`):
    !! it is **not** binary-safe — a value containing an embedded NUL byte
    !! is truncated there on read.  For arbitrary/binary content use
    !! `DT_TEXT`, which length-prefixes the bytes in the blob file and has
    !! no terminator convention.
    integer, parameter, public :: DT_CHAR = 3
    integer, parameter, public :: DT_TEXT = 4  !! Arbitrary-length text; bytes in `<table>.blob`

    !! In-row descriptor size for a `DT_TEXT` column: int64 blob offset +
    !! int32 length.
    integer, parameter, public :: SQR_TEXT_DESC = 12

    ! --- Return codes ---
    integer, parameter, public :: SQR_OK        = 0  !! Success
    integer, parameter, public :: SQR_NOT_FOUND = 1  !! No such table / row / index / key
    integer, parameter, public :: SQR_DUP       = 2  !! Duplicate table or unique-key violation
    integer, parameter, public :: SQR_ERR       = 3  !! I/O or filesystem failure
    integer, parameter, public :: SQR_VERSION   = 4  !! Unsupported on-disk format version
    integer, parameter, public :: SQR_INVALID   = 5  !! Bad argument or corrupt on-disk metadata
    integer, parameter, public :: SQR_READONLY  = 6  !! Write attempted on a read-only open
    integer, parameter, public :: SQR_LOCKED    = 7  !! Database held by another connection

    ! --- On-disk format version ---
    !! Current on-disk format version.  There is a single format: composite
    !! index records (ncols, member names, key_size, unique) with each
    !! index stored as a generic on-disk B+-tree.  No migration path — a
    !! schema whose version differs is rejected with `SQR_VERSION` as a
    !! corruption guard.
    integer, parameter, public :: SQR_SCHEMA_VERSION = 1

    ! --- Row status byte values ---
    integer(int8), parameter, public :: ROW_ALIVE     = 1_int8  !! Live row
    integer(int8), parameter, public :: ROW_TOMBSTONE = 2_int8  !! Deleted row (space reclaimed by `db_compact`)

    integer, parameter, public :: SQR_NAME_LEN = 32  !! Max table/column name length (bytes)
    character(len=4), parameter, public :: SQR_MAGIC = 'SQRT'  !! Schema-file magic

    !! Byte-order mark written into the catalog and schema headers (just
    !! after the magic).  An asymmetric native `int32`: read back it equals
    !! `SQR_BOM` only if the file was written with this host's byte order, so
    !! a database moved to a host of the opposite endianness is rejected with
    !! `SQR_VERSION` instead of silently misreading every stored scalar.
    !! Cross-endian byte-swapping is deliberately out of scope.
    integer(int32), parameter, public :: SQR_BOM = int(z'01020304', int32)

    !! Sanity cap on a fixed record (status byte + all column bytes).  Used
    !! both to reject over-large schemas at create time and as a corruption
    !! guard when reading a schema back from disk.
    integer, parameter, public :: SQR_MAX_RECORD = 1024*1024

    ! --- Types ---

    !! One column definition.  Width and offset are derived at
    !! create-table time by `layout_columns`; callers normally set only
    !! `name`, `dtype` and (for `DT_CHAR`) `csize`.
    type, public :: column_t
        character(len=SQR_NAME_LEN) :: name = ''  !! Column name
        integer :: dtype  = 0    !! One of `DT_INT` / `DT_REAL` / `DT_CHAR` / `DT_TEXT`
        integer :: csize  = 0    !! Bytes on disk
        integer :: offset = 0    !! 1-based byte offset within the record
        integer :: null_bit = 0  !! 0-based bit ordinal in the per-row NULL bitmap
    end type

    !! A (possibly composite) secondary index.  The key is the member
    !! column bytes concatenated in declared order; a single-column index
    !! is just arity 1.  `unique` enforces that no two live rows share a
    !! key.
    type, public :: index_t
        integer :: ncols = 0  !! Number of member columns (1 = single-column)
        character(len=SQR_NAME_LEN), allocatable :: columns(:)  !! Ordered member names
        integer, allocatable :: col_idx(:)  !! Index of each member into the owning `table%cols(:)`
        integer, allocatable :: key_off(:)  !! 1-based offset of each member within the key
        integer :: key_size = 0  !! Sum of member `csize`s (the B+-tree key length)
        integer :: nentries = 0  !! Cached live-entry count, mirrored from `bt`
        type(btree_t) :: bt  !! On-disk B+-tree mapping the key to the `int32` row id
        logical :: unique   = .false.  !! Enforce no duplicate live keys
        class(*), pointer :: jctx => null()  !! Heap-owned `bt_jhook_ctx_t` while a txn's journal hook is installed on `bt`; freed at txn end
    end type

    !! One open table: schema, derived layout, open units and index set.
    type, public :: table_t
        character(len=SQR_NAME_LEN) :: name = ''  !! Table name
        integer                     :: ncols          = 0  !! Number of columns
        type(column_t), allocatable :: cols(:)  !! Column definitions
        integer                     :: record_size    = 0  !! Fixed record size in bytes
        integer                     :: next_id        = 1  !! Next row_id to assign
        integer                     :: live_count     = 0  !! Number of non-tombstoned rows
        integer                     :: schema_version = 0  !! On-disk format version of this table
        integer                     :: unit           = -1  !! Open unit for `<table>.dat`, -1 if closed
        integer                     :: nindices       = 0  !! Number of secondary indices
        type(index_t), allocatable  :: indices(:)  !! Secondary indices
        integer                     :: blob_unit      = -1       !! Open unit for `<table>.blob`, -1 if none
        integer(int64)              :: blob_next      = 1_int64  !! Next blob append position (1-based)
    end type

    !! One undo record captured before a transaction overwrites part of a
    !! base file.  A REGION record stores the original bytes of an in-place
    !! overwrite (rollback writes them back); an EXTEND record stores only
    !! the original file length (rollback truncates appended bytes away).
    !! Module-private — exposed only as a component of `journal_t`.
    type :: undo_rec_t
        integer :: kind = 0  !! `UNDO_REGION` or `UNDO_EXTEND`
        character(len=:), allocatable :: path  !! File path, relative to the db directory
        integer(int64) :: orig_len = 0  !! File length when first touched this txn
        integer(int64) :: offset   = 0  !! 1-based byte offset of the region (REGION only)
        integer(int64) :: length   = 0  !! Region length in bytes (REGION only)
        character(len=:), allocatable :: bytes  !! Original region bytes (REGION only)
    end type

    !! Pre-transaction snapshot of one table's in-memory counters.  The undo
    !! journal restores file bytes; these cached values (high-water row id, live
    !! count, blob append position) are advanced in memory by row mutations and
    !! are not on disk per-write, so a rollback restores them from here — the
    !! record analogue of `bt_reload` for the index trees.  Module-private —
    !! exposed only as a component of `journal_t`.
    type :: tbl_snap_t
        integer        :: next_id    = 1         !! `table%next_id` at txn_begin
        integer        :: live_count = 0         !! `table%live_count` at txn_begin
        integer(int64) :: blob_next  = 1_int64   !! `table%blob_next` at txn_begin
    end type

    !! Per-database rollback journal.  Opaque to callers, carried as a
    !! component of `db_t`; driven by the `txn_*` / `jrnl_*` procedures.  The
    !! file `<db>/_journal.dat` is a reusable sidecar — a hot (valid) journal
    !! exists iff a transaction is in flight or a crash interrupted one.
    type, public :: journal_t
        character(len=:), allocatable :: path  !! `<db>/_journal.dat`, set at db_open
        integer        :: unit     = -1       !! Open stream unit, -1 if not open
        logical        :: active   = .false.  !! A transaction is in flight
        logical        :: explicit = .false.  !! The in-flight txn was opened by `db_begin` (vs. auto-commit)
        logical        :: armed    = .false.  !! Undo records are durable (journal is hot)
        logical        :: sized    = .false.  !! File created + pre-sized this session
        integer(int64) :: capacity = 0        !! Pre-allocated size in bytes
        integer        :: nrec     = 0        !! Live undo-record count for the current txn
        type(undo_rec_t), allocatable :: recs(:)  !! In-memory undo set for the current txn
        type(tbl_snap_t), allocatable :: snaps(:)  !! Per-table counter snapshot, by table position, for the current txn
    end type

    !! An open database handle.  Obtain with `db_open`; release with
    !! `db_close`.  A handle is bound to one directory for its lifetime.
    type, public :: db_t
        character(len=:), allocatable :: dir  !! Database directory path
        type(table_t), allocatable    :: tables(:)  !! Open tables
        integer                       :: ntables  = 0  !! Number of open tables
        logical                       :: opened   = .false.  !! `.true.` between `db_open` and `db_close`
        logical                       :: readonly = .false.  !! `.true.` if opened read-only
        integer                       :: generation = 0  !! Bumped by every mutating call; cursors snapshot it
        integer(c_int64_t)            :: lock_tok = -1  !! Advisory-lock token held while open (-1 = none)
        type(journal_t)               :: jrnl  !! Rollback journal state
    contains
        !! Object-oriented spelling of the `db_*` operations: `call db%insert(...)`
        !! is exactly `call db_insert(db, ...)`.  The free `db_*` procedures remain
        !! public and callable unchanged; these bindings are a thin alternative
        !! face on the same module procedures (which is why the passed-object
        !! `db` argument is `class(db_t)` throughout).
        procedure :: open         => db_open
        procedure :: close        => db_close
        procedure :: set_readonly => db_set_readonly
        procedure :: create_table => db_create_table
        procedure :: drop_table   => db_drop_table
        procedure :: add_column   => db_add_column
        procedure :: drop_column  => db_drop_column
        procedure :: compact      => db_compact
        procedure :: list_tables  => db_list_tables
        procedure :: table_index  => db_table_index
        procedure :: insert       => db_insert
        procedure :: insert_many  => db_insert_many
        procedure :: get          => db_get
        procedure :: update       => db_update
        procedure :: delete       => db_delete
        procedure :: scan         => db_scan
        procedure :: verify       => db_verify
        procedure :: set_text     => db_set_text
        procedure :: get_text     => db_get_text
        procedure :: find_by_int  => db_find_by_int
        procedure :: find_by_real => db_find_by_real
        procedure :: find_by_char => db_find_by_char
        procedure :: get_by_key    => db_get_by_key
        procedure :: update_by_key => db_update_by_key
        procedure :: delete_by_key => db_delete_by_key
        procedure :: open_cursor  => db_open_cursor
        procedure :: cursor_next  => db_cursor_next
        procedure :: begin        => db_begin
        procedure :: commit       => db_commit
        procedure :: rollback     => db_rollback
        ! Overloaded operations: generic bindings over the same specifics that
        ! back the free generic interfaces below.
        procedure, private :: create_index_1 => db_create_index_1
        procedure, private :: create_index_m => db_create_index_m
        generic :: create_index => create_index_1, create_index_m
        procedure, private :: drop_index_1 => db_drop_index_1
        procedure, private :: drop_index_m => db_drop_index_m
        generic :: drop_index => drop_index_1, drop_index_m
        procedure, private :: find_range_int  => db_find_range_int
        procedure, private :: find_range_real => db_find_range_real
        procedure, private :: find_range_char => db_find_range_char
        generic :: find_range => find_range_int, find_range_real, find_range_char
    end type

    !! A forward (ascending) cursor over the live rows of a table in the key
    !! order of one of its single-column indices.  Obtain it from
    !! `db_open_cursor` (the whole index) or `db_find_range` (an inclusive
    !! `[lo,hi]` band), then pull rows with `db_cursor_next` until it reports
    !! exhaustion — the pull complement to the `db_scan` callback.
    !!
    !! CONTRACT: the cursor rides on the table's already-open index, so there
    !! is nothing to close; but it is invalidated by any mutating call on the
    !! handle (`db_insert` / `db_update` / `db_delete` / `db_compact`, and the
    !! structural `db_create_table` / `db_drop_table`, which can shift table
    !! slots) — re-open it after mutating.  This is enforced: the cursor
    !! snapshots `db%generation` at creation and `db_cursor_next` returns
    !! `SQR_INVALID` (rather than reading a stale/own slot) if it has since
    !! changed.  The component layout is exposed for `transfer`-free storage
    !! only; callers should treat it as opaque.
    type, public :: db_cursor_t
        integer :: ti = 0  !! Owning table slot in `db%tables` (0 = unset)
        integer :: j  = 0  !! Index slot in the owning table's `indices(:)`
        type(bt_cursor_t) :: bt  !! Underlying B+-tree cursor position
        logical :: bounded = .false.  !! `.true.` if `hikey` caps the range
        character(len=:), allocatable :: hikey  !! Inclusive upper-bound key bytes
        logical :: active = .false.  !! `.true.` while more rows may be yielded
        integer :: gen = -1  !! `db%generation` snapshot; mismatch ⇒ invalidated
    end type

    !! Context passed to `bt_journal_adapter` — the bridge that turns a
    !! `b_tree`'s pre-write hook (see `bt_set_journal_hook`) into rollback-journal
    !! captures.  It names the database whose journal receives the undo records
    !! and the tree's on-disk file *relative to* the database directory.  The
    !! `db` target must out-live every tree the adapter is installed on (it is so
    !! by construction: the trees are components of `db%tables`).
    type, public :: bt_jhook_ctx_t
        type(db_t), pointer           :: db  => null()  !! Database whose journal logs the undo
        character(len=:), allocatable :: rel            !! Tree file, relative to `db%dir`
    end type

    ! --- Public API ---
    public :: db_open, db_close, db_set_readonly
    public :: db_create_table, db_drop_table, db_compact
    public :: db_add_column, db_drop_column
    public :: db_list_tables, db_table_index, idx_live
    public :: db_insert, db_get, db_update, db_delete, db_scan
    public :: db_set_text, db_get_text
    public :: db_create_index, db_drop_index
    public :: db_find_by_int, db_find_by_real, db_find_by_char
    public :: db_get_by_key, db_update_by_key, db_delete_by_key
    public :: db_open_cursor, db_find_range, db_cursor_next
    public :: db_insert_many, db_verify
    ! Explicit transaction façade (what SQL BEGIN/COMMIT/ROLLBACK maps onto).
    public :: db_begin, db_commit, db_rollback
    ! Rollback-journal internals (durability/atomicity layer). Public only so
    ! the b_tree adapter and the mutator paths in sibling submodules can reach
    ! them — application code should use the db_begin/commit/rollback façade
    ! above, not these primitives directly.
    public :: txn_begin, txn_arm, txn_commit, txn_rollback
    public :: jrnl_log_region, jrnl_log_extend, jrnl_recover, jrnl_hot
    public :: bt_journal_adapter

    !! Create a secondary index.  Accepts either a single column name or a
    !! rank-1 array of member column names (composite key), each with an
    !! optional `unique=`.
    interface db_create_index
        module procedure db_create_index_1
        module procedure db_create_index_m
    end interface

    !! Drop a secondary index.  Accepts either a single column name or a rank-1
    !! array of member column names (the same shape that created it).
    interface db_drop_index
        module procedure db_drop_index_1
        module procedure db_drop_index_m
    end interface

    !! Open an ascending cursor over the live rows whose indexed value lies
    !! in the inclusive band `[lo, hi]`.  Typed on the column: `int32`,
    !! `real64` (where a *tolerance* match belongs — `lo = x-eps`,
    !! `hi = x+eps` — never fuzzy equality), or `DT_CHAR` (NUL-padded to the
    !! column width).  `col_name` may be an exact single-column index or the
    !! **leading** member of a composite index (a leading-prefix range, so no
    !! redundant single-column index is needed).  Pull rows with
    !! `db_cursor_next`.  `lo > hi` yields an empty cursor; NULL-member rows are
    !! excluded.
    interface db_find_range
        module procedure db_find_range_int
        module procedure db_find_range_real
        module procedure db_find_range_char
    end interface

    ! Row buffer helpers
    public :: row_alloc, row_clear
    public :: row_status, row_set_status
    public :: row_set_null, row_clear_null, row_is_null
    public :: row_set_int, row_get_int
    public :: row_set_real, row_get_real
    public :: row_set_char, row_get_char

    abstract interface
        !! Signature of a `db_scan` callback.  Invoked once per live row;
        !! set `stop` to `.true.` to end the scan early.  The scanning `db`
        !! is passed through so the callback can resolve `DT_TEXT` columns
        !! for the current row via `db_get_text(db, table, row_id, ...)` —
        !! the in-row `buf` holds only the blob descriptor, not the text.
        !! The callback must not make structural changes to `db` (create or
        !! drop a table) during the scan, as that would invalidate the scan
        !! in progress; reading rows / text and mutating row data are fine.
        subroutine scan_cb(db, row_id, buf, ctx, stop)
            import :: int32, db_t
            class(db_t),      intent(inout) :: db  !! The database being scanned (for TEXT resolution)
            integer(int32),   intent(in)    :: row_id  !! Row id of the current row
            character(len=*), intent(in)    :: buf  !! The row's record buffer (read-only)
            class(*),         intent(inout) :: ctx  !! Opaque caller context, threaded through unchanged
            logical,          intent(out)   :: stop  !! Set `.true.` to stop the scan
        end subroutine
    end interface
    public :: scan_cb

    ! --- Submodule procedure interfaces ---
    interface
        !! Open (or create) a database directory.
        !!
        !! A read-write open creates the directory if needed; a read-only
        !! open requires an already-initialised database.
        !!
        !! CONTRACT: `db` is `intent(out)`, so any state from a prior open
        !! is discarded before `db_open` can act on it.  The caller MUST
        !! `db_close` an open handle before reopening it (or opening a
        !! different db into it): the old data/index/blob unit numbers
        !! would otherwise be leaked with the files left open.  `db_open`
        !! cannot defend against this internally — the handle is already
        !! wiped on entry.
        module subroutine db_open(db, dir, stat, errmsg, readonly)
            class(db_t),       intent(out)             :: db  !! Database handle (overwritten)
            character(len=*), intent(in)              :: dir  !! Database directory name
            integer,          intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*), intent(inout), optional :: errmsg  !! Human-readable failure detail
            logical,          intent(in),   optional  :: readonly  !! Open read-only (default `.false.`)
        end subroutine

        !! Close a database handle: flush schema/catalog (read-write
        !! opens), close all units, and mark the handle closed.  Optional
        !! `stat` reports the first flush failure (schema counters are
        !! persisted only here, so a failed close is where recent data is
        !! lost); the handle is still fully closed regardless.
        module subroutine db_close(db, stat)
            class(db_t), intent(inout)               :: db  !! Database handle
            integer,    intent(out),       optional :: stat  !! First flush failure, else `SQR_OK`
        end subroutine

        !! Demote an open read-write handle to read-only: subsequent writes
        !! return `SQR_READONLY`, and the exclusive lock is downgraded to a
        !! shared one so other read-only connections may attach.  Refused
        !! (`SQR_INVALID`) on a closed handle or while a transaction is live;
        !! a no-op on a handle already read-only.  A failure to downgrade the
        !! lock leaves the handle safely read-only but reports `SQR_ERR`.
        module subroutine db_set_readonly(db, stat)
            class(db_t), intent(inout)               :: db  !! Database handle
            integer,    intent(out),       optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Create a new table from a column-definition array.  Fails with
        !! `SQR_DUP` if the table already exists, `SQR_INVALID` for a bad
        !! name or column set.
        module subroutine db_create_table(db, name, cols, stat, errmsg)
            class(db_t),       intent(inout)           :: db  !! Database handle
            character(len=*), intent(in)              :: name  !! New table name
            type(column_t),   intent(in)              :: cols(:)  !! Column definitions (name/dtype/csize)
            integer,          intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*), intent(inout), optional :: errmsg  !! Human-readable failure detail
        end subroutine

        !! Drop a table and delete all of its files (data, schema,
        !! indices, blob).
        module subroutine db_drop_table(db, name, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: name  !! Table to drop
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Reclaim space for one table: drop tombstoned rows, copy only
        !! the blob bytes still referenced by live rows, renumber the
        !! survivors `1..live_count`, and rebuild every index off the
        !! compacted data.
        !!
        !! CONTRACT: row_ids are **not** stable across a compaction —
        !! every surviving row is renumbered, so any row_id a caller holds
        !! across this call is invalid afterward.  (Stable handles are the
        !! natural-key feature: `db_get_by_key` and friends.)  Requires a
        !! read-write open db; a read-only open is rejected with
        !! `SQR_READONLY`.
        !!
        !! On-disk consistency is preserved on any failure
        !! (build-then-swap).  But if the post-swap reopen of the
        !! compacted data/blob fails, that table's in-memory handle is
        !! left wedged (units = -1) for the rest of the session even
        !! though the on-disk state is the correct compacted file: `stat`
        !! reports the error, and the caller should `db_close` and
        !! `db_open` afresh rather than keep using the handle.
        module subroutine db_compact(db, table_name, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Table to compact
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Add a column to an existing table (schema evolution by table
        !! rewrite).  `col` carries the new column's `name`, `dtype` and (for
        !! `DT_CHAR`) `csize`, exactly as for `db_create_table`; `offset` and
        !! `null_bit` are derived.  The column is appended after the existing
        !! ones and every live and tombstoned record is rewritten into the
        !! wider layout with the new column **NULL** — so existing values read
        !! back unchanged and the new column reads as absent until written.
        !!
        !! CONTRACT: row_ids are **preserved** (unlike `db_compact`, which
        !! renumbers) — a row_id held across this call stays valid.  Existing
        !! secondary indices are untouched: their keys and row_ids do not
        !! change, so no index is rebuilt or dropped.  Adding a `DT_TEXT`
        !! column to a table that had none creates its blob file.  Fails with
        !! `SQR_NOT_FOUND` (no such table), `SQR_INVALID` (bad column
        !! definition, or a name already in the table), or `SQR_READONLY`.
        !!
        !! On-disk consistency is build-then-swap as in `db_compact`: the
        !! rewritten data file is renamed in and the schema rewritten back to
        !! back; a hard crash strictly between those two steps is the
        !! documented pre-journal residual window.
        module subroutine db_add_column(db, table_name, col, stat, errmsg)
            class(db_t),      intent(inout)           :: db  !! Database handle
            character(len=*), intent(in)              :: table_name  !! Target table
            type(column_t),   intent(in)              :: col  !! New column (name/dtype/csize)
            integer,          intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*), intent(inout), optional :: errmsg  !! Human-readable failure detail
        end subroutine

        !! Drop a column from an existing table (schema evolution by table
        !! rewrite).  Every record is rewritten without the column's bytes and
        !! the surviving columns repacked.  **CASCADE**: any secondary index
        !! that includes the dropped column is dropped too (its slot
        !! tombstoned, its file deleted); indices that do not reference the
        !! column are kept, their keys and row_ids unchanged.
        !!
        !! CONTRACT: row_ids are **preserved**.  Dropping the last `DT_TEXT`
        !! column deletes the table's blob file.  Fails with `SQR_NOT_FOUND`
        !! (no such table or column), `SQR_INVALID` (the column is the table's
        !! only one — a table must keep at least one column), or `SQR_READONLY`.
        !! Same build-then-swap durability as `db_add_column`.
        module subroutine db_drop_column(db, table_name, col_name, stat, errmsg)
            class(db_t),      intent(inout)           :: db  !! Database handle
            character(len=*), intent(in)              :: table_name  !! Target table
            character(len=*), intent(in)              :: col_name  !! Column to drop
            integer,          intent(out),  optional  :: stat  !! `SQR_OK` or an error code
            character(len=*), intent(inout), optional :: errmsg  !! Human-readable failure detail
        end subroutine

        !! Return the names of all tables in the database.
        module subroutine db_list_tables(db, names)
            class(db_t),                                 intent(in)  :: db  !! Database handle
            character(len=SQR_NAME_LEN), allocatable,   intent(out) :: names(:)  !! Table names
        end subroutine

        !! 1-based index of `name` in `db%tables`, or 0 if not found.
        pure module function db_table_index(db, name) result(idx)
            class(db_t),       intent(in) :: db  !! Database handle
            character(len=*), intent(in) :: name  !! Table name to look up
            integer                      :: idx  !! Slot in `db%tables`, 0 if absent
        end function

        !! `.true.` if an index slot is live; `.false.` if it has been dropped
        !! (tombstoned with `ncols = 0`).  Callers walking `table_t%indices`
        !! must skip dead slots — their `columns` array is deallocated.
        pure module function idx_live(ix) result(yes)
            type(index_t), intent(in) :: ix  !! Index slot to test
            logical                   :: yes  !! Live (not dropped)
        end function

        !! Insert a row.  `buf` is a row-shaped buffer filled via the
        !! `row_set_*` helpers; `DT_TEXT` columns are zeroed here and
        !! populated afterwards with `db_set_text`.  A unique-index
        !! violation fails with `SQR_DUP` and writes no row.
        module subroutine db_insert(db, table_name, buf, row_id, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            character(len=*), intent(in)           :: buf  !! Row buffer to insert
            integer(int32),   intent(out)          :: row_id  !! Assigned row id (0 on failure)
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Fetch a live row by id into `buf`.  A tombstoned or
        !! out-of-range row returns `SQR_NOT_FOUND`.
        module subroutine db_get(db, table_name, row_id, buf, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            integer(int32),   intent(in)           :: row_id  !! Row id to fetch
            character(len=*), intent(out)          :: buf  !! Receives the record buffer
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Rewrite an existing live row in place.  Records are fixed-size
        !! so the on-disk slot never changes; index entries are maintained
        !! for any indexed column whose key bytes change.  `DT_TEXT`
        !! descriptors are preserved from the stored row (text is changed
        !! via `db_set_text`, as for insert).
        module subroutine db_update(db, table_name, row_id, buf, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            integer(int32),   intent(in)           :: row_id  !! Row id to rewrite
            character(len=*), intent(in)           :: buf  !! New record buffer
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Tombstone a live row.  Space is not reclaimed until
        !! `db_compact`.
        module subroutine db_delete(db, table_name, row_id, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            integer(int32),   intent(in)           :: row_id  !! Row id to delete
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Iterate every live row, invoking `cb` for each until it sets
        !! `stop` or the table is exhausted.
        module subroutine db_scan(db, table_name, cb, ctx, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            procedure(scan_cb)                     :: cb  !! Per-row callback
            class(*),         intent(inout)        :: ctx  !! Opaque context threaded to `cb`
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Set (or replace) the text of a `DT_TEXT` column on a live row.
        !! Bytes are appended to `<table>.blob` and the in-row descriptor
        !! updated.
        module subroutine db_set_text(db, table_name, row_id, col_name, text, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            integer(int32),   intent(in)           :: row_id  !! Row id
            character(len=*), intent(in)           :: col_name  !! `DT_TEXT` column name
            character(len=*), intent(in)           :: text  !! New text value
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Read the text of a `DT_TEXT` column from a live row.  Returns
        !! an empty string for an empty value.
        module subroutine db_get_text(db, table_name, row_id, col_name, text, stat)
            class(db_t),       intent(inout)            :: db  !! Database handle
            character(len=*), intent(in)               :: table_name  !! Target table
            integer(int32),   intent(in)               :: row_id  !! Row id
            character(len=*), intent(in)               :: col_name  !! `DT_TEXT` column name
            character(len=:), allocatable, intent(out) :: text  !! Receives the text value
            integer,          intent(out), optional    :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Single-column overload of `db_create_index`.
        module subroutine db_create_index_1(db, table_name, col_name, stat, unique)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_name  !! Column to index
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
            logical,          intent(in),  optional :: unique  !! Enforce uniqueness (default `.false.`)
        end subroutine

        !! Composite overload of `db_create_index`.  Member columns form
        !! the key in the given order.
        module subroutine db_create_index_m(db, table_name, col_names, stat, unique)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_names(:)  !! Ordered member columns
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
            logical,          intent(in),  optional :: unique  !! Enforce uniqueness (default `.false.`)
        end subroutine

        !! Single-column overload of `db_drop_index`.
        module subroutine db_drop_index_1(db, table_name, col_name, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_name  !! Indexed column
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Drop the secondary index whose member columns exactly match
        !! `col_names`.  The index file is deleted and the slot tombstoned —
        !! slot numbers stay stable so the `__i<slot>` file naming of surviving
        !! indices is undisturbed, and a later `db_create_index` simply appends a
        !! fresh slot.  `SQR_NOT_FOUND` if no index covers exactly those columns.
        module subroutine db_drop_index_m(db, table_name, col_names, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_names(:)  !! Index member columns
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Insert a batch of rows in one call, deferring index maintenance to a
        !! single rebuild per index (the bulk-load path) rather than a
        !! per-row tree insert.  `bufs(k)` is the row buffer for row `k` (filled
        !! like `db_insert`'s `buf`); `row_ids(k)` receives its assigned id.
        !! All rows are validated (NULL-member skip, NaN reject, uniqueness
        !! against the existing index *and* within the batch) before anything is
        !! written, so a `SQR_DUP` / `SQR_INVALID` violation rejects the whole
        !! batch with nothing inserted (`row_ids = 0`).  `row_ids` must be at
        !! least `size(bufs)` long.
        module subroutine db_insert_many(db, table_name, bufs, row_ids, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: bufs(:)  !! Row buffers to insert
            integer(int32),   intent(out)           :: row_ids(:)  !! Assigned ids (0 on failure)
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Walk a table's on-disk structures and check they agree: the live-row
        !! recount matches `live_count`, `next_id` covers every written record,
        !! every live non-NULL-member row is present in each index, every index
        !! entry points at a live row whose key matches, and a unique index has
        !! no duplicate live keys.  Read-only.  `SQR_OK` if consistent,
        !! `SQR_INVALID` (with `errmsg` describing the first problem) otherwise.
        module subroutine db_verify(db, table_name, stat, errmsg)
            class(db_t),       intent(inout)           :: db  !! Database handle
            character(len=*), intent(in)              :: table_name  !! Table to check
            integer,          intent(out),  optional  :: stat  !! `SQR_OK` / `SQR_INVALID` / `SQR_ERR`
            character(len=*), intent(inout), optional :: errmsg  !! First inconsistency detail
        end subroutine

        !! Fetch a row by natural key.  Resolves the unique index over
        !! `col_names`, finds the live row whose key columns in `keyrow`
        !! match, and copies it into `buf`.  `keyrow` is a row-shaped
        !! buffer the caller filled with just the key columns via the
        !! `row_set_*` helpers.  `row_id` optionally returns the resolved
        !! live row's id (0 if not resolved) so the caller can follow up
        !! with row-id-keyed operations such as `db_get_text`.
        module subroutine db_get_by_key(db, table_name, col_names, keyrow, buf, stat, row_id)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_names(:)  !! Unique index member columns
            character(len=*), intent(in)            :: keyrow  !! Row-shaped buffer holding the key columns
            character(len=*), intent(out)           :: buf  !! Receives the matched record
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
            integer(int32),   intent(out), optional :: row_id  !! Resolved row id (0 if unresolved)
        end subroutine

        !! Update a row by natural key (resolve via the unique index,
        !! then delegate to `db_update`).
        module subroutine db_update_by_key(db, table_name, col_names, keyrow, newrow, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_names(:)  !! Unique index member columns
            character(len=*), intent(in)            :: keyrow  !! Row-shaped buffer holding the key columns
            character(len=*), intent(in)            :: newrow  !! New record buffer
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Delete a row by natural key (resolve via the unique index,
        !! then delegate to `db_delete`).
        module subroutine db_delete_by_key(db, table_name, col_names, keyrow, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle
            character(len=*), intent(in)            :: table_name  !! Target table
            character(len=*), intent(in)            :: col_names(:)  !! Unique index member columns
            character(len=*), intent(in)            :: keyrow  !! Row-shaped buffer holding the key columns
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Equality lookup of the first live row whose indexed `int32`
        !! column equals `key`.
        module subroutine db_find_by_int(db, table_name, col_name, key, row_id, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            character(len=*), intent(in)           :: col_name  !! Indexed column
            integer(int32),   intent(in)           :: key  !! Value to match
            integer(int32),   intent(out)          :: row_id  !! Matched row id (0 if none)
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Equality lookup on an indexed `real64` column.
        !!
        !! Exact, bit-for-bit equality — deliberately no epsilon.  Storage
        !! is a pure binary `transfer` with no decimal round-trip, so the
        !! same `real64` value that was inserted matches; a value the
        !! caller recomputes differently (`0.1+0.2` vs a stored `0.3`)
        !! will not — that is inherent to floating point.  Tolerance
        !! matching is a range query, not an equality lookup.
        module subroutine db_find_by_real(db, table_name, col_name, key, row_id, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            character(len=*), intent(in)           :: col_name  !! Indexed column
            real(real64),     intent(in)           :: key  !! Value to match (exact)
            integer(int32),   intent(out)          :: row_id  !! Matched row id (0 if none)
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Equality lookup on an indexed `DT_CHAR` column.  The key is
        !! NUL-padded to the column width before comparison.
        module subroutine db_find_by_char(db, table_name, col_name, key, row_id, stat)
            class(db_t),       intent(inout)        :: db  !! Database handle
            character(len=*), intent(in)           :: table_name  !! Target table
            character(len=*), intent(in)           :: col_name  !! Indexed column
            character(len=*), intent(in)           :: key  !! Value to match
            integer(int32),   intent(out)          :: row_id  !! Matched row id (0 if none)
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        ! ===== Ordered cursor / range queries =====

        !! Open an ascending cursor over every live row, in the key order of an
        !! index on `col_name`: an exact single-column index if one exists,
        !! otherwise a composite index whose **leading** member is `col_name`
        !! (its B+-tree order is primarily by that member).  The whole-index
        !! complement to `db_find_range`; pull rows with `db_cursor_next`.  Fails
        !! with `SQR_NOT_FOUND` if the table has no such index.  NULL-member rows
        !! are not in the index and so are never yielded.
        module subroutine db_open_cursor(db, table_name, col_name, cur, stat)
            class(db_t),        intent(inout)         :: db  !! Database handle
            character(len=*),  intent(in)            :: table_name  !! Target table
            character(len=*),  intent(in)            :: col_name  !! Indexed column to order by
            type(db_cursor_t), intent(out)           :: cur  !! Positioned cursor
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! `int32` band overload of `db_find_range`.
        module subroutine db_find_range_int(db, table_name, col_name, lo, hi, cur, stat)
            class(db_t),        intent(inout)         :: db  !! Database handle
            character(len=*),  intent(in)            :: table_name  !! Target table
            character(len=*),  intent(in)            :: col_name  !! Indexed column
            integer(int32),    intent(in)            :: lo  !! Inclusive lower bound
            integer(int32),    intent(in)            :: hi  !! Inclusive upper bound
            type(db_cursor_t), intent(out)           :: cur  !! Positioned cursor
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! `real64` band overload of `db_find_range`.
        module subroutine db_find_range_real(db, table_name, col_name, lo, hi, cur, stat)
            class(db_t),        intent(inout)         :: db  !! Database handle
            character(len=*),  intent(in)            :: table_name  !! Target table
            character(len=*),  intent(in)            :: col_name  !! Indexed column
            real(real64),      intent(in)            :: lo  !! Inclusive lower bound
            real(real64),      intent(in)            :: hi  !! Inclusive upper bound
            type(db_cursor_t), intent(out)           :: cur  !! Positioned cursor
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! `DT_CHAR` band overload of `db_find_range` (bounds NUL-padded to
        !! the column width).
        module subroutine db_find_range_char(db, table_name, col_name, lo, hi, cur, stat)
            class(db_t),        intent(inout)         :: db  !! Database handle
            character(len=*),  intent(in)            :: table_name  !! Target table
            character(len=*),  intent(in)            :: col_name  !! Indexed column
            character(len=*),  intent(in)            :: lo  !! Inclusive lower bound
            character(len=*),  intent(in)            :: hi  !! Inclusive upper bound
            type(db_cursor_t), intent(out)           :: cur  !! Positioned cursor
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Yield the next live row at or after the cursor, in ascending key
        !! order, advancing past it.  `ok` is `.false.` (with `stat == SQR_OK`)
        !! when the cursor is exhausted — for `db_find_range`, when the band's
        !! upper bound is passed — and `row_id`/`buf` are then unset.
        module subroutine db_cursor_next(db, cur, row_id, buf, ok, stat)
            class(db_t),        intent(inout)         :: db  !! Database handle
            type(db_cursor_t), intent(inout)         :: cur  !! Cursor (advanced)
            integer(int32),    intent(out)           :: row_id  !! Yielded row id (0 if none)
            character(len=*),  intent(out)           :: buf  !! Receives the record buffer
            logical,           intent(out)           :: ok  !! `.true.` if a row was yielded
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        ! ===== Row buffer helpers =====

        !! Allocate a zeroed row buffer of `n` bytes.
        pure module subroutine row_alloc(buf, n)
            character(len=:), allocatable, intent(out) :: buf  !! Allocated, zero-filled buffer
            integer,                       intent(in)  :: n  !! Buffer size in bytes
        end subroutine

        !! Zero an existing row buffer in place.
        pure module subroutine row_clear(buf)
            character(len=*), intent(inout) :: buf  !! Buffer to clear
        end subroutine

        !! Read the status byte (`ROW_ALIVE` / `ROW_TOMBSTONE`).
        pure module function row_status(buf) result(s)
            character(len=*), intent(in) :: buf  !! Row buffer
            integer(int8) :: s  !! Status byte value
        end function

        !! Write the status byte.
        pure module subroutine row_set_status(buf, s)
            character(len=*), intent(inout) :: buf  !! Row buffer
            integer(int8),    intent(in)    :: s  !! New status byte
        end subroutine

        !! Mark `col` NULL in the row's bitmap.  A NULL column reads back as
        !! absent and is omitted from any index it is a member of (a row with
        !! any NULL index member is simply not in that index).
        pure module subroutine row_set_null(buf, col)
            character(len=*), intent(inout) :: buf  !! Row buffer
            type(column_t),   intent(in)    :: col  !! Column to mark NULL
        end subroutine

        !! Clear `col`'s NULL bit (mark it as carrying a value).  The
        !! `row_set_int` / `row_set_real` / `row_set_char` helpers do this
        !! implicitly, so this is only needed to un-NULL without writing a value.
        pure module subroutine row_clear_null(buf, col)
            character(len=*), intent(inout) :: buf  !! Row buffer
            type(column_t),   intent(in)    :: col  !! Column to mark not-NULL
        end subroutine

        !! `.true.` if `col` is NULL in this row.
        pure module function row_is_null(buf, col) result(isnull)
            character(len=*), intent(in) :: buf  !! Row buffer
            type(column_t),   intent(in) :: col  !! Column to test
            logical :: isnull  !! `.true.` if the column's NULL bit is set
        end function

        !! Pack an `int32` value into a `DT_INT` column slot.
        pure module subroutine row_set_int(buf, col, val)
            character(len=*), intent(inout) :: buf  !! Row buffer
            type(column_t),   intent(in)    :: col  !! Target `DT_INT` column
            integer(int32),   intent(in)    :: val  !! Value to store
        end subroutine

        !! Unpack an `int32` value from a `DT_INT` column slot.
        pure module function row_get_int(buf, col) result(val)
            character(len=*), intent(in) :: buf  !! Row buffer
            type(column_t),   intent(in) :: col  !! Source `DT_INT` column
            integer(int32) :: val  !! Decoded value
        end function

        !! Pack a `real64` value into a `DT_REAL` column slot.
        pure module subroutine row_set_real(buf, col, val)
            character(len=*), intent(inout) :: buf  !! Row buffer
            type(column_t),   intent(in)    :: col  !! Target `DT_REAL` column
            real(real64),     intent(in)    :: val  !! Value to store
        end subroutine

        !! Unpack a `real64` value from a `DT_REAL` column slot.
        pure module function row_get_real(buf, col) result(val)
            character(len=*), intent(in) :: buf  !! Row buffer
            type(column_t),   intent(in) :: col  !! Source `DT_REAL` column
            real(real64) :: val  !! Decoded value
        end function

        !! Store a string into a `DT_CHAR` column slot (NUL-padded,
        !! truncated to the column width).
        pure module subroutine row_set_char(buf, col, val)
            character(len=*), intent(inout) :: buf  !! Row buffer
            type(column_t),   intent(in)    :: col  !! Target `DT_CHAR` column
            character(len=*), intent(in)    :: val  !! Value to store
        end subroutine

        !! Read a string from a `DT_CHAR` column slot (up to the first
        !! NUL).
        pure module function row_get_char(buf, col) result(val)
            character(len=*), intent(in)  :: buf  !! Row buffer
            type(column_t),   intent(in)  :: col  !! Source `DT_CHAR` column
            character(len=:), allocatable :: val  !! Decoded string
        end function

        ! --- Rollback journal (durability/atomicity) ---

        !! Open an explicit transaction.  Thin façade over `txn_begin` that
        !! also marks the in-flight txn as user-owned so the auto-commit
        !! brackets leave it open and so re-entry is detected.  No nesting in
        !! v1: a `db_begin` while a transaction is already in flight fails
        !! `SQR_INVALID`.  Maps onto SQL `BEGIN`.
        module subroutine db_begin(db, stat)
            class(db_t), intent(inout), target :: db  !! Database handle
            integer,    intent(out),  optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Commit the explicit transaction opened by `db_begin`, keeping every
        !! change and discarding the undo set.  Fails `SQR_INVALID` if no
        !! explicit transaction is in flight.  Maps onto SQL `COMMIT`.
        module subroutine db_commit(db, stat)
            class(db_t), intent(inout) :: db  !! Database handle
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Roll back the explicit transaction opened by `db_begin`, restoring
        !! every base file and in-memory counter to its pre-`db_begin` state.
        !! Fails `SQR_INVALID` if no explicit transaction is in flight.  Maps
        !! onto SQL `ROLLBACK`.
        module subroutine db_rollback(db, stat)
            class(db_t), intent(inout) :: db  !! Database handle
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Begin a transaction.  Clears the in-memory undo set and marks the
        !! journal header invalid (reusing the file).  Lazily creates and
        !! pre-sizes `<db>/_journal.dat` on the first transaction of a
        !! session.  Fails `SQR_READONLY` on a read-only handle.
        !! Also installs the rollback journal hook on every live index tree, so
        !! their B+-tree page writes capture undo records.  `db` is `target` so
        !! each hook context can hold a lasting pointer back to the handle — the
        !! caller's `db_t` must therefore have the `target` attribute for
        !! journalling to work.
        module subroutine txn_begin(db, stat)
            class(db_t), intent(inout), target :: db  !! Database handle
            integer,    intent(out),  optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Capture the original bytes of an in-place overwrite *before* the
        !! caller performs it.  Idempotent per `(path, offset, length)` within
        !! a transaction.  `path` is relative to the database directory.
        !! When `bytes` is supplied it is taken as the pre-image directly (the
        !! caller already holds a consistent view of the region, e.g. read via
        !! the same unit it is about to write); otherwise the region is read
        !! back from the file.  When `bytes` is present `length` is ignored and
        !! `len(bytes)` is used.
        module subroutine jrnl_log_region(db, path, offset, length, bytes, stat)
            class(db_t),       intent(inout)         :: db  !! Database handle (transaction active)
            character(len=*),  intent(in)            :: path  !! Base file, relative to the db directory
            integer(int64),    intent(in)            :: offset  !! 1-based byte offset of the region
            integer(int64),    intent(in)            :: length  !! Region length in bytes
            character(len=*),  intent(in),  optional :: bytes  !! Caller-supplied pre-image (overrides re-read)
            integer,           intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Capture a file's original length before the caller appends to or
        !! grows it; rollback truncates the appended bytes away.  Idempotent
        !! per `path` within a transaction.
        module subroutine jrnl_log_extend(db, path, stat)
            class(db_t),      intent(inout)         :: db  !! Database handle (transaction active)
            character(len=*), intent(in)            :: path  !! Base file, relative to the db directory
            integer,          intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Arm the journal (make it hot): serialise the undo set to the file,
        !! write a valid header with count + checksum, and `fsync`.  Must be
        !! called after all `jrnl_log_*` and before any base-file write, so a
        !! crash between here and commit is recoverable.
        module subroutine txn_arm(db, stat)
            class(db_t), intent(inout)        :: db  !! Database handle (transaction active)
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Commit: the durable commit point.  Zeroes the journal header and
        !! `fsync`s it, so recovery sees nothing to do.  The caller must have
        !! already `fsync`ed its base-file writes.
        module subroutine txn_commit(db, stat)
            class(db_t), intent(inout)        :: db  !! Database handle (transaction active)
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Roll back the active transaction from the in-memory undo set:
        !! restore captured regions, truncate extended files, `fsync`, then
        !! invalidate the journal.  Used on a same-process failure path.
        module subroutine txn_rollback(db, stat)
            class(db_t), intent(inout)        :: db  !! Database handle (transaction active)
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! Recover at open: if a hot (valid) journal exists, replay its undo
        !! records in reverse to restore the pre-transaction state, `fsync`,
        !! then invalidate it.  A missing, empty, invalidated or corrupt
        !! journal is a no-op success.
        module subroutine jrnl_recover(db, stat)
            class(db_t), intent(inout)        :: db  !! Database handle
            integer,    intent(out), optional :: stat  !! `SQR_OK` or an error code
        end subroutine

        !! `.true.` if a hot (valid, un-committed) journal is present on disk —
        !! a read-only probe that writes nothing, used by a read-only `db_open`
        !! to refuse a database that needs recovery it cannot perform.  An
        !! absent, voided or unreadable journal reports `.false.`.
        module function jrnl_hot(db) result(hot)
            class(db_t), intent(in) :: db  !! Database handle
            logical                 :: hot  !! A hot journal is present
        end function

        !! `bt_journal_hook` implementation that records a B+-tree page write in
        !! the rollback journal.  Install it on a tree with `bt_set_journal_hook`,
        !! passing a `bt_jhook_ctx_t` as the context.  An in-place overwrite
        !! (`is_new = .false.`) is captured as a region with the tree's own
        !! pre-image `old_bytes` (a consistent view — see `jrnl_log_region`'s
        !! `bytes`); a freshly allocated page (`is_new = .true.`) is captured as
        !! an extend of the tree file.  A non-`SQR_OK` journal result (or a
        !! foreign context) returns a non-zero `stat`, which aborts the page
        !! write so an un-recorded overwrite never reaches disk.
        module subroutine bt_journal_adapter(ctx, offset, old_bytes, is_new, stat)
            class(*),         intent(in)  :: ctx        !! A `bt_jhook_ctx_t`
            integer(int64),   intent(in)  :: offset     !! 1-based byte position of the page
            character(len=*), intent(in)  :: old_bytes  !! Page pre-image (empty if `is_new`)
            logical,          intent(in)  :: is_new     !! Page newly allocated this txn
            integer,          intent(out) :: stat       !! `0` = OK; non-zero aborts the write
        end subroutine
    end interface

contains

end module sqr

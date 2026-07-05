! sqr_base — shared engine internals for the sqr implementation submodules.
!
! Intermediate submodule of `sqr`: every descendant feature submodule sees
! the entities declared here by host association, whereas sibling submodules
! of `sqr` do not.  This is the storage/engine core
! — path and filesystem helpers, name/column validation, the on-disk catalog
! and schema codecs, data/index/blob file opening, the composite-key
! comparison and extraction primitives, the B+-tree bulk rebuild, and the
! per-row NULL-bitmap helper.  The feature submodules build on top of it.
!
! Catalog layout (<dir>/_catalog.dat, stream-access binary):
!   [4 bytes  magic "SQRC"]
!   [int32    schema_version]
!   [int32    ntables]
!   For each table:
!     [SQR_NAME_LEN bytes name]
!
! Schema file layout (<dir>/<name>.schema, stream-access binary):
!   [4 bytes  magic "SQRT"]
!   [int32    schema_version, ncols, record_size, next_id, live_count, nindices]
!   For each column: [SQR_NAME_LEN bytes name, int32 dtype, int32 csize, int32 offset]
!   For each index : [int32 ncols, ncols * SQR_NAME_LEN-byte member name,
!                     int32 key_size, int32 unique]
!   (the entry count is authoritative in the index's B+-tree, not here)
!
! Data file layout (<dir>/<name>.dat, direct-access, recl = record_size):
!   record N (1..next_id-1) is one fixed-size binary blob.
!
! Index file layout (<dir>/<name>__i<slot>.idx): a generic on-disk
! B+-tree (see the b_tree module) keyed by the composite key bytes with
! the int32 row id as the payload.
!
! Submodule map — the five feature submodules below all descend from sqr_base
! and reach its entities (and each other's module procedures) by host
! association; they cannot see each other's private contained procedures, so
! anything shared lives here:
!   sqr_table  — table lifecycle: db_open/close, create/drop table, compact, list
!   sqr_record — per-row API: insert/get/update/delete/scan, text, per-row index upkeep
!   sqr_index  — index query/maintenance: create index, find-by, cursors, ranges, by-key
!   sqr_admin  — whole-table maintenance: drop index, batch insert, verify
!   sqr_rowbuf — typed row-buffer accessors (row_*)

submodule (sqr) sqr_base
    use, intrinsic :: iso_fortran_env, only: error_unit  ! int8/int32/int64/real64 via host association from sqr
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use :: clib_wrap, only: c_mkdir, c_path_exists, c_lock_release, &
                            c_rename, c_fsync_path, c_fsync_dir
    use :: sqr_fault, only: io_check
    use :: b_tree, only: btree_t, bt_open, bt_close, bt_reload, bt_sync, bt_insert, &
                         bt_remove, bt_bulk_load, bt_seek, bt_first, bt_next, &
                         bt_cursor_t, bt_set_journal_hook, BT_OK, BT_VERSION
    implicit none

    ! (kc_ctx_t, the opaque comparator context threaded through the
    ! B+-tree, lives in the sqr module now that index_t caches one.)

    character(len=4), parameter :: CATALOG_MAGIC = 'SQRC'
    character(len=*), parameter :: CATALOG_FILE  = '_catalog.dat'
    character(len=*), parameter :: LOCK_FILE     = '_lock'

    ! Upper bound on a database directory path (Linux PATH_MAX).
    integer, parameter :: SQR_MAX_DIR = 4096

    ! SQR_BOM (public, in the sqr module) byte-swapped: the value the mark
    ! reads back as on a host of the opposite endianness, so a wrong-byte-
    ! order database can be reported as such rather than as generic corruption.
    integer(int32), parameter :: SQR_BOM_SWAP = int(z'04030201', int32)

contains

    ! Byte position (1-based) and bit (0..7) of a column's NULL flag within the
    ! bitmap that follows the status byte. Bit `null_bit` lives in byte
    ! 2 + null_bit/8 (see layout_columns).
    pure subroutine null_bit_pos(col, bytepos, bit)
        type(column_t), intent(in)  :: col
        integer,        intent(out) :: bytepos, bit
        bytepos = 2 + col%null_bit / 8
        bit     = mod(col%null_bit, 8)
    end subroutine

    ! ===== Path helpers =====

    ! Path-separator test. Every sqr-internal path uses '/', and any
    ! user-supplied path is folded to '/' on entry (norm_seps, called from
    ! db_open), so '/' is the one and only separator the engine reasons about
    ! -- no per-platform branch, no preprocessing.
    pure function is_sep(ch) result(yes)
        character(len=1), intent(in) :: ch
        logical :: yes
        yes = ch == '/'
    end function

    ! Normalise a user-supplied path to the engine's single separator. Windows
    ! accepts '/' and '\' interchangeably; folding '\' to '/' here means
    ! validation, component splitting and mkdir_p all reason about '/' alone,
    ! with no platform awareness anywhere in the Fortran. The one consequence
    ! is that a database directory name may not contain a literal '\' (a legal
    ! byte in a POSIX filename) -- an entirely acceptable restriction for a db
    ! path, alongside the existing "no control characters, no '..'" rules.
    pure function norm_seps(path) result(out)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: out
        integer :: k
        out = path
        do k = 1, len(out)
            if (out(k:k) == char(92)) out(k:k) = '/'
        end do
    end function

    pure function pathjoin(d, f) result(p)
        character(len=*), intent(in)  :: d, f
        character(len=:), allocatable :: p
        if (len_trim(d) == 0) then
            p = trim(f)
        else if (d(len_trim(d):len_trim(d)) == '/') then
            p = trim(d) // trim(f)
        else
            p = trim(d) // '/' // trim(f)
        end if
    end function

    pure function catalog_path(db) result(p)
        type(db_t), intent(in)        :: db
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, CATALOG_FILE)
    end function

    pure function lock_path(db) result(p)
        type(db_t), intent(in)        :: db
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, LOCK_FILE)
    end function

    pure function schema_path(db, name) result(p)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, trim(name) // '.schema')
    end function

    pure function data_path(db, name) result(p)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, data_relpath(name))
    end function

    ! The data file name relative to the db directory — the form the journal
    ! records (it joins db%dir itself).  data_path() prepends db%dir for opens.
    pure function data_relpath(name) result(rel)
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: rel
        rel = trim(name) // '.dat'
    end function

    ! Index files are named by their 1-based slot in the table's index
    ! list, not by member column names: column names pass the permissive
    ! valid_name (may contain '+', '-', spaces) so joining them into a path
    ! is unsafe, and a composite index has several. Dropping an index
    ! tombstones its slot rather than renumbering, so a live slot is stable
    ! for the table's lifetime.
    pure function index_path(db, table_name, slot) result(p)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: table_name
        integer,          intent(in)  :: slot
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, index_relpath(table_name, slot))
    end function

    ! The index file name relative to the db directory — the form the journal
    ! records (it joins db%dir itself).  index_path() prepends db%dir for opens.
    pure function index_relpath(table_name, slot) result(rel)
        character(len=*), intent(in)  :: table_name
        integer,          intent(in)  :: slot
        character(len=:), allocatable :: rel
        character(len=16) :: s
        write(s, '(i0)') slot
        rel = trim(table_name) // '__i' // trim(s) // '.idx'
    end function

    pure function blob_path(db, name) result(p)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, blob_relpath(name))
    end function

    ! Marker dropped by db_compact between building the compacted temp files
    ! and clearing them after a durable swap.  Its presence on open means a
    ! compact was interrupted mid-swap and must be finished (the .compact temps
    ! are authoritative once the marker exists — see complete_compact_swap).
    pure function compact_marker_path(db, name) result(p)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: p
        p = pathjoin(db%dir, trim(name) // '.compacting')
    end function

    ! Order-sensitive rolling checksum over a byte image, folded into a
    ! non-negative int32. Used by the journal (torn-payload detection) and the
    ! pack/unpack codec (truncated-container detection); one definition here so
    ! both submodules share it by host association.
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

    ! The blob file name relative to the db directory — the form the journal
    ! records (it joins db%dir itself).  blob_path() prepends db%dir for opens.
    pure function blob_relpath(name) result(rel)
        character(len=*), intent(in)  :: name
        character(len=:), allocatable :: rel
        rel = trim(name) // '.blob'
    end function

    pure function table_has_text(tbl) result(yes)
        type(table_t), intent(in) :: tbl
        logical :: yes
        yes = any(tbl%cols(1:tbl%ncols)%dtype == DT_TEXT)
    end function

    ! An index slot is live unless it has been dropped: db_drop_index deletes
    ! the file and tombstones the slot (ncols = 0) rather than renumbering, so
    ! surviving indices keep their __i<slot> file names. Every loop over a
    ! table's indices skips dead slots.
    pure module function idx_live(ix) result(yes)
        type(index_t), intent(in) :: ix
        logical :: yes
        yes = ix%ncols > 0
    end function

    ! ===== Filesystem probe =====

    function file_exists(p) result(ok)
        character(len=*), intent(in) :: p
        logical :: ok
        inquire(file=p, exist=ok)
    end function

    ! Create `path` and every missing parent (mkdir -p semantics) via libc
    ! mkdir(2) — no shell. A component that already exists (EEXIST, or any
    ! mkdir failure where the path is in fact present) is fine; any other
    ! failure stops and reports .false.
    function mkdir_p(path) result(ok)
        character(len=*), intent(in) :: path
        logical :: ok
        integer :: k, n, ios
        n = len_trim(path)
        ok = .true.
        make: do k = 2, n
            if (.not. is_sep(path(k:k))) cycle make
            ios = c_mkdir(path(1:k-1))
            if (ios /= 0 .and. .not. c_path_exists(path(1:k-1))) then
                ok = .false.
                return
            end if
        end do make
        ios = c_mkdir(path(1:n))
        ok = ios == 0 .or. c_path_exists(path(1:n))
    end function

    ! ===== Name / schema validation =====

    ! Database directory: an ordinary filesystem path. The old conservative
    ! character class existed only so the name was safe to embed in
    ! `mkdir -p '...'`, but the shell went away with the 2026-05-18 clib_wrap
    ! conversion — c_mkdir calls libc mkdir(2) directly, no subprocess, no
    ! quoting surface — so that rationale has expired. A path now only has to
    ! be sane: non-empty, within PATH_MAX, no control characters, and no '..'
    ! path component (which could let a name escape its intended location).
    ! '/' is allowed, so a database may be nested or live at an absolute /
    ! network-mounted location (the CAD shared-data use case).
    pure function valid_dir_name(name) result(ok)
        character(len=*), intent(in) :: name
        logical :: ok
        integer :: n, k, cstart
        logical :: boundary
        n = len_trim(name)
        ok = n > 0 .and. n <= SQR_MAX_DIR
        if (.not. ok) return
        ! No control characters anywhere.
        ctrl_scan: do k = 1, n
            if (iachar(name(k:k)) < 32 .or. iachar(name(k:k)) == 127) then
                ok = .false.
                return
            end if
        end do ctrl_scan
        ! Reject any '..' path component. Walk separator-delimited segments;
        ! a boundary is a path separator ('/', the engine's sole separator
        ! after norm_seps) or the end of the string. (.or. does not
        ! short-circuit, so test the end-of-string case before indexing.)
        cstart = 1
        comp_scan: do k = 1, n + 1
            boundary = (k == n + 1)
            if (.not. boundary) boundary = is_sep(name(k:k))
            if (boundary) then
                if (k - cstart == 2) then
                    if (name(cstart:cstart+1) == '..') then
                        ok = .false.
                        exit comp_scan
                    end if
                end if
                cstart = k + 1
            end if
        end do comp_scan
    end function

    ! Accept non-empty names up to SQR_NAME_LEN bytes, with no path separators,
    ! no parent-directory traversal, and no control characters. Used as a guard
    ! before any name is concatenated into a filesystem path.
    pure function valid_name(name) result(ok)
        character(len=*), intent(in) :: name
        logical :: ok
        integer :: n, k
        n = len_trim(name)
        ok = n > 0 .and. n <= SQR_NAME_LEN                        &
             .and. scan(name(1:n), '/' // char(92)) == 0           &
             .and. index(name(1:n), '..') == 0
        ! Reject control characters (0..31) and DEL (127). A scalar loop avoids
        ! the logical array temporary an all([...]) constructor would create.
        if (ok) then
            scan_ctrl: do k = 1, n
                if (iachar(name(k:k)) < 32 .or. iachar(name(k:k)) == 127) then
                    ok = .false.
                    exit scan_ctrl
                end if
            end do scan_ctrl
        end if
    end function

    pure subroutine validate_columns(cols, stat, errmsg)
        type(column_t),    intent(in)              :: cols(:)
        integer,           intent(out)             :: stat
        character(len=*),  intent(inout), optional :: errmsg
        integer :: i, j, total
        stat = SQR_INVALID
        if (size(cols) == 0) then
            if (present(errmsg)) errmsg = 'table must have at least one column'
            return
        end if
        total = 0
        col_loop: do i = 1, size(cols)
            associate (c => cols(i))
                if (.not. valid_name(c%name)) then
                    if (present(errmsg)) errmsg = 'invalid column name: "' // trim(c%name) // '"'
                    return
                end if
                select case (c%dtype)
                case (DT_INT)
                    if (c%csize /= 4) then
                        if (present(errmsg)) errmsg = 'DT_INT column "' // trim(c%name) // '" must have csize=4'
                        return
                    end if
                case (DT_REAL)
                    if (c%csize /= 8) then
                        if (present(errmsg)) errmsg = 'DT_REAL column "' // trim(c%name) // '" must have csize=8'
                        return
                    end if
                case (DT_CHAR)
                    if (c%csize <= 0 .or. c%csize > 65536) then
                        if (present(errmsg)) errmsg = 'DT_CHAR column "' // trim(c%name) // '" csize must be 1..65536'
                        return
                    end if
                case (DT_TEXT)
                    if (c%csize /= SQR_TEXT_DESC) then
                        if (present(errmsg)) errmsg = 'DT_TEXT column "' // trim(c%name) // &
                            '" must have csize=SQR_TEXT_DESC'
                        return
                    end if
                case default
                    if (present(errmsg)) errmsg = 'unknown dtype for column "' // trim(c%name) // '"'
                    return
                end select
                dup_check: do j = 1, i - 1
                    if (trim(cols(j)%name) == trim(c%name)) then
                        if (present(errmsg)) errmsg = 'duplicate column name: "' // trim(c%name) // '"'
                        return
                    end if
                end do dup_check
                total = total + c%csize
            end associate
        end do col_loop
        if (total + 1 + null_bytes(size(cols)) > SQR_MAX_RECORD) then
            if (present(errmsg)) errmsg = 'record size too large'
            return
        end if
        stat = SQR_OK
    end subroutine

    ! ===== Column layout =====

    ! Bytes of NULL bitmap for a table of `ncols` columns: one bit per column,
    ! rounded up. The bitmap sits between the status byte and the column data,
    ! so the first column starts at offset 2 + null_bytes(ncols).
    pure function null_bytes(ncols) result(nb)
        integer, intent(in) :: ncols
        integer :: nb
        nb = (ncols + 7) / 8
    end function

    pure subroutine layout_columns(cols, record_size)
        type(column_t), intent(inout) :: cols(:)
        integer,        intent(out)   :: record_size
        integer :: i, off
        off = 2 + null_bytes(size(cols))     ! byte 1 = status, then NULL bitmap
        layout_loop: do i = 1, size(cols)
            associate (c => cols(i))
                c%offset   = off
                c%null_bit = i - 1
                off = off + c%csize
            end associate
        end do layout_loop
        record_size = off - 1                ! last used byte
    end subroutine

    pure function col_index(tbl, name) result(idx)
        type(table_t),    intent(in) :: tbl
        character(len=*), intent(in) :: name
        integer :: idx
        ! Linear scan over the contiguous cols array; indexing %name inside the
        ! loop avoids the strided component-section copy that findloc would force.
        do idx = 1, tbl%ncols
            if (tbl%cols(idx)%name == name) return
        end do
        idx = 0
    end function

    ! Slot of the index whose member columns exactly match `names` in order
    ! (0 if none). The single-column overload is the lookup the db_find_by_*
    ! equality APIs use.
    pure function index_for_columns(tbl, names) result(idx)
        type(table_t),    intent(in) :: tbl
        character(len=*), intent(in) :: names(:)
        integer :: idx, j, m
        idx = 0
        scan_idx: do j = 1, tbl%nindices
            associate (ix => tbl%indices(j))
                if (ix%ncols /= size(names)) cycle scan_idx
                do m = 1, ix%ncols
                    if (trim(ix%columns(m)) /= trim(names(m))) cycle scan_idx
                end do
                idx = j
                return
            end associate
        end do scan_idx
    end function

    pure function index_index(tbl, col_name) result(idx)
        type(table_t),    intent(in) :: tbl
        character(len=*), intent(in) :: col_name
        integer :: idx
        character(len=len(col_name)) :: one(1)   ! named 1-elt array: no constructor temp
        one(1) = col_name
        idx = index_for_columns(tbl, one)
    end function

    ! ===== Catalog I/O =====

    subroutine read_catalog(db, names, n, stat)
        type(db_t),                               intent(in)  :: db
        character(len=SQR_NAME_LEN), allocatable, intent(out) :: names(:)
        integer,                                  intent(out) :: n
        integer,                                  intent(out) :: stat
        integer :: u, ios, i, ver
        integer(int32) :: bom
        character(len=4) :: magic
        n = 0
        allocate(names(0))
        if (.not. file_exists(catalog_path(db))) then
            stat = SQR_OK
            return
        end if
        open(newunit=u, file=catalog_path(db), access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        read(u, iostat=ios) magic
        call io_check(ios)
        if (ios /= 0 .or. magic /= CATALOG_MAGIC) then
            close(u)
            stat = SQR_ERR
            return
        end if
        ! Byte-order mark, before any int field: a wrong-endian catalog would
        ! otherwise misread the version/count below. Either mismatch (opposite
        ! byte order or corruption) is an unreadable on-disk format.
        read(u, iostat=ios) bom
        call io_check(ios)
        if (ios /= 0 .or. bom /= SQR_BOM) then
            close(u)
            stat = SQR_VERSION
            return
        end if
        ! Single on-disk format: a differing version is corruption.
        read(u, iostat=ios) ver, n
        call io_check(ios)
        if (ios /= 0 .or. ver /= SQR_SCHEMA_VERSION) then
            close(u)
            stat = SQR_VERSION
            return
        end if
        ! ntables is untrusted on-disk data: a corrupt count would drive a
        ! bad allocate before any per-table validation runs. Bound it with
        ! the same corruption ceiling read_schema applies to header counts.
        if (n < 0 .or. n > SQR_MAX_RECORD) then
            close(u)
            stat = SQR_INVALID
            return
        end if
        deallocate(names)
        allocate(names(n), stat=ios)
        if (ios /= 0) then
            close(u)
            stat = SQR_ERR
            return
        end if
        read_names: do i = 1, n
            read(u, iostat=ios) names(i)
            call io_check(ios)
            if (ios /= 0) then
                close(u)
                stat = SQR_ERR
                return
            end if
            ! Names read back from disk are untrusted: a corrupt or crafted
            ! catalog entry (e.g. '../../x') is the only on-disk string that
            ! becomes a filesystem path (schema_path on open, write on close,
            ! delete on drop). Re-validate exactly as db_create_table does so
            ! a bad name is rejected as corruption, not followed out of the
            ! database directory.
            if (.not. valid_name(names(i))) then
                close(u)
                stat = SQR_INVALID
                return
            end if
        end do read_names
        close(u)
        stat = SQR_OK
    end subroutine

    ! Crash-atomic replace of `final` by the freshly-written `tmp`.  fsync the
    ! temp so its bytes are on stable storage, rename(2) it over the target
    ! (the rename is atomic, so a crash leaves either the whole old file or the
    ! whole new one — never a truncated header), then fsync the directory so the
    ! rename itself is durable.  Used by the catalog and schema writers: those
    ! files are the sole record of the database's table list and each table's
    ! column layout, so an in-place `status='replace'` truncate that a crash
    ! caught mid-write would lose the table (or the whole db) with the row data
    ! still intact.
    subroutine atomic_replace(db, tmp, final, stat)
        type(db_t),       intent(in)  :: db
        character(len=*), intent(in)  :: tmp, final
        integer,          intent(out) :: stat
        integer :: ios
        ios = c_fsync_path(tmp)
        call io_check(ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (c_rename(tmp, final) /= 0) then
            stat = SQR_ERR
            return
        end if
        ios = c_fsync_dir(db%dir)
        call io_check(ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    subroutine write_catalog(db, stat)
        type(db_t), intent(in)  :: db
        integer,    intent(out) :: stat
        integer :: u, ios, i
        character(len=4) :: magic
        character(len=SQR_NAME_LEN) :: nm
        character(len=:), allocatable :: final, tmp
        magic = CATALOG_MAGIC
        final = catalog_path(db)
        tmp   = final // '.tmp'
        open(newunit=u, file=tmp, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        write(u, iostat=ios) magic
        call io_check(ios)
        if (ios == 0) write(u, iostat=ios) SQR_BOM
        if (ios == 0) write(u, iostat=ios) SQR_SCHEMA_VERSION, db%ntables
        write_names: do i = 1, db%ntables
            if (ios /= 0) exit write_names
            nm = db%tables(i)%name
            write(u, iostat=ios) nm
            call io_check(ios)
        end do write_names
        if (ios /= 0) then
            close(u, status='delete', iostat=ios)
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)   ! flushes buffered writes; catch a late ENOSPC/EIO
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        call atomic_replace(db, tmp, final, stat)
    end subroutine

    ! ===== Schema I/O =====

    subroutine write_schema(db, tbl, stat)
        type(db_t),    intent(in)  :: db
        type(table_t), intent(in)  :: tbl
        integer,       intent(out) :: stat
        integer :: u, ios, i, m
        character(len=4) :: magic
        character(len=SQR_NAME_LEN) :: nm
        character(len=:), allocatable :: final, tmp
        magic = SQR_MAGIC
        final = schema_path(db, tbl%name)
        tmp   = final // '.tmp'
        open(newunit=u, file=tmp, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        write(u, iostat=ios) magic
        call io_check(ios)
        if (ios == 0) write(u, iostat=ios) SQR_BOM
        if (ios == 0) &
            write(u, iostat=ios) SQR_SCHEMA_VERSION, tbl%ncols, tbl%record_size, &
                                 tbl%next_id, tbl%live_count, tbl%nindices
        cols_out: do i = 1, tbl%ncols
            if (ios /= 0) exit cols_out
            associate (c => tbl%cols(i))
                nm = c%name
                write(u, iostat=ios) nm
                call io_check(ios)
                if (ios == 0) write(u, iostat=ios) c%dtype, c%csize, c%offset
            end associate
        end do cols_out
        idx_out: do i = 1, tbl%nindices
            if (ios /= 0) exit idx_out
            associate (ix => tbl%indices(i))
                write(u, iostat=ios) ix%ncols
                call io_check(ios)
                idx_members: do m = 1, ix%ncols
                    if (ios /= 0) exit idx_members
                    nm = ix%columns(m)
                    write(u, iostat=ios) nm
                    call io_check(ios)
                end do idx_members
                if (ios == 0) &
                    write(u, iostat=ios) ix%key_size, merge(1, 0, ix%unique)
            end associate
        end do idx_out
        if (ios /= 0) then
            close(u, status='delete', iostat=ios)
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)   ! flushes buffered writes; catch a late ENOSPC/EIO
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        call atomic_replace(db, tmp, final, stat)
    end subroutine

    subroutine read_schema(db, name, tbl, stat, errmsg)
        type(db_t),                  intent(in)            :: db
        character(len=*),            intent(in)            :: name
        type(table_t),               intent(out)           :: tbl
        integer,                     intent(out)           :: stat
        character(len=*),            intent(inout), optional :: errmsg
        integer :: u, ios, i
        integer(int32)              :: bom
        character(len=4)            :: magic
        character(len=SQR_NAME_LEN) :: nm
        tbl%name = name
        open(newunit=u, file=schema_path(db, name), access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            if (present(errmsg)) errmsg = 'cannot open schema for ' // trim(name)
            return
        end if
        read(u, iostat=ios) magic
        call io_check(ios)
        if (ios /= 0 .or. magic /= SQR_MAGIC) then
            close(u)
            stat = SQR_ERR
            if (present(errmsg)) errmsg = 'bad magic in schema for ' // trim(name)
            return
        end if
        ! Byte-order mark, before any int field: a wrong-endian schema would
        ! otherwise misread every header scalar below.
        read(u, iostat=ios) bom
        call io_check(ios)
        if (ios /= 0 .or. bom /= SQR_BOM) then
            close(u)
            stat = SQR_VERSION
            if (present(errmsg)) then
                if (bom == SQR_BOM_SWAP) then
                    errmsg = 'schema for ' // trim(name) // &
                             ' was written on a host of the opposite byte order'
                else
                    errmsg = 'bad byte-order mark in schema for ' // trim(name)
                end if
            end if
            return
        end if
        read(u, iostat=ios) tbl%schema_version, tbl%ncols, tbl%record_size, &
                            tbl%next_id, tbl%live_count, tbl%nindices
        call io_check(ios)
        if (ios /= 0) then
            close(u)
            stat = SQR_ERR
            return
        end if
        if (tbl%schema_version /= SQR_SCHEMA_VERSION) then
            close(u)
            stat = SQR_VERSION
            if (present(errmsg)) errmsg = 'unsupported schema version for ' // trim(name)
            return
        end if
        ! The header is untrusted on-disk data: a corrupt count would drive a
        ! bad allocate or later out-of-bounds access. Reject implausible values
        ! before allocating anything. nindices is NOT bounded by ncols: composite
        ! indices and dropped-but-tombstoned slots (db_drop_index keeps the slot
        ! so __i<slot> names stay stable) both push it higher, so bound it with
        ! the same corruption ceiling read_catalog applies to its count.
        if (tbl%ncols       < 1 .or. tbl%ncols       > SQR_MAX_RECORD .or. &
            tbl%record_size < 1 .or. tbl%record_size > SQR_MAX_RECORD .or. &
            tbl%next_id     < 1 .or. tbl%live_count  < 0              .or. &
            tbl%nindices    < 0 .or. tbl%nindices    > SQR_MAX_RECORD) then
            close(u)
            stat = SQR_INVALID
            if (present(errmsg)) errmsg = 'corrupt schema header for ' // trim(name)
            return
        end if
        allocate(tbl%cols(tbl%ncols))
        cols_in: do i = 1, tbl%ncols
            associate (c => tbl%cols(i))
                read(u, iostat=ios) nm
                call io_check(ios)
                c%name = nm
                c%null_bit = i - 1
                if (ios == 0) read(u, iostat=ios) c%dtype, c%csize, c%offset
                call io_check(ios)
                if (ios /= 0) then
                    close(u)
                    stat = SQR_ERR
                    return
                end if
            end associate
        end do cols_in
        ! Validate the column table against itself and against the stored
        ! record_size by re-deriving the fixed layout (status byte + packed
        ! columns, see layout_columns). Any mismatch means the schema is
        ! inconsistent on disk, not merely an unknown version.
        check_cols: block
            integer :: off
            off = 2 + null_bytes(tbl%ncols)
            cols_chk: do i = 1, tbl%ncols
                associate (c => tbl%cols(i))
                    if (c%csize < 1 .or. c%offset /= off) exit cols_chk
                    select case (c%dtype)
                    case (DT_INT, DT_REAL, DT_CHAR)
                    case (DT_TEXT)
                        if (c%csize /= SQR_TEXT_DESC) exit cols_chk
                    case default
                        exit cols_chk
                    end select
                    off = off + c%csize
                end associate
            end do cols_chk
            if (i <= tbl%ncols .or. off - 1 /= tbl%record_size) then
                close(u)
                stat = SQR_INVALID
                if (present(errmsg)) errmsg = 'corrupt column layout for ' // trim(name)
                return
            end if
        end block check_cols
        allocate(tbl%indices(max(1, tbl%nindices)))
        idx_in: do i = 1, tbl%nindices
            associate (ix => tbl%indices(i))
                ! Index record: ncols, member names, key_size, unique flag.
                ! The entry count lives in the index's B+-tree meta page,
                ! not here, and is read back by open_index.
                rec_in: block
                    integer :: nc, m, uflag
                    read(u, iostat=ios) nc
                    call io_check(ios)
                    if (ios /= 0) then
                        close(u)
                        stat = SQR_ERR
                        return
                    end if
                    ! nc == 0 is a tombstoned (dropped) slot — see db_drop_index.
                    ! It carries no member names; key_size/unique are still
                    ! written (0/0) so the record stays fixed-shape.
                    if (nc < 0 .or. nc > tbl%ncols) then
                        close(u)
                        stat = SQR_INVALID
                        if (present(errmsg)) errmsg = 'corrupt index arity in ' // trim(name)
                        return
                    end if
                    ix%ncols = nc
                    allocate(ix%columns(nc), ix%col_idx(nc))
                    members_in: do m = 1, nc
                        read(u, iostat=ios) nm
                        call io_check(ios)
                        if (ios /= 0) then
                            close(u)
                            stat = SQR_ERR
                            return
                        end if
                        ix%columns(m) = nm
                        ix%col_idx(m) = col_index(tbl, nm)
                    end do members_in
                    read(u, iostat=ios) ix%key_size, uflag
                    call io_check(ios)
                    if (ios /= 0) then
                        close(u)
                        stat = SQR_ERR
                        return
                    end if
                    ix%unique = uflag /= 0
                end block rec_in
                if (ix%ncols == 0) cycle idx_in   ! dead slot: nothing to validate
                ! Members must resolve to non-TEXT columns and the per-member
                ! key offsets must pack to exactly key_size; col_index returns
                ! 0 for an absent column (would index tbl%cols(0) later).
                geom_chk: block
                    integer :: m, koff
                    koff = 1
                    allocate(ix%key_off(ix%ncols))   ! nc is scoped to rec_in
                    members_chk: do m = 1, ix%ncols
                        if (ix%col_idx(m) < 1 .or. ix%col_idx(m) > tbl%ncols) &
                            exit members_chk
                        if (tbl%cols(ix%col_idx(m))%dtype == DT_TEXT) exit members_chk
                        ix%key_off(m) = koff
                        koff = koff + tbl%cols(ix%col_idx(m))%csize
                    end do members_chk
                    if (m <= ix%ncols .or. koff - 1 /= ix%key_size) then
                        close(u)
                        stat = SQR_INVALID
                        if (present(errmsg)) errmsg = 'corrupt index geometry in ' // trim(name)
                        return
                    end if
                end block geom_chk
            end associate
        end do idx_in
        close(u)
        stat = SQR_OK
    end subroutine

    ! ===== Data file open =====

    subroutine open_data(db, tbl, mode, stat)
        type(db_t),       intent(in)    :: db
        type(table_t),    intent(inout) :: tbl
        character(len=*), intent(in)    :: mode    ! 'old' or 'new'
        integer,          intent(out)   :: stat
        integer          :: u, ios
        integer(int32)   :: recovered
        integer(int64)   :: fsize
        character(len=9) :: act
        act = 'readwrite'
        if (db%readonly) act = 'read'
        open(newunit=u, file=data_path(db, tbl%name), access='direct', &
             form='unformatted', recl=tbl%record_size, status=mode, &
             action=trim(act), iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        tbl%unit = u
        ! Crash-recovery guard: next_id/live_count are only persisted at
        ! db_close (and create/compact), so a crash after inserts leaves the
        ! schema's next_id stale. The .dat file size is the true high-water
        ! record count, so recover next_id from it on open. Without this a
        ! reopened crash-window row is rejected by db_get yet found by an
        ! index, and the next insert would reuse the stale id and overwrite a
        ! live row (silent corruption). inquire(size=) is bytes, recl is bytes
        ! (-assume byterecl on ifx; gfortran default), so size/record_size is
        ! the record count. The Phase-2 journal makes this exact; this keeps
        ! it safe now.
        if (mode == 'old') then
            inquire(unit=u, size=fsize)
            if (fsize > 0) then
                ! Compute in int64: a file implying more records than the
                ! int32 id space can address is clamped — the rows beyond
                ! huge(int32)-1 are unreachable and the insert guard then
                ! refuses new rows with SQR_FULL.
                recovered = int(min(fsize / int(tbl%record_size, int64) + 1_int64, &
                                    int(huge(0_int32), int64)), int32)
                if (recovered > tbl%next_id) then
                    ! A crash left both counters stale together. Having moved
                    ! next_id to the true high-water, recount the live rows so
                    ! the schema's live_count is not carried forward wrong: it
                    ! is public state, shown by the shell, written back by
                    ! db_close, and the baseline for later insert/delete.
                    tbl%next_id = recovered
                    call recount_live(u, tbl, ios)
                    if (ios /= 0) then
                        stat = SQR_ERR
                        return
                    end if
                end if
            end if
        end if
        stat = SQR_OK
    end subroutine

    ! Count the live (ROW_ALIVE) records in an open data unit and store the
    ! total in tbl%live_count. Used by open_data's crash-recovery path, where
    ! the persisted count cannot be trusted. ios is non-zero on a read failure.
    subroutine recount_live(u, tbl, ios)
        integer,       intent(in)    :: u
        type(table_t), intent(inout) :: tbl
        integer,       intent(out)   :: ios
        integer(int32) :: rid, live
        character(len=:), allocatable :: rbuf
        allocate(character(len=tbl%record_size) :: rbuf)
        live = 0
        scan: do rid = 1, tbl%next_id - 1
            read(u, rec=rid, iostat=ios) rbuf
            if (ios /= 0) return
            if (row_status(rbuf) == ROW_ALIVE) live = live + 1
        end do scan
        tbl%live_count = live
        ios = 0
    end subroutine

    ! Map a b_tree status onto the sqr return code space.
    pure function sqr_of_bt(b) result(s)
        integer, intent(in) :: b
        integer :: s
        select case (b)
        case (BT_OK)
            s = SQR_OK
        case (BT_VERSION)
            s = SQR_VERSION
        case default                 ! BT_ERR / BT_CORRUPT
            s = SQR_ERR
        end select
    end function

    ! Open (mode=='old') or truncate-create (any other mode) the index's
    ! B+-tree file. nentries is refreshed from the tree's authoritative
    ! meta count.
    subroutine open_index(db, tbl, ix, slot, mode, stat)
        type(db_t),       intent(in)    :: db
        type(table_t),    intent(in)    :: tbl
        type(index_t),    intent(inout) :: ix
        integer,          intent(in)    :: slot
        character(len=*), intent(in)    :: mode
        integer,          intent(out)   :: stat
        integer :: bs
        call bt_open(ix%bt, index_path(db, tbl%name, slot), ix%key_size, &
                     .not. db%readonly, mode /= 'old', bs)
        stat = sqr_of_bt(bs)
        if (stat == SQR_OK) ix%nentries = int(ix%bt%nentries)
    end subroutine

    ! The B+-tree key order: the index's member-by-member, per-dtype
    ! composite compare (key_cmp_ix), repackaged as a pure comparator over
    ! the contiguous key bytes with the geometry carried in `ctx`.
    pure function bt_key_cmp(a, b, ctx) result(c)
        character(len=*), intent(in) :: a, b
        class(*),         intent(in) :: ctx
        integer :: c, m, lo, hi
        c = 0
        select type (ctx)
        type is (kc_ctx_t)
            members: do m = 1, ctx%nmem
                lo = ctx%koff(m)
                hi = lo + ctx%csz(m) - 1
                c = key_cmp(a(lo:hi), b(lo:hi), ctx%dt(m))
                if (c /= 0) return
            end do members
        end select
    end function

    pure function make_kc_ctx(t, ix) result(c)
        type(table_t), intent(in) :: t
        type(index_t), intent(in) :: ix
        type(kc_ctx_t) :: c
        integer :: m
        c%nmem = ix%ncols
        allocate(c%koff(ix%ncols), c%csz(ix%ncols), c%dt(ix%ncols))
        members: do m = 1, ix%ncols
            c%koff(m) = ix%key_off(m)
            c%csz(m)  = t%cols(ix%col_idx(m))%csize
            c%dt(m)   = t%cols(ix%col_idx(m))%dtype
        end do members
    end function

    ! Get-or-build the cached comparator context: the geometry changes
    ! only on schema evolution / index rebuild (which clear kc_valid), so
    ! every index touch after the first is allocation-free.
    subroutine ensure_kc_ctx(t, ix)
        type(table_t), intent(in)    :: t
        type(index_t), intent(inout) :: ix
        if (ix%kc_valid) return
        ix%kc = make_kc_ctx(t, ix)
        ix%kc_valid = .true.
    end subroutine

    ! Open the per-table blob file for a table that has >=1 DT_TEXT column.
    ! mode is the OPEN status: 'old' for an existing db, 'replace' on create.
    subroutine open_blob(db, tbl, mode, stat)
        type(db_t),       intent(in)    :: db
        type(table_t),    intent(inout) :: tbl
        character(len=*), intent(in)    :: mode
        integer,          intent(out)   :: stat
        integer          :: u, ios
        integer(int64)   :: sz
        character(len=9) :: act
        if (db%readonly) then
            act = 'read'
        else
            act = 'readwrite'
        end if
        open(newunit=u, file=blob_path(db, tbl%name), access='stream', &
             form='unformatted', status=mode, action=trim(act), iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        tbl%blob_unit = u
        if (mode == 'old') then
            inquire(unit=u, size=sz)
            tbl%blob_next = sz + 1_int64
        else
            tbl%blob_next = 1_int64
        end if
        stat = SQR_OK
    end subroutine

    subroutine abandon_open(db)
        type(db_t), intent(inout) :: db
        integer :: i, j
        if (allocated(db%tables)) then
            tables_abandon: do i = 1, size(db%tables)
                associate (t => db%tables(i))
                    if (t%unit /= -1) then
                        close(t%unit)
                        t%unit = -1
                    end if
                    if (t%blob_unit /= -1) then
                        close(t%blob_unit)
                        t%blob_unit = -1
                    end if
                    if (allocated(t%indices)) then
                        idx_abandon: do j = 1, size(t%indices)
                            associate (ix => t%indices(j))
                                ! Abort path: drop the unit without
                                ! flushing meta (the tree may be only
                                ! half-initialised).
                                if (ix%bt%unit /= -1) then
                                    close(ix%bt%unit)
                                    ix%bt%unit = -1
                                end if
                            end associate
                        end do idx_abandon
                    end if
                end associate
            end do tables_abandon
            deallocate(db%tables)
        end if
        if (allocated(db%dir)) deallocate(db%dir)
        ! Drop any advisory lock taken before the open failed.
        call c_lock_release(db%lock_tok)
        db%ntables = 0
        db%opened = .false.
    end subroutine

    ! Guard for write entry points. Returns .true. and sets stat=SQR_READONLY
    ! when the caller should refuse the request.
    function readonly_block(db, stat) result(blocked)
        type(db_t), intent(in)            :: db
        integer,    intent(out), optional :: stat
        logical :: blocked
        blocked = db%readonly
        if (blocked .and. present(stat)) stat = SQR_READONLY
    end function

    ! Guard for structural / whole-table operations (create/drop table,
    ! compact, add/drop column, create/drop index). These mutate the store
    ! through un-journalled file renames, creates and deletes and shift the
    ! table slots the rollback snapshot is indexed by, so they cannot run
    ! inside an explicit transaction: txn_begin snapshots table positions and
    ! counters and a rollback could not undo their on-disk effects, leaving the
    ! handle inconsistent. Returns .true. (and sets stat=SQR_INVALID) when a
    ! transaction is in flight, matching db_set_readonly's "refused while a
    ! transaction is live" contract.
    function txn_block(db, stat) result(blocked)
        type(db_t), intent(in)            :: db
        integer,    intent(out), optional :: stat
        logical :: blocked
        blocked = db%jrnl%active
        if (blocked .and. present(stat)) stat = SQR_INVALID
    end function

    ! ===== Auto-commit brackets =====
    ! A public mutator wraps its work in `ac_begin … <body> … ac_end` so the op
    ! is all-or-nothing: an implicit transaction opens on entry, commits on
    ! success and rolls back on any error.  If a transaction is already in
    ! flight — an explicit db_begin, or an outer mutator — `owns` comes back
    ! .false. and both helpers are no-ops: the owning scope decides the outcome.
    ! On a read-only handle no transaction is opened; the body reports
    ! SQR_READONLY itself (via readonly_block) and ac_end stays a no-op.
    !
    ! The hook txn_begin installs holds a pointer to `db` that the bracket only
    ! dereferences between these two calls (during the body's page writes), so
    ! `db` need not be `target` here — that is required only for the cross-call
    ! explicit-transaction path, which already declares it.
    subroutine ac_begin(db, owns, stat)
        type(db_t), intent(inout) :: db
        logical,    intent(out)   :: owns
        integer,    intent(out)   :: stat
        stat = SQR_OK
        owns = .false.
        if (db%jrnl%active .or. db%readonly) return   ! nested, or read-only: no-op
        call txn_begin(db, stat)
        owns = (stat == SQR_OK)
    end subroutine

    ! Close an implicit transaction.  `stat` carries the body's result in and
    ! the combined result out: on a clean body commit and report the commit
    ! status; on a failed body roll back but keep the body's (more meaningful)
    ! error code.  No-op unless this scope opened the transaction.
    subroutine ac_end(db, owns, stat)
        type(db_t), intent(inout) :: db
        logical,    intent(in)    :: owns
        integer,    intent(inout) :: stat
        integer :: st
        if (.not. owns) return
        if (stat == SQR_OK) then
            call txn_commit(db, st)
            stat = st
        else
            call txn_rollback(db, st)   ! preserve the body's error code
        end if
    end subroutine

    ! Surface a failure: if `stat` is present set it to `code` and return the
    ! message via `errmsg`; otherwise write the message to error_unit. Never
    ! stops the program — a caller without `stat` still sees the message but
    ! its process continues. Mirrors cmdgraph's raise().
    ! `msg` is optional: omit it when a callee (e.g. read_schema) has already
    ! written its own detailed text into `errmsg` — raise then only routes the
    ! code, and falls back to that errmsg for the no-stat stderr path.
    subroutine raise(code, stat, errmsg, msg)
        integer,          intent(in)               :: code
        integer,          intent(out),   optional  :: stat
        character(len=*), intent(inout), optional  :: errmsg
        character(len=*), intent(in),    optional  :: msg
        if (present(stat)) then
            stat = code
            if (present(msg) .and. present(errmsg)) errmsg = msg
            return
        end if
        if (present(msg)) then
            write(error_unit,'(a)') 'sqr: ' // msg
        else if (present(errmsg)) then
            write(error_unit,'(a)') 'sqr: ' // trim(errmsg)
        end if
    end subroutine

    pure function key_cmp(a, b, dtype) result(r)
        character(len=*), intent(in) :: a, b
        integer,          intent(in) :: dtype
        integer :: r
        integer(int32) :: ia, ib
        real(real64)   :: ra, rb
        select case (dtype)
        case (DT_INT)
            ia = transfer(a(1:4), ia)
            ib = transfer(b(1:4), ib)
            if (ia < ib) then
                r = -1
            else if (ia > ib) then
                r = 1
            else
                r = 0
            end if
        case (DT_REAL)
            ra = transfer(a(1:8), ra)
            rb = transfer(b(1:8), rb)
            if (ra < rb) then
                r = -1
            else if (ra > rb) then
                r = 1
            else
                r = 0
            end if
        case default                        ! DT_CHAR — lexicographic on bytes
            if (a < b) then
                r = -1
            else if (a > b) then
                r = 1
            else
                r = 0
            end if
        end select
    end function

    ! Gather an index's member-column bytes out of a row buffer into a
    ! contiguous key buffer (key(1:ix%key_size)), in declared member order.
    pure subroutine extract_key(t, ix, rowbuf, key)
        type(table_t),    intent(in)  :: t
        type(index_t),    intent(in)  :: ix
        character(len=*), intent(in)  :: rowbuf
        character(len=*), intent(out) :: key
        integer :: m, k, vlen
        members: do m = 1, ix%ncols
            associate (c => t%cols(ix%col_idx(m)))
                if (c%dtype == DT_CHAR) then
                    ! Canonicalise trailing blanks out of the key. The stored
                    ! member is NUL-padded (row_set_char); its value runs to the
                    ! first NUL. Every value comparison in the store trims trailing
                    ! blanks (cond_true, db_find_by/range_char), so 'a', 'a ' and
                    ! 'a  ' must yield one identical key — trim, then NUL-pad. Keeps
                    ! the index and the scan in agreement on CHAR identity.
                    associate (mb => rowbuf(c%offset : c%offset + c%csize - 1))
                        k = scan(mb, char(0))
                        if (k == 0) then
                            vlen = len_trim(mb)
                        else
                            vlen = len_trim(mb(1:k-1))
                        end if
                    end associate
                    key(ix%key_off(m) : ix%key_off(m) + c%csize - 1) = repeat(char(0), c%csize)
                    if (vlen > 0) key(ix%key_off(m) : ix%key_off(m) + vlen - 1) = &
                        rowbuf(c%offset : c%offset + vlen - 1)
                else
                    key(ix%key_off(m) : ix%key_off(m) + c%csize - 1) = &
                        rowbuf(c%offset : c%offset + c%csize - 1)
                end if
            end associate
        end do members
    end subroutine

    ! .true. if any DT_REAL member of this index key holds a NaN. A NaN has
    ! no position in the B+-tree's total order — key_cmp returns 0 against
    ! every other value (both ra<rb and ra>rb are false) — so a NaN key would
    ! misroute the tree and never match on lookup. Callers keep such keys out
    ! of the index entirely (reject the row), consistent with the store's
    ! exact-equality stance for reals.
    pure function key_has_nan(t, ix, key) result(bad)
        type(table_t),    intent(in) :: t
        type(index_t),    intent(in) :: ix
        character(len=*), intent(in) :: key
        logical :: bad
        integer :: m, lo
        real(real64) :: rv
        bad = .false.
        members: do m = 1, ix%ncols
            associate (c => t%cols(ix%col_idx(m)))
                if (c%dtype == DT_REAL) then
                    lo = ix%key_off(m)
                    rv = transfer(key(lo:lo + 7), rv)
                    if (ieee_is_nan(rv)) then
                        bad = .true.
                        return
                    end if
                end if
            end associate
        end do members
    end function

    ! .true. if any member column of index ix is NULL in this row buffer. Such
    ! a row is omitted from the index entirely (partial-index / SQL NULL
    ! semantics): it never matches an equality or range lookup, and a unique
    ! index places no constraint on it (multiple NULL-member rows are allowed).
    ! Operates on the full row buffer (which carries the NULL bitmap), not the
    ! extracted key bytes.
    pure function key_has_null(t, ix, rowbuf) result(yes)
        type(table_t),    intent(in) :: t
        type(index_t),    intent(in) :: ix
        character(len=*), intent(in) :: rowbuf
        logical :: yes
        integer :: m
        yes = .false.
        members: do m = 1, ix%ncols
            if (row_is_null(rowbuf, t%cols(ix%col_idx(m)))) then
                yes = .true.
                return
            end if
        end do members
    end function

    ! Composite compare of two key buffers: member by member in declared
    ! order, each with its own dtype via key_cmp; the first non-equal member
    ! decides. A single-member index is byte-identical to the old behaviour.
    pure function key_cmp_ix(t, ix, a, b) result(r)
        type(table_t),    intent(in) :: t
        type(index_t),    intent(in) :: ix
        character(len=*), intent(in) :: a, b
        integer :: r
        r = key_cmp_lead(t, ix, a, b, ix%ncols)
    end function

    ! key_cmp_ix restricted to the first `nmem` members — comparison on a
    ! leading prefix of a composite key (nmem = ix%ncols is the full key).
    pure function key_cmp_lead(t, ix, a, b, nmem) result(r)
        type(table_t),    intent(in) :: t
        type(index_t),    intent(in) :: ix
        character(len=*), intent(in) :: a, b
        integer,          intent(in) :: nmem
        integer :: r, m, lo, hi
        r = 0
        members: do m = 1, nmem
            associate (c => t%cols(ix%col_idx(m)))
                lo = ix%key_off(m)
                hi = lo + c%csize - 1
                r = key_cmp(a(lo:hi), b(lo:hi), c%dtype)
            end associate
            if (r /= 0) return
        end do members
    end function

    ! Truncate index j of table ti and rebuild it from the live rows of
    ! the table's current data file. Shared by db_create_index (first
    ! build) and db_compact (rebuild after renumbering). Two passes (count,
    ! then gather) feed the B+-tree's O(N log N) perfectly-packed bulk
    ! load — no per-row reinsertion. Leaves the index open for the caller.
    subroutine rebuild_index(db, ti, j, stat)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: ti, j
        integer,    intent(out)   :: stat
        integer :: ios, bs, nlive
        integer(int32) :: rid
        character(len=:), allocatable :: rbuf
        character(len=:), allocatable :: keys(:)
        integer(int32),   allocatable :: pays(:)
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            ix%kc_valid = .false.   ! geometry may have changed (add/drop column)
            if (ix%bt%unit /= -1) then
                close(ix%bt%unit)
                ix%bt%unit = -1
            end if
            ! Under an active transaction the truncating bt_open below would wipe
            ! the index file with no pre-image in the journal — and bt_open's
            ! intent(out) clears the page-write hook, so the bulk-load writes go
            ! unrecorded too.  A rollback could then restore the (truncated) data
            ! file while leaving the index rebuilt against rows that no longer
            ! exist.  Capture the whole committed file up front: an EXTEND so a
            ! longer rebuild is truncated back, and a REGION at offset 0 holding
            ! the full original bytes so the content is restored exactly.  The
            ! close above has flushed the tree's buffer, so the on-disk image is
            ! the committed truth.  Outside a txn (db_create_index first build,
            ! db_compact) this is skipped, leaving those callers unchanged.
            if (db%jrnl%active) then
                capture: block
                    integer(int64) :: isz
                    inquire(file=index_path(db, t%name, j), size=isz)
                    call jrnl_log_extend(db, index_relpath(t%name, j), stat)
                    ! Whole file from the 1-based stream start (pos 1, not 0).
                    if (stat == SQR_OK .and. isz > 0) &
                        call jrnl_log_region(db, index_relpath(t%name, j), &
                                             1_int64, isz, stat=stat)
                    if (stat /= SQR_OK) return
                end block capture
            end if
            call bt_open(ix%bt, index_path(db, t%name, j), ix%key_size, &
                         .true., .true., bs)
            stat = sqr_of_bt(bs)
            if (stat /= SQR_OK) return
            allocate(character(len=t%record_size) :: rbuf)
            nlive = 0
            count_rows: do rid = 1, t%next_id - 1
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then
                    stat = SQR_ERR
                    return
                end if
                if (row_status(rbuf) == ROW_ALIVE) nlive = nlive + 1
            end do count_rows
            allocate(character(len=ix%key_size) :: keys(max(1, nlive)))
            allocate(pays(max(1, nlive)))
            nlive = 0
            gather: do rid = 1, t%next_id - 1
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then
                    stat = SQR_ERR
                    return
                end if
                if (row_status(rbuf) /= ROW_ALIVE) cycle gather
                ! A row with any NULL index member is not indexed (partial-index
                ! semantics). keys(:) was sized to the live-row count, so leaving
                ! these out simply uses fewer slots than allocated.
                if (key_has_null(t, ix, rbuf)) cycle gather
                nlive = nlive + 1
                call extract_key(t, ix, rbuf, keys(nlive))
                ! A NaN has no place in the index's total order. On a first
                ! build (db_create_index over existing data) this rejects the
                ! whole index; a compact-time rebuild can never hit it, as
                ! db_insert/db_update already keep NaN keys out of the table.
                if (key_has_nan(t, ix, keys(nlive))) then
                    stat = SQR_INVALID
                    return
                end if
                pays(nlive) = rid
            end do gather
            call ensure_kc_ctx(t, ix)
            call bt_bulk_load(ix%bt, keys(1:nlive), pays(1:nlive), &
                              bt_key_cmp, ix%kc, bs)
            stat = sqr_of_bt(bs)
            if (stat == SQR_OK) ix%nentries = int(ix%bt%nentries)
        end associate
    end subroutine

    ! ===== Text descriptor helpers =====

    ! DT_TEXT in-row descriptor: int64 blob offset || int32 length.
    pure subroutine row_set_text_desc(buf, col, off, length)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        integer(int64),   intent(in)    :: off
        integer(int32),   intent(in)    :: length
        character(len=8) :: a
        character(len=4) :: b
        a = transfer(off, a)
        b = transfer(length, b)
        buf(col%offset     : col%offset + 7)  = a
        buf(col%offset + 8 : col%offset + 11) = b
    end subroutine

    pure subroutine row_get_text_desc(buf, col, off, length)
        character(len=*), intent(in)  :: buf
        type(column_t),   intent(in)  :: col
        integer(int64),   intent(out) :: off
        integer(int32),   intent(out) :: length
        off    = transfer(buf(col%offset     : col%offset + 7),  off)
        length = transfer(buf(col%offset + 8 : col%offset + 11), length)
    end subroutine

    ! ===== Uniqueness check =====

    subroutine unique_violation(db, ti, j, key, exclude_row, viol, stat)
        type(db_t),       intent(inout) :: db
        integer,          intent(in)    :: ti, j
        character(len=*), intent(in)    :: key
        integer(int32),   intent(in)    :: exclude_row
        logical,          intent(out)   :: viol
        integer,          intent(out)   :: stat
        integer :: bs, ios
        integer(int32) :: rid
        logical :: ok
        character(len=:), allocatable :: ckey, rbuf
        type(bt_cursor_t) :: cur
        viol = .false.
        stat = SQR_OK
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            allocate(character(len=ix%key_size) :: ckey)
            allocate(character(len=t%record_size) :: rbuf)
            call ensure_kc_ctx(t, ix)
            call bt_seek(ix%bt, key, bt_key_cmp, ix%kc, cur, bs)
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
                if (rid /= exclude_row) then
                    read(t%unit, rec=rid, iostat=ios) rbuf
                    call io_check(ios)
                    if (ios /= 0) then
                        stat = SQR_ERR
                        exit scan
                    end if
                    if (row_status(rbuf) == ROW_ALIVE) then
                        viol = .true.
                        exit scan
                    end if
                end if
            end do scan
        end associate
    end subroutine


    ! Walk index j in ascending (key,row_id) order; report whether two live
    ! rows share a key. Dead entries are skipped, not treated as run
    ! breakers: the previous key kept for comparison is the last *live* key
    ! seen, so (k,live)(k,dead)(k,live) is still a duplicate. `found` is
    ! in/out: the unique flag goes in, .true. comes back iff a duplicate
    ! live key exists.
    subroutine has_dup_live_keys(db, ti, j, found, stat)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: ti, j
        logical,    intent(inout) :: found
        integer,    intent(out)   :: stat
        integer :: bs, ios
        integer(int32) :: rid
        logical :: ok, have_live
        character(len=:), allocatable :: ckey, pkey, rbuf
        type(bt_cursor_t) :: cur
        found = .false.
        stat  = SQR_OK
        associate (t => db%tables(ti), ix => db%tables(ti)%indices(j))
            allocate(character(len=ix%key_size) :: ckey, pkey)
            allocate(character(len=t%record_size) :: rbuf)
            call bt_first(ix%bt, cur, bs)
            if (bs /= BT_OK) then
                stat = SQR_ERR
                return
            end if
            have_live = .false.
            pairs: do
                call bt_next(ix%bt, cur, ckey, rid, ok, bs)
                if (bs /= BT_OK) then
                    stat = SQR_ERR
                    return
                end if
                if (.not. ok) exit pairs
                read(t%unit, rec=rid, iostat=ios) rbuf
                call io_check(ios)
                if (ios /= 0) then
                    stat = SQR_ERR
                    return
                end if
                if (row_status(rbuf) /= ROW_ALIVE) cycle pairs
                if (have_live) then
                    if (key_cmp_ix(t, ix, pkey, ckey) == 0) then
                        found = .true.
                        return
                    end if
                end if
                pkey      = ckey
                have_live = .true.
            end do pairs
        end associate
    end subroutine
end submodule sqr_base

! sqr_pack — single-file pack/unpack container for a database directory.
!
! Descendant of sqr_base: the path helpers, catalog/schema codecs, lock helper,
! checksum and hot-journal probe it uses all come from the parent by host
! association. A trivial pure-Fortran archive — no zlib — so a database
! directory can be moved or saved as one `.sqr` document and unpacked back to a
! working directory. Touches no I/O, journalling or locking path in the engine:
! pack reads a consistent read-locked snapshot, unpack materialises a fresh dir.
!
! Container layout (version 2):
!   ["SQRP" | int32 version | int32 BOM | int32 nfiles | int32 checksum]
!   nfiles x [int32 namelen | char(namelen) name | int64 size | int64 offset]
!   payload: each file's raw bytes, concatenated in TOC order
! offset is the byte position of a file's bytes within the payload; checksum is
! the engine's rolling checksum over the table of contents followed by the
! payload, so a damaged name, size or offset is detected as damage.  Version 1
! (sqr up to 2.0.0) summed the payload alone; it is still read.  Version 2 is
! refused by 2.0.0 and earlier (SQR_VERSION).

submodule (sqr:sqr_base) sqr_pack
    use :: clib_wrap, only: c_lock_try, c_rmtree, c_remove   ! others host-associated
    implicit none

    character(len=4), parameter :: PACK_MAGIC   = 'SQRP'
    integer(int32),   parameter :: PACK_VERSION = 2_int32
    integer,          parameter :: NAME_CAP     = 4096       ! max archived namelen
    integer,          parameter :: NFILE_CAP    = 1000000    ! sanity bound on nfiles
    ! The smallest TOC entry: namelen, a one-byte name, size and offset.
    integer,          parameter :: MIN_ENTRY    = I32_BYTES + 1 + 2 * I64_BYTES

    ! One archived file: its name relative to the db directory and its bytes.
    type :: pfile_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: bytes
    end type

contains

    ! No handle is exposed here (the snapshot db_t is local), so there is no
    ! sticky state to record: route the code and its canonical text only.
    subroutine pk_report(rs, stat, errmsg, why)
        integer,          intent(in)              :: rs
        integer,          intent(out),   optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=*), intent(in),    optional :: why     ! whole detail text, if any
        if (present(stat)) stat = rs
        if (rs == SQR_OK .or. .not. present(errmsg)) return
        errmsg = sqr_errstr(rs)
        if (present(why)) then
            if (len(why) > 0) errmsg = why
        end if
    end subroutine

    ! Why files of these sizes cannot go into one container, or '' if they
    ! can.  The pack holds each file, and then the whole payload, in one
    ! character string, whose length is a default integer: a size beyond it
    ! would wrap, and the pack would silently drop the rest of the file.
    pure function pack_size_fault(names, sizes) result(why)
        type(pfile_t),  intent(in) :: names(:)
        integer(int64), intent(in) :: sizes(:)
        character(len=:), allocatable :: why
        integer(int64), parameter :: CAP = int(huge(0), int64)
        integer :: k
        why = ''
        k = findloc(sizes > CAP, .true., dim=1)
        if (k > 0) then
            why = '"' // names(k)%name // '" is larger than 2 GiB'
        else if (sum(sizes) > CAP) then        ! each is <= CAP here: no overflow
            why = 'the database is larger than 2 GiB in total'
        end if
    end function

    module subroutine db_pack(dir, file, stat, errmsg)
        character(len=*), intent(in)            :: dir, file
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        type(db_t) :: sdb                     ! only %dir / %lock_tok are used
        character(len=:), allocatable :: why
        integer    :: rs, lerr

        sdb%dir = trim(dir)
        if (.not. file_exists(catalog_path(sdb))) then
            call pk_report(SQR_NOT_FOUND, stat, errmsg)
            return
        end if
        ! Read-lock the snapshot (shared) and refuse a hot journal — the same
        ! consistency a read-only open enforces, without opening the data files
        ! (which would then be connected to units and unreadable via stream).
        call c_lock_try(lock_path(sdb), .false., sdb%lock_tok, lerr)
        if (lerr == 1) then
            call pk_report(SQR_LOCKED, stat, errmsg)
            return
        else if (lerr /= 0) then
            call pk_report(SQR_ERR, stat, errmsg)
            return
        end if
        call pack_snapshot(sdb, file, rs, why)
        call c_lock_release(sdb%lock_tok)       ! snapshot read; drop the shared lock
        if (len(why) > 0) why = 'cannot pack: ' // why
        call pk_report(rs, stat, errmsg, why)
    end subroutine

    module subroutine db_pack_live(db, file, stat, errmsg)
        class(db_t),      intent(inout)           :: db
        character(len=*), intent(in)              :: file
        integer,          intent(out),   optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        type(db_t) :: sdb                     ! snapshot view: only %dir is used
        character(len=:), allocatable :: detail, why
        integer    :: rs, qs
        call clear_last_err(db)
        if (.not. db%opened) then
            call raise_db(db, SQR_INVALID, stat, errmsg, 'database is not open')
            return
        end if
        ! An in-flight transaction has uncommitted base images in the journal:
        ! packing it would capture a state no COMMIT ever produced.  The caller
        ! ends the transaction and retries.
        if (db%jrnl%active) then
            call raise_db(db, SQR_INVALID, stat, errmsg, &
                'cannot pack while a transaction is open')
            return
        end if
        ! Drop the file units for the duration: the pack reads the same files
        ! through fresh stream units, and a file may be connected to only one
        ! unit at a time.  The handle, its catalogue and the exclusive lock all
        ! stay — no other process can slip in while the snapshot is taken.
        call db_quiesce(db, qs)
        if (qs /= SQR_OK) then
            call report(db, qs, stat, errmsg)   ! quiesce's own detail is the useful one
            return
        end if
        sdb%dir = db%dir
        call pack_snapshot(sdb, file, rs, why)
        call db_resume(db, qs)
        ! A failed resume leaves the handle wedged (units = -1) and outranks a
        ! pack error: the archive may be perfectly good, but the caller must
        ! reopen before using this handle again.  Copy the detail out before
        ! raise_db overwrites the buffer it would otherwise be read from.
        if (qs /= SQR_OK) then
            detail = db%last_errmsg(1:db%last_errlen)
            call raise_db(db, qs, stat, errmsg, &
                'packed, but the database could not be reopened: ' // detail)
            return
        end if
        if (rs /= SQR_OK) then
            if (len(why) == 0) why = sqr_errstr(rs)
            call raise_db(db, rs, stat, errmsg, 'pack failed: ' // why)
            return
        end if
        call report(db, SQR_OK, stat, errmsg)
    end subroutine

    ! Read every file of the database directory `sdb%dir` and write them out as
    ! one container at `file`.  The caller guarantees exclusion — either by
    ! holding the shared snapshot lock (offline `db_pack`) or by owning the
    ! exclusive lock of an open, quiesced handle (`db_pack_live`).
    subroutine pack_snapshot(sdb, file, stat, why)
        type(db_t),       intent(inout) :: sdb
        character(len=*), intent(in)    :: file
        integer,          intent(out)   :: stat
        character(len=:), allocatable, intent(out) :: why   ! detail of a refusal, else ''
        type(table_t), allocatable :: tbls(:)
        type(pfile_t), allocatable :: pf(:)
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        character(len=:), allocatable :: payload, toc
        integer(int64), allocatable   :: offs(:), sizes(:)
        integer(int64) :: total
        integer :: n, nf, i, j, k, rs, cks, ios

        rs = SQR_OK
        why = ''
        pack_body: block
            if (jrnl_hot(sdb)) then
                rs = SQR_READONLY               ! needs recovery: reopen read-write first
                exit pack_body
            end if
            call read_catalog(sdb, names, n, rs)
            if (rs /= SQR_OK) exit pack_body

            ! Read every table's schema ONCE into tbls(:), counting files as we
            ! go: the catalog, then each table's schema/data, its blob (only if
            ! it has a TEXT column) and every live index.
            allocate(tbls(n))
            nf = 1
            count_tables: do i = 1, n
                call read_schema(sdb, trim(names(i)), tbls(i), rs)
                if (rs /= SQR_OK) exit pack_body
                nf = nf + 2                                   ! schema + data
                if (table_has_text(tbls(i))) nf = nf + 1
                do j = 1, tbls(i)%nindices
                    if (idx_live(tbls(i)%indices(j))) nf = nf + 1
                end do
            end do count_tables

            ! Build the file-name list from the schemas already in memory.
            allocate(pf(nf))
            k = 0
            k = k + 1; pf(k)%name = CATALOG_FILE
            list_tables: do i = 1, n
                k = k + 1; pf(k)%name = trim(names(i)) // '.schema'
                k = k + 1; pf(k)%name = data_relpath(trim(names(i)))
                if (table_has_text(tbls(i))) then
                    k = k + 1; pf(k)%name = blob_relpath(trim(names(i)))
                end if
                do j = 1, tbls(i)%nindices
                    if (.not. idx_live(tbls(i)%indices(j))) cycle
                    k = k + 1; pf(k)%name = index_relpath(trim(names(i)), j)
                end do
            end do list_tables

            ! Measure before reading anything: a file, or a total, beyond what
            ! one string can hold is refused here rather than cut short below.
            allocate(sizes(nf))
            measure: do k = 1, nf
                sizes(k) = max(0_int64, c_file_size(pathjoin(sdb%dir, pf(k)%name)))
            end do measure
            why = pack_size_fault(pf, sizes)
            if (len(why) > 0) then
                rs = SQR_FULL
                exit pack_body
            end if

            ! Read every file's bytes, sizing the payload from the running
            ! total, then assemble it in one allocation (no O(n^2) concat)
            ! alongside the TOC offsets.
            allocate(offs(nf))
            total = 0_int64
            read_bytes: do k = 1, nf
                call read_file_bytes(pathjoin(sdb%dir, pf(k)%name), pf(k)%bytes, rs)
                if (rs /= SQR_OK) exit pack_body
                offs(k)  = total
                sizes(k) = int(len(pf(k)%bytes), int64)      ! what was read, for the TOC
                total = total + sizes(k)
            end do read_bytes
            allocate(character(len=int(total)) :: payload, stat=ios)
            if (ios /= 0) then
                rs = SQR_ERR
                why = 'not enough memory'
                exit pack_body
            end if
            assemble: do k = 1, nf
                if (len(pf(k)%bytes) > 0) &
                    payload(int(offs(k)) + 1 : int(offs(k)) + len(pf(k)%bytes)) = pf(k)%bytes
                deallocate(pf(k)%bytes)                  ! now held in the payload
            end do assemble
            ! What is summed is exactly what is written: the TOC, then the payload.
            toc = toc_bytes(pf, sizes, offs)
            cks = checksum_fold(checksum(toc), payload)

            call write_container(file, nf, cks, toc, payload, rs)
        end block pack_body
        stat = rs
    end subroutine

    ! The table of contents exactly as it is written: per entry int32 name
    ! length, the name, int64 size, int64 offset, in native byte order.
    pure function toc_bytes(pf, sizes, offs) result(s)
        type(pfile_t),  intent(in) :: pf(:)
        integer(int64), intent(in) :: sizes(:), offs(:)
        character(len=:), allocatable :: s
        character(len=I32_BYTES) :: b4
        character(len=I64_BYTES) :: b8
        integer :: k, n, p, ln
        n = 0
        measure: do k = 1, size(pf)
            n = n + I32_BYTES + len(pf(k)%name) + 2 * I64_BYTES
        end do measure
        allocate(character(len=n) :: s)
        p = 0
        entries: do k = 1, size(pf)
            associate (nm => pf(k)%name)
                ln = len(nm)
                s(p+1 : p+I32_BYTES) = transfer(int(ln, int32), b4)
                p = p + I32_BYTES
                s(p+1 : p+ln) = nm
                p = p + ln
                s(p+1 : p+I64_BYTES) = transfer(sizes(k), b8)
                p = p + I64_BYTES
                s(p+1 : p+I64_BYTES) = transfer(offs(k), b8)
                p = p + I64_BYTES
            end associate
        end do entries
    end function

    ! Write the container to `file` atomically: build a temp sibling, fsync it,
    ! rename over the target, then fsync the target's directory.  Every failure
    ! goes through the one tail, which removes the temporary file (a no-op once
    ! the rename has happened), so a failed pack leaves the target as it was.
    subroutine write_container(file, nfiles, cks, toc, payload, stat)
        character(len=*), intent(in)  :: file, toc, payload
        integer,          intent(in)  :: nfiles, cks
        integer,          intent(out) :: stat
        character(len=:), allocatable :: tmp
        integer :: u, ios, ignore
        tmp = file // '.tmp'
        open(newunit=u, file=tmp, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        write_body: block
            write(u, iostat=ios) PACK_MAGIC, PACK_VERSION, SQR_BOM, int(nfiles, int32), &
                                 int(cks, int32), toc
            if (ios == 0 .and. len(payload) > 0) write(u, iostat=ios) payload
            call io_check(ios)
            if (ios /= 0) then
                close(u, iostat=ignore)
                exit write_body
            end if
            close(u, iostat=ios)
            call io_check(ios)
            if (ios /= 0) exit write_body
            ios = c_fsync_path(tmp)
            call io_check(ios)
            if (ios /= 0) exit write_body
            ios = c_rename(tmp, file)
            call io_check(ios)
            if (ios /= 0) exit write_body
            ios = c_fsync_dir(parent_dir(file))
            call io_check(ios)
            if (ios /= 0) exit write_body
            stat = SQR_OK
            return
        end block write_body
        call remove_file(tmp)
        stat = SQR_ERR
    end subroutine

    module subroutine db_unpack(file, dir, stat, errmsg)
        character(len=*), intent(in)            :: file, dir
        integer,          intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), allocatable :: payload, toc, tmpd, d, why
        type(pfile_t),  allocatable   :: ent(:)          ! only %name is used
        integer(int64), allocatable   :: sizes(:), offs(:)
        integer(int64)   :: total, fsz, hdr, toc_end
        character(len=len(PACK_MAGIC)) :: magic
        integer(int32)   :: ver, bom, nfiles, cks, namelen
        integer          :: u, ios, i, rs, got

        rs = SQR_OK
        why = ''
        if (len_trim(dir) == 0) then
            call pk_report(SQR_INVALID, stat, errmsg)
            return
        end if
        ! One spelling of the target: 'out/' names the same directory as 'out'
        ! (and would otherwise put the temporary directory inside it).
        d = strip_trailing_seps(dir)
        ! c_path_exists (access(2)): inquire on a directory is unreliable
        ! across compilers (ifx reports .false.), and file_exists now goes
        ! the same way for every path (see c_file_size).
        if (c_path_exists(d)) then              ! never overwrite (Save-As semantics)
            call pk_report(SQR_DUP, stat, errmsg)
            return
        end if
        open(newunit=u, file=file, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            call pk_report(SQR_ERR, stat, errmsg)
            return
        end if
        read_body: block
            read(u, iostat=ios) magic, ver, bom, nfiles, cks
            if (ios /= 0 .or. magic /= PACK_MAGIC) then
                rs = SQR_ERR                    ! not a container / truncated header
                exit read_body
            end if
            if (bom /= SQR_BOM .or. ver > PACK_VERSION) then
                rs = SQR_VERSION                ! wrong byte order or a newer format
                exit read_body
            end if
            ! Every size is bounded by the container's real size before
            ! anything is allocated from it.
            inquire(unit=u, size=fsz)
            hdr = int(len(PACK_MAGIC) + 4 * I32_BYTES, int64)   ! magic + ver, bom, nfiles, cks
            if (nfiles < 0 .or. nfiles > NFILE_CAP) then
                rs = SQR_ERR
                exit read_body
            end if
            if (hdr + int(MIN_ENTRY, int64) * int(nfiles, int64) > fsz) then
                rs = SQR_ERR                    ! more entries than the file can hold
                exit read_body
            end if

            ! Table of contents, read once. Names are UNTRUSTED input that
            ! become file paths: bound the length, reject path traversal, and
            ! accept only the names db_pack writes.
            allocate(ent(nfiles), sizes(nfiles), offs(nfiles), stat=ios)
            if (ios /= 0) then
                rs = SQR_ERR
                exit read_body
            end if
            total = 0_int64
            toc_entries: do i = 1, nfiles
                read(u, iostat=ios) namelen
                if (ios /= 0 .or. namelen < 1 .or. namelen > NAME_CAP) then
                    rs = SQR_ERR
                    exit read_body
                end if
                allocate(character(len=namelen) :: ent(i)%name)
                read(u, iostat=ios) ent(i)%name, sizes(i), offs(i)
                if (ios /= 0 .or. sizes(i) < 0 .or. sizes(i) > fsz) then
                    rs = SQR_ERR
                    exit read_body
                end if
                if (.not. safe_arc_name(ent(i)%name)) then
                    rs = SQR_INVALID
                    exit read_body
                end if
                if (.not. arc_name_allowed(ent(i)%name)) then
                    rs = SQR_INVALID
                    why = 'cannot unpack: "' // ent(i)%name // '" is not a database file'
                    exit read_body
                end if
                if (offs(i) /= total) then
                    rs = SQR_ERR                ! non-contiguous / tampered TOC
                    exit read_body
                end if
                total = total + sizes(i)        ! each <= fsz: no overflow
            end do toc_entries
            inquire(unit=u, pos=toc_end)        ! first byte of the payload
            if (total > fsz - (toc_end - 1_int64) .or. total > int(huge(0), int64)) then
                rs = SQR_ERR                    ! more bytes claimed than are there
                exit read_body
            end if

            allocate(character(len=int(total)) :: payload, stat=ios)
            if (ios /= 0) then
                rs = SQR_ERR
                exit read_body
            end if
            if (total > 0_int64) read(u, pos=toc_end, iostat=ios) payload
            if (ios /= 0) then
                rs = SQR_ERR
                exit read_body
            end if
            if (ver >= 2_int32) then            ! the TOC is summed too
                allocate(character(len=int(toc_end - 1_int64 - hdr)) :: toc, stat=ios)
                if (ios == 0 .and. len(toc) > 0) read(u, pos=hdr + 1_int64, iostat=ios) toc
                if (ios /= 0) then
                    rs = SQR_ERR
                    exit read_body
                end if
                got = checksum_fold(checksum(toc), payload)
            else
                got = checksum(payload)
            end if
            if (got /= cks) then
                rs = SQR_ERR                    ! truncated or corrupt transfer
                exit read_body
            end if

            ! Materialise into a temp sibling directory, then rename into place
            ! so a failure leaves no partial database behind.  Everything is
            ! made durable BEFORE the rename - each file as it is written, then
            ! the temp directory's entries - so a power loss cannot leave the
            ! final name over short or empty files.  A repeated name fails its
            ! second open (status='new'), and so the unpack.
            tmpd = d // '.unpack-tmp'
            if (c_path_exists(tmpd)) ios = c_rmtree(tmpd)
            if (.not. mkdir_p(tmpd)) then
                rs = SQR_ERR
                exit read_body
            end if
            extract: do i = 1, nfiles
                call write_file_bytes(pathjoin(tmpd, ent(i)%name), &
                     payload(int(offs(i)) + 1 : int(offs(i) + sizes(i))), rs)
                if (rs /= SQR_OK) then
                    ios = c_rmtree(tmpd)
                    exit read_body
                end if
            end do extract
            ios = c_fsync_dir(tmpd)
            call io_check(ios)
            if (ios == 0) then
                ios = c_rename(tmpd, d)
                call io_check(ios)
            end if
            if (ios /= 0) then
                ios = c_rmtree(tmpd)             ! whichever name it now has:
                ios = c_rmtree(d)                ! `dir` did not exist on entry
                rs = SQR_ERR
                exit read_body
            end if
            ! The rename itself.  If it cannot be made durable the unpack has
            ! failed, and a failed unpack leaves nothing behind.
            ios = c_fsync_dir(parent_dir(d))
            call io_check(ios)
            if (ios /= 0) then
                ios = c_rmtree(d)
                rs = SQR_ERR
            end if
        end block read_body
        close(u, iostat=ios)
        call pk_report(rs, stat, errmsg, why)
    end subroutine

    ! Read a whole file into an allocatable byte string.
    subroutine read_file_bytes(path, bytes, stat)
        character(len=*),              intent(in)  :: path
        character(len=:), allocatable, intent(out) :: bytes
        integer,                       intent(out) :: stat
        integer :: u, ios
        integer(int64) :: sz
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        inquire(unit=u, size=sz)
        if (sz > int(huge(0), int64)) then       ! would wrap; pack_snapshot refuses earlier
            close(u)
            stat = SQR_FULL
            return
        end if
        allocate(character(len=int(sz)) :: bytes, stat=ios)
        if (ios /= 0) then
            close(u)
            stat = SQR_ERR
            return
        end if
        if (sz > 0_int64) read(u, iostat=ios) bytes
        close(u)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! Write a byte string to a new file.  The file must not exist: unpack
    ! writes into a fresh directory, so an existing name is a repeated entry.
    subroutine write_file_bytes(path, bytes, stat)
        character(len=*), intent(in)  :: path, bytes
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='new', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (len(bytes) > 0) then
            write(u, iostat=ios) bytes
            call io_check(ios)
        end if
        if (ios /= 0) then
            close(u, iostat=ios)                 ! never strand the unit
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)                     ! the final flush
        call io_check(ios)
        if (ios == 0) then
            ios = c_fsync_path(path)
            call io_check(ios)
        end if
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! An archived name is safe to turn into a path iff it is a bare filename:
    ! non-empty, no directory separators, no parent reference, no NUL.
    pure function safe_arc_name(name) result(ok)
        character(len=*), intent(in) :: name
        logical :: ok
        ok = len_trim(name) > 0                          .and. &
             scan(name, PATH_SEPS) == 0                  .and. &
             index(name, '..') == 0                      .and. &
             index(name, CH_NUL) == 0
    end function

    ! An archived name is accepted only if db_pack could have written it: the
    ! catalog, or a non-empty stem with a schema, data, blob or index suffix.
    ! The journal and the lock are refused whatever their ASCII case (a
    ! planted journal would be replayed by the next open), and so is anything
    ! else, such as a staged `.alter` file.
    pure function arc_name_allowed(name) result(ok)
        character(len=*), intent(in) :: name
        logical :: ok
        character(len=*), parameter :: SUFFIXES(4) = &
            [character(len=7) :: '.schema', '.dat', '.blob', '.idx']
        character(len=:), allocatable :: sfx
        integer :: k, n
        n = len(name)
        ok = .false.
        if (same_text(lower_ascii(name), '_journal.dat') .or. &
            same_text(lower_ascii(name), LOCK_FILE)) return
        if (same_text(name, CATALOG_FILE)) then
            ok = .true.
            return
        end if
        suffixes_loop: do k = 1, size(SUFFIXES)
            sfx = trim(SUFFIXES(k))
            if (n <= len(sfx)) cycle suffixes_loop      ! the stem must be non-empty
            if (name(n - len(sfx) + 1:) == sfx) then
                ok = .true.
                exit suffixes_loop
            end if
        end do suffixes_loop
    end function

    ! Equal as written: '==' would pad the shorter operand with blanks.
    pure logical function same_text(a, b)
        character(len=*), intent(in) :: a, b
        same_text = len(a) == len(b)
        if (same_text) same_text = a == b
    end function

    ! A path without its trailing separators; one made of nothing else keeps
    ! its first character (the root).
    pure function strip_trailing_seps(path) result(d)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: d
        integer :: k
        k = verify(trim(path), PATH_SEPS, back=.true.)
        d = path(1:max(k, 1))
    end function

    ! The directory part of a path ('.' if none, '/' for a root-level path).
    pure function parent_dir(path) result(d)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: d
        integer :: k
        k = scan(path, PATH_SEPS, back=.true.)
        if (k == 0) then
            d = '.'
        else if (k == 1) then
            d = path(1:1)
        else
            d = path(1:k-1)
        end if
    end function

end submodule sqr_pack

! sqr_pack — single-file pack/unpack container for a database directory.
!
! Descendant of sqr_base: the path helpers, catalog/schema codecs, lock helper,
! checksum and hot-journal probe it uses all come from the parent by host
! association. A trivial pure-Fortran archive — no zlib — so a database
! directory can be moved or saved as one `.sqr` document and unpacked back to a
! working directory. Touches no I/O, journalling or locking path in the engine:
! pack reads a consistent read-locked snapshot, unpack materialises a fresh dir.
!
! Container layout:
!   ["SQRP" | int32 version | int32 BOM | int32 nfiles | int32 checksum]
!   nfiles x [int32 namelen | char(namelen) name | int64 size | int64 offset]
!   payload: each file's raw bytes, concatenated in TOC order
! offset is the byte position of a file's bytes within the payload; checksum is
! the engine's rolling checksum over the whole payload (truncation detection).

submodule (sqr:sqr_base) sqr_pack
    use :: clib_wrap, only: c_lock_try, c_rmtree, c_remove   ! others host-associated
    implicit none

    character(len=4), parameter :: PACK_MAGIC   = 'SQRP'
    integer(int32),   parameter :: PACK_VERSION = 1_int32
    integer,          parameter :: NAME_CAP     = 4096       ! max archived namelen
    integer,          parameter :: NFILE_CAP    = 1000000    ! sanity bound on nfiles

    ! One archived file: its name relative to the db directory and its bytes.
    type :: pfile_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: bytes
    end type

contains

    module subroutine db_pack(dir, file, stat)
        character(len=*), intent(in)            :: dir, file
        integer,          intent(out), optional :: stat
        type(db_t)     :: sdb                 ! only %dir / %lock_tok are used
        type(table_t)  :: tbl
        type(pfile_t), allocatable :: pf(:)
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        character(len=:), allocatable :: payload
        integer(int64), allocatable   :: offs(:)
        integer :: n, nf, i, j, k, rs, lerr, cks

        rs = SQR_OK
        sdb%dir = trim(dir)
        if (.not. file_exists(catalog_path(sdb))) then
            if (present(stat)) stat = SQR_NOT_FOUND
            return
        end if
        ! Read-lock the snapshot (shared) and refuse a hot journal — the same
        ! consistency a read-only open enforces, without opening the data files
        ! (which would then be connected to units and unreadable via stream).
        call c_lock_try(lock_path(sdb), .false., sdb%lock_tok, lerr)
        if (lerr == 1) then
            if (present(stat)) stat = SQR_LOCKED
            return
        else if (lerr /= 0) then
            if (present(stat)) stat = SQR_ERR
            return
        end if
        pack_body: block
            if (jrnl_hot(sdb)) then
                rs = SQR_READONLY               ! needs recovery: reopen read-write first
                exit pack_body
            end if
            call read_catalog(sdb, names, n, rs)
            if (rs /= SQR_OK) exit pack_body

            ! File list: the catalog, then each table's schema/data, its blob
            ! (only if it has a TEXT column) and every live index.
            nf = 1
            count_tables: do i = 1, n
                call read_schema(sdb, trim(names(i)), tbl, rs)
                if (rs /= SQR_OK) exit pack_body
                nf = nf + 2                                   ! schema + data
                if (table_has_text(tbl)) nf = nf + 1
                do j = 1, tbl%nindices
                    if (idx_live(tbl%indices(j))) nf = nf + 1
                end do
            end do count_tables

            allocate(pf(nf))
            k = 0
            k = k + 1; pf(k)%name = CATALOG_FILE
            list_tables: do i = 1, n
                call read_schema(sdb, trim(names(i)), tbl, rs)
                if (rs /= SQR_OK) exit pack_body
                k = k + 1; pf(k)%name = trim(names(i)) // '.schema'
                k = k + 1; pf(k)%name = data_relpath(trim(names(i)))
                if (table_has_text(tbl)) then
                    k = k + 1; pf(k)%name = blob_relpath(trim(names(i)))
                end if
                do j = 1, tbl%nindices
                    if (.not. idx_live(tbl%indices(j))) cycle
                    k = k + 1; pf(k)%name = index_relpath(trim(names(i)), j)
                end do
            end do list_tables

            ! Read every file's bytes and assemble the payload + TOC offsets.
            allocate(offs(nf))
            payload = ''
            read_bytes: do k = 1, nf
                call read_file_bytes(pathjoin(sdb%dir, pf(k)%name), pf(k)%bytes, rs)
                if (rs /= SQR_OK) exit pack_body
                offs(k) = int(len(payload), int64)
                payload = payload // pf(k)%bytes
            end do read_bytes
            cks = checksum(payload)

            call write_container(file, pf, offs, cks, rs)
        end block pack_body
        call c_lock_release(sdb%lock_tok)       ! snapshot read; drop the shared lock
        if (present(stat)) stat = rs
    end subroutine

    ! Write the container to `file` atomically: build a temp sibling, fsync it,
    ! rename over the target, then fsync the target's directory.
    subroutine write_container(file, pf, offs, cks, stat)
        character(len=*), intent(in)  :: file
        type(pfile_t),    intent(in)  :: pf(:)
        integer(int64),   intent(in)  :: offs(:)
        integer,          intent(in)  :: cks
        integer,          intent(out) :: stat
        character(len=:), allocatable :: tmp
        integer :: u, ios, k
        tmp = file // '.tmp'
        open(newunit=u, file=tmp, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        write(u, iostat=ios) PACK_MAGIC, PACK_VERSION, SQR_BOM, int(size(pf), int32), &
                             int(cks, int32)
        toc: do k = 1, size(pf)
            if (ios /= 0) exit toc
            write(u, iostat=ios) int(len(pf(k)%name), int32), pf(k)%name, &
                                 int(len(pf(k)%bytes), int64), offs(k)
        end do toc
        bytes: do k = 1, size(pf)
            if (ios /= 0) exit bytes
            if (len(pf(k)%bytes) > 0) write(u, iostat=ios) pf(k)%bytes
        end do bytes
        if (ios /= 0) then
            close(u, status='delete', iostat=ios)
            stat = SQR_ERR
            return
        end if
        close(u, iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (c_fsync_path(tmp) /= 0) then
            stat = SQR_ERR
            return
        end if
        if (c_rename(tmp, file) /= 0) then
            stat = SQR_ERR
            return
        end if
        stat = merge(SQR_ERR, SQR_OK, c_fsync_dir(parent_dir(file)) /= 0)
    end subroutine

    module subroutine db_unpack(file, dir, stat)
        character(len=*), intent(in)            :: file, dir
        integer,          intent(out), optional :: stat
        character(len=:), allocatable :: payload, tmpd, nm
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable   :: sizes(:), offs(:)
        integer(int64)   :: total, pos
        character(len=4) :: magic
        integer(int32)   :: ver, bom, nfiles, cks
        integer          :: u, ios, i, namelen, maxlen, rs

        rs = SQR_OK
        ! c_path_exists (stat), not file_exists (inquire) — inquire on a
        ! directory is unreliable across compilers (ifx reports .false.).
        if (c_path_exists(trim(dir))) then      ! never overwrite (Save-As semantics)
            if (present(stat)) stat = SQR_DUP
            return
        end if
        open(newunit=u, file=file, access='stream', form='unformatted', &
             status='old', action='read', iostat=ios)
        if (ios /= 0) then
            if (present(stat)) stat = SQR_ERR
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
            if (nfiles < 0 .or. nfiles > NFILE_CAP) then
                rs = SQR_ERR
                exit read_body
            end if

            ! Table of contents. Names are UNTRUSTED input that become file
            ! paths, so bound the length and reject path traversal.
            allocate(sizes(nfiles), offs(nfiles))
            maxlen = 1
            first_toc: do i = 1, nfiles
                read(u, iostat=ios) namelen
                if (ios /= 0 .or. namelen < 1 .or. namelen > NAME_CAP) then
                    rs = SQR_ERR
                    exit read_body
                end if
                allocate(character(len=namelen) :: nm)
                read(u, iostat=ios) nm, sizes(i), offs(i)
                if (ios /= 0 .or. sizes(i) < 0) then
                    rs = SQR_ERR
                    exit read_body
                end if
                if (.not. safe_arc_name(nm)) then
                    rs = SQR_INVALID
                    exit read_body
                end if
                maxlen = max(maxlen, namelen)
                deallocate(nm)
            end do first_toc

            ! Re-read the TOC to keep the names (kept in a fixed-width array now
            ! that the maximum length is known) and validate the offsets.
            allocate(character(len=maxlen) :: names(nfiles))
            total = 0_int64
            rewind_toc: block
                integer(int64) :: hdr
                hdr = 4_int64 + 4_int64 + 4_int64 + 4_int64 + 4_int64   ! magic+ver+bom+nfiles+cks
                pos = hdr + 1_int64
                second_toc: do i = 1, nfiles
                    read(u, pos=pos, iostat=ios) namelen
                    pos = pos + 4_int64
                    allocate(character(len=namelen) :: nm)
                    read(u, pos=pos, iostat=ios) nm
                    pos = pos + int(namelen, int64) + 16_int64   ! name + size + offset
                    names(i) = nm
                    deallocate(nm)
                    if (offs(i) /= total) then
                        rs = SQR_ERR             ! non-contiguous / tampered TOC
                        exit read_body
                    end if
                    total = total + sizes(i)
                end do second_toc
            end block rewind_toc
            if (total < 0_int64 .or. total > int(huge(0), int64)) then
                rs = SQR_ERR
                exit read_body
            end if

            allocate(character(len=int(total)) :: payload, stat=ios)
            if (ios /= 0) then
                rs = SQR_ERR
                exit read_body
            end if
            if (total > 0_int64) read(u, pos=pos, iostat=ios) payload
            if (ios /= 0) then
                rs = SQR_ERR
                exit read_body
            end if
            if (checksum(payload) /= cks) then
                rs = SQR_ERR                    ! truncated or corrupt transfer
                exit read_body
            end if

            ! Materialise into a temp sibling directory, then rename into place
            ! so a failure leaves no partial database behind.
            tmpd = trim(dir) // '.unpack-tmp'
            if (c_path_exists(tmpd)) ios = c_rmtree(tmpd)
            if (.not. mkdir_p(tmpd)) then
                rs = SQR_ERR
                exit read_body
            end if
            extract: do i = 1, nfiles
                call write_file_bytes(pathjoin(tmpd, trim(names(i))), &
                     payload(int(offs(i)) + 1 : int(offs(i) + sizes(i))), rs)
                if (rs /= SQR_OK) then
                    ios = c_rmtree(tmpd)
                    exit read_body
                end if
            end do extract
            if (c_rename(tmpd, trim(dir)) /= 0) then
                ios = c_rmtree(tmpd)
                rs = SQR_ERR
                exit read_body
            end if
            ios = c_fsync_dir(parent_dir(trim(dir)))
        end block read_body
        close(u, iostat=ios)
        if (present(stat)) stat = rs
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

    ! Write a byte string to a new file.
    subroutine write_file_bytes(path, bytes, stat)
        character(len=*), intent(in)  :: path, bytes
        integer,          intent(out) :: stat
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            stat = SQR_ERR
            return
        end if
        if (len(bytes) > 0) write(u, iostat=ios) bytes
        if (ios == 0) close(u, iostat=ios)
        stat = merge(SQR_ERR, SQR_OK, ios /= 0)
    end subroutine

    ! An archived name is safe to turn into a path iff it is a bare filename:
    ! non-empty, no directory separators, no parent reference, no NUL.
    pure function safe_arc_name(name) result(ok)
        character(len=*), intent(in) :: name
        logical :: ok
        ok = len_trim(name) > 0                          .and. &
             scan(name, '/' // char(92)) == 0            .and. &  ! '/' or '\'
             index(name, '..') == 0                      .and. &
             index(name, char(0)) == 0
    end function

    ! The directory part of a path ('.' if none, '/' for a root-level path).
    pure function parent_dir(path) result(d)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: d
        integer :: k
        k = scan(path, '/' // char(92), back=.true.)
        if (k == 0) then
            d = '.'
        else if (k == 1) then
            d = path(1:1)
        else
            d = path(1:k-1)
        end if
    end function

end submodule sqr_pack

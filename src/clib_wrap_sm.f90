submodule (clib_wrap) clib_wrap_impl
    implicit none

contains

    ! Copy a Fortran string into a NUL-terminated c_char target buffer
    ! sized exactly len_trim+1.
    pure subroutine to_cstr(s, buf)
        character(len=*),       intent(in)  :: s
        character(kind=c_char), intent(out) :: buf(:)
        integer :: i
        do i = 1, len_trim(s)
            buf(i) = s(i:i)
        end do
        buf(size(buf)) = c_null_char
    end subroutine

    module function c_rename(oldpath, newpath) result(ierr)
        character(len=*), intent(in) :: oldpath, newpath
        integer :: ierr
        character(kind=c_char), target :: o(len_trim(oldpath) + 1)
        character(kind=c_char), target :: n(len_trim(newpath) + 1)
        call to_cstr(oldpath, o)
        call to_cstr(newpath, n)
#ifdef _WIN32
        ! CRT rename() does NOT overwrite an existing target; MoveFileEx with
        ! REPLACE_EXISTING gives POSIX rename's atomic-replace semantics that
        ! db_compact's temp-file swap relies on. BOOL: nonzero = success.
        if (win_movefileexa(c_loc(o), c_loc(n), MOVEFILE_REPLACE_EXISTING) /= 0) then
            ierr = 0
        else
            ierr = 1
        end if
#else
        ierr = int(libc_rename(c_loc(o), c_loc(n)))
#endif
    end function

    module function c_remove(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(libc_remove(c_loc(p)))
    end function

    module function c_mkdir(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        call to_cstr(path, p)
#ifdef _WIN32
        ierr = int(libc_mkdir(c_loc(p)))           ! CRT _mkdir: no mode arg
#else
        ierr = int(libc_mkdir(c_loc(p), MKDIR_MODE))
#endif
    end function

    ! Shared on both platforms: libc_isatty is _isatty on Windows, isatty on
    ! POSIX, with the same signature, so one body serves both. fd 0 = stdin.
    module function c_isatty_stdin() result(yes)
        logical :: yes
        yes = libc_isatty(0_c_int) /= 0_c_int
    end function

    module function c_path_exists(path) result(yes)
        character(len=*), intent(in) :: path
        logical :: yes
        character(kind=c_char), target :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        yes = libc_access(c_loc(p), F_OK) == 0_c_int
    end function

#ifdef _WIN32
    module function c_rmtree(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        if (.not. c_path_exists(path)) then
            ierr = 0          ! nothing to remove
            return
        end if
        ierr = rmtree_win(trim(path))
    end function

    ! Depth-first removal via the Win32 FindFirstFile family (no nftw on
    ! Windows). Recurses into subdirectories, deletes files, then removes the
    ! now-empty directory. ierr is set nonzero if any step fails but the walk
    ! continues so a partial tree is still cleared as far as possible.
    recursive function rmtree_win(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        type(win32_find_data)          :: fd
        integer(c_intptr_t)            :: h
        character(kind=c_char), target :: pat(len_trim(path) + 3)   ! \ * NUL
        character(kind=c_char), target :: cpath(len_trim(path) + 1)
        character(len=:), allocatable  :: child, name
        logical :: more
        integer :: junk

        ierr = 0
        call to_cstr(path // '\*', pat)
        h = win_findfirst(c_loc(pat), fd)
        if (h == INVALID_HANDLE_VALUE) then
            ! No listing — remove the (presumably empty) directory itself.
            call to_cstr(path, cpath)
            if (win_rmdir(c_loc(cpath)) == 0) ierr = 1
            return
        end if

        more = .true.
        scan: do while (more)
            name = cname(fd%cFileName)
            skip_dots: if (name /= '.' .and. name /= '..') then
                child = path // '\' // name
                if (iand(fd%dwFileAttributes, FILE_ATTRIBUTE_DIRECTORY) /= 0) then
                    if (rmtree_win(child) /= 0) ierr = 1
                else
                    rm_file: block
                        character(kind=c_char), target :: cc(len(child) + 1)
                        call to_cstr(child, cc)
                        if (win_delfile(c_loc(cc)) == 0) ierr = 1
                    end block rm_file
                end if
            end if skip_dots
            more = win_findnext(h, fd) /= 0
        end do scan

        junk = win_findclose(h)
        call to_cstr(path, cpath)
        if (win_rmdir(c_loc(cpath)) == 0) ierr = 1
    end function

    ! Extract a NUL-terminated c_char array into a Fortran string.
    pure function cname(buf) result(s)
        character(kind=c_char), intent(in) :: buf(:)
        character(len=:), allocatable :: s
        integer :: i
        s = ''
        scan_name: do i = 1, size(buf)
            if (buf(i) == c_null_char) exit scan_name
            s = s // buf(i)
        end do scan_name
    end function

    module function c_fsync_path(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        integer(c_int) :: fd
        call to_cstr(path, p)
        ! Read/write open so _commit (FlushFileBuffers) has a writable handle.
        fd = libc_open(c_loc(p), O_RDWR)
        if (fd < 0) then
            ierr = 1
            return
        end if
        ierr = int(libc_fsync(fd))
        if (libc_close(fd) /= 0 .and. ierr == 0) ierr = 1
    end function

    module function c_fsync_dir(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        ! NTFS journals directory metadata, so an explicit directory flush is
        ! unnecessary on Windows. `path` is referenced to keep -Wextra quiet.
        ierr = 0
        if (len(path) < 0) ierr = 1   ! never true
    end function

    module function c_truncate(path, length) result(ierr)
        character(len=*),   intent(in) :: path
        integer(c_int64_t), intent(in) :: length
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        integer(c_int) :: fd
        call to_cstr(path, p)
        fd = libc_open(c_loc(p), O_RDWR)
        if (fd < 0) then
            ierr = 1
            return
        end if
        ierr = int(libc_chsize(fd, length))
        if (libc_close(fd) /= 0 .and. ierr == 0) ierr = 1
    end function

    ! Zero an OVERLAPPED so LockFileEx/UnlockFileEx lock from offset 0.
    subroutine zero_overlapped(ov)
        type(overlapped_t), intent(out) :: ov
        ov%internal      = 0_c_intptr_t
        ov%internal_high = 0_c_intptr_t
        ov%offset_low    = 0_c_int32_t
        ov%offset_high   = 0_c_int32_t
        ov%h_event       = 0_c_intptr_t
    end subroutine

    module subroutine c_lock_try(path, exclusive, tok, ierr)
        character(len=*),   intent(in)  :: path
        logical,            intent(in)  :: exclusive
        integer(c_int64_t), intent(out) :: tok
        integer,            intent(out) :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        integer(c_intptr_t) :: h
        integer(c_int32_t)  :: flags
        type(overlapped_t)  :: ov
        integer(c_int)      :: rc
        tok = -1_c_int64_t
        call to_cstr(path, p)
        h = win_createfile(c_loc(p), ior(GENERIC_READ_W, GENERIC_WRITE_W), &
                           FILE_SHARE_RDWR, c_null_ptr, OPEN_ALWAYS_W,      &
                           FILE_ATTR_NORMAL, 0_c_intptr_t)
        if (h == INVALID_HANDLE_VALUE) then
            ierr = 2
            return
        end if
        call zero_overlapped(ov)
        flags = LOCKFILE_FAIL_IMMEDIATELY
        if (exclusive) flags = ior(flags, LOCKFILE_EXCLUSIVE_LOCK)
        ! Lock a single byte at offset 0 — every opener locks the same region.
        if (win_lockfileex(h, flags, 0_c_int32_t, 1_c_int32_t, 0_c_int32_t, ov) /= 0) then
            tok  = int(h, c_int64_t)
            ierr = 0
        else
            rc   = win_closehandle(h)   ! best-effort; we are failing anyway
            ierr = 1                    ! non-blocking lock denied => contention
        end if
    end subroutine

    module subroutine c_lock_release(tok)
        integer(c_int64_t), intent(inout) :: tok
        integer(c_intptr_t) :: h
        type(overlapped_t)  :: ov
        integer(c_int)      :: rc
        if (tok < 0) return
        h = int(tok, c_intptr_t)
        call zero_overlapped(ov)
        rc  = win_unlockfileex(h, 0_c_int32_t, 1_c_int32_t, 0_c_int32_t, ov)
        rc  = win_closehandle(h)        ! closing also drops the lock
        tok = -1_c_int64_t
    end subroutine

    module function c_lock_share(tok) result(ierr)
        integer(c_int64_t), intent(in) :: tok
        integer :: ierr
        integer(c_intptr_t) :: h
        type(overlapped_t)  :: ov
        ! Windows has no atomic convert: unlock the exclusive byte, then take
        ! it shared. A brief window exists between the two — acceptable for a
        ! cooperative single-writer downgrade.
        if (tok < 0) then
            ierr = 1
            return
        end if
        h = int(tok, c_intptr_t)
        call zero_overlapped(ov)
        if (win_unlockfileex(h, 0_c_int32_t, 1_c_int32_t, 0_c_int32_t, ov) == 0) then
            ierr = 1
            return
        end if
        call zero_overlapped(ov)
        if (win_lockfileex(h, LOCKFILE_FAIL_IMMEDIATELY, 0_c_int32_t, &
                           1_c_int32_t, 0_c_int32_t, ov) == 0) then
            ierr = 1
        else
            ierr = 0
        end if
    end function
#else
    module function c_rmtree(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        if (libc_access(c_loc(p), F_OK) /= 0_c_int) then
            ierr = 0          ! nothing to remove
            return
        end if
        ! FTW_DEPTH => a directory is visited only after its contents, so
        ! remove() (which needs empty dirs) always succeeds bottom-up.
        ! FTW_PHYS => do not follow symlinks.
        ierr = int(libc_nftw(c_loc(p), c_funloc(rm_entry), 16_c_int, &
                             ior(FTW_PHYS, FTW_DEPTH)))
    end function

    ! Open `path` read-only, fsync the resulting fd, close it. fsync flushes
    ! the inode's dirty pages regardless of which descriptor opened the file,
    ! so a fresh O_RDONLY fd is sufficient. Used for both file and directory
    ! flushes (a directory fd makes its entry creation/deletion durable).
    function fsync_via_open(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        integer(c_int) :: fd
        call to_cstr(path, p)
        fd = libc_open(c_loc(p), O_RDONLY)
        if (fd < 0) then
            ierr = 1
            return
        end if
        ierr = int(libc_fsync(fd))
        if (libc_close(fd) /= 0 .and. ierr == 0) ierr = 1
    end function

    module function c_fsync_path(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        ierr = fsync_via_open(path)
    end function

    module function c_fsync_dir(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        ierr = fsync_via_open(path)
    end function

    module function c_truncate(path, length) result(ierr)
        character(len=*),   intent(in) :: path
        integer(c_int64_t), intent(in) :: length
        integer :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(libc_truncate(c_loc(p), length))
    end function

    module subroutine c_lock_try(path, exclusive, tok, ierr)
        character(len=*),   intent(in)  :: path
        logical,            intent(in)  :: exclusive
        integer(c_int64_t), intent(out) :: tok
        integer,            intent(out) :: ierr
        character(kind=c_char), target :: p(len_trim(path) + 1)
        integer(c_int) :: fd, op, rc
        tok = -1_c_int64_t
        call to_cstr(path, p)
        fd = libc_open3(c_loc(p), ior(O_RDWR, O_CREAT), LOCK_MODE)
        if (fd < 0) then
            ierr = 2          ! cannot open/create the lock file
            return
        end if
        if (exclusive) then
            op = ior(LOCK_EX, LOCK_NB)
        else
            op = ior(LOCK_SH, LOCK_NB)
        end if
        if (libc_flock(fd, op) == 0) then
            tok  = int(fd, c_int64_t)
            ierr = 0
        else
            ! A non-blocking flock on a freshly-opened fd only fails because
            ! an incompatible lock is already held: treat it as contention.
            rc   = libc_close(fd)
            ierr = 1
        end if
    end subroutine

    module subroutine c_lock_release(tok)
        integer(c_int64_t), intent(inout) :: tok
        integer(c_int) :: fd, rc
        if (tok < 0) return
        fd  = int(tok, c_int)
        rc  = libc_flock(fd, LOCK_UN)   ! explicit unlock; the close also drops it
        rc  = libc_close(fd)
        tok = -1_c_int64_t
    end subroutine

    module function c_lock_share(tok) result(ierr)
        integer(c_int64_t), intent(in) :: tok
        integer :: ierr
        if (tok < 0) then
            ierr = 1
            return
        end if
        ! flock converts an existing lock atomically; non-blocking because a
        ! downgrade by the current holder never has to wait.
        ierr = int(libc_flock(int(tok, c_int), ior(LOCK_SH, LOCK_NB)))
    end function
#endif

end submodule clib_wrap_impl

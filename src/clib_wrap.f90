!! Thin, generic Fortran wrappers around a handful of C library / OS
!! filesystem calls.
!!
!! On POSIX (Linux, macOS, Android) these are `rename(2)`, `remove(3)`,
!! `mkdir(2)`, `access(2)` and `nftw(3)`. On Windows (`_WIN32`) the same
!! five operations map onto the CRT / Win32 API: `_mkdir`, `_access`,
!! `remove`, `MoveFileExA` (so a rename atomically *replaces* the target,
!! matching POSIX `rename`) and a `FindFirstFileA` recursion for the tree
!! removal `nftw` provides on POSIX. The platform split is confined to this
!! file via `#ifdef _WIN32`; every caller sees the identical interface.
!!
!! Nothing here is sqr-specific — it is a standalone OS shim used in
!! place of shelling out via `execute_command_line`, so there is no
!! subprocess and no shell-quoting surface.
!!
!! The raw C interfaces (and, on POSIX, the `nftw` callback — a `bind(c)`
!! callback cannot be a submodule-completed procedure) live in the module;
!! the public wrappers are separate module procedures completed in the
!! submodule.
module clib_wrap
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int32_t,        &
                                           c_int64_t, c_intptr_t, c_ptr,     &
                                           c_funptr, c_null_char, c_loc,     &
                                           c_funloc, c_null_ptr
    implicit none
    private

    public :: c_rename      !! atomic rename/replace (same filesystem)
    public :: c_remove      !! unlink a file or remove an empty directory
    public :: c_mkdir       !! create one directory (mode 0777 & umask)
    public :: c_path_exists !! does the path exist?
    public :: c_rmtree      !! recursively remove a directory tree
    public :: c_fsync_path  !! flush a file's data to stable storage
    public :: c_fsync_dir   !! flush a directory's entries to stable storage
    public :: c_truncate    !! set a file's length (shrink or grow)
    public :: c_lock_try    !! try (non-blocking) to take an advisory lock
    public :: c_lock_release!! release an advisory lock and close its handle
    public :: c_lock_share  !! downgrade an exclusive advisory lock to shared
    public :: c_isatty_stdin!! is standard input a terminal?

    ! access(2)/_access mode for an existence test (F_OK) — 0 on both.
    integer(c_int), parameter :: F_OK = 0_c_int

#ifdef _WIN32
    ! ----- Windows (CRT + Win32 API) -----
    integer(c_int),     parameter :: MOVEFILE_REPLACE_EXISTING = 1_c_int
    integer(c_int32_t), parameter :: FILE_ATTRIBUTE_DIRECTORY  = int(z'10', c_int32_t)
    integer(c_intptr_t),parameter :: INVALID_HANDLE_VALUE      = -1_c_intptr_t
    integer,            parameter :: WIN_MAX_PATH = 260
    integer(c_int),     parameter :: O_RDWR = 2_c_int   ! CRT _O_RDWR

    ! CreateFileA / LockFileEx constants for advisory locking. The bit
    ! pattern of GENERIC_READ/WRITE exceeds the signed range, so the BOZ form
    ! (as used for FILE_ATTRIBUTE_DIRECTORY above) carries the exact bits.
    integer(c_int32_t), parameter :: GENERIC_READ_W   = int(z'80000000', c_int32_t)
    integer(c_int32_t), parameter :: GENERIC_WRITE_W  = int(z'40000000', c_int32_t)
    integer(c_int32_t), parameter :: FILE_SHARE_RDWR  = 3_c_int32_t   ! READ | WRITE
    integer(c_int32_t), parameter :: OPEN_ALWAYS_W    = 4_c_int32_t
    integer(c_int32_t), parameter :: FILE_ATTR_NORMAL = int(z'80', c_int32_t)
    integer(c_int32_t), parameter :: LOCKFILE_FAIL_IMMEDIATELY = 1_c_int32_t
    integer(c_int32_t), parameter :: LOCKFILE_EXCLUSIVE_LOCK   = 2_c_int32_t

    ! OVERLAPPED — only the Offset pair (lock start = 0) is used; the rest is
    ! zeroed. Layout matches Win64: two ULONG_PTR, the Offset/OffsetHigh
    ! union (two DWORDs), then the hEvent HANDLE = 32 bytes.
    type, bind(c) :: overlapped_t
        integer(c_intptr_t) :: internal
        integer(c_intptr_t) :: internal_high
        integer(c_int32_t)  :: offset_low
        integer(c_int32_t)  :: offset_high
        integer(c_intptr_t) :: h_event
    end type

    ! WIN32_FIND_DATAA — all members are 4-byte DWORDs or char arrays, so the
    ! C layout has no internal padding surprises (struct size 320). FILETIME
    ! is modelled as two DWORDs; only dwFileAttributes and cFileName are read.
    type, bind(c) :: win32_find_data
        integer(c_int32_t)     :: dwFileAttributes
        integer(c_int32_t)     :: ftCreationTime(2)
        integer(c_int32_t)     :: ftLastAccessTime(2)
        integer(c_int32_t)     :: ftLastWriteTime(2)
        integer(c_int32_t)     :: nFileSizeHigh
        integer(c_int32_t)     :: nFileSizeLow
        integer(c_int32_t)     :: dwReserved0
        integer(c_int32_t)     :: dwReserved1
        character(kind=c_char) :: cFileName(WIN_MAX_PATH)
        character(kind=c_char) :: cAlternateFileName(14)
    end type

    interface
        ! CRT: _mkdir(const char*) — no mode argument on Windows.
        function libc_mkdir(p) bind(c, name='_mkdir') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int)     :: r
        end function

        ! CRT: _access(const char*, int).
        function libc_access(p, mode) bind(c, name='_access') result(r)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: mode
            integer(c_int)        :: r
        end function

        ! CRT: remove(const char*) — files only on Windows; directories are
        ! handled by the FindFirstFile recursion in c_rmtree.
        function libc_remove(p) bind(c, name='remove') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int)     :: r
        end function

        ! Win32: MoveFileExA(existing, new, flags) -> BOOL (nonzero=success).
        ! REPLACE_EXISTING gives POSIX rename's atomic-overwrite semantics.
        function win_movefileexa(oldp, newp, flags) bind(c, name='MoveFileExA') result(r)
            import :: c_ptr, c_int
            type(c_ptr),    value :: oldp, newp
            integer(c_int), value :: flags
            integer(c_int)        :: r
        end function

        ! Win32: FindFirstFileA(pattern, *find_data) -> HANDLE.
        function win_findfirst(pat, fd) bind(c, name='FindFirstFileA') result(h)
            import :: c_ptr, c_intptr_t, win32_find_data
            type(c_ptr), value           :: pat
            type(win32_find_data)        :: fd
            integer(c_intptr_t)          :: h
        end function

        ! Win32: FindNextFileA(handle, *find_data) -> BOOL.
        function win_findnext(h, fd) bind(c, name='FindNextFileA') result(r)
            import :: c_intptr_t, win32_find_data, c_int
            integer(c_intptr_t), value :: h
            type(win32_find_data)      :: fd
            integer(c_int)             :: r
        end function

        ! Win32: FindClose(handle) -> BOOL.
        function win_findclose(h) bind(c, name='FindClose') result(r)
            import :: c_intptr_t, c_int
            integer(c_intptr_t), value :: h
            integer(c_int)             :: r
        end function

        ! Win32: DeleteFileA(path) -> BOOL.
        function win_delfile(p) bind(c, name='DeleteFileA') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int)     :: r
        end function

        ! Win32: RemoveDirectoryA(path) -> BOOL (the directory must be empty).
        function win_rmdir(p) bind(c, name='RemoveDirectoryA') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int)     :: r
        end function

        ! CRT: _open(const char*, int oflag) -> fd (negative on failure).
        function libc_open(p, oflag) bind(c, name='_open') result(fd)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: oflag
            integer(c_int)        :: fd
        end function

        ! CRT: _commit(int fd) -> 0 on success (FlushFileBuffers under the hood).
        function libc_fsync(fd) bind(c, name='_commit') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function

        ! CRT: _close(int fd) -> 0 on success.
        function libc_close(fd) bind(c, name='_close') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function

        ! CRT: _chsize_s(int fd, __int64 size) -> 0 on success.
        function libc_chsize(fd, length) bind(c, name='_chsize_s') result(r)
            import :: c_int, c_int64_t
            integer(c_int),     value :: fd
            integer(c_int64_t), value :: length
            integer(c_int)            :: r
        end function

        ! Win32: CreateFileA(name, access, share, sec, disp, flags, templ)
        ! -> HANDLE (INVALID_HANDLE_VALUE on failure). OPEN_ALWAYS creates the
        ! lock file if absent. The byte-range lock, not the share mode, gives
        ! the exclusion, so the file is shared read+write.
        function win_createfile(name, access, share, sec, disp, flags, templ) &
                bind(c, name='CreateFileA') result(h)
            import :: c_ptr, c_int32_t, c_intptr_t
            type(c_ptr),         value :: name
            integer(c_int32_t),  value :: access, share, disp, flags
            type(c_ptr),         value :: sec
            integer(c_intptr_t), value :: templ
            integer(c_intptr_t)        :: h
        end function

        ! Win32: LockFileEx(handle, flags, reserved, lenLow, lenHigh, *ovl)
        ! -> BOOL. FAIL_IMMEDIATELY makes it non-blocking; EXCLUSIVE_LOCK
        ! selects a write lock (omitted = shared).
        function win_lockfileex(h, flags, reserved, len_lo, len_hi, ovl) &
                bind(c, name='LockFileEx') result(r)
            import :: c_intptr_t, c_int32_t, c_int, overlapped_t
            integer(c_intptr_t), value :: h
            integer(c_int32_t),  value :: flags, reserved, len_lo, len_hi
            type(overlapped_t)         :: ovl
            integer(c_int)             :: r
        end function

        ! Win32: UnlockFileEx(handle, reserved, lenLow, lenHigh, *ovl) -> BOOL.
        function win_unlockfileex(h, reserved, len_lo, len_hi, ovl) &
                bind(c, name='UnlockFileEx') result(r)
            import :: c_intptr_t, c_int32_t, c_int, overlapped_t
            integer(c_intptr_t), value :: h
            integer(c_int32_t),  value :: reserved, len_lo, len_hi
            type(overlapped_t)         :: ovl
            integer(c_int)             :: r
        end function

        ! Win32: CloseHandle(handle) -> BOOL.
        function win_closehandle(h) bind(c, name='CloseHandle') result(r)
            import :: c_intptr_t, c_int
            integer(c_intptr_t), value :: h
            integer(c_int)             :: r
        end function

        ! CRT: _isatty(int fd) -> nonzero if fd is a character device (tty).
        function libc_isatty(fd) bind(c, name='_isatty') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function
    end interface

#else
    ! ----- POSIX (Linux, macOS, Android) -----
    integer(c_int), parameter :: FTW_PHYS   = 1_c_int
    integer(c_int), parameter :: FTW_DEPTH  = 8_c_int
    integer(c_int), parameter :: MKDIR_MODE = int(o'777', c_int)
    integer(c_int), parameter :: O_RDONLY   = 0_c_int
    ! Lock-file open flags / mode and flock(2) operation bits.
    integer(c_int), parameter :: O_RDWR     = 2_c_int
    integer(c_int), parameter :: O_CREAT    = int(o'100', c_int)  ! 0100
    integer(c_int), parameter :: LOCK_MODE  = int(o'644', c_int)  ! 0644 lock file
    integer(c_int), parameter :: LOCK_SH    = 1_c_int
    integer(c_int), parameter :: LOCK_EX    = 2_c_int
    integer(c_int), parameter :: LOCK_NB    = 4_c_int
    integer(c_int), parameter :: LOCK_UN    = 8_c_int

    ! Raw C / POSIX entry points. const char* is taken as type(c_ptr) so an
    ! nftw-supplied path can be forwarded straight into remove() unchanged.
    interface
        function libc_rename(oldp, newp) bind(c, name='rename') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: oldp, newp
            integer(c_int)     :: r
        end function

        function libc_remove(p) bind(c, name='remove') result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int)     :: r
        end function

        function libc_mkdir(p, mode) bind(c, name='mkdir') result(r)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: mode
            integer(c_int)        :: r
        end function

        function libc_access(p, mode) bind(c, name='access') result(r)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: mode
            integer(c_int)        :: r
        end function

        function libc_nftw(p, fn, fdlimit, flags) bind(c, name='nftw') result(r)
            import :: c_ptr, c_funptr, c_int
            type(c_ptr),    value :: p
            type(c_funptr), value :: fn
            integer(c_int), value :: fdlimit, flags
            integer(c_int)        :: r
        end function

        ! open(const char*, int flags) -> fd (negative on failure). Only the
        ! two-argument form is used (no O_CREAT), so the variadic mode is moot.
        function libc_open(p, flags) bind(c, name='open') result(fd)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: flags
            integer(c_int)        :: fd
        end function

        ! fsync(int fd) -> 0 on success.
        function libc_fsync(fd) bind(c, name='fsync') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function

        ! close(int fd) -> 0 on success.
        function libc_close(fd) bind(c, name='close') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function

        ! truncate(const char*, off_t length) -> 0 on success.
        function libc_truncate(p, length) bind(c, name='truncate') result(r)
            import :: c_ptr, c_int, c_int64_t
            type(c_ptr),        value :: p
            integer(c_int64_t), value :: length
            integer(c_int)            :: r
        end function

        ! open(const char*, int flags, mode_t mode) -> fd. The 3-argument
        ! form, declared separately from libc_open so the mode for O_CREAT is
        ! always supplied when creating the lock file.
        function libc_open3(p, flags, mode) bind(c, name='open') result(fd)
            import :: c_ptr, c_int
            type(c_ptr),    value :: p
            integer(c_int), value :: flags, mode
            integer(c_int)        :: fd
        end function

        ! flock(int fd, int operation) -> 0 on success, -1 on failure. The
        ! lock is held by the open file description, so it is released when
        ! the fd is closed (or the process dies) — exactly the lifetime an
        ! open db_t handle needs.
        function libc_flock(fd, op) bind(c, name='flock') result(r)
            import :: c_int
            integer(c_int), value :: fd, op
            integer(c_int)        :: r
        end function

        ! isatty(int fd) -> 1 if fd refers to a terminal, 0 otherwise.
        function libc_isatty(fd) bind(c, name='isatty') result(r)
            import :: c_int
            integer(c_int), value :: fd
            integer(c_int)        :: r
        end function
    end interface
#endif

    ! Public wrappers — separate module procedures, body in the submodule.
    interface
        !! Atomically rename/replace `oldpath` to `newpath` (same
        !! filesystem).  Returns 0 on success, nonzero on failure.
        module function c_rename(oldpath, newpath) result(ierr)
            character(len=*), intent(in) :: oldpath  !! Existing path
            character(len=*), intent(in) :: newpath  !! Destination path (replaced if it exists)
            integer                      :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Unlink a file or remove an empty directory.  Returns 0 on
        !! success, nonzero on failure.
        module function c_remove(path) result(ierr)
            character(len=*), intent(in) :: path  !! Path to remove
            integer                      :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Create one directory (mode `0777` & umask on POSIX).  Returns 0
        !! on success, nonzero on failure (including "already exists").
        module function c_mkdir(path) result(ierr)
            character(len=*), intent(in) :: path  !! Directory to create
            integer                      :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Test whether `path` exists (`access(F_OK)` / `_access`).
        module function c_path_exists(path) result(yes)
            character(len=*), intent(in) :: path  !! Path to test
            logical                      :: yes  !! `.true.` if the path exists
        end function

        !! Depth-first removal of `path` and everything under it.
        !! Returns 0 if the tree was removed or the path did not exist;
        !! nonzero if a remove failed partway.
        module function c_rmtree(path) result(ierr)
            character(len=*), intent(in) :: path  !! Root of the tree to remove
            integer                      :: ierr  !! 0 on success, nonzero on partial failure
        end function

        !! Flush a file's data to stable storage (`fsync` / `_commit`).
        !! `flush` the owning Fortran unit first so the runtime buffer has
        !! reached the OS before this call drives the OS cache to disk.
        !! Returns 0 on success, nonzero on failure.
        module function c_fsync_path(path) result(ierr)
            character(len=*), intent(in) :: path  !! File to flush
            integer                      :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Flush a directory's entries to stable storage, making a file
        !! creation/deletion within it durable.  A no-op on Windows (NTFS
        !! journals directory metadata).  Returns 0 on success.
        module function c_fsync_dir(path) result(ierr)
            character(len=*), intent(in) :: path  !! Directory to flush
            integer                      :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Set a file's length to `length` bytes, shrinking or growing it
        !! (`truncate` / `_chsize_s`).  Returns 0 on success, nonzero on
        !! failure.
        module function c_truncate(path, length) result(ierr)
            character(len=*),   intent(in) :: path    !! File to resize
            integer(c_int64_t), intent(in) :: length  !! New length in bytes
            integer                        :: ierr  !! 0 on success, nonzero on failure
        end function

        !! Try (non-blocking) to take an advisory lock on `path`, which is
        !! created if absent.  `exclusive` selects a write lock; otherwise a
        !! shared (read) lock that coexists with other shared holders.  On
        !! success `ierr` is 0 and `tok` holds the lock (an opaque token —
        !! a POSIX fd or a Win32 HANDLE — pass it to `c_lock_release`).  On
        !! contention `ierr` is 1 and `tok` is -1; on a genuine open/create
        !! failure `ierr` is 2.  The lock lives only as long as `tok` is
        !! held, and is dropped automatically if the process dies.
        module subroutine c_lock_try(path, exclusive, tok, ierr)
            character(len=*),   intent(in)  :: path        !! Lock-file path
            logical,            intent(in)  :: exclusive   !! Write lock if `.true.`
            integer(c_int64_t), intent(out) :: tok         !! Held token, or -1
            integer,            intent(out) :: ierr        !! 0 ok / 1 contended / 2 error
        end subroutine

        !! Release a lock taken by `c_lock_try` and close its descriptor or
        !! handle.  A no-op for an unheld (-1) token; resets `tok` to -1.
        module subroutine c_lock_release(tok)
            integer(c_int64_t), intent(inout) :: tok  !! Token to release (set to -1)
        end subroutine

        !! Downgrade an exclusive lock to shared so other read-only
        !! connections may attach.  Returns 0 on success, nonzero on failure.
        module function c_lock_share(tok) result(ierr)
            integer(c_int64_t), intent(in) :: tok   !! A held lock token
            integer                        :: ierr  !! 0 on success, nonzero on failure
        end function

        !! `.true.` when standard input (fd 0) is a terminal, so an
        !! interactive prompt is appropriate.  Wraps POSIX `isatty` / CRT
        !! `_isatty`.
        module function c_isatty_stdin() result(yes)
            logical :: yes  !! `.true.` if stdin is a TTY
        end function
    end interface

#ifndef _WIN32
contains

    ! nftw callback (POSIX). Only the path is needed; the stat buffer / FTW
    ! info are ignored. Returning nonzero aborts the walk and is propagated.
    function rm_entry(fpath, sb, typeflag, ftwbuf) bind(c) result(r)
        type(c_ptr),    value :: fpath, sb, ftwbuf
        integer(c_int), value :: typeflag
        integer(c_int)        :: r
        r = libc_remove(fpath)
    end function
#endif

end module clib_wrap

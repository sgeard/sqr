!! Thin, generic Fortran wrappers around a handful of OS filesystem and
!! advisory-locking operations.
!!
!! The platform split (POSIX vs Windows) lives entirely in the companion C
!! file `osshim.c`, behind the C preprocessor's `_WIN32` — which a C compiler
!! always predefines correctly, whereas Fortran preprocessors predefine no
!! platform macro at all. So this Fortran needs no preprocessing, no `-D`
!! flags and no per-compiler coaxing: every caller sees one interface, and the
!! same source builds on every OS and compiler.
!!
!! Nothing here is sqr-specific — it is a standalone OS shim used in place of
!! shelling out via `execute_command_line`, so there is no subprocess and no
!! shell-quoting surface. The private `sqr_os_*` interfaces bind to `osshim.c`;
!! the public `c_*` wrappers (which take ordinary Fortran strings) are module
!! procedures completed in the submodule.
module clib_wrap
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    implicit none
    private

    public :: c_rename      !! atomic rename/replace (same filesystem)
    public :: c_remove      !! unlink a file or remove an empty directory
    public :: c_mkdir       !! create one directory (mode 0777 & umask on POSIX)
    public :: c_path_exists !! does the path exist?
    public :: c_rmtree      !! recursively remove a directory tree
    public :: c_fsync_path  !! flush a file's data to stable storage
    public :: c_fsync_dir   !! flush a directory's entries to stable storage
    public :: c_truncate    !! set a file's length (shrink or grow)
    public :: c_lock_try    !! try (non-blocking) to take an advisory lock
    public :: c_lock_release!! release an advisory lock and close its handle
    public :: c_lock_share  !! downgrade an exclusive advisory lock to shared
    public :: c_isatty_stdin!! is standard input a terminal?

    ! ----- Private bind(c) interfaces to osshim.c -----
    ! Each takes a NUL-terminated c_char path (passed as an assumed-size array
    ! = a C char*). Functions return 0 on success / nonzero on failure unless
    ! noted; the lock token is an opaque 64-bit value (POSIX fd or Win32
    ! HANDLE), -1 when not held.
    interface
        function sqr_os_rename(oldp, newp) bind(c, name='sqr_os_rename') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: oldp(*), newp(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_remove(p) bind(c, name='sqr_os_remove') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_mkdir(p) bind(c, name='sqr_os_mkdir') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        ! Returns 1 if the path exists, 0 otherwise.
        function sqr_os_path_exists(p) bind(c, name='sqr_os_path_exists') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_rmtree(p) bind(c, name='sqr_os_rmtree') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_fsync_path(p) bind(c, name='sqr_os_fsync_path') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_fsync_dir(p) bind(c, name='sqr_os_fsync_dir') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int)                     :: r
        end function

        function sqr_os_truncate(p, length) bind(c, name='sqr_os_truncate') result(r)
            import :: c_char, c_int, c_int64_t
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int64_t),     value      :: length
            integer(c_int)                     :: r
        end function

        ! Returns 0 ok / 1 contended / 2 error; sets tok on success.
        function sqr_os_lock_try(p, exclusive, tok) bind(c, name='sqr_os_lock_try') result(r)
            import :: c_char, c_int, c_int64_t
            character(kind=c_char), intent(in)  :: p(*)
            integer(c_int),         value       :: exclusive
            integer(c_int64_t),     intent(out) :: tok
            integer(c_int)                      :: r
        end function

        subroutine sqr_os_lock_release(tok) bind(c, name='sqr_os_lock_release')
            import :: c_int64_t
            integer(c_int64_t), intent(inout) :: tok
        end subroutine

        function sqr_os_lock_share(tok) bind(c, name='sqr_os_lock_share') result(r)
            import :: c_int, c_int64_t
            integer(c_int64_t), value :: tok
            integer(c_int)            :: r
        end function

        ! Returns 1 if stdin is a terminal, 0 otherwise.
        function sqr_os_isatty_stdin() bind(c, name='sqr_os_isatty_stdin') result(r)
            import :: c_int
            integer(c_int) :: r
        end function
    end interface

    ! ----- Public wrappers — Fortran-string fronts, bodies in the submodule -----
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

end module clib_wrap

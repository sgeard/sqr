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
    public :: c_chmod       !! change a path's permission bits
    public :: c_path_exists !! does the path exist?
    public :: c_rmtree      !! recursively remove a directory tree
    public :: c_fsync_path  !! flush a file's data to stable storage
    public :: c_fsync_dir   !! flush a directory's entries to stable storage
    public :: c_truncate    !! set a file's length (shrink or grow)
    public :: c_file_size   !! a file's length in bytes by stat, -1 if absent
    public :: c_realpath    !! canonical absolute path of an existing path
    public :: c_abspath     !! absolute path, final component need not exist
    public :: c_joincwd     !! absolute path with symlinks and `..` left intact
    public :: c_getcwd      !! the current working directory
    public :: c_getpid      !! this process's id
    public :: c_exit        !! end the process with an exit status, silently
    public :: c_lock_try    !! try (non-blocking) to take an advisory lock
    public :: c_lock_release!! release an advisory lock and close its handle
    public :: c_lock_share  !! downgrade an exclusive advisory lock to shared
    public :: c_isatty_stdin!! is standard input a terminal?
    public :: c_sock_listen !! open a loopback TCP listening socket
    public :: c_sock_port   !! actual bound port of a listening socket
    public :: c_sock_accept !! accept one pending connection
    public :: c_sock_connect!! connect to a numeric IPv4 host:port
    public :: c_sock_poll   !! wait for sockets to become readable
    public :: c_sock_recv   !! receive up to len(buf) bytes
    public :: c_sock_send   !! send all bytes
    public :: c_sock_close  !! close a socket

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

        function sqr_os_chmod(p, mode) bind(c, name='sqr_os_chmod') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in)        :: p(*)
            integer(c_int),         intent(in), value :: mode
            integer(c_int)                            :: r
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

        function sqr_os_file_size(p) bind(c, name='sqr_os_file_size') result(n)
            import :: c_char, c_int64_t
            character(kind=c_char), intent(in) :: p(*)
            integer(c_int64_t)                 :: n
        end function

        subroutine sqr_os_exit(code) bind(c, name='sqr_os_exit')
            import :: c_int
            integer(c_int), value :: code
        end subroutine

        ! Fills buf with the NUL-terminated working directory; 0 ok / 1 fail.
        function sqr_os_getcwd(buf, cap) bind(c, name='sqr_os_getcwd') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int),         value       :: cap
            integer(c_int)                      :: r
        end function

        function sqr_os_getpid() bind(c, name='sqr_os_getpid') result(r)
            import :: c_int
            integer(c_int) :: r
        end function

        ! Fills buf with the NUL-terminated canonical absolute path.
        ! Returns 0 ok / 1 cannot resolve / 2 buffer too small.
        function sqr_os_realpath(p, buf, cap) bind(c, name='sqr_os_realpath') result(r)
            import :: c_char, c_int
            character(kind=c_char), intent(in)  :: p(*)
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int),         value       :: cap
            integer(c_int)                      :: r
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

        ! ----- TCP sockets (sqrd wire service) -----
        ! A socket is an opaque 64-bit token (POSIX fd or Winsock SOCKET),
        ! -1 when there is none.

        function sqr_os_sock_listen(port) bind(c, name='sqr_os_sock_listen') result(s)
            import :: c_int, c_int64_t
            integer(c_int), value :: port
            integer(c_int64_t)    :: s
        end function

        function sqr_os_sock_port(s) bind(c, name='sqr_os_sock_port') result(port)
            import :: c_int, c_int64_t
            integer(c_int64_t), value :: s
            integer(c_int)            :: port
        end function

        function sqr_os_sock_accept(ls) bind(c, name='sqr_os_sock_accept') result(s)
            import :: c_int64_t
            integer(c_int64_t), value :: ls
            integer(c_int64_t)        :: s
        end function

        function sqr_os_sock_connect(host, port) bind(c, name='sqr_os_sock_connect') result(s)
            import :: c_char, c_int, c_int64_t
            character(kind=c_char), intent(in)    :: host(*)
            integer(c_int),         value         :: port
            integer(c_int64_t)                    :: s
        end function

        ! Returns the number of ready sockets (0 on timeout, -1 on error);
        ! ready(i) is nonzero if socks(i) is readable (or errored/hung up).
        function sqr_os_sock_poll(socks, n, timeout_ms, ready) bind(c, name='sqr_os_sock_poll') result(r)
            import :: c_int, c_int64_t
            integer(c_int64_t), intent(in)  :: socks(*)
            integer(c_int),     value       :: n
            integer(c_int),     value       :: timeout_ms
            integer(c_int),     intent(out) :: ready(*)
            integer(c_int)                  :: r
        end function

        ! Returns bytes received (0 = orderly close, -1 = error).
        function sqr_os_sock_recv(s, buf, cap) bind(c, name='sqr_os_sock_recv') result(n)
            import :: c_char, c_int64_t
            integer(c_int64_t),     value       :: s
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int64_t),     value       :: cap
            integer(c_int64_t)                  :: n
        end function

        ! Sends ALL nbytes (looping over short writes); returns nbytes or -1.
        function sqr_os_sock_send(s, buf, nbytes) bind(c, name='sqr_os_sock_send') result(n)
            import :: c_char, c_int64_t
            integer(c_int64_t),     value      :: s
            character(kind=c_char), intent(in) :: buf(*)
            integer(c_int64_t),     value      :: nbytes
            integer(c_int64_t)                 :: n
        end function

        subroutine sqr_os_sock_close(s) bind(c, name='sqr_os_sock_close')
            import :: c_int64_t
            integer(c_int64_t), intent(inout) :: s
        end subroutine
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

        !! Change a path's permission bits (`chmod`; pass an octal literal
        !! such as `int(o'444')`).  On Windows only the read-only attribute
        !! exists: any owner-write bit means writable.  Returns 0 on
        !! success, nonzero on failure.
        module function c_chmod(path, mode) result(ierr)
            character(len=*), intent(in) :: path  !! Path to change
            integer,          intent(in) :: mode  !! POSIX-style permission bits
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

        !! A file's length in bytes from `stat` alone: -1 if the path does not
        !! exist or cannot be examined.  Preferred over `inquire(file=, size=)`
        !! on every engine path: a name-based inquire makes the ifx runtime
        !! resolve the real path of every connected unit, so its cost grows
        !! with the number of open tables.  Reports the on-disk length, as the
        !! inquire did; bytes still buffered in a unit are not counted either way.
        module function c_file_size(path) result(n)
            character(len=*), intent(in) :: path  !! File to measure
            integer(c_int64_t)           :: n     !! Length in bytes, -1 if absent
        end function

        !! Canonical absolute path of `path` — `.`/`..` folded away and,
        !! on POSIX, symbolic links resolved (`realpath` / `_fullpath`).
        !! Every component must exist: the result is a zero-length string
        !! if it cannot be resolved, so callers can test with `len`.  Use
        !! `c_abspath` when the last component may not exist yet.
        module function c_realpath(path) result(abspath)
            character(len=*), intent(in)  :: path  !! Path to canonicalise
            character(len=:), allocatable :: abspath  !! Absolute path, or `''` on failure
        end function

        !! Absolute path of `path` when the final component need not exist —
        !! the case for a file about to be created.  An existing path is
        !! canonicalised exactly as `c_realpath` does; otherwise the parent
        !! directory is canonicalised and the final component appended
        !! verbatim (so an unresolvable parent still yields `''`).
        module function c_abspath(path) result(abspath)
            character(len=*), intent(in)  :: path  !! Path to make absolute
            character(len=:), allocatable :: abspath  !! Absolute path, or `''` on failure
        end function

        !! Make `path` absolute WITHOUT resolving anything: an already-absolute
        !! path is returned unchanged, a relative one is joined to the working
        !! directory.  Symbolic links and `..` components survive verbatim.
        !!
        !! This is the counterpart to `c_realpath`, not a lesser version of
        !! it.  A symlink is often the name that matters — `db/current`
        !! pointing at a dated directory — and resolving it discards the
        !! caller's intent.  Note that `..` CANNOT be folded away here:
        !! `a/link/..` is not `a` when `link` is a symlink, so the components
        !! are left for the kernel to resolve as it always would.
        !! Returns `''` only if the working directory cannot be read.
        module function c_joincwd(path) result(abspath)
            character(len=*), intent(in)  :: path  !! Path to anchor
            character(len=:), allocatable :: abspath  !! Absolute path, unresolved
        end function

        !! The current working directory, or `''` if it cannot be read.
        module function c_getcwd() result(dir)
            character(len=:), allocatable :: dir  !! Working directory
        end function

        !! This process's id — for a daemon advertising itself in a file.
        module function c_getpid() result(pid)
            integer :: pid  !! Process id
        end function

        !! End the process with `code` as its exit status and no message of
        !! any kind — what a command-line tool wants after printing its own
        !! diagnostic, where `stop <n>` would add the compiler's rendering of
        !! the stop code to stderr.  Buffered Fortran output is NOT flushed:
        !! `flush` any unit that matters first.  Never returns.
        module subroutine c_exit(code)
            integer, intent(in) :: code  !! Process exit status
        end subroutine

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

        !! Open a TCP listening socket bound to loopback (127.0.0.1) on
        !! `port` (0 asks the OS for an ephemeral port — read it back with
        !! `c_sock_port`).  Returns the socket token, or -1 on failure.
        module function c_sock_listen(port) result(sock)
            integer, intent(in) :: port  !! Port to bind (0 = ephemeral)
            integer(c_int64_t)  :: sock  !! Listening socket, or -1
        end function

        !! The actual bound port of a listening socket — the useful case is
        !! reading back an ephemeral (port-0) bind.  Returns -1 on failure.
        module function c_sock_port(sock) result(port)
            integer(c_int64_t), intent(in) :: sock  !! Listening socket
            integer                        :: port  !! Bound port, or -1
        end function

        !! Accept one pending connection on a listening socket (blocks;
        !! poll first).  Returns the connection socket, or -1 on failure.
        module function c_sock_accept(listen_sock) result(sock)
            integer(c_int64_t), intent(in) :: listen_sock  !! Listening socket
            integer(c_int64_t)             :: sock  !! Accepted connection, or -1
        end function

        !! Connect to `host`:`port`.  `host` is a numeric IPv4 address
        !! (e.g. `'127.0.0.1'` — no name resolution).  Returns the connected
        !! socket, or -1 on failure.
        module function c_sock_connect(host, port) result(sock)
            character(len=*), intent(in) :: host  !! Numeric IPv4 address
            integer,          intent(in) :: port  !! Port to connect to
            integer(c_int64_t)           :: sock  !! Connected socket, or -1
        end function

        !! Wait up to `timeout_ms` (0 = just check, negative = forever) for
        !! any of `socks` to become readable — a hangup or error also counts
        !! as readable, so the next `c_sock_recv` reports it.  `nready` is
        !! the number of ready sockets (0 on timeout, -1 on error).
        module subroutine c_sock_poll(socks, timeout_ms, ready, nready)
            integer(c_int64_t), intent(in)  :: socks(:)    !! Sockets to watch
            integer,            intent(in)  :: timeout_ms  !! Wait limit in ms
            logical,            intent(out) :: ready(:)    !! Per-socket readable flag
            integer,            intent(out) :: nready      !! Ready count, or -1
        end subroutine

        !! Receive up to `len(buf)` bytes into the front of `buf` (blocks;
        !! poll first).  `nrecv` is the byte count, 0 on orderly close by the
        !! peer, -1 on error.
        module subroutine c_sock_recv(sock, buf, nrecv)
            integer(c_int64_t), intent(in)    :: sock   !! Connected socket
            character(len=*),   intent(inout) :: buf    !! Receive buffer (front-filled)
            integer,            intent(out)   :: nrecv  !! Bytes received, 0 EOF, -1 error
        end subroutine

        !! Send ALL of `bytes` (short writes are looped internally).
        !! Returns 0 on success, nonzero on failure.
        module function c_sock_send(sock, bytes) result(ierr)
            integer(c_int64_t), intent(in) :: sock   !! Connected socket
            character(len=*),   intent(in) :: bytes  !! Bytes to send
            integer                        :: ierr   !! 0 on success, nonzero on failure
        end function

        !! Close a socket.  A no-op for an unheld (-1) token; resets `sock`
        !! to -1.
        module subroutine c_sock_close(sock)
            integer(c_int64_t), intent(inout) :: sock  !! Socket to close (set to -1)
        end subroutine
    end interface

end module clib_wrap

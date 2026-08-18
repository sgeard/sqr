submodule (clib_wrap) clib_wrap_impl
    implicit none

    ! Path separators accepted when splitting a path: POSIX uses '/' only,
    ! Windows accepts either, and a POSIX name containing a backslash is
    ! pathological enough that treating it as a separator costs nothing.
    character(len=*), parameter :: SEPARATORS = '/' // achar(92)

contains

    ! Copy a Fortran string into a NUL-terminated c_char target buffer sized
    ! exactly len_trim+1, ready to pass as a C char*.
    pure subroutine to_cstr(s, buf)
        character(len=*),       intent(in)  :: s
        character(kind=c_char), intent(out) :: buf(:)
        integer :: i
        do i = 1, len_trim(s)
            buf(i) = s(i:i)
        end do
        buf(size(buf)) = c_null_char
    end subroutine

    ! A path that needs no working directory to interpret: POSIX '/...', or
    ! a Windows drive ('c:\...') or UNC ('\\host\share') prefix.
    pure function is_rooted(path) result(yes)
        character(len=*), intent(in) :: path
        logical :: yes
        yes = .false.
        if (len(path) == 0) return
        if (index(SEPARATORS, path(1:1)) > 0) then
            yes = .true.
        else if (len(path) >= 3) then
            yes = path(2:2) == ':' .and. index(SEPARATORS, path(3:3)) > 0
        end if
    end function

    ! The Fortran string held in a NUL-terminated c_char buffer (everything
    ! before the first NUL; the whole buffer if there is none).
    pure function from_cstr(buf) result(s)
        character(kind=c_char), intent(in) :: buf(:)
        character(len=:), allocatable :: s
        integer :: i
        do i = 1, size(buf)
            if (buf(i) == c_null_char) exit
        end do
        allocate(character(len=i - 1) :: s)
        do i = 1, len(s)
            s(i:i) = buf(i)
        end do
    end function

    module function c_rename(oldpath, newpath) result(ierr)
        character(len=*), intent(in) :: oldpath, newpath
        integer :: ierr
        character(kind=c_char) :: o(len_trim(oldpath) + 1)
        character(kind=c_char) :: n(len_trim(newpath) + 1)
        call to_cstr(oldpath, o)
        call to_cstr(newpath, n)
        ierr = int(sqr_os_rename(o, n))
    end function

    module function c_remove(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_remove(p))
    end function

    module function c_mkdir(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_mkdir(p))
    end function

    module function c_chmod(path, mode) result(ierr)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: mode
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_chmod(p, int(mode, c_int)))
    end function

    module function c_path_exists(path) result(yes)
        character(len=*), intent(in) :: path
        logical :: yes
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        yes = sqr_os_path_exists(p) /= 0_c_int
    end function

    module function c_rmtree(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_rmtree(p))
    end function

    module function c_fsync_path(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_fsync_path(p))
    end function

    module function c_fsync_dir(path) result(ierr)
        character(len=*), intent(in) :: path
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_fsync_dir(p))
    end function

    module function c_truncate(path, length) result(ierr)
        character(len=*),   intent(in) :: path
        integer(c_int64_t), intent(in) :: length
        integer :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        call to_cstr(path, p)
        ierr = int(sqr_os_truncate(p, length))
    end function

    module function c_realpath(path) result(abspath)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: abspath
        integer, parameter :: CAP = 4096          ! > PATH_MAX on every target
        character(kind=c_char) :: p(len_trim(path) + 1)
        character(kind=c_char) :: buf(CAP)
        abspath = ''
        if (len_trim(path) == 0) return
        call to_cstr(path, p)
        if (sqr_os_realpath(p, buf, int(CAP, c_int)) /= 0_c_int) return
        abspath = from_cstr(buf)
    end function

    module function c_abspath(path) result(abspath)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: abspath
        character(len=:), allocatable :: parent, leaf
        integer :: e, s
        abspath = c_realpath(path)
        if (len(abspath) > 0) return              ! it exists: already canonical
        e = len_trim(path)
        if (e == 0) return
        ! Split off the final component; a trailing separator means there is
        ! no leaf to append and the path simply does not resolve.
        s = scan(path(1:e), SEPARATORS, back=.true.)
        if (s == e) return
        leaf = path(s + 1:e)
        if (s == 0) then
            parent = '.'                          ! bare name: relative to the cwd
        else if (s == 1) then
            parent = path(1:1)                    ! at the root
        else
            parent = path(1:s - 1)
        end if
        parent = c_realpath(parent)
        if (len(parent) == 0) return              ! unresolvable parent
        if (parent(len(parent):len(parent)) == '/' .or. &
            parent(len(parent):len(parent)) == achar(92)) then
            abspath = parent // leaf
        else
            abspath = parent // '/' // leaf
        end if
    end function

    module function c_getcwd() result(dir)
        character(len=:), allocatable :: dir
        integer, parameter :: CAP = 4096
        character(kind=c_char) :: buf(CAP)
        dir = ''
        if (sqr_os_getcwd(buf, int(CAP, c_int)) /= 0_c_int) return
        dir = from_cstr(buf)
    end function

    module function c_getpid() result(pid)
        integer :: pid
        pid = int(sqr_os_getpid())
    end function

    module function c_joincwd(path) result(abspath)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: abspath
        character(len=:), allocatable :: cwd
        integer :: e
        abspath = ''
        e = len_trim(path)
        if (e == 0) return
        if (is_rooted(path(1:e))) then
            abspath = path(1:e)
            return
        end if
        cwd = c_getcwd()
        if (len(cwd) == 0) return
        if (index(SEPARATORS, cwd(len(cwd):len(cwd))) > 0) then
            abspath = cwd // path(1:e)            ! cwd is the root itself
        else
            abspath = cwd // '/' // path(1:e)
        end if
    end function

    module subroutine c_exit(code)
        integer, intent(in) :: code
        call sqr_os_exit(int(code, c_int))
    end subroutine

    module subroutine c_lock_try(path, exclusive, tok, ierr)
        character(len=*),   intent(in)  :: path
        logical,            intent(in)  :: exclusive
        integer(c_int64_t), intent(out) :: tok
        integer,            intent(out) :: ierr
        character(kind=c_char) :: p(len_trim(path) + 1)
        integer(c_int) :: excl
        call to_cstr(path, p)
        excl = 0_c_int
        if (exclusive) excl = 1_c_int
        ierr = int(sqr_os_lock_try(p, excl, tok))
    end subroutine

    module subroutine c_lock_release(tok)
        integer(c_int64_t), intent(inout) :: tok
        call sqr_os_lock_release(tok)
    end subroutine

    module function c_lock_share(tok) result(ierr)
        integer(c_int64_t), intent(in) :: tok
        integer :: ierr
        ierr = int(sqr_os_lock_share(tok))
    end function

    module function c_isatty_stdin() result(yes)
        logical :: yes
        yes = sqr_os_isatty_stdin() /= 0_c_int
    end function

    module function c_sock_listen(port) result(sock)
        integer, intent(in) :: port
        integer(c_int64_t)  :: sock
        sock = sqr_os_sock_listen(int(port, c_int))
    end function

    module function c_sock_port(sock) result(port)
        integer(c_int64_t), intent(in) :: sock
        integer                        :: port
        port = int(sqr_os_sock_port(sock))
    end function

    module function c_sock_accept(listen_sock) result(sock)
        integer(c_int64_t), intent(in) :: listen_sock
        integer(c_int64_t)             :: sock
        sock = sqr_os_sock_accept(listen_sock)
    end function

    module function c_sock_connect(host, port) result(sock)
        character(len=*), intent(in) :: host
        integer,          intent(in) :: port
        integer(c_int64_t)           :: sock
        character(kind=c_char) :: h(len_trim(host) + 1)
        call to_cstr(host, h)
        sock = sqr_os_sock_connect(h, int(port, c_int))
    end function

    module subroutine c_sock_poll(socks, timeout_ms, ready, nready)
        integer(c_int64_t), intent(in)  :: socks(:)
        integer,            intent(in)  :: timeout_ms
        logical,            intent(out) :: ready(:)
        integer,            intent(out) :: nready
        integer(c_int) :: r(size(socks))
        r = 0_c_int
        nready = int(sqr_os_sock_poll(socks, int(size(socks), c_int), &
                                      int(timeout_ms, c_int), r))
        ready = r /= 0_c_int
    end subroutine

    module subroutine c_sock_recv(sock, buf, nrecv)
        integer(c_int64_t), intent(in)    :: sock
        character(len=*),   intent(inout) :: buf
        integer,            intent(out)   :: nrecv
        character(kind=c_char), allocatable :: tmp(:)
        if (len(buf) == 0) then
            nrecv = 0
            return
        end if
        allocate(tmp(len(buf)))
        nrecv = int(sqr_os_sock_recv(sock, tmp, int(len(buf), c_int64_t)))
        if (nrecv > 0) buf(1:nrecv) = transfer(tmp(1:nrecv), buf(1:nrecv))
    end subroutine

    module function c_sock_send(sock, bytes) result(ierr)
        integer(c_int64_t), intent(in) :: sock
        character(len=*),   intent(in) :: bytes
        integer                        :: ierr
        character(kind=c_char), allocatable :: tmp(:)
        ierr = 0
        if (len(bytes) == 0) return
        allocate(tmp(len(bytes)))
        tmp = transfer(bytes, tmp)
        if (sqr_os_sock_send(sock, tmp, int(len(bytes), c_int64_t)) /= len(bytes)) ierr = 1
    end function

    module subroutine c_sock_close(sock)
        integer(c_int64_t), intent(inout) :: sock
        call sqr_os_sock_close(sock)
    end subroutine

end submodule clib_wrap_impl

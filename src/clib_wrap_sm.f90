submodule (clib_wrap) clib_wrap_impl
    implicit none

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

end submodule clib_wrap_impl

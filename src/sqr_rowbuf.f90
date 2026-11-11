! sqr_rowbuf — typed row-buffer accessors for the sqr module.
!
! Descendant of `sqr_base`: the per-row NULL-bitmap helper `null_bit_pos`
! comes from the parent submodule by host association.  These are the public
! `row_*` helpers (interfaces in sqr.f90) that pack and unpack a fixed-size
! record buffer — the status byte, the NULL bitmap, and the typed columns —
! through `transfer`.

submodule (sqr:sqr_base) sqr_rowbuf
    ! int8/int32/real64 reach here by host association from sqr (via sqr_base)
    implicit none
contains

    ! max(0, n): repeat's ncopies must be non-negative, so a negative size
    ! (an arithmetic slip in the caller) would otherwise be a runtime abort
    ! rather than the empty buffer that plainly cannot hold a row.
    pure module subroutine row_alloc(buf, n)
        character(len=:), allocatable, intent(out) :: buf
        integer,                       intent(in)  :: n
        buf = repeat(CH_NUL, max(0, n))
    end subroutine

    pure module subroutine row_clear(buf)
        character(len=*), intent(inout) :: buf
        buf = repeat(CH_NUL, len(buf))
    end subroutine

    pure module function row_status(buf) result(s)
        character(len=*), intent(in) :: buf
        integer(int8) :: s
        s = transfer(buf(ROW_STATUS_OFF:ROW_STATUS_OFF), s)
    end function

    pure module subroutine row_set_status(buf, s)
        character(len=*), intent(inout) :: buf
        integer(int8),    intent(in)    :: s
        character(len=1) :: c
        c = transfer(s, c)
        buf(ROW_STATUS_OFF:ROW_STATUS_OFF) = c
    end subroutine

    pure module subroutine row_set_null(buf, col)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        integer :: bytepos, bit
        integer(int8) :: b
        character(len=1) :: c
        call null_bit_pos(col, bytepos, bit)
        b = transfer(buf(bytepos:bytepos), b)
        b = ibset(b, bit)
        c = transfer(b, c)
        buf(bytepos:bytepos) = c
    end subroutine

    pure module subroutine row_clear_null(buf, col)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        integer :: bytepos, bit
        integer(int8) :: b
        character(len=1) :: c
        call null_bit_pos(col, bytepos, bit)
        b = transfer(buf(bytepos:bytepos), b)
        b = ibclr(b, bit)
        c = transfer(b, c)
        buf(bytepos:bytepos) = c
    end subroutine

    pure module function row_is_null(buf, col) result(isnull)
        character(len=*), intent(in) :: buf
        type(column_t),   intent(in) :: col
        logical :: isnull
        integer :: bytepos, bit
        integer(int8) :: b
        call null_bit_pos(col, bytepos, bit)
        b = transfer(buf(bytepos:bytepos), b)
        isnull = btest(b, bit)
    end function

    pure module subroutine row_set_int(buf, col, val)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        integer(int32),   intent(in)    :: val
        character(len=I32_BYTES) :: c
        c = transfer(val, c)
        buf(col%offset : col%offset + I32_BYTES - 1) = c
        call row_clear_null(buf, col)   ! a stored value is not NULL
    end subroutine

    pure module function row_get_int(buf, col) result(val)
        character(len=*), intent(in) :: buf
        type(column_t),   intent(in) :: col
        integer(int32) :: val
        val = transfer(buf(col%offset : col%offset + I32_BYTES - 1), val)
    end function

    pure module subroutine row_set_real(buf, col, val)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        real(real64),     intent(in)    :: val
        character(len=R64_BYTES) :: c
        c = transfer(val, c)
        buf(col%offset : col%offset + R64_BYTES - 1) = c
        call row_clear_null(buf, col)   ! a stored value is not NULL
    end subroutine

    pure module function row_get_real(buf, col) result(val)
        character(len=*), intent(in) :: buf
        type(column_t),   intent(in) :: col
        real(real64) :: val
        val = transfer(buf(col%offset : col%offset + R64_BYTES - 1), val)
    end function

    pure module subroutine row_set_char(buf, col, val)
        character(len=*), intent(inout) :: buf
        type(column_t),   intent(in)    :: col
        character(len=*), intent(in)    :: val
        integer :: nc
        associate (n => col%csize, off => col%offset)
            nc = min(n, len(val))
            buf(off : off + n - 1)  = repeat(CH_NUL, n)
            if (nc > 0) buf(off : off + nc - 1) = val(1:nc)
        end associate
        call row_clear_null(buf, col)   ! a stored value is not NULL
    end subroutine

    pure module function row_get_char(buf, col) result(val)
        character(len=*), intent(in)  :: buf
        type(column_t),   intent(in)  :: col
        character(len=:), allocatable :: val
        integer :: k
        associate (n => col%csize, off => col%offset)
            k = scan(buf(off : off + n - 1), CH_NUL)
            if (k == 0) then
                val = buf(off : off + n - 1)
            else
                val = buf(off : off + k - 2)
            end if
        end associate
    end function

end submodule sqr_rowbuf

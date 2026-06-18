!! Coverage / fault-test submodule: global Nth-call I/O injection. Lives
!! in `fault/` (not `src/`) so fpm never sees it — only Make's coverage
!! and faulttest path compiles it (with FAULT=on, into a debug ODIR that
!! is separate from the production release archive).
submodule (sqr_fault) sqr_fault_on
    implicit none

    !! Distinctive synthetic iostat. The implementation only ever tests
    !! `ios /= 0` (never the specific value), but a recognisable value
    !! aids debugging if one ever leaks into a message.
    integer, parameter :: FAULT_IOS = -4242

    integer, save :: g_count  = 0    !! io_check calls since arm/disarm
    integer, save :: g_target = -1   !! armed ordinal; <= 0 means disarmed
contains
    module subroutine io_check(ios)
        integer, intent(inout) :: ios
        g_count = g_count + 1
        if (ios /= 0)      return    ! never clobber a real I/O error
        if (g_target <= 0) return    ! disarmed
        if (g_count == g_target) ios = FAULT_IOS
    end subroutine io_check

    module subroutine fault_arm(n)
        integer, intent(in) :: n
        g_target = n
        g_count  = 0
    end subroutine fault_arm

    module subroutine fault_disarm()
        g_target = -1
        g_count  = 0
    end subroutine fault_disarm

    module function fault_count() result(c)
        integer :: c
        c = g_count
    end function fault_count
end submodule sqr_fault_on

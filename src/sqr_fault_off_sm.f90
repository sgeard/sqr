!! Production fault submodule: injection compiled out entirely. Lives in
!! `src/` so both Make (default `FAULT=off`) and fpm build it; the pure
!! storage core therefore ships with no fault state and no overhead.
submodule (sqr_fault) sqr_fault_off
    implicit none
contains
    module subroutine io_check(ios)
        integer, intent(inout) :: ios
    end subroutine io_check

    module subroutine fault_arm(n)
        integer, intent(in) :: n
    end subroutine fault_arm

    module subroutine fault_disarm()
    end subroutine fault_disarm

    module function fault_count() result(c)
        integer :: c
        c = 0
    end function fault_count
end submodule sqr_fault_off

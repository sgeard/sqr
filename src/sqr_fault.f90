!! sqr_fault — fault-injection seam for sqr's low-level I/O failure branches.
!!
!! Every instrumented `read`/`write` in the implementation is followed by
!! `call io_check(ios)`. Two submodule bodies are selected at build time
!! by the Makefile `FAULT` switch:
!!
!!  * `off` (default / production, in `src/`) — `io_check` is an empty
!!    body and the arming entry points are no-ops. No counter, no saved
!!    state: the pure storage core ships with zero fault machinery and
!!    zero overhead, and fpm only ever sees this variant.
!!  * `on`  (coverage / fault test, in `fault/`) — a saved global counter
!!    ticks once per `io_check` call; when the armed ordinal is reached
!!    and the I/O had not already failed, `ios` is forced to a synthetic
!!    error, driving the otherwise-unreachable `iostat` error branches.
!!
!! The injection model is *global Nth-call*: a test sweeps the armed
!! ordinal across a whole high-level operation rather than tagging
!! individual call sites, so the implementation diff is a uniform
!! "one `call io_check(ios)` after each instrumented I/O statement" and
!! every existing `iostat` conditional is left exactly as written.
module sqr_fault
    implicit none
    private
    public :: io_check, fault_arm, fault_disarm, fault_count

    interface
        !! Post-I/O hook. Counts one I/O event. In the `on` build, if the
        !! armed ordinal has been reached and `ios` is still zero, sets
        !! `ios` to a synthetic non-zero error; it never clears a real
        !! error and never alters `ios` in the `off` build. Not `pure`:
        !! the `on` body advances saved state — the seam is inherently
        !! stateful, so the interface cannot promise purity.
        module subroutine io_check(ios)
            integer, intent(inout) :: ios
        end subroutine io_check

        !! Arm injection: the `n`-th [[io_check]] call counted from now
        !! forces a synthetic failure. `n <= 0` disarms. The call ordinal
        !! is reset to zero. No-op in the `off` build.
        module subroutine fault_arm(n)
            integer, intent(in) :: n
        end subroutine fault_arm

        !! Disarm injection and reset the global call ordinal. No-op in
        !! the `off` build.
        module subroutine fault_disarm()
        end subroutine fault_disarm

        !! Number of [[io_check]] calls since the last [[fault_arm]] /
        !! [[fault_disarm]]. Always 0 in the `off` build — a test can use
        !! this both to size an injection sweep and to detect that
        !! injection is unavailable (production library linked).
        module function fault_count() result(c)
            integer :: c
        end function fault_count
    end interface
end module sqr_fault

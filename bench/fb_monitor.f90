!> Progress feedback for long loops.
!!
!! A ruler of dashes is written when the monitor is created, one dot per
!! n_step iterations while the loop runs, and the line is terminated by the
!! finalizer -- so the loop body carries a single `call mon%update` and there
!! is nothing to write after it.  Declare the monitor in a block construct (or
!! any scope that ends with the loop) so finalization closes the line at the
!! right moment.
module fb_monitor
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none

    type fb_monitor_t
        integer :: n_max = 1
        integer :: n_step = 1
        integer :: counter = -1
        character(len=:), allocatable :: dots
    contains
        procedure :: create => create_fb_monitor_t
        procedure :: update => update_fb_monitor_t
        final :: delete_fb_monitor_t
    end type fb_monitor_t

contains

    subroutine create_fb_monitor_t(self, n_max, n_step)
        class(fb_monitor_t), intent(inout) :: self
        integer, intent(in)                :: n_max, n_step
        self%n_max = n_max
        self%n_step = n_step
        self%counter = 0
        write(*,'(a)') repeat('-',n_max/n_step)//'|'
    end subroutine create_fb_monitor_t

    subroutine update_fb_monitor_t(self)
        class(fb_monitor_t), intent(inout) :: self
        if (self%counter == -1) then
            write(output_unit,'(a)') "fb_monitor_t not initialized - call create first"
            return
        end if
        self%counter = self%counter + 1
        if (mod(self%counter-1,self%n_step) == 0) then
            if (self%counter /= (self%n_max - self%n_step+1)) then   ! The last . so newline and flush
                write(output_unit,fmt='(a)',advance='no') '.'; flush(output_unit)
            end if
        end if

    end subroutine update_fb_monitor_t

    subroutine delete_fb_monitor_t(self)
        type(fb_monitor_t) :: self
        write(output_unit,'(a)') '.'
    end subroutine delete_fb_monitor_t

end module fb_monitor

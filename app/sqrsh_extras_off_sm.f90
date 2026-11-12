! Default optional-command set for sqrsh: none. Linked by the ordinary build
! and by fpm; `make sqrsh-regex` links regex/sqrsh_regex.f90 in its place.
submodule (sqrsh_extras) sqrsh_extras_off
    implicit none
contains
    module subroutine add_extra_commands(ui)
        type(engine_t), intent(inout) :: ui
    end subroutine add_extra_commands
end submodule sqrsh_extras_off

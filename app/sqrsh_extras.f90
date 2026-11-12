! Commands a build may add to the shell, chosen at link time by which
! submodule is linked: app/sqrsh_extras_off_sm.f90 adds none (the default,
! and what fpm builds); regex/sqrsh_regex.f90 adds `match <col> <regex>` and
! needs the sibling tcl_re engine (`make sqrsh-regex`). The source is the same
! for every build: no preprocessor conditional selects the feature.
!
! A file of its own, not a module in sqrsh.f90: fpm links every submodule in
! app/ into every program there, and with it the file of its parent module,
! which must therefore not be a program's.
module sqrsh_extras
    use cmdgraph, only: engine_t
    implicit none
    private
    public :: add_extra_commands

    interface
        ! Add the optional commands to the "table" state.
        module subroutine add_extra_commands(ui)
            type(engine_t), intent(inout) :: ui
        end subroutine add_extra_commands
    end interface
end module sqrsh_extras

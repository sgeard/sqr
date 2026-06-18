!! SPDX-License-Identifier: MIT
!! Copyright (c) 2026 Simon Geard
!! Vendored into sqr from https://github.com/sgeard/cmdgraph (fortran/src); kept in sync by hand.
!!
submodule (cmdgraph) cmdgraph_sm
    implicit none

    ! Per-state commands snapshot. Used by finalize_engine to resolve includes
    ! from the originally-parsed graph rather than from already-merged states
    ! (so includes are flat and order-independent, matching the Tcl version).
    type :: cmd_array_t
        type(command_t), allocatable :: items(:)
    end type

contains

    module function version_t_string(this) result(s)
        class(version_t), intent(in)  :: this
        character(len=:), allocatable :: s
        character(len=40)             :: buf
        write(buf, '(i0,".",i0,".",i0)') this%major, this%minor, this%patch
        s = trim(buf)
    end function version_t_string

    ! ===== Construction =====

    module subroutine add_state_engine(this, name, prompt, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: name
        character(len=*), intent(in), optional               :: prompt
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        type(state_t), allocatable                           :: tmp(:)
        integer                                              :: n

        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: add_state: engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        if (.not. allocated(this%states)) allocate(this%states(0))
        if (find_state_idx(this, name) /= 0) then
            call set_build_error(this, "cmdgraph: state '" // name // "' already added")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        n = size(this%states)
        allocate(tmp(n+1))
        tmp(1:n) = this%states
        tmp(n+1)%name = name
        if (present(prompt)) tmp(n+1)%prompt = prompt
        call move_alloc(tmp, this%states)
        if (present(stat)) stat = 0
    end subroutine add_state_engine

    module subroutine add_command_engine(this, state, spec, kind, target, proc, help, args, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: state
        character(len=*), intent(in)                         :: spec
        integer, intent(in)                                  :: kind
        character(len=*), intent(in), optional               :: target
        procedure(action_fun), optional                      :: proc
        character(len=*), intent(in), optional               :: help
        type(arg_spec_t), intent(in), optional               :: args(:)
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer                                              :: sidx
        character(len=32)                                    :: kind_buf
        type(command_t), allocatable                         :: tmp(:)
        type(command_t)                                      :: new_cmd

        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: add_command: engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        sidx = find_state_idx(this, state)
        if (sidx == 0) then
            call set_build_error(this, "cmdgraph: add_command: unknown state '" // state // "'")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if

        new_cmd%spec = spec
        new_cmd%kind = kind
        if (present(help))   new_cmd%help   = help
        if (present(target)) new_cmd%target = target
        if (present(proc))   new_cmd%proc   => proc
        if (present(args))   new_cmd%args   = args

        select case (kind)
        case (EDGE_ACTION)
            if (.not. associated(new_cmd%proc)) then
                call die_missing(this, "action", spec, "proc", stat, errmsg)
                return
            end if
        case (EDGE_GOTO)
            if (.not. allocated(new_cmd%target)) then
                call die_missing(this, "goto", spec, "target", stat, errmsg)
                return
            end if
        case (EDGE_DO_GOTO)
            if (.not. allocated(new_cmd%target)) then
                call die_missing(this, "do_goto", spec, "target", stat, errmsg)
                return
            end if
            if (.not. associated(new_cmd%proc)) then
                call die_missing(this, "do_goto", spec, "proc", stat, errmsg)
                return
            end if
        case (EDGE_DO_POP)
            if (.not. associated(new_cmd%proc)) then
                call die_missing(this, "do_pop", spec, "proc", stat, errmsg)
                return
            end if
        case (EDGE_POP, EDGE_QUIT)
            continue
        case default
            write(kind_buf,'(i0)') kind
            call set_build_error(this, "cmdgraph: unknown edge kind " // trim(kind_buf))
            call raise(this%build_error_msg, stat, errmsg)
            return
        end select

        if (allocated(new_cmd%args)) then
            block
                integer :: rest_pos
                rest_pos = findloc(new_cmd%args%kind, ARG_REST, dim=1)
                if (rest_pos > 0 .and. rest_pos /= size(new_cmd%args)) then
                    call set_build_error(this, "cmdgraph: add_command '" // spec // &
                        "': a rest arg must be the last spec slot")
                    call raise(this%build_error_msg, stat, errmsg)
                    return
                end if
            end block
        end if

        associate(st => this%states(sidx))
            if (.not. allocated(st%commands)) then
                allocate(st%commands(8))
            else if (st%build_count == size(st%commands)) then
                allocate(tmp(size(st%commands) * 2))
                tmp(1:st%build_count) = st%commands(1:st%build_count)
                call move_alloc(tmp, st%commands)
            end if
            st%build_count = st%build_count + 1
            st%commands(st%build_count) = new_cmd
        end associate
        if (present(stat)) stat = 0
    end subroutine add_command_engine

    module subroutine add_include_engine(this, state, included, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: state
        character(len=*), intent(in)                         :: included
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        character(len=:), allocatable                        :: tmp(:)
        integer                                              :: sidx, n, max_len

        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: add_include: engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        sidx = find_state_idx(this, state)
        if (sidx == 0) then
            call set_build_error(this, "cmdgraph: add_include: unknown state '" // state // "'")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if

        if (.not. allocated(this%states(sidx)%includes)) then
            allocate(character(len=len(included)) :: this%states(sidx)%includes(1))
            this%states(sidx)%includes(1) = included
            if (present(stat)) stat = 0
            return
        end if

        n = size(this%states(sidx)%includes)
        max_len = max(len(this%states(sidx)%includes), len(included))
        allocate(character(len=max_len) :: tmp(n+1))
        tmp(1:n) = this%states(sidx)%includes
        tmp(n+1) = included
        call move_alloc(tmp, this%states(sidx)%includes)
        if (present(stat)) stat = 0
    end subroutine add_include_engine

    module subroutine set_on_enter_engine(this, state, proc, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: state
        procedure(on_enter_fun)                              :: proc
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer                                              :: sidx

        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: set_on_enter: engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        sidx = find_state_idx(this, state)
        if (sidx == 0) then
            call set_build_error(this, "cmdgraph: set_on_enter: unknown state '" // state // "'")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        this%states(sidx)%on_enter => proc
        if (present(stat)) stat = 0
    end subroutine set_on_enter_engine

    module subroutine finalize_engine(this, initial, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: initial
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer                                              :: i, j, sidx, tidx, k
        type(command_t), allocatable                         :: merged(:)
        character(len=:), allocatable                        :: cycle_msg
        logical                                              :: has_cycle
        type(cmd_array_t), allocatable                       :: original(:), snapshot(:)
        character(len=:), allocatable                        :: emsg
        logical                                              :: ok

        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        if (.not. allocated(this%states)) then
            call raise("cmdgraph: cannot finalize: no states defined", stat, errmsg)
            return
        end if

        ! Snapshot the per-state command arrays before any mutation. finalize is
        ! atomic: it transforms (trim/parse/merge) then validates, and only
        ! commits on full success. On any validation failure the snapshot is
        ! restored so the engine is byte-identical to before the call and a
        ! corrected retry starts from a clean (untrimmed/unmerged) state.
        allocate(snapshot(size(this%states)))
        do i = 1, size(this%states)
            if (allocated(this%states(i)%commands)) then
                snapshot(i)%items = this%states(i)%commands
            else
                allocate(snapshot(i)%items(0))
            end if
        end do

        ok   = .true.
        emsg = ""
        transform: block
            ! Trim command arrays from build capacity to exact count.
            do i = 1, size(this%states)
                associate(st => this%states(i))
                    if (allocated(st%commands)) then
                        st%commands = st%commands(1:st%build_count)
                    else
                        allocate(st%commands(0))
                    end if
                end associate
            end do

            ! Parse every command's spec into req/opt
            do i = 1, size(this%states)
                do j = 1, size(this%states(i)%commands)
                    call parse_spec(this%states(i)%commands(j)%spec, &
                                    this%states(i)%commands(j)%req, &
                                    this%states(i)%commands(j)%opt)
                end do
            end do

            ! Merge includes: included commands first, then state's own (so
            ! state wins). Snapshot every state's commands first, then resolve
            ! includes from the snapshot — that keeps includes flat (no
            ! transitive inheritance) and order-independent, matching Tcl.
            allocate(original(size(this%states)))
            do i = 1, size(this%states)
                original(i)%items = this%states(i)%commands
            end do
            do i = 1, size(this%states)
                if (.not. allocated(this%states(i)%includes)) cycle
                allocate(merged(0))
                do k = 1, size(this%states(i)%includes)
                    sidx = find_state_idx(this, trim(this%states(i)%includes(k)))
                    if (sidx == 0) then
                        emsg = "cmdgraph: state '" // this%states(i)%name // &
                               "' includes unknown state '" // &
                               trim(this%states(i)%includes(k)) // "'"
                        ok = .false.
                        exit transform
                    end if
                    call merge_commands(merged, original(sidx)%items)
                end do
                call merge_commands(merged, original(i)%items)
                call move_alloc(merged, this%states(i)%commands)
            end do

            ! Validate goto/do_goto targets
            do i = 1, size(this%states)
                do j = 1, size(this%states(i)%commands)
                    if (this%states(i)%commands(j)%kind == EDGE_GOTO .or. &
                        this%states(i)%commands(j)%kind == EDGE_DO_GOTO) then
                        tidx = find_state_idx(this, this%states(i)%commands(j)%target)
                        if (tidx == 0) then
                            emsg = "cmdgraph: state '" // this%states(i)%name // &
                                   "' has '" // this%states(i)%commands(j)%spec // &
                                   "' targeting unknown state '" // &
                                   this%states(i)%commands(j)%target // "'"
                            ok = .false.
                            exit transform
                        end if
                        if (.not. allocated(this%states(tidx)%prompt)) then
                            emsg = "cmdgraph: state '" // this%states(i)%name // &
                                   "' has '" // this%states(i)%commands(j)%spec // &
                                   "' targeting abstract state '" // &
                                   this%states(i)%commands(j)%target // "'"
                            ok = .false.
                            exit transform
                        end if
                    end if
                end do
            end do

            ! Validate DAG-ness of forward edges (goto/do_goto) between concrete
            ! states. pop is the return path; abstract states are command
            ! mix-ins, not nodes.
            call find_cycle(this, has_cycle, cycle_msg)
            if (has_cycle) then
                emsg = cycle_msg
                ok = .false.
                exit transform
            end if

            ! Validate initial state
            sidx = find_state_idx(this, initial)
            if (sidx == 0) then
                emsg = "cmdgraph: initial state '" // initial // "' not in graph"
                ok = .false.
                exit transform
            end if
            if (.not. allocated(this%states(sidx)%prompt)) then
                emsg = "cmdgraph: initial state '" // initial // "' is abstract"
                ok = .false.
                exit transform
            end if
        end block transform

        if (.not. ok) then
            do i = 1, size(this%states)
                this%states(i)%commands = snapshot(i)%items
            end do
            call raise(emsg, stat, errmsg)
            return
        end if

        ! Initialise stack with [initial, ""]
        allocate(this%stack(8))
        this%stack_top = 1
        this%stack(1)%state_idx = sidx
        this%stack(1)%context   = ""
        this%initial_state_idx  = sidx
        this%finalized = .true.

        if (present(stat)) stat = 0
    end subroutine finalize_engine

    ! ===== Execution =====

    module subroutine run_engine(this)
        class(engine_t), intent(inout)         :: this
        character(len=4096)                    :: buffer
        integer                                :: iostat, rc
        character(len=:), allocatable          :: prompt

        do while (this%is_running())
            prompt = this%states(this%stack(this%stack_top)%state_idx)%prompt
            call emit_prompt(this, prompt)
            read(this%input_unit, '(a)', iostat=iostat) buffer
            if (iostat /= 0) exit
            rc = this%dispatch(trim(buffer))
        end do
    end subroutine run_engine

    ! Drive the engine from a script file. Returns ok=.true. iff the file
    ! opened and every dispatched line succeeded (or the script quit cleanly).
    ! On failure ok=.false. and the optional out args report which line and
    ! why: stat = -1 for a file-open failure, otherwise the failing line's
    ! dispatch return code (RC_UNKNOWN/RC_AMBIGUOUS/RC_ERROR); line = the
    ! 1-based file line number (0 for a file-open failure); errmsg = the
    ! diagnostic text. Stops at the first failing line (the engine is left
    ! in whatever state that line produced).
    module function run_file_engine(this, path, echo, stat, errmsg, line) result(ok)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: path
        logical, intent(in), optional                        :: echo
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer, intent(out), optional                       :: line
        logical                                              :: ok
        integer                                :: unit, iostat, rc, lineno
        logical                                :: do_echo
        character(len=4096)                    :: buffer
        character(len=:), allocatable          :: src, trimmed, prompt

        if (present(stat))   stat = 0
        if (present(errmsg)) errmsg = ""
        if (present(line))   line = 0

        do_echo = .true.
        if (present(echo)) do_echo = echo

        open(newunit=unit, file=path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            call set_error(this, "could not open script file: " // path)
            if (present(stat))   stat = -1
            if (present(errmsg)) errmsg = this%last_error
            ok = .false.
            return
        end if

        lineno = 0
        do while (this%is_running())
            read(unit, '(a)', iostat=iostat) buffer
            if (iostat /= 0) exit
            lineno  = lineno + 1
            src     = trim(buffer)
            trimmed = adjustl(src)
            if (len_trim(trimmed) == 0)   cycle
            if (trimmed(1:1) == '#')      cycle
            prompt = this%states(this%stack(this%stack_top)%state_idx)%prompt
            if (do_echo) call emit_info(this, prompt // src)
            rc = this%dispatch(src)
            if (rc == RC_UNKNOWN .or. rc == RC_AMBIGUOUS .or. rc == RC_ERROR) then
                if (present(stat)) stat = rc
                if (present(line)) line = lineno
                if (present(errmsg)) then
                    if (rc == RC_ERROR) then
                        errmsg = this%last_error
                    else
                        errmsg = this%last_message
                    end if
                end if
                close(unit)
                ok = .false.
                return
            end if
        end do

        close(unit)
        ok = .true.
    end function run_file_engine

    ! Return a finalized engine to its initial runtime state without
    ! rebuilding: stack rewound to the initial state, contexts dropped,
    ! last_message/last_error cleared. The graph is untouched (still
    ! finalized and immutable). Misuse on an unfinalized engine is reported
    ! via the optional stat/errmsg (no error stop), like the builder methods.
    module subroutine reset_engine(this, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg

        if (.not. this%finalized) then
            call raise("cmdgraph: reset: engine not finalized", stat, errmsg)
            return
        end if

        this%stack_top = 1
        this%stack(1)%state_idx = this%initial_state_idx
        this%stack(1)%context   = ""
        this%last_message = ""
        this%last_error   = ""
        if (present(stat)) stat = 0
    end subroutine reset_engine

    module function dispatch_engine(this, line) result(rc)
        class(engine_t), intent(inout)         :: this
        character(len=*), intent(in)           :: line
        integer                                :: rc
        character(len=:), allocatable          :: cmd, rest
        integer                                :: n_matches, match_idx
        type(dlist_t)                          :: args

        if (.not. this%is_running()) then
            rc = RC_EXITED
            return
        end if

        call split_first_token(line, cmd, rest)
        if (.not. allocated(cmd) .or. len(cmd) == 0) then
            rc = RC_OK
            return
        end if

        call find_matches(this, cmd, n_matches, match_idx)
        select case (n_matches)
        case (0)
            if (cmd == "help" .or. cmd == "?") then
                call show_help(this)
                rc = RC_OK
            else
                call emit_info(this, "unknown: " // cmd)
                rc = RC_UNKNOWN
            end if
        case (1)
            block
                integer                       :: sidx_cur, rest_idx, n_lead
                logical                       :: ok
                character(len=:), allocatable :: vmsg, tail, lead_src
                sidx_cur = this%stack(this%stack_top)%state_idx
                rest_idx = 0
                if (allocated(this%states(sidx_cur)%commands(match_idx)%args)) then
                    associate (cargs => this%states(sidx_cur)%commands(match_idx)%args)
                        if (size(cargs) > 0) then
                            if (cargs(size(cargs))%kind == ARG_REST) &
                                rest_idx = size(cargs)
                        end if
                    end associate
                end if

                if (rest_idx > 0) then
                    ! Spec ends in a rest slot: tokenise only the leading
                    ! structured args, then take the remainder verbatim.
                    n_lead = rest_idx - 1
                    call parse_args_lead(rest, n_lead, args, tail)
                    ! Quote balance only constrains the structured lead; the
                    ! rest portion is free text and may contain a lone '"'.
                    lead_src = rest(1 : len(rest) - len(tail))
                    if (.not. has_balanced_quotes(lead_src)) then
                        call emit_error(this, "unmatched quote in arguments")
                        rc = RC_ERROR
                        call args%clear()
                        return
                    end if
                    tail = strip_leading_arg_space(tail)
                    if (len(tail) > 0) call args%append(char_node(tail))
                else
                    if (.not. has_balanced_quotes(rest)) then
                        call emit_error(this, "unmatched quote in arguments")
                        rc = RC_ERROR
                        call args%clear()
                        return
                    end if
                    call parse_args(rest, args)
                end if

                if (allocated(this%states(sidx_cur)%commands(match_idx)%args)) then
                    call validate_args( &
                        this%states(sidx_cur)%commands(match_idx)%args, &
                        args, ok, vmsg)
                    if (.not. ok) then
                        call emit_error(this, vmsg)
                        rc = RC_ERROR
                        call args%clear()
                        return
                    end if
                    ! Post-validate normalisation: a token parsed as integer in
                    ! a real slot is promoted to real(8) so the action receives
                    ! a real-typed node (parity with Tcl/C++).
                    call normalise_int_to_real( &
                        this%states(sidx_cur)%commands(match_idx)%args, args)
                end if
            end block
            rc = apply_edge(this, match_idx, args)
            call args%clear()
        case default
            call report_ambiguous(this, cmd)
            rc = RC_AMBIGUOUS
        end select
    end function dispatch_engine

    module subroutine set_io_units_engine(this, input_unit, output_unit, error_unit)
        class(engine_t), intent(inout)         :: this
        integer, intent(in), optional          :: input_unit
        integer, intent(in), optional          :: output_unit
        integer, intent(in), optional          :: error_unit

        if (present(input_unit))  this%input_unit  = input_unit
        if (present(output_unit)) this%output_unit = output_unit
        if (present(error_unit))  this%error_unit  = error_unit
    end subroutine set_io_units_engine

    ! ===== Inspection =====

    module function current_state_engine(this) result(name)
        class(engine_t), intent(in)            :: this
        character(len=:), allocatable          :: name
        if (this%stack_top <= 0) then
            name = ""
        else
            name = this%states(this%stack(this%stack_top)%state_idx)%name
        end if
    end function current_state_engine

    module function current_context_engine(this) result(ctx)
        class(engine_t), intent(in)            :: this
        character(len=:), allocatable          :: ctx
        if (this%stack_top <= 0) then
            ctx = ""
        else
            ctx = this%stack(this%stack_top)%context
        end if
    end function current_context_engine

    module function is_running_engine(this) result(b)
        class(engine_t), intent(in)            :: this
        logical                                :: b
        b = (this%stack_top > 0)
    end function is_running_engine

    ! Read-only enumeration of the current state's commands for menu/GUI
    ! builders. Pure map over the already-resolved commands(:) (includes were
    ! merged at finalize): no graph walk, no parse, no I/O. Empty array when
    ! the engine is not running. Result depends only on the current state, so
    ! callers may cache it.
    module function available_commands_engine(this) result(cmds)
        class(engine_t), intent(in)            :: this
        type(command_info_t), allocatable      :: cmds(:)
        integer                                :: sidx, i, n

        if (this%stack_top <= 0) then
            allocate(cmds(0))
            return
        end if
        sidx = this%stack(this%stack_top)%state_idx
        n = size(this%states(sidx)%commands)
        allocate(cmds(n))
        do i = 1, n
            associate (c => this%states(sidx)%commands(i))
                cmds(i)%spec = c%spec
                cmds(i)%req  = c%req
                cmds(i)%opt  = c%opt
                cmds(i)%kind = c%kind
                if (allocated(c%target)) cmds(i)%target = c%target
                if (allocated(c%help))   cmds(i)%help   = c%help
                if (allocated(c%args))   cmds(i)%args   = c%args
            end associate
        end do
    end function available_commands_engine

    ! State names from initial (index 1) to current top. Blank-padded to the
    ! longest name (mirrors the includes(:) array convention) — use trim().
    ! Empty when the engine is not running.
    module function state_path_engine(this) result(names)
        class(engine_t), intent(in)            :: this
        character(len=:), allocatable          :: names(:)
        character(len=:), allocatable          :: tmp(:)
        integer                                :: i, n, max_len

        n = max(this%stack_top, 0)
        max_len = 1
        do i = 1, n
            max_len = max(max_len, &
                len(this%states(this%stack(i)%state_idx)%name))
        end do
        ! Allocate via a local then move_alloc: gfortran 15.2.0 ICEs on a
        ! typed character allocate applied directly to the function result.
        allocate(character(len=max_len) :: tmp(n))
        do i = 1, n
            tmp(i) = this%states(this%stack(i)%state_idx)%name
        end do
        call move_alloc(tmp, names)
    end function state_path_engine

    ! ===== Internal helpers =====

    function find_state_idx(this, name) result(idx)
        class(engine_t), intent(in)            :: this
        character(len=*), intent(in)           :: name
        integer                                :: idx, i
        idx = 0
        if (.not. allocated(this%states)) return
        do i = 1, size(this%states)
            if (this%states(i)%name == name) then
                idx = i
                return
            end if
        end do
    end function find_state_idx

    subroutine parse_spec(spec, req, opt)
        character(len=*), intent(in)             :: spec
        character(len=:), allocatable, intent(out) :: req, opt
        integer                                  :: p1, p2
        p1 = index(spec, '(')
        p2 = index(spec, ')', back=.true.)
        if (p1 > 0 .and. p2 > p1) then
            req = spec(1:p1-1)
            opt = spec(p1+1:p2-1)
        else
            req = spec
            opt = ""
        end if
    end subroutine parse_spec

    subroutine split_first_token(line, cmd, rest)
        character(len=*), intent(in)             :: line
        character(len=:), allocatable, intent(out) :: cmd, rest
        character(len=:), allocatable            :: trimmed
        integer                                  :: sep

        trimmed = strip_leading_arg_space(line)
        if (len(trimmed) == 0) then
            cmd  = ""
            rest = ""
            return
        end if

        sep = first_arg_separator(trimmed)
        if (sep == 0) then
            cmd  = trimmed
            rest = ""
        else
            cmd  = trimmed(1:sep-1)
            rest = strip_leading_arg_space(trimmed(sep+1:))
        end if
    end subroutine split_first_token

    recursive function strip_leading_arg_space(text) result(rest)
        character(len=*), intent(in)             :: text
        character(len=:), allocatable            :: rest

        if (len(text) == 0) then
            rest = ""
        else if (scan(text(1:1), ARG_DELIMITERS) == 1) then
            if (len(text) == 1) then
                rest = ""
            else
                rest = strip_leading_arg_space(text(2:))
            end if
        else
            rest = text
        end if
    end function strip_leading_arg_space

    function first_arg_separator(text) result(pos)
        character(len=*), intent(in)             :: text
        integer                                  :: pos, i
        logical                                  :: in_quote

        pos = 0
        in_quote = .false.
        do i = 1, len(text)
            if (text(i:i) == '"') then
                in_quote = .not. in_quote
            else if (.not. in_quote) then
                if (scan(text(i:i), ARG_DELIMITERS) == 1) then
                    pos = i
                    return
                end if
            end if
        end do
    end function first_arg_separator

    function has_balanced_quotes(text) result(ok)
        character(len=*), intent(in)             :: text
        logical                                  :: ok

        ok = mod(count_char(text, '"'), 2) == 0
    end function has_balanced_quotes

    recursive function count_char(text, ch) result(n)
        character(len=*), intent(in)             :: text
        character(len=1), intent(in)             :: ch
        integer                                  :: n, p

        p = index(text, ch)
        if (p == 0) then
            n = 0
        else
            n = 1 + count_char(text(p+1:), ch)
        end if
    end function count_char

    subroutine parse_args(text, args)
        character(len=*), intent(in)             :: text
        type(dlist_t), intent(out)               :: args

        call parse_args_rec(text, args)
    end subroutine parse_args

    recursive subroutine parse_args_rec(text, args)
        character(len=*), intent(in)             :: text
        type(dlist_t), intent(inout)             :: args
        character(len=:), allocatable            :: trimmed, token, rest
        integer                                  :: sep

        trimmed = strip_leading_arg_space(text)
        if (len(trimmed) == 0) return

        sep = first_arg_separator(trimmed)
        if (sep == 0) then
            token = trimmed
            rest  = ""
        else
            token = trimmed(1:sep-1)
            rest  = trimmed(sep+1:)
        end if

        call append_arg_token(unquote_arg_token(token), args)
        call parse_args_rec(rest, args)
    end subroutine parse_args_rec

    ! Tokenise at most n_lead leading args (like parse_args), then return the
    ! unconsumed remainder verbatim in `tail` (a true suffix of `text`, so
    ! len(text)-len(tail) is the consumed-prefix length). Used for the
    ! rest-of-line slot: the caller takes `tail` as free text.
    subroutine parse_args_lead(text, n_lead, args, tail)
        character(len=*), intent(in)               :: text
        integer, intent(in)                        :: n_lead
        type(dlist_t), intent(out)                 :: args
        character(len=:), allocatable, intent(out) :: tail
        character(len=:), allocatable              :: cur, trimmed, token
        integer                                    :: taken, sep

        cur   = text
        taken = 0
        do while (taken < n_lead)
            trimmed = strip_leading_arg_space(cur)
            if (len(trimmed) == 0) then
                cur = ""
                exit
            end if
            sep = first_arg_separator(trimmed)
            if (sep == 0) then
                token = trimmed
                cur   = ""
            else
                token = trimmed(1:sep-1)
                cur   = trimmed(sep+1:)
            end if
            call append_arg_token(unquote_arg_token(token), args)
            taken = taken + 1
        end do
        tail = cur
    end subroutine parse_args_lead

    recursive function unquote_arg_token(token) result(unquoted)
        character(len=*), intent(in)             :: token
        character(len=:), allocatable            :: unquoted

        if (len(token) >= 2 .and. token(1:1) == '"' .and. token(len(token):len(token)) == '"') then
            if (len(token) == 2) then
                unquoted = ""
            else
                unquoted = token(2:len(token)-1)
            end if
        else
            unquoted = token
        end if
    end function unquote_arg_token

    subroutine append_arg_token(token, args)
        character(len=*), intent(in)             :: token
        type(dlist_t), intent(inout)             :: args
        integer                                  :: ival, iostat
        real(8)                                  :: rval

        if (is_integer_token(token)) then
            read(token, *, iostat=iostat) ival
            if (iostat == 0) then
                call args%append(int_node(ival))
                return
            end if
        end if

        if (is_real_token(token)) then
            read(token, *, iostat=iostat) rval
            if (iostat == 0) then
                call args%append(real_node(rval))
                return
            end if
        end if

        call args%append(char_node(token))
    end subroutine append_arg_token

    logical function is_integer_token(s)
        character(len=*), intent(in) :: s
        integer                      :: n
        character(len=*), parameter  :: integer_signs = '+-'
        character(len=*), parameter  :: integer_chars = '0123456789'

        n = len_trim(s)
        if (n == 0) then
            is_integer_token = .false.
            return
        end if
        is_integer_token = (verify(s(1:1), integer_chars) == 0) .or. &
                            (verify(s(1:1), integer_signs) == 0) .and. n > 1
        if (is_integer_token) then
            is_integer_token = verify(s(2:n), integer_chars) == 0
        end if

    end function is_integer_token

    ! True iff s is parseable as a Fortran real literal.
    ! Pre-screen with verify/scan to reject tokens whose characters fall
    ! outside the real-literal alphabet (handles `/` which is a list-directed
    ! input terminator, `.false.`, paths, etc.), then delegate to read.
    logical function is_real_token(s)
        character(len=*), intent(in) :: s
        real(8) :: rval
        integer :: n, iostat
        character(len=*), parameter :: real_chars = '+-0123456789.eEdD'
        character(len=*), parameter :: digits     = '0123456789'
        n = len_trim(s)
        is_real_token = .false.
        if (n == 0) return
        if (verify(s(:n), real_chars) /= 0) return
        if (scan(s(:n), digits) == 0) return
        read(s(:n), *, iostat=iostat) rval
        is_real_token = (iostat == 0)
    end function is_real_token

    subroutine find_matches(this, cmd, n_matches, match_idx)
        class(engine_t), intent(in)              :: this
        character(len=*), intent(in)             :: cmd
        integer, intent(out)                     :: n_matches, match_idx
        integer                                  :: i, sidx, clen, rlen, flen
        character(len=:), allocatable            :: req, opt, full

        n_matches = 0
        match_idx = 0
        sidx = this%stack(this%stack_top)%state_idx
        clen = len(cmd)
        do i = 1, size(this%states(sidx)%commands)
            req = this%states(sidx)%commands(i)%req
            opt = this%states(sidx)%commands(i)%opt
            full = req // opt
            rlen = len(req)
            flen = len(full)
            if (clen >= rlen .and. clen <= flen) then
                if (full(1:clen) == cmd) then
                    n_matches = n_matches + 1
                    match_idx = i
                end if
            end if
        end do
    end subroutine find_matches

    function apply_edge(this, cmd_idx, args) result(rc)
        class(engine_t), intent(inout)           :: this
        integer, intent(in)                      :: cmd_idx
        type(dlist_t), intent(in)                :: args
        integer                                  :: rc
        integer                                  :: sidx, tidx
        type(action_result_t)                    :: r
        character(len=:), allocatable            :: ctx

        sidx = this%stack(this%stack_top)%state_idx
        select case (this%states(sidx)%commands(cmd_idx)%kind)
        case (EDGE_ACTION)
            ctx = this%stack(this%stack_top)%context
            r = this%states(sidx)%commands(cmd_idx)%proc(args, ctx)
            if (r%errored) then
                if (allocated(r%errmsg)) then
                    if (len(r%errmsg) > 0) call emit_error(this, r%errmsg)
                end if
                rc = RC_ERROR
            else
                rc = RC_OK
            end if
        case (EDGE_GOTO)
            tidx = find_state_idx(this, this%states(sidx)%commands(cmd_idx)%target)
            call push_stack(this, tidx, "")
            call fire_on_enter(this)
            rc = RC_TRANSITIONED
        case (EDGE_DO_GOTO)
            ctx = this%stack(this%stack_top)%context
            r = this%states(sidx)%commands(cmd_idx)%proc(args, ctx)
            if (r%errored) then
                if (allocated(r%errmsg)) then
                    if (len(r%errmsg) > 0) call emit_error(this, r%errmsg)
                end if
                rc = RC_ERROR
            else if (allocated(r%value)) then
                if (len(r%value) > 0) then
                    tidx = find_state_idx(this, this%states(sidx)%commands(cmd_idx)%target)
                    call push_stack(this, tidx, r%value)
                    call fire_on_enter(this)
                    rc = RC_TRANSITIONED
                else
                    rc = RC_OK
                end if
            else
                rc = RC_OK
            end if
        case (EDGE_POP)
            this%stack_top = this%stack_top - 1
            if (this%stack_top <= 0) then
                rc = RC_EXITED
            else
                rc = RC_TRANSITIONED
            end if
        case (EDGE_DO_POP)
            ctx = this%stack(this%stack_top)%context
            r = this%states(sidx)%commands(cmd_idx)%proc(args, ctx)
            if (r%errored) then
                if (allocated(r%errmsg)) then
                    if (len(r%errmsg) > 0) call emit_error(this, r%errmsg)
                end if
                rc = RC_ERROR
            else
                this%stack_top = this%stack_top - 1
                if (this%stack_top <= 0) then
                    rc = RC_EXITED
                else
                    rc = RC_TRANSITIONED
                end if
            end if
        case (EDGE_QUIT)
            this%stack_top = 0
            rc = RC_EXITED
        case default
            rc = RC_ERROR
        end select
    end function apply_edge

    subroutine push_stack(this, state_idx, ctx)
        class(engine_t), intent(inout)           :: this
        integer, intent(in)                      :: state_idx
        character(len=*), intent(in)             :: ctx
        type(stack_entry_t), allocatable         :: tmp(:)
        integer                                  :: cap

        cap = size(this%stack)
        if (this%stack_top + 1 > cap) then
            allocate(tmp(cap * 2))
            tmp(1:this%stack_top) = this%stack(1:this%stack_top)
            call move_alloc(tmp, this%stack)
        end if
        this%stack_top = this%stack_top + 1
        this%stack(this%stack_top)%state_idx = state_idx
        this%stack(this%stack_top)%context   = ctx
    end subroutine push_stack

    subroutine fire_on_enter(this)
        class(engine_t), intent(inout)           :: this
        integer                                  :: sidx
        if (this%stack_top <= 0) return
        sidx = this%stack(this%stack_top)%state_idx
        if (associated(this%states(sidx)%on_enter)) then
            call this%states(sidx)%on_enter(this%stack(this%stack_top)%context)
        end if
    end subroutine fire_on_enter

    pure function arg_kind_name(k) result(nm)
        integer, intent(in)           :: k
        character(len=:), allocatable :: nm
        select case (k)
        case (ARG_INT);  nm = "int"
        case (ARG_REAL); nm = "real"
        case (ARG_CHAR); nm = "char"
        case (ARG_REST); nm = "rest"
        case default;    nm = "?"
        end select
    end function arg_kind_name

    ! Usage label for a command: the spec followed by its arg specs.
    ! Required args render as <name:kind>, optional as [name:kind].
    function command_usage(c) result(label)
        type(command_t), intent(in)   :: c
        character(len=:), allocatable :: label
        integer                       :: i
        label = c%spec
        if (allocated(c%args)) then
            do i = 1, size(c%args)
                if (c%args(i)%optional) then
                    label = label // " [" // trim(c%args(i)%name) // ":" // &
                            arg_kind_name(c%args(i)%kind) // "]"
                else
                    label = label // " <" // trim(c%args(i)%name) // ":" // &
                            arg_kind_name(c%args(i)%kind) // ">"
                end if
            end do
        end if
    end function command_usage

    subroutine show_help(this)
        class(engine_t), intent(inout)           :: this
        integer                                  :: sidx, i, max_len
        character(len=:), allocatable            :: label
        sidx = this%stack(this%stack_top)%state_idx
        max_len = 0
        do i = 1, size(this%states(sidx)%commands)
            label = command_usage(this%states(sidx)%commands(i))
            if (len(label) > max_len) max_len = len(label)
        end do
        do i = 1, size(this%states(sidx)%commands)
            associate (c => this%states(sidx)%commands(i))
                label = command_usage(c)
                if (allocated(c%help)) then
                    call emit_info(this, "  " // label // &
                          repeat(" ", max_len - len(label)) // "  " // c%help)
                else
                    call emit_info(this, "  " // label)
                end if
            end associate
        end do
    end subroutine show_help

    subroutine report_ambiguous(this, cmd)
        class(engine_t), intent(inout)           :: this
        character(len=*), intent(in)             :: cmd
        integer                                  :: sidx, i, clen, rlen, flen
        character(len=:), allocatable            :: req, opt, full, msg, sep
        logical                                  :: first

        sidx  = this%stack(this%stack_top)%state_idx
        clen  = len(cmd)
        msg   = "ambiguous: " // cmd // " matches"
        first = .true.
        do i = 1, size(this%states(sidx)%commands)
            req = this%states(sidx)%commands(i)%req
            opt = this%states(sidx)%commands(i)%opt
            full = req // opt
            rlen = len(req)
            flen = len(full)
            if (clen >= rlen .and. clen <= flen) then
                if (full(1:clen) == cmd) then
                    ! Canonical wording (matches Tcl / C++): the first match is
                    ! introduced by a space after "matches"; subsequent matches
                    ! are joined with ", ".
                    if (first) then
                        sep   = " "
                        first = .false.
                    else
                        sep = ", "
                    end if
                    msg = msg // sep // this%states(sidx)%commands(i)%spec
                end if
            end if
        end do
        call emit_info(this, msg)
    end subroutine report_ambiguous

    subroutine emit_prompt(this, msg)
        class(engine_t), intent(inout)           :: this
        character(len=*), intent(in)             :: msg

        this%last_message = msg
        if (this%output_unit /= QUIET_UNIT) then
            write(this%output_unit,'(a)', advance='no') msg
            flush(this%output_unit)
        end if
    end subroutine emit_prompt

    subroutine emit_info(this, msg)
        class(engine_t), intent(inout)           :: this
        character(len=*), intent(in)             :: msg

        this%last_message = msg
        if (this%output_unit /= QUIET_UNIT) write(this%output_unit,'(a)') msg
    end subroutine emit_info

    subroutine emit_error(this, msg)
        class(engine_t), intent(inout)           :: this
        character(len=*), intent(in)             :: msg

        call set_error(this, msg)
        if (this%error_unit /= QUIET_UNIT) write(this%error_unit,'(a)') msg
    end subroutine emit_error

    subroutine set_error(this, msg)
        class(engine_t), intent(inout)           :: this
        character(len=*), intent(in)             :: msg

        this%last_error = msg
    end subroutine set_error

    subroutine merge_commands(into, from)
        type(command_t), allocatable, intent(inout) :: into(:)
        type(command_t), intent(in)                 :: from(:)
        type(command_t), allocatable                :: tmp(:)
        integer                                     :: n_into, n_from, i, j
        logical                                     :: overridden

        n_into = size(into)
        n_from = size(from)

        ! Each `from` either overrides an existing `into` entry (matched by spec)
        ! or gets appended.
        allocate(tmp(n_into + n_from))
        tmp(1:n_into) = into
        j = n_into
        do i = 1, n_from
            overridden = .false.
            block
                integer :: k
                do k = 1, j
                    if (tmp(k)%spec == from(i)%spec) then
                        tmp(k) = from(i)
                        overridden = .true.
                        exit
                    end if
                end do
            end block
            if (.not. overridden) then
                j = j + 1
                tmp(j) = from(i)
            end if
        end do
        deallocate(into)
        allocate(into, source=tmp(1:j))
    end subroutine merge_commands

    subroutine die_missing(this, kind_str, spec, what, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: kind_str, spec, what
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        call set_build_error(this, "cmdgraph: " // kind_str // " edge '" // spec // &
                                   "' missing required " // what)
        call raise(this%build_error_msg, stat, errmsg)
    end subroutine die_missing

    ! Record the first construction-time error on the engine. Subsequent
    ! errors are ignored so the original cause is preserved.
    subroutine set_build_error(this, msg)
        class(engine_t), intent(inout) :: this
        character(len=*), intent(in)   :: msg
        if (this%build_error_stat /= 0) return
        this%build_error_stat = 1
        this%build_error_msg  = msg
    end subroutine set_build_error

    ! Propagate a sticky build error to caller-supplied stat/errmsg. Returns
    ! .true. if the engine is already in an error state and the caller should
    ! treat the current operation as a no-op.
    function propagate_build_error(this, stat, errmsg) result(blocked)
        class(engine_t), intent(in)                          :: this
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        logical :: blocked
        blocked = this%build_error_stat /= 0
        if (.not. blocked) return
        if (present(stat))   stat   = this%build_error_stat
        if (present(errmsg)) errmsg = this%build_error_msg
    end function propagate_build_error

    ! Surface a construction error: if `stat` is present set it and return the
    ! message via `errmsg`; otherwise write the message to stderr. Never stops
    ! the program — callers without `stat` still see the message but their
    ! program continues. Build errors are also recorded on the engine via
    ! `set_build_error` so `finalize` can re-surface them.
    subroutine raise(msg, stat, errmsg)
        character(len=*), intent(in)                         :: msg
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        if (present(stat)) then
            stat = 1
            if (present(errmsg)) errmsg = msg
            return
        end if
        write(DEFAULT_ERROR_UNIT,'(a)') msg
    end subroutine raise

    ! DFS cycle detection over goto/do_goto edges between concrete states.
    ! Sets found=.true. and emits a "A -> B -> ... -> A" message on first cycle.
    subroutine find_cycle(this, found, msg)
        class(engine_t), intent(in)                          :: this
        logical, intent(out)                                 :: found
        character(len=:), allocatable, intent(out)           :: msg
        integer, allocatable                                 :: color(:), parent(:)
        integer                                              :: n, i, ancestor, descendant

        n = size(this%states)
        allocate(color(n), parent(n))
        color  = 0   ! 0 = white, 1 = gray (in current DFS path), 2 = black (done)
        parent = 0
        found     = .false.
        ancestor  = 0
        descendant = 0

        do i = 1, n
            if (.not. allocated(this%states(i)%prompt)) cycle  ! skip abstract
            if (color(i) == 0) then
                call dfs(this, i, color, parent, found, ancestor, descendant)
                if (found) exit
            end if
        end do

        if (found) call build_cycle_message(this, ancestor, descendant, parent, msg)
    end subroutine find_cycle

    recursive subroutine dfs(this, u, color, parent, found, ancestor, descendant)
        class(engine_t), intent(in)              :: this
        integer, intent(in)                      :: u
        integer, intent(inout)                   :: color(:), parent(:)
        logical, intent(inout)                   :: found
        integer, intent(inout)                   :: ancestor, descendant
        integer                                  :: j, vidx, kind

        color(u) = 1
        do j = 1, size(this%states(u)%commands)
            kind = this%states(u)%commands(j)%kind
            if (kind /= EDGE_GOTO .and. kind /= EDGE_DO_GOTO) cycle
            vidx = find_state_idx(this, this%states(u)%commands(j)%target)
            if (vidx == 0) cycle                                ! validated earlier
            if (.not. allocated(this%states(vidx)%prompt)) cycle ! abstract; validated earlier
            select case (color(vidx))
            case (0)
                parent(vidx) = u
                call dfs(this, vidx, color, parent, found, ancestor, descendant)
                if (found) return
            case (1)
                found      = .true.
                ancestor   = vidx
                descendant = u
                return
            end select
        end do
        color(u) = 2
    end subroutine dfs

    ! Build "ancestor -> ... -> descendant -> ancestor" by walking parent[] up
    ! from descendant to ancestor and reversing.
    subroutine build_cycle_message(this, ancestor, descendant, parent, msg)
        class(engine_t), intent(in)                  :: this
        integer, intent(in)                          :: ancestor, descendant
        integer, intent(in)                          :: parent(:)
        character(len=:), allocatable, intent(out)   :: msg
        integer, allocatable                         :: path(:)
        integer                                      :: n, cur, i

        n = 1
        cur = descendant
        do while (cur /= ancestor)
            cur = parent(cur)
            n = n + 1
        end do

        allocate(path(n + 1))
        cur = descendant
        do i = n, 1, -1
            path(i) = cur
            if (cur == ancestor) exit
            cur = parent(cur)
        end do
        path(n + 1) = ancestor  ! close

        msg = "cmdgraph: cycle detected: " // this%states(path(1))%name
        do i = 2, n + 1
            msg = msg // " -> " // this%states(path(i))%name
        end do
    end subroutine build_cycle_message

    ! ===== Argument-spec constructors =====

    module function arg_is_int(name, optional) result(s)
        character(len=*), intent(in)   :: name
        logical, intent(in), optional  :: optional
        type(arg_spec_t)               :: s
        s%name = name
        s%kind = ARG_INT
        if (present(optional)) s%optional = optional
    end function arg_is_int

    module function arg_is_rest(name, optional) result(s)
        character(len=*), intent(in)   :: name
        logical, intent(in), optional  :: optional
        type(arg_spec_t)               :: s
        s%name = name
        s%kind = ARG_REST
        if (present(optional)) s%optional = optional
    end function arg_is_rest

    module function arg_is_real(name, optional) result(s)
        character(len=*), intent(in)   :: name
        logical, intent(in), optional  :: optional
        type(arg_spec_t)               :: s
        s%name = name
        s%kind = ARG_REAL
        if (present(optional)) s%optional = optional
    end function arg_is_real

    module function arg_is_char(name, optional) result(s)
        character(len=*), intent(in)   :: name
        logical, intent(in), optional  :: optional
        type(arg_spec_t)               :: s
        s%name = name
        s%kind = ARG_CHAR
        if (present(optional)) s%optional = optional
    end function arg_is_char

    module function arg_int_n(name, n) result(s)
        character(len=*), intent(in)  :: name
        integer, intent(in)           :: n
        type(arg_spec_t), allocatable :: s(:)
        type(arg_spec_t)              :: proto
        proto%name = name
        proto%kind = ARG_INT
        allocate(s(n), source=proto)
    end function arg_int_n

    module function arg_real_n(name, n) result(s)
        character(len=*), intent(in)  :: name
        integer, intent(in)           :: n
        type(arg_spec_t), allocatable :: s(:)
        type(arg_spec_t)              :: proto
        proto%name = name
        proto%kind = ARG_REAL
        allocate(s(n), source=proto)
    end function arg_real_n

    module function action_ok(ctx) result(rv)
        character(len=*), intent(in), optional :: ctx
        type(action_result_t)                  :: rv
        if (present(ctx)) rv%value = ctx
    end function action_ok

    module function action_error(msg) result(rv)
        character(len=*), intent(in), optional :: msg
        type(action_result_t)                  :: rv
        rv%errored = .true.
        if (present(msg)) rv%errmsg = msg
    end function action_error

    ! Validate `args` against `spec`. Returns ok=.true. on success, or
    ! ok=.false. with a stderr-ready message describing the mismatch.
    !
    ! Rules:
    !   - Required (non-optional) positions in `spec` must be present.
    !     A non-optional spec slot following an optional one is still
    !     required if any args after it are present.
    !   - Trailing optional positions may be omitted; the engine does not
    !     reorder, so once an arg is omitted no later args are allowed.
    !   - Extra args beyond size(spec) are an error.
    !   - Each provided arg's parsed dynamic type must match the slot kind.
    subroutine validate_args(spec, args, ok, msg)
        type(arg_spec_t), intent(in)               :: spec(:)
        type(dlist_t),    intent(in)               :: args
        logical,          intent(out)              :: ok
        character(len=:), allocatable, intent(out) :: msg
        integer                                    :: i, n_args, n_required
        class(dlist_node_data_t), allocatable      :: node

        n_args = args%size()

        ! Smallest acceptable n_args: position of the last non-optional slot.
        n_required = findloc(spec%optional, .false., dim=1, back=.true.)

        if (n_args < n_required) then
            msg = "missing required argument <" // trim(spec(n_args + 1)%name) // ">"
            ok  = .false.
            return
        end if

        if (n_args > size(spec)) then
            msg = "unexpected extra argument"
            ok  = .false.
            return
        end if

        do i = 1, n_args
            node = args%get(i)
            select case (spec(i)%kind)
            case (ARG_INT)
                select type (node)
                type is (dlist_node_integer)
                    continue
                class default
                    msg = "argument <" // trim(spec(i)%name) // "> expects integer"
                    ok  = .false.
                    return
                end select
            case (ARG_REAL)
                ! Promote int → real: an integer literal is accepted in a real
                ! slot.  The actual node-type substitution is done by
                ! `normalise_int_to_real` after this routine returns (validate
                ! has intent(in) args so it cannot mutate the list itself).
                select type (node)
                type is (dlist_node_real)
                    continue
                type is (dlist_node_integer)
                    continue
                class default
                    msg = "argument <" // trim(spec(i)%name) // "> expects real"
                    ok  = .false.
                    return
                end select
            case (ARG_CHAR)
                select type (node)
                type is (dlist_node_char)
                    continue
                class default
                    msg = "argument <" // trim(spec(i)%name) // "> expects string"
                    ok  = .false.
                    return
                end select
            end select
        end do

        ok = .true.
    end subroutine validate_args

    ! Post-validate normalisation. For every ARG_REAL slot whose actual list
    ! node is an integer (which validate_args accepts), replace it in place
    ! with a real(8) node so the action sees a real-typed value. Mirrors the
    ! C++ ARG_REAL int-variant promotion and the Tcl validate_args acceptance.
    subroutine normalise_int_to_real(spec, args)
        type(arg_spec_t),  intent(in)         :: spec(:)
        type(dlist_t),     intent(inout)      :: args
        class(dlist_node_data_t), allocatable :: node
        integer                               :: i, n, ival

        n = min(size(spec), args%size())
        do i = 1, n
            if (spec(i)%kind /= ARG_REAL) cycle
            node = args%get(i)
            select type (node)
            type is (dlist_node_integer)
                ival = node%data
                call args%remove(i)
                call args%insert(i, real_node(real(ival, 8)))
            end select
        end do
    end subroutine normalise_int_to_real

end submodule cmdgraph_sm

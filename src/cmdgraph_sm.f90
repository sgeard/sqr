!! SPDX-License-Identifier: MIT
!! Copyright (c) 2026 Simon Geard
!! Vendored into sqr from https://github.com/sgeard/cmdgraph (fortran/src) at svn r2451, 2026-11-23 (v1.5.0); kept in sync by hand.
!!
submodule (cmdgraph) cmdgraph_sm
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    ! Per-state commands snapshot. Used by finalize_engine to resolve includes
    ! from the originally-parsed graph rather than from already-merged states
    ! (so includes are flat and order-independent, matching the Tcl version).
    type :: cmd_array_t
        type(command_t), allocatable :: items(:)
    end type

    ! --- Input-encoding signatures ---
    integer, parameter :: MAX_SIG_BYTES = 4

    ! One byte-order mark (or other leading signature) and what it identifies.
    type :: signature_t
        character(len=MAX_SIG_BYTES) :: bytes   ! blank-padded; only 1:nbytes count
        integer                      :: nbytes
        integer                      :: enc
    end type

    character(len=1), parameter :: NUL = achar(0)

    ! --- Argument tokens ---
    character(len=*), parameter :: QUOTE_CHARS    = '''"'
    character(len=*), parameter :: DIGIT_CHARS    = '0123456789'
    character(len=*), parameter :: EXPONENT_CHARS = 'eEdD'

    ! Tested in this order, so a signature that begins with a shorter one
    ! (UTF-32 LE starts with the UTF-16 LE mark) must come first.
    type(signature_t), parameter :: SIGNATURES(*) = [ &
        signature_t(char(int(z'FF')) // char(int(z'FE')) // NUL // NUL,                             4, ENC_UTF32_LE),   &
        signature_t(NUL // NUL // char(int(z'FE')) // char(int(z'FF')),                             4, ENC_UTF32_BE),   &
        signature_t(char(int(z'DD')) // char(int(z'73')) // char(int(z'66')) // char(int(z'73')),   4, ENC_UTF_EBCDIC), &
        signature_t(char(int(z'84')) // char(int(z'31')) // char(int(z'95')) // char(int(z'33')),   4, ENC_GB18030),    &
        signature_t(char(int(z'EF')) // char(int(z'BB')) // char(int(z'BF')) // ' ',                3, ENC_UTF8_BOM),   &
        signature_t(char(int(z'2B')) // char(int(z'2F')) // char(int(z'76')) // ' ',                3, ENC_UTF7),       &
        signature_t(char(int(z'F7')) // char(int(z'64')) // char(int(z'4C')) // ' ',                3, ENC_UTF1),       &
        signature_t(char(int(z'0E')) // char(int(z'FE')) // char(int(z'FF')) // ' ',                3, ENC_SCSU),       &
        signature_t(char(int(z'FB')) // char(int(z'EE')) // char(int(z'28')) // ' ',                3, ENC_BOCU1),      &
        signature_t(char(int(z'FF')) // char(int(z'FE')) // '  ',                                   2, ENC_UTF16_LE),   &
        signature_t(char(int(z'FE')) // char(int(z'FF')) // '  ',                                   2, ENC_UTF16_BE)    ]

contains

    module function version_t_string(this) result(s)
        class(version_t), intent(in)  :: this
        character(len=:), allocatable :: s
        character(len=40)             :: buf
        write(buf, '(i0,".",i0,".",i0)') this%major, this%minor, this%patch
        s = trim(buf)
    end function version_t_string

    ! ===== Construction =====

    ! Common builder-method preamble: propagate any sticky build error, reject
    ! use after finalize, and (when `state` is supplied) resolve it to an index.
    ! Returns .false. — having set the sticky error and raised stat/errmsg — on
    ! any failure, whereupon the caller returns immediately.  add_state omits
    ! `state`: it does its own inverted duplicate-name check.
    function builder_guard(this, opname, stat, errmsg, state, sidx) result(ok)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: opname
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        character(len=*), intent(in), optional               :: state
        integer, intent(out), optional                       :: sidx
        logical                                              :: ok
        integer                                              :: idx

        ok = .false.
        if (propagate_build_error(this, stat, errmsg)) return
        if (this%finalized) then
            call set_build_error(this, "cmdgraph: " // opname // ": engine already finalized")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        if (present(state)) then
            idx = find_state_idx(this, state)
            if (idx == 0) then
                call set_build_error(this, "cmdgraph: " // opname // ": unknown state '" // state // "'")
                call raise(this%build_error_msg, stat, errmsg)
                return
            end if
            if (present(sidx)) sidx = idx
        end if
        ok = .true.
    end function builder_guard

    module subroutine add_state_engine(this, name, prompt, stat, errmsg)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: name
        character(len=*), intent(in), optional               :: prompt
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        type(state_t), allocatable                           :: tmp(:)
        integer                                              :: n

        if (.not. builder_guard(this, "add_state", stat=stat, errmsg=errmsg)) return
        if (.not. allocated(this%states)) allocate(this%states(0))
        if (find_state_idx(this, name) /= 0) then
            call set_build_error(this, "cmdgraph: state '" // name // "' already added")
            call raise(this%build_error_msg, stat, errmsg)
            return
        end if
        ! Grow the states array by capacity-doubling (mirrors the command-array
        ! scheme in add_command), avoiding an O(N) deep copy of every state_t
        ! per add.  state_count is the live count; finalize trims to it.
        if (this%state_count == size(this%states)) then
            n = max(size(this%states) * 2, 8)
            allocate(tmp(n))
            tmp(1:this%state_count) = this%states(1:this%state_count)
            call move_alloc(tmp, this%states)
        end if
        this%state_count = this%state_count + 1
        this%states(this%state_count)%name = name
        if (present(prompt)) this%states(this%state_count)%prompt = prompt
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

        if (.not. builder_guard(this, "add_command", state=state, sidx=sidx, &
                                stat=stat, errmsg=errmsg)) return

        new_cmd%spec = spec
        new_cmd%kind = kind
        if (present(help))   new_cmd%help   = help
        if (present(target)) new_cmd%target = target
        if (present(proc))   new_cmd%proc   => proc
        if (present(args))   new_cmd%args   = args

        ! Validate that the edge kind is known and carries the components it
        ! needs.  edge_kind_name doubles as the known-kind test (empty ⇒ unknown)
        ! and as the spelling used in die_missing.  For do_goto/do_swap the
        ! missing-target error must precede the missing-proc error, so target is
        ! tested first.
        validate_kind: block
            logical                       :: needs_target, needs_proc
            character(len=:), allocatable :: kname
            kname = edge_kind_name(kind)
            if (len(kname) == 0) then
                write(kind_buf,'(i0)') kind
                call set_build_error(this, "cmdgraph: unknown edge kind " // trim(kind_buf))
                call raise(this%build_error_msg, stat, errmsg)
                return
            end if
            needs_target = any(kind == [EDGE_GOTO, EDGE_SWAP, EDGE_DO_GOTO, EDGE_DO_SWAP])
            needs_proc   = any(kind == [EDGE_ACTION, EDGE_DO_GOTO, EDGE_DO_SWAP, EDGE_DO_POP])
            if (needs_target) then
                if (.not. allocated(new_cmd%target)) then
                    call die_missing(this, kname, spec, "target", stat, errmsg)
                    return
                end if
            end if
            if (needs_proc) then
                if (.not. associated(new_cmd%proc)) then
                    call die_missing(this, kname, spec, "proc", stat, errmsg)
                    return
                end if
            end if
        end block validate_kind

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

        if (.not. builder_guard(this, "add_include", state=state, sidx=sidx, &
                                stat=stat, errmsg=errmsg)) return

        ! Grow-by-one is retained here (unlike states/commands): includes are
        ! rare (1-2 per state), the element is a short state name rather than a
        ! deep state_t, and the shared deferred length must be re-fitted on each
        ! add anyway — capacity headroom would buy nothing.
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

        if (.not. builder_guard(this, "set_on_enter", state=state, sidx=sidx, &
                                stat=stat, errmsg=errmsg)) return
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

        ! Trim the states array from build capacity (add_state doubles) to the
        ! exact count, so every `size(this%states)` loop below — snapshot,
        ! transform, rollback — sees only real states.  Done before the
        ! snapshot; a failed finalize leaves the array trimmed but unfinalized,
        ! and a retry re-grows it via add_state.
        if (this%state_count < size(this%states)) then
            trim_states: block
                type(state_t), allocatable :: kept(:)
                allocate(kept(this%state_count))
                kept(1:this%state_count) = this%states(1:this%state_count)
                call move_alloc(kept, this%states)
            end block trim_states
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

            ! Parse every command's spec into req/opt/full and cache the rest-arg
            ! slot.  Done before the include-merge so these caches ride through
            ! merge_commands' whole-command copy (only target_idx, which needs
            ! the merged graph, is deferred to the target-validation pass).
            do i = 1, size(this%states)
                do j = 1, size(this%states(i)%commands)
                    associate (cmd => this%states(i)%commands(j))
                        call parse_spec(cmd%spec, cmd%req, cmd%opt, cmd%full)
                        if (allocated(cmd%args)) then
                            if (size(cmd%args) > 0) &
                                cmd%rest_idx = findloc(cmd%args%kind, ARG_REST, dim=1)
                        end if
                    end associate
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

            ! Validate goto/do_goto/swap/do_swap targets
            do i = 1, size(this%states)
                do j = 1, size(this%states(i)%commands)
                    if (this%states(i)%commands(j)%kind == EDGE_GOTO    .or. &
                        this%states(i)%commands(j)%kind == EDGE_DO_GOTO .or. &
                        this%states(i)%commands(j)%kind == EDGE_SWAP    .or. &
                        this%states(i)%commands(j)%kind == EDGE_DO_SWAP) then
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
                        ! Cache the resolved index so apply_edge/dfs skip the
                        ! per-transition find_state_idx scan (E1).
                        this%states(i)%commands(j)%target_idx = tidx
                    end if
                end do
            end do

            ! Validate DAG-ness of forward edges (goto/do_goto) between concrete
            ! states. pop is the return path; swap/do_swap replace the top frame
            ! (pop-then-push) so they are inherently cyclic and exempt too;
            ! abstract states are command mix-ins, not nodes.
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

    module subroutine run_engine(this, stat)
        class(engine_t), intent(inout)         :: this
        integer, intent(out), optional         :: stat
        character(len=4096)                    :: buffer
        integer                                :: iostat, rc, enc
        logical                                :: first
        character(len=:), allocatable          :: prompt, src

        if (present(stat)) stat = RUN_OK

        first = .true.
        do while (this%is_running())
            prompt = this%states(this%stack(this%stack_top)%state_idx)%prompt
            call emit_prompt(this, prompt)
            read(this%input_unit, '(a)', iostat=iostat) buffer
            if (iostat /= 0) exit
            src = trim(buffer)
            call screen_line(src, first, enc)
            first = .false.
            if (.not. encoding_supported(enc)) then
                call emit_error(this, unsupported_message(enc))
                if (present(stat)) stat = RUN_BAD_ENCODING
                return
            end if
            rc = this%dispatch(src)
        end do
    end subroutine run_engine

    ! Drive the engine from a script file. Returns ok=.true. iff the file
    ! opened and every dispatched line succeeded (or the script quit cleanly).
    ! On failure ok=.false. and the optional out args report which line and
    ! why: stat = RUN_OPEN_FAILED for a file-open failure, RUN_BAD_ENCODING
    ! for a line in an encoding the engine does not read (that line is not
    ! dispatched), otherwise the failing line's dispatch return code
    ! (RC_UNKNOWN/RC_AMBIGUOUS/RC_ERROR); line = the 1-based file line number
    ! (0 for a file-open failure); errmsg = the diagnostic text. Stops at the
    ! first failing line (the engine is left in whatever state that line
    ! produced).
    module function run_file_engine(this, path, echo, stat, errmsg, line) result(ok)
        class(engine_t), intent(inout)                       :: this
        character(len=*), intent(in)                         :: path
        logical, intent(in), optional                        :: echo
        integer, intent(out), optional                       :: stat
        character(len=:), allocatable, intent(out), optional :: errmsg
        integer, intent(out), optional                       :: line
        logical                                              :: ok
        integer                                :: unit, iostat, rc, lineno, enc
        logical                                :: do_echo
        character(len=4096)                    :: buffer
        character(len=:), allocatable          :: src, trimmed, prompt

        if (present(stat))   stat = RUN_OK
        if (present(errmsg)) errmsg = ""
        if (present(line))   line = 0

        do_echo = .true.
        if (present(echo)) do_echo = echo

        open(newunit=unit, file=path, status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            call set_error(this, "could not open script file: " // path)
            if (present(stat))   stat = RUN_OPEN_FAILED
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
            call screen_line(src, lineno == 1, enc)
            if (.not. encoding_supported(enc)) then
                ! Populated silently, like the open failure: the error
                ! channel is for events raised by a dispatched line.
                call set_error(this, unsupported_message(enc) // ": " // path)
                if (present(stat))   stat = RUN_BAD_ENCODING
                if (present(line))   line = lineno
                if (present(errmsg)) errmsg = this%last_error
                close(unit)
                ok = .false.
                return
            end if
            trimmed = adjustl(src)
            if (len_trim(trimmed) == 0)   cycle
            if (trimmed(1:1) == '#')      cycle
            prompt = this%states(this%stack(this%stack_top)%state_idx)%prompt
            if (do_echo) call emit_info(this, prompt // src)
            rc = this%dispatch(src)
            if (rc == RC_UNKNOWN .or. rc == RC_AMBIGUOUS .or. rc == RC_ERROR) then
                if (present(stat)) stat = rc
                if (present(line)) line = lineno
                if (present(errmsg)) errmsg = this%last_error
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
        integer                                :: n_matches, match_idx, sidx_cur
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
                call emit_error(this, "unknown: " // cmd)
                rc = RC_UNKNOWN
            end if
        case (1)
            sidx_cur = this%stack(this%stack_top)%state_idx
            rc = parse_and_validate(this, sidx_cur, match_idx, rest, args)
            if (rc == RC_OK) rc = apply_edge(this, match_idx, args)
            ! Single cleanup point (Q7).  dlist_t has final :: finalize_ll so
            ! args is freed on scope exit anyway; this explicit clear is
            ! belt-and-braces against historic gfortran local-finalization gaps
            ! and is kept deliberately — do not "simplify" it away.  On the
            ! parse/validate error path parse_and_validate returns a
            ! populated-then-abandoned list; this same clear cleans it up.
            call args%clear()
        case default
            call report_ambiguous(this, cmd)
            rc = RC_AMBIGUOUS
        end select
    end function dispatch_engine

    ! Parse `rest` into `args` for the single matched command and validate it
    ! against the command's arg spec (R6).  Returns RC_OK when args are ready
    ! for apply_edge, or RC_ERROR (having emitted the message) on a malformed
    ! quoted token or a failed validation.  `args` is intent(out): it is reset
    ! on entry and, on an error return, left populated for the caller's single
    ! clear.  For a rest slot only the leading structured args are tokenised,
    ! so quote characters in the free-text remainder mean nothing.
    function parse_and_validate(this, sidx, match_idx, rest, args) result(rc)
        class(engine_t), intent(inout)         :: this
        integer, intent(in)                    :: sidx, match_idx
        character(len=*), intent(in)           :: rest
        type(dlist_t), intent(out)             :: args
        integer                                :: rc
        integer                                :: rest_idx, n_lead
        logical                                :: ok
        character(len=:), allocatable          :: vmsg, tail, perr
        integer                                :: next

        rc = RC_OK
        rest_idx = this%states(sidx)%commands(match_idx)%rest_idx

        ! Spec ends in a rest slot: tokenise only the leading structured args,
        ! then take the remainder verbatim.
        n_lead = merge(rest_idx - 1, -1, rest_idx > 0)
        call parse_args(rest, n_lead, args, next, perr)
        if (len(perr) > 0) then
            call emit_error(this, perr)
            rc = RC_ERROR
            return
        end if
        if (rest_idx > 0) then
            tail = strip_leading_arg_space(rest(next:))
            if (len(tail) > 0) call args%append(char_node(tail))
        end if

        if (allocated(this%states(sidx)%commands(match_idx)%args)) then
            call validate_args(this%states(sidx)%commands(match_idx)%args, args, ok, vmsg)
            if (.not. ok) then
                call emit_error(this, vmsg)
                rc = RC_ERROR
                return
            end if
            ! Post-validate normalisation: a token parsed as integer in a real
            ! slot is promoted to a real node so the action receives a
            ! real-typed value (parity with Tcl/C++).
            call normalise_int_to_real(this%states(sidx)%commands(match_idx)%args, args)
        end if
    end function parse_and_validate

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
        do i = 1, this%state_count
            if (this%states(i)%name == name) then
                idx = i
                return
            end if
        end do
    end function find_state_idx

    subroutine parse_spec(spec, req, opt, full)
        character(len=*), intent(in)               :: spec
        character(len=:), allocatable, intent(out) :: req, opt, full
        integer                                    :: p1, p2
        p1 = index(spec, '(')
        p2 = index(spec, ')', back=.true.)
        if (p1 > 0 .and. p2 > p1) then
            req = spec(1:p1-1)
            opt = spec(p1+1:p2-1)
        else
            req = spec
            opt = ""
        end if
        full = req // opt   ! longest acceptable abbreviation (E2)
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

    pure function strip_leading_arg_space(text) result(rest)
        character(len=*), intent(in)             :: text
        character(len=:), allocatable            :: rest
        integer                                  :: p

        p = verify(text, ARG_DELIMITERS)   ! first non-delimiter, 0 if none/empty
        if (p == 0) then
            rest = ""
        else
            rest = text(p:)
        end if
    end function strip_leading_arg_space

    ! Position of the first delimiter in text, 0 if none.
    pure function first_arg_separator(text) result(pos)
        character(len=*), intent(in)             :: text
        integer                                  :: pos

        pos = scan(text, ARG_DELIMITERS)
    end function first_arg_separator

    ! Tokenise `text`, at most `max_toks` tokens (all of them when max_toks is
    ! negative), appending each to `args`.  `next` is the position just after
    ! the last token taken, so text(next:) is the unconsumed remainder (empty
    ! when next > len(text)).  `errmsg` is "" or the message for a malformed
    ! quoted token.
    !
    ! A token that starts with ' or " is a quoted token: always text, flagged
    ! `quoted` on its node.  Any other token runs to the next delimiter, a
    ! quote character inside it is ordinary, and its type is inferred.
    subroutine parse_args(text, max_toks, args, next, errmsg)
        character(len=*), intent(in)               :: text
        integer, intent(in)                        :: max_toks
        type(dlist_t), intent(out)                 :: args
        integer, intent(out)                       :: next
        character(len=:), allocatable, intent(out) :: errmsg
        character(len=:), allocatable              :: value
        type(dlist_node_char)                      :: qnode
        integer                                    :: p, sep, taken, after

        errmsg = ""
        next   = 1
        taken  = 0
        tokens: do
            if (max_toks >= 0 .and. taken >= max_toks) exit tokens
            if (next > len(text)) exit tokens
            p = verify(text(next:), ARG_DELIMITERS)
            if (p == 0) then                         ! only delimiters remain
                next = len(text) + 1
                exit tokens
            end if
            next = next + p - 1
            if (scan(text(next:next), QUOTE_CHARS) == 1) then
                call read_quoted(text, next, value, after, errmsg)
                if (len(errmsg) > 0) exit tokens
                next = after
                qnode = char_node(value)
                qnode%quoted = .true.
                call args%append(qnode)
            else
                sep = first_arg_separator(text(next:))
                if (sep == 0) sep = len(text) - next + 2
                call append_arg_token(text(next:next+sep-2), args)
                next = next + sep - 1
            end if
            taken = taken + 1
        end do tokens
    end subroutine parse_args

    ! Read the quoted token whose opening quote is at text(start:start).  The
    ! token runs to the next occurrence of the same quote character; that
    ! character written twice is one of it.  `next` is the position just after
    ! the closing quote, which must be a delimiter or the end of the text.
    pure subroutine read_quoted(text, start, value, next, errmsg)
        character(len=*), intent(in)               :: text
        integer, intent(in)                        :: start
        character(len=:), allocatable, intent(out) :: value
        integer, intent(out)                       :: next
        character(len=:), allocatable, intent(out) :: errmsg
        character(len=1)                           :: q
        integer                                    :: from, p

        q      = text(start:start)
        value  = ""
        errmsg = ""
        next   = start
        from   = start + 1
        pieces: do
            p = 0
            if (from <= len(text)) p = index(text(from:), q)
            if (p == 0) then
                errmsg = "unmatched quote in arguments"
                return
            end if
            p = from + p - 1                         ! the quote, in text
            value = value // text(from:p-1)
            if (p == len(text)) exit pieces
            if (text(p+1:p+1) /= q) exit pieces
            value = value // q                       ! doubled: one of it
            from = p + 2
        end do pieces
        next = p + 1
        if (next <= len(text)) then
            if (scan(text(next:next), ARG_DELIMITERS) == 0) &
                errmsg = "unexpected character after closing quote"
        end if
    end subroutine read_quoted

    ! Append an unquoted token as an integer, a real or text, by its spelling.
    subroutine append_arg_token(token, args)
        character(len=*), intent(in)             :: token
        type(dlist_t), intent(inout)             :: args
        integer                                  :: ival, iostat
        real(dp)                                 :: rval
        logical                                  :: is_int

        call read_integer_token(token, is_int, ival)
        if (is_int) then
            call args%append(int_node(ival))
            return
        end if

        if (is_real_token(token)) then
            ! The spelling is already known to be a real literal, so the read
            ! only converts.  A value too large to hold is not a number; one
            ! too small to hold is zero.
            read(token, *, iostat=iostat) rval
            if (iostat /= 0) then
                if (has_negative_exponent(token)) then
                    rval   = 0.0_dp
                    iostat = 0
                end if
            end if
            if (iostat == 0) then
                if (ieee_is_finite(rval)) then
                    call args%append(real_node(rval))
                    return
                end if
            end if
        end if

        call args%append(char_node(token))
    end subroutine append_arg_token

    ! Number of leading digits in s(from:); 0 when from is past the end.
    pure function digit_run(s, from) result(n)
        character(len=*), intent(in) :: s
        integer, intent(in)          :: from
        integer                      :: n

        n = 0
        if (from > len(s)) return
        n = verify(s(from:), DIGIT_CHARS) - 1
        if (n < 0) n = len(s) - from + 1
    end function digit_run

    ! `is_int` is true iff s is [+-]digits with a value that fits a default
    ! integer; `ival` is then that value (0 otherwise).
    pure subroutine read_integer_token(s, is_int, ival)
        character(len=*), intent(in) :: s
        logical, intent(out)         :: is_int
        integer, intent(out)         :: ival
        integer                      :: first, i
        integer(int64)               :: v
        logical                      :: negative

        is_int = .false.
        ival   = 0
        if (len(s) == 0) return
        negative = s(1:1) == '-'
        first = merge(2, 1, scan(s(1:1), '+-') == 1)
        if (first > len(s)) return
        if (verify(s(first:), DIGIT_CHARS) /= 0) return
        ! Leading zeros carry no value; ten digits is the most that can fit.
        i = verify(s(first:), '0')
        if (i == 0) then
            is_int = .true.
            return
        end if
        first = first + i - 1
        if (len(s) - first + 1 > 10) return
        v = 0_int64
        do i = first, len(s)
            v = 10_int64 * v + int(index(DIGIT_CHARS, s(i:i)) - 1, int64)
        end do
        if (negative) v = -v
        if (v < -int(huge(ival), int64) - 1_int64 .or. v > int(huge(ival), int64)) return
        ival   = int(v)
        is_int = .true.
    end subroutine read_integer_token

    ! True iff s is spelt as a real literal:
    !   [+-](digits[.digits] | .digits)[(e|E|d|D)[+-]digits]
    ! Decided by the spelling alone, so the answer is the same on every
    ! compiler (list-directed input accepts more, and differs between them).
    pure function is_real_token(s) result(is_real)
        character(len=*), intent(in) :: s
        logical                      :: is_real
        integer                      :: pos, n_int, n_frac, n_exp

        is_real = .false.
        if (len(s) == 0) return
        pos   = merge(2, 1, scan(s(1:1), '+-') == 1)
        n_int = digit_run(s, pos)
        pos   = pos + n_int
        if (pos > len(s)) then
            is_real = n_int > 0
            return
        end if
        if (s(pos:pos) == '.') then
            n_frac = digit_run(s, pos + 1)
            if (n_int == 0 .and. n_frac == 0) return
            pos = pos + 1 + n_frac
        else if (n_int == 0) then
            return
        end if
        if (pos > len(s)) then
            is_real = .true.
            return
        end if
        if (scan(s(pos:pos), EXPONENT_CHARS) == 0) return
        pos = pos + 1
        if (pos > len(s)) return
        if (scan(s(pos:pos), '+-') == 1) pos = pos + 1
        n_exp   = digit_run(s, pos)
        is_real = n_exp > 0 .and. pos + n_exp - 1 == len(s)
    end function is_real_token

    ! True iff the exponent of a real literal is negative.
    pure function has_negative_exponent(s) result(negative)
        character(len=*), intent(in) :: s
        logical                      :: negative
        integer                      :: pos

        negative = .false.
        pos = scan(s, EXPONENT_CHARS)
        if (pos == 0 .or. pos == len(s)) return
        negative = s(pos+1:pos+1) == '-'
    end function has_negative_exponent

    ! Prefix-match test shared by find_matches and report_ambiguous (R2): cmd
    ! matches iff it is at least the required prefix, at most the full spelling,
    ! and equals that leading slice of full.  Uses the finalize-cached full,
    ! so it allocates nothing.
    pure function command_matches(c, cmd) result(ok)
        type(command_t), intent(in)  :: c
        character(len=*), intent(in) :: cmd
        logical                      :: ok
        integer                      :: clen
        clen = len(cmd)
        ok = clen >= len(c%req) .and. clen <= len(c%full)
        if (ok) ok = c%full(1:clen) == cmd
    end function command_matches

    subroutine find_matches(this, cmd, n_matches, match_idx)
        class(engine_t), intent(in)              :: this
        character(len=*), intent(in)             :: cmd
        integer, intent(out)                     :: n_matches, match_idx
        integer                                  :: i

        n_matches = 0
        match_idx = 0
        associate (cmds => this%states(this%stack(this%stack_top)%state_idx)%commands)
            do i = 1, size(cmds)
                if (command_matches(cmds(i), cmd)) then
                    n_matches = n_matches + 1
                    match_idx = i
                end if
            end do
        end associate
    end subroutine find_matches

    ! Invoke the matched command's proc and emit any error message.  Fetches
    ! the context from the CURRENT top frame itself (the ordering contract:
    ! ctx is read at invoke time, before any stack mutation).  Returns .true.
    ! iff the action errored; `r` carries the result to the caller.
    function invoke_proc(this, sidx, cmd_idx, args, r) result(errored)
        class(engine_t), intent(inout)           :: this
        integer, intent(in)                      :: sidx, cmd_idx
        type(dlist_t), intent(in)                :: args
        type(action_result_t), intent(out)       :: r
        logical                                  :: errored
        character(len=:), allocatable            :: ctx

        ctx = this%stack(this%stack_top)%context
        r = this%states(sidx)%commands(cmd_idx)%proc(args, ctx)
        errored = r%errored
        if (errored) then
            if (allocated(r%errmsg)) then
                if (len(r%errmsg) > 0) call emit_error(this, r%errmsg)
            end if
        end if
    end function invoke_proc

    ! Enter state `tidx` with context `ctx`.  When `replace` is true the current
    ! top frame is popped first (swap: pop-then-push); otherwise the frame is
    ! pushed on top (goto).  Decrement-before-push and the on_enter-after-push
    ! ordering are observable through on_enter hooks and must not change.
    subroutine transition(this, tidx, ctx, replace)
        class(engine_t), intent(inout)           :: this
        integer, intent(in)                      :: tidx
        character(len=*), intent(in)             :: ctx
        logical, intent(in)                      :: replace
        if (replace) this%stack_top = this%stack_top - 1
        call push_stack(this, tidx, ctx)
        call fire_on_enter(this)
    end subroutine transition

    function apply_edge(this, cmd_idx, args) result(rc)
        class(engine_t), intent(inout)           :: this
        integer, intent(in)                      :: cmd_idx
        type(dlist_t), intent(in)                :: args
        integer                                  :: rc
        integer                                  :: sidx
        type(action_result_t)                    :: r

        sidx = this%stack(this%stack_top)%state_idx
        associate (cmd => this%states(sidx)%commands(cmd_idx))
        select case (cmd%kind)
        case (EDGE_ACTION)
            if (invoke_proc(this, sidx, cmd_idx, args, r)) then
                rc = RC_ERROR
            else
                rc = RC_OK
            end if
        case (EDGE_GOTO)
            call transition(this, cmd%target_idx, "", replace=.false.)
            rc = RC_TRANSITIONED
        case (EDGE_DO_GOTO)
            if (invoke_proc(this, sidx, cmd_idx, args, r)) then
                rc = RC_ERROR
            else if (allocated(r%value)) then
                if (len(r%value) > 0) then
                    call transition(this, cmd%target_idx, r%value, replace=.false.)
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
            if (invoke_proc(this, sidx, cmd_idx, args, r)) then
                rc = RC_ERROR
            else
                this%stack_top = this%stack_top - 1
                if (this%stack_top <= 0) then
                    rc = RC_EXITED
                else
                    rc = RC_TRANSITIONED
                end if
            end if
        case (EDGE_SWAP)
            ! Replace the top frame: pop-then-push, empty context.
            call transition(this, cmd%target_idx, "", replace=.true.)
            rc = RC_TRANSITIONED
        case (EDGE_DO_SWAP)
            if (invoke_proc(this, sidx, cmd_idx, args, r)) then
                rc = RC_ERROR
            else if (allocated(r%value)) then
                if (len(r%value) > 0) then
                    ! Non-empty return replaces the top frame with that value as context.
                    call transition(this, cmd%target_idx, r%value, replace=.true.)
                    rc = RC_TRANSITIONED
                else
                    rc = RC_OK
                end if
            else
                rc = RC_OK
            end if
        case (EDGE_QUIT)
            this%stack_top = 0
            rc = RC_EXITED
        case default
            rc = RC_ERROR
        end select
        end associate
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

    ! Spelling of an edge kind for build diagnostics; empty for an unknown kind
    ! (add_command uses that as its known-kind test).  Mirror of arg_kind_name.
    pure function edge_kind_name(k) result(nm)
        integer, intent(in)           :: k
        character(len=:), allocatable :: nm
        select case (k)
        case (EDGE_ACTION);  nm = "action"
        case (EDGE_GOTO);    nm = "goto"
        case (EDGE_SWAP);    nm = "swap"
        case (EDGE_DO_GOTO); nm = "do_goto"
        case (EDGE_DO_SWAP); nm = "do_swap"
        case (EDGE_DO_POP);  nm = "do_pop"
        case (EDGE_POP);     nm = "pop"
        case (EDGE_QUIT);    nm = "quit"
        case default;        nm = ""
        end select
    end function edge_kind_name

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
                    label = label // " [" // c%args(i)%name // ":" // &
                            arg_kind_name(c%args(i)%kind) // "]"
                else
                    label = label // " <" // c%args(i)%name // ":" // &
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
        integer                                  :: sidx, i
        character(len=:), allocatable            :: msg, sep
        logical                                  :: first

        sidx  = this%stack(this%stack_top)%state_idx
        msg   = "ambiguous: " // cmd // " matches"
        first = .true.
        associate (cmds => this%states(sidx)%commands)
            do i = 1, size(cmds)
                if (command_matches(cmds(i), cmd)) then
                    ! Canonical wording (matches Tcl / C++): the first match is
                    ! introduced by a space after "matches"; subsequent matches
                    ! are joined with ", ".
                    if (first) then
                        sep   = " "
                        first = .false.
                    else
                        sep = ", "
                    end if
                    msg = msg // sep // cmds(i)%spec
                end if
            end do
        end associate
        call emit_error(this, msg)
    end subroutine report_ambiguous

    ! ===== Input encoding =====

    pure module subroutine strip_bom(text, enc)
        character(len=:), allocatable, intent(inout) :: text
        integer,                       intent(out)   :: enc
        integer                                      :: i, n

        enc = ENC_PLAIN
        if (.not. allocated(text)) return

        signature: do i = 1, size(SIGNATURES)
            n = SIGNATURES(i)%nbytes
            if (len(text) < n) cycle signature
            if (text(1:n) == SIGNATURES(i)%bytes(1:n)) then
                enc = SIGNATURES(i)%enc
                if (enc == ENC_UTF8_BOM) text = text(n + 1:)
                exit signature
            end if
        end do signature

        ! A NUL is never part of a command, so in text with no signature (or
        ! behind a UTF-8 mark) it means a wide encoding with no mark.
        if (encoding_supported(enc)) then
            if (index(text, NUL) > 0) enc = ENC_WIDE_NO_BOM
        end if
    end subroutine strip_bom

    pure module function encoding_name(enc) result(name)
        integer, intent(in)           :: enc
        character(len=:), allocatable :: name

        select case (enc)
        case (ENC_PLAIN);       name = "ASCII / UTF-8"
        case (ENC_UTF8_BOM);    name = "UTF-8 with a byte-order mark"
        case (ENC_UTF16_LE);    name = "UTF-16 / UCS-2, little-endian"
        case (ENC_UTF16_BE);    name = "UTF-16 / UCS-2, big-endian"
        case (ENC_UTF32_LE);    name = "UTF-32 / UCS-4, little-endian"
        case (ENC_UTF32_BE);    name = "UTF-32 / UCS-4, big-endian"
        case (ENC_UTF7);        name = "UTF-7"
        case (ENC_UTF1);        name = "UTF-1"
        case (ENC_UTF_EBCDIC);  name = "UTF-EBCDIC"
        case (ENC_SCSU);        name = "SCSU"
        case (ENC_BOCU1);       name = "BOCU-1"
        case (ENC_GB18030);     name = "GB18030"
        case (ENC_WIDE_NO_BOM); name = "NUL bytes; probably UTF-16 / UCS-2 or UTF-32 / UCS-4 " // &
                                       "without a byte-order mark"
        case default;           name = "unknown"
        end select
    end function encoding_name

    pure elemental module function encoding_supported(enc) result(ok)
        integer, intent(in) :: enc
        logical             :: ok

        ok = (enc == ENC_PLAIN) .or. (enc == ENC_UTF8_BOM)
    end function encoding_supported

    ! Classify one line read by run / run_file.  The first line of a stream
    ! is tested for a signature (and loses a UTF-8 mark); every line is
    ! tested for NUL bytes, which catch a wide encoding that has no mark.
    pure subroutine screen_line(text, first, enc)
        character(len=:), allocatable, intent(inout) :: text
        logical,                       intent(in)    :: first
        integer,                       intent(out)   :: enc

        if (first) then
            call strip_bom(text, enc)
        else
            enc = merge(ENC_WIDE_NO_BOM, ENC_PLAIN, index(text, NUL) > 0)
        end if
    end subroutine screen_line

    ! The one wording used for a refused encoding (parity contract).
    pure function unsupported_message(enc) result(msg)
        integer, intent(in)           :: enc
        character(len=:), allocatable :: msg

        msg = "unsupported encoding (" // encoding_name(enc) // "); expected ASCII or UTF-8"
    end function unsupported_message

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
    ! pop/do_pop/swap/do_swap are exempt (return/replace paths, not forward
    ! tree-edges). Sets found=.true. and emits a "A -> B -> ... -> A" message
    ! on first cycle.
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
        integer                                  :: j, vidx, ekind

        color(u) = 1
        do j = 1, size(this%states(u)%commands)
            ekind = this%states(u)%commands(j)%kind
            if (ekind /= EDGE_GOTO .and. ekind /= EDGE_DO_GOTO) cycle  ! swap/pop/quit exempt
            vidx = this%states(u)%commands(j)%target_idx
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
            msg = "missing required argument <" // spec(n_args + 1)%name // ">"
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
                    msg = "argument <" // spec(i)%name // "> expects integer"
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
                    msg = "argument <" // spec(i)%name // "> expects real"
                    ok  = .false.
                    return
                end select
            case (ARG_CHAR)
                select type (node)
                type is (dlist_node_char)
                    continue
                class default
                    msg = "argument <" // spec(i)%name // "> expects string"
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
                call args%replace(i, real_node(real(ival, dp)))
            end select
        end do
    end subroutine normalise_int_to_real

end submodule cmdgraph_sm

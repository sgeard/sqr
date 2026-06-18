!! SPDX-License-Identifier: MIT
!! Copyright (c) 2026 Simon Geard
!! Vendored into sqr from https://github.com/sgeard/cmdgraph (fortran/src); kept in sync by hand.
!!
!! State-graph driven command interpreter — Fortran implementation.
!!
!! Build the graph with `add_state` / `add_command` / `add_include` /
!! `set_on_enter`, call `finalize`, then `dispatch` or `run`.  The graph is
!! immutable after `finalize`; all introspection results are safe to cache
!! keyed by the state name.
!!
!! Construction errors are reported via optional `stat`/`errmsg` out arguments
!! (idiomatic Fortran library style — no `error stop` in library code).  A
!! sticky build-error flag on `engine_t` means a single error check on
!! `finalize` is sufficient: earlier failures propagate silently until then.

module cmdgraph
    use dlist
    use iso_fortran_env, only: stdin_unit => input_unit, stdout_unit => output_unit, stderr_unit => error_unit
    implicit none
    private

    public :: engine_t
    public :: action_result_t
    public :: arg_spec_t
    public :: command_info_t
    public :: action_fun, on_enter_fun
    public :: action_ok, action_error
    public :: arg_is_int, arg_is_real, arg_is_char, arg_is_rest
    public :: arg_int_n, arg_real_n
    public :: EDGE_ACTION, EDGE_GOTO, EDGE_DO_GOTO, EDGE_POP, EDGE_DO_POP, EDGE_QUIT
    public :: RC_OK, RC_UNKNOWN, RC_AMBIGUOUS, RC_TRANSITIONED, RC_EXITED, RC_ERROR
    public :: ARG_INT, ARG_REAL, ARG_CHAR, ARG_REST
    public :: QUIET_UNIT
    public :: version_t, CMDGRAPH_VERSION

    !! Library version.  `major` and `minor` are synchronised across all
    !! implementations; `patch` may differ per implementation.
    type :: version_t
        integer :: major = 0  !! Breaking API change
        integer :: minor = 0  !! Backwards-compatible feature addition
        integer :: patch = 0  !! Bug fix
    contains
        procedure :: string => version_t_string
    end type version_t

    !! Compile-time library version constant.
    type(version_t), parameter :: CMDGRAPH_VERSION = version_t(1, 1, 0)

    interface
        module function version_t_string(this) result(s)
            class(version_t), intent(in)   :: this
            character(len=:), allocatable  :: s
        end function version_t_string
    end interface

    integer, parameter :: ARG_INT  = 1  !! Integer token
    integer, parameter :: ARG_REAL = 2  !! Floating-point token (accepts d/D Fortran-style exponent)
    integer, parameter :: ARG_CHAR = 3  !! Word or double-quoted string (quotes stripped)
    !! Verbatim remainder of the line — no tokenising or unquoting.  Must be the last spec slot.
    integer, parameter :: ARG_REST = 4

    ! Command-line token delimiter. Single printable character by deliberate
    ! choice: tab was removed (non-printable delimiters are undiagnosable).
    ! Private to this module, visible to cmdgraph_sm through host association.
    character(len=*), parameter :: ARG_DELIMITERS = " "

    integer, parameter :: DEFAULT_INPUT_UNIT  = stdin_unit
    integer, parameter :: DEFAULT_OUTPUT_UNIT = stdout_unit
    integer, parameter :: DEFAULT_ERROR_UNIT  = stderr_unit

    !! Sentinel passed to `set_io_units` to suppress an output or error stream.
    !! `-1` is safe: the standard guarantees `open(newunit=)` returns a negative
    !! value that is never `-1`, so a real file unit cannot collide with this sentinel.
    integer, parameter :: QUIET_UNIT = -1

    integer, parameter :: EDGE_ACTION  = 1  !! Invoke proc; stay in current state
    integer, parameter :: EDGE_GOTO    = 2  !! Push target state with empty context (no proc)
    integer, parameter :: EDGE_DO_GOTO = 3  !! Invoke proc; non-empty return value pushes target with that value as context
    integer, parameter :: EDGE_POP     = 4  !! Pop the stack — the canonical back/esc path (no proc)
    integer, parameter :: EDGE_DO_POP  = 5  !! Invoke proc, then pop on success (commit-and-return)
    integer, parameter :: EDGE_QUIT    = 6  !! Exit the engine

    integer, parameter :: RC_OK           = 0  !! Action ran, DoGoto stayed, help shown, or blank line
    integer, parameter :: RC_UNKNOWN      = 1  !! No command matched the input
    integer, parameter :: RC_AMBIGUOUS    = 2  !! Multiple commands matched — input was a common prefix
    integer, parameter :: RC_TRANSITIONED = 3  !! State changed: Goto pushed, DoGoto succeeded, or Pop returned
    integer, parameter :: RC_EXITED       = 4  !! Engine stopped: Quit, stack exhausted, or already dead
    integer, parameter :: RC_ERROR        = 5  !! Action returned an error, or arg validation failed

    !! Return value from an action proc.  Use `action_ok` / `action_error`
    !! rather than direct initialisation.
    type :: action_result_t
        logical                       :: errored = .false.  !! Set `.true.` to report an error
        character(len=:), allocatable :: value    !! DoGoto context; unallocated or `""` means no transition
        character(len=:), allocatable :: errmsg   !! Written to error channel when `errored`; omit to leave `last_error` unchanged
    end type action_result_t

    !! Declarative argument specification attached to a command.
    !! Preferred over direct initialisation — use the `arg_is_int` /
    !! `arg_is_real` / `arg_is_char` / `arg_is_rest` constructor functions.
    !! When a command has an `args=` spec the engine validates count and types
    !! before invoking the action; failure short-circuits with `RC_ERROR`.
    type :: arg_spec_t
        character(len=:), allocatable :: name              !! Displayed in help: `<name:kind>` or `[name:kind]`
        integer                       :: kind     = 0      !! `ARG_INT`, `ARG_REAL`, `ARG_CHAR`, or `ARG_REST`
        logical                       :: optional = .false. !! Trailing optionals may be omitted; positional only
    end type arg_spec_t

    abstract interface
        !! Signature of an action or gate procedure.
        function action_fun(args, ctx) result(rv)
            import :: dlist_t, action_result_t
            type(dlist_t), intent(in)    :: args  !! Parsed, validated argument list
            character(len=*), intent(in) :: ctx   !! Context string of the current state (set by the preceding DoGoto)
            type(action_result_t)        :: rv
        end function action_fun

        !! Signature of an on_enter hook.  Called after every successful state transition.
        subroutine on_enter_fun(ctx)
            character(len=*), intent(in) :: ctx  !! Context string of the newly entered state
        end subroutine on_enter_fun
    end interface

    ! Each command in a state: spec is the original "s(elect)" form, req is
    ! the required prefix ("s"), opt is the optional completion ("elect").
    type :: command_t
        character(len=:), allocatable                    :: spec
        character(len=:), allocatable                    :: req
        character(len=:), allocatable                    :: opt
        integer                                          :: kind = EDGE_ACTION
        character(len=:), allocatable                    :: target
        procedure(action_fun), pointer, nopass           :: proc => null()
        character(len=:), allocatable                    :: help
        type(arg_spec_t),      allocatable               :: args(:)
    end type command_t

    !! Read-only description of one command in the current state, returned by
    !! `engine_t%available_commands`.  The result depends only on the state name;
    !! safe to cache keyed by state name after `finalize`.
    type :: command_info_t
        character(len=:), allocatable :: spec    !! Full spec string, e.g. `p(airs)`
        character(len=:), allocatable :: req     !! Required prefix, e.g. `p`
        character(len=:), allocatable :: opt     !! Optional suffix, e.g. `airs`
        integer                       :: kind = EDGE_ACTION  !! `EDGE_*` constant
        character(len=:), allocatable :: target  !! Destination state for Goto / DoGoto
        type(arg_spec_t), allocatable :: args(:)
        character(len=:), allocatable :: help
    end type command_info_t

    ! A state. `prompt` unallocated = abstract state (include-only).
    type :: state_t
        character(len=:), allocatable                    :: name
        character(len=:), allocatable                    :: prompt
        procedure(on_enter_fun), pointer, nopass         :: on_enter => null()
        character(len=:), allocatable                    :: includes(:)
        type(command_t), allocatable                     :: commands(:)
        ! Number of commands added so far; commands(:) may be over-allocated
        ! during construction (capacity-doubling).  finalize trims to exact count.
        integer                                          :: build_count = 0
    end type state_t

    ! Runtime stack entry: index into engine's states array + opaque context.
    type :: stack_entry_t
        integer                                          :: state_idx = 0
        character(len=:), allocatable                    :: context
    end type stack_entry_t

    !! State-graph driven command interpreter.
    !!
    !! Typical usage:
    !!```fortran
    !! type(engine_t) :: eng
    !! call eng%add_state("root", prompt="> ")
    !! call eng%add_command("root", "q(uit)", EDGE_QUIT, help="exit")
    !! call eng%finalize("root")
    !! call eng%run()
    !!```
    !!
    !! Construct, populate, finalize, then run — in that order.
    !! Calling `dispatch` or `run` before `finalize` is undefined behaviour.
    type :: engine_t
        private
        type(state_t),       allocatable, public         :: states(:)
        type(stack_entry_t), allocatable                 :: stack(:)
        integer                                          :: stack_top = 0
        integer                                          :: initial_state_idx = 0
        logical                                          :: finalized = .false.
        integer,                          public         :: build_error_stat = 0
        character(len=:), allocatable,    public         :: build_error_msg
        integer                                          :: input_unit = DEFAULT_INPUT_UNIT
        integer                                          :: output_unit = DEFAULT_OUTPUT_UNIT
        integer                                          :: error_unit = DEFAULT_ERROR_UNIT
        character(len=:), allocatable,    public         :: last_message  !! Last info/prompt string written to the output channel
        character(len=:), allocatable,    public         :: last_error    !! Last error string written to the error channel
    contains
        procedure, public :: add_state       => add_state_engine    !! Add a concrete or abstract state
        procedure, public :: add_command     => add_command_engine   !! Add a command edge to a state
        procedure, public :: add_include     => add_include_engine   !! Merge commands from an abstract state
        procedure, public :: set_on_enter    => set_on_enter_engine  !! Register a post-transition hook
        procedure, public :: finalize        => finalize_engine      !! Validate the graph and set initial state

        procedure, public :: run             => run_engine           !! Run an interactive loop on the configured input stream
        procedure, public :: run_file        => run_file_engine      !! Execute commands from a file, stopping at first error
        procedure, public :: reset           => reset_engine         !! Rewind to initial state without rebuilding the graph
        procedure, public :: dispatch        => dispatch_engine      !! Dispatch one line of input
        procedure, public :: set_io_units    => set_io_units_engine  !! Redirect I/O channels; pass `QUIET_UNIT` to suppress

        procedure, public :: current_state      => current_state_engine      !! Name of the active state
        procedure, public :: current_context    => current_context_engine    !! Context of the active state
        procedure, public :: is_running         => is_running_engine         !! False after Quit or stack exhausted
        procedure, public :: available_commands => available_commands_engine  !! Commands visible in the current state
        procedure, public :: state_path         => state_path_engine         !! State names on the stack, bottom-first
    end type engine_t

    ! Implementations live in cmdgraph_sm
    interface
        !! Add a concrete state (has a prompt) or abstract state (mix-in, no prompt).
        module subroutine add_state_engine(this, name, prompt, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: name    !! Unique state identifier
            character(len=*), intent(in), optional               :: prompt  !! Displayed before each input line; omit for an abstract state
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine add_state_engine

        !! Add a command edge to `state`.
        module subroutine add_command_engine(this, state, spec, kind, target, proc, help, args, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: state   !! Owning state name
            character(len=*), intent(in)                         :: spec    !! Command spec, e.g. `p(airs)` — required prefix + optional suffix
            integer, intent(in)                                  :: kind    !! Edge behaviour (`EDGE_*` constant)
            character(len=*), intent(in), optional               :: target  !! Destination state (Goto / DoGoto)
            procedure(action_fun), optional                      :: proc    !! Action / gate proc (Action, DoGoto, DoPop)
            character(len=*), intent(in), optional               :: help    !! Shown by built-in help command
            type(arg_spec_t), intent(in), optional               :: args(:) !! Validated before proc is called
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine add_command_engine

        !! Build an integer `arg_spec_t`.
        module function arg_is_int(name, optional) result(s)
            character(len=*), intent(in)            :: name
            logical, intent(in), optional           :: optional
            type(arg_spec_t)                        :: s
        end function arg_is_int

        !! Build a real `arg_spec_t`.
        module function arg_is_real(name, optional) result(s)
            character(len=*), intent(in)            :: name
            logical, intent(in), optional           :: optional
            type(arg_spec_t)                        :: s
        end function arg_is_real

        !! Build a character `arg_spec_t`.
        module function arg_is_char(name, optional) result(s)
            character(len=*), intent(in)            :: name
            logical, intent(in), optional           :: optional
            type(arg_spec_t)                        :: s
        end function arg_is_char

        !! Build a rest `arg_spec_t` (verbatim remainder of the line; must be last).
        module function arg_is_rest(name, optional) result(s)
            character(len=*), intent(in)            :: name
            logical, intent(in), optional           :: optional
            type(arg_spec_t)                        :: s
        end function arg_is_rest

        !! Return `n` copies of an integer spec (fixed-size tuple, e.g. a 2D point).
        module function arg_int_n(name, n) result(s)
            character(len=*), intent(in)            :: name
            integer, intent(in)                     :: n
            type(arg_spec_t), allocatable           :: s(:)
        end function arg_int_n

        !! Return `n` copies of a real spec.
        module function arg_real_n(name, n) result(s)
            character(len=*), intent(in)            :: name
            integer, intent(in)                     :: n
            type(arg_spec_t), allocatable           :: s(:)
        end function arg_real_n

        !! Successful result, optionally carrying a DoGoto context string.
        module function action_ok(ctx) result(rv)
            character(len=*), intent(in), optional  :: ctx
            type(action_result_t)                   :: rv
        end function action_ok

        !! Error result, optionally with a message written to the error channel.
        module function action_error(msg) result(rv)
            character(len=*), intent(in), optional  :: msg
            type(action_result_t)                   :: rv
        end function action_error

        !! Merge all commands from abstract state `included` into `state`.
        !! The state's own commands override included ones with the same spec.
        module subroutine add_include_engine(this, state, included, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: state
            character(len=*), intent(in)                         :: included
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine add_include_engine

        !! Register a hook called after every successful transition into `state`.
        module subroutine set_on_enter_engine(this, state, proc, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: state
            procedure(on_enter_fun)                              :: proc
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine set_on_enter_engine

        !! Validate the graph and set the initial state.
        !! Sets `stat` non-zero if a cycle is detected in goto/do_goto edges
        !! or if any state reference is unresolved.
        module subroutine finalize_engine(this, initial, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: initial  !! Name of the starting state
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine finalize_engine

        !! Run an interactive loop reading from the configured input stream.
        module subroutine run_engine(this)
            class(engine_t), intent(inout)         :: this
        end subroutine run_engine

        !! Execute commands from `path`, stopping at the first error.
        !! Returns `.true.` if the file ran to completion (or ended with quit).
        module function run_file_engine(this, path, echo, stat, errmsg, line) result(ok)
            class(engine_t), intent(inout)                       :: this
            character(len=*), intent(in)                         :: path
            logical, intent(in), optional                        :: echo    !! If `.true.`, print each prompt+line as it is read
            integer, intent(out), optional                       :: stat    !! RC of the failing dispatch, or `RC_OK`
            character(len=:), allocatable, intent(out), optional :: errmsg
            integer, intent(out), optional                       :: line    !! 1-based line number of the failure
            logical                                              :: ok
        end function run_file_engine

        !! Rewind to the initial state without rebuilding the graph.
        !! Clears the stack, contexts, `last_message`, and `last_error`.
        module subroutine reset_engine(this, stat, errmsg)
            class(engine_t), intent(inout)                       :: this
            integer, intent(out), optional                       :: stat
            character(len=:), allocatable, intent(out), optional :: errmsg
        end subroutine reset_engine

        !! Redirect I/O channels.  Pass `QUIET_UNIT` to suppress a channel.
        !! Default: `stdin` / `stdout` / `stderr`.
        module subroutine set_io_units_engine(this, input_unit, output_unit, error_unit)
            class(engine_t), intent(inout)         :: this
            integer, intent(in), optional          :: input_unit
            integer, intent(in), optional          :: output_unit
            integer, intent(in), optional          :: error_unit
        end subroutine set_io_units_engine

        !! Dispatch one line of input.
        !! Performs prefix matching, arg validation, and edge traversal.
        module function dispatch_engine(this, line) result(rc)
            class(engine_t), intent(inout)         :: this
            character(len=*), intent(in)           :: line
            integer                                :: rc
        end function dispatch_engine

        !! Name of the active state.
        module function current_state_engine(this) result(name)
            class(engine_t), intent(in)            :: this
            character(len=:), allocatable          :: name
        end function current_state_engine

        !! Context of the active state.
        module function current_context_engine(this) result(ctx)
            class(engine_t), intent(in)            :: this
            character(len=:), allocatable          :: ctx
        end function current_context_engine

        !! Returns `.false.` after Quit or stack exhausted.
        module function is_running_engine(this) result(b)
            class(engine_t), intent(in)            :: this
            logical                                :: b
        end function is_running_engine

        !! Commands visible in the current state (own + included, help/? excluded).
        !! Result depends only on the state name; safe to cache keyed by state name.
        module function available_commands_engine(this) result(cmds)
            class(engine_t), intent(in)            :: this
            type(command_info_t), allocatable      :: cmds(:)
        end function available_commands_engine

        !! State names on the stack, bottom-first.
        module function state_path_engine(this) result(names)
            class(engine_t), intent(in)            :: this
            character(len=:), allocatable          :: names(:)
        end function state_path_engine
    end interface

end module cmdgraph

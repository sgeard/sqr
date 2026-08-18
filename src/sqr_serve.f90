!! `sqr_serve` — the sqrd server core: sessions, request dispatch and
!! response encoding for the wire protocol (reports/DESIGN-wire-protocol.md).
!!
!! `sqr_net` frames bytes; this module assigns them meaning: `HELLO` / `SQL`
!! / `TABLES` / `COLUMNS` / `INFO` / `PACK` / `PING` / `QUIT` requests in,
!! `NONE` / `COUNT` / `MSG` / `ERR` / `ROWS` responses out.  Statements execute serially
!! against one open database through `sql_exec` in binary-cell mode, so no
!! engine locking is needed.  The `sqrd` program is only argument parsing
!! around a `serve_step` loop; tests drive `serve_step` directly in-process
!! against loopback client sockets.
!!
!! Transaction contention (v1): the server records which session opened the
!! in-flight transaction; any non-SELECT statement from another session is
!! refused with stat `SQRD_STAT_BUSY` while it stays open, and a session that
!! disconnects holding it is rolled back.  Reads from other sessions are
!! allowed (and see the uncommitted state — one engine handle, one storage).
module sqr_serve
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use :: sqr,     only: db_t
    use :: sqr_net, only: net_conn_t
    implicit none
    private

    integer, parameter, public :: SQRD_PROTOCOL = 1  !! Wire-protocol major version
    character(len=*), parameter, public :: SQRD_VERSION = '1.0.0'  !! Server version advertised in `OK`

    !! `ERR` stat for a write refused because another session's transaction
    !! is open.  Outside the engine's `SQR_*` range; the ODBC driver maps it
    !! to SQLSTATE 40001.
    integer, parameter, public :: SQRD_STAT_BUSY = 100

    integer, parameter, public :: SQRD_MAX_SESSIONS = 16  !! Concurrent-connection cap

    !! One client connection and its protocol state.
    type, public :: session_t
        type(net_conn_t) :: conn  !! Framed connection
        logical :: active  = .false.  !! Slot in use
        logical :: greeted = .false.  !! `HELLO` exchange completed
    end type

    !! One serving instance: the listener, its sessions and the served
    !! database.  POINTER COVENANT: `db` points at the caller's handle — the
    !! caller keeps it open and in place for the server's lifetime.
    type, public :: server_t
        type(db_t), pointer :: db => null()  !! Served database (caller-owned)
        integer(c_int64_t)  :: lsock = -1    !! Listening socket
        integer             :: port  = 0     !! Actual bound port
        type(session_t)     :: sessions(SQRD_MAX_SESSIONS)  !! Session slots
        integer             :: txn_owner = 0  !! Session slot holding the open transaction (0 = none)
        character(len=:), allocatable :: dbname  !! Name advertised in `OK` (db dir basename)
        !! The database directory as the caller named it, made absolute but
        !! NOT resolved — a symlink here is usually the name that matters
        !! (`db/current` pointing at a dated directory), and resolving it
        !! would report something the caller never chose.  `INFO`'s `dir`.
        character(len=:), allocatable :: dbpath
        !! The same directory with every symlink resolved: identity rather
        !! than name, for anything that must decide whether two paths are the
        !! same database.  `INFO`'s `realdir`, reported only when it differs.
        character(len=:), allocatable :: dbreal
    end type

    public :: serve_open   !! bind the loopback listener for a database
    public :: serve_close  !! drop every session and stop listening
    public :: serve_step   !! one poll cycle: accept + service ready sessions

    interface
        !! Start serving `db`: bind a loopback listener on `port` (0 asks
        !! for an ephemeral port; the actual choice lands in `srv%port`).
        !! `db` must already be open, and must outlive `srv` (see the
        !! pointer covenant on `server_t`).
        module subroutine serve_open(srv, db, port, stat, errmsg)
            type(server_t),     intent(out)   :: srv   !! Server to initialise
            type(db_t), target, intent(inout) :: db    !! Open database to serve
            integer,            intent(in)    :: port  !! Port to bind (0 = ephemeral)
            integer,            intent(out)   :: stat  !! `SQR_OK` or `SQR_ERR`
            character(len=*),   intent(inout), optional :: errmsg  !! Failure detail
        end subroutine

        !! Drop every session (rolling back an open transaction) and close
        !! the listener.  Safe on a server that never opened.
        module subroutine serve_close(srv)
            type(server_t), intent(inout) :: srv  !! Server to shut down
        end subroutine

        !! One poll cycle: wait up to `timeout_ms` for connection attempts
        !! or requests, accept and service them (one request per ready
        !! session), and report the number of events handled (0 = idle
        !! timeout).  The serving loop is simply `serve_step` forever.
        module subroutine serve_step(srv, timeout_ms, nevents)
            type(server_t), intent(inout) :: srv         !! Serving instance
            integer,        intent(in)    :: timeout_ms  !! Poll wait limit (ms)
            integer,        intent(out)   :: nevents     !! Events handled this cycle
        end subroutine
    end interface

end module sqr_serve

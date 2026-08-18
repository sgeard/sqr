!! `sqr_net` — message framing for the sqrd wire protocol
!! (reports/DESIGN-wire-protocol.md).
!!
!! Engine-independent: this module knows sockets and the framing rules — one
!! printable header line terminated by LF, optionally followed immediately by
!! a byte-counted payload — and nothing about SQL, requests or responses
!! (those live in `sqr_serve`).  Both the server and Fortran test clients
!! frame their traffic through it.
!!
!! Each connection buffers received bytes (`net_conn_t` carries the buffer),
!! so a burst holding several messages is consumed message by message.  Calls
!! block until their whole message part has arrived: poll for readability
!! first (`c_sock_poll`), then read — a peer that stalls mid-message stalls
!! its reader, which is acceptable for a loopback protocol.
!!
!! Framing limits are enforced here: a header line over `NET_MAX_LINE` or a
!! payload over `NET_MAX_PAYLOAD` reports `NET_LIMIT`, and the design's
!! response to either is to drop the connection.
module sqr_net
    use, intrinsic :: iso_c_binding, only: c_int64_t
    implicit none
    private

    ! Outcome codes for every fallible operation here.
    integer, parameter, public :: NET_OK    = 0  !! Success
    integer, parameter, public :: NET_EOF   = 1  !! Peer closed the connection
    integer, parameter, public :: NET_ERR   = 2  !! Socket failure
    integer, parameter, public :: NET_LIMIT = 3  !! Framing limit exceeded

    integer, parameter, public :: NET_MAX_LINE    = 1024     !! Header-line cap (bytes, excluding the LF)
    integer, parameter, public :: NET_MAX_PAYLOAD = 1048576  !! Payload cap (bytes, 1 MiB)

    integer, parameter :: NET_BUFSIZE = 65536  ! per-connection receive buffer

    !! One framed connection: the socket plus its receive buffer.  Treat as
    !! opaque; obtain with `net_attach`, release with `net_close`.
    type, public :: net_conn_t
        integer(c_int64_t) :: sock = -1  !! Underlying socket (-1 = closed)
        character(len=NET_BUFSIZE) :: rbuf = ''  !! Buffered received bytes
        integer :: rlo = 1  !! First unconsumed position in `rbuf`
        integer :: rhi = 0  !! Last valid position in `rbuf`
    end type

    public :: net_attach       !! wrap an open socket in a connection
    public :: net_close        !! close the socket and reset the connection
    public :: net_send_line    !! send one header line (LF appended)
    public :: net_send_payload !! send counted payload bytes verbatim
    public :: net_recv_line    !! receive one header line (LF stripped)
    public :: net_recv_payload !! receive exactly n payload bytes

    interface
        !! Wrap an open socket in a fresh connection (empty read buffer).
        !! The connection owns the socket from here: close it only through
        !! `net_close`.
        module subroutine net_attach(conn, sock)
            type(net_conn_t),   intent(out) :: conn  !! Connection to initialise
            integer(c_int64_t), intent(in)  :: sock  !! Open socket to adopt
        end subroutine

        !! Close the underlying socket (a no-op if already closed) and reset
        !! the connection to its initial state.
        module subroutine net_close(conn)
            type(net_conn_t), intent(inout) :: conn  !! Connection to close
        end subroutine

        !! Send one header line: `line` plus the terminating LF.  Returns
        !! `NET_OK` or `NET_ERR`.
        module function net_send_line(conn, line) result(ierr)
            type(net_conn_t), intent(inout) :: conn  !! Connection
            character(len=*), intent(in)    :: line  !! Header line (no LF)
            integer                         :: ierr  !! `NET_OK` / `NET_ERR`
        end function

        !! Send payload bytes verbatim (the byte count travels in the header
        !! line the caller sent first).  Returns `NET_OK` or `NET_ERR`.
        module function net_send_payload(conn, bytes) result(ierr)
            type(net_conn_t), intent(inout) :: conn   !! Connection
            character(len=*), intent(in)    :: bytes  !! Payload bytes
            integer                         :: ierr   !! `NET_OK` / `NET_ERR`
        end function

        !! Receive one header line into `line` (terminating LF stripped; a
        !! trailing CR is stripped too, so telnet's CRLF works).  `ierr` is
        !! `NET_OK`, `NET_EOF`, `NET_ERR`, or `NET_LIMIT` for a line over
        !! `NET_MAX_LINE`; `line` stays unallocated except on `NET_OK`.
        module subroutine net_recv_line(conn, line, ierr)
            type(net_conn_t),              intent(inout) :: conn  !! Connection
            character(len=:), allocatable, intent(out)   :: line  !! Received line
            integer,                       intent(out)   :: ierr  !! Outcome code
        end subroutine

        !! Receive exactly `nbytes` payload bytes into `bytes`.  `ierr` is
        !! `NET_OK`, `NET_EOF` (peer closed mid-payload), `NET_ERR`, or
        !! `NET_LIMIT` for a count outside [0, `NET_MAX_PAYLOAD`] — checked
        !! before any byte is read, so an oversized announcement is rejected
        !! without allocating for it.
        module subroutine net_recv_payload(conn, nbytes, bytes, ierr)
            type(net_conn_t),              intent(inout) :: conn    !! Connection
            integer,                       intent(in)    :: nbytes  !! Announced payload size
            character(len=:), allocatable, intent(out)   :: bytes   !! Received payload
            integer,                       intent(out)   :: ierr    !! Outcome code
        end subroutine
    end interface

end module sqr_net

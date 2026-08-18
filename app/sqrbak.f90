!! `sqrbak` — ask a running `sqrd` where its database is, and tell it to write
!! a backup container (reports/DESIGN-wire-protocol.md).
!!
!! Usage:
!!   `sqrbak info   [<host>:]<port>`          report the served database
!!   `sqrbak backup [<host>:]<port> <file>`   write a `.sqr` container
!!
!! `<host>` defaults to 127.0.0.1 and must be a numeric IPv4 address (the
!! socket shim does no name resolution).
!!
!! The point of `info` is `dir =`: a client connects to a host and a port, and
!! nothing else on the wire says which directory on disk that is.  A backup
!! script reads it to know what it is protecting; a person reads it to find
!! out which of several sqrd instances they are talking to.
!!
!! `backup` makes `<file>` absolute against THIS process's working directory
!! before sending it, because the server insists on an absolute path — its own
!! working directory is not the one the user typed the name in.  The server
!! takes the snapshot without closing the database (see `db_pack_live`), so
!! there is no window in which the database is unavailable and no other
!! process can take the lock meanwhile.
!!
!! Exit status is 0 on success and 1 on any failure, so it drops straight into
!! a backup script.
program sqrbak
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use, intrinsic :: iso_c_binding,   only: c_int64_t
    use :: sqr_net
    use :: clib_wrap, only: c_sock_connect, c_sock_close, c_abspath, c_exit
    implicit none

    character(len=*), parameter :: DEFAULT_HOST = '127.0.0.1'

    type(net_conn_t) :: conn
    character(len=4096) :: arg
    character(len=:), allocatable :: cmd, host, file, payload
    integer(c_int64_t) :: sock
    integer :: port, nargs

    nargs = command_argument_count()
    if (nargs < 2) call die('usage: sqrbak info   [<host>:]<port>' // new_line('a') // &
                            '       sqrbak backup [<host>:]<port> <file>')
    call get_command_argument(1, arg)
    cmd = trim(arg)
    call get_command_argument(2, arg)
    call parse_endpoint(trim(arg), host, port)

    select case (cmd)
    case ('info')
        if (nargs /= 2) call die('usage: sqrbak info [<host>:]<port>')
        call connect_and_greet(host, port)
        call request('INFO', '', payload)
        write(output_unit, '(a)', advance='no') payload
    case ('backup')
        if (nargs /= 3) call die('usage: sqrbak backup [<host>:]<port> <file>')
        call get_command_argument(3, arg)
        ! Resolve against OUR working directory: the server refuses a relative
        ! path precisely because it would mean something different there.
        file = c_abspath(trim(arg))
        if (len(file) == 0) call die('sqrbak: cannot resolve destination: ' // trim(arg))
        call connect_and_greet(host, port)
        call request('PACK', file, payload)
        write(output_unit, '(a)') payload
    case default
        call die('sqrbak: unknown command "' // cmd // '" (info | backup)')
    end select

    call finish()

contains

    !! Report `msg` on stderr and exit non-zero.  Any socket is already
    !! doomed by the exit, so there is nothing to unwind.  c_exit rather
    !! than `stop 1`: the diagnostic above is the whole message, and ifx
    !! would otherwise print the stop code after it.
    subroutine die(msg)
        character(len=*), intent(in) :: msg
        write(error_unit, '(a)') msg
        flush(error_unit)
        call c_exit(1)
    end subroutine

    !! Split `[<host>:]<port>`, defaulting the host.  A bad port is fatal.
    subroutine parse_endpoint(spec, host, port)
        character(len=*),              intent(in)  :: spec
        character(len=:), allocatable, intent(out) :: host
        integer,                       intent(out) :: port
        integer :: c, ios
        c = index(spec, ':', back=.true.)
        if (c == 0) then
            host = DEFAULT_HOST
            read(spec, *, iostat=ios) port
        else
            host = spec(1:c - 1)
            if (len(host) == 0) host = DEFAULT_HOST
            read(spec(c + 1:), *, iostat=ios) port
        end if
        if (ios /= 0 .or. port <= 0 .or. port > 65535) &
            call die('sqrbak: bad endpoint "' // spec // '" (want [<host>:]<port>)')
    end subroutine

    !! Connect and complete the HELLO exchange, or die trying.  The
    !! connection and its socket are the program's, reached by host
    !! association rather than passed in — an internal procedure may not
    !! alias what it already sees.
    subroutine connect_and_greet(host, port)
        character(len=*), intent(in) :: host
        integer,          intent(in) :: port
        character(len=:), allocatable :: line
        integer :: ne
        sock = c_sock_connect(host, port)
        if (sock < 0) call die('sqrbak: cannot connect to ' // host // ':' // itoa(port))
        call net_attach(conn, sock)
        if (net_send_line(conn, 'HELLO 1 sqrbak') /= NET_OK) call die('sqrbak: send failed')
        call net_recv_line(conn, line, ne)
        if (ne /= NET_OK) call die('sqrbak: no greeting from the server')
        if (line(1:min(3, len(line))) /= 'OK ') call die('sqrbak: server refused: ' // line)
    end subroutine

    !! Send one request — with `body` as its counted payload when non-empty —
    !! and return the payload of the `MSG` response.  An `ERR` response, or
    !! any other reply, is fatal.
    subroutine request(verb, body, payload)
        character(len=*),              intent(in)  :: verb, body
        character(len=:), allocatable, intent(out) :: payload
        character(len=:), allocatable :: line, tail
        integer :: nb, ne, sp, ios
        if (len(body) > 0) then
            if (net_send_line(conn, verb // ' ' // itoa(len(body))) /= NET_OK) &
                call die('sqrbak: send failed')
            if (net_send_payload(conn, body) /= NET_OK) call die('sqrbak: send failed')
        else
            if (net_send_line(conn, verb) /= NET_OK) call die('sqrbak: send failed')
        end if
        call net_recv_line(conn, line, ne)
        if (ne /= NET_OK) call die('sqrbak: no response from the server')
        sp = index(line, ' ')
        if (sp == 0) call die('sqrbak: unexpected response: ' // line)
        tail = line(sp + 1:)
        select case (line(1:sp - 1))
        case ('MSG')
            read(tail, *, iostat=ios) nb
            if (ios /= 0) call die('sqrbak: malformed MSG response: ' // line)
            call net_recv_payload(conn, nb, payload, ne)
            if (ne /= NET_OK) call die('sqrbak: truncated response')
        case ('ERR')
            ! ERR <stat> <nbytes>: the text after the code is the useful part.
            sp = index(tail, ' ')
            if (sp == 0) call die('sqrbak: malformed ERR response: ' // line)
            read(tail(sp + 1:), *, iostat=ios) nb
            if (ios /= 0) call die('sqrbak: malformed ERR response: ' // line)
            call net_recv_payload(conn, nb, payload, ne)
            if (ne /= NET_OK) call die('sqrbak: truncated error response')
            call die('sqrbak: ' // payload)
        case default
            call die('sqrbak: unexpected response: ' // line)
        end select
    end subroutine

    !! Say goodbye and close down cleanly.  The server answers QUIT and drops
    !! the session; a failed send here changes nothing, so it is not checked.
    subroutine finish()
        integer :: ne
        ne = net_send_line(conn, 'QUIT')
        call net_close(conn)
        call c_sock_close(sock)
    end subroutine

    pure function itoa(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=12) :: tmp
        write(tmp, '(i0)') v
        s = trim(tmp)
    end function

end program sqrbak

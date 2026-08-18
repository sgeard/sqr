!! `sqrbak` — ask a running `sqrd` where its database is, and tell it to write
!! a backup container (reports/DESIGN-wire-protocol.md).
!!
!! Usage:
!!   `sqrbak info   <db-dir>|[<host>:]<port>`          report the database
!!   `sqrbak backup <db-dir>|[<host>:]<port> <file>`   write a `.sqr` container
!!
!! The endpoint is a **database directory** unless it looks like a port (all
!! digits, optionally after `<host>:`).  Naming the directory is the better
!! habit: it is what you actually care about, it survives a daemon restarting
!! on a different port, and it works through symbolic links for free.  sqrd
!! records its port in `<db-dir>/_sqrd`, which is where the directory form
!! reads it from — and because that file is only a hint (nothing removes it
!! when a daemon dies), the connection is then VERIFIED by asking `INFO`
!! which directory it is really serving.  That one check also catches a
!! recycled port and a reused pid.
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
    use :: clib_wrap, only: c_sock_connect, c_sock_close, c_abspath, c_exit, &
                            c_joincwd, c_realpath, c_path_exists
    implicit none

    character(len=*), parameter :: DEFAULT_HOST = '127.0.0.1'

    type(net_conn_t) :: conn
    character(len=4096) :: arg
    character(len=:), allocatable :: cmd, host, file, payload, endpoint, wanted_dir, info_text
    integer(c_int64_t) :: sock
    integer :: port, nargs

    nargs = command_argument_count()
    if (nargs < 2) call die('usage: sqrbak info   <db-dir>|[<host>:]<port>' // new_line('a') // &
                            '       sqrbak backup <db-dir>|[<host>:]<port> <file>')
    call get_command_argument(1, arg)
    cmd = trim(arg)
    call get_command_argument(2, arg)
    endpoint = trim(arg)
    call resolve_endpoint(endpoint, host, port, wanted_dir)

    select case (cmd)
    case ('info')
        if (nargs /= 2) call die('usage: sqrbak info <db-dir>|[<host>:]<port>')
        call connect_and_greet(host, port)
        write(output_unit, '(a)', advance='no') info_text
    case ('backup')
        if (nargs /= 3) call die('usage: sqrbak backup <db-dir>|[<host>:]<port> <file>')
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

    !! Work out where to connect.  An endpoint that looks like a port (all
    !! digits, optionally after `<host>:`) is taken as one; anything else is
    !! a database directory, whose `_sqrd` file is read for the port.  In the
    !! directory case `wanted_dir` comes back non-empty, and the greeting
    !! verifies that the daemon really is serving it.
    subroutine resolve_endpoint(spec, host, port, wanted_dir)
        character(len=*),              intent(in)  :: spec
        character(len=:), allocatable, intent(out) :: host
        integer,                       intent(out) :: port
        character(len=:), allocatable, intent(out) :: wanted_dir
        character(len=:), allocatable :: portpart
        integer :: c, ios
        wanted_dir = ''
        c = index(spec, ':', back=.true.)
        if (c == 0) then
            host     = DEFAULT_HOST
            portpart = spec
        else
            host     = spec(1:c - 1)
            portpart = spec(c + 1:)
            if (len(host) == 0) host = DEFAULT_HOST
        end if
        if (len_trim(portpart) > 0 .and. verify(trim(portpart), '0123456789') == 0) then
            read(portpart, *, iostat=ios) port
            if (ios /= 0 .or. port <= 0 .or. port > 65535) &
                call die('sqrbak: bad port in "' // spec // '"')
            return
        end if
        host       = DEFAULT_HOST      ! a path may legitimately contain ':'
        wanted_dir = spec
        call read_advert(spec, host, port)
    end subroutine

    !! Read `<dir>/_sqrd`, the file sqrd writes to say where it is listening.
    !! Absent means nothing has served this directory (or the directory is
    !! wrong); present means only that something once did — the caller still
    !! has to verify.
    subroutine read_advert(dir, host, port)
        character(len=*),              intent(in)    :: dir
        character(len=:), allocatable, intent(inout) :: host
        integer,                       intent(out)   :: port
        character(len=:), allocatable :: path, key, val
        character(len=1024) :: line
        integer :: u, ios, eq
        port = 0
        path = dir
        if (len(path) > 0) then
            if (path(len(path):len(path)) /= '/') path = path // '/'
        end if
        path = path // '_sqrd'
        if (.not. c_path_exists(path)) then
            if (.not. c_path_exists(dir)) call die('sqrbak: no such database directory: ' // dir)
            call die('sqrbak: no ' // path // ' — is sqrd running on that database?' &
                // new_line('a') // '       (start it, or give an explicit port)')
        end if
        open(newunit=u, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) call die('sqrbak: cannot read ' // path)
        read_keys: do
            read(u, '(a)', iostat=ios) line
            if (ios /= 0) exit read_keys
            eq = index(line, ' = ')
            if (eq == 0) cycle read_keys
            key = trim(adjustl(line(1:eq - 1)))
            val = trim(adjustl(line(eq + 3:)))
            select case (key)
            case ('port')
                read(val, *, iostat=ios) port
                if (ios /= 0) port = 0
            case ('host')
                if (len(val) > 0) host = val
            end select
        end do read_keys
        close(u)
        if (port <= 0 .or. port > 65535) call die('sqrbak: no usable port in ' // path)
    end subroutine

    !! Confirm the daemon we reached is serving the directory we asked for.
    !! The `_sqrd` file is a hint that nothing cleans up, so a stale one can
    !! point at a port some other daemon has since taken.  Any spelling of
    !! the directory will do — as given, anchored, or fully resolved, against
    !! either `dir` or `realdir` — because a symlink is a legitimate name for
    !! a database, not an error to be corrected.
    subroutine verify_dir(wanted)
        character(len=*), intent(in) :: wanted
        character(len=:), allocatable :: got_dir, got_real, resolved
        got_dir  = info_value('dir')
        got_real = info_value('realdir')
        if (len(got_real) == 0) got_real = got_dir
        if (same_dir(wanted, got_dir, got_real))             return
        if (same_dir(c_joincwd(wanted), got_dir, got_real))  return
        resolved = c_realpath(wanted)
        if (same_dir(resolved, got_dir, got_real))           return
        call die('sqrbak: ' // host // ':' // itoa(port) // ' is serving ' // got_dir &
            // new_line('a') // '       not ' // wanted &
            // new_line('a') // '       (a stale _sqrd file, or the daemon was restarted elsewhere)')
    end subroutine

    !! Does candidate `p` name the same directory as either form the server
    !! reported?  An empty candidate never matches (c_realpath returns '' for
    !! a path it cannot resolve).
    pure function same_dir(p, as_named, resolved) result(yes)
        character(len=*), intent(in) :: p, as_named, resolved
        logical :: yes
        yes = .false.
        if (len(p) == 0) return
        yes = p == as_named .or. p == resolved
    end function

    !! The value of one `key = value` line of the cached INFO payload, or ''.
    !! Walks whole lines rather than searching the text, so `dir` cannot
    !! match inside `realdir`.
    function info_value(key) result(val)
        character(len=*), intent(in)  :: key
        character(len=:), allocatable :: val, line
        integer :: p, e, eq
        val = ''
        p = 1
        scan_lines: do while (p <= len(info_text))
            e = index(info_text(p:), new_line('a'))
            if (e == 0) then
                line = info_text(p:)
                p    = len(info_text) + 1
            else
                line = info_text(p:p + e - 2)
                p    = p + e
            end if
            eq = index(line, ' = ')
            if (eq == 0) cycle scan_lines
            if (line(1:eq - 1) == key) then
                val = line(eq + 3:)
                exit scan_lines
            end if
        end do scan_lines
    end function

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
        ! INFO always, not just for `info`: it is one round trip on loopback,
        ! and it is what turns a port read out of a file into a checked fact.
        call request('INFO', '', info_text)
        if (len(wanted_dir) > 0) call verify_dir(wanted_dir)
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

! Unit tests for the socket shim (clib_wrap c_sock_*) and the wire framing
! layer (sqr_net).  Everything runs single-process over a loopback pair:
! listen on an ephemeral port, connect, accept — the two ends then exchange
! framed messages.  Single-threaded is safe because every exchange is
! send-then-receive with sizes far below the socket buffers.
program utest_net
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use :: clib_wrap
    use :: sqr_net
    implicit none

    integer :: pass = 0, fail = 0
    integer(c_int64_t) :: ls = -1
    ! The connection pair every test attaches afresh.  Program-scope (static)
    ! rather than per-test locals: net_conn_t carries a 64 KiB buffer, which
    ! as a local would draw gfortran's moved-to-static -Wsurprising warning.
    type(net_conn_t) :: a, b   ! a = server end, b = client end

    call t_shim()
    call t_lines()
    call t_payloads()
    call t_large_payload()
    call t_limits()
    call t_eof()

    call c_sock_close(ls)

    print '(a,i0,a,i0,a)', 'net tests: ', pass, ' passed, ', fail, ' failed'
    if (fail > 0) error stop 1

contains

    subroutine check(cond, label)
        logical,          intent(in) :: cond
        character(len=*), intent(in) :: label
        if (cond) then
            pass = pass + 1
            print '(a,a)', '  OK   ', label
        else
            fail = fail + 1
            print '(a,a)', '  FAIL ', label
        end if
    end subroutine

    ! Fresh loopback pair on the shared listener (created on first use).
    subroutine new_pair(a, b)
        type(net_conn_t), intent(out) :: a, b
        integer(c_int64_t) :: sa, sc
        if (ls < 0) ls = c_sock_listen(0)
        sc = c_sock_connect('127.0.0.1', c_sock_port(ls))
        sa = c_sock_accept(ls)
        call net_attach(a, sa)
        call net_attach(b, sc)
    end subroutine

    ! ---- raw shim ----

    subroutine t_shim()
        integer(c_int64_t) :: socks(2)
        logical :: ready(2)
        integer :: nready, ierr
        ls = c_sock_listen(0)
        call check(ls >= 0, 'shim: listen on an ephemeral port')
        call check(c_sock_port(ls) > 0, 'shim: bound port readable')
        call check(c_sock_listen(c_sock_port(ls)) < 0, 'shim: rebinding the same port fails')
        call check(c_sock_connect('127.0.0.1', 1) < 0, 'shim: connect to a dead port fails')
        call new_pair(a, b)
        socks = [a%sock, b%sock]
        call c_sock_poll(socks, 0, ready, nready)
        call check(nready == 0 .and. .not. any(ready), 'shim: poll finds nothing pending')
        ierr = c_sock_send(b%sock, 'x')
        call check(ierr == 0, 'shim: send succeeds')
        call c_sock_poll(socks, 2000, ready, nready)
        call check(nready == 1 .and. ready(1) .and. .not. ready(2), &
                   'shim: poll flags exactly the readable end')
        call net_close(a)
        call net_close(b)
        call check(a%sock == -1_c_int64_t, 'shim: close resets the token')
    end subroutine

    ! ---- header lines ----

    subroutine t_lines()
        character(len=:), allocatable :: line
        integer :: ne
        call new_pair(a, b)
        ne = net_send_line(b, 'HELLO 1 utest')
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'HELLO 1 utest', 'line: round trip')
        ! several lines in one burst are consumed one by one from the buffer
        ne = net_send_line(b, 'one')
        ne = net_send_line(b, 'two')
        ne = net_send_line(b, '')
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'one', 'line: burst, first')
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'two', 'line: burst, second')
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. len(line) == 0, 'line: empty line')
        ! telnet-style CRLF: the CR is stripped
        ne = net_send_payload(b, 'PING' // achar(13) // achar(10))
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'PING', 'line: CRLF tolerated')
        ! the other direction works over the same pair
        ne = net_send_line(a, 'OK')
        call net_recv_line(b, line, ne)
        call check(ne == NET_OK .and. line == 'OK', 'line: reverse direction')
        call net_close(a)
        call net_close(b)
    end subroutine

    ! ---- counted payloads ----

    subroutine t_payloads()
        character(len=:), allocatable :: line, bytes
        character(len=6) :: raw
        integer :: ne
        call new_pair(a, b)
        ! payload bytes are arbitrary: NUL, LF and CR must all pass through
        raw = 'a' // achar(0) // achar(10) // achar(13) // 'z' // char(255)
        ne = net_send_line(b, 'SQL 6')
        ne = net_send_payload(b, raw)
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'SQL 6', 'payload: header first')
        call net_recv_payload(a, 6, bytes, ne)
        call check(ne == NET_OK .and. bytes == raw, 'payload: arbitrary bytes verbatim')
        ! a zero-length payload is legal and reads as empty
        call net_recv_payload(a, 0, bytes, ne)
        call check(ne == NET_OK .and. len(bytes) == 0, 'payload: zero length')
        ! header after payload stays in sync
        ne = net_send_line(b, 'PING')
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'PING', 'payload: stream stays framed')
        call net_close(a)
        call net_close(b)
    end subroutine

    subroutine t_large_payload()
        character(len=:), allocatable :: bytes, big
        integer :: i, ne
        call new_pair(a, b)
        allocate(character(len=100000) :: big)
        do i = 1, len(big)
            big(i:i) = char(mod(i * 7 + 3, 256))
        end do
        ! larger than the 64 KiB connection buffer: exercises the buffered
        ! prefix + direct-remainder path in net_recv_payload
        ne = net_send_payload(b, big)
        call net_recv_payload(a, len(big), bytes, ne)
        call check(ne == NET_OK .and. bytes == big, 'payload: 100 kB crosses the buffer intact')
        call net_close(a)
        call net_close(b)
    end subroutine

    ! ---- limits and EOF ----

    subroutine t_limits()
        character(len=:), allocatable :: line, bytes
        integer :: ne
        call new_pair(a, b)
        call net_recv_payload(a, NET_MAX_PAYLOAD + 1, bytes, ne)
        call check(ne == NET_LIMIT, 'limit: oversized payload announcement rejected')
        call net_recv_payload(a, -1, bytes, ne)
        call check(ne == NET_LIMIT, 'limit: negative payload count rejected')
        ne = net_send_line(b, repeat('a', NET_MAX_LINE + 100))
        call net_recv_line(a, line, ne)
        call check(ne == NET_LIMIT, 'limit: over-long header line rejected')
        call net_close(a)
        call net_close(b)
    end subroutine

    subroutine t_eof()
        character(len=:), allocatable :: line, bytes
        integer :: ne
        call new_pair(a, b)
        ne = net_send_line(b, 'last')
        call net_close(b)
        call net_recv_line(a, line, ne)
        call check(ne == NET_OK .and. line == 'last', 'eof: buffered line still delivered')
        call net_recv_line(a, line, ne)
        call check(ne == NET_EOF, 'eof: then end of stream on a line')
        call net_close(a)
        call new_pair(a, b)
        ne = net_send_payload(b, 'abc')
        call net_close(b)
        call net_recv_payload(a, 10, bytes, ne)
        call check(ne == NET_EOF, 'eof: payload cut short reports EOF')
        call net_close(a)
    end subroutine

end program utest_net

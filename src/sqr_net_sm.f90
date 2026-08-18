submodule (sqr_net) sqr_net_impl
    use :: clib_wrap, only: c_sock_recv, c_sock_send, c_sock_close
    implicit none

contains

    ! Pull more bytes into the connection's buffer, compacting first so
    ! there is always room while the buffered content is under the framing
    ! limits.  NET_OK / NET_EOF / NET_ERR (a full buffer, which only an
    ! over-limit line could cause, reports NET_LIMIT).
    subroutine fill(conn, ierr)
        type(net_conn_t), intent(inout) :: conn
        integer,          intent(out)   :: ierr
        integer :: i, n
        if (conn%rlo > 1) then
            ! Compact by an explicit forward loop: substring-to-substring
            ! assignment with overlap is not defined behaviour.
            n = conn%rhi - conn%rlo + 1
            do i = 1, n
                conn%rbuf(i:i) = conn%rbuf(conn%rlo + i - 1:conn%rlo + i - 1)
            end do
            conn%rlo = 1
            conn%rhi = n
        end if
        if (conn%rhi >= len(conn%rbuf)) then
            ierr = NET_LIMIT
            return
        end if
        call c_sock_recv(conn%sock, conn%rbuf(conn%rhi + 1:), n)
        if (n > 0) then
            conn%rhi = conn%rhi + n
            ierr = NET_OK
        else if (n == 0) then
            ierr = NET_EOF
        else
            ierr = NET_ERR
        end if
    end subroutine

    module subroutine net_attach(conn, sock)
        type(net_conn_t),   intent(out) :: conn
        integer(c_int64_t), intent(in)  :: sock
        conn%sock = sock
    end subroutine

    module subroutine net_close(conn)
        type(net_conn_t), intent(inout) :: conn
        call c_sock_close(conn%sock)
        conn%rlo = 1
        conn%rhi = 0
    end subroutine

    module function net_send_line(conn, line) result(ierr)
        type(net_conn_t), intent(inout) :: conn
        character(len=*), intent(in)    :: line
        integer                         :: ierr
        ierr = NET_OK
        if (c_sock_send(conn%sock, line // achar(10)) /= 0) ierr = NET_ERR
    end function

    module function net_send_payload(conn, bytes) result(ierr)
        type(net_conn_t), intent(inout) :: conn
        character(len=*), intent(in)    :: bytes
        integer                         :: ierr
        ierr = NET_OK
        if (c_sock_send(conn%sock, bytes) /= 0) ierr = NET_ERR
    end function

    module subroutine net_recv_line(conn, line, ierr)
        type(net_conn_t),              intent(inout) :: conn
        character(len=:), allocatable, intent(out)   :: line
        integer,                       intent(out)   :: ierr
        integer :: p, n
        scan: do
            if (conn%rhi >= conn%rlo) then
                p = index(conn%rbuf(conn%rlo:conn%rhi), achar(10))
                if (p > 0) then
                    n = p - 1
                    if (n > NET_MAX_LINE) then
                        ierr = NET_LIMIT
                        return
                    end if
                    line = conn%rbuf(conn%rlo:conn%rlo + n - 1)
                    conn%rlo = conn%rlo + p
                    if (n > 0) then
                        if (line(n:n) == achar(13)) line = line(1:n - 1)
                    end if
                    ierr = NET_OK
                    return
                end if
                if (conn%rhi - conn%rlo + 1 > NET_MAX_LINE) then
                    ierr = NET_LIMIT
                    return
                end if
            end if
            call fill(conn, ierr)
            if (ierr /= NET_OK) return
        end do scan
    end subroutine

    module subroutine net_recv_payload(conn, nbytes, bytes, ierr)
        type(net_conn_t),              intent(inout) :: conn
        integer,                       intent(in)    :: nbytes
        character(len=:), allocatable, intent(out)   :: bytes
        integer,                       intent(out)   :: ierr
        integer :: have, got, n
        if (nbytes < 0 .or. nbytes > NET_MAX_PAYLOAD) then
            ierr = NET_LIMIT
            return
        end if
        allocate(character(len=nbytes) :: bytes)
        ! Drain what the buffer already holds, then receive the remainder
        ! straight into place — no second copy through the buffer.
        got  = 0
        have = conn%rhi - conn%rlo + 1
        if (have > 0) then
            got = min(have, nbytes)
            bytes(1:got) = conn%rbuf(conn%rlo:conn%rlo + got - 1)
            conn%rlo = conn%rlo + got
        end if
        do while (got < nbytes)
            call c_sock_recv(conn%sock, bytes(got + 1:), n)
            if (n <= 0) then
                deallocate(bytes)
                if (n == 0) then
                    ierr = NET_EOF
                else
                    ierr = NET_ERR
                end if
                return
            end if
            got = got + n
        end do
        ierr = NET_OK
    end subroutine

end submodule sqr_net_impl

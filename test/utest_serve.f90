! Unit tests for the sqrd server core (sqr_serve): protocol session,
! dispatch, response encoding, transaction contention and rollback-on-drop.
!
! The server runs in-process against a scratch database; clients are real
! loopback sockets speaking the wire protocol through sqr_net.  The pattern
! throughout is: client writes a complete request, pump() runs serve_step
! until the server goes idle, client reads the complete response.  That is
! deadlock-free single-threaded because requests and responses here are far
! smaller than the socket buffers.
program utest_serve
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use, intrinsic :: iso_c_binding,   only: c_int64_t
    use :: sqr
    use :: sqr_net
    use :: sqr_serve
    use :: clib_wrap, only: c_sock_connect, c_rmtree
    implicit none

    integer :: pass = 0, fail = 0
    character(len=*), parameter :: TEST_DIR = 'utest_serve_db'

    type(db_t), target, save :: db
    type(server_t) :: srv
    type(net_conn_t) :: cli1, cli2   ! shared across the ordered tests below
    integer :: rs

    call cleanup_dir()
    call db_open(db, TEST_DIR, rs)
    call check(rs == SQR_OK, 'setup: scratch database opens')

    call t_open()
    call t_hello()
    call t_sql_basics()
    call t_rows_binary()
    call t_catalogue()
    call t_errors()
    call t_busy()
    call t_rollback_on_drop()
    call t_quit()

    call serve_close(srv)
    call db_close(db)
    call cleanup_dir()

    print '(a,i0,a,i0,a)', 'serve tests: ', pass, ' passed, ', fail, ' failed'
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

    subroutine cleanup_dir()
        integer :: ios
        ios = c_rmtree(TEST_DIR)
    end subroutine

    ! ---- client-side plumbing ----

    ! Run the server until a full poll cycle finds nothing to do.
    subroutine pump()
        integer :: n
        do
            call serve_step(srv, 50, n)
            if (n == 0) exit
        end do
    end subroutine

    ! Connect a client and complete the HELLO exchange.
    subroutine new_client(cli, okline)
        type(net_conn_t),              intent(out) :: cli
        character(len=:), allocatable, intent(out) :: okline
        integer(c_int64_t) :: sock
        integer :: ne
        sock = c_sock_connect('127.0.0.1', srv%port)
        call net_attach(cli, sock)
        ne = net_send_line(cli, 'HELLO 1 utest_serve')
        call pump()
        call net_recv_line(cli, okline, ne)
        if (ne /= NET_OK) okline = '(no reply)'
    end subroutine

    ! Send one SQL statement and pump the server; the caller reads the reply.
    subroutine send_sql(cli, text)
        type(net_conn_t), intent(inout) :: cli
        character(len=*), intent(in)    :: text
        character(len=12) :: nb
        integer :: ne
        write(nb, '(i0)') len(text)
        ne = net_send_line(cli, 'SQL ' // trim(nb))
        ne = net_send_payload(cli, text)
        call pump()
    end subroutine

    ! Read one non-ROWS reply: hdr is the header line, payload the counted
    ! bytes of MSG/ERR (empty otherwise).
    subroutine read_reply(cli, hdr, payload)
        type(net_conn_t),              intent(inout) :: cli
        character(len=:), allocatable, intent(out)   :: hdr, payload
        integer :: ne, nb, sp
        payload = ''
        call net_recv_line(cli, hdr, ne)
        if (ne /= NET_OK) then
            hdr = '<recv failed>'
            return
        end if
        if (hdr(1:min(3, len(hdr))) == 'MSG' .or. hdr(1:min(3, len(hdr))) == 'ERR') then
            sp = scan(hdr, ' ', back=.true.)
            read(hdr(sp + 1:), *) nb
            call net_recv_payload(cli, nb, payload, ne)
        end if
    end subroutine

    ! Read a complete ROWS response into flat client-side arrays.
    subroutine read_rows(cli, nrows, ncols, collines, isnull, cells, ok)
        type(net_conn_t), intent(inout) :: cli
        integer,          intent(out)   :: nrows, ncols
        character(len=64),             allocatable, intent(out) :: collines(:)
        logical,                       allocatable, intent(out) :: isnull(:,:)
        character(len=64),             allocatable, intent(out) :: cells(:,:)  ! padded; lengths implicit
        logical,          intent(out)   :: ok
        character(len=:), allocatable :: line, bytes
        integer :: ne, r, j, nb
        ok     = .false.
        nrows  = 0
        ncols  = 0
        call net_recv_line(cli, line, ne)
        if (ne /= NET_OK .or. line(1:min(5, len(line))) /= 'ROWS ') return
        read(line(6:), *) nrows, ncols
        allocate(collines(ncols), isnull(max(nrows, 1), max(ncols, 1)), &
                 cells(max(nrows, 1), max(ncols, 1)))
        isnull = .false.
        cells  = ''
        do j = 1, ncols
            call net_recv_line(cli, line, ne)
            if (ne /= NET_OK) return
            collines(j) = line
        end do
        do r = 1, nrows
            do j = 1, ncols
                call net_recv_line(cli, line, ne)
                if (ne /= NET_OK) return
                if (line == 'N') then
                    isnull(r, j) = .true.
                else if (line(1:min(2, len(line))) == 'C ') then
                    read(line(3:), *) nb
                    call net_recv_payload(cli, nb, bytes, ne)
                    if (ne /= NET_OK) return
                    cells(r, j) = bytes
                else
                    return
                end if
            end do
        end do
        call net_recv_line(cli, line, ne)
        ok = ne == NET_OK .and. line == 'END'
    end subroutine

    ! Decode a binary INT / REAL cell (little-endian native bytes).
    function cell_int(s) result(v)
        character(len=*), intent(in) :: s
        integer(int32) :: v
        v = transfer(s(1:4), v)
    end function

    function cell_real(s) result(v)
        character(len=*), intent(in) :: s
        real(real64) :: v
        v = transfer(s(1:8), v)
    end function

    ! ---- tests ----

    subroutine t_open()
        character(len=256) :: emsg
        emsg = ''
        call serve_open(srv, db, 0, rs, emsg)
        call check(rs == SQR_OK, 'open: server binds an ephemeral port')
        call check(srv%port > 0, 'open: port is known')
        call check(srv%dbname == 'utest_serve_db', 'open: advertised name is the dir basename')
    end subroutine

    subroutine t_hello()
        character(len=:), allocatable :: okline
        call new_client(cli1, okline)
        call check(okline(1:min(10, len(okline))) == 'OK 1 sqrd ', 'hello: OK reply')
        call check(index(okline, 'utest_serve_db') > 0, 'hello: reply names the database')
    end subroutine

    subroutine t_sql_basics()
        character(len=:), allocatable :: hdr, payload
        call send_sql(cli1, 'CREATE TABLE t (id INTEGER, x REAL, name CHAR(10), note TEXT)')
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'MSG ' .and. len(payload) > 0, 'sql: DDL returns MSG')
        call send_sql(cli1, 'CREATE UNIQUE INDEX ON t (id)')
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'MSG ', 'sql: index DDL returns MSG')
        call send_sql(cli1, "INSERT INTO t VALUES (1, 0.1, 'Alice', 'hello')")
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'COUNT 1', 'sql: INSERT returns COUNT 1')
        call send_sql(cli1, "INSERT INTO t VALUES (2, -1.5e-300, 'Bob', 'x')")
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'COUNT 1', 'sql: second INSERT')
        call send_sql(cli1, 'INSERT INTO t (id) VALUES (3)')
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'COUNT 1', 'sql: partial INSERT (NULL row)')
    end subroutine

    subroutine t_rows_binary()
        character(len=64), allocatable :: collines(:), cells(:,:)
        logical,           allocatable :: isnull(:,:)
        integer :: nr, nc
        logical :: ok
        call send_sql(cli1, 'SELECT * FROM t ORDER BY id')
        call read_rows(cli1, nr, nc, collines, isnull, cells, ok)
        call check(ok .and. nr == 3 .and. nc == 4, 'rows: 3x4 result set framed')
        if (.not. ok) return
        call check(collines(1) == 'COL id INT 4 null 0',      'rows: INT column line')
        call check(collines(2) == 'COL x REAL 8 null 0',      'rows: REAL column line')
        call check(collines(3) == 'COL name CHAR 10 null 0',  'rows: CHAR column line')
        call check(collines(4) == 'COL note TEXT 12 null 0',  'rows: TEXT column line')
        call check(cell_int(cells(1, 1)) == 1_int32 .and. &
                   cell_int(cells(2, 1)) == 2_int32 .and. &
                   cell_int(cells(3, 1)) == 3_int32,          'rows: INT cells decode')
        call check(cell_real(cells(1, 2)) == 0.1_real64,      'rows: REAL cell is bit-exact 0.1')
        call check(cell_real(cells(2, 2)) == -1.5e-300_real64, 'rows: REAL cell survives extreme exponent')
        call check(trim(cells(1, 3)) == 'Alice' .and. trim(cells(1, 4)) == 'hello', &
                   'rows: CHAR trimmed and TEXT verbatim')
        call check(isnull(3, 2) .and. isnull(3, 3) .and. isnull(3, 4), &
                   'rows: NULL cells travel as N')
        call check(.not. any(isnull(1:2, :)), 'rows: populated cells are not N')
    end subroutine

    subroutine t_catalogue()
        character(len=64), allocatable :: collines(:), cells(:,:)
        logical,           allocatable :: isnull(:,:)
        integer :: nr, nc, ne
        logical :: ok
        ne = net_send_line(cli1, 'TABLES')
        call pump()
        call read_rows(cli1, nr, nc, collines, isnull, cells, ok)
        call check(ok .and. nr == 1 .and. nc == 1 .and. trim(cells(1, 1)) == 't', &
                   'catalogue: TABLES lists t')
        ne = net_send_line(cli1, 'COLUMNS t')
        call pump()
        call read_rows(cli1, nr, nc, collines, isnull, cells, ok)
        call check(ok .and. nr == 4 .and. nc == 5, 'catalogue: COLUMNS is 4x5')
        if (.not. ok) return
        call check(trim(cells(1, 1)) == 'id' .and. trim(cells(1, 2)) == 'INT' .and. &
                   cell_int(cells(1, 3)) == 4_int32 .and. trim(cells(1, 4)) == 'null', &
                   'catalogue: id column described')
        call check(cell_int(cells(1, 5)) == 1_int32, 'catalogue: unique index gives id key-ordinal 1')
        call check(cell_int(cells(2, 5)) == 0_int32 .and. cell_int(cells(3, 5)) == 0_int32, &
                   'catalogue: non-key columns have ordinal 0')
        call check(trim(cells(4, 2)) == 'TEXT', 'catalogue: TEXT column type token')
    end subroutine

    subroutine t_errors()
        character(len=:), allocatable :: hdr, payload
        integer :: ne
        call send_sql(cli1, 'SELEC oops')
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'ERR ' .and. len(payload) > 0, &
                   'errors: bad SQL returns ERR with a message')
        ne = net_send_line(cli1, 'COLUMNS nosuch')
        call pump()
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(6, len(hdr))) == 'ERR 1 ', 'errors: COLUMNS on a missing table is ERR NOT_FOUND')
        ne = net_send_line(cli1, 'BOGUS request')
        call pump()
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'ERR ' .and. index(payload, 'unknown') > 0, &
                   'errors: unknown request is ERR, connection stays up')
        ne = net_send_line(cli1, 'PING')
        call pump()
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'NONE', 'errors: PING still answered afterwards')
    end subroutine

    subroutine t_busy()
        character(len=:), allocatable :: okline, hdr, payload
        character(len=64), allocatable :: collines(:), cells(:,:)
        logical,           allocatable :: isnull(:,:)
        integer :: nr, nc
        logical :: ok
        call new_client(cli2, okline)
        call check(okline(1:min(5, len(okline))) == 'OK 1 ', 'busy: second client connects')
        call send_sql(cli1, 'BEGIN')
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'MSG ' .and. payload == 'transaction started', &
                   'busy: BEGIN on client 1')
        call send_sql(cli2, "INSERT INTO t VALUES (10, 1.0, 'X', 'y')")
        call read_reply(cli2, hdr, payload)
        call check(hdr(1:min(8, len(hdr))) == 'ERR 100 ', 'busy: write from client 2 refused with stat 100')
        call send_sql(cli2, 'SELECT id FROM t WHERE id = 1')
        call read_rows(cli2, nr, nc, collines, isnull, cells, ok)
        call check(ok .and. nr == 1, 'busy: read from client 2 still allowed')
        call send_sql(cli1, "INSERT INTO t VALUES (4, 4.0, 'Dee', 'd')")
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'COUNT 1', 'busy: owner keeps writing inside its transaction')
        call send_sql(cli1, 'COMMIT')
        call read_reply(cli1, hdr, payload)
        call check(hdr(1:min(4, len(hdr))) == 'MSG ' .and. payload == 'committed', &
                   'busy: COMMIT on client 1')
        call send_sql(cli2, "INSERT INTO t VALUES (10, 1.0, 'X', 'y')")
        call read_reply(cli2, hdr, payload)
        call check(hdr == 'COUNT 1', 'busy: client 2 writes once the transaction closes')
    end subroutine

    subroutine t_rollback_on_drop()
        character(len=:), allocatable :: hdr, payload
        character(len=64), allocatable :: collines(:), cells(:,:)
        logical,           allocatable :: isnull(:,:)
        integer :: nr, nc
        logical :: ok
        call send_sql(cli1, 'BEGIN')
        call read_reply(cli1, hdr, payload)
        call send_sql(cli1, "INSERT INTO t VALUES (99, 9.9, 'Zed', 'z')")
        call read_reply(cli1, hdr, payload)
        call check(hdr == 'COUNT 1' .and. db_in_txn(db), 'drop: transaction open with a pending row')
        call net_close(cli1)          ! client vanishes mid-transaction
        call pump()
        call check(.not. db_in_txn(db), 'drop: server rolled the transaction back')
        call send_sql(cli2, 'SELECT id FROM t WHERE id = 99')
        call read_rows(cli2, nr, nc, collines, isnull, cells, ok)
        call check(ok .and. nr == 0, 'drop: the pending row is gone')
    end subroutine

    subroutine t_quit()
        character(len=:), allocatable :: line
        integer :: ne
        ne = net_send_line(cli2, 'QUIT')
        call pump()
        call net_recv_line(cli2, line, ne)
        call check(ne == NET_OK .and. line == 'NONE', 'quit: acknowledged')
        call net_recv_line(cli2, line, ne)
        call check(ne == NET_EOF, 'quit: server closed the connection')
        call net_close(cli2)
    end subroutine

end program utest_serve

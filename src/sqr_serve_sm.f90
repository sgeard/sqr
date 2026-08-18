submodule (sqr_serve) sqr_serve_impl
    use, intrinsic :: iso_fortran_env, only: int32
    use :: sqr, only: SQR_OK, SQR_ERR, SQR_INVALID, SQR_NOT_FOUND, &
                      SQR_NAME_LEN, SQR_ERRMSG_LEN, &
                      DT_INT, DT_REAL, DT_CHAR, DT_TEXT, column_t, &
                      db_in_txn, db_rollback, db_list_tables, db_table_index, db_describe, &
                      db_pack_live
    use :: sql, only: sql_parse, sql_exec, sql_stmt_t, sql_result_t, sql_cell_t, &
                      SQLRES_ROWS, SQLRES_COUNT, SQLRES_MSG, &
                      ST_SELECT, ST_BEGIN, ST_COMMIT, ST_ROLLBACK
    use :: sqr_net, only: net_attach, net_close, net_send_line, net_send_payload, &
                          net_recv_line, net_recv_payload, NET_OK
    use :: clib_wrap, only: c_sock_listen, c_sock_port, c_sock_accept, c_sock_poll, c_sock_close, &
                            c_realpath
    implicit none

    character(len=*), parameter :: LF = achar(10)

    ! Response accumulator: a whole ROWS/catalogue response is built here and
    ! sent with one socket write.  Amortised doubling keeps the per-cell
    ! appends linear overall.
    type :: buf_t
        character(len=:), allocatable :: s
        integer :: n = 0
    end type

contains

    ! ---- server lifecycle ----

    module subroutine serve_open(srv, db, port, stat, errmsg)
        type(server_t),     intent(out)   :: srv
        type(db_t), target, intent(inout) :: db
        integer,            intent(in)    :: port
        integer,            intent(out)   :: stat
        character(len=*),   intent(inout), optional :: errmsg
        if (.not. db%opened) then
            stat = SQR_INVALID
            if (present(errmsg)) errmsg = 'database is not open'
            return
        end if
        srv%lsock = c_sock_listen(port)
        if (srv%lsock < 0) then
            stat = SQR_ERR
            if (present(errmsg)) errmsg = 'cannot listen on port ' // itoa(port)
            return
        end if
        srv%db => db
        srv%port   = c_sock_port(srv%lsock)
        srv%dbname = basename(db%dir)
        ! The physical location is what a backup needs, and the argument sqrd
        ! was started with is usually relative to a cwd the client cannot see.
        ! Resolve it once, here, and fall back to the literal argument if the
        ! platform cannot (the database is open, so this should not happen).
        srv%dbpath = c_realpath(db%dir)
        if (len(srv%dbpath) == 0) srv%dbpath = db%dir
        stat = SQR_OK
    end subroutine

    module subroutine serve_close(srv)
        type(server_t), intent(inout) :: srv
        integer :: i
        do i = 1, SQRD_MAX_SESSIONS
            if (srv%sessions(i)%active) call drop_session(srv, i)
        end do
        call c_sock_close(srv%lsock)
        srv%db => null()
    end subroutine

    module subroutine serve_step(srv, timeout_ms, nevents)
        type(server_t), intent(inout) :: srv
        integer,        intent(in)    :: timeout_ms
        integer,        intent(out)   :: nevents
        integer(c_int64_t) :: socks(SQRD_MAX_SESSIONS + 1)
        logical            :: ready(SQRD_MAX_SESSIONS + 1)
        integer            :: map(SQRD_MAX_SESSIONS + 1)
        integer :: n, i, k, nready
        nevents = 0
        if (srv%lsock < 0) return
        n = 1
        socks(1) = srv%lsock
        map(1)   = 0
        do i = 1, SQRD_MAX_SESSIONS
            if (srv%sessions(i)%active) then
                n = n + 1
                socks(n) = srv%sessions(i)%conn%sock
                map(n)   = i
            end if
        end do
        call c_sock_poll(socks(1:n), timeout_ms, ready(1:n), nready)
        if (nready <= 0) return
        if (ready(1)) then
            call accept_new(srv)
            nevents = nevents + 1
        end if
        do k = 2, n
            if (ready(k)) then
                call service(srv, map(k))
                nevents = nevents + 1
            end if
        end do
    end subroutine

    ! ---- connection handling ----

    subroutine accept_new(srv)
        type(server_t), intent(inout) :: srv
        integer(c_int64_t) :: sock
        integer :: i
        sock = c_sock_accept(srv%lsock)
        if (sock < 0) return
        do i = 1, SQRD_MAX_SESSIONS
            if (.not. srv%sessions(i)%active) then
                call net_attach(srv%sessions(i)%conn, sock)
                srv%sessions(i)%active  = .true.
                srv%sessions(i)%greeted = .false.
                return
            end if
        end do
        call c_sock_close(sock)   ! every slot taken: refuse (the client sees EOF)
    end subroutine

    ! Read and answer one request from session i.  Any framing failure
    ! (EOF, socket error, over-limit line) drops the session — there is no
    ! way to resynchronise a broken frame stream.
    subroutine service(srv, i)
        type(server_t), intent(inout) :: srv
        integer,        intent(in)    :: i
        character(len=:), allocatable :: line, w1, rest
        integer :: ne
        call net_recv_line(srv%sessions(i)%conn, line, ne)
        if (ne /= NET_OK) then
            call drop_session(srv, i)
            return
        end if
        call split2(line, w1, rest)
        if (.not. srv%sessions(i)%greeted) then
            call do_hello(srv, i, w1, rest)
            return
        end if
        select case (w1)
        case ('SQL')
            call do_sql(srv, i, rest)
        case ('TABLES')
            call do_tables(srv, i)
        case ('COLUMNS')
            call do_columns(srv, i, rest)
        case ('INFO')
            call do_info(srv, i)
        case ('PACK')
            call do_pack(srv, i, rest)
        case ('PING')
            ne = net_send_line(srv%sessions(i)%conn, 'NONE')
            if (ne /= NET_OK) call drop_session(srv, i)
        case ('QUIT')
            ne = net_send_line(srv%sessions(i)%conn, 'NONE')
            call drop_session(srv, i)
        case default
            call send_err(srv%sessions(i), SQR_INVALID, 'unknown request: ' // w1)
        end select
    end subroutine

    subroutine drop_session(srv, i)
        type(server_t), intent(inout) :: srv
        integer,        intent(in)    :: i
        if (srv%txn_owner == i) then
            if (db_in_txn(srv%db)) call db_rollback(srv%db)
            srv%txn_owner = 0
        end if
        call net_close(srv%sessions(i)%conn)
        srv%sessions(i)%active  = .false.
        srv%sessions(i)%greeted = .false.
    end subroutine

    ! ---- requests ----

    subroutine do_hello(srv, i, w1, rest)
        type(server_t),   intent(inout) :: srv
        integer,          intent(in)    :: i
        character(len=*), intent(in)    :: w1, rest
        character(len=:), allocatable :: vtok, cname
        integer :: ver, ne
        logical :: ok
        call split2(rest, vtok, cname)   ! cname: client's free-text identity, unused in v1
        call parse_uint(vtok, ver, ok)
        if (w1 /= 'HELLO' .or. .not. ok) then
            call send_err(srv%sessions(i), SQR_INVALID, 'expected HELLO <version> <name>')
            call drop_session(srv, i)
            return
        end if
        if (ver /= SQRD_PROTOCOL) then
            call send_err(srv%sessions(i), SQR_INVALID, 'unsupported protocol version ' &
                // vtok // ' (server speaks ' // itoa(SQRD_PROTOCOL) // ')')
            call drop_session(srv, i)
            return
        end if
        srv%sessions(i)%greeted = .true.
        ne = net_send_line(srv%sessions(i)%conn, 'OK ' // itoa(SQRD_PROTOCOL) &
            // ' sqrd ' // SQRD_VERSION // ' ' // srv%dbname)
        if (ne /= NET_OK) call drop_session(srv, i)
    end subroutine

    subroutine do_sql(srv, i, rest)
        type(server_t),   intent(inout) :: srv
        integer,          intent(in)    :: i
        character(len=*), intent(in)    :: rest
        character(len=:), allocatable :: text
        ! SAVE keeps the statement's allocatable-component descriptors alive
        ! across calls: gfortran -O3 otherwise reports spurious -Wuninitialized
        ! on the entry/exit deallocation code of a bare local.  Semantically
        ! neutral — sql_parse's intent(out) resets it every call, and the
        ! server is single-threaded.
        type(sql_stmt_t), save :: stmt
        type(sql_result_t) :: res
        character(len=SQR_ERRMSG_LEN) :: emsg
        integer :: nb, rs, ne
        logical :: ok
        call parse_uint(rest, nb, ok)
        if (.not. ok) then                   ! malformed frame: cannot resync
            call drop_session(srv, i)
            return
        end if
        call net_recv_payload(srv%sessions(i)%conn, nb, text, ne)
        if (ne /= NET_OK) then               ! includes an over-limit payload
            call drop_session(srv, i)
            return
        end if
        emsg = ''
        call sql_parse(text, stmt, rs, emsg)
        if (rs /= SQR_OK) then
            call send_err(srv%sessions(i), rs, trim(emsg))
            return
        end if
        if (srv%txn_owner /= 0 .and. srv%txn_owner /= i .and. stmt%kind /= ST_SELECT) then
            if (db_in_txn(srv%db)) then
                call send_err(srv%sessions(i), SQRD_STAT_BUSY, &
                    'busy: a transaction is open on another connection')
                return
            end if
            srv%txn_owner = 0    ! engine transaction already gone: stale owner
        end if
        emsg = ''
        call sql_exec(srv%db, stmt, res, rs, emsg, binary_cells=.true.)
        if (rs /= SQR_OK) then
            call send_err(srv%sessions(i), rs, trim(emsg))
            return
        end if
        select case (stmt%kind)
        case (ST_BEGIN)
            srv%txn_owner = i
        case (ST_COMMIT, ST_ROLLBACK)
            srv%txn_owner = 0
        end select
        call send_result(srv%sessions(i), res)
    end subroutine

    subroutine do_tables(srv, i)
        type(server_t), intent(inout) :: srv
        integer,        intent(in)    :: i
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        type(buf_t) :: b
        integer :: k, ne
        call db_list_tables(srv%db, names)
        call buf_add(b, 'ROWS ' // itoa(size(names)) // ' 1' // LF)
        call buf_add(b, 'COL name CHAR ' // itoa(SQR_NAME_LEN) // ' null 0' // LF)
        do k = 1, size(names)
            call buf_add(b, 'C ' // itoa(len_trim(names(k))) // LF)
            call buf_add(b, trim(names(k)))
        end do
        call buf_add(b, 'END' // LF)
        ne = net_send_payload(srv%sessions(i)%conn, b%s(1:b%n))
        if (ne /= NET_OK) call drop_session(srv, i)
    end subroutine

    subroutine do_columns(srv, i, rest)
        type(server_t),   intent(inout) :: srv
        integer,          intent(in)    :: i
        character(len=*), intent(in)    :: rest
        character(len=:), allocatable :: tname
        type(column_t), allocatable :: cols(:)
        integer,        allocatable :: keyord(:)
        type(buf_t) :: b
        ! a plain local, not an associate of the function result: gfortran
        ! double-frees an allocatable function result used as an associate
        ! selector
        character(len=:), allocatable :: tok
        integer :: ti, k, ki, m, ne
        tname = trim(adjustl(rest))
        if (len(tname) == 0) then
            call send_err(srv%sessions(i), SQR_INVALID, 'usage: COLUMNS <table>')
            return
        end if
        ti = db_table_index(srv%db, tname)
        if (ti == 0) then
            call send_err(srv%sessions(i), SQR_NOT_FOUND, 'no such table: ' // tname)
            return
        end if
        call db_describe(srv%db, tname, cols)
        ! Key ordinals: the first UNIQUE index is the designated key — each
        ! member column carries its 1-based position within that key, all
        ! other columns 0.  This is what lets a client (LO Base via the ODBC
        ! driver's SQLPrimaryKeys) treat rows as addressable.
        allocate(keyord(size(cols)), source=0)
        associate (t => srv%db%tables(ti))
            ki = 0
            find_key: do k = 1, t%nindices
                if (t%indices(k)%unique) then
                    ki = k
                    exit find_key
                end if
            end do find_key
            if (ki > 0) then
                do m = 1, t%indices(ki)%ncols
                    keyord(t%indices(ki)%col_idx(m)) = m
                end do
            end if
        end associate
        call buf_add(b, 'ROWS ' // itoa(size(cols)) // ' 5' // LF)
        call buf_add(b, 'COL name CHAR ' // itoa(SQR_NAME_LEN) // ' null 0' // LF)
        call buf_add(b, 'COL type CHAR 8 null 0' // LF)
        call buf_add(b, 'COL csize INT 4 null 0' // LF)
        call buf_add(b, 'COL nullable CHAR 8 null 0' // LF)
        call buf_add(b, 'COL key INT 4 null 0' // LF)
        do k = 1, size(cols)
            call buf_add(b, 'C ' // itoa(len_trim(cols(k)%name)) // LF)
            call buf_add(b, trim(cols(k)%name))
            tok = type_token(cols(k)%dtype)
            call buf_add(b, 'C ' // itoa(len(tok)) // LF)
            call buf_add(b, tok)
            call buf_add(b, 'C 4' // LF)
            call buf_add(b, bin4(cols(k)%csize))
            call buf_add(b, 'C 4' // LF)    ! sqr has no NOT NULL: always nullable
            call buf_add(b, 'null')
            call buf_add(b, 'C 4' // LF)
            call buf_add(b, bin4(keyord(k)))
        end do
        call buf_add(b, 'END' // LF)
        ne = net_send_payload(srv%sessions(i)%conn, b%s(1:b%n))
        if (ne /= NET_OK) call drop_session(srv, i)
    end subroutine

    ! Where this server's database physically is, and what state it is in.
    ! One `key = value` line per fact so a client can grow new keys without a
    ! protocol change, and a human can read the payload straight off a telnet
    ! session.  `dir` is the one that matters: it is what a backup, an archive
    ! or a "which database am I actually connected to?" question needs, and
    ! nothing else on the wire carries it.
    subroutine do_info(srv, i)
        type(server_t), intent(inout) :: srv
        integer,        intent(in)    :: i
        type(buf_t) :: b
        call buf_add(b, 'name = '     // srv%dbname // LF)
        call buf_add(b, 'dir = '      // srv%dbpath // LF)
        call buf_add(b, 'server = sqrd ' // SQRD_VERSION // LF)
        call buf_add(b, 'protocol = ' // itoa(SQRD_PROTOCOL) // LF)
        call buf_add(b, 'readonly = ' // yesno(srv%db%readonly) // LF)
        call buf_add(b, 'txn = '      // yesno(db_in_txn(srv%db)) // LF)
        call buf_add(b, 'tables = '   // itoa(srv%db%ntables) // LF)
        call send_msg(srv%sessions(i), b%s(1:b%n))
    end subroutine

    ! Write a consistent snapshot of the served database to a container file,
    ! without closing the database (see db_pack_live).  The destination is a
    ! counted payload, not a header token, so paths with spaces need no
    ! quoting; it must be absolute, because a relative path would resolve
    ! against sqrd's working directory, which no client can see.
    subroutine do_pack(srv, i, rest)
        type(server_t),   intent(inout) :: srv
        integer,          intent(in)    :: i
        character(len=*), intent(in)    :: rest
        character(len=:), allocatable :: file
        character(len=SQR_ERRMSG_LEN) :: emsg
        integer :: nb, rs, ne
        logical :: ok
        call parse_uint(rest, nb, ok)
        if (.not. ok) then                   ! malformed frame: cannot resync
            call drop_session(srv, i)
            return
        end if
        call net_recv_payload(srv%sessions(i)%conn, nb, file, ne)
        if (ne /= NET_OK) then
            call drop_session(srv, i)
            return
        end if
        file = trim(file)
        if (len(file) == 0 .or. .not. is_absolute(file)) then
            call send_err(srv%sessions(i), SQR_INVALID, &
                'PACK needs an absolute destination path (sqrd''s working directory is not yours)')
            return
        end if
        ! Refuse to write into the database being packed: the container and its
        ! .tmp sibling would sit among the files a later reader enumerates.
        ! The separator matters — a sibling named "<db>-backup.sqr" shares the
        ! directory's prefix without being inside it.
        if (is_within(file, srv%dbpath)) then
            call send_err(srv%sessions(i), SQR_INVALID, &
                'PACK destination is inside the database directory')
            return
        end if
        ! An open transaction belongs to another connection's in-flight work;
        ! the on-disk state is mid-gesture, so there is nothing worth archiving
        ! until it ends.  Same stat code as a refused write, for the same reason.
        if (srv%txn_owner /= 0 .and. db_in_txn(srv%db)) then
            call send_err(srv%sessions(i), SQRD_STAT_BUSY, &
                'busy: a transaction is open on another connection')
            return
        end if
        emsg = ''
        call db_pack_live(srv%db, file, rs, emsg)
        if (rs /= SQR_OK) then
            call send_err(srv%sessions(i), rs, trim(emsg))
            return
        end if
        call send_msg(srv%sessions(i), 'packed ' // srv%dbpath // ' -> ' // file)
    end subroutine

    ! ---- responses ----

    subroutine send_err(sess, stat, msg)
        type(session_t),  intent(inout) :: sess
        integer,          intent(in)    :: stat
        character(len=*), intent(in)    :: msg
        integer :: ne
        ne = net_send_line(sess%conn, 'ERR ' // itoa(stat) // ' ' // itoa(len(msg)))
        if (ne == NET_OK .and. len(msg) > 0) ne = net_send_payload(sess%conn, msg)
    end subroutine

    subroutine send_msg(sess, msg)
        type(session_t),  intent(inout) :: sess
        character(len=*), intent(in)    :: msg
        integer :: ne
        ne = net_send_line(sess%conn, 'MSG ' // itoa(len(msg)))
        if (ne == NET_OK .and. len(msg) > 0) ne = net_send_payload(sess%conn, msg)
    end subroutine

    subroutine send_result(sess, res)
        type(session_t),    intent(inout) :: sess
        type(sql_result_t), intent(in)    :: res
        integer :: ne
        select case (res%kind)
        case (SQLRES_ROWS)
            call send_rows_result(sess, res)
        case (SQLRES_COUNT)
            ne = net_send_line(sess%conn, 'COUNT ' // itoa(res%count))
        case (SQLRES_MSG)
            block
                character(len=:), allocatable :: msg
                msg = ''
                if (allocated(res%message)) msg = res%message
                ne = net_send_line(sess%conn, 'MSG ' // itoa(len(msg)))
                if (ne == NET_OK .and. len(msg) > 0) ne = net_send_payload(sess%conn, msg)
            end block
        case default
            ne = net_send_line(sess%conn, 'NONE')
        end select
    end subroutine

    subroutine send_rows_result(sess, res)
        type(session_t),    intent(inout) :: sess
        type(sql_result_t), intent(in)    :: res
        type(buf_t) :: b
        integer :: r, j, ne
        call buf_add(b, 'ROWS ' // itoa(res%nrows) // ' ' // itoa(res%ncols) // LF)
        do j = 1, res%ncols
            ! Result-set columns are all nullable / key-ordinal 0: sqr has no
            ! NOT NULL, and key discovery is COLUMNS' job, not SELECT's.
            call buf_add(b, 'COL ' // trim(res%colnames(j)) // ' ' &
                // type_token(res%coltypes(j)) // ' ' // itoa(res%colsizes(j)) &
                // ' null 0' // LF)
        end do
        do r = 1, res%nrows
            do j = 1, res%ncols
                call add_cell(b, res%cells(r, j))
            end do
        end do
        call buf_add(b, 'END' // LF)
        ne = net_send_payload(sess%conn, b%s(1:b%n))
        if (ne /= NET_OK) call net_close(sess%conn)
    end subroutine

    pure subroutine add_cell(b, cell)
        type(buf_t),      intent(inout) :: b
        type(sql_cell_t), intent(in)    :: cell
        if (cell%is_null) then
            call buf_add(b, 'N' // LF)
        else if (.not. allocated(cell%text)) then
            call buf_add(b, 'C 0' // LF)
        else
            call buf_add(b, 'C ' // itoa(len(cell%text)) // LF)
            call buf_add(b, cell%text)
        end if
    end subroutine

    ! ---- helpers ----

    pure subroutine buf_add(b, piece)
        type(buf_t),      intent(inout) :: b
        character(len=*), intent(in)    :: piece
        character(len=:), allocatable :: tmp
        integer :: need, cap
        need = b%n + len(piece)
        if (.not. allocated(b%s)) allocate(character(len=max(1024, need)) :: b%s)
        if (need > len(b%s)) then
            cap = len(b%s)
            grow: do while (cap < need)
                cap = 2 * cap
            end do grow
            allocate(character(len=cap) :: tmp)
            tmp(1:b%n) = b%s(1:b%n)
            call move_alloc(tmp, b%s)
        end if
        if (len(piece) > 0) b%s(b%n + 1:need) = piece
        b%n = need
    end subroutine

    pure function itoa(v) result(s)
        integer, intent(in) :: v
        character(len=:), allocatable :: s
        character(len=12) :: tmp
        write(tmp, '(i0)') v
        s = trim(tmp)
    end function

    pure function yesno(flag) result(s)
        logical, intent(in) :: flag
        character(len=:), allocatable :: s
        if (flag) then
            s = 'yes'
        else
            s = 'no'
        end if
    end function

    ! A rooted path: POSIX '/...', or a Windows drive ('c:\...') or UNC
    ! ('\\host\share') prefix.  Anything else would resolve against sqrd's
    ! working directory rather than the client's.
    pure function is_absolute(path) result(yes)
        character(len=*), intent(in) :: path
        logical :: yes
        yes = .false.
        if (len(path) == 0) return
        if (path(1:1) == '/' .or. path(1:1) == achar(92)) then
            yes = .true.
        else if (len(path) >= 3) then
            yes = path(2:2) == ':' .and. (path(3:3) == achar(92) .or. path(3:3) == '/')
        end if
    end function

    ! Is `path` under directory `dir`?  A prefix match alone would also catch
    ! siblings whose names merely start with the directory's, so the next
    ! character must be a separator.
    pure function is_within(path, dir) result(yes)
        character(len=*), intent(in) :: path, dir
        logical :: yes
        integer :: n
        n = len(dir)
        yes = .false.
        if (n == 0 .or. len(path) <= n) return
        if (path(1:n) /= dir) return
        yes = path(n + 1:n + 1) == '/' .or. path(n + 1:n + 1) == achar(92)
    end function

    pure function type_token(dtype) result(tok)
        integer, intent(in) :: dtype
        character(len=:), allocatable :: tok
        select case (dtype)
        case (DT_INT)
            tok = 'INT'
        case (DT_REAL)
            tok = 'REAL'
        case (DT_CHAR)
            tok = 'CHAR'
        case (DT_TEXT)
            tok = 'TEXT'
        case default
            tok = '?'
        end select
    end function

    ! Little-endian (native x86-64/ARM) 4-byte image of an int — the wire
    ! form of an INT cell.
    pure function bin4(v) result(s)
        integer, intent(in) :: v
        character(len=4)    :: s
        s = transfer(int(v, int32), s)
    end function

    ! First blank-delimited word and the (trimmed) remainder.
    pure subroutine split2(s, w1, rest)
        character(len=*),              intent(in)  :: s
        character(len=:), allocatable, intent(out) :: w1, rest
        integer :: p
        p = index(s, ' ')
        if (p == 0) then
            w1   = trim(s)
            rest = ''
        else
            w1   = s(1:p - 1)
            rest = trim(adjustl(s(p + 1:)))
        end if
    end subroutine

    ! Strict non-negative decimal integer; ok is false for anything else
    ! (empty, signs, stray characters, overflow).
    pure subroutine parse_uint(s, v, ok)
        character(len=*), intent(in)  :: s
        integer,          intent(out) :: v
        logical,          intent(out) :: ok
        integer :: ios
        v  = 0
        ok = .false.
        if (len_trim(s) == 0) return
        if (verify(trim(s), '0123456789') /= 0) return
        read(s, *, iostat=ios) v
        ok = ios == 0 .and. v >= 0
    end subroutine

    ! Last path component (trailing separators ignored), blanks mapped to
    ! '_' so the advertised name stays one header token.
    pure function basename(path) result(name)
        character(len=*), intent(in)  :: path
        character(len=:), allocatable :: name
        integer :: e, k
        e = len_trim(path)
        trailing: do while (e > 1)
            if (path(e:e) /= '/' .and. path(e:e) /= achar(92)) exit trailing
            e = e - 1
        end do trailing
        name = path(scan(path(1:e), '/' // achar(92), back=.true.) + 1:e)
        do k = 1, len(name)
            if (name(k:k) == ' ') name(k:k) = '_'
        end do
        if (len(name) == 0) name = 'db'
    end function

end submodule sqr_serve_impl

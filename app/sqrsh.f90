! sqrsh — interactive shell for the sqr database, driven by cmdgraph.
!
! Top-level state holds a single database. Tables are entered as a sub-state
! (`use <table>`) where row-level commands operate. New tables are built in
! a `creator` sub-state where columns are added one at a time and committed
! with `done`.

module sqrsh_state
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use sqr
    implicit none

    type(db_t), save :: db

    ! Pending-table-creation state
    integer, parameter :: MAX_PENDING_COLS = 64
    type(column_t), save :: pending_cols(MAX_PENDING_COLS)
    integer, save        :: pending_ncols = 0

contains

    ! Decode a dlist node to a printable string regardless of its kind.
    pure function node_as_char(node) result(s)
        use dlist
        class(dlist_node_data_t), intent(in) :: node
        character(len=:), allocatable        :: s
        character(len=64) :: buf
        select type (node)
        type is (dlist_node_integer)
            write(buf, '(i0)') node%data
            s = trim(buf)
        type is (dlist_node_real)
            write(buf, '(es15.8)') node%data
            s = trim(adjustl(buf))
        type is (dlist_node_char)
            s = node%data
        class default
            s = ''
        end select
    end function

    function node_as_int(node, ok) result(v)
        use dlist
        class(dlist_node_data_t), intent(in) :: node
        logical,                  intent(out) :: ok
        integer :: v
        v = 0; ok = .true.
        select type (node)
        type is (dlist_node_integer)
            v = node%data
        class default
            ok = .false.
        end select
    end function

    function node_as_real(node, ok) result(v)
        use dlist
        class(dlist_node_data_t), intent(in) :: node
        logical,                  intent(out) :: ok
        real(real64) :: v
        v = 0.0_real64; ok = .true.
        select type (node)
        type is (dlist_node_real)
            v = node%data
        type is (dlist_node_integer)
            v = real(node%data, kind=real64)
        class default
            ok = .false.
        end select
    end function

    subroutine require_db_open(ok)
        logical, intent(out) :: ok
        ok = db%opened
        if (.not. ok) write(*,'(a)') 'no database open — use `open <dir>`'
    end subroutine

    pure function lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: k, c
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) c = c + 32
            r(k:k) = achar(c)
        end do
    end function

    ! A value token of `null` (any case) means SQL NULL for that column.
    function tok_is_null(nd) result(yes)
        use dlist, only: dlist_node_data_t
        class(dlist_node_data_t), intent(in) :: nd
        logical :: yes
        yes = lower(trim(node_as_char(nd))) == 'null'
    end function

    pure function dtype_name(dt) result(s)
        integer, intent(in) :: dt
        character(len=:), allocatable :: s
        select case (dt)
        case (DT_INT);  s = 'int'
        case (DT_REAL); s = 'real'
        case (DT_CHAR); s = 'char'
        case (DT_TEXT); s = 'text'
        case default;   s = '?'
        end select
    end function

end module sqrsh_state


module sqrsh_actions
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use cmdgraph
    use dlist
    use sqr
    use sqrsh_state
    implicit none

contains

    ! ---- top-level ----

    function act_open(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: dir
        character(len=128) :: emsg
        integer :: rs
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: open <directory>'
            return
        end if
        n = args%get(1); dir = node_as_char(n)
        if (db%opened) call db_close(db)
        emsg = ''
        call db_open(db, dir, rs, emsg)
        if (rs /= SQR_OK) then
            write(*,'(2a)') 'open failed: ', trim(emsg)
            rv%errored = .true.
            return
        end if
        write(*,'(3a,i0,a)') 'opened ', dir, ' (', db%ntables, ' tables)'
    end function

    function act_close(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        if (.not. db%opened) then
            write(*,'(a)') 'no database open'
            return
        end if
        call db_close(db)
        write(*,'(a)') 'closed'
    end function

    function act_pack(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: dir, file
        integer :: rs
        if (args%size() /= 2) then
            write(*,'(a)') 'usage: pack <dir> <file>'
            return
        end if
        n = args%get(1); dir  = node_as_char(n)
        n = args%get(2); file = node_as_char(n)
        call db_pack(dir, file, rs)
        if (rs /= SQR_OK) then
            write(*,'(3a,i0)') 'pack of "', dir, '" failed: ', rs
            rv%errored = .true.
            return
        end if
        write(*,'(4a)') 'packed ', dir, ' -> ', file
    end function

    function act_unpack(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: file, dir
        integer :: rs
        if (args%size() /= 2) then
            write(*,'(a)') 'usage: unpack <file> <dir>'
            return
        end if
        n = args%get(1); file = node_as_char(n)
        n = args%get(2); dir  = node_as_char(n)
        call db_unpack(file, dir, rs)
        if (rs /= SQR_OK) then
            write(*,'(3a,i0)') 'unpack of "', file, '" failed: ', rs
            rv%errored = .true.
            return
        end if
        write(*,'(4a)') 'unpacked ', file, ' -> ', dir
    end function

    function act_readonly(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: rs
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        call db_set_readonly(db, rs)
        if (rs == SQR_OK) then
            write(*,'(a)') 'database is now read-only (concurrent readers allowed)'
        else
            write(*,'(a)') 'set read-only failed (a transaction may be open)'
            rv%errored = .true.
        end if
    end function

    function act_tables(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        integer :: i
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        call db_list_tables(db, names)
        if (size(names) == 0) then
            write(*,'(a)') '(no tables)'
        else
            do i = 1, size(names)
                associate (t => db%tables(i))
                    write(*,'(3a,2(i0,a))') '  ', trim(names(i)), &
                        '  (', t%live_count, ' rows, ', t%ncols, ' cols)'
                end associate
            end do
        end if
    end function

    function act_desc(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: name
        integer :: ti, i
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: desc <table>'
            return
        end if
        n = args%get(1); name = node_as_char(n)
        ti = db_table_index(db, name)
        if (ti == 0) then
            write(*,'(2a)') 'no such table: ', trim(name)
            rv%errored = .true.
            return
        end if
        associate (t => db%tables(ti))
            write(*,'(3a,2(i0,a),i0)') 'table ', trim(name), &
                ': record_size=', t%record_size, ', next_id=', t%next_id, ', live=', t%live_count
            do i = 1, t%ncols
                associate (c => t%cols(i))
                    write(*,'(a,i0,5a,i0,a)') '  col ', i, ' ', &
                        trim(c%name), ' (', dtype_name(c%dtype), ', ', c%csize, ' bytes)'
                end associate
            end do
            do i = 1, t%nindices
                if (.not. idx_live(t%indices(i))) cycle   ! skip dropped slots
                associate (ix => t%indices(i))
                    block
                        character(len=:), allocatable :: cols
                        integer :: m
                        cols = trim(ix%columns(1))
                        do m = 2, ix%ncols
                            cols = cols // ',' // trim(ix%columns(m))
                        end do
                        write(*,'(4a,i0,a)') '  ', &
                            merge('unique index on ', 'index on        ', ix%unique), &
                            cols, ' (', ix%nentries, ' entries)'
                    end block
                end associate
            end do
        end associate
    end function

    function act_drop(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: name
        integer :: rs
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: drop <table>'
            return
        end if
        n = args%get(1); name = node_as_char(n)
        call db_drop_table(db, name, rs)
        if (rs == SQR_OK) then
            write(*,'(2a)') 'dropped ', trim(name)
        else
            write(*,'(2a)') 'drop failed: ', trim(name)
            rv%errored = .true.
        end if
    end function

    ! `create <name>` pushes the creator state, ctx = table name.
    function act_create(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: name
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: create <table>'
            return
        end if
        n = args%get(1); name = node_as_char(n)
        if (db_table_index(db, name) > 0) then
            write(*,'(2a)') 'table already exists: ', trim(name)
            rv%errored = .true.
            return
        end if
        pending_ncols = 0
        rv%value = name
    end function

    ! `use <name>` pushes a table-bound state, ctx = table name.
    function act_use(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: name
        logical :: ok
        call require_db_open(ok); if (.not. ok) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: use <table>'
            return
        end if
        n = args%get(1); name = node_as_char(n)
        if (db_table_index(db, name) == 0) then
            write(*,'(2a)') 'no such table: ', trim(name)
            rv%errored = .true.
            return
        end if
        rv%value = name
    end function

    ! ---- creator state ----

    ! Parse a `<name> int | real | text | char <N>` column spec from `args`
    ! starting at 1-based position `base`, into `col`. `cmd` names the calling
    ! command for the usage message. ok=.false. (with a message printed) on a
    ! malformed type or a missing char width. Shared by `col` (creator) and
    ! `addcolumn` (table).
    subroutine parse_col_spec(args, base, cmd, col, ok)
        type(dlist_t),    intent(in)  :: args
        integer,          intent(in)  :: base
        character(len=*), intent(in)  :: cmd
        type(column_t),   intent(out) :: col
        logical,          intent(out) :: ok
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: cname, ctype
        integer :: csize, cn
        logical :: cvt
        ok = .false.
        if (args%size() < base + 1) then
            write(*,'(a)') 'usage: ' // cmd // ' <name> int | real | text | char <N>'
            return
        end if
        n = args%get(base);     cname = node_as_char(n)
        n = args%get(base + 1); ctype = node_as_char(n)
        select case (ctype)
        case ('int', 'i')
            csize = 4;             cn = DT_INT
        case ('real', 'r')
            csize = 8;             cn = DT_REAL
        case ('text', 't')
            csize = SQR_TEXT_DESC; cn = DT_TEXT
        case ('char', 'c')
            if (args%size() < base + 2) then
                write(*,'(a)') 'usage: ' // cmd // ' <name> char <N>'
                return
            end if
            n = args%get(base + 2); csize = node_as_int(n, cvt)
            if (.not. cvt .or. csize < 1) then
                write(*,'(a)') 'char size must be a positive integer'
                return
            end if
            cn = DT_CHAR
        case default
            write(*,'(2a)') 'unknown type: ', ctype
            return
        end select
        col%name  = cname
        col%dtype = cn
        col%csize = csize
        ok = .true.
    end subroutine

    function act_col(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        type(column_t) :: col
        logical :: ok
        if (pending_ncols >= MAX_PENDING_COLS) then
            write(*,'(a)') 'too many columns'
            rv%errored = .true.
            return
        end if
        call parse_col_spec(args, 1, 'col', col, ok)
        if (.not. ok) return
        pending_ncols = pending_ncols + 1
        pending_cols(pending_ncols) = col
        write(*,'(a,i0,5a,i0,a)') '  +col ', pending_ncols, ' ', &
            trim(col%name), ' (', dtype_name(col%dtype), ', ', col%csize, ')'
    end function

    function act_done(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: rs
        character(len=128) :: emsg
        if (pending_ncols == 0) then
            write(*,'(a)') 'no columns defined — add some with `col` or `cancel`'
            rv%errored = .true.
            return
        end if
        emsg = ''
        call db_create_table(db, ctx, pending_cols(1:pending_ncols), rs, emsg)
        if (rs /= SQR_OK) then
            write(*,'(2a)') 'create failed: ', trim(emsg)
            rv%errored = .true.
            return
        end if
        write(*,'(3a,i0,a)') 'created table ', trim(ctx), ' with ', pending_ncols, ' columns'
        pending_ncols = 0
    end function

    ! ---- table-bound state ----

    function act_insert(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nd
        integer :: ti, i, rs
        integer(int32) :: ival, rid
        real(real64)   :: rval
        character(len=:), allocatable :: buf, sval
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= db%tables(ti)%ncols) then
            write(*,'(a,i0,a)') 'expected ', db%tables(ti)%ncols, ' values'
            rv%errored = .true.
            return
        end if
        associate (t => db%tables(ti))
            call row_alloc(buf, t%record_size)
            do i = 1, t%ncols
                nd = args%get(i)
                associate (c => t%cols(i))
                    if (tok_is_null(nd)) then
                        call row_set_null(buf, c)   ! `null` token ⇒ SQL NULL
                    else
                        select case (c%dtype)
                        case (DT_INT)
                            ival = node_as_int(nd, ok)
                            if (.not. ok) then
                                write(*,'(a,i0,a)') 'col ', i, ': expected integer'
                                rv%errored = .true.
                                return
                            end if
                            call row_set_int(buf, c, ival)
                        case (DT_REAL)
                            rval = node_as_real(nd, ok)
                            if (.not. ok) then
                                write(*,'(a,i0,a)') 'col ', i, ': expected number'
                                rv%errored = .true.
                                return
                            end if
                            call row_set_real(buf, c, rval)
                        case (DT_CHAR)
                            sval = node_as_char(nd)
                            call row_set_char(buf, c, sval)
                        case (DT_TEXT)
                            continue   ! written after insert via db_set_text
                        end select
                    end if
                end associate
            end do
        end associate
        call db_insert(db, ctx, buf, rid, rs)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'insert failed'
            rv%errored = .true.
            return
        end if
        set_text: do i = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(i)%dtype /= DT_TEXT) cycle set_text
            nd = args%get(i)
            if (tok_is_null(nd)) cycle set_text   ! left NULL, no blob write
            call db_set_text(db, ctx, rid, trim(db%tables(ti)%cols(i)%name), &
                             node_as_char(nd), rs)
            if (rs /= SQR_OK) then
                write(*,'(a)') 'set_text failed'
                rv%errored = .true.
                return
            end if
        end do set_text
        write(*,'(a,i0)') 'inserted row ', rid
    end function

    function act_update(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nd
        integer :: ti, i, rs
        integer(int32) :: ival, rid
        real(real64)   :: rval
        character(len=:), allocatable :: buf, sval
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= db%tables(ti)%ncols + 1) then
            write(*,'(a,i0,a)') 'usage: update <row_id> followed by ', &
                db%tables(ti)%ncols, ' values'
            rv%errored = .true.
            return
        end if
        nd = args%get(1); rid = node_as_int(nd, ok)
        if (.not. ok) then
            write(*,'(a)') 'row_id must be an integer'
            rv%errored = .true.
            return
        end if
        associate (t => db%tables(ti))
            call row_alloc(buf, t%record_size)
            do i = 1, t%ncols
                nd = args%get(i + 1)
                associate (c => t%cols(i))
                    if (tok_is_null(nd)) then
                        call row_set_null(buf, c)   ! `null` token ⇒ SQL NULL
                    else
                        select case (c%dtype)
                        case (DT_INT)
                            ival = node_as_int(nd, ok)
                            if (.not. ok) then
                                write(*,'(a,i0,a)') 'col ', i, ': expected integer'
                                rv%errored = .true.
                                return
                            end if
                            call row_set_int(buf, c, ival)
                        case (DT_REAL)
                            rval = node_as_real(nd, ok)
                            if (.not. ok) then
                                write(*,'(a,i0,a)') 'col ', i, ': expected number'
                                rv%errored = .true.
                                return
                            end if
                            call row_set_real(buf, c, rval)
                        case (DT_CHAR)
                            sval = node_as_char(nd)
                            call row_set_char(buf, c, sval)
                        case (DT_TEXT)
                            continue   ! written after update via db_set_text
                        end select
                    end if
                end associate
            end do
        end associate
        call db_update(db, ctx, rid, buf, rs)
        if (rs /= SQR_OK) then
            write(*,'(a,i0)') 'update failed: no such row ', rid
            rv%errored = .true.
            return
        end if
        set_text: do i = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(i)%dtype /= DT_TEXT) cycle set_text
            nd = args%get(i + 1)
            if (tok_is_null(nd)) cycle set_text   ! left NULL, no blob write
            call db_set_text(db, ctx, rid, trim(db%tables(ti)%cols(i)%name), &
                             node_as_char(nd), rs)
            if (rs /= SQR_OK) then
                write(*,'(a)') 'set_text failed'
                rv%errored = .true.
                return
            end if
        end do set_text
        write(*,'(a,i0)') 'updated row ', rid
    end function

    ! db_scan callback for `select`: prints each live row. The table index
    ! is threaded through the opaque scan context as a plain integer.
    subroutine select_emit_cb(scan_db, row_id, buf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: row_id
        character(len=*), intent(in)    :: buf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        stop = .false.
        select type (ctx)
        type is (integer)
            call print_row(ctx, row_id, buf)   ! TEXT resolved via module db
        end select
    end subroutine

    function act_select(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: ti, rs
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        call print_header(ti)
        call db_scan(db, ctx, select_emit_cb, ti, rs)
    end function

    function act_get(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        integer :: ti, rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: get <row_id>'
            return
        end if
        n = args%get(1); rid = node_as_int(n, ok)
        if (.not. ok) then
            write(*,'(a)') 'row_id must be an integer'
            rv%errored = .true.
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, ctx, rid, buf, rs)
        if (rs /= SQR_OK) then
            write(*,'(a,i0)') 'no such row: ', rid
            rv%errored = .true.
            return
        end if
        call print_header(ti)
        call print_row(ti, rid, buf)
    end function

    function act_delete(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        integer :: ti, rs
        integer(int32) :: rid
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: delete <row_id>'
            return
        end if
        n = args%get(1); rid = node_as_int(n, ok)
        if (.not. ok) then
            write(*,'(a)') 'row_id must be an integer'
            rv%errored = .true.
            return
        end if
        call db_delete(db, ctx, rid, rs)
        if (rs /= SQR_OK) then
            write(*,'(a,i0)') 'no such row: ', rid
            rv%errored = .true.
            return
        end if
        write(*,'(a,i0)') 'deleted row ', rid
    end function

    function act_compact(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: ti, rs
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= 0) then
            write(*,'(a)') 'usage: compact'
            return
        end if
        call db_compact(db, ctx, rs)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'compact failed'
            rv%errored = .true.
            return
        end if
        write(*,'(a,i0,a)') 'compacted ', db%tables(ti)%live_count, ' live rows'
    end function

    function act_verify(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: ti, rs
        character(len=128) :: emsg
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= 0) then
            write(*,'(a)') 'usage: verify'
            return
        end if
        emsg = ''
        call db_verify(db, ctx, rs, emsg)
        if (rs == SQR_OK) then
            write(*,'(a)') 'verify: OK'
        else
            write(*,'(a)') 'verify: FAILED — ' // trim(emsg)
            rv%errored = .true.
        end if
    end function

    function act_dropindex(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        integer :: ti, rs, nargs, k
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        nargs = args%size()
        if (nargs < 1) then
            write(*,'(a)') 'usage: dropindex <col> [<col> ...]'
            return
        end if
        allocate(cols(nargs))
        do k = 1, nargs
            n = args%get(k)
            cols(k) = node_as_char(n)
        end do
        call db_drop_index(db, ctx, cols, rs)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'dropindex: no index over those columns'
            rv%errored = .true.
            return
        end if
        write(*,'(a)') 'index dropped'
    end function

    ! addcolumn <name> int|real|text|char <N> — add a column to the current
    ! table (existing rows get it NULL; row_ids are preserved).
    function act_addcolumn(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        type(column_t) :: col
        integer :: ti, rs
        character(len=128) :: emsg
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        call parse_col_spec(args, 1, 'addcolumn', col, ok)
        if (.not. ok) then
            rv%errored = .true.
            return
        end if
        emsg = ''
        call db_add_column(db, ctx, col, rs, emsg)
        if (rs /= SQR_OK) then
            write(*,'(2a)') 'addcolumn failed: ', trim(emsg)
            rv%errored = .true.
            return
        end if
        write(*,'(5a)') 'added column ', trim(col%name), &
            ' (', dtype_name(col%dtype), ')'
    end function

    ! dropcolumn <name> — drop a column from the current table; any index that
    ! uses it is CASCADE-dropped. row_ids are preserved.
    function act_dropcolumn(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: cname
        integer :: ti, rs
        character(len=128) :: emsg
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        if (args%size() /= 1) then
            write(*,'(a)') 'usage: dropcolumn <name>'
            return
        end if
        n = args%get(1); cname = node_as_char(n)
        emsg = ''
        call db_drop_column(db, ctx, cname, rs, emsg)
        if (rs /= SQR_OK) then
            write(*,'(2a)') 'dropcolumn failed: ', trim(emsg)
            rv%errored = .true.
            return
        end if
        write(*,'(2a)') 'dropped column ', trim(cname)
    end function

    function act_index(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        integer :: rs, nargs, first, k
        logical :: uniq
        nargs = args%size()
        if (nargs < 1) then
            write(*,'(a)') 'usage: index [unique] <col> [<col> ...]'
            return
        end if
        n = args%get(1)
        uniq  = trim(node_as_char(n)) == 'unique'
        first = merge(2, 1, uniq)
        if (nargs < first) then
            write(*,'(a)') 'usage: index [unique] <col> [<col> ...]'
            return
        end if
        allocate(cols(nargs - first + 1))
        do k = first, nargs
            n = args%get(k)
            cols(k - first + 1) = node_as_char(n)
        end do
        call db_create_index(db, ctx, cols, rs, unique=uniq)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'create_index failed'
            rv%errored = .true.
            return
        end if
        write(*,'(a,i0,a)') 'indexed ', size(cols), ' column(s)'
    end function

    ! Build a row-shaped buffer with just the key columns set, plus the
    ! ordered member-name array, from `col v` pairs. ti must be valid.
    subroutine parse_key_pairs(ti, args, buf, cols, ok)
        integer,                                  intent(in)  :: ti
        type(dlist_t),                            intent(in)  :: args
        character(len=:),            allocatable, intent(out) :: buf
        character(len=SQR_NAME_LEN), allocatable, intent(out) :: cols(:)
        logical,                                  intent(out) :: ok
        class(dlist_node_data_t), allocatable :: nc, nv
        integer :: np, p, ci, j
        integer(int32) :: ival
        real(real64)   :: rval
        logical :: cvt
        ok = .false.
        if (mod(args%size(), 2) /= 0 .or. args%size() < 2) then
            write(*,'(a)') 'usage: <col> <value> [<col> <value> ...]'
            return
        end if
        np = args%size() / 2
        allocate(cols(np))
        associate (t => db%tables(ti))
            call row_alloc(buf, t%record_size)
            do p = 1, np
                nc = args%get(2*p - 1)
                nv = args%get(2*p)
                cols(p) = node_as_char(nc)
                ci = 0
                find_col: do j = 1, t%ncols
                    if (t%cols(j)%name == trim(cols(p))) then
                        ci = j
                        exit find_col
                    end if
                end do find_col
                if (ci == 0) then
                    write(*,'(2a)') 'no such column: ', trim(cols(p))
                    return
                end if
                associate (c => t%cols(ci))
                    select case (c%dtype)
                    case (DT_INT)
                        ival = node_as_int(nv, cvt)
                        if (.not. cvt) then
                            write(*,'(a)') 'value must be an integer'
                            return
                        end if
                        call row_set_int(buf, c, ival)
                    case (DT_REAL)
                        rval = node_as_real(nv, cvt)
                        if (.not. cvt) then
                            write(*,'(a)') 'value must be a number'
                            return
                        end if
                        call row_set_real(buf, c, rval)
                    case (DT_CHAR)
                        call row_set_char(buf, c, node_as_char(nv))
                    case default
                        write(*,'(a)') 'key column type not supported'
                        return
                    end select
                end associate
            end do
        end associate
        ok = .true.
    end subroutine

    function act_getk(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        character(len=:), allocatable :: keyrow, buf
        integer :: ti, rs
        integer(int32) :: rid
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        call parse_key_pairs(ti, args, keyrow, cols, ok)
        if (.not. ok) then
            rv%errored = .true.
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get_by_key(db, ctx, cols, keyrow, buf, rs, row_id=rid)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'no row for that key'
            rv%errored = .true.
            return
        end if
        call print_header(ti)
        call print_row(ti, rid, buf)
    end function

    function act_delk(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        character(len=:), allocatable :: keyrow
        integer :: ti, rs
        logical :: ok
        ti = db_table_index(db, ctx)
        if (ti == 0) return
        call parse_key_pairs(ti, args, keyrow, cols, ok)
        if (.not. ok) then
            rv%errored = .true.
            return
        end if
        call db_delete_by_key(db, ctx, cols, keyrow, rs)
        if (rs /= SQR_OK) then
            write(*,'(a)') 'no row for that key'
            rv%errored = .true.
            return
        end if
        write(*,'(a)') 'deleted by key'
    end function

    function act_find(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nc, nv
        character(len=:), allocatable :: col, buf
        integer :: ti, ci, rs, j
        integer(int32) :: rid, ival
        real(real64)   :: rval
        logical :: ok
        ti = db_table_index(db, ctx); if (ti == 0) return
        if (args%size() /= 2) then
            write(*,'(a)') 'usage: find <col> <value>'
            return
        end if
        nc = args%get(1); col = node_as_char(nc)
        ci = 0
        find_col: do j = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(j)%name == trim(col)) then
                ci = j
                exit find_col
            end if
        end do find_col
        if (ci == 0) then
            write(*,'(2a)') 'no such column: ', trim(col)
            rv%errored = .true.
            return
        end if
        nv = args%get(2)
        select case (db%tables(ti)%cols(ci)%dtype)
        case (DT_INT)
            ival = node_as_int(nv, ok)
            if (.not. ok) then
                write(*,'(a)') 'value must be an integer'
                rv%errored = .true.
                return
            end if
            call db_find_by_int(db, ctx, col, ival, rid, rs)
        case (DT_REAL)
            rval = node_as_real(nv, ok)
            if (.not. ok) then
                write(*,'(a)') 'value must be a number'
                rv%errored = .true.
                return
            end if
            call db_find_by_real(db, ctx, col, rval, rid, rs)
        case (DT_CHAR)
            call db_find_by_char(db, ctx, col, node_as_char(nv), rid, rs)
        case default
            write(*,'(a)') 'find on this column type not supported'
            rv%errored = .true.
            return
        end select
        if (rs /= SQR_OK) then
            write(*,'(a)') 'no match'
            rv%errored = .true.
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, ctx, rid, buf, rs)
        if (rs /= SQR_OK) return          ! found id but row unreadable
        call print_header(ti)
        call print_row(ti, rid, buf)
    end function

    ! range <col> <lo> <hi>: print every live row whose indexed <col> is in
    ! the inclusive band [lo,hi], in ascending key order (the pull-cursor
    ! complement to select/find).
    function act_range(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nc, nlo, nhi
        character(len=:), allocatable :: col, buf
        integer :: ti, ci, rs, j, nrows
        integer(int32) :: rid, ilo, ihi
        real(real64)   :: rlo, rhi
        logical :: ok, oklo, okhi
        type(db_cursor_t) :: cur
        ti = db_table_index(db, ctx); if (ti == 0) return
        if (args%size() /= 3) then
            write(*,'(a)') 'usage: range <col> <lo> <hi>'
            return
        end if
        nc = args%get(1); col = node_as_char(nc)
        ci = 0
        find_col: do j = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(j)%name == trim(col)) then
                ci = j
                exit find_col
            end if
        end do find_col
        if (ci == 0) then
            write(*,'(2a)') 'no such column: ', trim(col)
            rv%errored = .true.
            return
        end if
        nlo = args%get(2); nhi = args%get(3)
        select case (db%tables(ti)%cols(ci)%dtype)
        case (DT_INT)
            ilo = node_as_int(nlo, oklo); ihi = node_as_int(nhi, okhi)
            if (.not. (oklo .and. okhi)) then
                write(*,'(a)') 'bounds must be integers'
                rv%errored = .true.
                return
            end if
            call db_find_range(db, ctx, col, ilo, ihi, cur, rs)
        case (DT_REAL)
            rlo = node_as_real(nlo, oklo); rhi = node_as_real(nhi, okhi)
            if (.not. (oklo .and. okhi)) then
                write(*,'(a)') 'bounds must be numbers'
                rv%errored = .true.
                return
            end if
            call db_find_range(db, ctx, col, rlo, rhi, cur, rs)
        case (DT_CHAR)
            call db_find_range(db, ctx, col, node_as_char(nlo), node_as_char(nhi), cur, rs)
        case default
            write(*,'(a)') 'range on this column type not supported'
            rv%errored = .true.
            return
        end select
        if (rs /= SQR_OK) then
            write(*,'(a)') 'no index on that column'
            rv%errored = .true.
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call print_header(ti)
        nrows = 0
        pull: do
            call db_cursor_next(db, cur, rid, buf, ok, rs)
            if (rs /= SQR_OK) then          ! I/O error / invalidated cursor
                write(*,'(a)') 'error reading range'
                rv%errored = .true.
                return
            end if
            if (.not. ok) exit pull
            call print_row(ti, rid, buf)
            nrows = nrows + 1
        end do pull
        if (nrows == 0) write(*,'(a)') '(no rows in range)'
    end function

    ! ---- row printing ----

    subroutine print_header(ti)
        integer, intent(in) :: ti
        integer :: i
        write(*,'(a8)', advance='no') 'row_id'
        do i = 1, db%tables(ti)%ncols
            associate (c => db%tables(ti)%cols(i))
                write(*,'(2x,a16)', advance='no') trim(c%name)
            end associate
        end do
        write(*,*)
    end subroutine

    subroutine print_row(ti, rid, buf)
        integer,          intent(in) :: ti
        integer(int32),   intent(in) :: rid
        character(len=*), intent(in) :: buf
        integer :: i
        write(*,'(i8)', advance='no') rid
        do i = 1, db%tables(ti)%ncols
            associate (c => db%tables(ti)%cols(i))
                if (row_is_null(buf, c)) then
                    write(*,'(2x,a16)', advance='no') 'NULL'
                    cycle
                end if
                select case (c%dtype)
                case (DT_INT)
                    write(*,'(2x,i16)', advance='no') row_get_int(buf, c)
                case (DT_REAL)
                    write(*,'(2x,es16.8)', advance='no') row_get_real(buf, c)
                case (DT_CHAR)
                    write(*,'(2x,a16)', advance='no') trim(row_get_char(buf, c))
                case (DT_TEXT)
                    get_text: block
                        character(len=:), allocatable :: tv
                        call db_get_text(db, trim(db%tables(ti)%name), rid, &
                                         trim(c%name), tv)
                        write(*,'(2x,a)', advance='no') tv
                    end block get_text
                end select
            end associate
        end do
        write(*,*)
    end subroutine

end module sqrsh_actions


program sqrsh
    use cmdgraph
    use sqr
    use sqrsh_state
    use sqrsh_actions
    implicit none

    type(engine_t) :: ui
    character(len=256) :: arg

#ifdef SQR_WITH_REGEX
    ! The regex `match` command is supplied as an external procedure by the
    ! tcl_re-backed module regex/sqrsh_regex.f90, linked only by
    ! `make sqrsh-regex`. Declaring it external here — rather than `use`-ing
    ! that module — keeps this file free of any reference to tcl_re, which fpm
    ! cannot build and whose dependency scanner does not honour this #ifdef.
    interface
        function act_match(args, ctx) result(rv)
            use cmdgraph, only: action_result_t
            use dlist,    only: dlist_t
            type(dlist_t),    intent(in) :: args
            character(len=*), intent(in) :: ctx
            type(action_result_t)        :: rv
        end function act_match
    end interface
#endif

    ! root state — database-level commands
    call ui%add_state("root", prompt="sqr> ")
    call ui%add_command("root", "o(pen)",   EDGE_ACTION, proc=act_open,   help="open <directory>")
    call ui%add_command("root", "cl(ose)",  EDGE_ACTION, proc=act_close,  help="close database")
    call ui%add_command("root", "readonly", EDGE_ACTION, proc=act_readonly, &
                        help="demote the open database to read-only")
    call ui%add_command("root", "t(ables)", EDGE_ACTION, proc=act_tables, help="list tables")
    call ui%add_command("root", "d(esc)",   EDGE_ACTION, proc=act_desc,   help="desc <table>")
    call ui%add_command("root", "cr(eate)", EDGE_DO_GOTO, target="creator", proc=act_create, &
                        help="create <table> — enter column-building mode")
    call ui%add_command("root", "u(se)",    EDGE_DO_GOTO, target="table",   proc=act_use, &
                        help="use <table> — operate on a table")
    call ui%add_command("root", "dr(op)",   EDGE_ACTION, proc=act_drop,   help="drop <table>")
    call ui%add_command("root", "pack",     EDGE_ACTION, proc=act_pack,   help="pack <dir> <file>")
    call ui%add_command("root", "unpack",   EDGE_ACTION, proc=act_unpack, help="unpack <file> <dir>")
    call ui%add_command("root", "q(uit)",   EDGE_QUIT,                    help="exit")

    ! creator state — defining a new table's columns
    call ui%add_state("creator", prompt="cols> ")
    call ui%add_command("creator", "col",     EDGE_ACTION,  proc=act_col,  &
                        help="col <name> int | real | text | char <N>")
    call ui%add_command("creator", "done",    EDGE_DO_POP,  proc=act_done, &
                        help="commit the table")
    call ui%add_command("creator", "cancel",  EDGE_POP,                    help="discard pending columns")
    call ui%add_command("creator", "q(uit)",  EDGE_QUIT,                   help="exit")

    ! table-bound state — operations on a selected table (name in ctx)
    call ui%add_state("table", prompt="row> ")
    call ui%add_command("table", "i(nsert)", EDGE_ACTION, proc=act_insert, &
                        help="insert <v1> <v2> ... (one per column)")
    call ui%add_command("table", "u(pdate)", EDGE_ACTION, proc=act_update, &
                        help="update <row_id> <v1> <v2> ... (one per column)")
    call ui%add_command("table", "s(elect)", EDGE_ACTION, proc=act_select, help="show all rows")
    call ui%add_command("table", "g(et)",    EDGE_ACTION, proc=act_get,    help="get <row_id>")
    call ui%add_command("table", "del(ete)", EDGE_ACTION, proc=act_delete, help="delete <row_id>")
    call ui%add_command("table", "co(mpact)", EDGE_ACTION, proc=act_compact, &
         help="reclaim deleted rows + blob (renumbers row_ids)")
    call ui%add_command("table", "addcolumn", EDGE_ACTION, proc=act_addcolumn, &
         help="addcolumn <name> int | real | text | char <N>")
    call ui%add_command("table", "dropcolumn", EDGE_ACTION, proc=act_dropcolumn, &
         help="dropcolumn <name> (CASCADEs dependent indices)")
    call ui%add_command("table", "in(dex)",  EDGE_ACTION, proc=act_index, &
         help="index [unique] <col> [<col> ...]")
    call ui%add_command("table", "dropindex", EDGE_ACTION, proc=act_dropindex, &
         help="dropindex <col> [<col> ...] (remove a secondary index)")
    call ui%add_command("table", "v(erify)", EDGE_ACTION, proc=act_verify, &
         help="verify table/index integrity")
    call ui%add_command("table", "f(ind)",   EDGE_ACTION, proc=act_find,   help="find <col> <value>")
    call ui%add_command("table", "r(ange)",  EDGE_ACTION, proc=act_range, &
         help="range <col> <lo> <hi> (indexed rows in [lo,hi], ascending)")
#ifdef SQR_WITH_REGEX
    call ui%add_command("table", "m(atch)",  EDGE_ACTION, proc=act_match, &
         help="match <col> <regex> (char column, regex scan)")
#endif
    call ui%add_command("table", "getk",     EDGE_ACTION, proc=act_getk, &
         help="getk <col> <val> [<col> <val> ...] (by unique key)")
    call ui%add_command("table", "delk",     EDGE_ACTION, proc=act_delk, &
         help="delk <col> <val> [<col> <val> ...] (by unique key)")
    call ui%add_command("table", "b(ack)",   EDGE_POP,                    help="back to root")
    call ui%add_command("table", "q(uit)",   EDGE_QUIT,                   help="exit")

    call ui%finalize("root")

    ! Optional positional arg: open this directory at startup.
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        call open_initial(trim(arg))
    end if

    write(*,'(a)') 'sqr shell — type `help` for commands'
    call ui%run()

    if (db%opened) call db_close(db)

contains

    subroutine open_initial(dir)
        character(len=*), intent(in) :: dir
        integer :: rs
        character(len=128) :: emsg
        emsg = ''
        call db_open(db, dir, rs, emsg)
        if (rs /= SQR_OK) then
            write(*,'(4a)') 'open ', dir, ' failed: ', trim(emsg)
        else
            write(*,'(3a,i0,a)') 'opened ', dir, ' (', db%ntables, ' tables)'
        end if
    end subroutine

end program sqrsh

! sqrsh — interactive shell for the sqr database, driven by cmdgraph.
!
! Top-level state holds a single database. Tables are entered as a sub-state
! (`use <table>`) where row-level commands operate. New tables are built in
! a `creator` sub-state where columns are added one at a time and committed
! with `done`.

module sqrsh_state
    use, intrinsic :: iso_fortran_env, only: int32, real64
    use sqr
    use sql, only: SQL_MAX_COLS
    implicit none

    type(db_t), save, target :: db

    ! Pending-table-creation state: as many columns as CREATE TABLE allows.
    integer, parameter :: MAX_PENDING_COLS = SQL_MAX_COLS
    type(column_t), save :: pending_cols(MAX_PENDING_COLS)
    integer, save        :: pending_ncols = 0

contains

    ! Decode a dlist node to a printable string regardless of its kind.
    pure function node_as_char(node) result(s)
        use dlist
        use sqr_consts, only: NUM_TEXT_LEN
        class(dlist_node_data_t), intent(in) :: node
        character(len=:), allocatable        :: s
        character(len=NUM_TEXT_LEN) :: buf
        select type (node)
        type is (dlist_node_integer)
            write(buf, '(i0)') node%data
            s = trim(buf)
        type is (dlist_node_real)
            s = real_text(node%data)
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

    pure function lower(s) result(r)
        use :: sqr_consts, only: ASCII_CASE_OFFSET
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: k, c
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) c = c + ASCII_CASE_OFFSET
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
    use sql, only: char_count
    use sqrsh_state
    implicit none

    ! Width a field of the row display is right-justified in (wider values
    ! are written in full).
    integer, parameter :: FIELD_WIDTH = 16

contains

    ! Report a failure: cmdgraph writes `text` to its error channel (standard
    ! error) and the command counts as failed.  Every failure and usage
    ! message of the shell goes through here; none is written to output.
    subroutine fail(rv, text)
        type(action_result_t), intent(inout) :: rv
        character(len=*),      intent(in)    :: text
        rv%errored = .true.
        rv%errmsg  = text
    end subroutine

    ! The engine's message for the last failure, empty when it gave none.
    function engine_reason() result(why)
        character(len=:), allocatable :: why
        call db_last_error(db, msg=why)
        if (.not. allocated(why)) why = ''
    end function

    ! `what: why`, or `what` alone when there is no reason to add.
    pure function with_reason(what, why) result(s)
        character(len=*), intent(in)  :: what, why
        character(len=:), allocatable :: s
        if (len_trim(why) > 0) then
            s = what // ': ' // trim(why)
        else
            s = what
        end if
    end function

    ! An integer as text, for messages.
    pure function int_text(i) result(t)
        use sqr_consts, only: INT_TEXT_LEN
        integer, intent(in) :: i
        character(len=:), allocatable :: t
        character(len=INT_TEXT_LEN) :: buf
        write(buf, '(i0)') i
        t = trim(buf)
    end function

    ! Index slot of a live index whose first column is `col` (the index find
    ! and range use), or 0.
    function leading_index(ti, col) result(j)
        integer,          intent(in) :: ti
        character(len=*), intent(in) :: col
        integer :: j
        do j = 1, db%tables(ti)%nindices
            if (.not. idx_live(db%tables(ti)%indices(j))) cycle
            if (db%tables(ti)%indices(j)%columns(1) == col) return
        end do
        j = 0
    end function

    ! `.true.` when a live unique index has exactly the columns `cols`, in
    ! that order (the index getk and delk use).
    function has_unique_index(ti, cols) result(yes)
        integer,          intent(in) :: ti
        character(len=*), intent(in) :: cols(:)
        logical :: yes
        integer :: j
        yes = .false.
        do j = 1, db%tables(ti)%nindices
            associate (ix => db%tables(ti)%indices(j))
                if (.not. idx_live(ix)) cycle
                if (.not. ix%unique .or. ix%ncols /= size(cols)) cycle
                if (all(ix%columns(1:ix%ncols) == cols)) then
                    yes = .true.
                    return
                end if
            end associate
        end do
    end function

    subroutine require_db_open(ok, rv)
        logical,               intent(out)   :: ok
        type(action_result_t), intent(inout) :: rv
        ok = db%opened
        if (.not. ok) call fail(rv, 'no database open — use `open <dir>`')
    end subroutine

    ! Resolve the context table, reporting `no such table` (and flagging the
    ! action as errored) when it is gone, rather than the silent no-op the
    ! bare `if (ti == 0) return` used to give.
    function require_ctx_table(ctx, ti, rv) result(ok)
        character(len=*),      intent(in)    :: ctx
        integer,               intent(out)   :: ti
        type(action_result_t), intent(inout) :: rv
        logical :: ok
        ti = db_table_index(db, ctx)
        ok = ti /= 0
        if (.not. ok) call fail(rv, 'no such table: ' // trim(ctx))
    end function

    ! Column ordinal of `name` in table `ti`, or 0 if absent.
    function shell_col_index(ti, name) result(ci)
        integer,          intent(in) :: ti
        character(len=*), intent(in) :: name
        integer :: ci, j
        ci = 0
        do j = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(j)%name == trim(name)) then
                ci = j
                return
            end if
        end do
    end function

    ! ---- top-level ----

    function act_open(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: dir
        character(len=SQR_ERRMSG_LEN) :: emsg
        integer :: rs
        if (args%size() /= 1) then
            call fail(rv, 'usage: open <directory>')
            return
        end if
        n = args%get(1); dir = node_as_char(n)
        if (db%opened) call db_close(db)
        emsg = ''
        call db_open(db, dir, rs, emsg)
        if (rs /= SQR_OK) then
            call fail(rv, 'open failed: ' // trim(emsg))
            return
        end if
        write(*,'(3a,i0,a)') 'opened ', dir, ' (', db%ntables, ' tables)'
    end function

    function act_close(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        if (.not. db%opened) then
            call fail(rv, 'no database open')
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
        character(len=SQR_ERRMSG_LEN) :: emsg
        integer :: rs
        if (args%size() /= 2) then
            call fail(rv, 'usage: pack <dir> <file>')
            return
        end if
        n = args%get(1); dir  = node_as_char(n)
        n = args%get(2); file = node_as_char(n)
        emsg = ''
        call db_pack(dir, file, rs, emsg)
        if (rs /= SQR_OK) then
            call fail(rv, 'pack of "' // dir // '" failed: ' // trim(emsg))
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
            call fail(rv, 'usage: unpack <file> <dir>')
            return
        end if
        n = args%get(1); file = node_as_char(n)
        n = args%get(2); dir  = node_as_char(n)
        call db_unpack(file, dir, rs)
        if (rs /= SQR_OK) then
            block
                use sqr_consts, only: INT_TEXT_LEN
                character(len=INT_TEXT_LEN) :: code
                write(code, '(i0)') rs
                call fail(rv, 'unpack of "' // file // '" failed: ' // trim(code))
            end block
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
        call require_db_open(ok, rv); if (.not. ok) return
        call db_set_readonly(db, rs)
        if (rs == SQR_OK) then
            write(*,'(a)') 'database is now read-only (concurrent readers allowed)'
        else
            call fail(rv, with_reason('set read-only failed', engine_reason()))
        end if
    end function

    function act_tables(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        integer :: i
        logical :: ok
        call require_db_open(ok, rv); if (.not. ok) return
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
        call require_db_open(ok, rv); if (.not. ok) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: desc <table>')
            return
        end if
        n = args%get(1); name = node_as_char(n)
        ti = db_table_index(db, name)
        if (ti == 0) then
            call fail(rv, 'no such table: ' // trim(name))
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
                            cols, ' (', ix%nentries, ' entries, deleted rows included)'
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
        call require_db_open(ok, rv); if (.not. ok) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: drop <table>')
            return
        end if
        n = args%get(1); name = node_as_char(n)
        call db_drop_table(db, name, rs)
        if (rs == SQR_OK) then
            write(*,'(2a)') 'dropped ', trim(name)
        else
            call fail(rv, with_reason('drop failed: ' // trim(name), engine_reason()))
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
        call require_db_open(ok, rv); if (.not. ok) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: create <table>')
            return
        end if
        n = args%get(1); name = node_as_char(n)
        if (db_table_index(db, name) > 0) then
            call fail(rv, 'table already exists: ' // trim(name))
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
        call require_db_open(ok, rv); if (.not. ok) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: use <table>')
            return
        end if
        n = args%get(1); name = node_as_char(n)
        if (db_table_index(db, name) == 0) then
            call fail(rv, 'no such table: ' // trim(name))
            return
        end if
        rv%value = name
    end function

    ! ---- creator state ----

    ! Parse a `<name> int | real | text | char <N>` column spec from `args`
    ! starting at 1-based position `base`, into `col`. `cmd` names the calling
    ! command for the usage message. ok=.false. (with the failure set in rv) on
    ! a malformed type or a missing char width. Shared by `col` (creator) and
    ! `addcolumn` (table).
    subroutine parse_col_spec(args, base, cmd, col, ok, rv)
        type(dlist_t),         intent(in)    :: args
        integer,               intent(in)    :: base
        character(len=*),      intent(in)    :: cmd
        type(column_t),        intent(out)   :: col
        logical,               intent(out)   :: ok
        type(action_result_t), intent(inout) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=:), allocatable :: cname, ctype
        integer :: csize, cn
        logical :: cvt
        ok = .false.
        if (args%size() < base + 1) then
            call fail(rv, 'usage: ' // cmd // ' <name> int | real | text | char <N>')
            return
        end if
        n = args%get(base);     cname = node_as_char(n)
        n = args%get(base + 1); ctype = node_as_char(n)
        select case (ctype)
        case ('int', 'i')
            csize = SQR_INT_SIZE;  cn = DT_INT
        case ('real', 'r')
            csize = SQR_REAL_SIZE; cn = DT_REAL
        case ('text', 't')
            csize = SQR_TEXT_DESC; cn = DT_TEXT
        case ('char', 'c')
            if (args%size() < base + 2) then
                call fail(rv, 'usage: ' // cmd // ' <name> char <N>')
                return
            end if
            n = args%get(base + 2); csize = node_as_int(n, cvt)
            if (.not. cvt .or. csize < 1) then
                call fail(rv, 'char size must be a positive integer')
                return
            end if
            cn = DT_CHAR
        case default
            call fail(rv, 'unknown type: ' // ctype)
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
            call fail(rv, 'too many columns')
            return
        end if
        call parse_col_spec(args, 1, 'col', col, ok, rv)
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
        character(len=SQR_ERRMSG_LEN) :: emsg
        if (pending_ncols == 0) then
            call fail(rv, 'no columns defined — add some with `col` or `cancel`')
            return
        end if
        emsg = ''
        call db_create_table(db, ctx, pending_cols(1:pending_ncols), rs, emsg)
        if (rs /= SQR_OK) then
            call fail(rv, 'create failed: ' // trim(emsg))
            return
        end if
        write(*,'(3a,i0,a)') 'created table ', trim(ctx), ' with ', pending_ncols, ' columns'
        pending_ncols = 0
    end function

    ! ---- table-bound state ----

    ! A char value must fit its column: longer (trailing blanks aside, as in
    ! sqlsh) is refused rather than cut.
    function char_fits(c, val, rv) result(ok)
        type(column_t),        intent(in)    :: c
        character(len=*),      intent(in)    :: val
        type(action_result_t), intent(inout) :: rv
        logical :: ok
        ok = len_trim(val) <= c%csize
        if (.not. ok) call fail(rv, 'value too long for column "' // trim(c%name) // '"')
    end function

    ! Set column `c` of `buf` from one argument, the one decoding of a value
    ! that insert, update and the key commands share.  For a row (`pos` > 0,
    ! the column's position) `null` is NULL, a bad value is `col <pos>:
    ! expected ...`, and a text column is left for the caller to write.  For
    ! a key (`pos` = 0) `null` is ordinary text, a bad value is `value must
    ! be ...`, and a text column is refused.  A char value must fit.
    function decode_value(nd, c, pos, buf, rv) result(ok)
        use sqr_consts, only: INT_TEXT_LEN
        class(dlist_node_data_t), intent(in)    :: nd
        type(column_t),           intent(in)    :: c
        integer,                  intent(in)    :: pos
        character(len=*),         intent(inout) :: buf
        type(action_result_t),    intent(inout) :: rv
        logical :: ok
        integer(int32) :: ival
        real(real64)   :: rval
        character(len=INT_TEXT_LEN) :: where
        write(where, '(a,i0,a)') 'col ', pos, ': '
        if (pos > 0) then
            if (tok_is_null(nd)) then
                call row_set_null(buf, c)
                ok = .true.
                return
            end if
        end if
        select case (c%dtype)
        case (DT_INT)
            ival = node_as_int(nd, ok)
            if (ok) then
                call row_set_int(buf, c, ival)
            else if (pos > 0) then
                call fail(rv, trim(where) // ' expected integer')
            else
                call fail(rv, 'value must be an integer')
            end if
        case (DT_REAL)
            rval = node_as_real(nd, ok)
            if (ok) then
                call row_set_real(buf, c, rval)
            else if (pos > 0) then
                call fail(rv, trim(where) // ' expected number')
            else
                call fail(rv, 'value must be a number')
            end if
        case (DT_CHAR)
            ok = char_fits(c, node_as_char(nd), rv)
            if (ok) call row_set_char(buf, c, node_as_char(nd))
        case default
            ok = pos > 0                ! a text column: written by the caller
            if (.not. ok) call fail(rv, 'key column type not supported')
        end select
    end function

    ! Write the text columns of row `rid` from the arguments (the value of
    ! column i is argument i + `skip`); `null` leaves a column NULL.
    subroutine write_texts(ti, ctx, rid, args, skip, rs)
        integer,          intent(in)  :: ti
        character(len=*), intent(in)  :: ctx
        integer(int32),   intent(in)  :: rid
        type(dlist_t),    intent(in)  :: args
        integer,          intent(in)  :: skip
        integer,          intent(out) :: rs
        class(dlist_node_data_t), allocatable :: nd
        integer :: i
        rs = SQR_OK
        texts: do i = 1, db%tables(ti)%ncols
            if (db%tables(ti)%cols(i)%dtype /= DT_TEXT) cycle texts
            nd = args%get(i + skip)
            if (tok_is_null(nd)) cycle texts
            call db_set_text(db, ctx, rid, trim(db%tables(ti)%cols(i)%name), &
                             node_as_char(nd), rs)
            if (rs /= SQR_OK) return
        end do texts
    end subroutine

    ! End a failed one-step change: the engine's reason is taken, then the
    ! transaction rolled back.  The failure reported is `what: <reason>`,
    ! or the rollback's own failure if it could not finish.
    subroutine abandon(rv, what)
        type(action_result_t), intent(inout) :: rv
        character(len=*),      intent(in)    :: what
        character(len=:), allocatable :: why
        integer :: rs
        why = engine_reason()
        call db_rollback(db, rs)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('rollback failed', engine_reason()))
        else
            call fail(rv, with_reason(what, why))
        end if
    end subroutine

    ! insert: the row and its text values are one transaction, so a failure
    ! at any step leaves no row.
    function act_insert(args, ctx) result(rv)
        use sqr_consts, only: INT_TEXT_LEN
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nd
        integer :: ti, i, rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=INT_TEXT_LEN) :: num
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= db%tables(ti)%ncols) then
            write(num, '(i0)') db%tables(ti)%ncols
            call fail(rv, 'expected ' // trim(num) // ' values')
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        do i = 1, db%tables(ti)%ncols
            nd = args%get(i)
            if (.not. decode_value(nd, db%tables(ti)%cols(i), i, buf, rv)) return
        end do
        call db_begin(db, rs)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('insert failed', engine_reason()))
            return
        end if
        one_step: block
            call db_insert(db, ctx, buf, rid, rs)
            if (rs /= SQR_OK) exit one_step
            call write_texts(ti, ctx, rid, args, 0, rs)
            if (rs /= SQR_OK) exit one_step
            call db_commit(db, rs)     ! a failed commit has rolled back
            if (rs /= SQR_OK) then
                call fail(rv, with_reason('insert failed', engine_reason()))
                return
            end if
            write(*,'(a,i0)') 'inserted row ', rid
            return
        end block one_step
        call abandon(rv, 'insert failed')
    end function

    ! update: as insert, one transaction.  An id that is not a live row is
    ! `no such row`; any other failure gives the engine's reason.
    function act_update(args, ctx) result(rv)
        use sqr_consts, only: INT_TEXT_LEN
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nd
        integer :: ti, i, rs
        integer(int32) :: rid
        character(len=:), allocatable :: buf
        character(len=INT_TEXT_LEN) :: num
        logical :: ok
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= db%tables(ti)%ncols + 1) then
            write(num, '(i0)') db%tables(ti)%ncols
            call fail(rv, 'usage: update <row_id> followed by ' // trim(num) // ' values')
            return
        end if
        nd = args%get(1); rid = node_as_int(nd, ok)
        if (.not. ok) then
            call fail(rv, 'row_id must be an integer')
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        do i = 1, db%tables(ti)%ncols
            nd = args%get(i + 1)
            if (.not. decode_value(nd, db%tables(ti)%cols(i), i, buf, rv)) return
        end do
        write(num, '(i0)') rid
        call db_begin(db, rs)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('update failed', engine_reason()))
            return
        end if
        one_step: block
            call db_update(db, ctx, rid, buf, rs)
            if (rs == SQR_NOT_FOUND) then
                call db_rollback(db, rs)
                if (rs /= SQR_OK) then
                    call fail(rv, with_reason('rollback failed', engine_reason()))
                else
                    call fail(rv, 'update failed: no such row ' // trim(num))
                end if
                return
            end if
            if (rs /= SQR_OK) exit one_step
            call write_texts(ti, ctx, rid, args, 1, rs)
            if (rs /= SQR_OK) exit one_step
            call db_commit(db, rs)     ! a failed commit has rolled back
            if (rs /= SQR_OK) then
                call fail(rv, with_reason('update failed', engine_reason()))
                return
            end if
            write(*,'(a,i0)') 'updated row ', rid
            return
        end block one_step
        call abandon(rv, 'update failed')
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
        if (.not. require_ctx_table(ctx, ti, rv)) return
        call print_header(ti)
        call db_scan(db, ctx, select_emit_cb, ti, rs)
        if (rs /= SQR_OK) call fail(rv, with_reason('scan failed', engine_reason()))
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
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: get <row_id>')
            return
        end if
        n = args%get(1); rid = node_as_int(n, ok)
        if (.not. ok) then
            call fail(rv, 'row_id must be an integer')
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, ctx, rid, buf, rs)
        if (rs == SQR_NOT_FOUND) then
            call fail(rv, 'no such row: ' // int_text(rid))
            return
        else if (rs /= SQR_OK) then
            call fail(rv, with_reason('get failed', engine_reason()))
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
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: delete <row_id>')
            return
        end if
        n = args%get(1); rid = node_as_int(n, ok)
        if (.not. ok) then
            call fail(rv, 'row_id must be an integer')
            return
        end if
        call db_delete(db, ctx, rid, rs)
        if (rs == SQR_NOT_FOUND) then
            call fail(rv, 'no such row: ' // int_text(rid))
            return
        else if (rs /= SQR_OK) then
            call fail(rv, with_reason('delete failed', engine_reason()))
            return
        end if
        write(*,'(a,i0)') 'deleted row ', rid
    end function

    function act_compact(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: ti, rs
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 0) then
            call fail(rv, 'usage: compact')
            return
        end if
        call db_compact(db, ctx, rs)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('compact failed', engine_reason()))
            return
        end if
        write(*,'(a,i0,a)') 'compacted ', db%tables(ti)%live_count, ' live rows'
    end function

    function act_verify(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        integer :: ti, rs
        character(len=SQR_ERRMSG_LEN) :: emsg
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 0) then
            call fail(rv, 'usage: verify')
            return
        end if
        emsg = ''
        call db_verify(db, ctx, rs, emsg)
        if (rs == SQR_OK) then
            write(*,'(a)') 'verify: OK'
        else
            call fail(rv, 'verify: FAILED — ' // trim(emsg))
        end if
    end function

    function act_dropindex(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: n
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        integer :: ti, rs, nargs, k
        if (.not. require_ctx_table(ctx, ti, rv)) return
        nargs = args%size()
        if (nargs < 1) then
            call fail(rv, 'usage: dropindex <col> [<col> ...]')
            return
        end if
        allocate(cols(nargs))
        do k = 1, nargs
            n = args%get(k)
            cols(k) = node_as_char(n)
        end do
        call db_drop_index(db, ctx, cols, rs)
        if (rs /= SQR_OK) then
            call fail(rv, 'dropindex: no index over those columns')
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
        character(len=SQR_ERRMSG_LEN) :: emsg
        logical :: ok
        if (.not. require_ctx_table(ctx, ti, rv)) return
        call parse_col_spec(args, 1, 'addcolumn', col, ok, rv)
        if (.not. ok) return
        emsg = ''
        call db_add_column(db, ctx, col, rs, emsg)
        if (rs /= SQR_OK) then
            call fail(rv, 'addcolumn failed: ' // trim(emsg))
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
        character(len=SQR_ERRMSG_LEN) :: emsg
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 1) then
            call fail(rv, 'usage: dropcolumn <name>')
            return
        end if
        n = args%get(1); cname = node_as_char(n)
        emsg = ''
        call db_drop_column(db, ctx, cname, rs, emsg)
        if (rs /= SQR_OK) then
            call fail(rv, 'dropcolumn failed: ' // trim(emsg))
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
            call fail(rv, 'usage: index [unique] <col> [<col> ...]')
            return
        end if
        n = args%get(1)
        uniq  = trim(node_as_char(n)) == 'unique'
        first = merge(2, 1, uniq)
        if (nargs < first) then
            call fail(rv, 'usage: index [unique] <col> [<col> ...]')
            return
        end if
        allocate(cols(nargs - first + 1))
        do k = first, nargs
            n = args%get(k)
            cols(k - first + 1) = node_as_char(n)
        end do
        call db_create_index(db, ctx, cols, rs, unique=uniq)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('create_index failed', engine_reason()))
            return
        end if
        write(*,'(a,i0,a)') 'indexed ', size(cols), ' column(s)'
    end function

    ! Build a row-shaped buffer with just the key columns set, plus the
    ! ordered member-name array, from `col v` pairs. ti must be valid.
    subroutine parse_key_pairs(ti, args, buf, cols, ok, rv)
        integer,                                  intent(in)    :: ti
        type(dlist_t),                            intent(in)    :: args
        character(len=:),            allocatable, intent(out)   :: buf
        character(len=SQR_NAME_LEN), allocatable, intent(out)   :: cols(:)
        logical,                                  intent(out)   :: ok
        type(action_result_t),                    intent(inout) :: rv
        class(dlist_node_data_t), allocatable :: nc, nv
        integer :: np, p, ci
        ok = .false.
        if (mod(args%size(), 2) /= 0 .or. args%size() < 2) then
            call fail(rv, 'usage: <col> <value> [<col> <value> ...]')
            return
        end if
        np = args%size() / 2
        allocate(cols(np))
        call row_alloc(buf, db%tables(ti)%record_size)
        do p = 1, np
            nc = args%get(2*p - 1)
            nv = args%get(2*p)
            cols(p) = node_as_char(nc)
            ci = shell_col_index(ti, cols(p))
            if (ci == 0) then
                call fail(rv, 'no such column: ' // trim(cols(p)))
                return
            end if
            if (.not. decode_value(nv, db%tables(ti)%cols(ci), 0, buf, rv)) return
        end do
        ok = .true.
    end subroutine

    ! getk and delk: the key must be that of a unique index over exactly
    ! those columns; it is tested first, since the engine's status for "no
    ! such index" and "no such row" can be the same.
    function act_getk(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        character(len=SQR_NAME_LEN), allocatable :: cols(:)
        character(len=:), allocatable :: keyrow, buf
        integer :: ti, rs
        integer(int32) :: rid
        logical :: ok
        if (.not. require_ctx_table(ctx, ti, rv)) return
        call parse_key_pairs(ti, args, keyrow, cols, ok, rv)
        if (.not. ok) return
        if (.not. has_unique_index(ti, cols)) then
            call fail(rv, 'no unique index over those columns')
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get_by_key(db, ctx, cols, keyrow, buf, rs, row_id=rid)
        if (rs == SQR_NOT_FOUND) then
            call fail(rv, 'no row for that key')
            return
        else if (rs /= SQR_OK) then
            call fail(rv, with_reason('getk failed', engine_reason()))
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
        if (.not. require_ctx_table(ctx, ti, rv)) return
        call parse_key_pairs(ti, args, keyrow, cols, ok, rv)
        if (.not. ok) return
        if (.not. has_unique_index(ti, cols)) then
            call fail(rv, 'no unique index over those columns')
            return
        end if
        call db_delete_by_key(db, ctx, cols, keyrow, rs)
        if (rs == SQR_NOT_FOUND) then
            call fail(rv, 'no row for that key')
            return
        else if (rs /= SQR_OK) then
            call fail(rv, with_reason('delk failed', engine_reason()))
            return
        end if
        write(*,'(a)') 'deleted by key'
    end function

    ! find: the value is checked against the column, then the column must
    ! lead an index (as for range), then `no match` means just that.
    function act_find(args, ctx) result(rv)
        type(dlist_t),    intent(in) :: args
        character(len=*), intent(in) :: ctx
        type(action_result_t) :: rv
        class(dlist_node_data_t), allocatable :: nc, nv
        character(len=:), allocatable :: col, buf
        integer :: ti, ci, rs
        integer(int32) :: rid, ival
        real(real64)   :: rval
        logical :: ok
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 2) then
            call fail(rv, 'usage: find <col> <value>')
            return
        end if
        nc = args%get(1); col = node_as_char(nc)
        ci = shell_col_index(ti, col)
        if (ci == 0) then
            call fail(rv, 'no such column: ' // trim(col))
            return
        end if
        nv = args%get(2)
        associate (c => db%tables(ti)%cols(ci))
            select case (c%dtype)
            case (DT_INT)
                ival = node_as_int(nv, ok)
                if (.not. ok) call fail(rv, 'value must be an integer')
            case (DT_REAL)
                rval = node_as_real(nv, ok)
                if (.not. ok) call fail(rv, 'value must be a number')
            case (DT_CHAR)
                ok = char_fits(c, node_as_char(nv), rv)
            case default
                ok = .false.
                call fail(rv, 'find on this column type not supported')
            end select
            if (.not. ok) return
            if (leading_index(ti, col) == 0) then
                call fail(rv, 'no index on that column')
                return
            end if
            select case (c%dtype)
            case (DT_INT);  call db_find_by_int(db, ctx, col, ival, rid, rs)
            case (DT_REAL); call db_find_by_real(db, ctx, col, rval, rid, rs)
            case default;   call db_find_by_char(db, ctx, col, node_as_char(nv), rid, rs)
            end select
        end associate
        if (rs == SQR_NOT_FOUND) then
            call fail(rv, 'no match')
            return
        else if (rs /= SQR_OK) then
            call fail(rv, with_reason('find failed', engine_reason()))
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, ctx, rid, buf, rs)
        if (rs /= SQR_OK) then
            call fail(rv, with_reason('find failed', engine_reason()))
            return
        end if
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
        integer :: ti, ci, rs, nrows
        integer(int32) :: rid, ilo, ihi
        real(real64)   :: rlo, rhi
        logical :: ok, oklo, okhi
        type(db_cursor_t) :: cur
        if (.not. require_ctx_table(ctx, ti, rv)) return
        if (args%size() /= 3) then
            call fail(rv, 'usage: range <col> <lo> <hi>')
            return
        end if
        nc = args%get(1); col = node_as_char(nc)
        ci = shell_col_index(ti, col)
        if (ci == 0) then
            call fail(rv, 'no such column: ' // trim(col))
            return
        end if
        nlo = args%get(2); nhi = args%get(3)
        select case (db%tables(ti)%cols(ci)%dtype)
        case (DT_INT)
            ilo = node_as_int(nlo, oklo); ihi = node_as_int(nhi, okhi)
            if (.not. (oklo .and. okhi)) then
                call fail(rv, 'bounds must be integers')
                return
            end if
            call db_find_range(db, ctx, col, ilo, ihi, cur, rs)
        case (DT_REAL)
            rlo = node_as_real(nlo, oklo); rhi = node_as_real(nhi, okhi)
            if (.not. (oklo .and. okhi)) then
                call fail(rv, 'bounds must be numbers')
                return
            end if
            call db_find_range(db, ctx, col, rlo, rhi, cur, rs)
        case (DT_CHAR)
            call db_find_range(db, ctx, col, node_as_char(nlo), node_as_char(nhi), cur, rs)
        case default
            call fail(rv, 'range on this column type not supported')
            return
        end select
        if (rs /= SQR_OK) then
            call fail(rv, 'no index on that column')
            return
        end if
        call row_alloc(buf, db%tables(ti)%record_size)
        call print_header(ti)
        nrows = 0
        pull: do
            call db_cursor_next(db, cur, rid, buf, ok, rs)
            if (rs /= SQR_OK) then          ! I/O error / invalidated cursor
                call fail(rv, with_reason('error reading range', engine_reason()))
                return
            end if
            if (.not. ok) exit pull
            call print_row(ti, rid, buf)
            nrows = nrows + 1
        end do pull
        if (nrows == 0) write(*,'(a)') '(no rows in range)'
    end function

    ! ---- row printing ----

    ! One field of the row display: two blanks, then the text right-justified
    ! in FIELD_WIDTH characters; a longer text is written in full and the
    ! rest of the line moves right.
    subroutine put_field(text)
        character(len=*), intent(in) :: text
        write(*,'(a)', advance='no') '  ' // &
            repeat(' ', max(0, FIELD_WIDTH - char_count(text))) // text
    end subroutine

    subroutine print_header(ti)
        integer, intent(in) :: ti
        integer :: i
        write(*,'(a8)', advance='no') 'row_id'
        do i = 1, db%tables(ti)%ncols
            associate (c => db%tables(ti)%cols(i))
                call put_field(trim(c%name))
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
                    call put_field('NULL')
                    cycle
                end if
                select case (c%dtype)
                case (DT_INT)
                    write(*,'(2x,i16)', advance='no') row_get_int(buf, c)
                case (DT_REAL)
                    call put_field(real_text(row_get_real(buf, c)))
                case (DT_CHAR)
                    call put_field(trim(row_get_char(buf, c)))
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
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, iostat_end
    use cmdgraph
    use sqr
    use sqrsh_state
    use sqrsh_actions
    use sqrsh_extras, only: add_extra_commands
    use :: clib_wrap, only: c_exit, c_isatty_stdin
    implicit none

    ! The prompt of each state, written only when input is a terminal.
    character(len=*), parameter :: PROMPT_ROOT    = 'sqr> '
    character(len=*), parameter :: PROMPT_CREATOR = 'cols> '
    character(len=*), parameter :: PROMPT_TABLE   = 'row> '

    type(engine_t) :: ui
    character(len=SQR_MAX_DIR) :: arg
    logical :: interactive
    logical :: any_failed = .false.   ! a command, the start-up open or the encoding failed: status 1

    ! root state — database-level commands
    call ui%add_state("root", prompt=PROMPT_ROOT)
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
    call ui%add_state("creator", prompt=PROMPT_CREATOR)
    call ui%add_command("creator", "col",     EDGE_ACTION,  proc=act_col,  &
                        help="col <name> int | real | text | char <N>")
    call ui%add_command("creator", "done",    EDGE_DO_POP,  proc=act_done, &
                        help="commit the table")
    call ui%add_command("creator", "cancel",  EDGE_POP,                    help="discard pending columns")
    call ui%add_command("creator", "q(uit)",  EDGE_QUIT,                   help="exit")

    ! table-bound state — operations on a selected table (name in ctx)
    call ui%add_state("table", prompt=PROMPT_TABLE)
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
    call ui%add_command("table", "ind(ex)",  EDGE_ACTION, proc=act_index, &
         help="index [unique] <col> [<col> ...]")
    call ui%add_command("table", "dropindex", EDGE_ACTION, proc=act_dropindex, &
         help="dropindex <col> [<col> ...] (remove a secondary index)")
    call ui%add_command("table", "v(erify)", EDGE_ACTION, proc=act_verify, &
         help="verify table/index integrity")
    call ui%add_command("table", "f(ind)",   EDGE_ACTION, proc=act_find,   help="find <col> <value>")
    call ui%add_command("table", "r(ange)",  EDGE_ACTION, proc=act_range, &
         help="range <col> <lo> <hi> (indexed rows in [lo,hi], ascending)")
    call add_extra_commands(ui)
    call ui%add_command("table", "getk",     EDGE_ACTION, proc=act_getk, &
         help="getk <col> <val> [<col> <val> ...] (by unique key)")
    call ui%add_command("table", "delk",     EDGE_ACTION, proc=act_delk, &
         help="delk <col> <val> [<col> <val> ...] (by unique key)")
    call ui%add_command("table", "b(ack)",   EDGE_POP,                    help="back to root")
    call ui%add_command("table", "q(uit)",   EDGE_QUIT,                   help="exit")

    call ui%finalize("root")

    ! Optional positional arg: open this directory at startup.  sqrsh has
    ! no options, so a leading '-' is a mistake and must not become a
    ! database directory; one really so named is given as ./-name.
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        if (arg(1:1) == '-') then
            write(error_unit,'(a)') 'sqrsh: unknown option: ' // trim(arg)
            write(error_unit,'(a)') 'usage: sqrsh [<db-dir>]'
            flush(error_unit)
            call c_exit(2)
        end if
        call open_initial(trim(arg))
    end if

    interactive = c_isatty_stdin()
    if (interactive) write(output_unit,'(a)') 'sqr shell — type `help` for commands'
    call read_loop()

    if (db%opened) call db_close(db)
    call quit(merge(1, 0, any_failed))

contains

    !! Read and dispatch lines until `quit` or the end of the input, as
    !! cmdgraph's own `run` does but with whole lines, prompts only at a
    !! terminal, and every failed command remembered for the exit status.
    !! The first line loses a UTF-8 byte-order mark and is screened for
    !! another encoding's signature; every line is screened for NUL bytes
    !! (a wide encoding without a mark).  A refused encoding ends the loop.
    subroutine read_loop()
        character(len=:), allocatable :: line
        logical :: first
        integer :: enc, rc
        first = .true.
        lines: do while (ui%is_running())
            if (interactive) then
                write(output_unit,'(a)', advance='no') prompt_now()
                flush(output_unit)
            end if
            call read_line(line)
            if (.not. allocated(line)) exit lines
            if (first) then
                call strip_bom(line, enc)
            else
                enc = merge(ENC_WIDE_NO_BOM, ENC_PLAIN, index(line, achar(0)) > 0)
            end if
            first = .false.
            if (.not. encoding_supported(enc)) then
                write(error_unit,'(a)') 'unsupported encoding (' // encoding_name(enc) // &
                                        '); expected ASCII or UTF-8'
                any_failed = .true.
                exit lines
            end if
            rc = ui%dispatch(trim(line))
            if (rc == RC_UNKNOWN .or. rc == RC_AMBIGUOUS .or. rc == RC_ERROR) any_failed = .true.
        end do lines
    end subroutine

    !! The prompt of the current state.
    function prompt_now() result(p)
        character(len=:), allocatable :: p
        select case (ui%current_state())
        case ('creator'); p = PROMPT_CREATOR
        case ('table');   p = PROMPT_TABLE
        case default;     p = PROMPT_ROOT
        end select
    end function

    !! Read one line of any length from standard input into `line`; leaves
    !! it unallocated at the end of the input.
    subroutine read_line(line)
        character(len=:), allocatable, intent(out) :: line
        integer, parameter :: CHUNK_LEN = 512   ! bytes read per non-advancing read
        character(len=CHUNK_LEN) :: chunk
        integer :: ios, n
        line = ''
        do
            read(*, '(a)', advance='no', size=n, iostat=ios) chunk
            if (n > 0) line = line // chunk(1:n)
            if (ios == iostat_end) then
                if (len(line) == 0) deallocate(line)   ! true end, nothing buffered
                return
            end if
            if (is_iostat_eor(ios)) return             ! end of record: done
            if (ios /= 0) return                       ! other error: take what we have
        end do
    end subroutine

    !! End the process with `code`, both streams flushed first (c_exit is the
    !! C library's exit, which knows nothing of Fortran's buffers).
    subroutine quit(code)
        integer, intent(in) :: code
        flush(output_unit)
        flush(error_unit)
        call c_exit(code)
    end subroutine

    subroutine open_initial(dir)
        character(len=*), intent(in) :: dir
        integer :: rs
        character(len=SQR_ERRMSG_LEN) :: emsg
        emsg = ''
        call db_open(db, dir, rs, emsg)
        if (rs /= SQR_OK) then
            write(error_unit,'(a)') 'open ' // dir // ' failed: ' // trim(emsg)
            any_failed = .true.
        else
            write(output_unit,'(3a,i0,a)') 'opened ', dir, ' (', db%ntables, ' tables)'
        end if
    end subroutine

end program sqrsh

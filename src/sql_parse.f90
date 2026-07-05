!! `sql` parser submodule: recursive descent from a token stream into one
!! `sql_stmt_t`.  Shares the helpers of `sql_base` by host association.  All
!! parse helpers are internal to `sql_parse`, sharing the token cursor (`p`),
!! the token array and the error state through host scope — so the parser is
!! reentrant (no submodule-level state).
!!
!! The lexer `sql_lex` is implemented here too (rather than in `sql_base`):
!! it is only called by the parser, and co-locating it with `sql_parse`
!! avoids a GCC 16.1.0 ICE on a cross-submodule call to a module procedure
!! with an `allocatable, intent(out)` derived-type array dummy.

submodule (sql:sql_base) sql_parser
    implicit none

contains

    ! ===== lexer =====

    module subroutine sql_lex(text, toks, ntok, stat, errmsg)
        character(len=*),               intent(in)              :: text
        type(sql_token_t), allocatable, intent(out)             :: toks(:)
        integer,                        intent(out)             :: ntok
        integer,                        intent(out),   optional :: stat
        character(len=*),               intent(inout), optional :: errmsg

        integer :: i, n, start
        character :: ch

        if (present(stat)) stat = SQR_OK
        n = len(text)
        ntok = 0
        allocate(toks(max(1, n)))

        i = 1
        scan: do while (i <= n)
            ch = text(i:i)

            ! whitespace
            if (ch == ' ' .or. ch == char(9)) then
                i = i + 1
                cycle scan
            end if

            ! word: identifier or keyword
            if (is_alpha(ch) .or. ch == '_') then
                start = i
                do while (i <= n)
                    if (.not. (is_alnum(text(i:i)) .or. text(i:i) == '_')) exit
                    i = i + 1
                end do
                call push(TK_IDENT, text(start:i-1), start)
                cycle scan
            end if

            ! number
            if (is_digit(ch)) then
                call lex_number(text, n, i, start)
                cycle scan
            end if

            ! quoted string
            if (ch == "'") then
                call lex_string(text, n, i, start)
                if (i < 0) return   ! lex_string flagged an error
                cycle scan
            end if

            ! two-char operators
            if (i < n) then
                select case (text(i:i+1))
                case ('<=', '>=', '<>')
                    call push(TK_PUNCT, text(i:i+1), i)
                    i = i + 2
                    cycle scan
                end select
            end if

            ! single-char punctuation / operators
            select case (ch)
            case ('(', ')', ',', '*', ';', '=', '<', '>', '+', '-', '.')
                call push(TK_PUNCT, ch, i)
                i = i + 1
                cycle scan
            end select

            ! anything else is a lexical error
            call set_err(stat, errmsg, SQR_INVALID, &
                'col ' // itoa(i) // ': unexpected character "' // ch // '"')
            return
        end do scan

    contains

        subroutine push(kind, txt, col)
            integer,          intent(in) :: kind
            character(len=*), intent(in) :: txt
            integer,          intent(in) :: col
            ntok = ntok + 1
            toks(ntok)%kind = kind
            toks(ntok)%text = txt
            toks(ntok)%col  = col
        end subroutine

        ! Lex a number starting at i: integer unless it carries a '.' or an
        ! exponent, in which case it is real.  Advances i past the number.
        subroutine lex_number(s, slen, i, start)
            character(len=*), intent(in)    :: s
            integer,          intent(in)    :: slen
            integer,          intent(inout) :: i
            integer,          intent(out)   :: start
            logical :: is_real
            start = i
            is_real = .false.
            do while (i <= slen)
                if (.not. is_digit(s(i:i))) exit
                i = i + 1
            end do
            if (i <= slen) then
                if (s(i:i) == '.') then
                    is_real = .true.
                    i = i + 1
                    do while (i <= slen)
                        if (.not. is_digit(s(i:i))) exit
                        i = i + 1
                    end do
                end if
            end if
            if (i <= slen) then
                if (s(i:i) == 'e' .or. s(i:i) == 'E') then
                    is_real = .true.
                    i = i + 1
                    if (i <= slen) then
                        if (s(i:i) == '+' .or. s(i:i) == '-') i = i + 1
                    end if
                    do while (i <= slen)
                        if (.not. is_digit(s(i:i))) exit
                        i = i + 1
                    end do
                end if
            end if
            if (is_real) then
                call push(TK_REAL, s(start:i-1), start)
            else
                call push(TK_INT, s(start:i-1), start)
            end if
        end subroutine

        ! Lex a single-quoted string starting at the opening quote (i).  A
        ! doubled '' is an escaped quote.  On an unterminated string set the
        ! error and signal the caller by setting i = -1.
        subroutine lex_string(s, slen, i, start)
            character(len=*), intent(in)    :: s
            integer,          intent(in)    :: slen
            integer,          intent(inout) :: i
            integer,          intent(out)   :: start
            character(len=:), allocatable :: body
            start = i
            body = ''
            i = i + 1   ! past opening quote
            do
                if (i > slen) then
                    call set_err(stat, errmsg, SQR_INVALID, &
                        'col ' // itoa(start) // ': unterminated string')
                    i = -1
                    return
                end if
                if (s(i:i) == "'") then
                    if (i < slen) then
                        if (s(i+1:i+1) == "'") then   ! escaped quote
                            body = body // "'"
                            i = i + 2
                            cycle
                        end if
                    end if
                    i = i + 1   ! closing quote
                    exit
                end if
                body = body // s(i:i)
                i = i + 1
            end do
            ntok = ntok + 1
            toks(ntok)%kind = TK_STR
            toks(ntok)%text = body
            toks(ntok)%col  = start
        end subroutine

    end subroutine sql_lex

    pure function is_alpha(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

    pure function is_digit(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = c >= '0' .and. c <= '9'
    end function

    pure function is_alnum(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = is_alpha(c) .or. is_digit(c)
    end function

    module subroutine sql_parse(text, stmt, stat, errmsg)
        character(len=*),  intent(in)              :: text
        type(sql_stmt_t),  intent(out)             :: stmt
        integer,           intent(out),  optional  :: stat
        character(len=*),  intent(inout), optional :: errmsg

        type(sql_token_t), allocatable :: toks(:)
        integer :: ntok, p, eofcol, lerr
        logical :: had_err
        character(len=:), allocatable :: emsg
        character(len=160) :: lmsg

        if (present(stat)) stat = SQR_OK
        had_err = .false.
        emsg = ''

        lmsg = ''
        call sql_lex(text, toks, ntok, lerr, lmsg)
        if (lerr /= SQR_OK) then
            call set_err(stat, errmsg, SQR_INVALID, trim(lmsg))
            return
        end if

        ! Column to point at for an "unexpected end of input" error.
        if (ntok > 0) then
            eofcol = toks(ntok)%col + len(toks(ntok)%text)
        else
            eofcol = 1
        end if

        if (ntok == 0) then
            stmt%kind = ST_NONE     ! blank line — a no-op for the caller
            return
        end if

        p = 1
        if (is_punct(tk(), ';') .and. ntok == 1) then
            stmt%kind = ST_NONE     ! a bare ';' — an empty statement, no-op
            return
        end if
        call parse_statement()
        if (had_err) then
            call set_err(stat, errmsg, SQR_INVALID, emsg)
            return
        end if

        ! Optional trailing ';', then nothing else.
        if (is_punct(tk(), ';')) call adv()
        if (p <= ntok) then
            call errf(cur_col(), 'unexpected token "' // cur_text() // '" after statement')
            call set_err(stat, errmsg, SQR_INVALID, emsg)
        end if

    contains

        ! ---- cursor / diagnostics ----

        function tk() result(t)
            type(sql_token_t) :: t
            if (p <= ntok) then
                t = toks(p)
            else
                t%kind = TK_EOF
                t%text = ''
                t%col  = eofcol
            end if
        end function

        subroutine adv()
            p = p + 1
        end subroutine

        ! Scalar accessors for the current token's fields.  Fortran forbids a
        ! function reference as the leftmost part-ref of a data-ref (`cur_col()`
        ! is illegal), so component access goes through these.
        function cur_col() result(c)
            integer :: c
            type(sql_token_t) :: t
            t = tk()
            c = t%col
        end function

        function cur_kind() result(k)
            integer :: k
            type(sql_token_t) :: t
            t = tk()
            k = t%kind
        end function

        function cur_text() result(s)
            character(len=:), allocatable :: s
            type(sql_token_t) :: t
            t = tk()
            if (allocated(t%text)) then
                s = t%text
            else
                s = ''
            end if
        end function

        ! Record the first parse error (later ones are suppressed).
        subroutine errf(col, msg)
            integer,          intent(in) :: col
            character(len=*), intent(in) :: msg
            if (had_err) return
            had_err = .true.
            emsg = 'col ' // itoa(col) // ': ' // msg
        end subroutine

        ! Consume the punctuation `s` or record an error.
        subroutine eat_punct(s)
            character(len=*), intent(in) :: s
            if (is_punct(tk(), s)) then
                call adv()
            else
                call errf(cur_col(), 'expected "' // s // '"')
            end if
        end subroutine

        ! Consume the keyword `kw` or record an error.
        subroutine eat_kw(kw)
            character(len=*), intent(in) :: kw
            if (is_kw(tk(), kw)) then
                call adv()
            else
                call errf(cur_col(), 'expected ' // lower(kw))
            end if
        end subroutine

        ! Take an identifier into a fixed-width name, checking length.
        subroutine take_name(name, what)
            character(len=SQR_NAME_LEN), intent(out) :: name
            character(len=*),            intent(in)  :: what
            name = ''
            if (cur_kind() /= TK_IDENT) then
                call errf(cur_col(), 'expected ' // what)
                return
            end if
            if (len(cur_text()) > SQR_NAME_LEN) then
                call errf(cur_col(), what // ' too long (max ' // itoa(SQR_NAME_LEN) // ')')
                return
            end if
            name = cur_text()
            call adv()
        end subroutine

        ! ---- statement dispatch ----

        subroutine parse_statement()
            if      (is_kw(tk(), 'select'))   then; call parse_select()
            else if (is_kw(tk(), 'insert'))   then; call parse_insert()
            else if (is_kw(tk(), 'update'))   then; call parse_update()
            else if (is_kw(tk(), 'delete'))   then; call parse_delete()
            else if (is_kw(tk(), 'create'))   then; call parse_create()
            else if (is_kw(tk(), 'drop'))     then; call parse_drop()
            else if (is_kw(tk(), 'alter'))    then; call parse_alter()
            else if (is_kw(tk(), 'begin'))    then; call adv(); stmt%kind = ST_BEGIN
            else if (is_kw(tk(), 'commit'))   then; call adv(); stmt%kind = ST_COMMIT
            else if (is_kw(tk(), 'rollback')) then; call adv(); stmt%kind = ST_ROLLBACK
            else
                call errf(cur_col(), 'unknown statement "' // cur_text() // '"')
            end if
        end subroutine

        ! ---- types ----

        ! Parse a column type into `col` (name set by the caller).  Handles
        ! INTEGER/INT, REAL, TEXT, CHAR(n)/CHARACTER(n).
        subroutine parse_type(col)
            type(column_t), intent(inout) :: col
            integer :: w
            logical :: ok
            if (cur_kind() /= TK_IDENT) then
                call errf(cur_col(), 'expected a column type')
                return
            end if
            select case (lower(cur_text()))
            case ('integer', 'int')
                col%dtype = DT_INT;  col%csize = 4;  call adv()
            case ('real')
                col%dtype = DT_REAL; col%csize = 8;  call adv()
            case ('text')
                col%dtype = DT_TEXT; col%csize = SQR_TEXT_DESC; call adv()
            case ('char', 'character')
                call adv()
                call eat_punct('(')
                if (had_err) return
                call read_int_token(w, ok)
                if (.not. ok) then
                    call errf(cur_col(), 'CHAR width must be a positive integer')
                    return
                end if
                call adv()
                call eat_punct(')')
                if (w < 1) then
                    call errf(cur_col(), 'CHAR width must be positive')
                    return
                end if
                col%dtype = DT_CHAR; col%csize = w
            case default
                call errf(cur_col(), 'unknown column type "' // cur_text() // '"')
            end select
        end subroutine

        ! ---- literals ----

        ! Read the current TK_INT token's value (no advance).
        subroutine read_int_token(v, ok)
            integer, intent(out) :: v
            logical, intent(out) :: ok
            integer :: ios
            character(len=:), allocatable :: s
            v = 0; ok = .false.
            if (cur_kind() /= TK_INT) return
            s = cur_text()
            read(s, *, iostat=ios) v
            ok = ios == 0
        end subroutine

        ! Parse a literal value: optional leading +/- sign then an integer,
        ! real, string, or the keyword NULL.
        subroutine parse_literal(lit)
            type(sql_lit_t), intent(out) :: lit
            logical :: neg
            integer :: ios
            integer(int64) :: big
            character(len=:), allocatable :: s
            neg = .false.
            if (is_punct(tk(), '-')) then
                neg = .true.;  call adv()
            else if (is_punct(tk(), '+')) then
                call adv()
            end if
            select case (cur_kind())
            case (TK_INT)
                lit%ltype = LIT_INT
                s = cur_text()
                ! The token is an unsigned magnitude; the sign is applied
                ! afterwards.  Read in int64 and range-check the *signed*
                ! value so -2147483648 (whose magnitude overflows int32)
                ! is accepted.
                read(s, *, iostat=ios) big
                if (ios /= 0) then
                    call errf(cur_col(), 'integer literal out of range')
                    return
                end if
                if (neg) big = -big
                if (big > int(huge(0_int32), int64) .or. &
                    big < -int(huge(0_int32), int64) - 1_int64) then
                    call errf(cur_col(), 'integer literal out of range')
                    return
                end if
                lit%ival = int(big, int32)
                call adv()
            case (TK_REAL)
                lit%ltype = LIT_REAL
                s = cur_text()
                read(s, *, iostat=ios) lit%rval
                if (ios /= 0) then
                    call errf(cur_col(), 'malformed real literal')
                    return
                end if
                if (neg) lit%rval = -lit%rval
                call adv()
            case (TK_STR)
                if (neg) then
                    call errf(cur_col(), 'sign not allowed before a string literal')
                    return
                end if
                lit%ltype = LIT_STR
                lit%sval  = cur_text()
                call adv()
            case default
                if (is_kw(tk(), 'null')) then
                    if (neg) then
                        call errf(cur_col(), 'sign not allowed before NULL')
                        return
                    end if
                    lit%ltype = LIT_NULL
                    call adv()
                else
                    call errf(cur_col(), 'expected a literal value')
                end if
            end select
        end subroutine

        ! ---- name lists ----

        ! Parse `name [, name]*` into `out(1:nout)` (caller pre-sizes `out`).
        subroutine parse_name_list(out, nout, what)
            character(len=SQR_NAME_LEN), intent(inout) :: out(:)
            integer,                     intent(out)   :: nout
            character(len=*),            intent(in)    :: what
            nout = 0
            do
                if (nout >= size(out)) then
                    call errf(cur_col(), 'too many ' // what)
                    return
                end if
                nout = nout + 1
                call take_name(out(nout), what)
                if (had_err) return
                if (.not. is_punct(tk(), ',')) exit
                call adv()
            end do
        end subroutine

        ! ---- WHERE (DNF: OR of AND-groups) ----

        subroutine parse_where()
            type(sql_cond_group_t), allocatable :: groups(:)
            type(sql_cond_group_t) :: g
            integer :: ng
            allocate(groups(0))
            ng = 0
            do
                call parse_and_group(g)
                if (had_err) return
                groups = [groups, g]
                ng = ng + 1
                if (.not. is_kw(tk(), 'or')) exit
                call adv()
            end do
            stmt%has_where    = .true.
            stmt%where_groups = groups
        end subroutine

        subroutine parse_and_group(g)
            type(sql_cond_group_t), intent(out) :: g
            type(sql_cond_t) :: c
            allocate(g%conds(0))
            do
                call parse_cond(c)
                if (had_err) return
                g%conds = [g%conds, c]
                if (.not. is_kw(tk(), 'and')) exit
                call adv()
            end do
        end subroutine

        subroutine parse_cond(c)
            type(sql_cond_t), intent(out) :: c
            call take_name(c%col, 'column name')
            if (had_err) return
            if (is_kw(tk(), 'is')) then
                call adv()
                if (is_kw(tk(), 'not')) then
                    call adv()
                    c%op = OP_ISNOTNULL
                else
                    c%op = OP_ISNULL
                end if
                call eat_kw('null')
            else if (is_kw(tk(), 'between')) then
                call adv()
                c%op = OP_BETWEEN
                call parse_literal(c%lit)
                if (had_err) return
                call eat_kw('and')
                if (had_err) return
                call parse_literal(c%lit2)
            else
                call parse_compare_op(c%op)
                if (had_err) return
                call parse_literal(c%lit)
            end if
        end subroutine

        subroutine parse_compare_op(op)
            integer, intent(out) :: op
            op = 0
            if (cur_kind() /= TK_PUNCT) then
                call errf(cur_col(), 'expected a comparison operator')
                return
            end if
            select case (cur_text())
            case ('=');  op = OP_EQ
            case ('<>'); op = OP_NE
            case ('<');  op = OP_LT
            case ('<='); op = OP_LE
            case ('>');  op = OP_GT
            case ('>='); op = OP_GE
            case default
                call errf(cur_col(), 'expected a comparison operator')
                return
            end select
            call adv()
        end subroutine

        ! Optional trailing WHERE / ORDER BY / LIMIT, shared by SELECT (all
        ! three) and DELETE/UPDATE (WHERE only — they pass want_order=.false.).
        subroutine parse_tail(want_order)
            logical, intent(in) :: want_order
            integer :: n
            logical :: ok
            if (is_kw(tk(), 'where')) then
                call adv()
                call parse_where()
                if (had_err) return
            end if
            if (.not. want_order) return
            if (is_kw(tk(), 'order')) then
                call adv()
                call eat_kw('by')
                if (had_err) return
                call take_name(stmt%order_col, 'column name')
                if (had_err) return
                stmt%has_order = .true.
                if (is_kw(tk(), 'asc')) then
                    call adv()
                else if (is_kw(tk(), 'desc')) then
                    call adv(); stmt%order_desc = .true.
                end if
            end if
            if (is_kw(tk(), 'limit')) then
                call adv()
                call read_int_token(n, ok)
                if (.not. ok .or. n < 0) then
                    call errf(cur_col(), 'LIMIT must be a non-negative integer')
                    return
                end if
                call adv()
                stmt%has_limit = .true.
                stmt%limit_n   = n
            end if
        end subroutine

        ! ---- SELECT ----

        subroutine parse_select()
            character(len=SQR_NAME_LEN) :: cols(256)
            integer :: nc
            call adv()   ! SELECT
            stmt%kind = ST_SELECT
            if (is_punct(tk(), '*')) then
                stmt%select_star = .true.
                call adv()
            else
                call parse_name_list(cols, nc, 'projection columns')
                if (had_err) return
                stmt%names = cols(1:nc)
            end if
            call eat_kw('from')
            if (had_err) return
            call take_name(stmt%table, 'table name')
            if (had_err) return
            call parse_tail(want_order=.true.)
        end subroutine

        ! ---- INSERT ----

        subroutine parse_insert()
            character(len=SQR_NAME_LEN) :: cols(256)
            integer :: nc, ncols, nrows
            type(sql_lit_t), allocatable :: flat(:)
            type(sql_lit_t) :: lit
            integer :: row_w, cap
            call adv()   ! INSERT
            call eat_kw('into')
            if (had_err) return
            stmt%kind = ST_INSERT
            call take_name(stmt%table, 'table name')
            if (had_err) return
            if (is_punct(tk(), '(')) then
                call adv()
                call parse_name_list(cols, nc, 'column names')
                if (had_err) return
                call eat_punct(')')
                if (had_err) return
                stmt%names = cols(1:nc)
                stmt%insert_named = .true.
            end if
            call eat_kw('values')
            if (had_err) return
            ! one or more parenthesised literal tuples
            allocate(flat(0))
            ncols = -1
            nrows = 0
            do
                call eat_punct('(')
                if (had_err) return
                row_w = 0
                do
                    call parse_literal(lit)
                    if (had_err) return
                    flat = [flat, lit]
                    row_w = row_w + 1
                    if (.not. is_punct(tk(), ',')) exit
                    call adv()
                end do
                call eat_punct(')')
                if (had_err) return
                if (ncols < 0) then
                    ncols = row_w
                else if (row_w /= ncols) then
                    call errf(cur_col(), 'every VALUES tuple must have the same number of values')
                    return
                end if
                nrows = nrows + 1
                if (.not. is_punct(tk(), ',')) exit
                call adv()
            end do
            if (stmt%insert_named) then
                if (ncols /= size(stmt%names)) then
                    call errf(cur_col(), 'VALUES count does not match the column list')
                    return
                end if
            end if
            allocate(stmt%values(nrows, ncols))
            cap = 0
            block
                integer :: r, c
                do r = 1, nrows
                    do c = 1, ncols
                        cap = cap + 1
                        stmt%values(r, c) = flat(cap)
                    end do
                end do
            end block
        end subroutine

        ! ---- UPDATE ----

        subroutine parse_update()
            character(len=SQR_NAME_LEN), allocatable :: scols(:)
            type(sql_lit_t),             allocatable :: svals(:)
            character(len=SQR_NAME_LEN) :: cname
            type(sql_lit_t) :: lit
            call adv()   ! UPDATE
            stmt%kind = ST_UPDATE
            call take_name(stmt%table, 'table name')
            if (had_err) return
            call eat_kw('set')
            if (had_err) return
            allocate(scols(0), svals(0))
            do
                call take_name(cname, 'column name')
                if (had_err) return
                call eat_punct('=')
                if (had_err) return
                call parse_literal(lit)
                if (had_err) return
                scols = [scols, cname]
                svals = [svals, lit]
                if (.not. is_punct(tk(), ',')) exit
                call adv()
            end do
            stmt%set_cols = scols
            stmt%set_vals = svals
            call parse_tail(want_order=.false.)
        end subroutine

        ! ---- DELETE ----

        subroutine parse_delete()
            call adv()   ! DELETE
            call eat_kw('from')
            if (had_err) return
            stmt%kind = ST_DELETE
            call take_name(stmt%table, 'table name')
            if (had_err) return
            call parse_tail(want_order=.false.)
        end subroutine

        ! ---- CREATE (TABLE | [UNIQUE] INDEX) ----

        subroutine parse_create()
            call adv()   ! CREATE
            if (is_kw(tk(), 'table')) then
                call adv()
                call parse_create_table()
            else if (is_kw(tk(), 'unique') .or. is_kw(tk(), 'index')) then
                call parse_create_index()
            else
                call errf(cur_col(), 'expected TABLE, INDEX or UNIQUE after CREATE')
            end if
        end subroutine

        subroutine parse_create_table()
            type(column_t) :: defs(256)
            integer :: nd
            stmt%kind = ST_CREATE_TABLE
            call take_name(stmt%table, 'table name')
            if (had_err) return
            call eat_punct('(')
            if (had_err) return
            nd = 0
            do
                if (nd >= size(defs)) then
                    call errf(cur_col(), 'too many columns')
                    return
                end if
                nd = nd + 1
                call take_name(defs(nd)%name, 'column name')
                if (had_err) return
                call parse_type(defs(nd))
                if (had_err) return
                if (.not. is_punct(tk(), ',')) exit
                call adv()
            end do
            call eat_punct(')')
            if (had_err) return
            stmt%coldefs = defs(1:nd)
        end subroutine

        subroutine parse_create_index()
            character(len=SQR_NAME_LEN) :: cols(64)
            integer :: nc
            if (is_kw(tk(), 'unique')) then
                call adv()
                stmt%unique = .true.
            end if
            call eat_kw('index')
            if (had_err) return
            call eat_kw('on')
            if (had_err) return
            stmt%kind = ST_CREATE_INDEX
            call take_name(stmt%table, 'table name')
            if (had_err) return
            call eat_punct('(')
            if (had_err) return
            call parse_name_list(cols, nc, 'index columns')
            if (had_err) return
            call eat_punct(')')
            if (had_err) return
            stmt%names = cols(1:nc)
        end subroutine

        ! ---- DROP (TABLE | INDEX) ----

        subroutine parse_drop()
            character(len=SQR_NAME_LEN) :: cols(64)
            integer :: nc
            call adv()   ! DROP
            if (is_kw(tk(), 'table')) then
                call adv()
                stmt%kind = ST_DROP_TABLE
                call take_name(stmt%table, 'table name')
            else if (is_kw(tk(), 'index')) then
                call adv()
                call eat_kw('on')
                if (had_err) return
                stmt%kind = ST_DROP_INDEX
                call take_name(stmt%table, 'table name')
                if (had_err) return
                call eat_punct('(')
                if (had_err) return
                call parse_name_list(cols, nc, 'index columns')
                if (had_err) return
                call eat_punct(')')
                if (had_err) return
                stmt%names = cols(1:nc)
            else
                call errf(cur_col(), 'expected TABLE or INDEX after DROP')
            end if
        end subroutine

        ! ---- ALTER TABLE t (ADD|DROP) COLUMN ----

        subroutine parse_alter()
            type(column_t) :: col
            character(len=SQR_NAME_LEN) :: cname
            call adv()   ! ALTER
            call eat_kw('table')
            if (had_err) return
            call take_name(stmt%table, 'table name')
            if (had_err) return
            if (is_kw(tk(), 'add')) then
                call adv()
                call eat_kw('column')
                if (had_err) return
                stmt%kind = ST_ADD_COLUMN
                call take_name(col%name, 'column name')
                if (had_err) return
                call parse_type(col)
                if (had_err) return
                stmt%coldefs = [col]
            else if (is_kw(tk(), 'drop')) then
                call adv()
                call eat_kw('column')
                if (had_err) return
                stmt%kind = ST_DROP_COLUMN
                call take_name(cname, 'column name')
                if (had_err) return
                stmt%names = [cname]
            else
                call errf(cur_col(), 'expected ADD or DROP after ALTER TABLE')
            end if
        end subroutine

    end subroutine sql_parse

end submodule sql_parser

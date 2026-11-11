!! `sql` intermediate submodule: shared helpers host-associated by the parser
!! and executor descendants.  Mirrors `sqr_base` in the engine: anything two
!! sibling submodules both need lives here, because host association reaches
!! descendants but not across siblings.
!!
!! The lexer (`sql_lex`) deliberately lives with the parser in `sql_parse`
!! rather than here: it is only ever called by the parser, and a GCC 16.1.0
!! bug ICEs on a cross-submodule call to a module procedure with an
!! `allocatable, intent(out)` derived-type array dummy (`toks`), which is
!! exactly `sql_lex`'s signature.  Co-locating caller and callee sidesteps it.

submodule (sql) sql_base
    use :: sqr_consts, only: I32_BYTES, R64_BYTES, CH_NUL, CH_TAB, CH_HIGH, ASCII_CASE_OFFSET, &
                             INT_TEXT_LEN, NUM_TEXT_LEN
    implicit none

    ! Byte-order marks, as the bytes that open a file.
    character(len=*), parameter :: BOM_UTF8    = char(int(z'EF')) // char(int(z'BB')) // char(int(z'BF'))
    character(len=*), parameter :: BOM_UTF16LE = char(int(z'FF')) // char(int(z'FE'))
    character(len=*), parameter :: BOM_UTF16BE = char(int(z'FE')) // char(int(z'FF'))

contains

    ! ===== input encoding =====

    !! `.true.` if `text` begins with `prefix`.
    pure function starts_with(text, prefix) result(yes)
        character(len=*), intent(in) :: text, prefix
        logical :: yes
        yes = .false.
        if (len(text) < len(prefix)) return
        yes = text(1:len(prefix)) == prefix
    end function

    pure module subroutine sql_strip_bom(text, enc)
        character(len=:), allocatable, intent(inout) :: text
        integer,                       intent(out)   :: enc
        enc = SQL_ENC_PLAIN
        if (.not. allocated(text)) return
        if (starts_with(text, BOM_UTF8)) then
            enc  = SQL_ENC_UTF8
            text = text(len(BOM_UTF8) + 1:)
        else if (starts_with(text, BOM_UTF16LE) .or. starts_with(text, BOM_UTF16BE)) then
            enc = SQL_ENC_UTF16
        end if
    end subroutine

    ! ===== shared helpers (host-associated by sql_parse / sql_exec) =====

    !! Lower-case an ASCII string (for case-insensitive keyword matching).
    pure function lower(s) result(r)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: k, c
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) c = c + ASCII_CASE_OFFSET
            r(k:k) = achar(c)
        end do
    end function

    !! `.true.` if token `t` is the (case-insensitive) keyword `kw`.
    pure function is_kw(t, kw) result(yes)
        type(sql_token_t), intent(in) :: t
        character(len=*),  intent(in) :: kw
        logical :: yes
        yes = .false.
        if (t%kind /= TK_IDENT) return
        if (.not. allocated(t%text)) return
        yes = lower(t%text) == lower(kw)
    end function

    !! `.true.` if token `t` is the punctuation `p`.
    pure function is_punct(t, p) result(yes)
        type(sql_token_t), intent(in) :: t
        character(len=*),  intent(in) :: p
        logical :: yes
        yes = .false.
        if (t%kind /= TK_PUNCT) return
        if (.not. allocated(t%text)) return
        yes = t%text == p
    end function

    !! Record an error: set `stat` (when present) to `code` and copy `msg`
    !! into `errmsg` (when present, truncated to its length).
    pure subroutine set_err(stat, errmsg, code, msg)
        integer,          intent(out),   optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        integer,          intent(in)              :: code
        character(len=*), intent(in)              :: msg
        if (present(stat))   stat = code
        if (present(errmsg)) errmsg = msg
    end subroutine

    !! Decimal text of an integer.
    pure function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=INT_TEXT_LEN) :: buf
        write(buf, '(i0)') i
        s = trim(buf)
    end function

end submodule sql_base

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

    ! --- Input-encoding signatures ---
    integer, parameter :: MAX_SIG_BYTES = 4

    !! One byte-order mark (or other leading signature) and what it identifies.
    type :: signature_t
        character(len=MAX_SIG_BYTES) :: bytes   !! blank-padded; only 1:nbytes count
        integer                      :: nbytes
        integer                      :: enc
    end type

    !! Tested in this order, so a signature that begins with a shorter one
    !! (UTF-32 LE starts with the UTF-16 LE mark) must come first.
    type(signature_t), parameter :: SIGNATURES(*) = [ &
        signature_t(char(int(z'FF')) // char(int(z'FE')) // CH_NUL // CH_NUL,                     4, SQL_ENC_UTF32_LE),   &
        signature_t(CH_NUL // CH_NUL // char(int(z'FE')) // char(int(z'FF')),                     4, SQL_ENC_UTF32_BE),   &
        signature_t(char(int(z'DD')) // char(int(z'73')) // char(int(z'66')) // char(int(z'73')), 4, SQL_ENC_UTF_EBCDIC), &
        signature_t(char(int(z'84')) // char(int(z'31')) // char(int(z'95')) // char(int(z'33')), 4, SQL_ENC_GB18030),    &
        signature_t(char(int(z'EF')) // char(int(z'BB')) // char(int(z'BF')) // ' ',              3, SQL_ENC_UTF8),       &
        signature_t(char(int(z'2B')) // char(int(z'2F')) // char(int(z'76')) // ' ',              3, SQL_ENC_UTF7),       &
        signature_t(char(int(z'F7')) // char(int(z'64')) // char(int(z'4C')) // ' ',              3, SQL_ENC_UTF1),       &
        signature_t(char(int(z'0E')) // char(int(z'FE')) // char(int(z'FF')) // ' ',              3, SQL_ENC_SCSU),       &
        signature_t(char(int(z'FB')) // char(int(z'EE')) // char(int(z'28')) // ' ',              3, SQL_ENC_BOCU1),      &
        signature_t(char(int(z'FF')) // char(int(z'FE')) // '  ',                                 2, SQL_ENC_UTF16_LE),   &
        signature_t(char(int(z'FE')) // char(int(z'FF')) // '  ',                                 2, SQL_ENC_UTF16_BE)    ]

contains

    ! ===== input encoding =====

    pure module subroutine sql_strip_bom(text, enc)
        character(len=:), allocatable, intent(inout) :: text
        integer,                       intent(out)   :: enc
        integer :: i, n

        enc = SQL_ENC_PLAIN
        if (.not. allocated(text)) return

        signature: do i = 1, size(SIGNATURES)
            n = SIGNATURES(i)%nbytes
            if (len(text) < n) cycle signature
            if (text(1:n) == SIGNATURES(i)%bytes(1:n)) then
                enc = SIGNATURES(i)%enc
                if (enc == SQL_ENC_UTF8) text = text(n + 1:)
                exit signature
            end if
        end do signature

        ! A NUL is never part of a statement (DT_CHAR cannot hold one), so in
        ! text with no signature (or behind a UTF-8 mark) it means a wide
        ! encoding with no mark.
        if (sql_encoding_supported(enc)) then
            if (index(text, CH_NUL) > 0) enc = SQL_ENC_WIDE_NO_BOM
        end if
    end subroutine

    pure module subroutine sql_screen_line(text, first, enc)
        character(len=:), allocatable, intent(inout) :: text
        logical,                       intent(in)    :: first
        integer,                       intent(out)   :: enc

        if (first) then
            call sql_strip_bom(text, enc)
        else
            enc = SQL_ENC_PLAIN
            if (allocated(text)) enc = merge(SQL_ENC_WIDE_NO_BOM, SQL_ENC_PLAIN, index(text, CH_NUL) > 0)
        end if
    end subroutine

    pure module function sql_encoding_name(enc) result(name)
        integer, intent(in)           :: enc
        character(len=:), allocatable :: name

        select case (enc)
        case (SQL_ENC_PLAIN);       name = 'ASCII / UTF-8'
        case (SQL_ENC_UTF8);        name = 'UTF-8 with a byte-order mark'
        case (SQL_ENC_UTF16_LE);    name = 'UTF-16 / UCS-2, little-endian'
        case (SQL_ENC_UTF16_BE);    name = 'UTF-16 / UCS-2, big-endian'
        case (SQL_ENC_UTF32_LE);    name = 'UTF-32 / UCS-4, little-endian'
        case (SQL_ENC_UTF32_BE);    name = 'UTF-32 / UCS-4, big-endian'
        case (SQL_ENC_UTF7);        name = 'UTF-7'
        case (SQL_ENC_UTF1);        name = 'UTF-1'
        case (SQL_ENC_UTF_EBCDIC);  name = 'UTF-EBCDIC'
        case (SQL_ENC_SCSU);        name = 'SCSU'
        case (SQL_ENC_BOCU1);       name = 'BOCU-1'
        case (SQL_ENC_GB18030);     name = 'GB18030'
        case (SQL_ENC_WIDE_NO_BOM); name = 'NUL bytes; probably UTF-16 / UCS-2 or UTF-32 / UCS-4 ' // &
                                           'without a byte-order mark'
        case default;               name = 'unknown'
        end select
    end function

    pure elemental module function sql_encoding_supported(enc) result(ok)
        integer, intent(in) :: enc
        logical             :: ok

        ok = (enc == SQL_ENC_PLAIN) .or. (enc == SQL_ENC_UTF8)
    end function

    pure module function sql_encoding_message(enc) result(msg)
        integer, intent(in)           :: enc
        character(len=:), allocatable :: msg

        msg = 'unsupported encoding (' // sql_encoding_name(enc) // '); expected ASCII or UTF-8'
    end function

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

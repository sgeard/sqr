!! `sqlsh` — a line-oriented REPL for the `sql` subset over a `sqr` store.
!!
!! A thin demo front-end: each input line is either a dot meta-command
!! (`.open`, `.tables`, …) handled here, or a SQL statement passed straight
!! to `sql_run` and rendered with `sql_render`.  All of the SQL machinery
!! lives in the `sql` module; this program is only I/O plumbing.
!!
!! Prompts, banners and errors go to stderr; query output goes to stdout.  A
!! script piped on stdin therefore produces a clean result stream on stdout
!! (`sqlsh mydb < script.sql > out.txt`), independent of the chatter.  An
!! optional argument names a database directory to open at start-up; an
!! argument that starts with `-` is an option (`--help`, `--version`), never
!! a directory, so a mistyped option cannot create one.  A directory whose
!! name really does start with `-` is given as `./-name`.

program sqlsh
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, iostat_end
    use :: sqr
    use :: sql
    use :: clib_wrap, only: c_isatty_stdin, c_exit
    implicit none

    type(db_t), save :: db
    character(len=:), allocatable :: line
    logical :: interactive, first_line
    character(len=*), parameter :: USAGE = 'usage: sqlsh [<db-dir>]   (or --help, --version)'

    interactive = is_a_tty()       ! prompt only when reading from a terminal

    call start_up()

    if (interactive) call banner()

    first_line = .true.
    repl: do
        call prompt()
        call read_line(line)
        if (.not. allocated(line)) exit repl       ! EOF
        call check_encoding(line, first_line)
        first_line = .false.
        if (len_trim(line) == 0) cycle repl
        if (line(1:1) == '.') then
            if (meta(line)) exit repl               ! .quit / .exit
        else
            call run_sql(line)
        end if
    end do repl

    if (db%opened) call db_close(db)

contains

    ! ---- start-up ----

    !! Act on the command line: at most one argument, an option or the
    !! database directory to open.  Anything else ends the process with the
    !! usage and status 2, before any directory has been touched.
    subroutine start_up()
        character(len=SQR_MAX_DIR) :: arg
        integer :: nargs
        nargs = command_argument_count()
        if (nargs == 0) return
        call get_command_argument(1, arg)
        if (arg(1:1) == '-') then
            select case (trim(arg))
            case ('--help')
                write(output_unit,'(a)') USAGE
                call quit(0)
            case ('--version')
                write(output_unit,'(a)') 'sqlsh ' // SQR_RELEASE
                call quit(0)
            case default
                write(error_unit,'(a)') 'sqlsh: unknown option: ' // trim(arg)
                write(error_unit,'(a)') USAGE
                call quit(2)
            end select
        end if
        if (nargs > 1) then
            write(error_unit,'(a)') 'sqlsh: too many arguments'
            write(error_unit,'(a)') USAGE
            call quit(2)
        end if
        call open_arg_db(trim(arg))
    end subroutine

    !! End the process with `code`.  Both streams are flushed first: c_exit
    !! is the C library's exit, which knows nothing of Fortran's buffers.
    subroutine quit(code)
        integer, intent(in) :: code
        flush(output_unit)
        flush(error_unit)
        call c_exit(code)
    end subroutine

    subroutine open_arg_db(arg)
        character(len=*), intent(in) :: arg
        integer :: rs
        character(len=SQR_ERRMSG_LEN) :: emsg
        emsg = ''
        call db_open(db, trim(arg), rs, emsg)
        if (rs /= SQR_OK) then
            write(error_unit,'(3a)') 'cannot open "', trim(arg), '": ' // trim(emsg)
        end if
    end subroutine

    subroutine banner()
        write(error_unit,'(a)') 'sqlsh — SQL subset over sqr.  ".help" for commands, ".quit" to exit.'
    end subroutine

    subroutine prompt()
        if (interactive) then
            write(error_unit,'(a)', advance='no') 'sql> '
            flush(error_unit)
        end if
    end subroutine

    ! ---- input ----

    !! Deal with the encoding of a line of input: a UTF-8 byte-order mark on
    !! the first line is dropped; an encoding sqr cannot read (UTF-16, UTF-32,
    !! ... - see `sql_strip_bom`) is named and ends sqlsh with status 1,
    !! rather than a syntax error being reported on every line.
    subroutine check_encoding(line, first)
        character(len=:), allocatable, intent(inout) :: line
        logical,                       intent(in)    :: first
        integer :: enc
        call sql_screen_line(line, first, enc)
        if (sql_encoding_supported(enc)) return
        write(error_unit,'(2a)') 'sqlsh: ', sql_encoding_message(enc)
        if (enc == SQL_ENC_UTF16_LE) then
            write(error_unit,'(a)') '       Re-save the script as UTF-8 (Windows PowerShell 5.1: ' // &
                                    'Out-File -Encoding utf8 instead of ">").'
        end if
        if (db%opened) call db_close(db)
        flush(error_unit)
        call c_exit(1)
    end subroutine

    !! Read one logical line of arbitrary length from stdin into `line`.
    !! Leaves `line` unallocated on end-of-file.
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
                if (len(line) == 0) deallocate(line)   ! true EOF, nothing buffered
                return
            end if
            if (is_iostat_eor(ios)) return             ! end of record: done
            if (ios /= 0) return                       ! other error: take what we have
        end do
    end subroutine

    ! ---- SQL ----

    subroutine run_sql(text)
        character(len=*), intent(in) :: text
        type(sql_result_t) :: res
        integer :: rs
        character(len=SQR_ERRMSG_LEN) :: emsg
        emsg = ''
        call sql_run(db, text, res, rs, emsg)
        if (rs /= SQR_OK) then
            write(error_unit,'(2a)') 'error: ', trim(emsg)
            return
        end if
        call sql_render(res, output_unit)
    end subroutine

    ! ---- meta-commands (return .true. to quit) ----

    function meta(text) result(quit)
        character(len=*), intent(in) :: text
        logical :: quit
        character(len=:), allocatable :: cmd, rest
        integer :: sp
        quit = .false.
        sp = index(text, ' ')
        if (sp == 0) then
            cmd  = lower(trim(text))
            rest = ''
        else
            cmd  = lower(text(1:sp-1))
            rest = trim(adjustl(text(sp+1:)))
        end if
        select case (cmd)
        case ('.quit', '.exit', '.q')
            quit = .true.
        case ('.help', '.h')
            call meta_help()
        case ('.open')
            call meta_open(rest)
        case ('.close')
            call meta_close()
        case ('.tables')
            call meta_tables()
        case ('.schema')
            call meta_schema(rest)
        case ('.pack')
            call meta_pack(rest)
        case ('.unpack')
            call meta_unpack(rest)
        case default
            write(error_unit,'(2a)') 'unknown command: ', trim(cmd)
        end select
    end function

    subroutine meta_help()
        write(error_unit,'(a)') 'Meta-commands:'
        write(error_unit,'(a)') '  .open <dir>     open (creating if needed) a database directory'
        write(error_unit,'(a)') '  .close          close the current database'
        write(error_unit,'(a)') '  .tables         list tables (rows, columns)'
        write(error_unit,'(a)') '  .schema [table] show column/index definitions'
        write(error_unit,'(a)') '  .pack <dir> <file>   archive a database dir into one .sqr file'
        write(error_unit,'(a)') '  .unpack <file> <dir> restore a .sqr file into a new database dir'
        write(error_unit,'(a)') '  .help           this help'
        write(error_unit,'(a)') '  .quit           exit'
        write(error_unit,'(a)') 'Anything else is run as SQL (SELECT/INSERT/UPDATE/DELETE/CREATE/DROP/ALTER/BEGIN/COMMIT/ROLLBACK).'
    end subroutine

    subroutine meta_open(dir)
        character(len=*), intent(in) :: dir
        integer :: rs
        character(len=SQR_ERRMSG_LEN) :: emsg
        if (len_trim(dir) == 0) then
            write(error_unit,'(a)') 'usage: .open <dir>'
            return
        end if
        if (db%opened) call db_close(db)
        emsg = ''
        call db_open(db, trim(dir), rs, emsg)
        if (rs /= SQR_OK) then
            write(error_unit,'(3a)') 'cannot open "', trim(dir), '": ' // trim(emsg)
        else if (interactive) then
            write(error_unit,'(2a)') 'opened ', trim(dir)
        end if
    end subroutine

    subroutine meta_close()
        if (.not. db%opened) then
            write(error_unit,'(a)') 'no database open'
            return
        end if
        call db_close(db)
    end subroutine

    subroutine meta_tables()
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        integer :: i
        if (.not. require_open()) return
        call db_list_tables(db, names)
        if (size(names) == 0) then
            write(error_unit,'(a)') '(no tables)'
            return
        end if
        do i = 1, size(names)
            block
                type(column_t), allocatable :: cols(:)
                call db_describe(db, trim(names(i)), cols)
                write(output_unit,'(2a,2(i0,a))') trim(names(i)), &
                    '  (', db_row_count(db, trim(names(i))), ' rows, ', size(cols), ' cols)'
            end block
        end do
    end subroutine

    subroutine meta_schema(name)
        character(len=*), intent(in) :: name
        character(len=SQR_NAME_LEN), allocatable :: names(:)
        integer :: i
        if (.not. require_open()) return
        if (len_trim(name) == 0) then
            call db_list_tables(db, names)
            do i = 1, size(names)
                call schema_one(trim(names(i)))
            end do
        else
            call schema_one(trim(name))
        end if
    end subroutine

    subroutine schema_one(name)
        character(len=*), intent(in) :: name
        type(column_t), allocatable :: cols(:)
        integer :: rs, i
        call db_describe(db, name, cols, rs)
        if (rs /= SQR_OK) then
            write(error_unit,'(2a)') 'no such table: ', name
            return
        end if
        write(output_unit,'(2a)') 'CREATE TABLE ', name
        do i = 1, size(cols)
            associate (c => cols(i))
                write(output_unit,'(4a,a)') '    ', trim(c%name), ' ', &
                    type_text(c%dtype, c%csize), merge(',', ' ', i < size(cols))
            end associate
        end do
    end subroutine

    pure function type_text(dtype, csize) result(s)
        use :: sqr_consts, only: INT_TEXT_LEN
        integer, intent(in) :: dtype, csize
        character(len=:), allocatable :: s
        character(len=INT_TEXT_LEN) :: buf
        select case (dtype)
        case (DT_INT);  s = 'INTEGER'
        case (DT_REAL); s = 'REAL'
        case (DT_TEXT); s = 'TEXT'
        case (DT_CHAR)
            write(buf, '(i0)') csize
            s = 'CHAR(' // trim(buf) // ')'
        case default;   s = '?'
        end select
    end function

    subroutine meta_pack(args)
        character(len=*), intent(in) :: args
        character(len=:), allocatable :: dir, file
        integer :: rs
        call two_args(args, dir, file)
        if (len_trim(file) == 0) then
            write(error_unit,'(a)') 'usage: .pack <dir> <file>'
            return
        end if
        call db_pack(dir, file, rs)
        if (rs /= SQR_OK) then
            write(error_unit,'(3a,i0)') 'pack of "', trim(dir), '" failed: ', rs
        else if (interactive) then
            write(error_unit,'(4a)') 'packed ', trim(dir), ' -> ', trim(file)
        end if
    end subroutine

    subroutine meta_unpack(args)
        character(len=*), intent(in) :: args
        character(len=:), allocatable :: file, dir
        integer :: rs
        call two_args(args, file, dir)
        if (len_trim(dir) == 0) then
            write(error_unit,'(a)') 'usage: .unpack <file> <dir>'
            return
        end if
        call db_unpack(file, dir, rs)
        if (rs /= SQR_OK) then
            write(error_unit,'(3a,i0)') 'unpack of "', trim(file), '" failed: ', rs
        else if (interactive) then
            write(error_unit,'(4a)') 'unpacked ', trim(file), ' -> ', trim(dir)
        end if
    end subroutine

    ! Split a string on its first run of blanks into two trimmed tokens.
    subroutine two_args(s, a, b)
        character(len=*),              intent(in)  :: s
        character(len=:), allocatable, intent(out) :: a, b
        integer :: sp
        sp = index(trim(adjustl(s)), ' ')
        if (sp == 0) then
            a = trim(adjustl(s))
            b = ''
        else
            block
                character(len=:), allocatable :: t
                t = trim(adjustl(s))
                a = t(1:sp-1)
                b = trim(adjustl(t(sp+1:)))
            end block
        end if
    end subroutine

    ! ---- helpers ----

    function require_open() result(ok)
        logical :: ok
        ok = db%opened
        if (.not. ok) write(error_unit,'(a)') 'no database open (use .open <dir>)'
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

    !! `.true.` when stdin is a terminal (so we should print prompts).  Uses
    !! the engine's small C shim rather than a non-standard intrinsic.
    function is_a_tty() result(yes)
        logical :: yes
        yes = c_isatty_stdin()
    end function

end program sqlsh

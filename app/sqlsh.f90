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
!! optional first argument names a database directory to open at start-up.

program sqlsh
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, iostat_end
    use :: sqr
    use :: sql
    use :: clib_wrap, only: c_isatty_stdin
    implicit none

    type(db_t), save :: db
    character(len=:), allocatable :: line
    logical :: interactive
    integer :: nargs

    interactive = is_a_tty()       ! prompt only when reading from a terminal

    nargs = command_argument_count()
    if (nargs >= 1) call open_arg_db()

    if (interactive) call banner()

    repl: do
        call prompt()
        call read_line(line)
        if (.not. allocated(line)) exit repl       ! EOF
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

    subroutine open_arg_db()
        character(len=4096) :: arg
        integer :: rs
        character(len=256) :: emsg
        call get_command_argument(1, arg)
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

    !! Read one logical line of arbitrary length from stdin into `line`.
    !! Leaves `line` unallocated on end-of-file.
    subroutine read_line(line)
        character(len=:), allocatable, intent(out) :: line
        character(len=512) :: chunk
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
        character(len=256) :: emsg
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
        character(len=256) :: emsg
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
            associate (t => db%tables(i))
                write(output_unit,'(3a,2(i0,a))') trim(names(i)), &
                    '  (', t%live_count, ' rows, ', t%ncols, ' cols)'
            end associate
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
        integer :: ti, i
        ti = db_table_index(db, name)
        if (ti == 0) then
            write(error_unit,'(2a)') 'no such table: ', name
            return
        end if
        associate (t => db%tables(ti))
            write(output_unit,'(2a)') 'CREATE TABLE ', name
            do i = 1, t%ncols
                associate (c => t%cols(i))
                    write(output_unit,'(4a,a)') '    ', trim(c%name), ' ', &
                        type_text(c%dtype, c%csize), merge(',', ' ', i < t%ncols)
                end associate
            end do
        end associate
    end subroutine

    pure function type_text(dtype, csize) result(s)
        integer, intent(in) :: dtype, csize
        character(len=:), allocatable :: s
        character(len=16) :: buf
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
        character(len=*), intent(in) :: s
        character(len=len(s)) :: r
        integer :: k, c
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= iachar('A') .and. c <= iachar('Z')) c = c + 32
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

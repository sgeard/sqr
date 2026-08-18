!! `sqrd` — serve one sqr database over the wire protocol
!! (reports/DESIGN-wire-protocol.md).
!!
!! Usage: `sqrd <db-dir> <port>` — the directory is opened (created if
!! absent, like sqlsh's `.open`), a loopback listener is bound on `<port>`
!! (0 asks the OS for an ephemeral port), and requests are served until the
!! process is killed.
!!
!! Once ready it prints `LISTENING <port>` on stdout and everything else on
!! stderr — including the absolute path of the database being served, which
!! is what a backup needs and what tells two instances apart — so a launcher
!! (the functional-test harness, a desktop file, an
!! ODBC setup script) can read the one machine-readable line back.  There
!! is no shutdown command: sqrd runs until killed, and a kill mid-write is
!! exactly the crash case sqr's rollback journal already covers — the next
!! open recovers.
program sqrd
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use :: sqr
    use :: sqr_serve
    use :: clib_wrap, only: c_exit, c_getpid
    implicit none

    type(db_t), target, save :: db
    type(server_t) :: srv
    character(len=4096) :: dirarg
    character(len=32)   :: portarg
    character(len=256)  :: emsg
    integer :: rs, port, nev, ios

    ! Fatal paths exit through c_exit, not `stop 1`: the diagnostic just
    ! written is the whole message, and ifx would otherwise append its
    ! rendering of the stop code — which a launcher reading this stream (see
    ! run_sqrd) then has to filter out of the log.
    if (command_argument_count() /= 2) then
        write(error_unit, '(a)') 'usage: sqrd <db-dir> <port>   (port 0 = ephemeral)'
        call die()
    end if
    call get_command_argument(1, dirarg)
    call get_command_argument(2, portarg)
    read(portarg, *, iostat=ios) port
    if (ios /= 0 .or. port < 0 .or. port > 65535) then
        write(error_unit, '(2a)') 'sqrd: bad port: ', trim(portarg)
        call die()
    end if

    emsg = ''
    call db_open(db, trim(dirarg), rs, emsg)
    if (rs /= SQR_OK) then
        write(error_unit, '(4a)') 'sqrd: cannot open "', trim(dirarg), '": ', trim(emsg)
        call die()
    end if

    emsg = ''
    call serve_open(srv, db, port, rs, emsg)
    if (rs /= SQR_OK) then
        write(error_unit, '(2a)') 'sqrd: ', trim(emsg)
        call db_close(db)
        call die()
    end if

    call advertise()

    write(output_unit, '(a,i0)') 'LISTENING ', srv%port
    flush(output_unit)
    ! The absolute path, not the argument: whoever has to back this database
    ! up, or work out which of several sqrd instances is which, needs the
    ! physical location, and the argument is usually relative to a working
    ! directory that is not obvious from the process list.
    write(error_unit, '(5a)') 'sqrd: serving "', srv%dbname, '" from ', srv%dbpath, ', kill to stop'

    serve: do
        call serve_step(srv, 1000, nev)
    end do serve

contains

    !! Record where this daemon can be reached, in the one place a client
    !! that knows the database is certain to look: the database directory
    !! itself.  A client then names the database — which is what it cares
    !! about, and which works through symlinks for free — instead of a port
    !! number that means nothing on its own.
    !!
    !! The file is a HINT, never a fact.  sqrd has no orderly shutdown (it
    !! is killed, and the journal covers that), so nothing removes it: a
    !! stale `_sqrd` outlives every daemon that ever served this directory,
    !! and start-up simply overwrites it.  Every reader must therefore
    !! verify — connect, ask `INFO`, and check the directory it reports.
    !! That one test also catches a reused pid and a recycled port, which
    !! is why no liveness is attempted here.
    !!
    !! A failure to write is not fatal: the daemon serves perfectly well
    !! without the file, and clients given an explicit port never read it.
    subroutine advertise()
        character(len=:), allocatable :: path
        integer :: u, ios
        path = trim(dirarg)
        if (len(path) > 0) then
            if (path(len(path):len(path)) /= '/') path = path // '/'
        end if
        path = path // '_sqrd'
        open(newunit=u, file=path, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            write(error_unit, '(2a)') 'sqrd: cannot advertise in ', path
            return
        end if
        write(u, '(a,i0)') 'pid = ',  c_getpid()
        write(u, '(a,i0)') 'port = ', srv%port
        write(u, '(a)')    'host = 127.0.0.1'
        close(u)
    end subroutine

    !! Exit non-zero after the caller has written its diagnostic.  Flushes
    !! stderr first: c_exit is the C library's exit, which knows nothing of
    !! Fortran's buffers.
    subroutine die()
        flush(error_unit)
        call c_exit(1)
    end subroutine

end program sqrd

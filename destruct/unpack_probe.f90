! unpack_probe — drive db_unpack over a (corrupt) .sqr container, then open
! whatever it produced.  usage: unpack_probe <packfile> <outdir>
program unpack_probe
    use, intrinsic :: iso_fortran_env, only: output_unit
    use :: sqr
    implicit none
    type(db_t)         :: db
    character(len=512) :: pf, od
    character(len=256) :: emsg
    character(len=SQR_NAME_LEN), allocatable :: tn(:)
    integer :: st

    call get_command_argument(1, pf)
    call get_command_argument(2, od)
    write(output_unit, '(a)') 'STEP unpack'
    flush(output_unit)
    call db_unpack(trim(pf), trim(od), st)
    write(output_unit, '(a,i0)') 'STEP unpack.done stat=', st
    flush(output_unit)
    if (st == SQR_OK) then
        emsg = ''
        call db_open(db, trim(od), stat=st, errmsg=emsg)
        write(output_unit, '(a,i0)') 'STEP open.done stat=', st
        if (st == SQR_OK) then
            call db_list_tables(db, tn)
            call db_close(db, st)
        end if
    end if
    write(output_unit, '(a)') 'SURVIVED'
end program unpack_probe

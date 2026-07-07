!! sqr performance benchmark harness (ROADMAP "Before shipping" #2).
!!
!! Make-only (built by `make bench`), in bench/ so fpm never sees it.
!! Links the production FAULT=off optimised library.  Wall-clock timing
!! via system_clock(int64); pseudo-random keys via a deterministic LCG so
!! runs are reproducible and the indexed-insert path is exercised on
!! unsorted keys (the case that was quadratic before the B+-tree).

program bench_sqr
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, output_unit
    use sqr
    use sql
    use clib_wrap, only: c_rmtree
    implicit none

    type :: scan_ctx_t
        type(column_t) :: col
        integer(int32) :: target = 0
        integer(int32) :: found  = 0
    end type

    character(len=*), parameter :: DBDIR = 'bench_db'
    integer(int64) :: clk_rate

    ! Tunable problem sizes.
    integer, parameter :: SCALE_N(4) = [2000, 4000, 8000, 16000]
    integer, parameter :: TXN_SCALE(4) = [500, 1000, 2000, 4000]   ! explicit-txn arm scaling
    integer, parameter :: LOOKUP_ROWS  = 20000
    integer, parameter :: LOOKUP_HITS  = 20000   ! indexed lookups timed
    integer, parameter :: SCAN_HITS    = 200     ! full-scan lookups timed
    integer, parameter :: COMPACT_ROWS = 20000
    integer, parameter :: TEXT_ROWS    = 5000
    integer, parameter :: TEXT_BYTES   = 200
    integer, parameter :: SQL_ROWS     = 50000   ! WHERE-scan table
    integer, parameter :: SQL_REPS     = 5       ! timed scan queries
    integer, parameter :: ORDER_ROWS   = 10000   ! ORDER BY result size
    integer, parameter :: LIT_LEN      = 50000   ! long string literal (lexer)

    call system_clock(count_rate=clk_rate)
    call cleanup()

    print '(a)', '=== sqr benchmark ==='
    print '(a)', ''

    call bench_bulk_insert()
    call bench_index_scaling()
    call bench_txn_insert()
    call bench_lookup()
    call bench_delete_compact()
    call bench_text()
    call bench_sql()

    call cleanup()
    print '(a)', ''
    print '(a)', '=== done ==='

contains

    subroutine cleanup()
        integer :: ios
        ios = c_rmtree(DBDIR)
    end subroutine

    ! Wall-clock seconds between two system_clock ticks.
    pure function secs(t0, t1) result(s)
        integer(int64), intent(in) :: t0, t1
        real(real64) :: s
        s = real(t1 - t0, real64) / real(clk_rate, real64)
    end function

    ! Deterministic 31-bit LCG (glibc constants); int64 math avoids
    ! signed-overflow UB, result masked back to a positive int32.
    pure function lcg(state) result(nxt)
        integer(int64), intent(in) :: state
        integer(int64) :: nxt
        nxt = modulo(state * 1103515245_int64 + 12345_int64, 2147483648_int64)
    end function

    pure function bench_cols() result(c)
        type(column_t) :: c(3)
        c(1)%name = 'id'  ; c(1)%dtype = DT_INT  ; c(1)%csize = 4
        c(2)%name = 'k'   ; c(2)%dtype = DT_INT  ; c(2)%csize = 4
        c(3)%name = 'pad' ; c(3)%dtype = DT_CHAR ; c(3)%csize = 32
    end function

    subroutine die(label, rs)
        character(len=*), intent(in) :: label
        integer,          intent(in) :: rs
        if (rs /= SQR_OK) then
            print '(a,a,a,i0)', 'FATAL ', label, ' rs=', rs
            error stop 1
        end if
    end subroutine

    ! --- 1. Bulk insert, no index -------------------------------------
    subroutine bench_bulk_insert()
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: t0, t1
        integer, parameter :: N = 16000

        call cleanup()
        call db_open(db, DBDIR, rs);                       call die('open', rs)
        call db_create_table(db, 'noidx', bench_cols(), rs); call die('create', rs)
        ti = db_table_index(db, 'noidx')
        call row_alloc(buf, db%tables(ti)%record_size)

        call system_clock(t0)
        do i = 1, N
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int (buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int (buf, db%tables(ti)%cols(2), int(i, int32))
            call row_set_char(buf, db%tables(ti)%cols(3), 'payload')
            call db_insert(db, 'noidx', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert', rs)
        end do
        call system_clock(t1)
        call db_close(db)

        print '(a)', '-- 1. bulk insert, no index --'
        call report_rate('insert (no index)', N, secs(t0, t1))
        print '(a)', ''
    end subroutine

    ! --- 2. Indexed insert: B+-tree incremental-insert scaling --------
    ! A live non-unique index is present, keys arrive in pseudo-random
    ! order.  With the B+-tree index each insert is O(log N), so the
    ! per-insert cost is ~flat as N grows (ratio ~1.0) instead of the
    ! pre-B+-tree quadratic; ascending-key and no-index are baselines.
    subroutine bench_index_scaling()
        integer :: s
        real(real64) :: prev_us
        print '(a)', '-- 2. indexed insert, random keys (B+-tree, O(log N)) --'
        print '(a)', '       N   total(s)   us/insert   ratio   asc us/ins   noidx us/ins'
        prev_us = 0.0_real64
        do s = 1, size(SCALE_N)
            call one_scale(SCALE_N(s), prev_us)
        end do
        print '(a)', ''
        print '(a)', '   ratio = us/insert vs the previous (half-size) row;'
        print '(a)', '   ~1.0 means per-insert cost is ~constant in N (the'
        print '(a)', '   total is ~O(N log N), not the old O(N^2)).'
        print '(a)', ''
    end subroutine

    subroutine one_scale(n, prev_us)
        integer,      intent(in)    :: n
        real(real64), intent(inout) :: prev_us
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: st, t0, t1, t2, t3, t4, t5
        real(real64) :: us, ratio, asc_us, noidx_us

        ! (a) random-key insert with a live index
        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 't', bench_cols(), rs);     call die('create', rs)
        call db_create_index(db, 't', 'k', rs);              call die('index', rs)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        st = 12345_int64
        call system_clock(t0)
        do i = 1, n
            st = lcg(st)
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(st, int32))
            call db_insert(db, 't', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(idx)', rs)
        end do
        call system_clock(t1)
        call db_close(db)

        ! (b) ascending-key insert with a live index (append at end)
        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 't', bench_cols(), rs);     call die('create', rs)
        call db_create_index(db, 't', 'k', rs);              call die('index', rs)
        ti = db_table_index(db, 't')
        call system_clock(t2)
        do i = 1, n
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(asc)', rs)
        end do
        call system_clock(t3)
        call db_close(db)

        ! (c) no-index insert (pure data-file write baseline)
        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 't', bench_cols(), rs);     call die('create', rs)
        ti = db_table_index(db, 't')
        call system_clock(t4)
        do i = 1, n
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(noidx)', rs)
        end do
        call system_clock(t5)
        call db_close(db)

        us       = 1.0e6_real64 * secs(t0, t1) / real(n, real64)
        asc_us   = 1.0e6_real64 * secs(t2, t3) / real(n, real64)
        noidx_us = 1.0e6_real64 * secs(t4, t5) / real(n, real64)
        if (prev_us > 0.0_real64) then
            ratio = us / prev_us
        else
            ratio = 0.0_real64
        end if
        print '(i8,f11.3,f12.2,f8.2,f13.2,f15.2)', &
            n, secs(t0, t1), us, ratio, asc_us, noidx_us
        prev_us = us
    end subroutine

    ! --- 2b. Explicit-transaction insert: journal arm scaling ---------
    ! Every mutation inside an explicit txn arms the journal.  Before E1 the arm
    ! re-serialised and rewrote the ENTIRE undo set on each call (plus a have_rec
    ! linear scan and an O(nrec^2) commit fsync dedup), so a txn of N inserts was
    ! O(N^2).  After E1/E2/E3 the arm appends only the new record, dedup is a hash
    ! probe, and the commit fsyncs each distinct path once, so per-insert cost is
    ! ~flat (ratio ~1.0) instead of doubling with N.
    subroutine bench_txn_insert()
        integer :: s
        real(real64) :: prev_us
        print '(a)', '-- 2b. explicit-txn insert, journal arm scaling --'
        print '(a)', '       N   total(s)   us/insert   ratio'
        prev_us = 0.0_real64
        do s = 1, size(TXN_SCALE)
            call one_txn_scale(TXN_SCALE(s), prev_us)
        end do
        print '(a)', ''
        print '(a)', '   ratio = us/insert vs the previous (half-size) row;'
        print '(a)', '   ~1.0 means the per-insert arm cost is ~constant in N.'
        print '(a)', ''
    end subroutine

    subroutine one_txn_scale(n, prev_us)
        integer,      intent(in)    :: n
        real(real64), intent(inout) :: prev_us
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: t0, t1
        real(real64) :: us, ratio
        call cleanup()
        call db_open(db, DBDIR, rs);                         call die('open', rs)
        call db_create_table(db, 't', bench_cols(), rs);     call die('create', rs)
        call db_create_index(db, 't', 'k', rs);              call die('index', rs)
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_begin(db, rs);                               call die('begin', rs)
        call system_clock(t0)
        do i = 1, n
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 't', buf, rid, rs)
            if (rs /= SQR_OK) call die('txn insert', rs)
        end do
        call system_clock(t1)
        call db_commit(db, rs);                              call die('commit', rs)
        call db_close(db)
        us = 1.0e6_real64 * secs(t0, t1) / real(n, real64)
        if (prev_us > 0.0_real64) then
            ratio = us / prev_us
        else
            ratio = 0.0_real64
        end if
        print '(i8,f11.3,f12.2,f8.2)', n, secs(t0, t1), us, ratio
        prev_us = us
    end subroutine

    ! --- 3. Point lookup: index vs full scan --------------------------
    subroutine bench_lookup()
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: st, t0, t1
        type(scan_ctx_t) :: ctx

        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 'lk', bench_cols(), rs);    call die('create', rs)
        ti = db_table_index(db, 'lk')
        call row_alloc(buf, db%tables(ti)%record_size)
        do i = 1, LOOKUP_ROWS
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 'lk', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(lk)', rs)
        end do
        call db_create_index(db, 'lk', 'k', rs);             call die('index(lk)', rs)
        ti = db_table_index(db, 'lk')
        ctx%col = db%tables(ti)%cols(2)

        ! Indexed equality lookup (binary search).
        st = 999_int64
        call system_clock(t0)
        do i = 1, LOOKUP_HITS
            st = lcg(st)
            call db_find_by_int(db, 'lk', 'k', &
                int(modulo(st, int(LOOKUP_ROWS, int64)) + 1, int32), rid, rs)
            if (rs /= SQR_OK .or. rid == 0) call die('find', rs)
        end do
        call system_clock(t1)
        print '(a)', '-- 3. point lookup --'
        call report_per_op('indexed find (binary search)', LOOKUP_HITS, secs(t0, t1))

        ! Full-scan lookup (no index used): every call is an O(N) scan.
        st = 999_int64
        call system_clock(t0)
        do i = 1, SCAN_HITS
            st = lcg(st)
            ctx%target = int(modulo(st, int(LOOKUP_ROWS, int64)) + 1, int32)
            ctx%found  = 0
            call db_scan(db, 'lk', scan_match, ctx, rs)
            if (rs /= SQR_OK .or. ctx%found == 0) call die('scan', rs)
        end do
        call system_clock(t1)
        call report_per_op('full scan (no index)', SCAN_HITS, secs(t0, t1))
        call db_close(db)
        print '(a)', ''
    end subroutine

    ! --- 4. Delete half, then compact ---------------------------------
    subroutine bench_delete_compact()
        type(db_t) :: db
        character(len=:), allocatable :: buf
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: t0, t1, t2, t3

        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 'dc', bench_cols(), rs);    call die('create', rs)
        ti = db_table_index(db, 'dc')
        call row_alloc(buf, db%tables(ti)%record_size)
        do i = 1, COMPACT_ROWS
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int(buf, db%tables(ti)%cols(2), int(i, int32))
            call db_insert(db, 'dc', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(dc)', rs)
        end do

        call system_clock(t0)
        do i = 1, COMPACT_ROWS, 2          ! tombstone every other row
            call db_delete(db, 'dc', int(i, int32), rs)
            if (rs /= SQR_OK) call die('delete', rs)
        end do
        call system_clock(t1)

        call system_clock(t2)
        call db_compact(db, 'dc', rs);                       call die('compact', rs)
        call system_clock(t3)
        call db_close(db)

        print '(a)', '-- 4. delete + compact --'
        call report_rate('delete (tombstone)', COMPACT_ROWS / 2, secs(t0, t1))
        print '(a,f8.3,a,i0,a)', '   compact ', secs(t2, t3), &
            ' s for ', COMPACT_ROWS / 2, ' live rows'
        print '(a)', ''
    end subroutine

    ! --- 5. TEXT set / get --------------------------------------------
    subroutine bench_text()
        type(db_t) :: db
        character(len=:), allocatable :: buf, got
        character(len=TEXT_BYTES) :: payload
        integer :: rs, i, ti
        integer(int32) :: rid
        integer(int64) :: t0, t1, t2, t3
        type(column_t) :: tc(2)
        real(real64) :: mb

        payload = repeat('x', TEXT_BYTES)
        tc(1)%name = 'id'   ; tc(1)%dtype = DT_INT  ; tc(1)%csize = 4
        tc(2)%name = 'body' ; tc(2)%dtype = DT_TEXT ; tc(2)%csize = SQR_TEXT_DESC

        call cleanup()
        call db_open(db, DBDIR, rs);                            call die('open', rs)
        call db_create_table(db, 'tx', tc, rs);              call die('create(tx)', rs)
        ti = db_table_index(db, 'tx')
        call row_alloc(buf, db%tables(ti)%record_size)
        do i = 1, TEXT_ROWS
            call row_set_status(buf, ROW_ALIVE)
            call row_set_int(buf, db%tables(ti)%cols(1), int(i, int32))
            call db_insert(db, 'tx', buf, rid, rs)
            if (rs /= SQR_OK) call die('insert(tx)', rs)
        end do

        call system_clock(t0)
        do i = 1, TEXT_ROWS
            call db_set_text(db, 'tx', int(i, int32), 'body', payload, rs)
            if (rs /= SQR_OK) call die('set_text', rs)
        end do
        call system_clock(t1)

        call system_clock(t2)
        do i = 1, TEXT_ROWS
            call db_get_text(db, 'tx', int(i, int32), 'body', got, rs)
            if (rs /= SQR_OK .or. len(got) /= TEXT_BYTES) call die('get_text', rs)
        end do
        call system_clock(t3)
        call db_close(db)

        mb = real(TEXT_ROWS, real64) * real(TEXT_BYTES, real64) / 1.0e6_real64
        print '(a)', '-- 5. TEXT set / get --'
        print '(a,i0,a,i0,a)', '   ', TEXT_ROWS, ' rows x ', TEXT_BYTES, ' bytes'
        print '(a,f8.3,a,f8.2,a)', '   set_text ', secs(t0, t1), ' s  (', &
            mb / secs(t0, t1), ' MB/s)'
        print '(a,f8.3,a,f8.2,a)', '   get_text ', secs(t2, t3), ' s  (', &
            mb / secs(t2, t3), ' MB/s)'
        print '(a)', ''
    end subroutine

    ! --- reporting helpers --------------------------------------------
    subroutine report_rate(label, n, s)
        character(len=*), intent(in) :: label
        integer,          intent(in) :: n
        real(real64),     intent(in) :: s
        print '(a,a,a,i0,a,f8.3,a,f10.0,a)', '   ', label, ': ', n, &
            ' rows in ', s, ' s  (', real(n, real64) / s, ' rows/s)'
    end subroutine

    subroutine report_per_op(label, n, s)
        character(len=*), intent(in) :: label
        integer,          intent(in) :: n
        real(real64),     intent(in) :: s
        print '(a,a,a,f9.3,a,i0,a)', '   ', label, ': ', &
            1.0e6_real64 * s / real(n, real64), ' us/op over ', n, ' ops'
    end subroutine

    ! --- 6. SQL layer: WHERE scan, ORDER BY, long-literal lexing -------
    ! Targets the review's bottlenecks 4+5: cond_true's per-row column
    ! re-resolution (multi-condition scan), the O(n^2) ORDER BY insertion
    ! sort, and lex_string's O(len^2) one-char-at-a-time buffer growth.
    subroutine bench_sql()
        type(db_t) :: db
        type(sql_result_t) :: res
        type(sql_stmt_t)   :: stmt
        character(len=:), allocatable :: buf, longlit
        integer :: rs, i, r, ti
        integer(int32) :: rid
        integer(int64) :: t0, t1, st
        type(column_t) :: c(4)

        call cleanup()
        c(1)%name = 'id'; c(1)%dtype = DT_INT ; c(1)%csize = 4
        c(2)%name = 'v' ; c(2)%dtype = DT_INT ; c(2)%csize = 4
        c(3)%name = 'w' ; c(3)%dtype = DT_REAL; c(3)%csize = 8
        c(4)%name = 'c' ; c(4)%dtype = DT_CHAR; c(4)%csize = 8
        call db_open(db, DBDIR, rs);  call die('sql open', rs)
        call db_create_table(db, 's', c, rs);  call die('sql create', rs)
        ti = db_table_index(db, 's')
        call row_alloc(buf, db_record_size(db, 's'))
        st = 42_int64
        ! One explicit transaction around the fill: a single commit fsync
        ! instead of one per row (the fill is setup, not what is timed).
        call sql_run(db, "BEGIN", res, rs);  call die('sql begin', rs)
        fill: do i = 1, SQL_ROWS
            st = lcg(st)
            call row_set_int (buf, db%tables(ti)%cols(1), int(i, int32))
            call row_set_int (buf, db%tables(ti)%cols(2), int(modulo(st, 1000_int64), int32))
            call row_set_real(buf, db%tables(ti)%cols(3), real(st, real64))
            call row_set_char(buf, db%tables(ti)%cols(4), 'k' // achar(97 + int(modulo(st, 26_int64))))
            call db_insert(db, 's', buf, rid, rs)
        end do fill
        call sql_run(db, "COMMIT", res, rs);  call die('sql commit', rs)

        print '(a)', '-- 6. SQL layer --'

        ! (a) multi-condition WHERE over a full scan (no index): three
        !     conditions x SQL_ROWS rows x SQL_REPS queries of cond_true.
        call system_clock(t0)
        do r = 1, SQL_REPS
            call sql_run(db, &
                "SELECT id FROM s WHERE v >= 0 AND w >= 0.0 AND c >= 'a'", res, rs)
            call die('sql where', rs)
        end do
        call system_clock(t1)
        call report_per_op('3-cond WHERE scan (us/row)', SQL_ROWS * SQL_REPS, secs(t0, t1))

        ! (b) ORDER BY over an ORDER_ROWS-row result (sort cost dominates).
        call system_clock(t0)
        call sql_run(db, "SELECT id FROM s WHERE id <= " // itoa_b(ORDER_ROWS) // &
                         " ORDER BY w", res, rs)
        call die('sql order', rs)
        call system_clock(t1)
        print '(a,i0,a,f8.3,a)', '   ORDER BY over ', ORDER_ROWS, ' rows: ', secs(t0, t1), ' s'

        ! (c) lexing a statement carrying one long string literal.
        allocate(character(len=LIT_LEN) :: longlit)
        longlit = repeat('x', LIT_LEN)
        call system_clock(t0)
        call sql_parse("SELECT id FROM s WHERE c = '" // longlit // "'", stmt, rs)
        call die('sql lex', rs)
        call system_clock(t1)
        print '(a,i0,a,f8.3,a)', '   lex ', LIT_LEN, '-char literal: ', secs(t0, t1), ' s'
        print '(a)', ''

        call db_close(db)
        call cleanup()
    end subroutine

    ! Plain int-to-string for composing bench SQL text.
    pure function itoa_b(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: b
        write(b, '(i0)') n
        s = trim(b)
    end function

    ! Full-scan match callback for the lookup comparison.
    subroutine scan_match(scan_db, rid, rbuf, ctx, stop)
        class(db_t),      intent(inout) :: scan_db
        integer(int32),   intent(in)    :: rid
        character(len=*), intent(in)    :: rbuf
        class(*),         intent(inout) :: ctx
        logical,          intent(out)   :: stop
        ! associate (unused => scan_db)   ! present only to satisfy scan_cb
        stop = .false.
        select type (ctx)
        type is (scan_ctx_t)
            if (row_get_int(rbuf, ctx%col) == ctx%target) then
                ctx%found = rid
                stop = .true.
            end if
        end select
    end subroutine

end program bench_sqr

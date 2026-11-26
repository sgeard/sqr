! utest_history — the in-memory Undo/Redo history built on the rollback journal.
!
! A labelled db_begin … db_commit is one user GESTURE; db_commit records it as a
! single bidirectional history step.  These tests prove a gesture undoes/redoes
! as one unit (including a multi-row cascade and a row insert that grows the
! file), that the redo branch is dropped by a fresh gesture, that the depth cap
! and the read-only / empty-history guards hold, and that data AND index stay
! coherent across the round-trip.  They also pin the contract around a
! gesture: a committed change that is NOT a gesture empties the history
! rather than being overwritten by a later undo, undo/redo are refused inside
! a transaction, overlapping captures undo correctly, and an undo is applied
! through the journal and keeps the schema file openable.
program utest_history
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use sqr
    use clib_wrap, only: c_rmtree, c_remove
    implicit none

    integer :: pass = 0, fail = 0

    call test_single_gesture()
    call test_cascade_one_step()
    call test_insert_extend_roundtrip()
    call test_redo_forks()
    call test_no_op_gesture()
    call test_depth_cap()
    call test_empty_and_readonly()
    call test_reset_history()
    call test_structural_ops_reset_history()
    call test_plain_commit_resets()
    call test_refused_inside_txn()
    call test_overlapping_captures()
    call test_apply_is_journalled()
    call test_ids_not_reused()

    print '(a,i0,a,i0,a)', 'sqr history tests: ', pass, ' passed, ', fail, ' failed'
    if (fail > 0) error stop 1

contains

    subroutine check(cond, label)
        logical,          intent(in) :: cond
        character(len=*), intent(in) :: label
        if (cond) then
            pass = pass + 1
            print '(a,a)', '  OK   ', label
        else
            fail = fail + 1
            print '(a,a)', '  FAIL ', label
        end if
    end subroutine

    ! Open a fresh single-table store 't(v int)' with an index on v.
    subroutine make_db(db, dir)
        type(db_t),       intent(out) :: db
        character(len=*), intent(in)  :: dir
        type(column_t) :: c(1)
        integer :: rs, ios
        ios = c_rmtree(dir)
        c(1)%name = 'v'; c(1)%dtype = DT_INT; c(1)%csize = 4
        call db_open(db, dir, rs)
        call db_create_table(db, 't', c, rs)
        call db_create_index(db, 't', 'v', rs)
    end subroutine

    ! Insert a row with value v outside any gesture; return its id.
    integer function ins(db, v) result(rid32)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: v
        character(len=:), allocatable :: buf
        integer        :: rs, ti
        integer(int32) :: r
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), int(v, int32))
        call db_insert(db, 't', buf, r, rs)
        rid32 = int(r)
    end function

    ! Rewrite row rid to value v (caller brackets the gesture).
    subroutine setv(db, rid, v)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: rid, v
        character(len=:), allocatable :: buf
        integer :: rs, ti
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call row_set_int(buf, db%tables(ti)%cols(1), int(v, int32))
        call db_update(db, 't', int(rid, int32), buf, rs)
    end subroutine

    ! Value stored in row rid (or -huge if it is gone).
    integer function getv(db, rid) result(v)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: rid
        character(len=:), allocatable :: buf
        integer :: rs, ti
        ti = db_table_index(db, 't')
        call row_alloc(buf, db%tables(ti)%record_size)
        call db_get(db, 't', int(rid, int32), buf, rs)
        if (rs == SQR_OK) then
            v = int(row_get_int(buf, db%tables(ti)%cols(1)))
        else
            v = -huge(0)
        end if
    end function

    ! Row id the index maps value v to (0 if none) — proves index coherence.
    integer function findv(db, v) result(rid)
        type(db_t), intent(inout) :: db
        integer,    intent(in)    :: v
        integer        :: rs
        integer(int32) :: r
        call db_find_by_int(db, 't', 'v', int(v, int32), r, rs)
        rid = int(r)
    end function

    ! One gesture that rewrites a single row: undo restores it, redo re-applies,
    ! and the secondary index follows both ways.
    subroutine test_single_gesture()
        type(db_t), target :: db
        integer :: r2, rs, ios
        character(len=:), allocatable :: lbl
        call make_db(db, 'utest_hist_single')
        ios = ins(db, 1); r2 = ins(db, 2); ios = ins(db, 3)
        call check(.not. db_can_undo(db), 'single: no history before any gesture')

        call db_begin(db, rs, label='Bump')
        call setv(db, r2, 99)
        call db_commit(db, rs)
        call check(rs == SQR_OK,            'single: gesture commits')
        call check(getv(db, r2) == 99,      'single: value updated in the gesture')
        call check(db_can_undo(db),         'single: undo available after a gesture')
        call check(.not. db_can_redo(db),   'single: no redo yet')

        call db_undo(db, rs, lbl)
        call check(rs == SQR_OK,            'single: undo ok')
        call check(lbl == 'Bump',           'single: undo returns the gesture label')
        call check(getv(db, r2) == 2,       'single: undo restores the value')
        call check(findv(db, 2) == r2,      'single: index restored by undo (finds old value)')
        call check(findv(db, 99) == 0,      'single: index no longer finds the undone value')
        call check(.not. db_can_undo(db),   'single: undo stack now empty')
        call check(db_can_redo(db),         'single: redo now available')

        call db_redo(db, rs, lbl)
        call check(rs == SQR_OK,            'single: redo ok')
        call check(lbl == 'Bump',           'single: redo returns the gesture label')
        call check(getv(db, r2) == 99,      'single: redo re-applies the value')
        call check(findv(db, 99) == r2,     'single: index follows redo')
        call check(db_can_undo(db),         'single: undo available again after redo')
        call db_close(db)
        ios = c_rmtree('utest_hist_single')
    end subroutine

    ! Two row rewrites inside one bracket collapse to a SINGLE undo step.
    subroutine test_cascade_one_step()
        type(db_t), target :: db
        integer :: r1, r3, rs, ios
        call make_db(db, 'utest_hist_cascade')
        r1 = ins(db, 10); ios = ins(db, 20); r3 = ins(db, 30)

        call db_begin(db, rs, label='Edit')
        call setv(db, r1, 11)
        call setv(db, r3, 33)
        call db_commit(db, rs)
        call check(getv(db, r1) == 11 .and. getv(db, r3) == 33, 'cascade: both rows updated')

        call db_undo(db, rs)
        call check(getv(db, r1) == 10 .and. getv(db, r3) == 30, 'cascade: one undo reverts BOTH rows')
        call check(.not. db_can_undo(db), 'cascade: the two writes were one step')

        call db_redo(db, rs)
        call check(getv(db, r1) == 11 .and. getv(db, r3) == 33, 'cascade: one redo re-applies BOTH rows')
        call db_close(db)
        ios = c_rmtree('utest_hist_cascade')
    end subroutine

    ! A gesture that inserts a row grows the data file; undo must remove the
    ! row (and revert live_count) WITHOUT freeing its id — the record stays as
    ! a tombstone and next_id is not lowered — and redo must bring the same
    ! row back at the same id.
    subroutine test_insert_extend_roundtrip()
        type(db_t), target :: db
        integer :: r4, rs, ios, ti
        call make_db(db, 'utest_hist_extend')
        ios = ins(db, 1)
        ti = db_table_index(db, 't')

        call db_begin(db, rs, label='Add')
        r4 = ins(db, 44)
        call db_commit(db, rs)
        call check(getv(db, r4) == 44,   'extend: inserted row present after the gesture')
        call check(findv(db, 44) == r4,  'extend: index sees the inserted row')

        call db_undo(db, rs)
        call check(getv(db, r4) == -huge(0),     'extend: undo removes the inserted row')
        call check(findv(db, 44) == 0,           'extend: index no longer finds it')
        call check(db%tables(ti)%next_id == r4 + 1, 'extend: undo keeps next_id (the id stays spent)')
        call check(db%tables(ti)%live_count == 1,   'extend: undo reverted live_count')

        call db_redo(db, rs)
        call check(getv(db, r4) == 44,             'extend: redo re-inserts the row')
        call check(findv(db, 44) == r4,            'extend: index sees the redone row')
        call check(db%tables(ti)%next_id == r4 + 1, 'extend: next_id unchanged by redo')
        call check(db%tables(ti)%live_count == 2,   'extend: redo restored live_count')
        call db_close(db)
        ios = c_rmtree('utest_hist_extend')
    end subroutine

    ! A fresh gesture after an undo discards the redo branch.
    subroutine test_redo_forks()
        type(db_t), target :: db
        integer :: r1, rs, ios
        call make_db(db, 'utest_hist_fork')
        r1 = ins(db, 1)
        call db_begin(db, rs, label='A'); call setv(db, r1, 2); call db_commit(db, rs)
        call db_undo(db, rs)
        call check(db_can_redo(db), 'fork: redo available after an undo')
        call db_begin(db, rs, label='B'); call setv(db, r1, 5); call db_commit(db, rs)
        call check(.not. db_can_redo(db), 'fork: a new gesture clears the redo branch')
        call check(getv(db, r1) == 5,     'fork: the new gesture took effect')
        call db_close(db)
        ios = c_rmtree('utest_hist_fork')
    end subroutine

    ! A bracket that mutates nothing records no step.
    subroutine test_no_op_gesture()
        type(db_t), target :: db
        integer :: rs, ios
        call make_db(db, 'utest_hist_noop')
        ios = ins(db, 1)
        call db_begin(db, rs, label='Nothing')
        call db_commit(db, rs)
        call check(rs == SQR_OK,          'noop: empty gesture commits cleanly')
        call check(.not. db_can_undo(db), 'noop: empty gesture records no undo step')
        call db_close(db)
        ios = c_rmtree('utest_hist_noop')
    end subroutine

    ! The depth cap is a ring: once full it overwrites the OLDEST step, keeping
    ! the `cap` newest with their content and labels intact.  Undoing the
    ! survivors newest-first must restore each step's before-image (and stop at
    ! the oldest survivor, NOT the pre-first-gesture state), and redo re-applies
    ! them in order — the whole point of E6's ring buffer.
    subroutine test_depth_cap()
        type(db_t), target :: db
        integer :: r1, rs, ios, k
        character(len=:), allocatable :: lbl
        character(len=2) :: lb
        call make_db(db, 'utest_hist_cap')
        r1 = ins(db, 0)
        db%hist%cap = 2                     ! keep only the two newest gestures
        do k = 1, 3
            write(lb, '(a1,i1)') 'G', k     ! G1, G2, G3 rewrite v to 1, 2, 3
            call db_begin(db, rs, label=lb); call setv(db, r1, k); call db_commit(db, rs)
        end do
        call check(getv(db, r1) == 3,       'cap: latest gesture applied')

        call db_undo(db, rs, lbl)
        call check(rs == SQR_OK .and. lbl == 'G3', 'cap: undo 1 returns the newest label (G3)')
        call check(getv(db, r1) == 2,       'cap: undo 1 restores G3 before-image (2)')
        call db_undo(db, rs, lbl)
        call check(rs == SQR_OK .and. lbl == 'G2', 'cap: undo 2 returns G2')
        call check(getv(db, r1) == 1,       'cap: undo 2 restores G2 before-image (1)')
        call db_undo(db, rs)
        call check(rs == SQR_NO_UNDO,       'cap: third undo past the cap is empty (G1 dropped)')
        call check(getv(db, r1) == 1,       'cap: stops at the oldest survivor, not the pre-G1 value 0')

        call db_redo(db, rs, lbl)
        call check(rs == SQR_OK .and. lbl == 'G2', 'cap: redo 1 re-applies G2')
        call check(getv(db, r1) == 2,       'cap: redo 1 value (2)')
        call db_redo(db, rs, lbl)
        call check(rs == SQR_OK .and. lbl == 'G3', 'cap: redo 2 re-applies G3')
        call check(getv(db, r1) == 3,       'cap: redo 2 value (3)')
        call db_close(db)
        ios = c_rmtree('utest_hist_cap')
    end subroutine

    ! Empty-history and read-only guards.
    subroutine test_empty_and_readonly()
        type(db_t), target :: db
        integer :: r1, rs, ios
        call make_db(db, 'utest_hist_guard')
        r1 = ins(db, 1)
        call db_undo(db, rs); call check(rs == SQR_NO_UNDO, 'guard: undo with empty history')
        call db_redo(db, rs); call check(rs == SQR_NO_UNDO, 'guard: redo with empty history')
        call db_begin(db, rs, label='X'); call setv(db, r1, 2); call db_commit(db, rs)
        call db_close(db)

        ! A read-only handle refuses to undo (history starts empty there anyway).
        call db_open(db, 'utest_hist_guard', rs, readonly=.true.)
        call db_undo(db, rs); call check(rs == SQR_READONLY, 'guard: undo refused read-only')
        call db_close(db)
        ios = c_rmtree('utest_hist_guard')
    end subroutine

    ! reset_history drops both stacks.
    subroutine test_reset_history()
        type(db_t), target :: db
        integer :: r1, rs, ios
        call make_db(db, 'utest_hist_reset')
        r1 = ins(db, 1)
        call db_begin(db, rs, label='A'); call setv(db, r1, 2); call db_commit(db, rs)
        call db_begin(db, rs, label='B'); call setv(db, r1, 3); call db_commit(db, rs)
        call db_undo(db, rs)
        call check(db_can_undo(db) .and. db_can_redo(db), 'reset: both stacks populated')
        call db_reset_history(db)
        call check(.not. db_can_undo(db) .and. .not. db_can_redo(db), 'reset: history cleared')
        call db_close(db)
        ios = c_rmtree('utest_hist_reset')
    end subroutine

    ! Open 't(a int, b int)' with an index on a and insert one row (a=1); no
    ! gesture yet, so history starts empty.  The caller commits the gesture.
    subroutine prime2(db, dir)
        type(db_t),       intent(out) :: db
        character(len=*), intent(in)  :: dir
        type(column_t) :: c(2)
        integer :: rs, ios, r1
        ios = c_rmtree(dir)
        c(1)%name = 'a'; c(1)%dtype = DT_INT; c(1)%csize = 4
        c(2)%name = 'b'; c(2)%dtype = DT_INT; c(2)%csize = 4
        call db_open(db, dir, rs)
        call db_create_table(db, 't', c, rs)
        call db_create_index(db, 't', 'a', rs)
        r1 = ins(db, 1)
    end subroutine

    ! H3: every structural op (compact / drop table / add or drop column / create
    ! or drop index) shifts offsets, slots, the record layout or the index set, so
    ! a captured undo/redo step — which writes absolute byte ranges — can no longer
    ! be replayed.  Each such op must clear the history; otherwise a later db_undo
    ! splices stale bytes into the new shape with SQR_OK.  Each block primes one
    ! committed gesture (undo available), runs the op, asserts the history is gone.
    subroutine test_structural_ops_reset_history()
        type(db_t), target :: db
        integer :: r1, rs, ios
        type(column_t) :: newc
        newc%name = 'c'; newc%dtype = DT_INT; newc%csize = 4

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call check(db_can_undo(db), 'struct: history primed before the op')
        call db_compact(db, 't', rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db) .and. .not. db_can_redo(db), &
                   'struct: db_compact clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_drop_index(db, 't', 'a', rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_drop_index clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_create_index(db, 't', 'b', rs)          ! b was unindexed
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_create_index clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_add_column(db, 't', newc, rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_add_column clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_drop_column(db, 't', 'b', rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_drop_column clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        ! db_reshape_table: a real layout change clears the history; a reshape to
        ! the layout the table already has rewrites nothing and keeps it.
        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_reshape_table(db, 't', db%tables(db_table_index(db, 't'))%cols, rs)
        call check(rs == SQR_OK .and. db_can_undo(db), 'struct: a no-op db_reshape_table keeps history')
        call db_reshape_table(db, 't', [db%tables(db_table_index(db, 't'))%cols, newc], rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_reshape_table clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_drop_table(db, 't', rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_drop_table clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')
    end subroutine

    ! C1: a step holds absolute byte images, so a committed change the history
    ! did not record must empty it — otherwise the next undo writes the old
    ! images back over that change and reports SQR_OK.
    subroutine test_plain_commit_resets()
        type(db_t), target :: db
        integer :: r1, r2, r3, rs, ios
        character(len=128) :: emsg
        call make_db(db, 'utest_hist_plain')
        r1 = ins(db, 1)
        call db_begin(db, rs, label='A'); r2 = ins(db, 2); call db_commit(db, rs)
        call check(db_can_undo(db), 'plain: gesture recorded')
        r3 = ins(db, 30)                                   ! auto-commit, no gesture
        call check(.not. db_can_undo(db) .and. .not. db_can_redo(db), &
                   'plain: an auto-committed write empties the history')
        call db_undo(db, rs)
        call check(rs == SQR_NO_UNDO,   'plain: undo then reports nothing to undo')
        call check(getv(db, r3) == 30 .and. findv(db, 30) == r3, &
                   'plain: the plain row survives, data and index')
        call check(getv(db, r2) == 2,   'plain: the gesture row is still there too')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK,        'plain: db_verify clean')

        ! An unlabelled explicit transaction is the same kind of change.
        call db_begin(db, rs, label='B'); call setv(db, r1, 5); call db_commit(db, rs)
        call db_begin(db, rs); call setv(db, r1, 6); call db_commit(db, rs)
        call check(.not. db_can_undo(db), 'plain: an unlabelled commit empties the history')

        ! ...and it takes the redo side with it.
        call db_begin(db, rs, label='C'); call setv(db, r1, 7); call db_commit(db, rs)
        call db_undo(db, rs)
        call check(db_can_redo(db),     'plain: redo available after an undo')
        r3 = ins(db, 31)
        call check(.not. db_can_redo(db), 'plain: a plain write empties the redo side as well')

        ! What changes nothing keeps the history: an empty unlabelled
        ! transaction, and a plain change that is rolled back.
        call db_begin(db, rs, label='D'); call setv(db, r1, 8); call db_commit(db, rs)
        call db_begin(db, rs); call db_commit(db, rs)
        call check(db_can_undo(db),     'plain: an empty unlabelled transaction keeps the history')
        call db_begin(db, rs); call setv(db, r1, 9); call db_rollback(db, rs)
        call check(db_can_undo(db),     'plain: a rolled-back change keeps the history')
        call db_undo(db, rs)
        call check(rs == SQR_OK .and. getv(db, r1) == 6, 'plain: the kept step still undoes correctly')
        call db_close(db)
        ios = c_rmtree('utest_hist_plain')
    end subroutine

    ! C2: undo/redo inside an open transaction are refused, the transaction is
    ! unharmed (its later writes still roll back) and the history is intact.
    subroutine test_refused_inside_txn()
        type(db_t), target :: db
        integer :: r1, r2, r3, rs, ios
        character(len=128) :: emsg
        call make_db(db, 'utest_hist_intxn')
        r1 = ins(db, 10)
        call db_begin(db, rs, label='A'); call setv(db, r1, 20); call db_commit(db, rs)
        call db_begin(db, rs)
        r2 = ins(db, 21)
        call db_undo(db, rs)
        call check(rs == SQR_INVALID,   'intxn: undo refused inside a transaction')
        call db_redo(db, rs)
        call check(rs == SQR_INVALID,   'intxn: redo refused inside a transaction')
        r3 = ins(db, 22)
        call db_rollback(db, rs)
        call check(rs == SQR_OK,        'intxn: the transaction still rolls back')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK,        'intxn: db_verify clean after the rollback')
        call check(findv(db, 21) == 0 .and. findv(db, 22) == 0, 'intxn: both inserts rolled back')
        call check(findv(db, 20) == r1, 'intxn: the gesture is still in place')
        call check(db_can_undo(db),     'intxn: the history is intact')
        call db_undo(db, rs)
        call check(rs == SQR_OK .and. getv(db, r1) == 10 .and. findv(db, 10) == r1, &
                   'intxn: undo works once the transaction is over')
        call db_close(db)
        ios = c_rmtree('utest_hist_intxn')
    end subroutine

    ! C3: one gesture doing a single insert (captures one index page) and then
    ! a bulk insert (captures the whole index file, already holding that
    ! modified page).  Undo must let the EARLIER capture win.
    subroutine test_overlapping_captures()
        type(db_t), target :: db
        integer :: r1, ra, rs, ios, ti, i
        integer(int32) :: rids(3)
        character(len=:), allocatable :: bufs(:)
        character(len=128) :: emsg
        call make_db(db, 'utest_hist_overlap')
        r1 = ins(db, 1)
        ti = db_table_index(db, 't')
        allocate(character(len=db%tables(ti)%record_size) :: bufs(3))
        do i = 1, 3
            bufs(i) = repeat(achar(0), db%tables(ti)%record_size)
            call row_set_int(bufs(i), db%tables(ti)%cols(1), int(100 + i, int32))
        end do
        call db_begin(db, rs, label='Mixed')
        ra = ins(db, 50)
        call db_insert_many(db, 't', bufs, rids, rs)
        call check(rs == SQR_OK,        'overlap: bulk insert inside the gesture')
        call db_commit(db, rs)
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK,        'overlap: db_verify clean after the gesture')

        call db_undo(db, rs)
        call check(rs == SQR_OK,        'overlap: undo succeeds')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK,        'overlap: db_verify clean after undo')
        call check(findv(db, 50) == 0 .and. findv(db, 102) == 0, 'overlap: every inserted row gone')
        call check(findv(db, 1) == r1,  'overlap: the earlier row still indexed')
        call check(db%tables(ti)%live_count == 1, 'overlap: live_count back to 1')

        call db_redo(db, rs)
        call check(rs == SQR_OK,        'overlap: redo succeeds')
        call db_verify(db, 't', rs, emsg)
        call check(rs == SQR_OK,        'overlap: db_verify clean after redo')
        call check(findv(db, 50) == ra .and. findv(db, 102) == int(rids(2)), &
                   'overlap: every inserted row back')
        call db_close(db)
        ios = c_rmtree('utest_hist_overlap')
    end subroutine

    ! C4 / D2: an undo or redo is a transaction of its own — the journal is
    ! voided when it returns — and an undo that takes rows away first lowers
    ! a schema file that counts them, so a process that never reaches db_close
    ! reopens with the live rows recounted from the data file.
    subroutine test_apply_is_journalled()
        type(db_t), target :: db
        integer :: r1, r2, r3, rs, ios, ti
        character(len=*), parameter :: PK = 'utest_hist_apply.sqr'
        call make_db(db, 'utest_hist_apply')
        r1 = ins(db, 1)
        ti = db_table_index(db, 't')
        call db_begin(db, rs, label='Two')
        r2 = ins(db, 2); r3 = ins(db, 3)
        call db_commit(db, rs)
        ios = c_remove(PK)
        call db_pack_live(db, PK, rs)                      ! writes the schema: 3 rows
        call check(rs == SQR_OK,        'apply: db_pack_live after the gesture')
        call check(db%tables(ti)%disk_next_id == r3 + 1, 'apply: schema file records the gesture rows')
        call db_undo(db, rs)
        call check(rs == SQR_OK,        'apply: undo succeeds')
        call check(.not. jrnl_hot(db),  'apply: journal not hot after undo')
        call check(.not. db_in_txn(db), 'apply: no transaction left open by undo')
        call check(db%tables(ti)%disk_next_id <= db%tables(ti)%next_id, &
                   'apply: schema file no longer ahead of the data file')
        call check(db%tables(ti)%next_id == r3 + 1, 'apply: next_id kept (ids stay spent)')
        call check(getv(db, r2) == -huge(0) .and. getv(db, r3) == -huge(0), &
                   'apply: the undone rows are gone')
        call db_verify(db, 't', rs)
        call check(rs == SQR_OK,        'apply: db_verify clean after undo')
        call db_redo(db, rs)
        call check(rs == SQR_OK .and. .not. jrnl_hot(db), 'apply: redo succeeds, journal not hot')
        call check(getv(db, r3) == 3 .and. findv(db, 3) == r3, 'apply: redo restored the rows')
        call check(db_can_undo(db),     'apply: undo/redo themselves keep the history')
        call db_close(db)
        ios = c_remove(PK)
        ios = c_rmtree('utest_hist_apply')
    end subroutine

    ! A row id is never handed out twice: an insert after an undo gets a NEW
    ! id, the undone id reads as gone, and the store survives a reopen (the
    ! data file's length is still the high-water).  Deeper undos retire their
    ! own rows under the ones already retired, and redo restores each row at
    ! the id it had.
    subroutine test_ids_not_reused()
        type(db_t), target :: db
        integer :: r1, r2, r3, r4, r5, rs, ios, ti
        call make_db(db, 'utest_hist_ids')
        r1 = ins(db, 1)
        ti = db_table_index(db, 't')
        call db_begin(db, rs, label='A'); r2 = ins(db, 2); call db_commit(db, rs)
        call db_begin(db, rs, label='B'); r3 = ins(db, 3); call db_commit(db, rs)

        call db_undo(db, rs)                                  ! B gone
        call db_begin(db, rs, label='C'); r4 = ins(db, 4); call db_commit(db, rs)
        call check(r4 == r3 + 1,           'ids: an insert after an undo gets a fresh id')
        call check(getv(db, r3) == -huge(0), 'ids: the undone id reads as gone')
        call check(findv(db, 3) == 0 .and. findv(db, 4) == r4, 'ids: index follows')
        call check(db%tables(ti)%live_count == 3, 'ids: live_count counts live rows only')
        call db_verify(db, 't', rs)
        call check(rs == SQR_OK,           'ids: db_verify clean with a retired row')

        call db_undo(db, rs)                                  ! C gone
        call db_undo(db, rs)                                  ! A gone
        call check(getv(db, r2) == -huge(0) .and. getv(db, r4) == -huge(0), &
                   'ids: deeper undos retire their rows too')
        call check(db%tables(ti)%next_id == r4 + 1, 'ids: next_id never lowered')
        call check(db%tables(ti)%live_count == 1,   'ids: live_count back to 1')
        call db_redo(db, rs)                                  ! A back
        call db_redo(db, rs)                                  ! C back
        call check(getv(db, r2) == 2 .and. getv(db, r4) == 4, 'ids: redo restores each row at its id')
        call check(getv(db, r3) == -huge(0), 'ids: the forked-away row stays gone')
        call check(findv(db, 2) == r2 .and. findv(db, 4) == r4, 'ids: index follows redo')

        call db_undo(db, rs)                                  ! C gone again
        r5 = ins(db, 5)                                       ! plain insert
        call check(r5 == r4 + 1,           'ids: a plain insert after an undo is fresh too')
        call db_close(db)
        call db_open(db, 'utest_hist_ids', rs)
        call check(rs == SQR_OK,           'ids: reopens')
        ti = db_table_index(db, 't')
        call check(db%tables(ti)%next_id == r5 + 1, 'ids: high-water survives the reopen')
        call check(db%tables(ti)%live_count == 3,   'ids: live_count after the reopen')
        call check(getv(db, r3) == -huge(0) .and. getv(db, r4) == -huge(0), &
                   'ids: retired rows still gone after the reopen')
        call db_verify(db, 't', rs)
        call check(rs == SQR_OK,           'ids: db_verify clean after the reopen')
        call db_compact(db, 't', rs)
        call check(rs == SQR_OK .and. db%tables(ti)%next_id == 4, &
                   'ids: db_compact reclaims the retired rows')
        call db_close(db)
        ios = c_rmtree('utest_hist_ids')
    end subroutine

end program utest_history

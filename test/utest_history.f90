! utest_history — the in-memory Undo/Redo history built on the rollback journal.
!
! A labelled db_begin … db_commit is one user GESTURE; db_commit records it as a
! single bidirectional history step.  These tests prove a gesture undoes/redoes
! as one unit (including a multi-row cascade and a row insert that grows the
! file), that the redo branch is dropped by a fresh gesture, that the depth cap
! and the read-only / empty-history guards hold, and that data AND index stay
! coherent across the round-trip.
program utest_history
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use sqr
    use clib_wrap, only: c_rmtree
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

    ! A gesture that inserts a row grows the data file; undo must truncate the
    ! append away (and revert next_id/live_count), redo must re-grow it.
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
        call check(db%tables(ti)%next_id == r4,  'extend: undo reverted next_id')

        call db_redo(db, rs)
        call check(getv(db, r4) == 44,             'extend: redo re-inserts the row')
        call check(findv(db, 44) == r4,            'extend: index sees the redone row')
        call check(db%tables(ti)%next_id == r4 + 1, 'extend: redo restored next_id')
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

    ! The depth cap drops the oldest steps.
    subroutine test_depth_cap()
        type(db_t), target :: db
        integer :: r1, rs, ios, k
        call make_db(db, 'utest_hist_cap')
        r1 = ins(db, 0)
        db%hist%cap = 2                     ! keep only the two newest gestures
        do k = 1, 3
            call db_begin(db, rs, label='G'); call setv(db, r1, k); call db_commit(db, rs)
        end do
        call db_undo(db, rs); call check(rs == SQR_OK,       'cap: undo 1 of 2 retained')
        call db_undo(db, rs); call check(rs == SQR_OK,       'cap: undo 2 of 2 retained')
        call db_undo(db, rs); call check(rs == SQR_NO_UNDO,  'cap: third undo past the cap is empty')
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

        call prime2(db, 'utest_hist_struct'); r1 = ins(db, 5)
        call db_begin(db, rs, label='G'); call setv(db, r1, 6); call db_commit(db, rs)
        call db_drop_table(db, 't', rs)
        call check(rs == SQR_OK .and. .not. db_can_undo(db), 'struct: db_drop_table clears history')
        call db_close(db); ios = c_rmtree('utest_hist_struct')
    end subroutine

end program utest_history

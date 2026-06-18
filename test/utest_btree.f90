! Unit tests for the generic on-disk B+-tree (b_tree module).
!
! The key comparator lives in a module (not an internal procedure) so it
! is passed to the tree without a trampoline / executable stack.

module utest_btree_cmp
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none
contains
    ! int32 keys, big-endian-agnostic numeric order.
    pure function icmp(a, b, ctx) result(c)
        character(len=*), intent(in) :: a, b
        class(*),         intent(in) :: ctx
        integer :: c
        integer(int32) :: ia, ib
        ia = transfer(a(1:4), ia)
        ib = transfer(b(1:4), ib)
        c = 0
        if (ia < ib) c = -1
        if (ia > ib) c = 1
    end function
end module utest_btree_cmp

! Records every journal-hook invocation into module state, so a test can
! assert that page writes drive the hook with the right classification and
! geometry.  A module procedure (not internal) so it survives being taken as
! a procedure-pointer target without a trampoline.
module utest_btree_hook
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer        :: hk_region   = 0          !! in-place overwrites seen
    integer        :: hk_extend   = 0          !! new-page (is_new) writes seen
    integer        :: hk_last_len = 0          !! len(old_bytes) of the last region call
    integer(int64) :: hk_last_off = 0_int64    !! offset of the last region call
contains
    subroutine rec_hook(ctx, offset, old_bytes, is_new, stat)
        class(*),         intent(in)  :: ctx
        integer(int64),   intent(in)  :: offset
        character(len=*), intent(in)  :: old_bytes
        logical,          intent(in)  :: is_new
        integer,          intent(out) :: stat
        if (is_new) then
            hk_extend = hk_extend + 1
        else
            hk_region   = hk_region + 1
            hk_last_off = offset
            hk_last_len = len(old_bytes)
        end if
        stat = 0
    end subroutine
    subroutine hk_reset()
        hk_region = 0; hk_extend = 0; hk_last_len = 0; hk_last_off = 0_int64
    end subroutine
end module utest_btree_hook

program utest_btree
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use :: b_tree
    use :: utest_btree_cmp, only: icmp
    use :: utest_btree_hook
    use :: clib_wrap, only: c_remove
    implicit none

    integer :: pass = 0, fail = 0
    integer :: dummy = 0
    integer, parameter :: KWIDE = 512   !! wide key -> small fan-out (deep tree)

    call t_basic()
    call t_duplicates()
    call t_persistence()
    call t_bulk_load()
    call t_split_stress()
    call t_deep_tree()
    call t_endianness()
    call t_meta_geometry()
    call t_journal_hook()
    call t_reload()

    print '(a,i0,a,i0,a)', 'b_tree tests: ', pass, ' passed, ', fail, ' failed'
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

    function k4(i) result(s)
        integer(int32), intent(in) :: i
        character(len=4) :: s
        s = transfer(i, s)
    end function

    integer(int32) function kof(s)
        character(len=*), intent(in) :: s
        kof = transfer(s(1:4), kof)
    end function

    subroutine fresh(path)
        character(len=*), intent(in) :: path
        integer :: ios
        ios = c_remove(path)
    end subroutine

    subroutine t_basic()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i, n
        integer(int32) :: pay, prev
        character(len=4) :: kk
        logical :: ok, found
        call fresh('utest_btree_1.bt')
        call bt_open(bt, 'utest_btree_1.bt', 4, .true., .true., st)
        call check(st == BT_OK, 'basic: create')
        do i = 1, 4000
            call bt_insert(bt, k4(int(mod(int(i,int64)*48271_int64, 7919_int64), int32)), &
                           int(i, int32), icmp, dummy, st)
            if (st /= BT_OK) exit
        end do
        call check(st == BT_OK .and. bt%nentries == 4000, 'basic: 4000 inserted')

        call bt_first(bt, cur, st)
        n = 0
        prev = -2147483647_int32
        do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit
            n = n + 1
            if (kof(kk) < prev) then
                call check(.false., 'basic: ascending order')
                exit
            end if
            prev = kof(kk)
        end do
        call check(n == 4000, 'basic: scanned all in order')

        call bt_insert(bt, k4(999999_int32), 7_int32, icmp, dummy, st)
        call bt_seek(bt, k4(999999_int32), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. kof(kk) == 999999 .and. pay == 7, 'basic: seek finds key')

        call bt_remove(bt, k4(999999_int32), 7_int32, icmp, dummy, found, st)
        call check(found .and. st == BT_OK, 'basic: remove existing')
        call bt_remove(bt, k4(999999_int32), 7_int32, icmp, dummy, found, st)
        call check(.not. found .and. st == BT_OK, 'basic: remove again -> not found')
        call bt_seek(bt, k4(999999_int32), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(.not. (ok .and. kof(kk) == 999999), 'basic: removed key gone')
        call bt_close(bt, st)
    end subroutine

    subroutine t_duplicates()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i, n
        integer(int32) :: pay
        character(len=4) :: kk
        logical :: ok, found
        call fresh('utest_btree_2.bt')
        call bt_open(bt, 'utest_btree_2.bt', 4, .true., .true., st)
        do i = 1, 500
            call bt_insert(bt, k4(42_int32), int(i, int32), icmp, dummy, st)
        end do
        call check(bt%nentries == 500, 'dups: 500 same-key entries')
        call bt_seek(bt, k4(42_int32), icmp, dummy, cur, st)
        n = 0
        do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit
            if (kof(kk) /= 42) exit
            n = n + 1
        end do
        call check(n == 500, 'dups: all 500 retrievable')
        call bt_remove(bt, k4(42_int32), 250_int32, icmp, dummy, found, st)
        call check(found .and. bt%nentries == 499, 'dups: remove one by payload')
        call bt_remove(bt, k4(42_int32), 250_int32, icmp, dummy, found, st)
        call check(.not. found, 'dups: that payload now absent')
        call bt_close(bt, st)
    end subroutine

    subroutine t_persistence()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i
        integer(int32) :: pay
        character(len=4) :: kk
        logical :: ok
        call fresh('utest_btree_3.bt')
        call bt_open(bt, 'utest_btree_3.bt', 4, .true., .true., st)
        do i = 1, 3000
            call bt_insert(bt, k4(int(i, int32)), int(i * 3, int32), icmp, dummy, st)
        end do
        call bt_close(bt, st)
        call check(st == BT_OK, 'persist: closed')

        call bt_open(bt, 'utest_btree_3.bt', 4, .false., .false., st)
        call check(st == BT_OK .and. bt%nentries == 3000, 'persist: reopened, count kept')
        call bt_seek(bt, k4(1500_int32), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. kof(kk) == 1500 .and. pay == 4500, &
                   'persist: key+payload survived round-trip')
        call bt_close(bt, st)
    end subroutine

    subroutine t_bulk_load()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer, parameter :: N0 = 9000
        character(len=4), allocatable :: ks(:)
        integer(int32),   allocatable :: ps(:)
        integer :: st, i, n
        integer(int32) :: pay, prev
        character(len=4) :: kk
        logical :: ok
        allocate(ks(N0), ps(N0))
        do i = 1, N0
            ks(i) = k4(int(mod(int(i,int64)*2654435761_int64, 50000_int64), int32))
            ps(i) = int(i, int32)
        end do
        call fresh('utest_btree_4.bt')
        call bt_open(bt, 'utest_btree_4.bt', 4, .true., .true., st)
        call bt_bulk_load(bt, ks, ps, icmp, dummy, st)
        call check(st == BT_OK .and. bt%nentries == N0, 'bulk: loaded N entries')
        call bt_first(bt, cur, st)
        n = 0
        prev = -2147483647_int32
        do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit
            n = n + 1
            if (kof(kk) < prev) then
                call check(.false., 'bulk: ascending')
                exit
            end if
            prev = kof(kk)
        end do
        call check(n == N0, 'bulk: all entries present and sorted')
        ! Incremental insert into a bulk-built tree still works.
        call bt_insert(bt, k4(123456_int32), 11_int32, icmp, dummy, st)
        call bt_seek(bt, k4(123456_int32), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. pay == 11, 'bulk: insert after bulk load')
        call bt_close(bt, st)

        ! Empty bulk load yields a usable empty tree.
        call fresh('utest_btree_5.bt')
        call bt_open(bt, 'utest_btree_5.bt', 4, .true., .true., st)
        call bt_bulk_load(bt, ks(1:0), ps(1:0), icmp, dummy, st)
        call check(st == BT_OK .and. bt%nentries == 0, 'bulk: empty load')
        call bt_first(bt, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(.not. ok, 'bulk: empty tree scans nothing')
        call bt_insert(bt, k4(1_int32), 1_int32, icmp, dummy, st)
        call check(bt%nentries == 1, 'bulk: insert into empty-bulk tree')
        call bt_close(bt, st)
    end subroutine

    ! Many random inserts then removes: exercises leaf/internal splits and
    ! lazy delete; verifies global order is never violated.
    subroutine t_split_stress()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer, parameter :: N0 = 30000
        integer :: st, i, n
        integer(int32) :: pay, prev, r
        character(len=4) :: kk
        logical :: ok, found
        call fresh('utest_btree_6.bt')
        call bt_open(bt, 'utest_btree_6.bt', 4, .true., .true., st)
        r = 1_int32
        do i = 1, N0
            r = int(mod(int(r,int64)*1103515245_int64 + 12345_int64, &
                        2147483647_int64), int32)
            call bt_insert(bt, k4(r), int(i, int32), icmp, dummy, st)
            if (st /= BT_OK) exit
        end do
        call check(st == BT_OK .and. bt%nentries == N0, 'stress: N inserts with splits')
        call bt_first(bt, cur, st)
        n = 0
        prev = -2147483647_int32
        do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit
            n = n + 1
            if (kof(kk) < prev) then
                call check(.false., 'stress: global order after splits')
                exit
            end if
            prev = kof(kk)
        end do
        call check(n == N0, 'stress: all present, fully ordered')
        ! Remove the first half (replay the LCG) — lazy delete path.
        r = 1_int32
        do i = 1, N0 / 2
            r = int(mod(int(r,int64)*1103515245_int64 + 12345_int64, &
                        2147483647_int64), int32)
            call bt_remove(bt, k4(r), int(i, int32), icmp, dummy, found, st)
        end do
        call check(bt%nentries == N0 - N0 / 2, 'stress: count correct after lazy deletes')
        call bt_close(bt, st)
        call fresh('utest_btree_1.bt')
        call fresh('utest_btree_2.bt')
        call fresh('utest_btree_3.bt')
        call fresh('utest_btree_4.bt')
        call fresh('utest_btree_5.bt')
        call fresh('utest_btree_6.bt')
    end subroutine

    ! Build a wide key whose first 4 bytes carry the ordered int value and
    ! the rest is fixed padding (only the prefix is compared by icmp).
    function kw_key(v) result(s)
        integer, intent(in) :: v
        character(len=KWIDE) :: s
        s(1:4)      = transfer(int(v, int32), s(1:4))
        s(5:KWIDE)  = repeat('p', KWIDE - 4)
    end function

    ! Wide keys shrink the per-page fan-out (MAXK ~ 32), so tens of
    ! thousands of inserts build a 4-level tree that splits internal nodes
    ! AND the root. This is the path the key_len=4 stress never reached and
    ! the one the page-geometry overflow bug lived on; under -fcheck=all it
    ! also guards the page-bounds invariant. Keys are a bijection of 1..N0
    ! inserted in scrambled order, so the ascending scan must read out
    ! exactly 1,2,...,N0 and the trailing padding must survive round-trip.
    subroutine t_deep_tree()
        integer, parameter :: N0 = 30000
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i, n, v
        integer(int32) :: pay, prev
        character(len=KWIDE) :: kk
        character(len=KWIDE-4), parameter :: PAD = repeat('p', KWIDE - 4)
        logical :: ok, padok
        call fresh('utest_btree_7.bt')
        call bt_open(bt, 'utest_btree_7.bt', KWIDE, .true., .true., st)
        call check(st == BT_OK, 'deep: create wide-key tree')
        do i = 1, N0
            ! 40499 is coprime to N0=30000, so v ranges over 1..N0 once.
            v = 1 + int(mod(int(i, int64) * 40499_int64, int(N0, int64)))
            call bt_insert(bt, kw_key(v), int(i, int32), icmp, dummy, st)
            if (st /= BT_OK) exit
        end do
        call check(st == BT_OK .and. bt%nentries == N0, &
                   'deep: N inserts through internal+root splits')
        call bt_first(bt, cur, st)
        n = 0
        prev = 0_int32
        padok = .true.
        scan: do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit scan
            n = n + 1
            if (kof(kk) /= n .or. kof(kk) <= prev) then
                call check(.false., 'deep: keys fully ordered 1..N')
                exit scan
            end if
            if (kk(5:KWIDE) /= PAD .or. pay < 1 .or. pay > N0) padok = .false.
            prev = kof(kk)
        end do scan
        call check(n == N0, 'deep: every key present exactly once, in order')
        call check(padok, 'deep: wide-key padding and payload round-trip')
        ! Point lookups land on the right entry across the deep tree.
        call bt_seek(bt, kw_key(1), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. kof(kk) == 1, 'deep: seek first key')
        call bt_seek(bt, kw_key(N0 / 2), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. kof(kk) == N0 / 2, 'deep: seek middle key')
        call bt_seek(bt, kw_key(N0), icmp, dummy, cur, st)
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(ok .and. kof(kk) == N0, 'deep: seek last key')
        call bt_close(bt, st)
        call fresh('utest_btree_7.bt')
    end subroutine

    ! The meta page carries a byte-order mark (an asymmetric native int32 at
    ! page byte 5). A tree carried to a host of the opposite endianness reads
    ! it back byte-swapped and must be refused (BT_VERSION) rather than
    ! misreading every scalar; a garbage mark is corruption (BT_CORRUPT). Both
    ! are simulated here by poking the mark bytes via stream access, using the
    ! same native transfer the library reads with, so the test holds on either
    ! host.
    subroutine t_endianness()
        type(btree_t)    :: bt
        integer          :: st, u, ios
        character(len=4) :: mark
        call fresh('utest_btree_8.bt')
        call bt_open(bt, 'utest_btree_8.bt', 4, .true., .true., st)
        call check(st == BT_OK, 'endian: create')
        call bt_close(bt, st)

        mark = transfer(int(z'04030201', int32), mark)   ! BOM as a BE host writes it
        open(newunit=u, file='utest_btree_8.bt', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=5) mark
        close(u)
        call bt_open(bt, 'utest_btree_8.bt', 4, .false., .false., st)
        call check(st == BT_VERSION, 'endian: opposite-endian tree rejected (BT_VERSION)')

        mark = transfer(1234567_int32, mark)             ! neither BOM nor its swap
        open(newunit=u, file='utest_btree_8.bt', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=5) mark
        close(u)
        call bt_open(bt, 'utest_btree_8.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'endian: garbage BOM rejected (BT_CORRUPT)')
        call fresh('utest_btree_8.bt')
    end subroutine

    ! The meta validation must reject geometry that is individually plausible
    ! but internally inconsistent. page_size 64 clears the old ">= 64" gate yet
    ! is far too small for key_len 4 — maxk collapses to 3 (< 32), and before
    ! the fix the first split wrote leaf/separator slots past the page buffer
    ! (the 2026-06-08 CRITICAL class, reachable again via a corrupt meta page).
    ! Poke page_size (meta byte 13) the same way t_endianness pokes the BOM.
    subroutine t_meta_geometry()
        type(btree_t)    :: bt
        integer          :: st, u, ios
        character(len=4) :: psz
        call fresh('utest_btree_8.bt')
        call bt_open(bt, 'utest_btree_8.bt', 4, .true., .true., st)
        call check(st == BT_OK, 'geom: create')
        call bt_close(bt, st)

        psz = transfer(64_int32, psz)                    ! >= 64 but maxk -> 3
        open(newunit=u, file='utest_btree_8.bt', access='stream', &
             form='unformatted', status='old', action='write', iostat=ios)
        write(u, pos=13) psz
        close(u)
        call bt_open(bt, 'utest_btree_8.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'geom: undersized page_size rejected (BT_CORRUPT)')
        call fresh('utest_btree_8.bt')
    end subroutine

    ! The journal hook is plumbing only here (no real journal): install a
    ! recorder and verify page writes drive it with the right classification
    ! and geometry, that a full-page pre-image is handed over for an in-place
    ! overwrite, and that clearing the hook silences it again.
    subroutine t_journal_hook()
        type(btree_t)     :: bt
        integer           :: st, i
        integer, target   :: ctx_tgt = 0
        class(*), pointer :: ctxp

        call fresh('utest_btree_9.bt')
        call bt_open(bt, 'utest_btree_9.bt', 4, .true., .true., st)
        do i = 1, 600
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call bt_close(bt, st)

        ! Reopen and install the hook: jbase is now the on-disk page high-water.
        call bt_open(bt, 'utest_btree_9.bt', 4, .true., .false., st)
        call check(st == BT_OK, 'hook: reopen for journalled writes')
        ctxp => ctx_tgt
        call bt_set_journal_hook(bt, rec_hook, ctxp)
        call hk_reset()

        ! Re-insert the same keys as duplicates: existing leaves are rewritten
        ! in place (region) and the overflow splits allocate fresh pages (extend).
        do i = 1, 600
            call bt_insert(bt, k4(int(i, int32)), int(i + 1000, int32), icmp, dummy, st)
        end do
        call check(hk_region >= 1, 'hook: in-place overwrite recorded')
        call check(hk_extend >= 1, 'hook: new-page write recorded')
        call check(hk_last_len == bt%page_size, 'hook: pre-image is a full page')
        call check(hk_last_off >= 1_int64 .and. &
                   mod(hk_last_off - 1_int64, int(bt%page_size, int64)) == 0_int64, &
                   'hook: region offset on a page boundary')

        ! Clearing returns the tree to un-journalled writes.
        call bt_set_journal_hook(bt)
        call hk_reset()
        call bt_insert(bt, k4(700000_int32), 7_int32, icmp, dummy, st)
        call check(hk_region == 0 .and. hk_extend == 0, 'hook: cleared -> no calls')
        call bt_close(bt, st)
        call fresh('utest_btree_9.bt')
    end subroutine

    ! bt_reload re-reads the meta page into a handle whose cached fields have
    ! drifted from disk — the resync a journal rollback relies on.  Snapshot
    ! the meta page of a small tree, grow the tree (advancing both the disk
    ! meta and the cached fields), then write the snapshot back over page 1
    ! through the tree's own unit — exactly what a journal rollback does — so
    ! the cached fields are now stale.  bt_reload must snap them back.
    subroutine t_reload()
        type(btree_t) :: bt
        integer :: st, i, r0, np0, fl0
        integer(int64) :: ne0
        character(len=:), allocatable :: meta_old

        call fresh('utest_btree_10.bt')
        call bt_open(bt, 'utest_btree_10.bt', 4, .true., .true., st)
        do i = 1, 300
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call check(st == BT_OK, 'reload: 300-entry tree built')
        ! The on-disk meta page and the cached fields agree here: capture both.
        allocate(character(len=bt%page_size) :: meta_old)
        read(bt%unit, rec=1) meta_old
        r0 = bt%root; np0 = bt%npages; fl0 = bt%first_leaf; ne0 = bt%nentries

        ! Grow: every insert rewrites the meta page, so disk and cache advance
        ! together to the larger tree.
        do i = 301, 900
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call check(bt%nentries /= ne0 .and. bt%npages /= np0, &
                   'reload: tree grew (cache now describes the larger tree)')

        ! Roll the meta page back on disk under the open handle; the cached
        ! fields are now stale relative to it.
        write(bt%unit, rec=1) meta_old
        call bt_reload(bt, st)
        call check(st == BT_OK, 'reload: succeeds')
        call check(bt%root == r0 .and. bt%npages == np0 .and. &
                   bt%first_leaf == fl0 .and. bt%nentries == ne0, &
                   'reload: cached fields restored to on-disk meta')

        ! A reload on a closed handle is an error, not a crash.
        call bt_close(bt, st)
        call bt_reload(bt, st)
        call check(st == BT_ERR, 'reload: closed handle rejected')
        call fresh('utest_btree_10.bt')
    end subroutine

end program utest_btree

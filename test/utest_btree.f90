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
    integer        :: fk_calls    = 0          !! fail_hook calls since fk_arm
    integer        :: fk_at       = 0          !! call that fails (0 = none)
    integer        :: fk_stat     = 1          !! status the failing call returns
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

    ! A hook that counts its calls and fails the `fk_at`-th with status
    ! `fk_stat` (fk_at = 0: never fails).
    subroutine fail_hook(ctx, offset, old_bytes, is_new, stat)
        class(*),         intent(in)  :: ctx
        integer(int64),   intent(in)  :: offset
        character(len=*), intent(in)  :: old_bytes
        logical,          intent(in)  :: is_new
        integer,          intent(out) :: stat
        fk_calls = fk_calls + 1
        stat = 0
        if (fk_calls == fk_at) stat = fk_stat
    end subroutine
    subroutine fk_arm(at, st)
        integer, intent(in) :: at, st
        fk_calls = 0; fk_at = at; fk_stat = st
    end subroutine
end module utest_btree_hook

program utest_btree
    use, intrinsic :: iso_fortran_env, only: int8, int32, int64
    use :: b_tree
    use :: utest_btree_cmp, only: icmp
    use :: utest_btree_hook
    use :: clib_wrap, only: c_remove, c_truncate
    implicit none

    integer :: pass = 0, fail = 0
    integer :: dummy = 0
    integer, parameter :: KWIDE = 512   !! wide key -> small fan-out (deep tree)

    call t_basic()
    call t_duplicates()
    call t_persistence()
    call t_bulk_load()
    call t_bulk_shrink()
    call t_split_stress()
    call t_deep_tree()
    call t_endianness()
    call t_meta_geometry()
    call t_journal_hook()
    call t_reload()
    call t_page_bodies()
    call t_page_size_vs_file()
    call t_discard()
    call t_bad_arguments()
    call t_failed_mutators()
    call t_surplus_pages()

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

    ! A bulk load that needs fewer pages than the file already holds: the
    ! file is not shrunk, so the page count must not fall, or the next
    ! bt_open refuses the tree (file length /= page_size * npages).
    subroutine t_bulk_shrink()
        type(btree_t) :: bt
        integer, parameter :: NBIG = 20000, NSMALL = 10, NPACK = 20100
        character(len=4), allocatable :: ks(:)
        integer(int32),   allocatable :: ps(:)
        integer :: st, i, grown
        allocate(ks(NPACK), ps(NPACK))
        do i = 1, NPACK
            ks(i) = k4(int(i, int32))
            ps(i) = int(i, int32)
        end do

        ! Shrinking load: a tree grown by inserts, reloaded with a handful.
        call fresh('utest_btree_14.bt')
        call bt_open(bt, 'utest_btree_14.bt', 4, .true., .true., st)
        do i = 1, NBIG
            call bt_insert(bt, ks(i), ps(i), icmp, dummy, st)
        end do
        grown = bt%npages
        call bt_bulk_load(bt, ks(1:NSMALL), ps(1:NSMALL), icmp, dummy, st)
        call check(st == BT_OK .and. bt%npages == grown, &
                   'shrink: page count stays at the file size')
        call bt_close(bt, st)
        call bt_open(bt, 'utest_btree_14.bt', 4, .true., .false., st)
        call check(st == BT_OK .and. bt%nentries == NSMALL, 'shrink: reopens')
        call check(scan_count(bt) == NSMALL, 'shrink: holds the loaded entries')
        ! Growing again extends the file past the surplus and still reopens.
        do i = NSMALL + 1, NBIG
            call bt_insert(bt, ks(i), ps(i), icmp, dummy, st)
        end do
        call check(st == BT_OK .and. bt%npages > grown, 'shrink: inserts extend the file')
        call bt_close(bt, st)
        call bt_open(bt, 'utest_btree_14.bt', 4, .false., .false., st)
        call check(st == BT_OK .and. scan_count(bt) == NBIG, &
                   'shrink: reopens after growing again')
        call bt_close(bt, st)
        call fresh('utest_btree_14.bt')

        ! Repack: more entries than before, packed into fewer pages.
        call fresh('utest_btree_15.bt')
        call bt_open(bt, 'utest_btree_15.bt', 4, .true., .true., st)
        do i = 1, NBIG
            call bt_insert(bt, ks(i), ps(i), icmp, dummy, st)
        end do
        grown = bt%npages
        call bt_bulk_load(bt, ks, ps, icmp, dummy, st)
        call check(st == BT_OK .and. bt%npages == grown, 'repack: page count kept')
        call bt_close(bt, st)
        call bt_open(bt, 'utest_btree_15.bt', 4, .false., .false., st)
        call check(st == BT_OK .and. scan_count(bt) == NPACK, 'repack: reopens complete')
        call bt_close(bt, st)
        call fresh('utest_btree_15.bt')
    end subroutine

    ! Entries met by a full scan, or -1 if they are not in ascending order.
    integer function scan_count(bt) result(n)
        type(btree_t), intent(in) :: bt
        type(bt_cursor_t) :: cur
        integer :: st
        integer(int32) :: pay, prev
        character(len=4) :: kk
        logical :: ok
        n = 0
        prev = -huge(prev)
        call bt_first(bt, cur, st)
        scan: do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok) exit scan
            if (kof(kk) < prev) then
                n = -1
                exit scan
            end if
            prev = kof(kk)
            n = n + 1
        end do scan
    end function

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

        ! A stored key_len of -8 makes the per-key divisor in maxk
        ! (key_len + payload + child id) exactly zero. The key_len mismatch
        ! must be rejected before maxk is evaluated: Fortran does not
        ! short-circuit .or., so with both tests in one condition this open
        ! could die on an integer divide by zero instead of returning.
        call bt_open(bt, 'utest_btree_8.bt', 4, .true., .true., st)
        call check(st == BT_OK, 'geom: re-create')
        call bt_close(bt, st)
        call poke_i32('utest_btree_8.bt', 17, -8_int32)   ! meta key_len field
        call bt_open(bt, 'utest_btree_8.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'geom: zero-divisor key_len rejected (BT_CORRUPT)')
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

    ! The meta page was validated from the start; the leaf/internal pages under
    ! it were not — nkeys and the child/next ids were transfer'd out and
    ! trusted. One wrong int32 then drove a substring past the page buffer: a
    ! read on lookup, and (via the shift that makes room in a full leaf) a WRITE
    ! on insert, which in an optimised build is silent heap corruption. Each
    ! case below pokes exactly one field of a body page through stream access,
    ! the way t_meta_geometry pokes the meta page, and demands BT_CORRUPT
    ! instead. See the on-disk invariant list at the head of b_tree_sm.
    subroutine t_page_bodies()
        integer, parameter :: PS = 4096          ! page size for key_len 4
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i
        ! maxk for key_len 4 at page 4096: (page_size - key_len - 17) /
        ! (key_len + 8). Spelled as a literal because the compilers warn on a
        ! constant-folded integer division.
        integer, parameter :: MK = 339
        integer :: n
        integer(int32) :: pay
        character(len=4) :: kk
        logical :: ok, found

        ! A 600-entry tree: root + two leaves, so both a leaf and an internal
        ! page exist to corrupt.
        call fresh('utest_btree_11.bt')
        call bt_open(bt, 'utest_btree_11.bt', 4, .true., .true., st)
        do i = 1, 600
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call bt_close(bt, st)
        call check(st == BT_OK, 'body: 600-entry tree built')

        ! --- nkeys ------------------------------------------------------
        ! Just inside the bound must still work: the guard must not reject a
        ! legitimately full node.
        call poke_i32('utest_btree_11.bt', PS + 2, int(MK, int32))
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call check(st == BT_OK, 'body: nkeys == maxk accepted')
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_OK, 'body: seek over a maxk-full leaf')
        call bt_close(bt, st)

        ! One past it cannot have been written by this library.
        call poke_i32('utest_btree_11.bt', PS + 2, int(MK + 1, int32))
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: nkeys past maxk rejected on seek')
        call bt_close(bt, st)

        ! The value that reached the out-of-bounds WRITE: 510 is in-page for
        ! every read a lookup does, and one slot short for the shift that makes
        ! room on insert, so before the fix only the write ran off the end.
        ! Key 1 routes to this leaf (page 2 holds the low keys).
        call poke_i32('utest_btree_11.bt', PS + 2, 510_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .true., .false., st)
        call bt_insert(bt, k4(1_int32), 9001_int32, icmp, dummy, st)
        call check(st == BT_CORRUPT, 'body: nkeys 510 rejected on insert (the OOB write)')
        call bt_close(bt, st)

        call poke_i32('utest_btree_11.bt', PS + 2, 100000_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: absurd nkeys rejected')
        call bt_close(bt, st)

        call poke_i32('utest_btree_11.bt', PS + 2, -1_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: negative nkeys rejected')
        call bt_close(bt, st)
        call poke_i32('utest_btree_11.bt', PS + 2, 294_int32)   ! restore

        ! --- node kind --------------------------------------------------
        call poke_i8('utest_btree_11.bt', PS + 1, 0_int8)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: node kind 0 rejected')
        call bt_close(bt, st)

        call poke_i8('utest_btree_11.bt', PS + 1, 99_int8)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: unknown node kind rejected')
        call bt_close(bt, st)
        call poke_i8('utest_btree_11.bt', PS + 1, 1_int8)       ! restore K_LEAF

        ! --- leaf chain -------------------------------------------------
        ! A leaf whose next-id points at itself used to make the cursor yield
        ! entries forever: every bt_next call returned normally, so the CALLER
        ! (db_verify) never terminated. The ceiling has to live in the cursor.
        call poke_i32('utest_btree_11.bt', PS + 6, 2_int32)     ! page 2 -> page 2
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_first(bt, cur, st)
        drain: do i = 1, 100000
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok .or. st /= BT_OK) exit drain
        end do drain
        call check(st == BT_CORRUPT, 'body: self-referential leaf chain terminates')
        call bt_close(bt, st)

        call poke_i32('utest_btree_11.bt', PS + 6, 99999_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: leaf next id past npages rejected')
        call bt_close(bt, st)
        ! A leaf whose next id names the root (an internal page): the scan
        ! must stop with BT_CORRUPT after the 294 entries of page 2, not read
        ! entries out of the internal page.
        call poke_i32('utest_btree_11.bt', PS + 6, 4_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_first(bt, cur, st)
        n = 0
        scan_int: do i = 1, 100000
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok .or. st /= BT_OK) exit scan_int
            n = n + 1
        end do scan_int
        call check(st == BT_CORRUPT .and. n == 294, 'body: leaf chain into an internal page rejected')
        call bt_close(bt, st)
        call poke_i32('utest_btree_11.bt', PS + 6, 3_int32)     ! restore

        ! --- child ids --------------------------------------------------
        ! Root is page 4 (two leaves plus the meta page); its first child slot
        ! sits at page byte 6.
        call poke_i32('utest_btree_11.bt', 3 * PS + 6, 1_int32)  ! the meta page
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: child id 1 (meta page) rejected')
        call bt_close(bt, st)

        call poke_i32('utest_btree_11.bt', 3 * PS + 6, 4_int32)  ! the root itself
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_seek(bt, k4(1_int32), icmp, dummy, cur, st)
        call check(st == BT_CORRUPT, 'body: child cycling back to the root terminates')
        call bt_close(bt, st)

        call poke_i32('utest_btree_11.bt', 3 * PS + 6, 12345_int32)
        call bt_open(bt, 'utest_btree_11.bt', 4, .false., .false., st)
        call bt_remove(bt, k4(1_int32), 1_int32, icmp, dummy, found, st)
        call check(st == BT_CORRUPT, 'body: child id past npages rejected on remove')
        call bt_close(bt, st)

        call fresh('utest_btree_11.bt')
    end subroutine

    ! The stored page_size becomes the direct-access recl, so a value that does
    ! not divide the file leaves every page read straddling two real pages —
    ! nkeys and the pointers then come back from the middle of key data. The
    ! geometry check passes it (4095 and 4097 both clear page_size >= 64 with
    ! maxk >= 32), so the file length is what catches it.
    subroutine t_page_size_vs_file()
        type(btree_t) :: bt
        integer :: st, i
        call fresh('utest_btree_12.bt')
        call bt_open(bt, 'utest_btree_12.bt', 4, .true., .true., st)
        do i = 1, 600
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call bt_close(bt, st)

        call poke_i32('utest_btree_12.bt', 13, 4097_int32)
        call bt_open(bt, 'utest_btree_12.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'psize: page_size 4097 rejected (file length)')

        call poke_i32('utest_btree_12.bt', 13, 4095_int32)
        call bt_open(bt, 'utest_btree_12.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'psize: page_size 4095 rejected (file length)')

        call poke_i32('utest_btree_12.bt', 13, 4096_int32)
        call bt_open(bt, 'utest_btree_12.bt', 4, .false., .false., st)
        call check(st == BT_OK, 'psize: correct page_size still accepted')
        call bt_close(bt, st)

        ! npages inconsistent with the file is the same defect from the other side.
        call poke_i32('utest_btree_12.bt', 29, 9_int32)
        call bt_open(bt, 'utest_btree_12.bt', 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'psize: npages disagreeing with the file rejected')

        call fresh('utest_btree_12.bt')
    end subroutine

    ! B4: an argument that does not fit the tree is BT_INVALID, and the
    ! tree is untouched.
    subroutine t_bad_arguments()
        type(btree_t)     :: bt
        type(bt_cursor_t) :: cur
        integer :: st, i, n0, n1
        integer(int64) :: s0, s1
        integer(int32) :: pay
        character(len=2) :: short
        character(len=4) :: kk
        character(len=2) :: skeys(3)
        character(len=4) :: keys(3)
        integer(int32)   :: pays(2)
        logical :: ok, found

        call fresh('utest_btree_14.bt')
        call bt_open(bt, 'utest_btree_14.bt', 4, .true., .true., st)
        do i = 1, 600
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call scan_sum(bt, n0, s0)
        short = 'ab'

        call bt_insert(bt, short, 1_int32, icmp, dummy, st)
        call check(st == BT_INVALID, 'badarg: insert with a short key')
        call bt_remove(bt, short, 1_int32, icmp, dummy, found, st)
        call check(st == BT_INVALID .and. .not. found, 'badarg: remove with a short key')
        call bt_seek(bt, short, icmp, dummy, cur, st)
        call check(st == BT_INVALID .and. .not. cur%valid, 'badarg: seek with a short key')
        call bt_first(bt, cur, st)
        call bt_next(bt, cur, short, pay, ok, st)
        call check(st == BT_INVALID .and. .not. ok, 'badarg: next into a short key')
        call bt_next(bt, cur, kk, pay, ok, st)
        call check(st == BT_OK .and. ok .and. kof(kk) == 1_int32, &
                   'badarg: the refused next did not advance the cursor')
        skeys = ['aa', 'bb', 'cc']
        call bt_bulk_load(bt, skeys, [1_int32, 2_int32, 3_int32], icmp, dummy, st)
        call check(st == BT_INVALID, 'badarg: bulk load of short keys')
        keys = [k4(1_int32), k4(2_int32), k4(3_int32)]
        pays = [1_int32, 2_int32]
        call bt_bulk_load(bt, keys, pays, icmp, dummy, st)
        call check(st == BT_INVALID, 'badarg: bulk load with fewer payloads than keys')

        call scan_sum(bt, n1, s1)
        call check(n1 == n0 .and. s1 == s0 .and. bt%nentries == 600_int64, &
                   'badarg: the tree is unchanged')
        call bt_close(bt, st)
        call fresh('utest_btree_14.bt')
    end subroutine

    ! Entry count and payload sum of a whole-tree scan.
    subroutine scan_sum(bt, n, s)
        type(btree_t),  intent(in)  :: bt
        integer,        intent(out) :: n
        integer(int64), intent(out) :: s
        type(bt_cursor_t) :: cur
        integer :: st
        integer(int32) :: pay
        character(len=4) :: kk
        logical :: ok
        n = 0
        s = 0_int64
        call bt_first(bt, cur, st)
        scan: do
            call bt_next(bt, cur, kk, pay, ok, st)
            if (.not. ok .or. st /= BT_OK) exit scan
            n = n + 1
            s = s + int(pay, int64)
        end do scan
    end subroutine

    ! B5 and B7: a mutator that fails part-way leaves root, first_leaf,
    ! npages and nentries as they were (the meta page's values), and a hook's
    ! own status is reported as BT_ERR.  A fresh tree for each failure point.
    subroutine t_failed_mutators()
        integer, parameter :: MK = 339           ! entries per leaf, key_len 4
        character(len=*), parameter :: P = 'utest_btree_15.bt'
        type(btree_t)     :: bt, b0
        integer :: st, n, ncalls, bad, bad_open
        logical :: found, whole
        character(len=4), allocatable :: keys(:)
        integer(int32),   allocatable :: pays(:)

        ! --- insert that splits the root leaf -------------------------
        call hooked_tree(bt, P, MK, b0)
        call fk_arm(0, 1)
        call bt_insert(bt, k4(int(MK + 1, int32)), 1_int32, icmp, dummy, st)
        ncalls = fk_calls
        call check(st == BT_OK .and. ncalls >= 4, 'failmut: a splitting insert writes 4+ pages')
        call unhook(bt)
        bad = 0
        bad_open = 0
        whole = .false.
        ins_sweep: do n = 1, ncalls
            call hooked_tree(bt, P, MK, b0)
            call fk_arm(n, 1)
            call bt_insert(bt, k4(int(MK + 1, int32)), 1_int32, icmp, dummy, st)
            if (st == BT_OK .or. .not. same_meta(bt, b0)) bad = bad + 1
            ! Closed as it stands (the meta page keeps the old count, over
            ! whatever pages the failed insert left), the tree must reopen.
            call bt_set_journal_hook(bt)
            call bt_close(bt, st)
            call bt_open(bt, P, 4, .true., .false., st)
            if (st /= BT_OK) then
                bad_open = bad_open + 1
                cycle ins_sweep
            end if
            if (bt%npages < b0%npages) bad_open = bad_open + 1
            ! n = 2: only the new right leaf was written, so the tree is
            ! whole and one page is surplus.  It takes the insert and reopens.
            if (n == 2) then
                whole = bt%npages == b0%npages + 1 .and. scan_count(bt) == MK
                call bt_insert(bt, k4(int(MK + 1, int32)), 1_int32, icmp, dummy, st)
                whole = whole .and. st == BT_OK
                call bt_close(bt, st)
                call bt_open(bt, P, 4, .false., .false., st)
                whole = whole .and. st == BT_OK
                if (st == BT_OK) whole = whole .and. scan_count(bt) == MK + 1
            end if
            call bt_discard(bt)
        end do ins_sweep
        call check(bad == 0, 'failmut: a failed insert leaves the handle as it was')
        call check(bad_open == 0, 'failmut: the tree reopens after any failed insert')
        call check(whole, 'failmut: an orphan page is adopted, the tree whole and usable')

        ! --- remove --------------------------------------------------
        call hooked_tree(bt, P, 600, b0)
        call fk_arm(0, 1)
        call bt_remove(bt, k4(7_int32), 7_int32, icmp, dummy, found, st)
        ncalls = fk_calls
        call check(st == BT_OK .and. found .and. ncalls >= 2, 'failmut: remove writes 2+ pages')
        call unhook(bt)
        bad = 0
        rem_sweep: do n = 1, ncalls
            call hooked_tree(bt, P, 600, b0)
            call fk_arm(n, 1)
            call bt_remove(bt, k4(7_int32), 7_int32, icmp, dummy, found, st)
            if (st == BT_OK .or. .not. same_meta(bt, b0)) bad = bad + 1
            call unhook(bt)
        end do rem_sweep
        call check(bad == 0, 'failmut: a failed remove leaves the handle as it was')

        ! --- bulk load of a different number of entries ---------------
        allocate(keys(1000), pays(1000))
        do n = 1, 1000
            keys(n) = k4(int(n, int32))
            pays(n) = int(n, int32)
        end do
        call hooked_tree(bt, P, 600, b0)
        call fk_arm(0, 1)
        call bt_bulk_load(bt, keys, pays, icmp, dummy, st)
        ncalls = fk_calls
        call check(st == BT_OK .and. ncalls >= 2, 'failmut: bulk load writes pages')
        call unhook(bt)
        bad = 0
        bulk_sweep: do n = 1, ncalls
            call hooked_tree(bt, P, 600, b0)
            call fk_arm(n, 1)
            call bt_bulk_load(bt, keys, pays, icmp, dummy, st)
            if (st == BT_OK .or. bt%root /= b0%root .or. bt%first_leaf /= b0%first_leaf .or. &
                bt%nentries /= b0%nentries .or. bt%npages < b0%npages) bad = bad + 1
            call unhook(bt)
        end do bulk_sweep
        call check(bad == 0, 'failmut: a failed bulk load leaves the handle as it was')

        ! --- B7: the hook's own status is not a tree status -----------
        call hooked_tree(bt, P, MK, b0)
        call fk_arm(1, 3)                        ! 3 = BT_VERSION's value
        call bt_insert(bt, k4(int(MK + 1, int32)), 1_int32, icmp, dummy, st)
        call check(st == BT_ERR, 'failmut: a hook status of 3 is reported as BT_ERR')
        call unhook(bt)
        call fresh(P)
    end subroutine

    ! A file may hold whole pages past the count in its meta page (a failed
    ! mutation, or a crash, after a new page was written and before the meta
    ! page).  bt_open adopts them; a partial page or a short file is refused.
    subroutine t_surplus_pages()
        integer, parameter :: PS = 4096, NENT = 600
        character(len=*), parameter :: P = 'utest_btree_16.bt'
        type(btree_t) :: bt
        integer :: st, i, np0, ios

        call fresh(P)
        call bt_open(bt, P, 4, .true., .true., st)
        do i = 1, NENT
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        np0 = bt%npages
        call bt_close(bt, st)

        ! Three surplus pages, read-only: counted in the handle, file untouched.
        call append_bytes(P, 3 * PS)
        call bt_open(bt, P, 4, .false., .false., st)
        call check(st == BT_OK, 'surplus: whole extra pages open read-only')
        call check(bt%npages == np0 + 3, 'surplus: they are counted in npages')
        call check(scan_count(bt) == NENT, 'surplus: every entry is still met')
        call bt_close(bt, st)

        ! Read-write: the tree grows past the surplus and reopens exactly.
        call bt_open(bt, P, 4, .true., .false., st)
        call check(st == BT_OK .and. bt%npages == np0 + 3, 'surplus: open read-write')
        do i = NENT + 1, NENT + 2000
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        call check(st == BT_OK .and. bt%npages > np0 + 3, 'surplus: inserts extend past them')
        call bt_close(bt, st)
        call bt_open(bt, P, 4, .false., .false., st)
        call check(st == BT_OK, 'surplus: reopens after growing')
        call check(scan_count(bt) == NENT + 2000, 'surplus: every entry after growing')
        np0 = bt%npages
        call bt_close(bt, st)

        ! A partial page at the end is not a page.
        call append_bytes(P, 1)
        call bt_open(bt, P, 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'surplus: a partial last page is rejected')
        ios = c_truncate(P, int(np0, int64) * PS)
        call bt_open(bt, P, 4, .false., .false., st)
        call check(st == BT_OK, 'surplus: accepted again at whole pages')
        call bt_close(bt, st)

        ! Fewer pages than the meta page counts.
        ios = c_truncate(P, int(np0 - 1, int64) * PS)
        call bt_open(bt, P, 4, .false., .false., st)
        call check(st == BT_CORRUPT, 'surplus: a file short of npages is rejected')
        call fresh(P)
    end subroutine

    ! Append `n` zero bytes to a file.
    subroutine append_bytes(path, n)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: n
        integer :: u, ios
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='write', position='append', iostat=ios)
        if (ios /= 0) return
        write(u, iostat=ios) repeat(char(0), n)
        close(u, iostat=ios)
    end subroutine

    ! A fresh tree of `nent` ascending entries with fail_hook installed (not
    ! armed); `b0` is a copy of the handle as it then stands.
    subroutine hooked_tree(bt, path, nent, b0)
        type(btree_t),    intent(inout) :: bt
        character(len=*), intent(in)    :: path
        integer,          intent(in)    :: nent
        type(btree_t),    intent(out)   :: b0
        integer, target, save :: ctx_tgt = 0
        class(*), pointer :: ctxp
        integer :: i, st
        call fresh(path)
        call bt_open(bt, path, 4, .true., .true., st)
        do i = 1, nent
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
        end do
        ctxp => ctx_tgt
        call bt_set_journal_hook(bt, fail_hook, ctxp)
        call fk_arm(0, 1)
        b0 = bt
    end subroutine

    ! Remove the hook and close without writing the meta page.
    subroutine unhook(bt)
        type(btree_t), intent(inout) :: bt
        call bt_set_journal_hook(bt)
        call bt_discard(bt)
    end subroutine

    logical function same_meta(a, b)
        type(btree_t), intent(in) :: a, b
        same_meta = a%root == b%root .and. a%first_leaf == b%first_leaf .and. &
                    a%npages == b%npages .and. a%nentries == b%nentries
    end function

    ! Overwrite one int32 / int8 at a 1-based byte position, using the same
    ! native transfer the library reads with (so the test holds on either
    ! byte order) — the poke t_endianness and t_meta_geometry do inline,
    ! shared here because the body-page tests need a great many of them.
    subroutine poke_i32(path, pos, v)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: pos
        integer(int32),   intent(in) :: v
        character(len=4) :: b
        integer :: u, ios
        b = transfer(v, b)
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='write', iostat=ios)
        if (ios /= 0) return
        write(u, pos=pos) b
        close(u)
    end subroutine

    subroutine poke_i8(path, pos, v)
        character(len=*), intent(in) :: path
        integer,          intent(in) :: pos
        integer(int8),    intent(in) :: v
        character(len=1) :: b
        integer :: u, ios
        b = transfer(v, b)
        open(newunit=u, file=path, access='stream', form='unformatted', &
             status='old', action='write', iostat=ios)
        if (ios /= 0) return
        write(u, pos=pos) b
        close(u)
    end subroutine

    ! bt_discard: close without meta flush, handle reset, safe when already
    ! closed, and the on-disk image (meta written by the last mutator) stays
    ! openable.
    subroutine t_discard()
        type(btree_t) :: bt
        integer :: st, i
        call fresh('utest_btree_d.bt')
        call bt_open(bt, 'utest_btree_d.bt', 4, .true., .true., st)
        do i = 1, 100
            call bt_insert(bt, k4(int(i, int32)), int(i, int32), icmp, dummy, st)
            if (st /= BT_OK) exit
        end do
        call check(st == BT_OK, 'discard: 100 inserted')
        call bt_discard(bt)
        call check(bt%unit == -1, 'discard: handle closed')
        call bt_discard(bt)
        call check(bt%unit == -1, 'discard: idempotent on a closed handle')
        call bt_open(bt, 'utest_btree_d.bt', 4, .true., .false., st)
        call check(st == BT_OK .and. bt%nentries == 100, &
                   'discard: file reopens with every entry')
        call bt_close(bt, st)
        call fresh('utest_btree_d.bt')
    end subroutine

end program utest_btree

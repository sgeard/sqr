!! SPDX-License-Identifier: MIT
!! Copyright (c) 2026 Simon Geard
!! Vendored into sqr from https://github.com/sgeard/cmdgraph (fortran/src); kept in sync by hand.
!!
submodule (dlist) dlist_sm
    implicit none

contains

    module function make_int_node(v) result(n)
        integer, intent(in)      :: v
        type(dlist_node_integer) :: n
        n%data = v
    end function make_int_node

    module function make_real_node(v) result(n)
        real(8), intent(in)   :: v
        type(dlist_node_real) :: n
        n%data = v
    end function make_real_node

    module function make_real_a_node(v) result(n)
        real(8), intent(in)     :: v(:)
        type(dlist_node_real_a) :: n
        n%data = v
    end function make_real_a_node

    module function make_real_m_node(v) result(n)
        real(8), intent(in)     :: v(:,:)
        type(dlist_node_real_m) :: n
        n%data = v
    end function make_real_m_node

    module function make_char_node(v) result(n)
        character(len=*), intent(in) :: v
        type(dlist_node_char)        :: n
        n%data = v
    end function make_char_node

    module function iterate_ll(this, f) result(r)
        class(dlist_t), intent(inout), target :: this
        procedure(command_fun)                :: f
        logical :: r
        type(dlist_node_t), pointer :: token
        r = .true.
        token => this%begin
        do
            if (.not. associated(token)) exit
            call f(token%data, r)
            if (.not. r) exit
            token => token%next
        end do
    end function iterate_ll

    module function reverse_iterate_ll(this, f) result(r)
        class(dlist_t), intent(inout), target :: this
        procedure(command_fun)                :: f
        logical :: r
        type(dlist_node_t), pointer :: token
        r = .true.
        token => this%end
        do
            if (.not. associated(token)) exit
            call f(token%data, r)
            if (.not. r) exit
            token => token%previous
        end do
    end function reverse_iterate_ll

    module subroutine append_ll(lst, data)
        class(dlist_t), intent(inout)        :: lst
        class(dlist_node_data_t), intent(in) :: data
        if (.not. associated(lst%begin)) then
            allocate(lst%begin)
            lst%begin%data = data
            lst%end => lst%begin
        else
            allocate(lst%end%next)
            lst%end%next%data = data
            lst%end%next%previous => lst%end
            lst%end => lst%end%next
        end if
        lst%num_of_elements = lst%num_of_elements + 1
    end subroutine append_ll

    module subroutine insert_ll(lst, idx, data)
        class(dlist_t), intent(inout)        :: lst
        integer, intent(in)                  :: idx
        class(dlist_node_data_t), intent(in) :: data
        integer :: i
        type(dlist_node_t), pointer :: this, new_node, prev

        allocate(new_node)
        new_node%data = data

        if (.not. associated(lst%begin)) then
            ! Empty list — idx is irrelevant.
            lst%begin => new_node
            lst%end   => new_node
        else if (idx <= 1) then
            ! Insert at position 1: prepend (also covers out-of-range low).
            new_node%next      => lst%begin
            lst%begin%previous => new_node
            lst%begin          => new_node
        else if (idx > lst%num_of_elements) then
            ! Out-of-range high: append.
            new_node%previous => lst%end
            lst%end%next      => new_node
            lst%end           => new_node
        else
            ! Insert before the node currently at position idx (2..n), so the
            ! new element ends up AT idx and later elements shift right.
            this => lst%begin
            do i = 2, idx
                this => this%next
            end do
            prev => this%previous
            new_node%previous => prev
            new_node%next     => this
            prev%next          => new_node
            this%previous      => new_node
        end if
        lst%num_of_elements = lst%num_of_elements + 1
    end subroutine insert_ll

    module subroutine remove_ll(lst, idx)
        class(dlist_t), intent(inout) :: lst
        integer, intent(in)           :: idx
        integer :: i
        type(dlist_node_t), pointer   :: this
        if (idx < 1 .or. idx > lst%num_of_elements) then
            return ! index out of range ignored
        end if

        ! Locate the node to remove
        this => lst%begin
        do i=2,idx
            this => this%next
        end do

        ! Splice out: update neighbours
        if (associated(this%previous)) then
            this%previous%next => this%next
        else
            lst%begin => this%next
        end if
        if (associated(this%next)) then
            this%next%previous => this%previous
        else
            lst%end => this%previous
        end if

        deallocate(this)
        lst%num_of_elements = lst%num_of_elements - 1

    end subroutine remove_ll

    module subroutine print_ll(lst, unit)
        use iso_fortran_env, only: output_unit
        class(dlist_t), intent(in)    :: lst
        integer, intent(in), optional :: unit
        type(dlist_node_t), pointer   :: next
        integer                       :: idx, u
        u = output_unit
        if (present(unit)) u = unit
        write(u,'(a)') 'Nodes:'
        next => lst%begin
        if (.not. associated(next)) then
            write(u,'(a)') ' *** none found ***'
            return
        end if
        idx = 0
        each_node: do
            if (.not. associated(next)) exit each_node
            idx = idx + 1
            ! Render each built-in node kind; unknown extensions print a tag.
            select type (d => next%data)
            type is (dlist_node_integer)
                write(u,'(4x,i0,a,i0)') idx, ': int    = ', d%data
            type is (dlist_node_real)
                write(u,'(4x,i0,a,g0)') idx, ': real   = ', d%data
            type is (dlist_node_real_a)
                write(u,'(4x,i0,a,*(1x,g0))') &
                    idx, ': real_a =', d%data
            type is (dlist_node_real_m)
                write(u,'(4x,i0,a,i0,a,i0)') &
                    idx, ': real_m = [', size(d%data,1), &
                    ' x ', size(d%data,2)
            type is (dlist_node_char)
                write(u,'(4x,i0,a,a)') idx, ': char   = ', d%data
            class default
                write(u,'(4x,i0,a)') idx, ': <user-defined>'
            end select
            next => next%next
        end do each_node
    end subroutine print_ll

    module integer function size_ll(lst)
        class(dlist_t), intent(in)    :: lst
        size_ll = lst%num_of_elements
    end function size_ll

    integer function calc_size(lst)
        class(dlist_t), intent(inout) :: lst
        type(dlist_node_t), pointer   :: node
        calc_size = 0
        node => lst%begin
        do
            if (.not. associated(node)) exit
            calc_size = calc_size + 1
            node => node%next
        end do
    end function calc_size

    module subroutine clear_ll(lst)
        class(dlist_t), intent(inout) :: lst
        type(dlist_node_t), pointer   :: cur, next
        cur => lst%begin
        do
            if (.not. associated(cur)) exit
            next => cur%next
            deallocate(cur)
            cur => next
        end do
        nullify(lst%end)
        nullify(lst%begin)
        lst%num_of_elements = 0
    end subroutine clear_ll

    module subroutine assign_ll(lhs, rhs)
        class(dlist_t), intent(inout) :: lhs
        class(dlist_t), intent(in)    :: rhs
        type(dlist_node_t), pointer   :: cursor
        type(dlist_t)                 :: tmp

        cursor => rhs%begin
        do
            if (.not. associated(cursor)) exit
            call tmp%append(cursor%data)
            cursor => cursor%next
        end do

        call lhs%clear()
        lhs%begin => tmp%begin
        lhs%end => tmp%end
        lhs%num_of_elements = tmp%num_of_elements
        nullify(tmp%begin)
        nullify(tmp%end)
        tmp%num_of_elements = 0
    end subroutine assign_ll

    module subroutine finalize_ll(lst)
        type(dlist_t), intent(inout) :: lst
        call lst%clear()
    end subroutine finalize_ll

    module function get_ll(this, idx) result(node)
        class(dlist_t), intent(in)            :: this
        integer, intent(in)                   :: idx
        class(dlist_node_data_t), allocatable :: node
        type(dlist_node_t), pointer           :: cursor
        integer                               :: i

        if (idx < 1 .or. idx > this%num_of_elements) return

        cursor => this%begin
        do i = 2, idx
            cursor => cursor%next
        end do
        allocate(node, source=cursor%data)
    end function get_ll

end submodule dlist_sm

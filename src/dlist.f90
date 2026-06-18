!! SPDX-License-Identifier: MIT
!! Copyright (c) 2026 Simon Geard
!! Vendored into sqr from https://github.com/sgeard/cmdgraph (fortran/src); kept in sync by hand.
!!
!! Polymorphic doubly-linked list used as the parsed-argument container.
!!
!! Elements are stored as class(`dlist_node_data_t`) and accessed by
!! index (`get`) or by iterating with a visitor procedure (`iterate`).
!! The built-in node types cover the argument kinds used by cmdgraph:
!! integer, real(8), character, and real arrays.
module dlist
    public

    !! Doubly-linked list with polymorphic element storage.
    !! Iterate with `iterate` / `reverse_iterate`, or access by index with `get`.
    type dlist_t
        private
        type(dlist_node_t), pointer :: begin => null()
        type(dlist_node_t), pointer :: end   => null()
        integer                     :: num_of_elements = 0
    contains
        procedure, public :: iterate         => iterate_ll          !! Apply a visitor to each element in order
        procedure, public :: reverse_iterate => reverse_iterate_ll  !! Apply a visitor to each element in reverse
        procedure, public :: print           => print_ll            !! Print list contents to stdout
        procedure, public :: append          => append_ll           !! Append a node to the tail
        procedure, public :: insert          => insert_ll           !! Insert a node at 1-based index
        procedure, public :: remove          => remove_ll           !! Remove element at 1-based index
        procedure, public :: size            => size_ll             !! Number of elements
        procedure, public :: clear           => clear_ll            !! Remove all elements
        procedure, public :: get             => get_ll              !! Index-based accessor (1-based); unallocated if out of range
        procedure, private :: assign_ll
        generic, public :: assignment(=)     => assign_ll
        final :: finalize_ll
    end type dlist_t

    !! Abstract base for list element data.  Extend this type to store custom data.
    type, abstract :: dlist_node_data_t
    end type dlist_node_data_t

    !! Node holding a single `integer` value.
    type, extends(dlist_node_data_t) :: dlist_node_integer
        integer :: data = 0
    end type dlist_node_integer

    !! Node holding a single `real(8)` value.
    type, extends(dlist_node_data_t) :: dlist_node_real
        real(8) :: data = 0.0d0
    end type dlist_node_real

    !! Node holding a `real(8)` rank-1 array.
    type, extends(dlist_node_data_t) :: dlist_node_real_a
        real(8), allocatable :: data(:)
    end type dlist_node_real_a

    !! Node holding a `real(8)` rank-2 array.
    type, extends(dlist_node_data_t) :: dlist_node_real_m
        real(8), allocatable :: data(:,:)
    end type dlist_node_real_m

    !! Node holding an allocatable `character` string.
    type, extends(dlist_node_data_t) :: dlist_node_char
        character(len=:), allocatable :: data
    end type dlist_node_char
    
    !! Construct an integer node.
    interface int_node
        module procedure make_int_node
    end interface int_node

    !! Construct a real(8) scalar node.
    interface real_node
        module procedure make_real_node
    end interface real_node

    !! Construct a real(8) rank-1 array node.
    interface real_a_node
        module procedure make_real_a_node
    end interface real_a_node

    !! Construct a real(8) rank-2 array node.
    interface real_m_node
        module procedure make_real_m_node
    end interface real_m_node

    !! Construct a character node.
    interface char_node
        module procedure make_char_node
    end interface char_node

    !! Generic node constructor — dispatches on argument type.
    interface make_node
        module procedure make_int_node
        module procedure make_real_node
        module procedure make_real_a_node
        module procedure make_real_m_node
        module procedure make_char_node
    end interface

    interface
        module function make_int_node(v) result(n)
            integer, intent(in)          :: v
            type(dlist_node_integer)     :: n
        end function make_int_node

        module function make_real_node(v) result(n)
            real(8), intent(in)          :: v
            type(dlist_node_real)        :: n
        end function make_real_node

        module function make_real_a_node(v) result(n)
            real(8), intent(in)     :: v(:)
            type(dlist_node_real_a) :: n
        end function make_real_a_node

        module function make_real_m_node(v) result(n)
            real(8), intent(in)     :: v(:,:)
            type(dlist_node_real_m) :: n
        end function make_real_m_node

        module function make_char_node(v) result(n)
            character(len=*), intent(in) :: v
            type(dlist_node_char)        :: n
        end function make_char_node
    end interface

    type dlist_node_t
        private
        class(dlist_node_data_t), allocatable :: data
        type(dlist_node_t), pointer :: next     => null()
        type(dlist_node_t), pointer :: previous => null()
    end type dlist_node_t

    !! Visitor procedure signature for `iterate` / `reverse_iterate`.
    !! Set `ok = .false.` to stop iteration early.
    abstract interface
        subroutine command_fun(command, ok)
            import dlist_node_data_t
            class(dlist_node_data_t), intent(in) :: command
            logical, intent(out)                 :: ok
        end subroutine command_fun
    end interface

    interface
        !! Apply `f` to each element in forward order.
        !! Returns `.true.` if all elements were visited (i.e. `f` never set `ok=.false.`).
        module function iterate_ll(this, f) result(r)
            class(dlist_t), intent(inout), target :: this
            procedure(command_fun)                :: f
            logical :: r
        end function iterate_ll

        !! Insert `data` at 1-based `idx`, shifting later elements right.
        !! Out-of-range low values prepend; out-of-range high values append.
        module subroutine insert_ll(lst, idx, data)
            class(dlist_t), intent(inout)        :: lst
            integer, intent(in)                  :: idx
            class(dlist_node_data_t), intent(in) :: data
        end subroutine insert_ll

        !! Remove the element at 1-based `idx`.
        !! Silently does nothing if `idx` is out-of-range.
        module subroutine remove_ll(lst, idx)
            class(dlist_t), intent(inout) :: lst
            integer, intent(in)           :: idx
        end subroutine remove_ll

        !! Apply `f` to each element in reverse order.
        module function reverse_iterate_ll(this, f) result(r)
            class(dlist_t), intent(inout), target :: this
            procedure(command_fun)                :: f
            logical :: r
        end function reverse_iterate_ll

        !! Print list contents to `unit` (default `output_unit`).
        !! Renders the built-in node kinds; extensions print as `<user-defined>`.
        module subroutine print_ll(lst, unit)
            class(dlist_t), intent(in)    :: lst
            integer, intent(in), optional :: unit
        end subroutine print_ll

        !! Append `data` to the tail of the list.
        module subroutine append_ll(lst, data)
            class(dlist_t), intent(inout)        :: lst
            class(dlist_node_data_t), intent(in) :: data
        end subroutine append_ll

        !! Number of elements in the list.
        module integer function size_ll(lst)
            class(dlist_t), intent(in)    :: lst
        end function size_ll

        !! Remove all elements.
        module subroutine clear_ll(lst)
            class(dlist_t), intent(inout) :: lst
        end subroutine clear_ll

        module subroutine assign_ll(lhs, rhs)
            class(dlist_t), intent(inout) :: lhs
            class(dlist_t), intent(in)    :: rhs
        end subroutine assign_ll

        module subroutine finalize_ll(lst)
            type(dlist_t), intent(inout) :: lst
        end subroutine finalize_ll

        !! Index-based accessor (1-based).  Returns an allocatable polymorphic
        !! copy of the node data.  Unallocated if `idx` is out of range —
        !! check `allocated(node)` before `select type`.
        module function get_ll(this, idx) result(node)
            class(dlist_t), intent(in)            :: this
            integer, intent(in)                   :: idx
            class(dlist_node_data_t), allocatable :: node
        end function get_ll

    end interface

end module dlist

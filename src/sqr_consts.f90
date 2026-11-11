!! Named constants shared across sqr: scalar storage widths and the
!! character codes the engine, the wire layer and the shells test for.
!!
!! Parameters only — no procedures, so no submodule. Everything here is a
!! compile-time constant; using a name costs nothing at run time.
!!
!! `b_tree` deliberately does not use this module (it is a standalone
!! component and keeps its own private widths), nor does `clib_wrap`.

module sqr_consts
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    private

    ! --- Scalar storage widths (bytes) ---
    integer, parameter, public :: BITS_PER_BYTE = 8
    integer, parameter, public :: I32_BYTES = storage_size(0_int32)   / BITS_PER_BYTE  !! Bytes in an `int32`
    integer, parameter, public :: I64_BYTES = storage_size(0_int64)   / BITS_PER_BYTE  !! Bytes in an `int64`
    integer, parameter, public :: R64_BYTES = storage_size(0._real64) / BITS_PER_BYTE  !! Bytes in a `real64`

    ! --- Scratch buffers for formatting numbers as text ---
    !! Long enough for any default integer written with `i0` (an `int32` needs
    !! at most 11 characters, sign included).
    integer, parameter, public :: INT_TEXT_LEN = 12
    !! Long enough for any number the shells and the SQL layer format: an
    !! integer, or a real in the `es15.8` form they use.
    integer, parameter, public :: NUM_TEXT_LEN = 32

    ! --- Single characters ---
    character(len=1), parameter, public :: CH_NUL       = achar(0)    !! NUL: `DT_CHAR` padding / terminator
    character(len=1), parameter, public :: CH_TAB       = achar(9)    !! Horizontal tab
    character(len=1), parameter, public :: CH_LF        = achar(10)   !! Line feed
    character(len=1), parameter, public :: CH_CR        = achar(13)   !! Carriage return
    character(len=1), parameter, public :: CH_BACKSLASH = achar(92)   !! `\` (spelled by code so no source line ends in one)
    character(len=1), parameter, public :: CH_DEL       = achar(127)  !! DEL, the one control character above the printables
    !! The largest byte, 0xFF: the maximum under byte-wise ordering, used to
    !! build a key no stored string can exceed. It never occurs in well-formed
    !! UTF-8, so the bound holds for UTF-8 text as well as single-byte text.
    character(len=1), parameter, public :: CH_HIGH      = char(255)

    !! Either path separator: `/`, or `\` as written on Windows.
    character(len=*), parameter, public :: PATH_SEPS = '/' // CH_BACKSLASH

    ! --- ASCII code points ---
    !! Lowest printable ASCII code (space); every code below it is a control
    !! character.
    integer, parameter, public :: ASCII_FIRST_PRINTABLE = iachar(' ')
    !! Distance from an ASCII upper-case letter to its lower-case form.
    !! ASCII only: bytes of a multi-byte UTF-8 character are left alone by the
    !! `A`..`Z` range test that guards its use.
    integer, parameter, public :: ASCII_CASE_OFFSET = iachar('a') - iachar('A')

end module sqr_consts

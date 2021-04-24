module decompMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module provides a decomposition into a clumped data structure which can
  ! be mapped back to atmosphere physics chunks
  !-----------------------------------------------------------------------

  use shr_kind_mod, only : r8 => shr_kind_r8
  implicit none

  type bounds_type
     integer :: begg, endg       ! Beginning and ending gridcell index
     integer :: begl, endl       ! Beginning and ending landunit index
     integer :: begc, endc       ! Beginning and ending column index
     integer :: begp, endp       ! Beginning and ending patch index
  end type bounds_type
  public bounds_type

contains

  subroutine get_clump_bounds (n, bounds)

    implicit none
    integer, intent(in)  :: n                ! processor clump index
    type(bounds_type), intent(out) :: bounds

    ! Define grid cell (g), land unit (l), column (c), and patch (p) bounds
    ! for CLM g/l/c/p hierarchy. CLM processes clumps of gridcells (and
    ! associated subgrid-scale entities) with length defined by
    ! begg/endg, begl/endl, begc/endc, and begp/endp. This code assumes
    ! that a grid cell has one land unit with one column and one patch. It
    ! processes a single grid cell.

    bounds%begg = 1
    bounds%endg = 1

    bounds%begl = 1
    bounds%endl = 1

    bounds%begc = 1
    bounds%endc = 1

    bounds%begp = 1
    bounds%endp = 1

  end subroutine get_clump_bounds

end module decompMod

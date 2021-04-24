module initSubgridMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Lower-level routines for initializing the subgrid structure
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use PatchType, only : patch
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: add_patch
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine add_patch (pi, ptype)
    !
    ! !DESCRIPTION:
    ! Add an entry in the patch-level arrays. pi gives the index of the last patch added;
    ! the new patch is added at pi+1; and the pi argument is incremented accordingly. The
    ! input value of pi is the index of last patch added, and the output value is the index
    ! of the newly-added patch.
    !
    ! NOTE: The code (as used here) processes one patch (one grid cell with one column and
    ! one patch) and the subgrid patch structure is set accordingly.
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(inout) :: pi     ! patch index
    integer, intent(in)    :: ptype  ! patch type
    !---------------------------------------------------------------------

    pi = pi + 1

    patch%column(pi) = 1
    patch%gridcell(pi) = 1
    patch%itype(pi) = ptype

  end subroutine add_patch

end module initSubgridMod

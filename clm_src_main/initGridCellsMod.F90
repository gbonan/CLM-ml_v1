module initGridCellsMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initializes sub-grid mapping for each land grid cell
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: initGridcells
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private set_landunit_veg_compete
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine initGridcells
    !
    ! !DESCRIPTION:
    ! Initialize sub-grid mapping and allocate space for derived type hierarchy.
    ! For each land gridcell determine landunit, column and patch properties.
    !---------------------------------------------------------------------

    ! Determine naturally vegetated landunit

    call set_landunit_veg_compete

  end subroutine initGridcells

  !-----------------------------------------------------------------------
  subroutine set_landunit_veg_compete
    !
    ! !DESCRIPTION:
    ! Initialize vegetated landunit with competition
    !
    ! !USES:
    use TowerDataMod, only : tower_pft, tower_num
    use initSubgridMod, only : add_patch
    !
    ! !LOCAL VARIABLES:
    integer :: pi     ! patch index
    !---------------------------------------------------------------------

    ! The code (as used here) processes one patch (one grid cell with one
    ! column and one patch) and the subgrid patch structure is set accordingly.

    pi = 0
    call add_patch (pi, tower_pft(tower_num))

  end subroutine set_landunit_veg_compete

end module initGridCellsMod

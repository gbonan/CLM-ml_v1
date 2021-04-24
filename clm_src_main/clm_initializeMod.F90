module clm_initializeMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Performs land model initialization
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use decompMod, only : bounds_type
  use clm_varpar, only: clm_varpar_init
  use pftconMod, only : pftcon
  use GridcellType, only : grc
  use ColumnType, only : col
  use PatchType, only : patch
  use initGridCellsMod, only : initGridCells
  use filterMod, only : allocFilters, filter
  use clm_instMod, only : clm_instInit
  use MLCanopyTurbulenceMod , only : LookupPsihatINI   !!! CLMml !!!
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: initialize1  ! Phase one initialization
  public :: initialize2  ! Phase two initialization
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine initialize1 (bounds)
    !
    ! !DESCRIPTION:
    ! CLM initialization - first phase 
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    !---------------------------------------------------------------------

    ! Initialize run control variables

    call clm_varpar_init()

    ! Read list of PFTs and their parameter values

    call pftcon%Init()

    ! Initialize the look-up tables needed to calculate the CLMml
    ! roughness sublayer psihat functions

    call LookupPsihatINI

    ! Allocate memory for subgrid data structures

    call grc%Init   (bounds%begg, bounds%endg)
    call col%Init   (bounds%begc, bounds%endc)
    call patch%Init (bounds%begp, bounds%endp)

    ! Build subgrid hierarchy of landunit, column, and patch

    call initGridCells

    ! Allocate filters

    call allocFilters (filter, bounds%begp, bounds%endp, bounds%begc, bounds%endc)

  end subroutine initialize1

  !-----------------------------------------------------------------------
  subroutine initialize2 (bounds)
    !
    ! !DESCRIPTION:
    ! CLM initialization - second phase 
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    !---------------------------------------------------------------------

    ! Initialize instances of all derived types as well as
    ! time constant variables

    call clm_instInit (bounds)

  end subroutine initialize2

end module clm_initializeMod

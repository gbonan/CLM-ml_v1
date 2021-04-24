module lnd_comp_mct

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Interface of the active land model component of CESM (CLM, Community Land Model)
  ! with the main CESM driver.
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: lnd_init_mct      ! CLM initialization
  public :: lnd_run_mct       ! CLM run phase
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine lnd_init_mct (bounds)
    !
    ! !DESCRIPTION:
    ! Initialize land surface model
    !
    ! !USES:
    use clm_initializeMod, only : initialize1, initialize2
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    !---------------------------------------------------------------------

    call initialize1 (bounds)
    call initialize2 (bounds)

  end subroutine lnd_init_mct

  !-----------------------------------------------------------------------
  subroutine lnd_run_mct (bounds, time_indx, fin)
    !
    ! !DESCRIPTION:
    ! Run CLM model
    !
    ! !USES:
    use clm_driver, only : clm_drv
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    integer, intent(in) :: time_indx            ! Time index from reference date (0Z January 1 of current year, when calday = 1.000
    character(len=256) :: fin                   ! File name
    !
    ! !LOCAL VARIABLES:
    !---------------------------------------------------------------------

    call clm_drv (bounds, time_indx, fin)

  end subroutine lnd_run_mct

end module lnd_comp_mct

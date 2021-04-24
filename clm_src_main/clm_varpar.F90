module clm_varpar

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing various model parameters
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use MLclm_varctl, only : clm_phys
  !
  ! !PUBLIC TYPES:
  implicit none

  integer            :: nlevsno = -1          ! Maximum number of snow layers
  integer            :: nlevsoi = -1          ! Number of hydrologically active soil layers
  integer            :: nlevgrnd = -1         ! Number of ground layers (including hydrologically inactive)

  integer, parameter :: numrad = 2            ! Number of radiation wavebands
  integer, parameter :: ivis = 1              ! Visible waveband index
  integer, parameter :: inir = 2              ! Near-infrared waveband index
  integer, parameter :: mxpft = 78            ! Maximum number of plant functional types
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public clm_varpar_init                      ! Set parameters

contains

  !-----------------------------------------------------------------------
  subroutine clm_varpar_init()
    !
    ! !DESCRIPTION:
    ! Initialize module variables
    !
    ! !ARGUMENTS:
    implicit none
    !---------------------------------------------------------------------

    ! CLM4.5 and CLM5 have different snow/soil layers

    if (clm_phys == 'CLM5_0') then
       nlevsno  = 12
       nlevsoi  = 20
       nlevgrnd = nlevsoi + 5
    else if (clm_phys == 'CLM4_5') then
       nlevsno  = 5
       nlevsoi  = 10
       nlevgrnd = 15
    end if

  end subroutine clm_varpar_init

end module clm_varpar

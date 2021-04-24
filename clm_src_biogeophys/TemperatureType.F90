module TemperatureType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Temperature variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : nlevgrnd, nlevsno
  use clm_varcon, only : ispval, nan => spval
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: temperature_type

    real(r8), pointer :: t_soisno_col  (:,:)  ! col soil temperature (Kelvin) (-nlevsno+1:nlevgrnd)
    real(r8), pointer :: t_a10_patch   (:)    ! patch 10-day running mean of the 2 m temperature (K)
    real(r8), pointer :: t_ref2m_patch (:)    ! patch 2 m height surface air temperature (Kelvin)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type temperature_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(temperature_type) :: this
    type(bounds_type), intent(in) :: bounds

    call this%InitAllocate (bounds)

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate (this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(temperature_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    integer :: begc, endc   ! Column indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp
    begc = bounds%begc ; endc = bounds%endc

    allocate (this%t_soisno_col  (begc:endc,-nlevsno+1:nlevgrnd)) ; this%t_soisno_col  (:,:) = nan
    allocate (this%t_a10_patch   (begp:endp))                     ; this%t_a10_patch   (:)   = nan
    allocate (this%t_ref2m_patch (begp:endp))                     ; this%t_ref2m_patch (:)   = nan

  end subroutine InitAllocate

end module TemperatureType

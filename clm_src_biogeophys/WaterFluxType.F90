module WaterFluxType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Water flux variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon, only : ispval, nan => spval
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: waterflux_type

    real(r8), pointer :: qflx_evap_tot_patch (:)   ! patch qflx_evap_soi + qflx_evap_veg + qflx_tran_veg (kg H2O/m2/s)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type waterflux_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(waterflux_type) :: this
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
    class(waterflux_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp

    allocate (this%qflx_evap_tot_patch (begp:endp)) ; this%qflx_evap_tot_patch (:) = nan

  end subroutine InitAllocate

end module WaterFluxType

module EnergyFluxType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Energy flux variables
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
  type, public :: energyflux_type

    ! Fluxes
    real(r8), pointer :: eflx_sh_tot_patch    (:)   ! patch total sensible heat flux (W/m2) [+ to atm]
    real(r8), pointer :: eflx_lh_tot_patch    (:)   ! patch total latent heat flux (W/m2)  [+ to atm]
    real(r8), pointer :: eflx_lwrad_out_patch (:)   ! patch emitted infrared (longwave) radiation (W/m2)

    ! Wind Stress
    real(r8), pointer :: taux_patch           (:)   ! patch wind (shear) stress: e-w (kg/m/s**2)
    real(r8), pointer :: tauy_patch           (:)   ! patch wind (shear) stress: n-s (kg/m/s**2)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type energyflux_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(energyflux_type) :: this
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
    class(energyflux_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp

    allocate (this%eflx_sh_tot_patch    (begp:endp))  ; this%eflx_sh_tot_patch    (:) = nan
    allocate (this%eflx_lh_tot_patch    (begp:endp))  ; this%eflx_lh_tot_patch    (:) = nan
    allocate (this%eflx_lwrad_out_patch (begp:endp))  ; this%eflx_lwrad_out_patch (:) = nan
    allocate (this%taux_patch           (begp:endp))  ; this%taux_patch           (:) = nan
    allocate (this%tauy_patch           (begp:endp))  ; this%tauy_patch           (:) = nan

  end subroutine InitAllocate

end module EnergyFluxType

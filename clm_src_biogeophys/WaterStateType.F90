module WaterStateType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Water state variables
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

  type, public :: waterstate_type

    real(r8), pointer :: bw_col           (:,:)  ! col partial density of water in the snow pack (ice + liquid) [kg/m3]
    real(r8), pointer :: h2osno_col       (:)    ! col snow water (mm H2O)
    real(r8), pointer :: h2osoi_liq_col   (:,:)  ! col liquid water (kg H2O/m2) (-nlevsno+1:nlevgrnd)
    real(r8), pointer :: h2osoi_ice_col   (:,:)  ! col ice lens (kg H2O/m2) (-nlevsno+1:nlevgrnd)
    real(r8), pointer :: h2osoi_vol_col   (:,:)  ! col volumetric soil water (0<=h2osoi_vol<=watsat) [m3/m3]  (nlevgrnd)
    real(r8), pointer :: h2osfc_col       (:)    ! col surface water (mm H2O)
    real(r8), pointer :: q_ref2m_patch    (:)    ! patch 2 m height surface specific humidity (kg/kg)
    real(r8), pointer :: frac_sno_eff_col (:)    ! col fraction of ground covered by snow (0 to 1)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type waterstate_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(waterstate_type) :: this
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
    class(waterstate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    integer :: begc, endc   ! Column indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp
    begc = bounds%begc ; endc = bounds%endc

    allocate (this%bw_col           (begc:endc,-nlevsno+1:0))        ; this%bw_col           (:,:) = nan
    allocate (this%h2osno_col       (begc:endc))                     ; this%h2osno_col       (:)   = nan
    allocate (this%h2osoi_liq_col   (begc:endc,-nlevsno+1:nlevgrnd)) ; this%h2osoi_liq_col   (:,:) = nan
    allocate (this%h2osoi_ice_col   (begc:endc,-nlevsno+1:nlevgrnd)) ; this%h2osoi_ice_col   (:,:) = nan
    allocate (this%h2osoi_vol_col   (begc:endc,1:nlevgrnd))          ; this%h2osoi_vol_col   (:,:) = nan
    allocate (this%h2osfc_col       (begc:endc))                     ; this%h2osfc_col       (:)   = nan
    allocate (this%q_ref2m_patch    (begp:endp))                     ; this%q_ref2m_patch    (:)   = nan
    allocate (this%frac_sno_eff_col (begc:endc))                     ; this%frac_sno_eff_col (:)   = nan

  end subroutine InitAllocate

end module WaterStateType

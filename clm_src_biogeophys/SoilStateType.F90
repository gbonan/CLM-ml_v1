module SoilStateType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Soil state variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : nlevgrnd, nlevsno, nlevsoi
  use clm_varcon, only : ispval, nan => spval
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: soilstate_type

    ! sand/ clay/ organic matter
    real(r8), pointer :: cellorg_col   (:,:)  ! col organic matter (kg/m3)
    real(r8), pointer :: cellsand_col  (:,:)  ! col sand (percent)
    real(r8), pointer :: cellclay_col  (:,:)  ! col clay (percent)

    ! hydraulic properties
    real(r8), pointer :: hksat_col     (:,:)  ! col hydraulic conductivity at saturation (mm H2O /s)
    real(r8), pointer :: hk_l_col      (:,:)  ! col hydraulic conductivity (mm H2O/s)
    real(r8), pointer :: smp_l_col     (:,:)  ! col soil matric potential (mm)
    real(r8), pointer :: bsw_col       (:,:)  ! col Clapp and Hornberger "b"
    real(r8), pointer :: watsat_col    (:,:)  ! col volumetric soil water at saturation (porosity)
    real(r8), pointer :: sucsat_col    (:,:)  ! col minimum soil suction (mm)
    real(r8), pointer :: dsl_col       (:)    ! col dry surface layer thickness (mm)
    real(r8), pointer :: soilresis_col (:)    ! col soil evaporative resistance S&L14 (s/m)

    ! thermal conductivity / heat capacity
    real(r8), pointer :: thk_col       (:,:)  ! col thermal conductivity of each layer (W/m/K)
    real(r8), pointer :: tkmg_col      (:,:)  ! col thermal conductivity, soil minerals  (W/m/K)
    real(r8), pointer :: tkdry_col     (:,:)  ! col thermal conductivity, dry soil (W/m/K)
    real(r8), pointer :: csol_col      (:,:)  ! col heat capacity, soil solids (J/m**3/K)

    ! roots
    real(r8), pointer :: rootfr_patch  (:,:)  ! patch effective fraction of roots in each soil layer

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type soilstate_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(soilstate_type) :: this
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
    class(soilstate_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    integer :: begc, endc   ! Column indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp
    begc = bounds%begc ; endc = bounds%endc

    allocate (this%cellorg_col   (begc:endc,1:nlevsoi))           ; this%cellorg_col   (:,:) = nan
    allocate (this%cellsand_col  (begc:endc,1:nlevsoi))           ; this%cellsand_col  (:,:) = nan
    allocate (this%cellclay_col  (begc:endc,1:nlevsoi))           ; this%cellclay_col  (:,:) = nan

    allocate (this%hksat_col     (begc:endc,1:nlevgrnd))          ; this%hksat_col     (:,:) = nan
    allocate (this%hk_l_col      (begc:endc,1:nlevgrnd))          ; this%hk_l_col      (:,:) = nan
    allocate (this%smp_l_col     (begc:endc,1:nlevgrnd))          ; this%smp_l_col     (:,:) = nan
    allocate (this%bsw_col       (begc:endc,1:nlevgrnd))          ; this%bsw_col       (:,:) = nan
    allocate (this%watsat_col    (begc:endc,1:nlevgrnd))          ; this%watsat_col    (:,:) = nan
    allocate (this%sucsat_col    (begc:endc,1:nlevgrnd))          ; this%sucsat_col    (:,:) = nan
    allocate (this%dsl_col       (begc:endc))                     ; this%dsl_col       (:)   = nan
    allocate (this%soilresis_col (begc:endc))                     ; this%soilresis_col (:)   = nan

    allocate (this%thk_col       (begc:endc,-nlevsno+1:nlevgrnd)) ; this%thk_col       (:,:) = nan
    allocate (this%tkmg_col      (begc:endc,1:nlevgrnd))          ; this%tkmg_col      (:,:) = nan
    allocate (this%tkdry_col     (begc:endc,1:nlevgrnd))          ; this%tkdry_col     (:,:) = nan
    allocate (this%csol_col      (begc:endc,1:nlevgrnd))          ; this%csol_col      (:,:) = nan

    allocate (this%rootfr_patch  (begp:endp,1:nlevgrnd))          ; this%rootfr_patch  (:,:) = nan

  end subroutine InitAllocate

end module SoilStateType

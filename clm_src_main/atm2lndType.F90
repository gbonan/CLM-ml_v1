module atm2lndType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! atm -> land variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : numrad
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: atm2lnd_type

    ! atm -> land: not downscaled
    real(r8), pointer :: forc_u_grc                (:)    ! Atmospheric wind speed in east direction (m/s)
    real(r8), pointer :: forc_v_grc                (:)    ! Atmospheric wind speed in north direction (m/s)
    real(r8), pointer :: forc_pco2_grc             (:)    ! Atmospheric CO2 partial pressure (Pa)
    real(r8), pointer :: forc_po2_grc              (:)    ! Atmospheric O2 partial pressure (Pa)
    real(r8), pointer :: forc_solad_grc            (:,:)  ! Atmospheric direct beam radiation (W/m2)
    real(r8), pointer :: forc_solai_grc            (:,:)  ! Atmospheric diffuse radiation (W/m2)

    ! atm -> land: downscaled to column
    real(r8), pointer :: forc_t_downscaled_col     (:)    ! Atmospheric temperature (K)
    real(r8), pointer :: forc_q_downscaled_col     (:)    ! Atmospheric specific humidity (kg/kg)
    real(r8), pointer :: forc_pbot_downscaled_col  (:)    ! Atmospheric pressure (Pa)
    real(r8), pointer :: forc_lwrad_downscaled_col (:)    ! Atmospheric longwave radiation (W/m2)
    real(r8), pointer :: forc_rain_downscaled_col  (:)    ! Rainfall rate (mm/s)
    real(r8), pointer :: forc_snow_downscaled_col  (:)    ! Snowfall rate (mm/s)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type atm2lnd_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(atm2lnd_type) :: this
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
    class(atm2lnd_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    real(r8) :: ival  = 0.0_r8  ! Initial value
    integer  :: begg, endg      ! Grid cell indices
    integer  :: begc, endc      ! Column indices
    !---------------------------------------------------------------------

    begg = bounds%begg ; endg = bounds%endg
    begc = bounds%begc ; endc = bounds%endc

    allocate (this%forc_u_grc                (begg:endg))        ; this%forc_u_grc                (:)   = ival
    allocate (this%forc_v_grc                (begg:endg))        ; this%forc_v_grc                (:)   = ival
    allocate (this%forc_pco2_grc             (begg:endg))        ; this%forc_pco2_grc             (:)   = ival
    allocate (this%forc_po2_grc              (begg:endg))        ; this%forc_po2_grc              (:)   = ival
    allocate (this%forc_solad_grc            (begg:endg,numrad)) ; this%forc_solad_grc            (:,:) = ival
    allocate (this%forc_solai_grc            (begg:endg,numrad)) ; this%forc_solai_grc            (:,:) = ival
    allocate (this%forc_t_downscaled_col     (begc:endc))        ; this%forc_t_downscaled_col     (:)   = ival
    allocate (this%forc_q_downscaled_col     (begc:endc))        ; this%forc_q_downscaled_col     (:)   = ival
    allocate (this%forc_pbot_downscaled_col  (begc:endc))        ; this%forc_pbot_downscaled_col  (:)   = ival
    allocate (this%forc_lwrad_downscaled_col (begc:endc))        ; this%forc_lwrad_downscaled_col (:)   = ival
    allocate (this%forc_rain_downscaled_col  (begc:endc))        ; this%forc_rain_downscaled_col  (:)   = ival
    allocate (this%forc_snow_downscaled_col  (begc:endc))        ; this%forc_snow_downscaled_col  (:)   = ival

  end subroutine InitAllocate

end module atm2lndType

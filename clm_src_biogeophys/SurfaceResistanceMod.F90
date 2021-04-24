module SurfaceResistanceMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculates resistance for soil evaporation
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils, only : endrun
  use decompMod, only : bounds_type
  use SoilStateType, only : soilstate_type
  use WaterStateType, only : waterstate_type
  use TemperatureType, only : temperature_type
  use ColumnType, only : col
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: calc_soilevap_resis
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: calc_soil_resistance_sl14
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine calc_soilevap_resis (bounds, num_nolakec, filter_nolakec, &
  soilstate_inst, waterstate_inst, temperature_inst)
    !
    ! !DESCRIPTION:
    ! Resistance for soil evaporation
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in)       :: bounds             ! CLM column bounds
    integer, intent(in)                 :: num_nolakec        ! Number of non-lake points in CLM column filter
    integer, intent(in)                 :: filter_nolakec(:)  ! CLM column filter for non-lake points

    type(soilstate_type), intent(inout) :: soilstate_inst
    type(waterstate_type), intent(in)   :: waterstate_inst
    type(temperature_type), intent(in)  :: temperature_inst
    !---------------------------------------------------------------------

    associate ( &
    dsl         => soilstate_inst%dsl_col       , &  ! Soil dry surface layer thickness (mm)
    soilresis   => soilstate_inst%soilresis_col   &  ! Soil evaporative resistance (s/m)
    )

    call calc_soil_resistance_sl14 (bounds, num_nolakec, filter_nolakec, &
    soilstate_inst, waterstate_inst, temperature_inst, &
    dsl(bounds%begc:bounds%endc), soilresis(bounds%begc:bounds%endc))

    end associate

  end subroutine calc_soilevap_resis

  !-----------------------------------------------------------------------
  subroutine calc_soil_resistance_sl14 (bounds, num_nolakec, filter_nolakec, &
  soilstate_inst, waterstate_inst, temperature_inst, dsl, soilresis)
    !
    ! !DESCRIPTION:
    ! Resistance for soil evaporation
    !
    ! !USES:
    use shr_kind_mod, only : r8 => shr_kind_r8
    use clm_varcon, only : denh2o, denice
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in)      :: bounds            ! CLM column bounds
    integer, intent(in)                :: num_nolakec       ! Number of non-lake points in CLM column filter
    integer, intent(in)                :: filter_nolakec(:) ! CLM column filter for non-lake points
    real(r8), intent(out)              :: dsl(bounds%begc:bounds%endc)       ! Soil dry surface layer thickness (mm)
    real(r8), intent(out)              :: soilresis(bounds%begc:bounds%endc) ! Soil evaporative resistance (s/m)

    type(soilstate_type), intent(in)   :: soilstate_inst
    type(waterstate_type), intent(in)  :: waterstate_inst
    type(temperature_type), intent(in) :: temperature_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: f                      ! Filter index
    integer  :: c                      ! Column index for CLM g/l/c/p hierarchy
    real(r8) :: vwc_liq                ! Volumetric soil water content of first soil layer
    real(r8) :: eff_por_top            ! Effective porosity of first soil layer
    real(r8) :: aird                   ! "Air-dry" soil moisture
    real(r8) :: d0                     ! Diffusivity of water vapor (m2/s)
    real(r8) :: eps                    ! Air-filled pore space
    real(r8) :: tort                   ! Tortuosity
    !---------------------------------------------------------------------

    associate ( &
    watsat     => soilstate_inst%watsat_col      , &  ! Soil layer volumetric water content at saturation (porosity)
    sucsat     => soilstate_inst%sucsat_col      , &  ! Soil layer suction (negative matric potential) at saturation (mm)
    bsw        => soilstate_inst%bsw_col         , &  ! Soil layer Clapp and Hornberger "b" parameter
    t_soisno   => temperature_inst%t_soisno_col  , &  ! Soil temperature (K)
    h2osoi_ice => waterstate_inst%h2osoi_ice_col , &  ! Soil layer ice lens (kg H2O/m2)
    h2osoi_liq => waterstate_inst%h2osoi_liq_col , &  ! Soil layer liquid water (kg H2O/m2)
    dz         => col%dz                           &  ! Soil layer thickness (m)
    )

    do f = 1, num_nolakec
       c = filter_nolakec(f)

       vwc_liq = max( h2osoi_liq(c,1), 1.0e-6_r8 ) / (dz(c,1) * denh2o)
       eff_por_top = max( 0.01_r8, watsat(c,1) - min(watsat(c,1), h2osoi_ice(c,1)/(dz(c,1)*denice)) )
       aird = watsat(c,1) * (sucsat(c,1)/1.e7_r8)**(1./bsw(c,1))
       d0 = 2.12e-5 * (t_soisno(c,1)/273.15)**1.75
       eps = watsat(c,1) - aird
       tort = eps * eps * (eps/watsat(c,1))**(3._r8/max(3._r8,bsw(c,1)))
       dsl(c) = 15._r8 * max(0.001_r8,(0.8*eff_por_top - vwc_liq)) / max(0.001_r8,(0.8*watsat(c,1)-aird))
       dsl(c) = max(dsl(c), 0._r8)
       dsl(c) = min(dsl(c), 200._r8)
       soilresis(c) = dsl(c) / (d0 * tort * 1.e3) + 20._r8
       soilresis(c) = min(1.e6_r8, soilresis(c))

    end do

    end associate
  end subroutine calc_soil_resistance_sl14

end module SurfaceResistanceMod

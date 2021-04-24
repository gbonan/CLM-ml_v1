module clm_varcon

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing various model constants
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none

  ! Mathematical constants used in CLM

  real(r8) :: rpi = 3.141592654_r8          ! pi

  ! Physical constants used in CLM

  real(r8), parameter :: tfrz = 273.15_r8   ! Freezing point of water (K)
  real(r8) :: sb = 5.67e-08_r8              ! Stefan-Boltzmann constant (W/m2/K4)
  real(r8) :: grav = 9.80665_r8             ! Gravitational acceleration (m/s2)
  real(r8) :: vkc = 0.4_r8                  ! von Karman constant
  real(r8) :: denh2o = 1000._r8             ! Density of liquid water (kg/m3)
  real(r8) :: denice = 917._r8              ! Density of ice (kg/m3)
  real(r8) :: tkwat = 0.57_r8               ! Thermal conductivity of water (W/m/K)
  real(r8) :: tkice = 2.29_r8               ! Thermal conductivity of ice (W/m/K)
  real(r8) :: tkair = 0.023_r8              ! Thermal conductivity of air (W/m/K)
  real(r8) :: hfus = 0.3337e6_r8            ! Latent heat of fusion for water at 0 C (J/kg)
  real(r8) :: hvap = 2.5010e6_r8            ! Latent heat of evaporation (J/kg)
  real(r8) :: hsub = 2.8347e6_r8            ! Latent heat of sublimation (J/kg)
  real(r8) :: cpice = 2.11727e3_r8          ! Specific heat of ice (J/kg/K)
  real(r8) :: cpliq = 4.188e3_r8            ! Specific heat of water (J/kg/K)

  ! Constants used in CLM for bedrock

  real(r8) :: thk_bedrock = 3.0_r8          ! Thermal conductivity of saturated granitic rock (W/m/K)
  real(r8) :: csol_bedrock = 2.0e6_r8       ! Vol. heat capacity of granite/sandstone (J/m3/K)
  real(r8) :: zmin_bedrock = 0.4_r8         ! Minimum depth to bedrock (soil depth) [m]

  ! Special value flags used in CLM

  real(r8), parameter ::  spval = 1.e36_r8  ! Special value for real data
  integer , parameter :: ispval = -9999     ! Special value for integer data

end module clm_varcon

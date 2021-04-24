module SoilTexMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Parameters for soil texture classes
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  !-----------------------------------------------------------------------

  integer, parameter :: ntex = 11      ! Number of soil texture classes
  character(len=15) soil_tex(ntex)     ! Soil texture class
  real(r8) :: sand_tex(ntex)           ! Sand fraction
  real(r8) :: silt_tex(ntex)           ! Silt fraction
  real(r8) :: clay_tex(ntex)           ! Clay fraction
  real(r8) :: watsat_tex(ntex)         ! Volumetric soil water at saturation (porosity)
  real(r8) :: smpsat_tex(ntex)         ! Soil matric potential at saturation (mm)
  real(r8) :: hksat_tex(ntex)          ! Hydraulic conductivity at saturation (mm H2O/min)
  real(r8) :: bsw_tex(ntex)            ! Clapp and Hornberger "b" parameter

  ! Soil texture (Cosby et al. 1984. Water Resources Research 20:682-690)

  data soil_tex /'sand', 'loamy sand', 'sandy loam', 'silty loam', 'loam', 'sandy clay loam', &
                 'silty clay loam', 'clay loam', 'sandy clay', 'silty clay', 'clay'/

  data sand_tex /0.92_r8, 0.82_r8, 0.58_r8, 0.17_r8, 0.43_r8, 0.58_r8, 0.10_r8, 0.32_r8, 0.52_r8, 0.06_r8, 0.22_r8/
  data silt_tex /0.05_r8, 0.12_r8, 0.32_r8, 0.70_r8, 0.39_r8, 0.15_r8, 0.56_r8, 0.34_r8, 0.06_r8, 0.47_r8, 0.20_r8/
  data clay_tex /0.03_r8, 0.06_r8, 0.10_r8, 0.13_r8, 0.18_r8, 0.27_r8, 0.34_r8, 0.34_r8, 0.42_r8, 0.47_r8, 0.58_r8/

  ! Soil hydraulic parameters (Clapp and Hornberger. 1978. Water Resources Research 14:601-604)

  data watsat_tex /0.395_r8, 0.410_r8, 0.435_r8, 0.485_r8, 0.451_r8, 0.420_r8, &
                   0.477_r8, 0.476_r8, 0.426_r8, 0.492_r8, 0.482_r8/

  data smpsat_tex /-121._r8,  -90._r8, -218._r8, -786._r8, -478._r8, -299._r8, &
                   -356._r8, -630._r8, -153._r8, -490._r8, -405._r8/

  data hksat_tex /10.560_r8, 9.380_r8, 2.080_r8, 0.432_r8, 0.417_r8, 0.378_r8, &
                   0.102_r8, 0.147_r8, 0.130_r8, 0.062_r8, 0.077_r8/

  data bsw_tex /4.05_r8, 4.38_r8,  4.90_r8,  5.30_r8,  5.39_r8, 7.12_r8, &
                7.75_r8, 8.52_r8, 10.40_r8, 10.40_r8, 11.40_r8/

end module SoilTexMod

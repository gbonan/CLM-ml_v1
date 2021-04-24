module clm_varorb

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing orbital parameters
  !-----------------------------------------------------------------------

  use shr_kind_mod, only : r8 => shr_kind_r8
  implicit none

  ! Orbital information needed as input to orbit_parms

  real(r8) :: eccen   ! Orbital eccentricity factor

  ! Orbital information after processed by orbit_params

  real(r8) :: obliqr  ! Earth's obliquity in radians
  real(r8) :: lambm0  ! Mean longitude of perihelion at the vernal equinox (radians)
  real(r8) :: mvelpp  ! Earth's moving vernal equinox longitude of perihelion plus pi (radians)

end module clm_varorb

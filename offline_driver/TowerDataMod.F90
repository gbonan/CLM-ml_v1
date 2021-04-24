module TowerDataMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Parameters for flux tower sites
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  !-----------------------------------------------------------------------

  integer            :: tower_num              ! Tower site index (maps to TowerDataMod arrays) 

  integer, parameter :: ntower = 15            ! Number of tower sites
  character(len=6)   :: tower_id(ntower)       ! Tower site name
  real(r8)           :: tower_lat(ntower)      ! Latitude of tower (degrees)
  real(r8)           :: tower_lon(ntower)      ! Longitude of tower (degrees)
  integer            :: tower_pft(ntower)      ! CLM PFT for tower site
  character(len=15)  :: tower_tex(ntower)      ! Soil texture class for tower site
  real(r8)           :: tower_sand(ntower)     ! Percent sand (used if >= 0)
  real(r8)           :: tower_clay(ntower)     ! Percent clay (used if >= 0)
  real(r8)           :: tower_organic(ntower)  ! Soil organic matter (kg/m3)
  integer            :: tower_isoicol(ntower)  ! CLM soil color class for tower site
  real(r8)           :: tower_zbed(ntower)     ! Depth to bedrock for tower site (m)
  real(r8)           :: tower_ht(ntower)       ! Flux tower height (m)
  real(r8)           :: tower_canht(ntower)    ! Canopy height for tower site (m)
  integer            :: tower_time(ntower)     ! Time step of forcing data for tower (minutes)

  ! Tower site name

  data tower_id / 'US-Ha1', 'US-Ho1', 'US-MMS', 'US-UMB', 'US-Dk3', 'US-Me2', &
                  'US-Var', 'US-IB1', 'US-Ne3', 'US-ARM', 'US-Bo1', 'US-Dk1', &
                  'US-Dk2', 'CHATS7', 'UMBSmw'/

  ! Latitude and longitude of tower (degrees)

  data tower_lat / 42.54_r8, 45.20_r8, 39.32_r8, 45.56_r8, 35.98_r8, 44.45_r8, &
                   38.41_r8, 41.86_r8, 41.18_r8, 36.61_r8, 40.01_r8, 35.97_r8, &
                   35.97_r8, 38.49_r8, 45.56_r8/

  data tower_lon /  -72.17_r8,  -68.74_r8,  -86.41_r8,  -84.71_r8,  -79.09_r8, -121.56_r8, &
                   -120.95_r8,  -88.22_r8,  -96.44_r8,  -97.49_r8,  -88.29_r8,  -79.09_r8, &
                    -79.10_r8, -121.84_r8,  -84.71_r8/

  ! CLM PFT for tower site

  data tower_pft / 7, 2, 7, 7, 1, 2, 13, 15, 15, 15, 15, 13, 7, 7, 7/

  ! Use tower site sand and clay (if they are specified) to calculate
  ! hydraulic and thermal properties (as in CLM5). Otherwise, obtain
  ! them from the specified soil texture.

  data tower_tex / 'loam', 'sandy loam', 'clay', 'sand', 'sandy loam', 'sandy loam', &
                   'silty loam', 'silty clay loam', 'clay loam', 'clay', &
                   'silty loam', 'sandy loam', 'sandy loam', &
                   'silty clay loam', 'sand'/

  data tower_sand / -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, &
                    -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, &
                    -1._r8, 10._r8, -1._r8/

  data tower_clay / -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, &
                    -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, -1._r8, &
                    -1._r8, 35._r8, -1._r8/

  ! Soil organic matter (kg/m3)

  data tower_organic /  0._r8,  0._r8,  0._r8,  0._r8,  0._r8,  0._r8, &
                        0._r8,  0._r8,  0._r8,  0._r8,  0._r8,  0._r8, &
                        0._r8, 50._r8,  0._r8/

  ! CLM soil color class for tower site

  data tower_isoicol / 18, 16, 15, 17, 15, 20, 17, 15, 13, 13, 15, 15, 15, 1, 17/

  ! Depth to bedrock for tower site (m)

  data tower_zbed / 50._r8, 50._r8, 50._r8, 50._r8, 50._r8, 50._r8, 50._r8, &
                    50._r8, 50._r8, 50._r8, 50._r8, 50._r8, 50._r8,  2._r8, 50._r8/

  ! Flux tower height (m)

  data tower_ht    / 30._r8, 29._r8, 48._r8,   46._r8, 22._r8, 32._r8, &
                     2.5_r8,  4._r8,  6._r8, -999._r8,  6._r8,  5._r8, &
                     42._r8, 23._r8, 46._r8/

  ! Canopy height for tower site (m)

  data tower_canht / 23._r8, 20._r8, 27._r8, 21._r8, 17._r8, 14._r8, &
                     0.6_r8, 0.9_r8, 0.9_r8, 0.5_r8, 0.9_r8, 0.5_r8, &
                     25._r8, 10._r8, 21._r8/

  ! Time step of forcing data for tower (minutes)

  data tower_time / 60, 30, 60, 60, 30, 30, 30, 30, 60, 30, 30, 30, 30, 30, 60/

end module TowerDataMod

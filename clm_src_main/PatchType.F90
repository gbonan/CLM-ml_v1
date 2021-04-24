module PatchType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Patch data type
  ! -------------------------------------------------------- 
  ! patch types can have values of
  ! -------------------------------------------------------- 
  !    0 => not_vegetated
  !    1 => needleleaf_evergreen_temperate_tree
  !    2 => needleleaf_evergreen_boreal_tree
  !    3 => needleleaf_deciduous_boreal_tree
  !    4 => broadleaf_evergreen_tropical_tree
  !    5 => broadleaf_evergreen_temperate_tree
  !    6 => broadleaf_deciduous_tropical_tree
  !    7 => broadleaf_deciduous_temperate_tree
  !    8 => broadleaf_deciduous_boreal_tree
  !    9 => broadleaf_evergreen_shrub
  !   10 => broadleaf_deciduous_temperate_shrub
  !   11 => broadleaf_deciduous_boreal_shrub
  !   12 => c3_arctic_grass
  !   13 => c3_non-arctic_grass
  !   14 => c4_grass
  !   15 => c3_crop
  !   16 => c3_irrigated
  !   17 => temperate_corn
  !   18 => irrigated_temperate_corn
  !   19 => spring_wheat
  !   20 => irrigated_spring_wheat
  !   21 => winter_wheat
  !   22 => irrigated_winter_wheat
  !   23 => temperate_soybean
  !   24 => irrigated_temperate_soybean
  !   25 => barley
  !   26 => irrigated_barley
  !   27 => winter_barley
  !   28 => irrigated_winter_barley
  !   29 => rye
  !   30 => irrigated_rye
  !   31 => winter_rye
  !   32 => irrigated_winter_rye
  !   33 => cassava
  !   34 => irrigated_cassava
  !   35 => citrus
  !   36 => irrigated_citrus
  !   37 => cocoa
  !   38 => irrigated_cocoa
  !   39 => coffee
  !   40 => irrigated_coffee
  !   41 => cotton
  !   42 => irrigated_cotton
  !   43 => datepalm
  !   44 => irrigated_datepalm
  !   45 => foddergrass
  !   46 => irrigated_foddergrass
  !   47 => grapes
  !   48 => irrigated_grapes
  !   49 => groundnuts
  !   50 => irrigated_groundnuts
  !   51 => millet
  !   52 => irrigated_millet
  !   53 => oilpalm
  !   54 => irrigated_oilpalm
  !   55 => potatoes
  !   56 => irrigated_potatoes
  !   57 => pulses
  !   58 => irrigated_pulses
  !   59 => rapeseed
  !   60 => irrigated_rapeseed
  !   61 => rice
  !   62 => irrigated_rice
  !   63 => sorghum
  !   64 => irrigated_sorghum
  !   65 => sugarbeet
  !   66 => irrigated_sugarbeet
  !   67 => sugarcane
  !   68 => irrigated_sugarcane
  !   69 => sunflower
  !   70 => irrigated_sunflower
  !   71 => miscanthus
  !   72 => irrigated_miscanthus
  !   73 => switchgrass
  !   74 => irrigated_switchgrass
  !   75 => tropical_corn
  !   76 => irrigated_tropical_corn
  !   77 => tropical_soybean
  !   78 => irrigated_tropical_soybean
  ! --------------------------------------------------------
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon, only : ispval, nan => spval
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:
  type, public :: patch_type

    integer,  pointer :: column   (:)    ! Column of corresponding patch for CLM g/l/c/p hierarchy
    integer,  pointer :: gridcell (:)    ! Gridcell of corresponding patch for CLM g/l/c/p hierarchy
    integer , pointer :: itype    (:)    ! Vegetation type

  contains

    procedure, public  :: Init

  end type patch_type
  type(patch_type), public, target :: patch
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, begp, endp)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(patch_type) :: this
    integer, intent(in) :: begp, endp    ! Patch indices
    !---------------------------------------------------------------------

    allocate (this%column   (begp:endp)) ; this%column   (:) = ispval
    allocate (this%gridcell (begp:endp)) ; this%gridcell (:) = ispval
    allocate (this%itype    (begp:endp)) ; this%itype    (:) = ispval

  end subroutine Init

end module PatchType

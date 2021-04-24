module SurfaceAlbedoMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculates surface albedo
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use decompMod, only : bounds_type
  use WaterStateType, only : waterstate_type
  use SurfaceAlbedoType, only : surfalb_type
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: SoilAlbedo
  public :: SurfaceAlbedoInitTimeConst
  !
  ! !PRIVATE DATA FUNCTIONS:
  real(r8), allocatable, private :: albsat(:,:) ! Wet soil albedo by color class and waveband (1=vis,2=nir)
  real(r8), allocatable, private :: albdry(:,:) ! Dry soil albedo by color class and waveband (1=vis,2=nir)
  integer , allocatable, public  :: isoicol(:)  ! Column soil color class
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine SurfaceAlbedoInitTimeConst (bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module time constant variables
    !
    ! !USES:
    use clm_varpar, only : numrad, ivis, inir
    use abortutils, only : endrun
    use TowerDataMod, only : tower_isoicol, tower_num
    !
    ! !ARGUMENTS:
    type(bounds_type), intent(in) :: bounds   ! CLM column bounds
    !
    ! !LOCAL VARIABLES:
    integer :: mxsoil_color                   ! Maximum number of soil color classes
    integer :: ier                            ! Error status
    integer :: c, begc, endc                  ! Column indices
    !---------------------------------------------------------------------

    ! Allocate module variable for soil color and assign value

    begc = bounds%begc ; endc = bounds%endc
    allocate (isoicol(begc:endc))
    do c = begc, endc
       isoicol(c) = tower_isoicol(tower_num)
    end do

    ! Set saturated and dry soil albedos for mxsoil_color color classes
    ! and numrad wavebands (1=vis, 2=nir)

    mxsoil_color = 20
    allocate (albsat(mxsoil_color,numrad), albdry(mxsoil_color,numrad), stat=ier)
    if (ier /= 0) then
       call endrun (msg=' ERROR: SurfaceAlbedoInitTimeConst: allocation error for albsat, albdry')
    end if

    if (mxsoil_color == 8) then
       albsat(1:8,ivis) = (/0.12_r8,0.11_r8,0.10_r8,0.09_r8,0.08_r8,0.07_r8,0.06_r8,0.05_r8/)
       albsat(1:8,inir) = (/0.24_r8,0.22_r8,0.20_r8,0.18_r8,0.16_r8,0.14_r8,0.12_r8,0.10_r8/)
       albdry(1:8,ivis) = (/0.24_r8,0.22_r8,0.20_r8,0.18_r8,0.16_r8,0.14_r8,0.12_r8,0.10_r8/)
       albdry(1:8,inir) = (/0.48_r8,0.44_r8,0.40_r8,0.36_r8,0.32_r8,0.28_r8,0.24_r8,0.20_r8/)
    else if (mxsoil_color == 20) then
       albsat(1:20,ivis) = (/0.25_r8,0.23_r8,0.21_r8,0.20_r8,0.19_r8,0.18_r8,0.17_r8,0.16_r8,0.15_r8,0.14_r8,&
                             0.13_r8,0.12_r8,0.11_r8,0.10_r8,0.09_r8,0.08_r8,0.07_r8,0.06_r8,0.05_r8,0.04_r8/)
       albsat(1:20,inir) = (/0.50_r8,0.46_r8,0.42_r8,0.40_r8,0.38_r8,0.36_r8,0.34_r8,0.32_r8,0.30_r8,0.28_r8,&
                             0.26_r8,0.24_r8,0.22_r8,0.20_r8,0.18_r8,0.16_r8,0.14_r8,0.12_r8,0.10_r8,0.08_r8/)
       albdry(1:20,ivis) = (/0.36_r8,0.34_r8,0.32_r8,0.31_r8,0.30_r8,0.29_r8,0.28_r8,0.27_r8,0.26_r8,0.25_r8,&
                             0.24_r8,0.23_r8,0.22_r8,0.20_r8,0.18_r8,0.16_r8,0.14_r8,0.12_r8,0.10_r8,0.08_r8/)
       albdry(1:20,inir) = (/0.61_r8,0.57_r8,0.53_r8,0.51_r8,0.49_r8,0.48_r8,0.45_r8,0.43_r8,0.41_r8,0.39_r8,&
                             0.37_r8,0.35_r8,0.33_r8,0.31_r8,0.29_r8,0.27_r8,0.25_r8,0.23_r8,0.21_r8,0.16_r8/)
    else
       call endrun (msg=' ERROR: SurfaceAlbedoInitTimeConst: maximum color class is not supported')
    end if

  end subroutine SurfaceAlbedoInitTimeConst

  !-----------------------------------------------------------------------
  subroutine SoilAlbedo (bounds, num_nourbanc, filter_nourbanc, waterstate_inst, surfalb_inst)
    !
    ! !DESCRIPTION:
    ! Ground surface (soil) albedo
    !
    ! !USES:
    use clm_varpar, only : numrad, ivis, inir
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in)     :: bounds             ! CLM column bounds
    integer, intent(in)               :: num_nourbanc       ! Number of non-urban points in CLM column filter
    integer, intent(in)               :: filter_nourbanc(:) ! CLM column filter for non-urban points

    type(waterstate_type), intent(in) :: waterstate_inst
    type(surfalb_type), intent(out)   :: surfalb_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: f                     ! Filter index
    integer  :: c                     ! Column index for CLM g/l/c/p hierarchy
    integer  :: ib                    ! Waveband index
    integer  :: soilcol               ! Soil color
    real(r8) :: inc                   ! Soil water correction factor for soil albedo
    !---------------------------------------------------------------------

    associate ( &
                                                       ! *** Input ***
    h2osoi_vol => waterstate_inst%h2osoi_vol_col  , &  ! Soil layer volumetric water content (m3/m3)
                                                       ! *** Output ***
    albsoib    => surfalb_inst%albgrd_col         , &  ! Direct beam albedo of ground (soil)
    albsoid    => surfalb_inst%albgri_col           &  ! Diffuse albedo of ground (soil)
    )

    ! Calculate soil albedo

    do ib = 1, numrad
       do f = 1, num_nourbanc
          c = filter_nourbanc(f)

          soilcol = isoicol(c)
          inc = max((0.11_r8 - 0.40_r8 * h2osoi_vol(c,1)), 0._r8)
          albsoib(c,ib) = min(albsat(soilcol,ib)+inc, albdry(soilcol,ib))
          albsoid(c,ib) = albsoib(c,ib)

       end do
    end do

    end associate
  end subroutine SoilAlbedo

end module SurfaceAlbedoMod

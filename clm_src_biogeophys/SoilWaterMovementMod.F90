module SoilWaterMovementMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Contains different subroutines to couple soil and root water interactions
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: SoilWater
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: soilwater_moisture_form
  private :: compute_hydraulic_properties
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine SoilWater (bounds, num_hydrologyc, filter_hydrologyc, &
  soilstate_inst, waterstate_inst)
    !
    ! !DESCRIPTION:
    !
    ! !USES:
    use SoilStateType, only : soilstate_type
    use WaterStateType, only : waterstate_type
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in)      :: bounds               ! CLM column bounds
    integer, intent(in)                :: num_hydrologyc       ! Number of columns in CLM hydrology filter
    integer, intent(in)                :: filter_hydrologyc(:) ! CLM column filter for hydrology

    type(soilstate_type), intent(inout):: soilstate_inst
    type(waterstate_type), intent(in)  :: waterstate_inst
    !---------------------------------------------------------------------

    call soilwater_moisture_form (bounds, num_hydrologyc, filter_hydrologyc, &
    soilstate_inst, waterstate_inst)

  end subroutine SoilWater

  !-----------------------------------------------------------------------
  subroutine soilwater_moisture_form (bounds, num_hydrologyc, filter_hydrologyc, &
  soilstate_inst, waterstate_inst)
    !
    ! !DESCRIPTION:
    ! Soil hydrology
    !
    ! !USES:
    use clm_varpar, only : nlevsoi
    use clm_varcon, only : denh2o
    use ColumnType, only : col
    use SoilStateType, only : soilstate_type
    use WaterStateType, only : waterstate_type
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in)      :: bounds               ! CLM column bounds
    integer, intent(in)                :: num_hydrologyc       ! Number of columns in CLM hydrology filter
    integer, intent(in)                :: filter_hydrologyc(:) ! CLM column filter for hydrology

    type(soilstate_type), intent(inout):: soilstate_inst
    type(waterstate_type), intent(in)  :: waterstate_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: fc                                           ! Filter index
    integer  :: c                                            ! Column index for CLM g/l/c/p hierarchy
    integer  :: j                                            ! Soil layer index
    integer  :: nlayers                                      ! Number of layers
    real(r8) :: hk(bounds%begc:bounds%endc,1:nlevsoi)        ! Soil layer hydraulic conductivity (mm H2O/s)
    real(r8) :: smp(bounds%begc:bounds%endc,1:nlevsoi)       ! Soil layer matric potential (mm)
    real(r8) :: vwc_liq(bounds%begc:bounds%endc,1:nlevsoi)   ! Liquid volumetric water content
    !---------------------------------------------------------------------

    associate ( &
    nbedrock   => col%nbedrock                   , &  ! Depth to bedrock index
    dz         => col%dz                         , &  ! Soil layer thickness (m)
    h2osoi_liq => waterstate_inst%h2osoi_liq_col   &  ! Soil layer liquid water (kg H2O/m2)
    )

    do fc = 1, num_hydrologyc
       c = filter_hydrologyc(fc)

       ! Set number of layers over which to solve soilwater movement

       nlayers = nbedrock(c)

       ! Liquid volumetric water content

       do j = 1, nlayers
          vwc_liq(c,j) = max(h2osoi_liq(c,j),1.0e-6_r8)/(dz(c,j)*denh2o)
       end do

       ! Hydraulic conductivity and soil matric potential

       call compute_hydraulic_properties(c, nlayers, soilstate_inst, &
       vwc_liq(c,1:nlayers), hk(c,1:nlayers), smp(c,1:nlayers))

    end do

    end associate
  end subroutine soilwater_moisture_form

  !-----------------------------------------------------------------------
  subroutine compute_hydraulic_properties (c, nlayers, soilstate_inst, &
  vwc_liq, hk, smp)
    !
    ! !DESCRIPTION:
    ! Hydraulic conductivity and soil matric potential
    !
    ! !USES:
    use SoilStateType, only : soilstate_type
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: c                    ! Column index
    integer, intent(in) :: nlayers              ! Number of layers
    real(r8), intent(in)  :: vwc_liq(1:nlayers) ! Soil layer liquid volumetric water content (m3 H2O/m3)
    real(r8), intent(out) :: hk(1:nlayers)      ! Soil layer hydraulic conductivity (mm H2O/s)
    real(r8), intent(out) :: smp(1:nlayers)     ! Soil layer matric potential (mm)

    type(soilstate_type), intent(inout) :: soilstate_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: j                               ! Soil layer index
    real(r8) :: s                               ! Soil layer water content relative to saturation (fraction)
    !---------------------------------------------------------------------

    associate ( &
                                                      ! *** Input ***
    watsat     => soilstate_inst%watsat_col      , &  ! Soil layer volumetric water content at saturation (porosity)
    hksat      => soilstate_inst%hksat_col       , &  ! Soil layer hydraulic conductivity at saturation (mm H2O/s)
    sucsat     => soilstate_inst%sucsat_col      , &  ! Soil layer suction (negative matric potential) at saturation (mm)
    bsw        => soilstate_inst%bsw_col         , &  ! Soil layer Clapp and Hornberger "b" parameter
                                                      ! *** Output ***
    hk_l       => soilstate_inst%hk_l_col        , &  ! Soil layer hydraulic conductivity (mm H2O/s)
    smp_l      => soilstate_inst%smp_l_col         &  ! Soil layer matric potential (mm)
    )

    ! Hydraulic conductivity and matric potential for each layer
    ! using Clapp-Hornberger 1978. CLM5 allows for alternative
    ! soil water relationships. Here, the code is hardwired.

    do j = 1, nlayers
       s = vwc_liq(j) / watsat(c,j)
       s = min(s, 1._r8)
       s = max(0.01_r8, s)
       hk(j) = hksat(c,j) * s**(2._r8 * bsw(c,j) + 3._r8)
       smp(j) = -sucsat(c,j) * s**(-bsw(c,j))
       smp(j) = max(smp(j), -1.e08_r8)

       hk_l(c,j) = hk(j)
       smp_l(c,j) = smp(j)
    end do

    end associate
  end subroutine compute_hydraulic_properties

end module SoilWaterMovementMod

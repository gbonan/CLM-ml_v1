module SoilStateInitTimeConstMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Set hydraulic and thermal properties
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils, only : endrun
  use decompMod, only : bounds_type
  use PatchType, only : patch
  use ColumnType, only : col
  use pftconMod, only : pftcon
  use SoilStateType, only : soilstate_type
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: SoilStateInitTimeConst
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine SoilStateInitTimeConst (bounds, soilstate_inst)
    !
    ! !DESCRIPTION:
    ! Initialize module time constant variables
    !
    ! !USES:
    use clm_varcon, only : csol_bedrock
    use clm_varpar, only : nlevsoi, nlevgrnd
    use clm_varctl, only : iulog
    use MLclm_varctl, only : clm_phys, root_type
    use TowerDataMod, only : tower_num, tower_tex, tower_clay, tower_sand, tower_organic
    use SoilTexMod
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    type(soilstate_type), intent(out) :: soilstate_inst
    !
    ! !LOCAL VARIABLES:
    integer :: p                                       ! Patch index for CLM g/l/c/p hierarchy
    integer :: c                                       ! Column index for CLM g/l/c/p hierarchy
    integer :: j                                       ! Soil layer index
    integer :: tex                                     ! Soil texture class
    integer :: m                                       ! Soil texture loop index

    real(r8) :: clay                                   ! Percent clay
    real(r8) :: sand                                   ! Percent sand
    real(r8) :: om_frac                                ! Organic matter fraction
    real(r8) :: om_frac_therm                          ! Organic matter fraction (old simulations)
    real(r8) :: perc_frac                              ! Percolating fraction of organic soil
    real(r8) :: perc_norm                              ! Normalize to 1 when 100% organic soil
    real(r8) :: uncon_hksat                            ! Series conductivity of mineral/organic soil
    real(r8) :: uncon_frac                             ! Fraction of unconnected soil
    real(r8) :: bulk_dens_min                          ! Bulk density, mineral soil (kg/m3)
    real(r8) :: tkdry_min                              ! Dry thermal conductivity, mineral fraction (W/m/K)
    real(r8) :: quartz                                 ! Quartz fraction of soil (fraction)
    real(r8) :: tksol_quartz                           ! Thermal conductivity, quartz (W/m/K)
    real(r8) :: tksol_other                            ! Thermal conductivity, other minerals (W/m/K)
    real(r8) :: tksol_min                              ! Thermal conductivity, minerals (W/m/K)
    real(r8) :: tkm                                    ! Mineral conductivity (W/m/K)
    real(r8) :: cvsol                                  ! Heat capacity of soil mineral solids (J/m3/K)
    real(r8) :: om_watsat                              ! Porosity of organic soil
    real(r8) :: om_sucsat                              ! Saturated suction for organic soil (mm)
    real(r8) :: om_hksat                               ! Saturated hydraulic conductivity for organic soil (mm/s)
    real(r8) :: om_b                                   ! Clapp Hornberger parameter for organic soil
    real(r8) :: om_cvsol                               ! Heat capacity for organic material (J/m3/K)
    real(r8) :: om_tkdry                               ! Thermal conductivity, dry organic material (W/m/K)
    real(r8) :: om_tksol                               ! Thermal conductivity, organic material (W/m/K)

    real(r8), parameter :: organic_max = 130._r8       ! Organic matter content where soil acts like peat (kg/m3)
    real(r8), parameter :: zsapric = 0.5_r8            ! Depth (m) that organic matter takes on characteristics of sapric peat
    real(r8), parameter :: pcalpha = 0.5_r8            ! Percolation threshold
    real(r8), parameter :: pcbeta = 0.139_r8           ! Percolation exponent

    real(r8) :: beta                                   ! Root profile parameter
    real(r8), parameter :: m_to_cm = 1.e2_r8           ! Conversion for m to cm
    !---------------------------------------------------------------------

    associate ( &
                                                       ! *** Input ***
    roota_par     => pftcon%roota_par            , &   ! Zeng2001 rooting distribution parameter (1/m)
    rootb_par     => pftcon%rootb_par            , &   ! Zeng2001 rooting distribution parameter (1/m)
    rootprof_beta => pftcon%rootprof_beta        , &   ! Jackson1996 rooting distribution parameter (-)
    z             => col%z                       , &   ! Soil layer depth (m)
    zi            => col%zi                      , &   ! Soil layer depth at layer interface (m)
    nbedrock      => col%nbedrock                , &   ! Depth to bedrock index
                                                       ! *** Output ***
    rootfr        => soilstate_inst%rootfr_patch , &   ! Fraction of roots in each soil layer
    cellsand      => soilstate_inst%cellsand_col , &   ! Soil layer percent sand
    cellclay      => soilstate_inst%cellclay_col , &   ! Soil layer percent clay
    cellorg       => soilstate_inst%cellorg_col  , &   ! Soil layer organic matter (kg/m3)
    watsat        => soilstate_inst%watsat_col   , &   ! Soil layer volumetric water content at saturation (porosity)
    sucsat        => soilstate_inst%sucsat_col   , &   ! Soil layer suction (negative matric potential) at saturation (mm)
    hksat         => soilstate_inst%hksat_col    , &   ! Soil layer hydraulic conductivity at saturation (mm H2O/s)
    bsw           => soilstate_inst%bsw_col      , &   ! Soil layer Clapp and Hornberger "b" parameter
    tkmg          => soilstate_inst%tkmg_col     , &   ! Soil layer thermal conductivity, soil minerals  (W/m/K)
    tkdry         => soilstate_inst%tkdry_col    , &   ! Soil layer thermal conductivity, dry soil (W/m/K)
    csol          => soilstate_inst%csol_col       &   ! Soil layer heat capacity, soil solids (J/m3/K)
    )

    ! Initialize CLM root fraction

    do p = bounds%begp, bounds%endp
       c = patch%column(p)

       ! Soil layers

       select case (root_type)
       case (1)

          ! Compute root profile for soil water uptake using Zeng2001 method

          do j = 1, nlevsoi-1
             rootfr(p,j) = 0.5_r8 * &
             (exp(-roota_par(patch%itype(p))*zi(c,j-1)) + exp(-rootb_par(patch%itype(p))*zi(c,j-1)) - &
              exp(-roota_par(patch%itype(p))*zi(c,j  )) - exp(-rootb_par(patch%itype(p))*zi(c,j  )))
          end do

          j = nlevsoi
          rootfr(p,j) = 0.5_r8 * (exp(-roota_par(patch%itype(p))*zi(c,j-1)) + &
          exp(-rootb_par(patch%itype(p))*zi(c,j-1)))

       case (2)

          ! Compute root profile for soil water uptake using Jackson1996 method

          beta = rootprof_beta(patch%itype(p))
          do j = 1, nlevsoi
             rootfr(p,j) = ( beta ** (zi(c,j-1)*m_to_cm) - beta ** (zi(c,j)*m_to_cm) )
          end do

       case default

          call endrun (msg=' ERROR: SoilStateInitTimeConst: root_type not valid')

       end select

       ! Bedrock layers have no roots

       do j = nlevsoi+1, nlevgrnd
          rootfr(p,j) = 0._r8
       end do

       ! Adjust roots for depth of soil

       do j = 1, nbedrock(c)
          rootfr(p,j) = rootfr(p,j) + sum(rootfr(p,nbedrock(c)+1:nlevsoi)) / real(nbedrock(c))
       end do
       rootfr(p,nbedrock(c)+1:nlevsoi) = 0._r8

    end do

    ! Set soil hydraulic and thermal properties. This is simplified from CLM5
    ! and is adapted to the offline multilayer code. Here, properties are
    ! invariant with depth.

    do c = bounds%begc, bounds%endc

       ! Organic matter fraction

       om_frac = tower_organic(tower_num) / organic_max

       ! Use tower site clay and sand (if they are specified) to calculate
       ! hydraulic and thermal properties. Otherwise, obtain them from the
       ! specified soil texture.

       if (tower_clay(tower_num) >= 0._r8 .and. tower_sand(tower_num) >= 0._r8) then

          tex = 0
          clay = tower_clay(tower_num)
          sand = tower_sand(tower_num)

       else

          ! Use soil texture type for the tower site

          tex = 0
          do m = 1, ntex
             if (tower_tex(tower_num) == soil_tex(m)) then
                tex = m
                exit
             else
                cycle
             end if
          end do
          if (tex == 0) then
             write (iulog,*) ' ERROR: SoilStateInitTimeConst: soil type = ',tower_tex(tower_num), ' not found for c = ',c
             call endrun()
          end if

          clay = clay_tex(tex) * 100._r8   ! fraction -> percent
          sand = sand_tex(tex) * 100._r8   ! fraction -> percent

       end if

       do j = 1, nlevgrnd

          ! Set organic matter fraction to zero for deep soil (so that there is
          ! only a surface layer of organic material)

          if (z(c,j) > 0.5_r8) om_frac = 0._r8

          ! sand/ clay/ organic matter: only for soil layers

          if (j <= nlevsoi) then
             cellsand(c,j) = sand
             cellclay(c,j) = clay
             cellorg(c,j)  = om_frac * organic_max
          end if

          ! Hydraulic properties for mineral soil. These are calculated
          ! using (i) sand and clay as in CLM5 or (ii) soil texture classes
          ! as in Clapp and Hornberger (1978) Water Resources Research
          ! 14:601-604.

          if (tex == 0) then
             watsat(c,j) = 0.489_r8 - 0.00126_r8 * sand
             sucsat(c,j) = 10._r8 * ( 10._r8**(1.88_r8-0.0131_r8*sand) )
             hksat(c,j)  = 0.0070556_r8 * ( 10._r8**(-0.884_r8+0.0153_r8*sand) )
             bsw(c,j) = 2.91_r8 + 0.159_r8 * clay
          else
             watsat(c,j) = watsat_tex(tex)
             sucsat(c,j) = -smpsat_tex(tex)
             hksat(c,j) = hksat_tex(tex) / 60._r8    !  mm/min -> mm/s
             bsw(c,j) = bsw_tex(tex)
          end if

          ! Adjust hydraulic properties for organic matter

          om_watsat = max(0.93_r8 - 0.1_r8   *(z(c,j)/zsapric), 0.83_r8)
          om_sucsat = min(10.3_r8 - 0.2_r8   *(z(c,j)/zsapric), 10.1_r8)
          om_hksat  = max(0.28_r8 - 0.2799_r8*(z(c,j)/zsapric), hksat(c,j))
          om_b      = min(2.7_r8  + 9.3_r8   *(z(c,j)/zsapric), 12.0_r8)

          watsat(c,j) = (1._r8 - om_frac) * watsat(c,j) + om_watsat * om_frac
          sucsat(c,j) = (1._r8 - om_frac) * sucsat(c,j) + om_sucsat * om_frac
          bsw(c,j) = (1._r8 - om_frac) * bsw(c,j) + om_frac * om_b

          ! perc_frac is zero unless perf_frac greater than percolation threshold

          if (om_frac > pcalpha) then
             perc_norm = (1._r8 - pcalpha)**(-pcbeta)
             perc_frac = perc_norm * (om_frac - pcalpha)**pcbeta
          else
             perc_frac = 0._r8
          end if

          ! uncon_frac is fraction of mineral soil plus fraction of
          ! non-percolating organic soil

          uncon_frac = (1._r8 - om_frac) + (1._r8 - perc_frac) * om_frac

          ! uncon_hksat is series addition of mineral/organic conductivites

          if (om_frac < 1._r8) then
             uncon_hksat = uncon_frac / &
             ((1._r8-om_frac)/hksat(c,j) + ((1._r8-perc_frac)*om_frac)/om_hksat)
          else
             uncon_hksat = 0._r8
          end if
          hksat(c,j)  = uncon_frac * uncon_hksat + (perc_frac*om_frac)*om_hksat

          ! Older simulations used om_frac = 0.02 for soil thermal properties

          if (clm_phys /= 'CLM5_0') then
             om_frac_therm = 0.02_r8
          else
             om_frac_therm = om_frac
          end if

          ! Thermal conductivity, dry soil (W/m/K)

          om_tkdry = 0.05_r8
          if (j <= nlevsoi) then
             bulk_dens_min = 2700._r8 * (1._r8 - watsat(c,j))
             tkdry_min = (0.135_r8*bulk_dens_min + 64.7_r8) / (2700._r8 - 0.947_r8*bulk_dens_min)
             tkdry(c,j) = (1._r8 - om_frac_therm) * tkdry_min + om_frac_therm * om_tkdry
          else
             bulk_dens_min = 2700._r8
             tkdry_min = (0.135_r8*bulk_dens_min + 64.7_r8) / (2700._r8 - 0.947_r8*bulk_dens_min)
             tkdry(c,j) = tkdry_min
          end if

          ! Soil solids thermal conductivity (W/m/K)

          om_tksol = 0.25_r8
          if (j <= nlevsoi) then
             if (clm_phys == 'CLM5_0') then
                tksol_min = (8.80_r8 * sand + 2.92_r8 * clay) / (sand + clay)
             else
                quartz = cellsand(c,j) / 100._r8
                if (quartz > 0.2_r8) then
                   tksol_other = 2._r8
                else
                   tksol_other = 3._r8
                end if
                tksol_quartz = 7.7_r8
                tksol_min = tksol_quartz**quartz * tksol_other**(1._r8 - quartz)
             end if
             tkm = (1._r8 - om_frac_therm) * tksol_min + om_frac_therm * om_tksol
             tkmg(c,j) = tkm ** (1._r8 - watsat(c,j))
          else
             tkmg(c,j) = 3._r8
          end if

          ! Heat capacity, soil solids (J/m3/K)

          if (tex == 0) then
             cvsol = ((2.128_r8 * sand + 2.385_r8 * clay) / (sand + clay)) * 1.e6_r8
          else
             cvsol = 1.926e06_r8
          end if
          om_cvsol = 2.5e06_r8

          if (j <= nlevsoi) then
             csol(c,j) = (1._r8 - om_frac_therm) * cvsol + om_frac_therm * om_cvsol
          else
             csol(c,j) = csol_bedrock
          end if

       end do
    end do

    end associate
  end subroutine SoilStateInitTimeConst

end module SoilStateInitTimeConstMod

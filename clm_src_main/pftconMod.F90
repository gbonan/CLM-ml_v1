module pftconMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing vegetation constants and method to
  ! read and initialize vegetation (PFT) constants
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : mxpft, numrad, ivis, inir
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! PFT constants
  !
  type, public :: pftcon_type

    ! CLM pft parameters
    real(r8), allocatable :: dleaf            (:)   ! Characteristic leaf dimension (m)
    real(r8), allocatable :: c3psn            (:)   ! Photosynthetic pathway: 0. = C4, 1. = C3
    real(r8), allocatable :: xl               (:)   ! Leaf/stem orientation index
    real(r8), allocatable :: rhol             (:,:) ! Leaf reflectance: 1=vis, 2=nir
    real(r8), allocatable :: rhos             (:,:) ! Stem reflectance: 1=vis, 2=nir
    real(r8), allocatable :: taul             (:,:) ! Leaf transmittance: 1=vis, 2=nir
    real(r8), allocatable :: taus             (:,:) ! Stem transmittance: 1=vis, 2=nir
    real(r8), allocatable :: roota_par        (:)   ! Zeng2001 rooting distribution parameter (1/m)
    real(r8), allocatable :: rootb_par        (:)   ! Zeng2001 rooting distribution parameter (1/m)
    real(r8), allocatable :: rootprof_beta    (:)   ! Jackson1996 rooting distribution parameter (-)
    real(r8), allocatable :: slatop           (:)   ! Specific leaf area at top of canopy (m2/gC)

    ! pft parameters for CLMml
    real(r8), allocatable :: vcmaxpft         (:)   ! Maximum carboxylation rate at 25C (umol/m2/s)
    real(r8), allocatable :: gplant_SPA       (:)   ! Stem (xylem-to-leaf) hydraulic conductance (mmol H2O/m2 leaf area/s/Mpa)
    real(r8), allocatable :: capac_SPA        (:)   ! Plant capacitance (mmol H2O/m2 leaf area/MPa)
    real(r8), allocatable :: iota_SPA         (:)   ! Stomatal water-use efficiency (umol CO2/ mol H2O)
    real(r8), allocatable :: root_radius_SPA  (:)   ! Fine root radius (m)
    real(r8), allocatable :: root_density_SPA (:)   ! Fine root density (g biomass / m3 root)
    real(r8), allocatable :: root_resist_SPA  (:)   ! Hydraulic resistivity of root tissue (MPa.s.g/mmol H2O)
    real(r8), allocatable :: gsmin_SPA        (:)   ! Minimum stomatal conductance (mol H2O/m2/s)
    real(r8), allocatable :: g0_BB            (:)   ! Ball-Berry minimum leaf conductance (mol H2O/m2/s)
    real(r8), allocatable :: g1_BB            (:)   ! Ball-Berry slope of conductance-photosynthesis relationship
    real(r8), allocatable :: g0_MED           (:)   ! Medlyn minimum leaf conductance (mol H2O/m2/s)
    real(r8), allocatable :: g1_MED           (:)   ! Medlyn slope of conductance-photosynthesis relationship
    real(r8), allocatable :: psi50_gs         (:)   ! Leaf water potential at which 50% of stomatal conductance is lost (MPa)
    real(r8), allocatable :: shape_gs         (:)   ! Shape parameter for stomatal conductance in relation to leaf water potential (-)
    real(r8), allocatable :: emleaf           (:)   ! Leaf emissivity (-)
    real(r8), allocatable :: clump_fac        (:)   ! Foliage clumping index (-)
    real(r8), allocatable :: pbeta_lai        (:)   ! Parameter for the leaf area density beta distribution (-)
    real(r8), allocatable :: qbeta_lai        (:)   ! Parameter for the leaf area density beta distribution (-)
    real(r8), allocatable :: pbeta_sai        (:)   ! Parameter for the stem area density beta distribution (-)
    real(r8), allocatable :: qbeta_sai        (:)   ! Parameter for the stem area density beta distribution (-)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate
    procedure, private :: InitRead

  end type pftcon_type

  type(pftcon_type), public :: pftcon
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this)

    class(pftcon_type) :: this

    call this%InitAllocate()
    call this%InitRead()

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate (this)
    !
    ! !DESCRIPTION:
    ! Allocate memory for pft data structure
    !
    ! !ARGUMENTS:
    class(pftcon_type) :: this
    !---------------------------------------------------------------------

    ! CLM pft parameters
    allocate (this%dleaf            (0:mxpft))
    allocate (this%c3psn            (0:mxpft))
    allocate (this%xl               (0:mxpft))
    allocate (this%rhol             (0:mxpft,numrad))
    allocate (this%rhos             (0:mxpft,numrad))
    allocate (this%taul             (0:mxpft,numrad))
    allocate (this%taus             (0:mxpft,numrad))
    allocate (this%roota_par        (0:mxpft))
    allocate (this%rootb_par        (0:mxpft))
    allocate (this%rootprof_beta    (0:mxpft))
    allocate (this%slatop           (0:mxpft))

    ! pft parameters for CLMml
    allocate (this%vcmaxpft         (0:mxpft))
    allocate (this%gplant_SPA       (0:mxpft))
    allocate (this%capac_SPA        (0:mxpft))
    allocate (this%iota_SPA         (0:mxpft))
    allocate (this%root_radius_SPA  (0:mxpft))
    allocate (this%root_density_SPA (0:mxpft))
    allocate (this%root_resist_SPA  (0:mxpft))
    allocate (this%gsmin_SPA        (0:mxpft))
    allocate (this%g0_BB            (0:mxpft))
    allocate (this%g1_BB            (0:mxpft))
    allocate (this%g0_MED           (0:mxpft))
    allocate (this%g1_MED           (0:mxpft))
    allocate (this%psi50_gs         (0:mxpft))
    allocate (this%shape_gs         (0:mxpft))
    allocate (this%emleaf           (0:mxpft))
    allocate (this%clump_fac        (0:mxpft))
    allocate (this%pbeta_lai        (0:mxpft))
    allocate (this%qbeta_lai        (0:mxpft))
    allocate (this%pbeta_sai        (0:mxpft))
    allocate (this%qbeta_sai        (0:mxpft))

  end subroutine InitAllocate

  !-----------------------------------------------------------------------
  subroutine InitRead (this)
    !
    ! !DESCRIPTION:
    ! Read and initialize vegetation (PFT) constants
    !
    ! !ARGUMENTS:
    class(pftcon_type) :: this
    !---------------------------------------------------------------------

    ! PFTs
    ! -------------------------------
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
    ! -------------------------------

    ! Leaf dimension

    this%dleaf(:) = -999._r8
    this%dleaf(1:16) = 0.04_r8
!   this%dleaf(1:16) = 0.0081_r8
!   this%dleaf(1:16) = 0.243_r8

    ! Photosynthetic pathway: 1. = C3 plant and 0. = C4 plant

    this%c3psn(:) = -999._r8
    this%c3psn( 1:13) = 1._r8
    this%c3psn(14:14) = 0._r8
    this%c3psn(15:16) = 1._r8

    ! Leaf angle

    this%xl(:) = -999._r8
    this%xl( 1: 3) = 0.01_r8
    this%xl( 4: 5) = 0.10_r8
    this%xl( 6: 6) = 0.01_r8
    this%xl( 7: 8) = 0.25_r8
    this%xl( 9: 9) = 0.01_r8
    this%xl(10:11) = 0.25_r8
    this%xl(12:16) = -0.30_r8

    ! Leaf reflectance: visible and near-infrared

    this%rhol(:,:) = -999._r8

    this%rhol( 1: 3,ivis) = 0.07_r8
    this%rhol( 4: 8,ivis) = 0.10_r8
    this%rhol( 9: 9,ivis) = 0.07_r8
    this%rhol(10:11,ivis) = 0.10_r8
    this%rhol(12:16,ivis) = 0.11_r8

    this%rhol( 1: 3,inir) = 0.35_r8
    this%rhol( 4: 8,inir) = 0.45_r8
    this%rhol( 9: 9,inir) = 0.35_r8
    this%rhol(10:11,inir) = 0.45_r8
    this%rhol(12:16,inir) = 0.35_r8

    ! Stem reflectance: visible and near-infrared

    this%rhos(:,:) = -999._r8

    this%rhos( 1:11,ivis) = 0.16_r8
    this%rhos(12:16,ivis) = 0.31_r8

    this%rhos( 1:11,inir) = 0.39_r8
    this%rhos(12:16,inir) = 0.53_r8

    ! Leaf transmittance: visible and near-infrared

    this%taul(:,:) = -999._r8

    this%taul(1:16,ivis) = 0.05_r8

    this%taul( 1: 3,inir) = 0.10_r8
    this%taul( 4: 8,inir) = 0.25_r8
    this%taul( 9: 9,inir) = 0.10_r8
    this%taul(10:11,inir) = 0.25_r8
    this%taul(12:16,inir) = 0.34_r8

    ! Stem transmittance: visible and near-infrared

    this%taus(:,:) = -999._r8

    this%taus( 1:11,ivis) = 0.001_r8
    this%taus(12:16,ivis) = 0.12_r8

    this%taus( 1:11,inir) = 0.001_r8
    this%taus(12:16,inir) = 0.25_r8

    ! Zeng2001 rooting distribution parameters (1/m)

    this%roota_par(:) = -999._r8
    this%roota_par( 1: 5) = 7._r8
    this%roota_par( 6: 8) = 6._r8
    this%roota_par( 9:11) = 7._r8
    this%roota_par(12:14) = 11._r8
    this%roota_par(15:16) = 6._r8

    this%rootb_par(:) = -999._r8
    this%rootb_par( 1: 3) = 2._r8
    this%rootb_par( 4: 5) = 1._r8
    this%rootb_par( 6: 8) = 2._r8
    this%rootb_par( 9:11) = 1.5_r8
    this%rootb_par(12:14) = 2._r8
    this%rootb_par(15:16) = 3._r8

    ! Jackson1996 rooting distribution parameters (-)

    this%rootprof_beta(:) = -999._r8
    this%rootprof_beta( 1: 1) = 0.976_r8
    this%rootprof_beta( 2: 3) = 0.943_r8
    this%rootprof_beta( 4: 4) = 0.993_r8
    this%rootprof_beta( 5: 5) = 0.966_r8
    this%rootprof_beta( 6: 6) = 0.993_r8
    this%rootprof_beta( 7: 7) = 0.966_r8
    this%rootprof_beta( 8: 8) = 0.943_r8
    this%rootprof_beta( 9:10) = 0.964_r8
    this%rootprof_beta(11:12) = 0.914_r8
    this%rootprof_beta(13:16) = 0.943_r8

    ! Specific leaf area at top of canopy, projected area basis (m2/gC)

    this%slatop(:) = -999._r8
    this%slatop( 1) = 0.010_r8
    this%slatop( 2) = 0.008_r8
    this%slatop( 3) = 0.024_r8
    this%slatop( 4) = 0.012_r8
    this%slatop( 5) = 0.012_r8
    this%slatop( 6) = 0.030_r8
    this%slatop( 7) = 0.030_r8
    this%slatop( 8) = 0.030_r8
    this%slatop( 9) = 0.012_r8
    this%slatop(10) = 0.030_r8
    this%slatop(11) = 0.030_r8
    this%slatop(12) = 0.030_r8
    this%slatop(13) = 0.030_r8
    this%slatop(14) = 0.030_r8
    this%slatop(15) = 0.030_r8
    this%slatop(16) = 0.030_r8

    ! ========================
    ! pft parameters for CLMml
    ! ========================

    ! vcmax

    this%vcmaxpft(:) = -999._r8
    this%vcmaxpft( 1) = 62.5_r8
    this%vcmaxpft( 2) = 62.5_r8
    this%vcmaxpft( 3) = 39.1_r8
    this%vcmaxpft( 4) = 41.0_r8
    this%vcmaxpft( 5) = 61.4_r8
    this%vcmaxpft( 6) = 41.0_r8
    this%vcmaxpft( 7) = 57.7_r8
    this%vcmaxpft( 8) = 57.7_r8
    this%vcmaxpft( 9) = 61.7_r8
    this%vcmaxpft(10) = 54.0_r8
    this%vcmaxpft(11) = 54.0_r8
    this%vcmaxpft(12) = 78.2_r8
    this%vcmaxpft(13) = 78.2_r8
    this%vcmaxpft(14) = 51.6_r8
    this%vcmaxpft(15) = 100.7_r8
    this%vcmaxpft(16) = 100.7_r8

    ! Plant hydraulics

    this%gplant_SPA(:) = -999._r8
    this%gplant_SPA(1:16) = 4._r8

    this%capac_SPA(:) = -999._r8
    this%capac_SPA( 1:11) = 2500._r8
    this%capac_SPA(12:16) = 500._r8

    ! Stomatal optimization

    this%iota_SPA(:) = -999._r8
    this%iota_SPA(1: 1) = 750._r8
    this%iota_SPA(2: 3) = 1500._r8
    this%iota_SPA(4: 4) = 500._r8
    this%iota_SPA(5:16) = 750._r8

    ! Root hydraulics

    this%root_radius_SPA(:) = -999._r8
    this%root_density_SPA(:) = -999._r8
    this%root_resist_SPA(:) = -999._r8

    this%root_radius_SPA(1:16) = 0.29e-03_r8
    this%root_density_SPA(1:16) = 0.31e06_r8
    this%root_resist_SPA(1:16) = 25._r8

    ! Minimum stomatal conductance

    this%gsmin_SPA(:)= -999._r8
    this%gsmin_SPA(1:16)= 0.002_r8

    ! Ball-Berry stomatal conductance parameters

    this%g0_BB(:)= -999._r8
    this%g1_BB(:)= -999._r8

    this%g0_BB( 1:13) = 0.01_r8
    this%g0_BB(14:14) = 0.04_r8
    this%g0_BB(15:16) = 0.01_r8

    this%g1_BB( 1:13) = 9._r8
    this%g1_BB(14:14) = 4._r8
    this%g1_BB(15:16) = 9._r8

    ! Medlyn stomatal conductance parameters

    this%g0_MED(:)= -999._r8
    this%g1_MED(:)= -999._r8

    this%g0_MED(1:16) = 0.0001_r8

    this%g1_MED( 1) = 2.35_r8
    this%g1_MED( 2) = 2.35_r8
    this%g1_MED( 3) = 2.35_r8
    this%g1_MED( 4) = 4.12_r8
    this%g1_MED( 5) = 4.12_r8
    this%g1_MED( 6) = 4.45_r8
    this%g1_MED( 7) = 4.45_r8
    this%g1_MED( 8) = 4.45_r8
    this%g1_MED( 9) = 4.70_r8
    this%g1_MED(10) = 4.70_r8
    this%g1_MED(11) = 4.70_r8
    this%g1_MED(12) = 2.22_r8
    this%g1_MED(13) = 5.25_r8
    this%g1_MED(14) = 1.62_r8
    this%g1_MED(15) = 5.79_r8
    this%g1_MED(16) = 5.79_r8

    ! Leaf water potential at which 50% of stomatal conductance is lost

    this%psi50_gs(:) = -999._r8
    this%psi50_gs(1:16) = -2.3_r8

    ! Shape parameter for stomatal conductance in relation to leaf water potential

    this%shape_gs(:) = -999._r8
    this%shape_gs(1:16) = 40._r8

    ! Leaf emissivity

    this%emleaf(:) = -999._r8
    this%emleaf(1:16) = 0.98_r8

    ! Foliage clumping index

    this%clump_fac(:) = -999._r8

!   this%clump_fac( 1: 2) = 0.74_r8
!   this%clump_fac( 3: 3) = 0.78_r8
!   this%clump_fac( 4: 5) = 0.66_r8
!   this%clump_fac( 6: 8) = 0.70_r8
!   this%clump_fac( 9:11) = 0.75_r8
!   this%clump_fac(12:16) = 0.75_r8

    this%clump_fac(1:16) = 1._r8

    ! Parameters for the leaf/stem area density
    ! beta distribution. pbeta = qbeta = 1 gives
    ! a uniform distribution.

    this%pbeta_lai(:) = -999._r8
    this%qbeta_lai(:) = -999._r8
    this%pbeta_sai(:) = -999._r8
    this%qbeta_sai(:) = -999._r8

    this%pbeta_lai(1) = 11.5_r8
    this%qbeta_lai(1) = 3.5_r8

    this%pbeta_lai(2:3) = 3.5_r8
    this%qbeta_lai(2:3) = 2.0_r8

    this%pbeta_lai(4:5) = 3.5_r8
    this%qbeta_lai(4:5) = 2.0_r8

    this%pbeta_lai(6:8) = 3.5_r8
    this%qbeta_lai(6:8) = 2.0_r8

    this%pbeta_lai(9:11) = 3.5_r8
    this%qbeta_lai(9:11) = 2.0_r8

    this%pbeta_lai(12:16) = 2.5_r8
    this%qbeta_lai(12:16) = 2.5_r8

    this%pbeta_sai(1:16) = this%pbeta_lai(1:16)
    this%qbeta_sai(1:16) = this%qbeta_lai(1:16)

  end subroutine InitRead

end module pftconMod

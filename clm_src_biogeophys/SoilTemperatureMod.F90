module SoilTemperatureMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Calculate soil temperature. This is simplified from CLM5 to be
  ! compatible with CLMml.
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils, only : endrun
  use ColumnType, only : col
  use decompMod, only : bounds_type
  use SoilStateType, only : soilstate_type
  use TemperatureType, only : temperature_type
  use WaterStateType, only : waterstate_type
  use MLCanopyFluxesType, only : mlcanopy_type
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PRIVATE TYPES:
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: SoilTemperature             ! Compute soil temperature
  public :: SoilThermProp               ! Thermal conductivity and heat capacity
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: tridiag                    ! Solve a tridiagonal system of equations

  real(r8), private, parameter :: thin_sfclayer = 1.0e-6_r8   ! Threshold for thin surface layer
  !-----------------------------------------------------------------------

  contains

  !-----------------------------------------------------------------------
  subroutine SoilTemperature (bounds, num_nolakec, filter_nolakec, &
  soilstate_inst, temperature_inst, waterstate_inst, mlcanopy_inst)
    !
    ! !DESCRIPTION:
    ! Compute soil temperature
    !
    ! !USES:
    use clm_time_manager, only : get_step_size
    use clm_varpar, only : nlevgrnd, nlevsno
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    integer, intent(in) :: num_nolakec                 ! Number of non-lake points in CLM column filter
    integer, intent(in) :: filter_nolakec(:)           ! CLM column filter for non-lake points

    type(soilstate_type)   , intent(inout) :: soilstate_inst
    type(temperature_type) , intent(inout) :: temperature_inst
    type(waterstate_type)  , intent(inout) :: waterstate_inst
    type(mlcanopy_type)    , intent(inout) :: mlcanopy_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: fc                    ! Filter index
    integer  :: c                     ! Column index for CLM g/l/c/p hierarchy
    integer  :: j                     ! Soil layer index
    real(r8) :: dtime                 ! Model time step (s)
    real(r8) :: fact                  ! Term for soil temperature
    real(r8) :: dzm, dzp              ! Soil thickness terms
    real(r8) :: edif                  ! Change in energy (W/m2)

    integer  :: pfilter  (bounds%begc:bounds%endc)                     ! Filter to map patch (p) to column (c)
    real(r8) :: tk_h2osfc(bounds%begc:bounds%endc)                     ! Thermal conductivity of h2osfc (W/m/K)

    real(r8) :: tssbef   (bounds%begc:bounds%endc,-nlevsno+1:nlevgrnd) ! Soil temperature at beginning of time step (K)
    real(r8) :: cv       (bounds%begc:bounds%endc,-nlevsno+1:nlevgrnd) ! Heat capacity (J/m2/K)
    real(r8) :: tk       (bounds%begc:bounds%endc,-nlevsno+1:nlevgrnd) ! Thermal conductivity at the layer interface (W/m/K)
    real(r8) :: atri     (bounds%begc:bounds%endc,         1:nlevgrnd) ! For tridiagonal solution
    real(r8) :: btri     (bounds%begc:bounds%endc,         1:nlevgrnd) ! For tridiagonal solution
    real(r8) :: ctri     (bounds%begc:bounds%endc,         1:nlevgrnd) ! For tridiagonal solution
    real(r8) :: rtri     (bounds%begc:bounds%endc,         1:nlevgrnd) ! For tridiagonal solution
    real(r8) :: utri     (bounds%begc:bounds%endc,         1:nlevgrnd) ! For tridiagonal solution
    !---------------------------------------------------------------------

    associate ( &
    z         => col%z                          , &  ! CLM: Soil layer depth (m)
    t_soisno  => temperature_inst%t_soisno_col  , &  ! CLM: Soil temperature (K)
    gsoi      => mlcanopy_inst%gsoi_soil          &  ! CLMml: Soil heat flux (W/m2)
    )

    !!!!!!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Offline multilayer code (uncoupled to CLM) has one patch on a
    ! column so that p = c. This is not true when coupled to CLM.
    ! Here, pfilter is used to make p -> c for use with gsoi.
    ! This code is not called when coupled to CLM - instead CLM
    ! provides soil temperature.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       pfilter(c) = c
    end do

    ! Get current step size (dtime)

    dtime = get_step_size()

    ! Save current soil temperature for energy conservation check

    do j = 1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          tssbef(c,j) = t_soisno(c,j)
       end do
    end do

    ! Thermal conductivity and heat capacity

    call SoilThermProp (bounds, num_nolakec, filter_nolakec, tk(bounds%begc:bounds%endc,:), &
    cv(bounds%begc:bounds%endc,:), tk_h2osfc(bounds%begc:bounds%endc), &
    temperature_inst, waterstate_inst, soilstate_inst)

    ! Set up tridiagonal matrix

    do j = 1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)

          fact = dtime / cv(c,j)
          if (j == 1) then

             ! Top soil layer with gsoi as boundary condition

             dzp = z(c,j+1) - z(c,j)
             atri(c,j) = 0._r8
             btri(c,j) = 1._r8 + fact * tk(c,j)/dzp
             ctri(c,j) = -fact * tk(c,j) / dzp
             rtri(c,j) = t_soisno(c,j) + fact * gsoi(pfilter(c))

          else if (j <= nlevgrnd-1) then

             ! Layers 2 to nlevgrnd-1

             dzm = z(c,j) - z(c,j-1)
             dzp = z(c,j+1) - z(c,j)
             atri(c,j) = -fact * tk(c,j-1) / dzm
             btri(c,j) = 1._r8 + fact * (tk(c,j-1)/dzm + tk(c,j)/dzp)
             ctri(c,j) = -fact * tk(c,j) / dzp
             rtri(c,j) = t_soisno(c,j)

          else if (j == nlevgrnd) then

            ! Bottom soil layer with zero heat flux

             dzm = z(c,j) - z(c,j-1)
             atri(c,j) = -fact * tk(c,j-1) / dzm
             btri(c,j) = 1._r8 + fact * tk(c,j-1) / dzm
             ctri(c,j) = 0._r8
             rtri(c,j) = t_soisno(c,j)

          end if

       end do
    end do

    ! Solve for soil temperature

    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       call tridiag (atri(c,:), btri(c,:), ctri(c,:), rtri(c,:), utri(c,:), nlevgrnd)
       t_soisno(c,1:nlevgrnd) = utri(c,1:nlevgrnd)
    end do

    ! Check for energy conservation

    do fc = 1, num_nolakec
       c = filter_nolakec(fc)

       edif = 0._r8
       do j = 1, nlevgrnd
          edif = edif + cv(c,j) * (t_soisno(c,j) - tssbef(c,j)) / dtime
       end do

       if (abs(gsoi(pfilter(c))-edif) >= 1.e-06_r8) then
          call endrun (msg='ERROR: SoilTemperature: soil temperature energy conservation error')
       end if

    end do

    end associate 
  end subroutine SoilTemperature

  !-----------------------------------------------------------------------
  subroutine SoilThermProp (bounds, num_nolakec, filter_nolakec, &
  tk, cv, tk_h2osfc, temperature_inst, waterstate_inst, soilstate_inst)
    !
    ! !DESCRIPTION:
    ! Calculation of thermal conductivities and heat capacities of
    ! snow/soil layers
    ! (1) The volumetric heat capacity is calculated as a linear combination
    !     in terms of the volumetric fraction of the constituent phases.
    !
    ! (2) The thermal conductivity of soil is computed from the algorithm of
    !     Johansen (as reported by Farouki 1981), and of snow is from the
    !     formulation used in SNTHERM (Jordan 1991).
    ! The thermal conductivities at the interfaces between two neighboring
    ! layers (j, j+1) are derived from an assumption that the flux across
    ! the interface is equal to that from the node j to the interface and the
    ! flux from the interface to the node j+1.
    !
    ! !USES:
    use clm_varpar, only : nlevsno, nlevgrnd
    use clm_varcon, only : denh2o, denice, tfrz, tkwat, tkice, tkair, cpice, cpliq, thk_bedrock, csol_bedrock
    use clm_varctl, only : iulog
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds                   ! bounds
    integer, intent(in) :: num_nolakec                        ! Number of non-lake points in CLM column filter
    integer, intent(in) :: filter_nolakec(:)                  ! CLM column filter for non-lake points
    real(r8), intent(out) :: tk( bounds%begc: , -nlevsno+1: ) ! Thermal conductivity at the layer interface (W/m/K)
    real(r8), intent(out) :: cv( bounds%begc: , -nlevsno+1: ) ! Heat capacity (J/m2/K)
    real(r8), intent(out) :: tk_h2osfc( bounds%begc: )        ! Thermal conductivity of h2osfc (W/m/K)

    type(temperature_type) :: temperature_inst
    type(waterstate_type) :: waterstate_inst
    type(soilstate_type) :: soilstate_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: j                         ! Soil layer index
    integer  :: fc                        ! Filter index
    integer  :: c                         ! Column index for CLM g/l/c/p hierarchy
    real(r8) :: satw                      ! Relative total water content of soil
    real(r8) :: dke                       ! Kersten number
    real(r8) :: fl                        ! Volume fraction of liquid or unfrozen water to total water
    real(r8) :: dksat                     ! Thermal conductivity for saturated soil (W/m/K)
    real(r8) :: zh2osfc
    !-----------------------------------------------------------------------

    associate( & 
    nbedrock    =>  col%nbedrock                     , & ! Input:  [integer (:)]     depth to bedrock index
    snl         =>  col%snl                          , & ! Input:  [integer (:)]     number of snow layers                    
    dz          =>  col%dz                           , & ! Input:  [real(r8) (:,:)]  layer thickness (m)                       
    zi          =>  col%zi                           , & ! Input:  [real(r8) (:,:)]  layer depth at layer interface (m)
    z           =>  col%z                            , & ! Input:  [real(r8) (:,:)]  layer depth (m)                   
    t_soisno    =>  temperature_inst%t_soisno_col    , & ! Input:  [real(r8) (:,:)]  soil temperature (Kelvin)             
    frac_sno    =>  waterstate_inst%frac_sno_eff_col , & ! Input:  [real(r8) (:)]    fractional snow covered area            
    h2osfc      =>  waterstate_inst%h2osfc_col       , & ! Input:  [real(r8) (:)]    surface water (mm H2O)                        
    h2osno      =>  waterstate_inst%h2osno_col       , & ! Input:  [real(r8) (:)]    snow water (mm H2O)                     
    h2osoi_liq  =>  waterstate_inst%h2osoi_liq_col   , & ! Input:  [real(r8) (:,:)]  liquid water (kg/m2)                  
    h2osoi_ice  =>  waterstate_inst%h2osoi_ice_col   , & ! Input:  [real(r8) (:,:)]  ice lens (kg/m2)                      
    bw          =>  waterstate_inst%bw_col           , & ! Output: [real(r8) (:,:)]  partial density of water in the snow pack (ice + liquid) [kg/m3]
    tkmg        =>  soilstate_inst%tkmg_col          , & ! Input:  [real(r8) (:,:)]  thermal conductivity, soil minerals  [W/m-K]
    tkdry       =>  soilstate_inst%tkdry_col         , & ! Input:  [real(r8) (:,:)]  thermal conductivity, dry soil (W/m/Kelvin)
    csol        =>  soilstate_inst%csol_col          , & ! Input:  [real(r8) (:,:)]  heat capacity, soil solids (J/m**3/Kelvin)
    watsat      =>  soilstate_inst%watsat_col        , & ! Input:  [real(r8) (:,:)]  volumetric soil water at saturation (porosity)
    thk         =>  soilstate_inst%thk_col             & ! Output: [real(r8) (:,:)]  thermal conductivity of each layer  [W/m-K]
    )

    ! Thermal conductivity of soil from Farouki (1981)

    do j = -nlevsno+1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)

          ! Only examine levels from 1 -> nlevgrnd

          if (j >= 1) then
             satw = (h2osoi_liq(c,j)/denh2o + h2osoi_ice(c,j)/denice) / (dz(c,j)*watsat(c,j))
             satw = min(1._r8, satw)
             if (satw > 0.1e-6_r8) then
                if (t_soisno(c,j) >= tfrz) then       ! Unfrozen soil
                   dke = max(0._r8, log10(satw) + 1.0_r8)
                else                                  ! Frozen soil
                   dke = satw
                end if
                fl = (h2osoi_liq(c,j)/(denh2o*dz(c,j))) / (h2osoi_liq(c,j)/(denh2o*dz(c,j)) + &
                                                           h2osoi_ice(c,j)/(denice*dz(c,j)))
                dksat = tkmg(c,j)*tkwat**(fl*watsat(c,j))*tkice**((1._r8-fl)*watsat(c,j))
                thk(c,j) = dke*dksat + (1._r8-dke)*tkdry(c,j)
             else
                thk(c,j) = tkdry(c,j)
             end if
             if (j > nbedrock(c)) thk(c,j) = thk_bedrock
          end if

          ! Thermal conductivity of snow, from Jordan (1991) pp. 18
          ! Only examine levels from snl(c)+1 -> 0 where snl(c) < 1

          if (snl(c)+1 < 1 .AND. (j >= snl(c)+1) .AND. (j <= 0)) then  
             bw(c,j) = (h2osoi_ice(c,j)+h2osoi_liq(c,j))/(frac_sno(c)*dz(c,j))
             thk(c,j) = tkair + (7.75e-5_r8 *bw(c,j) + 1.105e-6_r8*bw(c,j)*bw(c,j))*(tkice-tkair)
          end if

       end do
    end do

    ! Thermal conductivity at the layer interface

    do j = -nlevsno+1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          if (j >= snl(c)+1 .AND. j <= nlevgrnd-1) then
             tk(c,j) = thk(c,j)*thk(c,j+1)*(z(c,j+1)-z(c,j)) &
                     / (thk(c,j)*(z(c,j+1)-zi(c,j))+thk(c,j+1)*(zi(c,j)-z(c,j)))
          else if (j == nlevgrnd) then
             tk(c,j) = 0._r8
          end if
       end do
    end do

    ! Calculate thermal conductivity of h2osfc

    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       zh2osfc = 1.0e-3*(0.5*h2osfc(c)) !convert to [m] from [mm]
       tk_h2osfc(c)= tkwat*thk(c,1)*(z(c,1)+zh2osfc) &
                   / (tkwat*z(c,1)+thk(c,1)*zh2osfc)
    end do

    ! Soil heat capacity, from de Vries (1963)

    do j = 1, nlevgrnd
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          cv(c,j) = csol(c,j)*(1-watsat(c,j))*dz(c,j) + (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
          if (j > nbedrock(c)) cv(c,j) = csol_bedrock*dz(c,j)
          if (j == 1) then
             if (snl(c)+1 == 1 .AND. h2osno(c) > 0._r8) then
                cv(c,j) = cv(c,j) + cpice*h2osno(c)
             end if
          end if
       end do
    end do

    ! Snow heat capacity

    do j = -nlevsno+1, 0
       do fc = 1, num_nolakec
          c = filter_nolakec(fc)
          if (snl(c)+1 < 1 .and. j >= snl(c)+1) then
             if (frac_sno(c) > 0._r8) then
                cv(c,j) = max(thin_sfclayer,(cpliq*h2osoi_liq(c,j) + cpice*h2osoi_ice(c,j))/frac_sno(c))
             else
                cv(c,j) = thin_sfclayer
             endif
          end if
       end do
    end do

    end associate 
   end subroutine SoilThermProp

  !-----------------------------------------------------------------------
  subroutine tridiag (a, b, c, r, u, n)
    !
    ! !DESCRIPTION:
    ! Solve a tridiagonal system of equations
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    integer,  intent(in)  :: n        ! Number of layers
    real(r8), intent(in)  :: a(n)     ! A vector for tridiagonal solution
    real(r8), intent(in)  :: b(n)     ! B vector for tridiagonal solution
    real(r8), intent(in)  :: c(n)     ! C vector for tridiagonal solution
    real(r8), intent(in)  :: r(n)     ! R vector for tridiagonal solution
    real(r8), intent(out) :: u(n)     ! U vector for tridiagonal solution
    !
    ! !LOCAL VARIABLES:
    real(r8) :: gam(n)                ! Temporary calculation
    real(r8) :: bet                   ! Temporary calculation
    integer :: j                      ! Layer index
    !---------------------------------------------------------------------

    ! Tridiagonal solution:
    !
    ! Solve for U given the set of equations F x U = R, where U is a vector
    ! of length N, R is a vector of length N, and F is an N x N tridiagonal
    ! matrix defined by the vectors A, B, C (each of length N). A(1) and
    ! C(N) are undefined and are not referenced by the subroutine.
    !
    !    | b(1) c(1)   0  ...                      |   | u(1)   |   | r(1)   |
    !    | a(2) b(2) c(2) ...                      |   | u(2)   |   | r(2)   |
    !    |                ...                      | x | ...    | = | ...    |
    !    |                ... a(n-1) b(n-1) c(n-1) |   | u(n-1) |   | r(n-1) |
    !    |                ...   0    a(n)   b(n)   |   | u(n)   |   | r(n)   |
    !

    bet = b(1)
    u(1) = r(1) / bet
    do j = 2, n
       gam(j) = c(j-1) / bet
       bet = b(j) - a(j) * gam(j)
       u(j) = (r(j) - a(j) * u(j-1)) / bet
    end do
    do j = n-1, 1, -1
       u(j) = u(j) - gam(j+1) * u(j+1)
    end do

  end subroutine tridiag

end module SoilTemperatureMod

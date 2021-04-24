module CLMml_driver

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Model driver
  !
  ! !USES:
  use abortutils, only : endrun
  use clm_varctl, only : iulog
  use decompMod, only : bounds_type
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: CLMml_drv             ! Model driver
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: init_acclim          ! Read tower meteorology data to get acclimation temperature
  private :: TowerVeg             ! Initialize tower vegetation
  private :: SoilInit             ! Initialize soil temperature and moisture profile
  private :: output               ! Write output files
  private :: ReadCanopyProfiles   ! Read T,Q,U profile data
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine CLMml_drv (bounds)
    !
    ! !DESCRIPTION:
    ! Model driver to process the tower site and year
    !
    ! !USES:
    use clm_instMod
    use MLclm_varctl, only : turb_type
    use clm_time_manager, only : start_date_ymd, start_date_tod, curr_date_tod, dtstep, itim
    use clm_time_manager, only : get_curr_date, get_curr_calday, get_curr_time
    use clm_varorb, only : eccen, mvelpp, lambm0, obliqr
    use controlMod, only : control
    use fileutils, only : getavu, relavu
    use filterMod, only : setFilters, filter
    use lnd_comp_mct, only : lnd_init_mct, lnd_run_mct
    use PatchType, only : patch
    use pftconMod, only : pftcon
    use shr_orb_mod, only : shr_orb_params
    use TowerDataMod, only : tower_id, tower_num
    use TowerMetMod, only : TowerMet
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    real(r8) :: obliq, mvelp                   ! Miscellaneous orbital parameters (not used)
    integer  :: ntim                           ! Number of time steps to process
    integer  :: time_indx                      ! Time index for CLM history file
    integer  :: curr_time_day                  ! Number of whole days
    integer  :: curr_time_sec                  ! Remaining seconds in the day
    integer  :: yr                             ! Year (1900, ...)
    integer  :: mon                            ! Month (1, ..., 12)
    integer  :: day                            ! Day of month (1, ..., 31)
    real(r8) :: curr_calday                    ! Current calendar day (equals 1.000 on 0Z January 1 of current year)
    real(r8) :: start_calday_clm               ! Calendar day at start of CLM history file
    integer  :: run_start_date                 ! Temporary variable
    integer  :: run_start_tod                  ! Temporary variable
    integer  :: clm_start_ymd                  ! CLM history file start date (yyyymmdd format)
    integer  :: clm_start_tod                  ! CLM history file start time-of-day (seconds past 0Z UTC)
    integer  :: nout1, nout2, nout3, nout4     ! Fortran unit number
    integer  :: nin1                           ! Fortran unit number

    character(len=256) :: diratm               ! Tower meteorology file directory path
    character(len=256) :: dirclm               ! CLM history file directory path
    character(len=256) :: dirout               ! Model output file directory path
    character(len=256) :: dirin                ! Model input file directory path for profile data
    character(len=256) :: ext                  ! Local file name
    character(len=256) :: fin_tower            ! Tower meteorology file name
    character(len=256) :: fin_clm              ! CLM file name
    character(len=256) :: fout1, fout2         ! Full output file name, including directory path
    character(len=256) :: fout3, fout4         ! Full output file name, including directory path
    character(len=256) :: fin1                 ! Full input file name for profile data, including directory path
    !---------------------------------------------------------------------

    ! Initialize namelist run control variables

    call control (ntim, clm_start_ymd, clm_start_tod, diratm, dirclm, dirout, dirin)

    !---------------------------------------------------------------
    ! Extract year (yr), month (mon), and day of month (day)
    ! from start_date_ymd
    !---------------------------------------------------------------

    itim = 1
    call get_curr_date (yr, mon, day, curr_date_tod)

    write (iulog,*) 'Processing: ',tower_id(tower_num),yr,mon

    !---------------------------------------------------------------
    ! Initialize CLM
    !---------------------------------------------------------------

    ! CLM uses a subgrid hierarchy consisting of grid cell (g),
    ! land unit (l), column (c), and patch (p). This code processes
    ! one patch (one grid cell with one column and one patch).

    call lnd_init_mct (bounds)

    ! Reset leaf/stem area density parameters for US-Me2 to pine.
    ! This is hardwired for one patch (p=1).

    if (tower_id(tower_num) == 'US-Me2') then
       pftcon%pbeta_lai(patch%itype(1)) = 11.5_r8
       pftcon%qbeta_lai(patch%itype(1)) = 3.5_r8
       pftcon%pbeta_sai(patch%itype(1)) = 11.5_r8
       pftcon%qbeta_sai(patch%itype(1)) = 3.5_r8
    end if

    ! Build the necessary CLM filters to process patches

    call setFilters (filter)

    !---------------------------------------------------------------
    ! Calculate orbital parameters for this year
    !---------------------------------------------------------------

    call shr_orb_params (yr, eccen, obliq, mvelp, obliqr, lambm0, mvelpp)

    !---------------------------------------------------------------
    ! Tower meteorology file (fin_tower) and CLM history file (fin_clm)
    ! to read forcing data. These are netcdf files.
    !---------------------------------------------------------------

    write (ext,'(a6,"/",i4.4,"-",i2.2,".nc")') tower_id(tower_num),yr,mon
    fin_tower = diratm(1:len(trim(diratm)))//ext(1:len(trim(ext)))

    if (tower_id(tower_num) == 'CHATS7') then
       write (ext,'("CHATS_A15.clm2.h1.2007-04-01-00000.nc")')
    else if (tower_id(tower_num) == 'UMBSmw') then
       write (ext,'("clm50d30wspinsp_US-UMB_WOZNIAK.clm2.h1.",i4.4,".nc")') yr
    else
       write (ext,'("lp67wspinPTCLM_",a6,"_I_2000_CLM45.clm2.h1.",i4.4,".nc")') tower_id(tower_num),yr
    end if
    fin_clm = dirclm(1:len(trim(dirclm)))//tower_id(tower_num)//"/"//ext(1:len(trim(ext)))

    !---------------------------------------------------------------
    ! Read tower meteorology data once to get acclimation temperature
    !---------------------------------------------------------------

    call init_acclim (fin_tower, tower_num, ntim, bounds%begp, bounds%endp, &
    atm2lnd_inst, temperature_inst, frictionvel_inst, mlcanopy_inst)

    !---------------------------------------------------------------
    ! Initialize tower vegetation
    !---------------------------------------------------------------

    call TowerVeg (tower_num, bounds%begp, bounds%endp, canopystate_inst, mlcanopy_inst)

    !---------------------------------------------------------------
    ! Read CLM history file to initialize soil temperature and
    ! moisture profiles
    !---------------------------------------------------------------

    ! Find calendar day of first time slice in CLM history file based on
    ! start date (clm_start_ymd) and start time-of-day (clm_start_tod).
    ! This is a hack because get_curr_calday uses start_date_ymd and
    ! start_date_tod, but it works.

    run_start_date = start_date_ymd             ! Save this
    run_start_tod  = start_date_tod             ! Save this

    start_date_ymd = clm_start_ymd              ! Use this to get calendar date for CLM history file
    start_date_tod = clm_start_tod              ! Use this to get calendar date for CLM history file

    itim = 1                                    ! Here, itim is the first time slice in CLM history file
    start_calday_clm = get_curr_calday(offset=0)

    ! Now find calendar day for start of the simulation run

    start_date_ymd = run_start_date             ! Reset to correct value for start of run
    start_date_tod = run_start_tod              ! Reset to correct value for start of run

    itim = 1                                    ! itim is the first time step of the simulation
    curr_calday = get_curr_calday(offset=0)

    ! Calculate correct time slice (number of time steps) into CLM history file

    time_indx = nint((curr_calday - start_calday_clm) * 86400._r8 / float(dtstep)) + 1

    ! Read history file

    call SoilInit (fin_clm, time_indx, bounds%begc, bounds%endc, soilstate_inst, &
    waterstate_inst, temperature_inst)

    !---------------------------------------------------------------
    ! Model output files for fluxes (fout1), auxillary data (fout2),
    ! profile data (fout3), and sun/shade fluxes (fout4). These are
    ! ascii data files and so must be opened here.
    !---------------------------------------------------------------

    write (ext,'(a6,"_",i4.4,"-",i2.2,"_flux.out")') tower_id(tower_num),yr,mon
    fout1 = dirout(1:len(trim(dirout)))//ext(1:len(trim(ext)))
    nout1 = getavu()
    open (unit=nout1, file=trim(fout1), action="write")

    write (ext,'(a6,"_",i4.4,"-",i2.2,"_aux.out")') tower_id(tower_num),yr,mon
    fout2 = dirout(1:len(trim(dirout)))//ext(1:len(trim(ext)))
    nout2 = getavu()
    open (unit=nout2, file=trim(fout2), action="write")

    write (ext,'(a6,"_",i4.4,"-",i2.2,"_profile.out")') tower_id(tower_num),yr,mon
    fout3 = dirout(1:len(trim(dirout)))//ext(1:len(trim(ext)))
    nout3 = getavu()
    open (unit=nout3, file=trim(fout3), action="write")

    write (ext,'(a6,"_",i4.4,"-",i2.2,"_fsun.out")') tower_id(tower_num),yr,mon
    fout4 = dirout(1:len(trim(dirout)))//ext(1:len(trim(ext)))
    nout4 = getavu()
    open (unit=nout4, file=trim(fout4), action="write")

    !---------------------------------------------------------------
    ! Open ascii profile data input file if desired
    !---------------------------------------------------------------

    if (turb_type .eq. -1) then
       write (ext,'(a6,"_",i4.4,"-",i2.2,"_profile.out")') tower_id(tower_num),yr,mon
       fin1 = dirin(1:len(trim(dirin)))//ext(1:len(trim(ext)))
       nin1 = getavu()
       open (unit=nin1, file=trim(fin1), action="read")
    end if

    !---------------------------------------------------------------
    ! Time stepping loop
    !---------------------------------------------------------------

    write (iulog,*) 'Starting time stepping loop .....'

    do itim = 1, ntim

       ! Get current date, time, and calendar day. These are for itim (at
       ! end of the time step). 
       !
       ! itim = time index from start date
       ! curr_calday = current calendar day (equal to 1.000 on 0Z January 1 of current year)

       call get_curr_date (yr, mon, day, curr_date_tod)
       call get_curr_time (curr_time_day, curr_time_sec)
       curr_calday = get_curr_calday(offset=0)

       ! Calculate correct time slice (number of time steps) into CLM history file

       time_indx = nint((curr_calday - start_calday_clm) * 86400._r8 / float(dtstep)) + 1

       ! Read tower meteorology for current time slice

       call TowerMet (fin_tower, itim, tower_num, bounds%begp, bounds%endp, atm2lnd_inst, &
       frictionvel_inst)

       ! Read T,Q,U profile data for current time step

       if (turb_type .eq. -1) call ReadCanopyProfiles (itim, curr_calday, nin1, mlcanopy_inst)

       ! Call model to calculate fluxes (as in CLM)

       call lnd_run_mct (bounds, time_indx, fin_clm)

       ! Write output files

       call output (curr_calday, nout1, nout2, nout3, nout4, mlcanopy_inst)

    end do

    !---------------------------------------------------------------
    ! Close ascii output and input files
    !---------------------------------------------------------------

    close (nout1)
    call relavu (nout1)
    close (nout2)
    call relavu (nout2)
    close (nout3)
    call relavu (nout3)
    close (nout4)
    call relavu (nout4)

    if (turb_type .eq. -1) then
       close (nin1)
       call relavu (nin1)
    end if

    write (iulog,*) 'Successfully finished simulation'

  end subroutine CLMml_drv

  !-----------------------------------------------------------------------
  subroutine init_acclim (fin, tower_num, ntim, begp, endp, &
  atm2lnd_inst, temperature_inst, frictionvel_inst, mlcanopy_inst)
    !
    ! !DESCRIPTION:
    ! Read tower meteorology data once to get acclimation temperature
    !
    ! !USES:
    use PatchType, only : patch
    use TowerMetMod, only : TowerMet
    use atm2lndType, only : atm2lnd_type
    use TemperatureType, only : temperature_type
    use FrictionVelocityMod, only : frictionvel_type
    use MLCanopyFluxesType, only : mlcanopy_type
    !
    ! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: fin     ! Tower meteorology file
    integer, intent(in) :: tower_num        ! Tower site index
    integer, intent(in) :: ntim             ! Number of time slices to process
    integer, intent(in) :: begp, endp       ! First and last patch
    type(atm2lnd_type), intent(inout) :: atm2lnd_inst
    type(temperature_type), intent(inout) :: temperature_inst
    type(frictionvel_type), intent(inout) :: frictionvel_inst
    type(mlcanopy_type), intent(inout) :: mlcanopy_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: p                           ! Patch index for CLM g/l/c/p hierarchy
    integer  :: c                           ! Column index for CLM g/l/c/p hierarchy
    integer  :: itim                        ! Time index
    !---------------------------------------------------------------------

    associate ( &
    forc_t    => atm2lnd_inst%forc_t_downscaled_col   , & ! CLM: Atmospheric temperature (K)
    forc_pbot => atm2lnd_inst%forc_pbot_downscaled_col, & ! CLM: Atmospheric pressure (Pa)
    t10       => temperature_inst%t_a10_patch         , & ! CLM: Average air temperature for acclimation (K)
    pref      => mlcanopy_inst%pref_forcing             & ! Air pressure at reference height (Pa)
    )

    ! Initialize accumulator to zero

    do p = begp, endp
       t10(p) = 0._r8
    end do

    ! Loop over all time slices to read tower data

    do itim = 1, ntim

       ! Read temperature for this time slice

       call TowerMet (fin, itim, tower_num, begp, endp, atm2lnd_inst, &
       frictionvel_inst)

       do p = begp, endp
          c = patch%column(p)

          ! Sum temperature

          t10(p) = t10(p) + forc_t(c)

          ! Save pressure for first timestep (used only if reading Q vertical profile from dataset)

          if (itim == 1) pref(p) = forc_pbot(c)
       end do

    end do

    ! Average temperature over all time slices

    do p = begp, endp
       t10(p) = t10(p) / float(ntim)
    end do

    end associate
  end subroutine init_acclim

  !-----------------------------------------------------------------------
  subroutine TowerVeg (it, begp, endp, canopystate_inst, mlcanopy_inst)
    !
    ! !DESCRIPTION:
    ! Initialize tower vegetation
    !
    ! !USES:
    use clm_varpar, only : mxpft
    use PatchType, only : patch
    use TowerDataMod, only : tower_pft, tower_canht
    use CanopyStateType, only : canopystate_type
    use MLCanopyFluxesType, only : mlcanopy_type
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: it                    ! Tower site index
    integer, intent(in) :: begp, endp            ! First and last patch
    type(canopystate_type), intent(inout) :: canopystate_inst
    type(mlcanopy_type), intent(inout) :: mlcanopy_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: p                                ! Patch index for CLM g/l/c/p hierarchy

    ! CLM top canopy height, by PFTs

    real(r8) :: htop_pft(0:mxpft)                ! CLM canopy top height, by PFT (m)
    data htop_pft(0) / 0._r8 /
    data htop_pft(1:16) / 17._r8, 17._r8, 14._r8, 35._r8, 35._r8, 18._r8, 20._r8, 20._r8, &
                          0.5_r8, 0.5_r8, 0.5_r8, 0.5_r8, 0.5_r8, 0.5_r8, 0.5_r8, 0.5_r8 /
    data htop_pft(17:mxpft) / 62*0 /
    !---------------------------------------------------------------------

    associate ( &
    htop         => canopystate_inst%htop_patch,       &  ! CLM: canopy height (m)
    root_biomass => mlcanopy_inst%root_biomass_canopy  &  ! Fine root biomass (g biomass / m2)
    )

    ! Assign PFT, fine root biomass, canopy height

    do p = begp, endp
       patch%itype(p) = tower_pft(it)
       root_biomass(p) = 500._r8
       htop(p) = htop_pft(patch%itype(p))

       ! Overwrite canopy height with tower data (if available)

       if (tower_canht(it) /= -999._r8) then
          htop(p) = tower_canht(it)
       end if
    end do

    end associate
  end subroutine TowerVeg

  !-----------------------------------------------------------------------
  subroutine SoilInit (ncfilename, strt, begc, endc, soilstate_inst, &
  waterstate_inst, temperature_inst)
    !
    ! !DESCRIPTION:
    ! Initialize soil temperature and soil moisture profile from CLM netcdf
    ! history file
    !
    ! !USES:
    use abortutils, only : handle_err
    use clm_varcon, only : denh2o
    use clm_varpar, only : nlevgrnd, nlevsoi
    use ColumnType, only : col
    use SoilStateType, only : soilstate_type
    use WaterStateType, only : waterstate_type
    use TemperatureType, only : temperature_type
    use MLclm_varctl, only : clm_phys
    !
    ! !ARGUMENTS:
    implicit none
    include 'netcdf.inc'
    character(len=*), intent(in) :: ncfilename ! CLM netcdf filename
    integer, intent(in) :: strt                ! Current time slice of data to retrieve from CLM history file
    integer, intent(in) :: begc, endc          ! First and last column
    type(soilstate_type), intent(in) :: soilstate_inst
    type(waterstate_type), intent(inout) :: waterstate_inst
    type(temperature_type), intent(inout) :: temperature_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: c                              ! Column index for CLM g/l/c/p hierarchy
    integer  :: j                              ! Soil layer index
    integer  :: ncid                           ! netcdf file ID
    integer  :: status                         ! Function return status
    integer  :: varid                          ! netcdf variable id
    integer  :: start3(3), count3(3)           ! Start and count arrays for reading 3-D data from netcdf files
    real(r8) :: tsoi_loc(1,1,nlevgrnd)         ! CLM: soil temperature (K)
    real(r8) :: h2osoi_loc_clm45(1,1,nlevgrnd) ! CLM4.5: volumetric soil moisture (m3/m3)
    real(r8) :: h2osoi_loc_clm50(1,1,nlevsoi)  ! CLM5.0: volumetric soil moisture (m3/m3)
    !---------------------------------------------------------------------

    associate ( &
    dz          => col%dz                         , &  ! CLM: Soil layer thickness (m)
    nbedrock    => col%nbedrock                   , &  ! CLM: Depth to bedrock index
    watsat      => soilstate_inst%watsat_col      , &  ! CLM: Soil layer volumetric water content at saturation (porosity)
    t_soisno    => temperature_inst%t_soisno_col  , &  ! CLM: Soil temperature (K)
    h2osoi_vol  => waterstate_inst%h2osoi_vol_col , &  ! CLM: Soil layer volumetric water content (m3/m3)
    h2osoi_ice  => waterstate_inst%h2osoi_ice_col , &  ! CLM: Soil layer ice lens (kg H2O/m2)
    h2osoi_liq  => waterstate_inst%h2osoi_liq_col   &  ! CLM: Soil layer liquid water (kg H2O/m2)
    )

    ! Open file

    status = nf_open(ncfilename, nf_nowrite, ncid)
    if (status /= nf_noerr) call handle_err(status, ncfilename)

    ! Dimensions in FORTRAN are in column major order: the first array index
    ! varies the most rapidly. In NetCDF file the dimensions appear in the
    ! opposite order: lat, lon (2-D); time, lat, lon (3-D); time, levgrnd, lat,
    ! lon (4-D)

    start3 = (/ 1,  1, strt /)

    ! Read TSOI(nlndgrid, nlevgrnd, ntime): soil temperature

    status = nf_inq_varid(ncid, "TSOI", varid)
    if (status /= nf_noerr) call handle_err(status, "TSOI")

    count3 = (/ 1, nlevgrnd, 1 /)
    status = nf_get_vara_double(ncid, varid, start3, count3, tsoi_loc)
    if (status /= nf_noerr) call handle_err(status, "tsoi_loc")

    ! Read H2OSOI(nlndgrid, nlevgrnd, ntime): volumetric soil water

    status = nf_inq_varid(ncid, "H2OSOI", varid)
    if (status /= nf_noerr) call handle_err(status, "H2OSOI")

    if (clm_phys == 'CLM4_5') then
       count3 = (/ 1, nlevgrnd, 1 /)
       status = nf_get_vara_double(ncid, varid, start3, count3, h2osoi_loc_clm45)
       if (status /= nf_noerr) call handle_err(status, "h2osoi_loc_clm45")
    else if (clm_phys == 'CLM5_0') then
       count3 = (/ 1, nlevsoi, 1 /)
       status = nf_get_vara_double(ncid, varid, start3, count3, h2osoi_loc_clm50)
       if (status /= nf_noerr) call handle_err(status, "h2osoi_loc_clm50")
    end if

    ! Close file

    status = nf_close(ncid)

    ! Copy data to model variables

    do c = begc, endc

       do j = 1, nlevgrnd
          t_soisno(c,j) = tsoi_loc(1,1,j)
       end do

       if (clm_phys == 'CLM4_5') then
          do j = 1, nlevgrnd
             h2osoi_vol(c,j) = h2osoi_loc_clm45(1,1,j)
          end do
       else if (clm_phys == 'CLM5_0') then
          do j = 1, nlevsoi
             h2osoi_vol(c,j) = h2osoi_loc_clm50(1,1,j)
          end do
          do j = nlevsoi+1, nlevgrnd
             h2osoi_vol(c,j) = 0._r8
          end do
       end if

       ! Limit hydrologically active soil layers to <= watsat. This is needed
       ! because the model's porosity (watsat) is not exactly the same as in
       ! CLM5.

       if (clm_phys == 'CLM5_0') then
          do j = 1, nbedrock(c)
             h2osoi_vol(c,j) = min(h2osoi_vol(c,j), watsat(c,j))
          end do
       end if

       ! Set liquid water and ice

       do j = 1, nlevgrnd
          h2osoi_liq(c,j) = h2osoi_vol(c,j) * dz(c,j) * denh2o
          h2osoi_ice(c,j) = 0._r8
       end do

    end do

    end associate
  end subroutine SoilInit

  !-----------------------------------------------------------------------
  subroutine output (curr_calday, nout1, nout2, nout3, nout4, mlcan)
    !
    ! !DESCRIPTION:
    ! Write output
    !
    ! !USES:
    use clm_varcon, only : tfrz
    use clm_varpar, only : ivis, inir
    use ColumnType, only : col
    use MLclm_varcon, only : mmdry, mmh2o
    use MLclm_varpar, only : isun, isha
    use MLCanopyFluxesType, only : mlcanopy_type
    !
    ! !ARGUMENTS:
    implicit none
    real(r8), intent(in) :: curr_calday
    integer,  intent(in) :: nout1, nout2, nout3, nout4
    type(mlcanopy_type), intent(in) :: mlcan
    !
    ! !LOCAL VARIABLES:
    integer  :: ic                     ! Aboveground layer index
    integer  :: top                    ! Top canopy layer index
    integer  :: mid                    ! Mid-canopy layer index
    integer  :: p                      ! Patch index for CLM g/l/c/p hierarchy
    real(r8) :: swup                   ! Reflected solar radiation (W/m2)
    real(r8) :: tair                   ! Air temperature
    real(r8) :: qair                   ! Specific humidity (g/kg)
    real(r8) :: eair                   ! Vapor pressure (kPa)
    real(r8) :: ra                     ! Aerodynamic resistance (s/m)
    real(r8) :: lad                    ! Leaf area density (m2/m3)
    real(r8) :: missing_value          ! Missing value
    real(r8) :: zero_value             ! Zero
    !---------------------------------------------------------------------

    missing_value = -999._r8
    zero_value = 0._r8

    p = 1

    ! -----------------------------------------------
    ! Output file ...flux.out - Canopy and soil fluxes
    ! -----------------------------------------------

    swup = mlcan%albcan_canopy(p,ivis)*(mlcan%swskyb_forcing(p,ivis)+mlcan%swskyd_forcing(p,ivis)) &
         + mlcan%albcan_canopy(p,inir)*(mlcan%swskyb_forcing(p,inir)+mlcan%swskyd_forcing(p,inir))

    write (nout1,'(13f10.3)') mlcan%rnet_canopy(p), mlcan%stflx_canopy(p), mlcan%shflx_canopy(p), &
    mlcan%lhflx_canopy(p), mlcan%gppveg_canopy(p), mlcan%ustar_canopy(p), swup, mlcan%lwup_canopy(p), &
    mlcan%taf_canopy(p), mlcan%gsoi_soil(p), mlcan%rnsoi_soil(p), mlcan%shsoi_soil(p), mlcan%lhsoi_soil(p)

    ! -----------------------------------------------------
    ! Output file ...fsun.out - Sunlit/shaded canopy fluxes
    ! -----------------------------------------------------

    write (nout4,'(32f10.3)') mlcan%solar_zen_forcing(p)*180._r8/3.1415927_r8, &
    mlcan%swskyb_forcing(p,ivis)+mlcan%swskyd_forcing(p,ivis), &
    mlcan%lai_canopy(p)+mlcan%sai_canopy(p), mlcan%laisun_canopy(p), mlcan%laisha_canopy(p), &
    mlcan%swveg_canopy(p,ivis), mlcan%swvegsun_canopy(p,ivis), mlcan%swvegsha_canopy(p,ivis), &
    mlcan%gppveg_canopy(p), mlcan%gppvegsun_canopy(p), mlcan%gppvegsha_canopy(p), &
    mlcan%lhveg_canopy(p), mlcan%lhvegsun_canopy(p), mlcan%lhvegsha_canopy(p), &
    mlcan%shveg_canopy(p), mlcan%shvegsun_canopy(p), mlcan%shvegsha_canopy(p), &
    mlcan%vcmax25veg_canopy(p), mlcan%vcmax25sun_canopy(p), mlcan%vcmax25sha_canopy(p), &
    mlcan%gsveg_canopy(p), mlcan%gsvegsun_canopy(p), mlcan%gsvegsha_canopy(p), &
    mlcan%windveg_canopy(p), mlcan%windvegsun_canopy(p), mlcan%windvegsha_canopy(p), &
    mlcan%tlveg_canopy(p), mlcan%tlvegsun_canopy(p), mlcan%tlvegsha_canopy(p), &
    mlcan%taveg_canopy(p), mlcan%tavegsun_canopy(p), mlcan%tavegsha_canopy(p)

    ! ----------------------------------------------------------------------
    ! Output file ...aux.out - Leaf water potential and soil moisture stress
    ! ----------------------------------------------------------------------

    top = mlcan%ntop_canopy(p)
    mid = max(1, mlcan%nbot_canopy(p) + (mlcan%ntop_canopy(p)-mlcan%nbot_canopy(p)+1)/2 - 1)

    write (nout2,'(f10.4,5f10.3)') mlcan%btran_soil(p), mlcan%lsc_profile(p,top), mlcan%psis_soil(p), &
    mlcan%lwp_mean_profile(p,top), mlcan%lwp_mean_profile(p,mid), mlcan%fracminlwp_canopy(p)

    ! ----------------------------------------------
    ! Output file ...profile.out - Vertical profiles
    ! ----------------------------------------------

    ! Above canopy layers

!   go to 200
    do ic = mlcan%ncan_canopy(p), mlcan%ntop_canopy(p)+1, -1
!      tair = mlcan%tair_profile(p,ic) - mlcan%tref_forcing(p)
!      tair = mlcan%tair_profile(p,ic) - tfrz
       tair = mlcan%tair_profile(p,ic)
       qair = 1000._r8 * (mmh2o/mmdry) * mlcan%eair_profile(p,ic) &
            / (mlcan%pref_forcing(p) - (1._r8-mmh2o/mmdry) * mlcan%eair_profile(p,ic))
       eair = mlcan%eair_profile(p,ic) / 1000._r8
       ra = mlcan%rhomol_forcing(p) / mlcan%gac_profile(p,ic)
       lad = mlcan%dpai_profile(p,ic) / mlcan%dz_profile(p,ic)

       write (nout3,'(f10.4,26f10.3)') curr_calday, mlcan%zs_profile(p,ic), zero_value, &
       zero_value, zero_value, zero_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       missing_value, missing_value, &
       mlcan%wind_profile(p,ic), tair, qair
    end do
200 continue

    ! Within canopy layers

    do ic = mlcan%ntop_canopy(p), 1, -1
!      tair = mlcan%tair_profile(p,ic) - mlcan%tref_forcing(p)
!      tair = mlcan%tair_profile(p,ic) - tfrz
       tair = mlcan%tair_profile(p,ic)
       qair = 1000._r8 * (mmh2o/mmdry) * mlcan%eair_profile(p,ic) &
            / (mlcan%pref_forcing(p) - (1._r8-mmh2o/mmdry) * mlcan%eair_profile(p,ic))
       eair = mlcan%eair_profile(p,ic) / 1000._r8
       ra = mlcan%rhomol_forcing(p) / mlcan%gac_profile(p,ic)
       lad = mlcan%dpai_profile(p,ic) / mlcan%dz_profile(p,ic)

       if (mlcan%dpai_profile(p,ic) > 0._r8) then
          ! Leaf layer source fluxes (per unit ground area)
          ! write (nout3,'(f10.4,14f10.3)') curr_calday, mlcan%zs_profile(p,ic), mlcan%dpai_profile(p,ic), &
          ! mlcan%rnsrc_profile(p,ic), mlcan%shsrc_profile(p,ic), mlcan%lhsrc_profile(p,ic), &
          ! mlcan%fco2src_profile(p,ic), mlcan%apar_mean_profile(p,ic), mlcan%gs_mean_profile(p,ic), &
          ! mlcan%lwp_mean_profile(p,ic), mlcan%tleaf_mean_profile(p,ic), mlcan%wind_profile(p,ic), &
          ! tair, eair, ra

          ! Leaf layer weighted mean leaf fluxes (per unit leaf area)
          ! write (nout3,'(f10.4,14f10.3)') curr_calday, mlcan%zs_profile(p,ic), lad, &
          ! mlcan%rnleaf_mean_profile(p,ic), mlcan%shleaf_mean_profile(p,ic), mlcan%lhleaf_mean_profile(p,ic), &
          ! mlcan%fco2_mean_profile(p,ic), mlcan%apar_mean_profile(p,ic), mlcan%gs_mean_profile(p,ic), &
          ! mlcan%lwp_mean_profile(p,ic), mlcan%tleaf_mean_profile(p,ic), mlcan%wind_profile(p,ic), &
          ! tair, eair, mlcan%vcmax25_profile(p,ic)

          ! Leaf fluxes (per unit leaf area)
          write (nout3,'(f10.4,26f10.3)') curr_calday, mlcan%zs_profile(p,ic), mlcan%fracsun_profile(p,ic), &
          lad, lad*mlcan%fracsun_profile(p,ic), lad*(1._r8-mlcan%fracsun_profile(p,ic)), &
          mlcan%rnleaf_leaf(p,ic,isun), mlcan%rnleaf_leaf(p,ic,isha), &
          mlcan%shleaf_leaf(p,ic,isun), mlcan%shleaf_leaf(p,ic,isha), &
          mlcan%lhleaf_leaf(p,ic,isun), mlcan%lhleaf_leaf(p,ic,isha), &
          mlcan%anet_leaf(p,ic,isun), mlcan%anet_leaf(p,ic,isha), &
          mlcan%apar_leaf(p,ic,isun), mlcan%apar_leaf(p,ic,isha), &
          mlcan%gs_leaf(p,ic,isun), mlcan%gs_leaf(p,ic,isha), &
          mlcan%lwp_hist_leaf(p,ic,isun), mlcan%lwp_hist_leaf(p,ic,isha), &
          mlcan%tleaf_hist_leaf(p,ic,isun), mlcan%tleaf_hist_leaf(p,ic,isha), &
          mlcan%vcmax25_leaf(p,ic,isun), mlcan%vcmax25_leaf(p,ic,isha), &
          mlcan%wind_profile(p,ic), tair, qair
       else
          ! Non-leaf layer
          ! write (nout3,'(f10.4,14f10.3)') curr_calday, mlcan%zs_profile(p,ic), zero_value, &
          ! missing_value, missing_value, missing_value, &
          ! missing_value, missing_value, missing_value, &
          ! missing_value, missing_value, mlcan%wind_profile(p,ic), &
          ! tair, eair, missing_value

          ! Non-leaf layer
          write (nout3,'(f10.4,26f10.3)') curr_calday, mlcan%zs_profile(p,ic), mlcan%fracsun_profile(p,ic), &
          zero_value, zero_value, zero_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          missing_value, missing_value, &
          mlcan%wind_profile(p,ic), tair, qair
       end if

    end do

    ! Ground

!   tair = mlcan%tg_soil(p) - mlcan%tref_forcing(p)
!   tair = mlcan%tg_soil(p) - tfrz
    tair = mlcan%tg_soil(p)
    eair = mlcan%eg_soil(p) / 1000._r8
    ra = mlcan%rhomol_forcing(p) / mlcan%gac0_soil(p)

!   write (nout3,'(f10.4,14f10.3)') curr_calday, zero_value, zero_value, &
!   mlcan%rnsoi_soil(p), mlcan%shsoi_soil(p), mlcan%lhsoi_soil(p), &
!   missing_value, missing_value, missing_value, &
!   missing_value, missing_value, zero_value, &
!   tair, eair, ra

  end subroutine output

  !-----------------------------------------------------------------------
  subroutine ReadCanopyProfiles (itim, curr_calday, nin1, mlcanopy_inst)
    !
    ! !DESCRIPTION:
    ! Read T,Q,U profile data for current time step
    !
    ! !USES:
    use MLclm_varcon, only : mmh2o, mmdry
    use MLCanopyFluxesType, only : mlcanopy_type
    !
    ! !ARGUMENTS:
    implicit none
    integer,  intent(in) :: itim        ! Time index
    real(r8), intent(in) :: curr_calday ! Current calendar day (equals 1.000 on 0Z January 1 of current year)
    integer,  intent(in) :: nin1        ! Fortran unit number
    type(mlcanopy_type), intent(inout) :: mlcanopy_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: p                       ! Patch index for CLM g/l/c/p hierarchy
    integer  :: ic                      ! Aboveground layer index
    real(r8) :: err                     ! Error check
    real(r8) :: curr_calday_data        ! Calendar day from dataset
    real(r8) :: zs_data                 ! Canopy layer height from dataset (m)
    real(r8) :: wind                    ! Canopy layer wind speed from dataset (m/s)
    real(r8) :: tair                    ! Canopy layer air temperature from dataset (K)
    real(r8) :: qair                    ! Canopy layer specific humidity from dataset (kg/kg)
    real(r8) :: x(22)                   ! Dummy variables from dataset
    integer  :: i                       ! Dummy index for x
    integer  :: nrec                    ! Number of vertical levels in data file
    real(r8) :: check                   ! Check for same calendar day
    !---------------------------------------------------------------------

    associate ( &
    ncan           => mlcanopy_inst%ncan_canopy           , &  ! Number of aboveground layers
    pref           => mlcanopy_inst%pref_forcing          , &  ! Air pressure at reference height (Pa)
    zs             => mlcanopy_inst%zs_profile            , &  ! Canopy layer height for scalar concentration and source (m)
    wind_data      => mlcanopy_inst%wind_data_profile     , &  ! Canopy layer wind speed FROM DATASET (m/s)
    tair_data      => mlcanopy_inst%tair_data_profile     , &  ! Canopy layer air temperature FROM DATASET (K)
    eair_data      => mlcanopy_inst%eair_data_profile       &  ! Canopy layer vapor pressure FROM DATASET (Pa)
    )

    ! Hardwired for one patch

    p = 1

    ! If first time step, read profile file to find number of vertical layers.
    ! Number of levels is found from records that have the same calendar day.

    if (itim == 1) then
       nrec = 0
       do
          read (nin1,'(f10.4,26f10.3)',end=100) curr_calday_data, zs_data, (x(i),i=1,22), &
          wind, tair, qair
          if (nrec == 0) check = curr_calday_data
          if (curr_calday_data == check) then
             nrec = nrec + 1
          else
             exit
          end if
       end do
100    ncan(p) = nrec
       rewind (nin1)
    end if

    ! Read profile data for the current time slice

    do ic = ncan(p), 1, -1
       read (nin1,'(f10.4,26f10.3)') curr_calday_data, zs_data, (x(i),i=1,22), &
       wind, tair, qair
       qair = qair / 1000._r8  ! g/kg -> kg/kg

       ! Error checks

       err = curr_calday_data - curr_calday
       if (abs(err) >= 1.e-04_r8) then
          call endrun (msg=' ERROR: ReadCanopyProfiles: calendar error')
       end if

       if (itim > 1) then
          err = zs_data - zs(p,ic)
          if (abs(err) >= 1.e-03_r8) then
             call endrun (msg=' ERROR: ReadCanopyProfiles: height profile error')
          end if
       end if

       wind_data(p,ic) = wind
       tair_data(p,ic) = tair
       eair_data(p,ic) = qair * pref(p) / (mmh2o / mmdry + (1._r8 - mmh2o / mmdry) * qair)
    end do

    end associate
  end subroutine ReadCanopyProfiles

end module CLMml_driver

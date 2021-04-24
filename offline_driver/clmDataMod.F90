module clmDataMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Read CLM forcing data for tower site
  !
  ! !USES:
  use abortutils, only : handle_err
  use ColumnType, only : col
  use shr_kind_mod, only : r8 => shr_kind_r8
  use SoilStateType, only : soilstate_type
  use WaterStateType, only : waterstate_type
  use CanopyStateType, only : canopystate_type
  use SurfaceAlbedoType, only : surfalb_type
  use MLclm_varctl, only : clm_phys
  !
  ! !PUBLIC TYPES:
  implicit none
  include 'netcdf.inc'
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: clmData               ! Read CLM forcing data
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: readCLMdata          ! Read single-level variables from CLM netcdf history file
  private :: readCLMsoil          ! Read soil water and temperature from CLM netcdf history file
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine clmData (ncfilename, strt, begp, endp, begc, endc, &
  soilstate_inst, waterstate_inst, canopystate_inst, surfalb_inst)
    !
    ! !DESCRIPTION:
    ! Read variables from CLM netcdf history file
    !
    ! !USES:
    use clm_varcon, only : denh2o
    use clm_varpar, only : nlevgrnd, nlevsoi
    !
    ! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: ncfilename   ! CLM netcdf filename
    integer, intent(in) :: strt                  ! Current time slice of data to retrieve from CLM history file
    integer, intent(in) :: begp, endp            ! First and last patch
    integer, intent(in) :: begc, endc            ! First and last column
    type(soilstate_type), intent(in) :: soilstate_inst
    type(waterstate_type) , intent(out) :: waterstate_inst
    type(canopystate_type), intent(out) :: canopystate_inst
    type(surfalb_type), intent(out) :: surfalb_inst
    !
    ! !LOCAL VARIABLES:
    integer  :: p                                ! Patch index for CLM g/l/c/p hierarchy
    integer  :: c                                ! Column index for CLM g/l/c/p hierarchy
    integer  :: j                                ! Soil layer index

    real(r8) :: elai_loc(1,1,1)                  ! CLM: Leaf area index (m2/m2)
    real(r8) :: esai_loc(1,1,1)                  ! CLM: Stem area index (m2/m2)
    real(r8) :: coszen_loc(1,1,1)                ! CLM: Cosine solar zenith angle
    real(r8) :: h2osoi_clm45(1,1,nlevgrnd)       ! CLM: Volumetric soil moisture for CLM4.5 soil layers (m3/m3)
    real(r8) :: h2osoi_clm50(1,1,nlevsoi)        ! CLM: Volumetric soil moisture for CLM5.0 soil layers (m3/m3)
    !---------------------------------------------------------------------

    associate ( &
                                                      ! *** Input ***
    dz         => col%dz                         , &  ! CLM: Soil layer thickness (m)
    nbedrock   => col%nbedrock                   , &  ! CLM: Depth to bedrock index
    watsat     => soilstate_inst%watsat_col      , &  ! CLM: Soil layer volumetric water content at saturation (porosity)
                                                      ! *** Output ***
    elai       => canopystate_inst%elai_patch    , &  ! CLM: Leaf area index of canopy (m2/m2)
    esai       => canopystate_inst%esai_patch    , &  ! CLM: Stem area index of canopy (m2/m2)
    coszen     => surfalb_inst%coszen_col        , &  ! CLM: Cosine of solar zenith angle
    h2osoi_vol => waterstate_inst%h2osoi_vol_col , &  ! CLM: Soil layer volumetric water content (m3/m3)
    h2osoi_liq => waterstate_inst%h2osoi_liq_col , &  ! CLM: Soil layer liquid water (kg H2O/m2)
    h2osoi_ice => waterstate_inst%h2osoi_ice_col   &  ! CLM: Soil layer ice lens (kg H2O/m2)
    )

    ! Read CLM single-level data for current time step. CLM coszen is for the next
    ! model time step, so must read value at previous time step (i.e., use strt-1)
    ! to get correct coszen. The model calculates coszen, so this variable is not
    ! used (but is useful for checking that the model calendar is the same as in CLM).

    call readCLMdata (ncfilename, strt, elai_loc, esai_loc, coszen_loc)

    ! All patches (p) get these values

    do p = begp, endp
       elai(p) = elai_loc(1,1,1)
       esai(p) = esai_loc(1,1,1)
    end do

    ! All columns (c) get the same coszen

    do c = begc, endc
       coszen(c) = coszen_loc(1,1,1)
    end do

    ! Read CLM volumetric soil water for current time step

    call readCLMsoil (ncfilename, strt, h2osoi_clm45, h2osoi_clm50)

    ! All columns get this soil water

    do c = begc, endc

       if (clm_phys == 'CLM4_5') then
          do j = 1, nlevgrnd
             h2osoi_vol(c,j) = h2osoi_clm45(1,1,j)
          end do
       else if (clm_phys == 'CLM5_0') then
          do j = 1, nlevsoi
             h2osoi_vol(c,j) = h2osoi_clm50(1,1,j)
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
  end subroutine clmData

  !-----------------------------------------------------------------------
  subroutine readCLMdata (ncfilename, strt, elai_mod, esai_mod, coszen_mod)
    !
    ! !DESCRIPTION:
    ! Read single-level variables from CLM netcdf history file
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: ncfilename     ! netcdf filename
    integer,  intent(in)  :: strt                  ! Time slice of data to retrieve
    real(r8), intent(out) :: elai_mod(1,1,1)       ! Leaf area index (m2/m2)
    real(r8), intent(out) :: esai_mod(1,1,1)       ! Stem area index (m2/m2)
    real(r8), intent(out) :: coszen_mod(1,1,1)     ! Cosine solar zenith angle
    !
    ! !LOCAL VARIABLES:
    integer :: ncid                                ! netcdf file ID
    integer :: status                              ! Function return status
    integer :: varid                               ! netcdf variable id
    integer :: start2(2), count2(2)                ! Start and count arrays for reading 2-D data from netcdf files
    !-----------------------------------------------------------------------

    ! Open file

    status = nf_open(ncfilename, nf_nowrite, ncid)
    if (status /= nf_noerr) call handle_err(status, ncfilename)

    ! Dimensions in FORTRAN are in column major order: the first array index varies the most rapidly.
    ! In NetCDF file the dimensions appear in the opposite order: lat, lon (2-D); time, lat, lon (3-D);
    ! time, levgrnd, lat, lon (4-D)
 
    start2 = (/ 1, strt /)
    count2 = (/ 1, 1 /)

    ! elai(nlndgrid, ntime): leaf area index (m2/m2)

    status = nf_inq_varid(ncid, "ELAI", varid)
    if (status /= nf_noerr) call handle_err(status, "ELAI")

    status = nf_get_vara_double(ncid, varid, start2, count2, elai_mod)
    if (status /= nf_noerr) call handle_err(status, "elai_mod")

    ! esai(nlndgrid, ntime): stem area index (m2/m2)

    status = nf_inq_varid(ncid, "ESAI", varid)
    if (status /= nf_noerr) call handle_err(status, "ESAI")

    status = nf_get_vara_double(ncid, varid, start2, count2, esai_mod)
    if (status /= nf_noerr) call handle_err(status, "esai_mod")

    ! coszen(nlndgrid, ntime): cosine of solar zenith angle

    status = nf_inq_varid(ncid, "COSZEN", varid)
    if (status /= nf_noerr) call handle_err(status, "COSZEN")

    start2 = (/ 1, strt-1 /)
    status = nf_get_vara_double(ncid, varid, start2, count2, coszen_mod)
    if (status /= nf_noerr) call handle_err(status, "coszen_mod")

    ! Close file

    status = nf_close(ncid)

  end subroutine readCLMdata

  !-----------------------------------------------------------------------
  subroutine readCLMsoil (ncfilename, strt, h2osoi_clm45, h2osoi_clm50)
    !
    ! !DESCRIPTION:
    ! Read volumetric soil water from CLM netcdf history file
    !
    ! !USES:
    use clm_varpar, only : nlevgrnd, nlevsoi
    use clm_varcon, only : spval
    !
    ! !ARGUMENTS:
    implicit none
    character(len=*), intent(in) :: ncfilename          ! netcdf filename
    integer, intent(in) :: strt                         ! Time slice of data to retrieve
    real(r8), intent(out) :: h2osoi_clm45(1,1,nlevgrnd) ! Volumetric soil moisture for CLM4.5 soil layers (m3/m3)
    real(r8), intent(out) :: h2osoi_clm50(1,1,nlevsoi)  ! Volumetric soil moisture for CLM5.0 soil layers (m3/m3)
    !
    ! !LOCAL VARIABLES:
    integer  :: ncid                             ! netcdf file ID
    integer  :: status                           ! Function return status
    integer  :: varid                            ! netcdf variable id
    integer  :: start3(3), count3(3)             ! Start and count arrays for reading 3-D data from netcdf files
    !---------------------------------------------------------------------

    ! Initialize arrays to special value

    h2osoi_clm45(:,:,:) = spval
    h2osoi_clm50(:,:,:) = spval

    ! Open file

    status = nf_open(ncfilename, nf_nowrite, ncid)
    if (status /= nf_noerr) call handle_err(status, ncfilename)

    ! Dimensions in FORTRAN are in column major order: the first array index varies the most rapidly.
    ! In NetCDF file the dimensions appear in the opposite order: lat, lon (2-D); time, lat, lon (3-D);
    ! time, levgrnd, lat, lon (4-D)

    start3 = (/ 1,  1, strt /)

    if (clm_phys == 'CLM4_5') then

       ! Read h2osoi_clm45(nlndgrid, nlevgrnd, ntime): volumetric soil water

       count3 = (/ 1, nlevgrnd, 1 /)

       status = nf_inq_varid(ncid, "H2OSOI", varid)
       if (status /= nf_noerr) call handle_err(status, "H2OSOI")

       status = nf_get_vara_double(ncid, varid, start3, count3, h2osoi_clm45)
       if (status /= nf_noerr) call handle_err(status, "h2osoi_clm45")

    else if (clm_phys == 'CLM5_0') then

       ! Read h2osoi_clm50(nlndgrid, nlevsoi, ntime): volumetic soil water

       count3 = (/ 1, nlevsoi, 1 /)

       status = nf_inq_varid(ncid, "H2OSOI", varid)
       if (status /= nf_noerr) call handle_err(status, "H2OSOI")

       status = nf_get_vara_double(ncid, varid, start3, count3, h2osoi_clm50)
       if (status /= nf_noerr) call handle_err(status, "h2osoi_clm50")

    end if

    ! Close file

    status = nf_close(ncid)

  end subroutine readCLMsoil

end module clmDataMod

module ncdio_pio

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Generic interfaces to write fields to netcdf files for CLM
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils, only : handle_err
  !
  ! !PUBLIC TYPES:
  implicit none
  include 'netcdf.inc'
  private
  save
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !
  public :: ncd_pio_openfile   ! open a file
  public :: ncd_pio_closefile  ! close a file
  public :: ncd_inqdid         ! inquire dimension id
  public :: ncd_inqdlen        ! inquire dimension length
  public :: ncd_defvar         ! define variables
  public :: ncd_inqvdlen       ! inquire variable dimension size
  public :: ncd_io             ! netcdf I/O of a variable
  public :: ncd_io_1d, ncd_io_2d

  integer, public :: ncd_double
  integer, public :: ncd_int

  interface ncd_io
     module procedure ncd_io_1d
     module procedure ncd_io_2d
  end interface

  type, public :: file_desc_t
     integer :: ncid
  end type file_desc_t
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine ncd_pio_openfile(ncid, fname, mode)
    !
    ! !DESCRIPTION:
    ! Open a NetCDF PIO file
    !
    ! !ARGUMENTS:
    type(file_desc_t), intent(inout) :: ncid  ! netcdf file id
    character(len=*), intent(in) :: fname     ! input filename to open
    integer, intent(in) :: mode               ! file mode
    !
    ! !LOCAL VARIABLES:
    integer :: status                         ! function return status
    !-----------------------------------------------------------------------

    status = nf_open(fname, nf_nowrite, ncid)
    if (status /= nf_noerr) call handle_err(status, fname)

  end subroutine ncd_pio_openfile

  !-----------------------------------------------------------------------
  subroutine ncd_pio_closefile(ncid)
    !
    ! !DESCRIPTION:
    ! Close a NetCDF PIO file
    !
    ! !ARGUMENTS:
    type(file_desc_t), intent(inout) :: ncid  ! netcdf file id
    !
    ! !LOCAL VARIABLES:
    integer :: status                         ! function return status
    !-----------------------------------------------------------------------

    status = nf_close(ncid)
    if (status /= nf_noerr) call handle_err(status, "ncd_pio_closefile")

  end subroutine ncd_pio_closefile

  !-----------------------------------------------------------------------
  subroutine ncd_inqdid(ncid, name, dimid)
    !
    ! !DESCRIPTION:
    ! Inquire on a dimension id
    !
    ! !ARGUMENTS:
    type(file_desc_t), intent(inout) :: ncid  ! netcdf file id
    character(len=*), intent(in) :: name      ! dimension name
    integer, intent(out):: dimid              ! dimension id
    !
    ! !LOCAL VARIABLES:
    integer :: status                         ! function return status
    !-----------------------------------------------------------------------

    status = nf_inq_dimid(ncid,name,dimid)
    if (status /= nf_noerr) call handle_err(status, name)

  end subroutine ncd_inqdid

  !-----------------------------------------------------------------------
  subroutine ncd_inqdlen(ncid, dimid, dimlen)
    !
    ! !DESCRIPTION:
    ! Gets the length of the given dimension
    !
    ! !ARGUMENTS:
    type(file_desc_t), intent(inout) :: ncid  ! netcdf file id
    integer, intent(in) :: dimid              ! dimension id
    integer, intent(out) :: dimlen            ! dimension len
    !
    ! !LOCAL VARIABLES:
    integer :: status                         ! function return status
    !-----------------------------------------------------------------------

    status = nf_inq_dimlen(ncid,dimid,dimlen)
    if (status /= nf_noerr) call handle_err(status,"ncd_inqdlen")

  end subroutine ncd_inqdlen

  !-----------------------------------------------------------------------
  subroutine ncd_defvar
    !
    ! !DESCRIPTION:
    ! Dummy routine
    !-----------------------------------------------------------------------

  end subroutine ncd_defvar

  !-----------------------------------------------------------------------
  subroutine ncd_inqvdlen
    !
    ! !DESCRIPTION:
    ! Dummy routine
    !-----------------------------------------------------------------------

  end subroutine ncd_inqvdlen

  !-----------------------------------------------------------------------
  subroutine ncd_io_1d(varname, data, flag, ncid, readvar, nt, posNOTonfile)
    !
    ! !DESCRIPTION:
    ! netcdf I/O of a variable
    !
    ! !ARGUMENTS:
    character(len=*), intent(in) :: varname       ! variable name
    real(r8), intent(inout) :: data(:)            ! raw data
    character(len=*), intent(in) :: flag          ! 'read' or 'write'
    type(file_desc_t), intent(inout) :: ncid      ! netcdf file id
    logical, optional, intent(out) :: readvar     ! was var read?
    integer, optional, intent(in) :: nt           ! time sample index
    logical, optional, intent(in) :: posNOTonfile ! position is NOT on this file
    !
    ! !LOCAL VARIABLES:
    integer :: status                             ! function return status
    integer :: varid                              ! netcdf variable id
    !-----------------------------------------------------------------------

    status = nf_inq_varid(ncid,varname,varid)
    if (status /= nf_noerr) call handle_err(status, varname)

    status = nf_get_var_double(ncid,varid,data)
    if (status == nf_noerr) then
       readvar = .true.
    else
       readvar = .false.
       call handle_err(status, varname)
    end if

  end subroutine ncd_io_1d

  !-----------------------------------------------------------------------
  subroutine ncd_io_2d(varname, data, flag, ncid, readvar, nt, posNOTonfile)
    !
    ! !DESCRIPTION:
    ! netcdf I/O of a variable
    !
    ! !ARGUMENTS:
    character(len=*), intent(in) :: varname       ! variable name
    real(r8), intent(inout) :: data(:,:)          ! raw data
    character(len=*), intent(in) :: flag          ! 'read' or 'write'
    type(file_desc_t), intent(inout) :: ncid      ! netcdf file id
    logical, optional, intent(out) :: readvar     ! was var read?
    integer, optional, intent(in) :: nt           ! time sample index
    logical, optional, intent(in) :: posNOTonfile ! position is NOT on this file
    !
    ! !LOCAL VARIABLES:
    integer :: status                             ! function return status
    integer :: varid                              ! netcdf variable id
    !-----------------------------------------------------------------------

    status = nf_inq_varid(ncid,varname,varid)
    if (status /= nf_noerr) call handle_err(status, varname)

    status = nf_get_var_double(ncid,varid,data)
    if (status == nf_noerr) then
       readvar = .true.
    else
       readvar = .false.
       call handle_err(status, varname)
    end if

  end subroutine ncd_io_2d

end module ncdio_pio

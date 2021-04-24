module restUtilMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Provides generic routines and types for use with restart files
  !
  ! !USES:
  use shr_kind_mod  , only : r8 => shr_kind_r8
  !
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: restartvar

  interface restartvar
     module procedure restartvar_1d
     module procedure restartvar_2d
  end interface
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine restartvar_1d (ncid, flag, varname, xtype, dim1name, dim2name, switchdim, &
  long_name, units, interpinic_flag, data, readvar, &
  comment, flag_meanings, missing_value, fill_value, &
  imissing_value, ifill_value, flag_values, nvalid_range )
    !
    ! !USES:
    use ncdio_pio, only : file_desc_t
    !
    ! Arguments
    type(file_desc_t) , intent(inout)        :: ncid             ! netcdf file id
    character(len=*)  , intent(in)           :: flag             ! 'read' or 'write'
    character(len=*)  , intent(in)           :: varname          ! variable name (or colon-delimited list: see above)
    integer           , intent(in)           :: xtype            ! netcdf data type
    character(len=*)  , intent(in)           :: long_name        ! long name for variable
    character(len=*)  , intent(in)           :: interpinic_flag  ! interpolate variable using interpinic
    real(r8)          , intent(in)           :: data(:)
    logical           , intent(inout)        :: readvar          ! was var read?
    logical           , intent(in), optional :: switchdim
    character(len=*)  , intent(in), optional :: dim1name         ! dimension name
    character(len=*)  , intent(in), optional :: dim2name         ! dimension name
    character(len=*)  , intent(in), optional :: units            ! long name for variable
    character(len=*)  , intent(in), optional :: comment          ! attribute
    character(len=*)  , intent(in), optional :: flag_meanings(:) ! attribute
    real(r8)          , intent(in), optional :: missing_value    ! attribute for real
    real(r8)          , intent(in), optional :: fill_value       ! attribute for real
    integer           , intent(in), optional :: imissing_value   ! attribute for int
    integer           , intent(in), optional :: ifill_value      ! attribute for int
    integer           , intent(in), optional :: flag_values(:)   ! attribute for int
    integer           , intent(in), optional :: nvalid_range(2)  ! attribute for int
    !-----------------------------------------------------------------------

  end subroutine restartvar_1d

  !-----------------------------------------------------------------------
  subroutine restartvar_2d (ncid, flag, varname, xtype, dim1name, dim2name, switchdim, &
  long_name, units, interpinic_flag, data, readvar, &
  comment, flag_meanings, missing_value, fill_value, &
  imissing_value, ifill_value, flag_values, nvalid_range )
    !
    ! !USES:
    use ncdio_pio, only : file_desc_t
    !
    ! Arguments
    type(file_desc_t) , intent(inout)        :: ncid             ! netcdf file id
    character(len=*)  , intent(in)           :: flag             ! 'read' or 'write'
    character(len=*)  , intent(in)           :: varname          ! variable name (or colon-delimited list: see above)
    integer           , intent(in)           :: xtype            ! netcdf data type
    character(len=*)  , intent(in)           :: long_name        ! long name for variable
    character(len=*)  , intent(in)           :: interpinic_flag  ! interpolate variable using interpinic
    real(r8)          , intent(in)           :: data(:,:)
    logical           , intent(inout)        :: readvar          ! was var read?
    logical           , intent(in), optional :: switchdim
    character(len=*)  , intent(in), optional :: dim1name         ! dimension name
    character(len=*)  , intent(in), optional :: dim2name         ! dimension name
    character(len=*)  , intent(in), optional :: units            ! long name for variable
    character(len=*)  , intent(in), optional :: comment          ! attribute
    character(len=*)  , intent(in), optional :: flag_meanings(:) ! attribute
    real(r8)          , intent(in), optional :: missing_value    ! attribute for real
    real(r8)          , intent(in), optional :: fill_value       ! attribute for real
    integer           , intent(in), optional :: imissing_value   ! attribute for int
    integer           , intent(in), optional :: ifill_value      ! attribute for int
    integer           , intent(in), optional :: flag_values(:)   ! attribute for int
    integer           , intent(in), optional :: nvalid_range(2)  ! attribute for int
    !-----------------------------------------------------------------------

  end subroutine restartvar_2d

end module restUtilMod

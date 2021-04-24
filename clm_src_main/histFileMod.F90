module histFileMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing methods to for CLM history file handling
  !
  ! !USES:
  use shr_kind_mod   , only : r8 => shr_kind_r8
  use decompMod      , only : bounds_type
  !
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: hist_addfld1d        ! Add a 1d single-level field to the master field list
  public :: hist_addfld2d        ! Add a 2d multi-level field to the master field list
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine hist_addfld1d (fname, units, avgflag, long_name, type1d_out, &
                        ptr_gcell, ptr_lunit, ptr_col, ptr_patch, ptr_lnd, &
                        ptr_atm, p2c_scale_type, c2l_scale_type, &
                        l2g_scale_type, set_lake, set_nolake, set_urb, set_nourb, &
                        set_noglcmec, set_spec, default)
    !
    ! !DESCRIPTION:
    ! Initialize a single level history field. The pointer, ptrhist,
    ! is a pointer to the data type array that the history buffer will use.
    ! The value of type1d passed to masterlist\_add\_fld determines which of the
    ! 1d type of the output and the beginning and ending indices the history
    ! buffer field). Default history contents for given field on all tapes
    ! are set by calling [masterlist\_make\_active] for the appropriate tape.
    ! After the masterlist is built, routine [htapes\_build] is called for an
    ! initial or branch run to initialize the actual history tapes.
    !
    ! !ARGUMENTS:
    character(len=*), intent(in)           :: fname          ! field name
    character(len=*), intent(in)           :: units          ! units of field
    character(len=*), intent(in)           :: avgflag        ! time averaging flag
    character(len=*), intent(in)           :: long_name      ! long name of field
    character(len=*), optional, intent(in) :: type1d_out     ! output type (from data type)
    real(r8)        , optional, pointer    :: ptr_gcell(:)   ! pointer to gridcell array
    real(r8)        , optional, pointer    :: ptr_lunit(:)   ! pointer to landunit array
    real(r8)        , optional, pointer    :: ptr_col(:)     ! pointer to column array
    real(r8)        , optional, pointer    :: ptr_patch(:)   ! pointer to patch array
    real(r8)        , optional, pointer    :: ptr_lnd(:)     ! pointer to lnd array
    real(r8)        , optional, pointer    :: ptr_atm(:)     ! pointer to atm array
    real(r8)        , optional, intent(in) :: set_lake       ! value to set lakes to
    real(r8)        , optional, intent(in) :: set_nolake     ! value to set non-lakes to
    real(r8)        , optional, intent(in) :: set_urb        ! value to set urban to
    real(r8)        , optional, intent(in) :: set_nourb      ! value to set non-urban to
    real(r8)        , optional, intent(in) :: set_noglcmec   ! value to set non-glacier_mec to
    real(r8)        , optional, intent(in) :: set_spec       ! value to set special to
    character(len=*), optional, intent(in) :: p2c_scale_type ! scale type for subgrid averaging of pfts to column
    character(len=*), optional, intent(in) :: c2l_scale_type ! scale type for subgrid averaging of columns to landunits
    character(len=*), optional, intent(in) :: l2g_scale_type ! scale type for subgrid averaging of landunits to gridcells
    character(len=*), optional, intent(in) :: default        ! if set to 'inactive, field will not appear on primary tape
    !-----------------------------------------------------------------------

  end subroutine hist_addfld1d

  !-----------------------------------------------------------------------
  subroutine hist_addfld2d (fname, type2d, units, avgflag, long_name, type1d_out, &
                        ptr_gcell, ptr_lunit, ptr_col, ptr_patch, ptr_lnd, ptr_atm, &
                        p2c_scale_type, c2l_scale_type, l2g_scale_type, &
                        set_lake, set_nolake, set_urb, set_nourb, set_spec, &
                        no_snow_behavior, default)
    !
    ! !DESCRIPTION:
    ! Initialize a single level history field. The pointer, ptrhist,
    ! is a pointer to the data type array that the history buffer will use.
    ! The value of type1d passed to masterlist\_add\_fld determines which of the
    ! 1d type of the output and the beginning and ending indices the history
    ! buffer field). Default history contents for given field on all tapes
    ! are set by calling [masterlist\_make\_active] for the appropriatae tape.
    ! After the masterlist is built, routine [htapes\_build] is called for an
    ! initial or branch run to initialize the actual history tapes.
    !
    ! !ARGUMENTS:
    character(len=*), intent(in) :: fname                      ! field name
    character(len=*), intent(in) :: type2d                     ! 2d output type
    character(len=*), intent(in) :: units                      ! units of field
    character(len=*), intent(in) :: avgflag                    ! time averaging flag
    character(len=*), intent(in) :: long_name                  ! long name of field
    character(len=*), optional, intent(in) :: type1d_out       ! output type (from data type)
    real(r8)        , optional, pointer    :: ptr_atm(:,:)     ! pointer to atm array
    real(r8)        , optional, pointer    :: ptr_lnd(:,:)     ! pointer to lnd array
    real(r8)        , optional, pointer    :: ptr_gcell(:,:)   ! pointer to gridcell array
    real(r8)        , optional, pointer    :: ptr_lunit(:,:)   ! pointer to landunit array
    real(r8)        , optional, pointer    :: ptr_col(:,:)     ! pointer to column array
    real(r8)        , optional, pointer    :: ptr_patch(:,:)     ! pointer to patch array
    real(r8)        , optional, intent(in) :: set_lake         ! value to set lakes to
    real(r8)        , optional, intent(in) :: set_nolake       ! value to set non-lakes to
    real(r8)        , optional, intent(in) :: set_urb          ! value to set urban to
    real(r8)        , optional, intent(in) :: set_nourb        ! value to set non-urban to
    real(r8)        , optional, intent(in) :: set_spec         ! value to set special to
    integer         , optional, intent(in) :: no_snow_behavior ! if a multi-layer snow field do special stuff
    character(len=*), optional, intent(in) :: p2c_scale_type   ! scale type for subgrid averaging of pfts to column
    character(len=*), optional, intent(in) :: c2l_scale_type   ! scale type for subgrid averaging of columns to landunits
    character(len=*), optional, intent(in) :: l2g_scale_type   ! scale type for subgrid averaging of landunits to gridcells
    character(len=*), optional, intent(in) :: default          ! if set to 'inactive, field will not appear on primary tape
    !-----------------------------------------------------------------------

  end subroutine hist_addfld2d

end module histFileMod

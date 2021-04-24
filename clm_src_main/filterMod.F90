module filterMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module of filters used for processing CLM columns and patches
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon, only : ispval, nan => spval
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: clumpfilter

    integer :: num_exposedvegp                ! number of patches in exposedvegp filter
    integer, pointer :: exposedvegp(:)        ! patches where frac_veg_nosno is non-zero

    integer :: num_nolakeurbanp               ! number of patches in non-lake, non-urban filter
    integer, pointer :: nolakeurbanp(:)       ! non-lake, non-urban filter (patches)

    integer :: num_nolakec                    ! number of columns in non-lake filter
    integer, pointer :: nolakec(:)            ! non-lake filter (columns)

    integer :: num_nourbanc                   ! number of columns in non-urban filter
    integer, pointer :: nourbanc(:)           ! non-urban filter (columns)

    integer :: num_hydrologyc                 ! number of columns in hydrology filter
    integer, pointer :: hydrologyc(:)         ! hydrology filter (columns)

  end type clumpfilter
  type(clumpfilter), public, target :: filter

  ! !PUBLIC MEMBER FUNCTIONS:
  public :: allocFilters                      ! Initialize data structure
  public :: setFilters                        ! Set filters
  public :: setExposedvegpFilter              ! Set the exposedvegp filter
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine allocFilters (filter, begp, endp, begc, endc)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    type(clumpfilter), intent(inout) :: filter
    integer, intent(in) :: begp, endp            ! Patch indices
    integer, intent(in) :: begc, endc            ! Column indices
    !---------------------------------------------------------------------

    allocate (filter%exposedvegp  (begp:endp))
    allocate (filter%nolakeurbanp (begp:endp))
    allocate (filter%nolakec      (begc:endc))
    allocate (filter%nourbanc     (begc:endc))
    allocate (filter%hydrologyc   (begc:endc))

  end subroutine allocFilters

  !-----------------------------------------------------------------------
  subroutine setFilters (filter)
    !
    ! !DESCRIPTION:
    ! Set CLM filters
    !
    ! !ARGUMENTS:
    type(clumpfilter), intent(inout) :: filter
    !---------------------------------------------------------------------

    filter%num_nolakeurbanp = 1 ; filter%nolakeurbanp(:) = 1
    filter%num_nolakec      = 1 ; filter%nolakec(:)      = 1
    filter%num_nourbanc     = 1 ; filter%nourbanc(:)     = 1
    filter%num_hydrologyc   = 1 ; filter%hydrologyc(:)   = 1

  end subroutine setFilters

  !-----------------------------------------------------------------------
  subroutine setExposedvegpFilter (filter, frac_veg_nosno)
    !
    ! !DESCRIPTION:
    ! Set the exposedvegp patch filter. This filter includes patches for
    ! which frac_veg_nosno > 0. It does not include urban or lake points.
    !
    ! !ARGUMENTS:
    type(clumpfilter), intent(inout) :: filter
    integer, intent(in) :: frac_veg_nosno(:)   ! Fraction of vegetation not covered by snow
    !
    ! !LOCAL VARIABLES:
    integer :: fp       ! Filter index
    integer :: p        ! Patch index
    integer :: fe       ! Filter count
    !---------------------------------------------------------------------

    fe = 0
    do fp = 1, filter%num_nolakeurbanp
       p = filter%nolakeurbanp(fp)
       if (frac_veg_nosno(p) > 0) then
          fe = fe + 1
          filter%exposedvegp(fe) = p
       end if
    end do
    filter%num_exposedvegp = fe

  end subroutine setExposedvegpFilter

end module filterMod

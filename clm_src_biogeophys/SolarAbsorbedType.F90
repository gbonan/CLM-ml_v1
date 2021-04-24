module SolarAbsorbedType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Solar absorbed variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon, only : ispval, nan => spval
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: solarabs_type

    real(r8), pointer :: fsa_patch (:) ! patch solar radiation absorbed (total) (W/m2)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type solarabs_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds

    call this%InitAllocate (bounds)

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine InitAllocate (this, bounds)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(solarabs_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp

    allocate (this%fsa_patch (begp:endp)) ; this%fsa_patch (:) = nan

  end subroutine InitAllocate

end module SolarAbsorbedType

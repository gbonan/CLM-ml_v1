module GridcellType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Gridcell data type
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varcon, only : nan => spval
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:
  type, public :: gridcell_type

    real(r8), pointer :: latdeg (:)   ! latitude (degrees)
    real(r8), pointer :: londeg (:)   ! longitude (degrees)

  contains

    procedure, public  :: Init

  end type gridcell_type
  type(gridcell_type), public, target :: grc
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, begg, endg)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(gridcell_type) :: this
    integer, intent(in)  :: begg, endg    ! Grid cell indices
    !---------------------------------------------------------------------

    allocate(this%latdeg (begg:endg)) ; this%latdeg (:) = nan
    allocate(this%londeg (begg:endg)) ; this%londeg (:) = nan

  end subroutine Init

end module GridcellType

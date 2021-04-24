module shr_file_mod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module to handle various file utilily functions
  !
  ! !USES:
  use shr_kind_mod
  !
  ! !PUBLIC TYPES:
  implicit none
  private
  integer(SHR_KIND_IN), parameter :: shr_file_minUnit = 10  ! Min unit number to give
  integer(SHR_KIND_IN), parameter :: shr_file_maxUnit = 99  ! Max unit number to give
  logical, save :: UnitTag(0:shr_file_maxUnit) = .false.    ! Logical units in use
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: shr_file_getUnit      ! Get a logical unit for reading or writing
  public :: shr_file_freeUnit     ! Free a logical unit
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  integer function shr_file_getUnit (unit)
    !
    ! !DESCRIPTION:
    ! Get the next free FORTRAN unit number
    !
    ! !USES:
    use clm_varctl, only : iulog
    use abortutils, only : endrun
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in), optional :: unit         ! Desired unit number
    !
    ! !LOCAL VARIABLES:
    integer :: n                                  ! Loop index
    logical :: opened                             ! If unit opened or not
    !---------------------------------------------------------------------

    if (present(unit)) then

       ! Use specified unit number

       inquire(unit, opened=opened)
       if (unit <= 0 .or. unit > shr_file_maxUnit) then
          write (iulog,*) 'invalid unit number request: ', unit
          call endrun()
       else if (opened .or. UnitTag(unit) .or. unit==5 .or. unit==6) then
          write (iulog,*) 'unit number ', unit, ' is already in use'
       else
          shr_file_getUnit = unit
          UnitTag(unit) = .true.
          return
       end if

    else

       ! Choose first available unit other than 0, 5, or 6

       do n = shr_file_maxUnit, shr_file_minUnit, -1
          inquire(n, opened=opened)
          if (n == 5 .or. n == 6 .or. opened) then
             cycle
          end if
          if (.not. UnitTag(n)) then
             shr_file_getUnit = n
             UnitTag(n) = .true.
             return
          end if
       end do
    end if

    write (iulog,*) 'Error: no available units found'

  end function shr_file_getUnit

  !-----------------------------------------------------------------------
  subroutine shr_file_freeUnit (unit)
    !
    ! !DESCRIPTION:
    ! Free up the given unit number
    !
    ! !USES:
    use clm_varctl, only : iulog
    use abortutils, only : endrun
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: unit      ! Unit number to be freed
    !---------------------------------------------------------------------

    if (unit < 0 .or. unit > shr_file_maxUnit) then
       write (iulog,*) 'invalid unit number request: ', unit
       call endrun()
    else if (unit == 0 .or. unit == 5 .or. unit == 6) then
       write (iulog,*) 'Error: units 0, 5, and 6 must not be freed'
       call endrun()
    else if (UnitTag(unit)) then
       UnitTag(unit) = .false.
    else
       write (iulog,*) 'unit ', unit, ' was not in use'
    end if

    return

  end subroutine shr_file_freeUnit

end module shr_file_mod

module abortutils

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module to abort the model for abnormal termination
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varctl, only : iulog
  !
  ! !PUBLIC TYPES:
  implicit none
  include 'netcdf.inc'
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: endrun
  public :: handle_err

contains

  subroutine endrun (msg)
    implicit none
    character(len=*), intent(in), optional :: msg    ! string to be printed

    if (present (msg)) then
       write (iulog,*) 'ENDRUN: ', msg
    else
       write (iulog,*) 'ENDRUN: called without a message string'
    end if

    stop

  end subroutine endrun

  subroutine handle_err (status, errmsg)
    integer, intent(in) :: status
    character(len=*), intent(in)    :: errmsg  ! append error message

    if (status /= nf_noerr) then
       print *, trim(nf_strerror(status)), ": ", errmsg
       stop "Stopped"
    endif
  end subroutine handle_err

end module abortutils

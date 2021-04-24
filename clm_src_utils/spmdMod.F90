module spmdMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! SPMD initialization
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  !
  ! !PUBLIC TYPES:
  implicit none
  save

  logical, parameter :: masterproc = .true.  ! proc 0 logical for printing msgs

end module spmdMod

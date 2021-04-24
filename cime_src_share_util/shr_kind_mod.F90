module shr_kind_mod

  !-----------------------------------------------------------------------
  ! precision/kind constants
  !-----------------------------------------------------------------------

  public
  integer, parameter :: shr_kind_r8 = selected_real_kind(12) ! 8 byte real
  integer, parameter :: SHR_KIND_IN = kind(1)                ! native integer

end module shr_kind_mod

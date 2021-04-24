module ColumnType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Column data type
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : nlevgrnd, nlevsno
  use clm_varcon, only : ispval, nan => spval
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:
  type, public :: column_type

    integer,  pointer :: snl      (:)     ! Number of snow layers
    real(r8), pointer :: dz       (:,:)   ! Soil layer thickness (m) [for -nlevsno+1:nlevgrnd layers]
    real(r8), pointer :: z        (:,:)   ! Soil layer depth (m) [for -nlevsno+1:nlevgrnd layers]
    real(r8), pointer :: zi       (:,:)   ! Soil layer depth at layer interface (m) [for -nlevsno:nlevgrnd layers]
    integer,  pointer :: nbedrock (:)     ! Variable depth to bedrock index

  contains

    procedure, public  :: Init

  end type column_type
  type(column_type), public, target :: col
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, begc, endc)
    !
    ! !DESCRIPTION:
    ! Initialize module data structure
    !
    ! !ARGUMENTS:
    class(column_type) :: this
    integer, intent(in) :: begc, endc     ! Column indices
    !---------------------------------------------------------------------

    allocate (this%snl      (begc:endc))                     ; this%snl      (:)   = ispval
    allocate (this%dz       (begc:endc,-nlevsno+1:nlevgrnd)) ; this%dz       (:,:) = nan
    allocate (this%z        (begc:endc,-nlevsno+1:nlevgrnd)) ; this%z        (:,:) = nan
    allocate (this%zi       (begc:endc,-nlevsno+0:nlevgrnd)) ; this%zi       (:,:) = nan
    allocate (this%nbedrock (begc:endc))                     ; this%nbedrock (:)   = ispval

  end subroutine Init

end module ColumnType

module SurfaceAlbedoType

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Surface albedo variables
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use clm_varpar, only : numrad
  use clm_varcon, only : ispval, nan => spval
  use decompMod, only : bounds_type
  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private
  !
  !PUBLIC DATA TYPES:

  type, public :: surfalb_type

    real(r8), pointer :: coszen_col (:)     ! col cosine of solar zenith angle
    real(r8), pointer :: albd_patch (:,:)   ! patch surface albedo (direct) (numrad)                    
    real(r8), pointer :: albi_patch (:,:)   ! patch surface albedo (diffuse) (numrad)
    real(r8), pointer :: albgrd_col (:,:)   ! col ground albedo (direct) (numrad)
    real(r8), pointer :: albgri_col (:,:)   ! col ground albedo (diffuse) (numrad)

  contains

    procedure, public  :: Init
    procedure, private :: InitAllocate

  end type surfalb_type
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine Init (this, bounds)

    class(surfalb_type) :: this
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
    class(surfalb_type) :: this
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer :: begp, endp   ! Patch indices
    integer :: begc, endc   ! Column indices
    !---------------------------------------------------------------------

    begp = bounds%begp ; endp = bounds%endp
    begc = bounds%begc ; endc = bounds%endc

    allocate (this%coszen_col (begc:endc))          ; this%coszen_col (:)   = nan
    allocate (this%albd_patch (begp:endp,1:numrad)) ; this%albd_patch (:,:) = nan
    allocate (this%albi_patch (begp:endp,1:numrad)) ; this%albi_patch (:,:) = nan
    allocate (this%albgrd_col (begc:endc,1:numrad)) ; this%albgrd_col (:,:) = nan
    allocate (this%albgri_col (begc:endc,1:numrad)) ; this%albgri_col (:,:) = nan

  end subroutine InitAllocate

end module SurfaceAlbedoType

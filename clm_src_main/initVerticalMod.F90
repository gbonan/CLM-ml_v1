module initVerticalMod

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Initialize vertical components of column datatype
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use decompMod, only : bounds_type
  use ColumnType, only : col
  use abortutils, only : endrun
  !
  ! !PUBLIC TYPES:
  implicit none
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: initVertical
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine initVertical (bounds)
    !
    ! !DESCRIPTION:
    ! Initialization
    !
    ! !USES:
    use clm_varpar, only : nlevsoi, nlevgrnd
    use clm_varcon, only: zmin_bedrock
    use MLclm_varctl, only : clm_phys
    use TowerDataMod, only : tower_num, tower_zbed
    !
    ! !ARGUMENTS:
    implicit none
    type(bounds_type), intent(in) :: bounds
    !
    ! !LOCAL VARIABLES:
    integer  :: c                     ! Column index for CLM g/l/c/p hierarchy
    integer  :: j                     ! Soil layer index
    integer  :: begc, endc            ! Column indices
    real(r8) :: scalez = 0.025_r8     ! Soil layer thickness discretization (m)
    integer  :: jmin_bedrock          ! Soil layer index for minimum soil depth
    real(r8) :: zbedrock              ! Depth to bedrock (m)
    !---------------------------------------------------------------------

    associate ( &
    dz        => col%dz          ,  & ! Soil layer thickness (m)
    z         => col%z           ,  & ! Soil layer depth (m)
    zi        => col%zi          ,  & ! Soil layer depth at layer interface (m)
    nbedrock  => col%nbedrock       & ! Depth to bedrock index
    )

    begc = bounds%begc ; endc= bounds%endc

    ! Define CLM layer structure for soil

    do c = begc, endc

       if (clm_phys == 'CLM4_5') then

          ! Layer depths

          do j = 1, nlevgrnd
             z(c,j) = scalez * (exp(0.5_r8*(j-0.5_r8)) - 1._r8)
          end do

          ! Layer thickness

          dz(c,1) = 0.5_r8 * (z(c,1) + z(c,2))
          do j = 2, nlevgrnd-1
             dz(c,j)= 0.5_r8 * (z(c,j+1) - z(c,j-1))
          end do
          dz(c,nlevgrnd) = z(c,nlevgrnd) - z(c,nlevgrnd-1)

          ! Interface depths

          zi(c,0) = 0._r8
          do j = 1, nlevgrnd-1
             zi(c,j) = 0.5_r8 * (z(c,j) + z(c,j+1))
          end do
          zi(c,nlevgrnd) = z(c,nlevgrnd) + 0.5_r8 * dz(c,nlevgrnd)

       else if (clm_phys == 'CLM5_0') then

          ! Layer thickness - soil layers

          do j = 1, 4
             dz(c,j)= j * 0.02_r8
          end do

          do j = 5, 13
             dz(c,j)= dz(c,4) + (j-4) * 0.04_r8
          end do

          do j = 14, nlevsoi
             dz(c,j)= dz(c,13) + (j-13) * 0.10_r8
          end do

          ! Layer thickness - bedrock layers

          do j = nlevsoi+1, nlevgrnd
             dz(c,j)= dz(c,nlevsoi) + (((j-nlevsoi)*25._r8)**1.5_r8)/100._r8
          end do

          ! Interface depths

          zi(c,0) = 0._r8
          do j = 1, nlevgrnd
             zi(c,j)= sum(dz(c,1:j))
          end do

          ! Layer depths

          do j = 1, nlevgrnd
             z(c,j) = 0.5 * (zi(c,j-1) + zi(c,j))
          end do

       else

          call endrun (msg=' ERROR: initVertical: clm_phys not valid')

       end if

    end do

    ! Set column bedrock index

    do c = begc, endc

       ! Get depth to bedrock for the tower site

       zbedrock = tower_zbed(tower_num)

       ! Determine minimum index of minimum soil depth

       jmin_bedrock = 3
       do j = 3, nlevsoi
          if (zi(c,j-1) < zmin_bedrock .and. zi(c,j) >= zmin_bedrock) then
             jmin_bedrock = j
          end if
       end do

       ! Now determine bedrock index

       nbedrock(c) = nlevsoi
       do j = jmin_bedrock, nlevsoi
          if (zi(c,j-1) < zbedrock .and. zi(c,j) >= zbedrock) then
             nbedrock(c) = j
          end if
       end do

    end do

    end associate
  end subroutine initVertical

end module initVerticalMod

module clm_time_manager

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module to mimic CLM time manager
  !
  ! DEFINITIONS
  !
  ! ---------------
  ! date - The term "date" is used to refer to an instant in time. It consists of
  ! year, month, day of month, and time of day components. The time of day is
  ! expressed in UTC. A date is represented by 2 integer values. One integer contains
  ! the calendar date (year, month and day of month), and the other contains the
  ! time of day (seconds past 0Z). 
  !
  ! The year, month, and day of month components are packed in an integer using the expression:
  ! yyyymmdd = year*10000 + month*100 + day, e.g., November 15, 2014 = 20141115
  !
  ! The time of day component of a date is represented as an integer number of
  ! seconds in the day
  !
  ! ---------------
  ! time - The term "time" is used in the sense of "simulation time" and expresses
  ! an elapsed time since a reference date.  Time is represented by 2 integer values.
  ! One integer contains the number of days and the other contains the partial day in seconds.
  !
  ! ---------------
  ! time of day - Time of day refers to the elapsed time since midnight of the
  ! current day. Time of day is expressed in UTC.
  !
  ! ---------------
  ! start date - The start date of a simulation is the date assigned to the initial conditions
  !
  ! ---------------
  ! current date - A simulation advances by timesteps. At any point during a simulation
  ! the current date is taken to be the date at the end of the current timestep.
  !
  ! ---------------
  ! current time - The current time of a simulation is the elapsed time from the
  ! start date to the current date.
  !
  ! ---------------
  ! calendar day - The day number in the calendar year. January 1 is calendar day 1. Calendar
  ! day may be expressed in a floating point format consisting of the integer day
  ! number plus the time of day (UTC) represented as a fractional day. For example
  ! assuming a Gregorian calendar:
  ! Date                Calendar day
  ! --------------------------------
  ! 10 Jan 2000, 6Z   ->   10.25
  ! 31 Dec 2000, 18Z  ->  366.75
  !
  ! !USES:
  use shr_kind_mod, only : r8 => shr_kind_r8
  use abortutils,   only : endrun
  use clm_varctl,   only : iulog
  !
  ! !PUBLIC TYPES:
  implicit none

  integer, public :: dtstep   ! Model time step (s)
  integer, public :: itim     ! Current model time step number

  integer  :: start_date_ymd  ! Year, month and day of the simulation start date in yyyymmdd format (e.g., 19960701)
  integer  :: start_date_tod  ! Time of day (UTC) of the simulation start date (seconds past 0Z)

  integer  :: curr_date_ymd   ! Year, month and day at the end of the current timestep in yyyymmdd format (e.g., 19960701)
  integer  :: curr_date_tod   ! Time of day (UTC) at the end of the current timestep (seconds past 0Z)

  ! Calendary type (NOLEAP or GREGORIAN)

  character(len=10), parameter :: calkindflag = 'GREGORIAN'

  ! Number of days in month (0-31) and cumulative day at end of month (0-365)

  integer, parameter :: mday(12)      = (/   31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31/)
  integer, parameter :: mdaycum(0:12) = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/)

  ! Same for leap year, (0-31) and (0-366)

  integer, parameter :: mdayleap(12) =      (/   31, 29, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31/)
  integer, parameter :: mdayleapcum(0:12) = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366/)

  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: get_step_size      ! Return step size in seconds
  public :: get_nstep          ! Return timestep number
  public :: isleap             ! Return true if a leap year
  public :: get_curr_date      ! Return date components at end of current timestep
  public :: get_curr_time      ! Return time components at end of current timestep
  public :: get_curr_calday    ! Return calendar day at end of current timestep
  public :: is_end_curr_day    ! Return true if current timestep is last timestep in current day
  public :: is_end_curr_month  ! Return true if current timestep is last timestep in current month
  !
  ! !PRIVATE MEMBER FUNCTIONS:
  private :: get_prev_date     ! Return date components at beginning of timestep
  private :: get_prev_calday   ! Return calendar day at beginning of timestep
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  integer function get_step_size()
    !
    ! !DESCRIPTION:
    ! Return the step size in seconds
    !---------------------------------------------------------------------

    get_step_size = dtstep

  end function get_step_size

  !-----------------------------------------------------------------------
  integer function get_nstep()
    !
    ! !DESCRIPTION:
    ! Return the timestep number
    !---------------------------------------------------------------------

    get_nstep = itim

  end function get_nstep

  !-----------------------------------------------------------------------
  logical function isleap (year, calendar)
    !
    ! !DESCRIPTION:
    ! Return true if a leap year
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: year                 ! Year (1900, ...)
    character(len=*), intent(in) :: calendar    ! Calendary type (NOLEAP or GREGORIAN)
    !---------------------------------------------------------------------

    isleap = .false.                            ! By default, February has 28 days ...

    if (trim(calendar) == 'GREGORIAN') then
       if (mod(year,4) == 0) then
          isleap = .true.                       ! But every four years, it has 29 days ...
          if (mod(year,100) == 0) then
             isleap = .false.                   ! Except every 100 years, when it has 28 days ...
             if (mod(year,400) == 0) then
                isleap = .true.                 ! Except every 400 years, when it has 29 days
             end if
          end if
       end if
    end if

end function isleap

  !-----------------------------------------------------------------------
  subroutine get_curr_date (yr, mon, day, tod)
    !
    ! !DESCRIPTION:
    ! Return date components valid at end of current timestep
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(out) :: yr               ! Year (1900, ...)
    integer, intent(out) :: mon              ! Month (1, ..., 12)
    integer, intent(out) :: day              ! Day of month (1, ..., 31)
    integer, intent(out) :: tod              ! Time of day (seconds past 0Z)
    !
    ! !LOCAL VARIABLES:
    integer :: nsecs                         ! Run time in seconds
    integer :: ndays                         ! Number of days since start
    integer :: nyears                        ! Number of years since start
    integer :: mcyear                        ! Current year
    integer :: mcmnth                        ! Current month
    integer :: mcday                         ! Current day
    integer :: days_per_month                ! Days in month
    !---------------------------------------------------------------------

    ! Current year

    mcyear = start_date_ymd / 10000

    ! Seconds and days since start
 
    nsecs = itim * dtstep                         ! Elapsed seconds
    ndays = (nsecs + start_date_tod) / 86400      ! Elapsed days
    if ( isleap(mcyear, calkindflag) ) then
       nyears = ndays / 366                       ! Elapsed years
    else
       nyears = ndays / 365                       ! Elapsed years
    end if
 
    ! Day of current year
    ! mod(A,P) computes the remainder of the division of A by P
 
    if ( isleap(mcyear, calkindflag) ) then
       ndays = mod(ndays,366)
    else
       ndays = mod(ndays,365)
    end if
 
    ! Current seconds of current date
 
    tod = mod(nsecs+start_date_tod,86400)
 
    ! Initialize current year, month, day

    mcyear = start_date_ymd / 10000 + nyears
    mcmnth = mod(start_date_ymd,10000) / 100
    mcday = mod(start_date_ymd,100) + ndays
 
    ! Now loop through months, converting yyyy, mm, and dd to yyyymmdd
    ! e.g., 19791235 becomes 19800104, 190001370 becomes 19010105
 
 10 continue
    if ( isleap(mcyear, calkindflag) ) then
       days_per_month = mdayleap(mcmnth)
    else
       days_per_month = mday(mcmnth)
    end if
    if (mcday > days_per_month) then
       mcday = mcday - days_per_month
       mcmnth = mcmnth + 1
       if (mcmnth == 13) then      ! add a year
          mcyear = mcyear + 1
          mcmnth = 1
       end if
       go to 10
    end if
    curr_date_ymd = mcyear*10000 + mcmnth*100 + mcday

    ! Extract year, month, day of current date 

    yr = curr_date_ymd / 10000
    mon = mod(curr_date_ymd,10000) / 100
    day = mod(curr_date_ymd,100)

  end subroutine get_curr_date

  !-----------------------------------------------------------------------
  subroutine get_prev_date (yr, mon, day, tod)
    !
    ! !DESCRIPTION:
    ! Return date components valid at beginning of timestep
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(out) :: yr               ! Year (1900, ...)
    integer, intent(out) :: mon              ! Month (1, ..., 12)
    integer, intent(out) :: day              ! Day of month (1, ..., 31)
    integer, intent(out) :: tod              ! Time of day (seconds past 0Z)
    !
    ! !LOCAL VARIABLES:
    integer :: nsecs                         ! Run time in seconds
    integer :: ndays                         ! Number of days since start
    integer :: nyears                        ! Number of years since start
    integer :: mcyear                        ! Current year
    integer :: mcmnth                        ! Current month
    integer :: mcday                         ! Current day
    integer :: days_per_month                ! Days in month
    integer :: date_ymd                      ! Year, month and day in yyyymmdd format (e.g., 19960701)
    !---------------------------------------------------------------------

    ! Year

    mcyear = start_date_ymd / 10000

    ! Seconds and days since start
 
    nsecs = (itim-1)* dtstep                      ! Elapsed seconds
    ndays = (nsecs + start_date_tod) / 86400      ! Elapsed days
    if ( isleap(mcyear, calkindflag) ) then
       nyears = ndays / 366                       ! Elapsed years
    else
       nyears = ndays / 365                       ! Elapsed years
    end if
 
    ! Day of year
    ! mod(A,P) computes the remainder of the division of A by P
 
    if ( isleap(mcyear, calkindflag) ) then
       ndays = mod(ndays,366)
    else
       ndays = mod(ndays,365)
    end if
 
    ! Seconds of date
 
    tod = mod(nsecs+start_date_tod,86400)
 
    ! Initialize year, month, day

    mcyear = start_date_ymd / 10000 + nyears
    mcmnth = mod(start_date_ymd,10000) / 100
    mcday = mod(start_date_ymd,100) + ndays
 
    ! Now loop through months, converting yyyy, mm, and dd to yyyymmdd
    ! e.g., 19791235 becomes 19800104, 190001370 becomes 19010105
 
 10 continue
    if ( isleap(mcyear, calkindflag) ) then
       days_per_month = mdayleap(mcmnth)
    else
       days_per_month = mday(mcmnth)
    end if
    if (mcday > days_per_month) then
       mcday = mcday - days_per_month
       mcmnth = mcmnth + 1
       if (mcmnth == 13) then      ! add a year
          mcyear = mcyear + 1
          mcmnth = 1
       end if
       go to 10
    end if
    date_ymd = mcyear*10000 + mcmnth*100 + mcday

    ! Extract year, month, day of date 

    yr = date_ymd / 10000
    mon = mod(date_ymd,10000) / 100
    day = mod(date_ymd,100)

  end subroutine get_prev_date

  !-----------------------------------------------------------------------
  subroutine get_curr_time (days, seconds)
    !
    ! !DESCRIPTION:
    ! Return the time components at the end of the current timestep. Current
    ! time is the time interval between the current date and the start date.
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(out) :: days    ! Number of whole days in time interval
    integer, intent(out) :: seconds ! Remaining seconds in the day
    !
    ! !LOCAL VARIABLES:
    integer  :: nsecs               ! Elapsed seconds
    !---------------------------------------------------------------------

    nsecs = itim * dtstep
    days = (nsecs + start_date_tod) / 86400
    seconds = mod(nsecs+start_date_tod, 86400)

  end subroutine get_curr_time

  !-----------------------------------------------------------------------
  function get_curr_calday(offset)
    !
    ! !DESCRIPTION:
    ! Return calendar day at end of current timestep with optional offset.
    ! Offset is positive for future times and negative for previous times.
    ! Calendar day 1.0 = 0Z on Jan 1.
    !
    ! Arguments
    integer, optional, intent(in) :: offset ! Offset from current time in seconds
    !
    ! !LOCAL VARIABLES:
    integer  :: yr                  ! Year (1900, ...)
    integer  :: mon                 ! Month (1, ..., 12)
    integer  :: day                 ! Day of month (1, ..., 31)
    integer  :: tod                 ! Time of day (seconds past 0Z)
    real(r8) :: calday              ! Calendar day
    real(r8) :: get_curr_calday     ! Return value
    !---------------------------------------------------------------------

    if (offset < 0) then

       ! Return calendar day at beginning of timestep

       calday = get_prev_calday()

    else if (offset > 0) then

       ! This option does not work

       write (iulog,*) 'get_curr_calday error: offset > 0'
       call endrun()

    else
 
       ! Current year, month, day of month, and time of day

       call get_curr_date (yr, mon, day, tod)

       ! Convert to day-of-year + fraction

       if ( isleap(yr, calkindflag) ) then
          calday = float(mdayleapcum(mon-1)) + float(day) + float(tod) / 86400._r8
       else
          calday = float(mdaycum(mon-1)) + float(day) + float(tod) / 86400._r8
       end if
 
       !----------------------------------------------------------------------------------------!
       !!!!!!!!!!!!!! WARNING HACK TO ENABLE Gregorian CALENDAR WITH SHR_ORB !!!!!!!!!!!!!!!!!!!!
       !!!! The following hack fakes day 366 by reusing day 365. This is just because the !!!!!!!
       !!!! current shr_orb_decl calculation can't handle days > 366.                     !!!!!!!
       !!!!       Dani Bundy-Coleman and Erik Kluzek Aug/2008                             !!!!!!!
       !----------------------------------------------------------------------------------------!

       if ( (calday > 366.) .and. (calday <= 367.) .and. (trim(calkindflag) == 'GREGORIAN') )then
          calday = calday - 1._r8
       end if

      !!!!!!!!!!!!!! END HACK TO ENABLE Gregorian CALENDAR WITH SHR_ORB
      !----------------------------------------------------------------------------------------!
 
       if (calday < 1. .or. calday > 366.) then
          write (iulog,*) 'get_curr_calday error: out of bounds'
          call endrun()
       end if

       ！get_curr_calday = calday

    end if
    
       get_curr_calday = calday
 
  end function get_curr_calday

  !-----------------------------------------------------------------------
  function get_prev_calday()
    !
    ! !DESCRIPTION:
    ! Return calendar day at beginning of timestep. Calendar day 1.0 = 0Z on Jan 1
    !
    ! !LOCAL VARIABLES:
    integer  :: yr                  ! Year (1900, ...)
    integer  :: mon                 ! Month (1, ..., 12)
    integer  :: day                 ! Day of month (1, ..., 31)
    integer  :: tod                 ! Time of day (seconds past 0Z)
    real(r8) :: calday              ! Calendar day
    real(r8) :: get_prev_calday     ! Return value
    !---------------------------------------------------------------------
 
    ! Year, month, day of month, and time of day

    call get_prev_date (yr, mon, day, tod)

    ! Convert to day-of-year + fraction

    if ( isleap(yr, calkindflag) ) then
       calday = float(mdayleapcum(mon-1)) + float(day) + float(tod) / 86400._r8
    else
       calday = float(mdaycum(mon-1)) + float(day) + float(tod) / 86400._r8
    end if
 
    !----------------------------------------------------------------------------------------!
    !!!!!!!!!!!!!! WARNING HACK TO ENABLE Gregorian CALENDAR WITH SHR_ORB !!!!!!!!!!!!!!!!!!!!
    !!!! The following hack fakes day 366 by reusing day 365. This is just because the !!!!!!!
    !!!! current shr_orb_decl calculation can't handle days > 366.                     !!!!!!!
    !!!!       Dani Bundy-Coleman and Erik Kluzek Aug/2008                             !!!!!!!
    !----------------------------------------------------------------------------------------!

    if ( (calday > 366.) .and. (calday <= 367.) .and. (trim(calkindflag) == 'GREGORIAN') )then
       calday = calday - 1._r8
    end if

   !!!!!!!!!!!!!! END HACK TO ENABLE Gregorian CALENDAR WITH SHR_ORB
   !----------------------------------------------------------------------------------------!
 
    if (calday < 1. .or. calday > 366.) then
       write (iulog,*) 'get_prev_calday error: out of bounds'
       call endrun()
    end if

    get_prev_calday = calday
 
  end function get_prev_calday

  !-----------------------------------------------------------------------
  logical function is_end_curr_day()
    !
    ! !DESCRIPTION:
    ! Return true if current timestep is last timestep in current day. The
    ! final timestep of a day is the one whose ending date is equal to or later
    ! than 0Z of the next day.
    !
    ! !LOCAL VARIABLES:
    integer :: yr               ! Year (1900, ...)
    integer :: mon              ! Month (1, ..., 12)
    integer :: day              ! Day of month (1, ..., 31)
    integer :: tod              ! Time of day (seconds past 0Z)
    !---------------------------------------------------------------------

    call get_curr_date (yr, mon, day, tod)
    is_end_curr_day = (tod == 0)

  end function is_end_curr_day

  !-----------------------------------------------------------------------
  logical function is_end_curr_month()
    !
    ! !DESCRIPTION:
    ! Return true if current timestep is last timestep in current month. The
    ! final timestep of a month is the one whose ending date is equal to or later
    ! than 0Z of the first day of the next month.
    !
    ! !LOCAL VARIABLES:
    integer :: yr               ! Year (1900, ...)
    integer :: mon              ! Month (1, ..., 12)
    integer :: day              ! Day of month (1, ..., 31)
    integer :: tod              ! Time of day (seconds past 0Z)
    !---------------------------------------------------------------------

    call get_curr_date (yr, mon, day, tod)
    is_end_curr_month = (day == 1  .and.  tod == 0)

  end function is_end_curr_month

end module clm_time_manager

!***********************************************************************
! Sets PRMS time variables
!***********************************************************************
      MODULE PRMS_SET_TIME
      IMPLICIT NONE
!   Local Variables
!      CHARACTER(LEN=10), SAVE :: MODNAME
      INTEGER, SAVE :: Modays(12), Yrdays, Summer_flag, Jday, Jsol, Julwater
      INTEGER, SAVE :: Nowtime(6), Nowday, Nowmonth, Nowyear, Nowhour, Nowminute
      REAL, SAVE :: Timestep_hours, Timestep_days, Timestep_minutes
      DOUBLE PRECISION, SAVE :: Cfs2inches, Cfs_conv, Timestep_seconds
      END MODULE PRMS_SET_TIME

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION prms_time()
      USE PRMS_SET_TIME
      USE PRMS_MODULE, ONLY: Process, Timestep, Starttime
      USE PRMS_BASIN, ONLY: Hemisphere, Basin_area_inv, FT2_PER_ACRE
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL
      INTEGER, EXTERNAL :: leap_day, julian_day
      DOUBLE PRECISION, EXTERNAL :: deltim
      EXTERNAL :: dattim, print_module
! Local Variables
      DOUBLE PRECISION :: dt
      CHARACTER(LEN=80), SAVE :: Version_prms_time
!***********************************************************************
      prms_time = 0

      IF ( Process(:3)=='run' .OR. Process(:4)=='init' ) THEN

        IF ( Process(:3)=='run' ) THEN
          Timestep = Timestep + 1

          CALL dattim('now', Nowtime)
          Jday = julian_day('now', 'calendar')
          Jsol = julian_day('now', 'solar')
          Julwater = julian_day('now', 'water')

        ELSE ! initialize
          Modays(1) = 31
          Modays(3) = 31
          Modays(4) = 30
          Modays(5) = 31
          Modays(6) = 30
          Modays(7) = 31
          Modays(8) = 31
          Modays(9) = 30
          Modays(10) = 31
          Modays(11) = 30
          Modays(12) = 31

          Nowtime = Starttime
          Jday = julian_day('start', 'calendar')
          Jsol = julian_day('start', 'solar')
          Julwater = julian_day('start', 'water')
        ENDIF

        Nowyear = Nowtime(1)
        Nowmonth = Nowtime(2)
        Nowday = Nowtime(3)
        Nowhour = Nowtime(4)
        Nowminute = Nowtime(5)

        IF ( leap_day(Nowyear)==1 ) THEN
          Yrdays = 366
          Modays(2) = 29
        ELSE
          Yrdays = 365
          Modays(2) = 28
        ENDIF

        ! Summer is based on equinox:
        !   Julian days 79 to 265 for Northern hemisphere
        !   Julian day 265 to 79 in Southern hemisphere
        Summer_flag = 1 ! 1 = summer, 0 = winter
        IF ( Hemisphere==0 ) THEN ! Northern Hemisphere
          IF ( Jday<79 .OR. Jday>265 ) Summer_flag = 0 ! Equinox
        ELSE ! Southern Hemisphere
          IF ( Jday>79 .AND. Jday<265 ) Summer_flag = 0 ! Equinox
        ENDIF

        dt = deltim()
        Timestep_hours = SNGL( dt )
        Timestep_days = Timestep_hours/24.0
        Timestep_minutes = Timestep_hours*60.0
        Timestep_seconds = dt*3600.0D0
        Cfs_conv = FT2_PER_ACRE/12.0D0/Timestep_seconds
        Cfs2inches = Basin_area_inv*12.0D0*Timestep_seconds/FT2_PER_ACRE

        ! Check to see if in a daily or subdaily time step
        IF ( Timestep_hours>24.0 ) THEN
          PRINT *, 'ERROR, timestep > daily, fix Data File, timestep:', Timestep_hours
          STOP
        ELSEIF ( Timestep_hours<24.0 ) THEN
          PRINT *, 'ERROR, timestep < daily for daily model, fix Data File', Timestep_hours
          STOP
        ENDIF

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_prms_time = 'prms_time.f90 2015-03-31 16:00:42Z'
        CALL print_module(Version_prms_time, 'PRMS Set Time Variables     ', 90)
!        MODNAME = 'prms_time'
        Timestep_seconds = 86400.0D0
        Cfs_conv = FT2_PER_ACRE/12.0D0/Timestep_seconds
        Cfs2inches = Basin_area_inv*12.0D0*Timestep_seconds/FT2_PER_ACRE
      ENDIF

      END FUNCTION prms_time

!***********************************************************************
! Writes climate data (tmin, tmax, precip, potential solar radiation,
! and/or potential evapotranspieration) by HRU to files for use by
! the climate_hru module
!***********************************************************************
      INTEGER FUNCTION write_climate_hru()
      USE PRMS_MODULE, ONLY: Process, Nhru, Climate_temp_flag, Climate_precip_flag, MAXFILE_LENGTH, &
     &    Climate_swrad_flag, Climate_potet_flag, Climate_transp_flag
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_CLIMATEVARS, ONLY: Tmaxf, Tminf, Hru_ppt, Potet, Swrad, Orad, Transp_on
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: control_string
      EXTERNAL :: read_error, PRMS_open_output_file, print_module
! Local Variables
      INTEGER, SAVE :: tmax_unit, tmin_unit, precip_unit, potet_unit, swrad_unit, transp_unit
      INTEGER :: i, ios, ierr
      CHARACTER(LEN=32), SAVE :: fmt1, fmt2, fmt3
!      CHARACTER(LEN=17), SAVE :: MODNAME
      CHARACTER(LEN=80), SAVE :: Version_write_climate_hru
! Control Parameters
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Tmin_day, Tmax_day, Precip_day, Potet_day, Swrad_day, Transp_day
!***********************************************************************
      write_climate_hru = 0

!***Run Procedure***
      IF ( Process(:3)=='run' ) THEN
        IF ( Climate_temp_flag/=1 ) THEN
          WRITE ( tmax_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tmaxf(i), i=1,Nhru)
          WRITE ( tmin_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Tminf(i), i=1,Nhru)
        ENDIF
        IF ( Climate_precip_flag/=1 ) WRITE ( precip_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Hru_ppt(i), i=1,Nhru)
        IF ( Climate_potet_flag/=1 ) WRITE ( potet_unit, fmt1 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Potet(i), i=1,Nhru)
        IF ( Climate_swrad_flag/=1 ) WRITE ( swrad_unit, fmt2 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Swrad(i), i=1,Nhru), Orad
        IF ( Climate_transp_flag/=1 ) WRITE ( transp_unit, fmt3 ) Nowyear, Nowmonth, Nowday, 0, 0, 0, (Transp_on(i), i=1,Nhru)

!***Declare Procedure***
      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_write_climate_hru = 'write_climate_hru.f90 2014-12-02 19:06:41Z'
        CALL print_module(Version_write_climate_hru, 'Preprocessing               ', 90)
!        MODNAME = 'write_climate_hru'

!***Initialize Procedure***
      ELSEIF ( Process(:4)=='init' ) THEN
        fmt1 = ' '
        fmt2 = ' '
        fmt2 = ' '
        WRITE ( fmt1, 9003 ) Nhru
        WRITE ( fmt2, 9003 ) Nhru + 1
        WRITE ( fmt3, 9004 ) Nhru
        ierr = 0
        IF ( Climate_temp_flag/=1 ) THEN
          IF ( control_string(Tmax_day, 'tmax_day')/=0 ) CALL read_error(5, 'tmax_day')
          CALL PRMS_open_output_file(tmax_unit, Tmax_day, 'tmax_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( tmax_unit, 9001 ) 'tmaxf', Nhru
          ENDIF

          IF ( control_string(Tmin_day, 'tmin_day')/=0 ) CALL read_error(5, 'tmin_day')
          CALL PRMS_open_output_file(tmin_unit, Tmin_day, 'tmin_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( tmin_unit, 9001 ) 'tminf', Nhru
          ENDIF
        ENDIF

        IF ( Climate_precip_flag/=1 ) THEN
          IF ( control_string(Precip_day, 'precip_day')/=0 ) CALL read_error(5, 'precip_day')
          CALL PRMS_open_output_file(precip_unit, Precip_day, 'precip_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( precip_unit, 9001 ) 'precip', Nhru
          ENDIF
        ENDIF

        IF ( Climate_potet_flag/=1 ) THEN
          IF ( control_string(Potet_day, 'potet_day')/=0 ) CALL read_error(5, 'potet_day')
          CALL PRMS_open_output_file(potet_unit, Potet_day, 'potet_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( potet_unit, 9001 ) 'potet', Nhru
          ENDIF
        ENDIF

        IF ( Climate_transp_flag/=1 ) THEN
          IF ( control_string(Transp_day, 'transp_day')/=0 ) CALL read_error(5, 'transp_day')
          CALL PRMS_open_output_file(transp_unit, Transp_day, 'transp_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( transp_unit, 9001 ) 'transp', Nhru
          ENDIF
        ENDIF

        IF ( Climate_swrad_flag/=1 ) THEN
          IF ( control_string(Swrad_day, 'swrad_day')/=0 ) CALL read_error(5, 'swrad_day')
          CALL PRMS_open_output_file(swrad_unit, Swrad_day, 'swrad_day', 0, ios)
          IF ( ios/=0 ) THEN
            ierr = 1
          ELSE
            WRITE ( swrad_unit, 9002 ) 'swrad', Nhru
            PRINT *, '******'
            PRINT *, '******WARNING****** BE SURE TO ADD orad_flag = 1 in Control File'
            PRINT *, '******'
          ENDIF
        ENDIF
        
        IF ( ierr==1 ) STOP

!***Clean-up Procedure***
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Climate_temp_flag/=1 ) THEN
          CLOSE ( tmax_unit )
          CLOSE ( tmin_unit )
        ENDIF
        IF ( Climate_precip_flag/=1 ) CLOSE ( precip_unit )
        IF ( Climate_potet_flag/=1 ) CLOSE ( potet_unit )
        IF ( Climate_swrad_flag/=1 ) CLOSE ( swrad_unit )
        IF ( Climate_transp_flag/=1 ) CLOSE ( transp_unit )

      ENDIF

 9001 FORMAT ( 'Generated for climate_hru module', /, A, I8, /, 40('#') )
 9002 FORMAT ( 'Generated for climate_hru module', /, A, I8, /, 'orad 1', /, 40('#') )
 9003 FORMAT ( '(I4,2I3,3I2,',I8,'E12.4)' )
 9004 FORMAT ( '(I4,2I3,3I2,',I8,'I3)' )

      END FUNCTION write_climate_hru

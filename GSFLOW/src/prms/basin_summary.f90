!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
      MODULE PRMS_BASIN_SUMMARY
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      character(len=*), parameter :: MODDESC = 'Output Summary'
      character(len=*), parameter :: MODNAME = 'basin_summary'
      character(len=*), parameter :: Version_basin_summary = '2024-09-01'
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear, Dailyunit, Monthlyunit, Yearlyunit, Basin_var_type
      INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2 !, Output_fmt3
      INTEGER, SAVE :: Daily_flag, Yeardays, Monthly_flag, Monthdays, save_year, save_month, save_day
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_daily(:), Basin_var_monthly(:), Basin_var_yearly(:)
! Parameters
      INTEGER, SAVE, ALLOCATABLE :: Nhm_id(:)
! Control Parameters
      INTEGER, SAVE :: BasinOutVars, BasinOut_freq
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: BasinOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: BasinOutBaseFileName
      END MODULE PRMS_BASIN_SUMMARY

!     ******************************************************************
!     Basin results module
!     ******************************************************************
      SUBROUTINE basin_summary()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, MEAN_MONTHLY
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_BASIN_SUMMARY, ONLY: Dailyunit, Monthlyunit, Yearlyunit, Daily_flag, Monthly_flag, BasinOut_freq
      IMPLICIT NONE
! Functions
      EXTERNAL :: basin_summarydecl, basin_summaryinit, basin_summaryrun
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        CALL basin_summaryrun()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL basin_summarydecl()
      ELSEIF ( Process_flag==INIT ) THEN
        CALL basin_summaryinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Daily_flag==ACTIVE ) CLOSE ( Dailyunit )
        IF ( BasinOut_freq>MEAN_MONTHLY ) CLOSE ( Yearlyunit )
        IF ( Monthly_flag==ACTIVE ) CLOSE ( Monthlyunit )
      ENDIF

      END SUBROUTINE basin_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE basin_summarydecl()
      USE PRMS_CONSTANTS, ONLY: DAILY, YEARLY, ERROR_control
      use PRMS_READ_PARAM_FILE, only: declparam
      use PRMS_CONTROL_FILE, only: control_string_array, control_integer, control_string
      USE PRMS_MODULE, ONLY: BasinOutON_OFF, Nhru
      USE PRMS_BASIN_SUMMARY
      use prms_utils, only: error_stop, print_module, read_error
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      CALL print_module(MODDESC, MODNAME, Version_basin_summary)

      IF ( control_integer(BasinOutVars, 'basinOutVars')/=0 ) BasinOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(BasinOut_freq, 'basinOut_freq')/=0 ) BasinOut_freq = 0
      IF ( BasinOut_freq<DAILY .OR. BasinOut_freq>YEARLY ) CALL error_stop('invalid basinOut_freq value', ERROR_control)

      IF ( BasinOutVars==0 ) THEN
        CALL error_stop('basin_summary requested with basinOutVars equal 0', ERROR_control)
      ELSE
        ALLOCATE ( BasinOutVar_names(BasinOutVars), Nc_vars(BasinOutVars) )
        BasinOutVar_names = ' '
        DO i = 1, BasinOutVars
          IF ( control_string_array(BasinOutVar_names(i), 'basinOutVar_names', i)/=0 ) CALL read_error(5, 'basinOutVar_names')
        ENDDO
        IF ( control_string(BasinOutBaseFileName, 'basinOutBaseFileName')/=0 ) CALL read_error(5, 'basinOutBaseFileName')
      ENDIF

! Declared Parameters
      IF ( BasinOutON_OFF==2 ) THEN
        ALLOCATE ( Nhm_id(Nhru) )
        IF ( declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &       '1', '1', '9999999', &
     &       'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &       'none') /= 0 ) CALL read_error(1, 'nhm_id')
      ENDIF

      END SUBROUTINE basin_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE basin_summaryinit()
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH, ACTIVE, OFF, DAILY_MONTHLY, MEAN_MONTHLY, MEAN_YEARLY, DAILY, MONTHLY, &
     &    DBLE_TYPE, ERROR_open_out, YEARLY
      use PRMS_MMFAPI, only: getvarsize, getvartype
      use PRMS_READ_PARAM_FILE, only: getparam_int
      USE PRMS_MODULE, ONLY: Start_year, Prms_warmup, BasinOutON_OFF, Nhru, Inputerror_flag
      USE PRMS_BASIN_SUMMARY
      use prms_utils, only: error_stop, numchars, PRMS_open_output_file, read_error
      IMPLICIT NONE
! Local Variables
      INTEGER :: ios, ierr, size, jj
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = ACTIVE
      IF ( Prms_warmup>0 ) Begin_results = OFF
      Begyr = Start_year + Prms_warmup
      Lastyear = Begyr

      WRITE ( Output_fmt, 9001 ) BasinOutVars

      ierr = 0
      DO jj = 1, BasinOutVars
        Nc_vars(jj) = numchars(BasinOutVar_names(jj))
        Basin_var_type = getvartype(BasinOutVar_names(jj)(:Nc_vars(jj)) )
        IF ( Basin_var_type/=DBLE_TYPE ) THEN
          PRINT *, 'ERROR, invalid basin_summary variable:', BasinOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(BasinOutVar_names(jj)(:Nc_vars(jj)))
        IF ( size/=1 ) THEN
          PRINT *, 'ERROR, invalid Basin_summary variable:', BasinOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only scalar variables are allowed'
          ierr = 1
        ENDIF
      ENDDO

      ALLOCATE ( Basin_var_daily(BasinOutVars) )
      Basin_var_daily = 0.0D0

      Daily_flag = OFF
      IF ( BasinOut_freq==DAILY .OR. BasinOut_freq==DAILY_MONTHLY ) Daily_flag = ACTIVE

      Monthly_flag = OFF
      IF ( BasinOut_freq==MONTHLY .OR. BasinOut_freq==DAILY_MONTHLY .OR. BasinOut_freq==MEAN_MONTHLY ) Monthly_flag = ACTIVE

      IF ( BasinOut_freq>MEAN_MONTHLY ) THEN
        Yeardays = 0
        ALLOCATE ( Basin_var_yearly(BasinOutVars) )
        Basin_var_yearly = 0.0D0
        !WRITE ( Output_fmt3, 9003 ) BasinOutVars
      ENDIF
      IF ( Monthly_flag==ACTIVE ) THEN
        Monthdays = 0
        ALLOCATE ( Basin_var_monthly(BasinOutVars) )
        Basin_var_monthly = 0.0D0
      ENDIF

      WRITE ( Output_fmt2, 9002 ) BasinOutVars

      IF ( BasinOutON_OFF==2 ) THEN
        IF ( getparam_int(MODNAME, 'nhm_id', Nhru, Nhm_id)/=0 ) CALL read_error(2, 'nhm_id')
      ENDIF

      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      IF ( Daily_flag==ACTIVE ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'.csv'
        CALL PRMS_open_output_file(Dailyunit, fileName, 'basin_summary, daily', 0, ios)
        IF ( ios/=0 ) CALL error_stop('in basin_summary, daily', ERROR_open_out)
        IF ( BasinOutON_OFF==2 ) WRITE ( Dailyunit, '(A, 1X, I0)') 'nhm_id:', Nhm_id(1)
        WRITE ( Dailyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ENDIF
      IF ( BasinOut_freq==MEAN_YEARLY ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_meanyearly.csv'
        CALL PRMS_open_output_file(Yearlyunit, fileName, 'basin_summary, mean yearly', 0, ios)
        IF ( ios/=0 ) CALL error_stop('in basin_summary, mean yearly', ERROR_open_out)
        IF ( BasinOutON_OFF==2 ) WRITE ( Yearlyunit, '(A, 1X, I0)') 'nhm_id:', Nhm_id(1)
        WRITE ( Yearlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ELSEIF ( BasinOut_freq==YEARLY ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_yearly.csv'
        CALL PRMS_open_output_file(Yearlyunit, fileName, 'basin_summary, yearly', 0, ios)
        IF ( ios/=0 ) CALL error_stop('in basin_summary, yearly', ERROR_open_out)
        IF ( BasinOutON_OFF==2 ) WRITE ( Yearlyunit, '(A, 1X, I0)') 'nhm_id:', Nhm_id(1)
        WRITE ( Yearlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ELSEIF ( Monthly_flag==1 ) THEN
        IF ( BasinOut_freq==MEAN_MONTHLY ) THEN
          fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_meanmonthly.csv'
        ELSE
          fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_monthly.csv'
        ENDIF
        CALL PRMS_open_output_file(Monthlyunit, fileName, 'basin_summary, monthly', 0, ios)
        IF ( ios/=0 ) CALL error_stop('in basin_summary, monthly', ERROR_open_out)
        IF ( BasinOutON_OFF==2 ) WRITE ( Monthlyunit, '(A, 1X, I0)') 'nhm_id:', Nhm_id(1)
        WRITE ( Monthlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ENDIF

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',ES10.3))')
 9002 FORMAT ('("Date"',I0,'('', ''A))')
 9003 FORMAT ('(I4,', I0,'('',''ES10.3))')

      END SUBROUTINE basin_summaryinit

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
      SUBROUTINE basin_summaryrun()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, MEAN_MONTHLY, MEAN_YEARLY
      use PRMS_MMFAPI, only: getvar_dble
      USE PRMS_MODULE, ONLY: Start_month, Start_day, End_year, End_month, End_day, Nowyear, Nowmonth, Nowday
      USE PRMS_BASIN_SUMMARY
      USE PRMS_SET_TIME, ONLY: Modays
      IMPLICIT NONE
! Local Variables
      INTEGER :: jj, write_month, last_day
      DOUBLE PRECISION :: yeardays_dble, monthdays_dble
!***********************************************************************
      IF ( Begin_results==OFF ) THEN
        IF ( Nowyear==Begyr .AND. Nowmonth==Start_month .AND. Nowday==Start_day ) THEN
          Begin_results = ACTIVE
        ELSE
          RETURN
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! need getvars for each variable (only can have short string)
      DO jj = 1, BasinOutVars
        CALL getvar_dble( MODNAME, BasinOutVar_names(jj)(:Nc_vars(jj)), 1, Basin_var_daily(jj) )
      ENDDO

      write_month = OFF
      IF ( BasinOut_freq>MEAN_MONTHLY ) THEN
        last_day = OFF
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = ACTIVE
        IF ( Lastyear/=Nowyear .OR. last_day==ACTIVE ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==ACTIVE ) THEN
            yeardays_dble = DBLE( Yeardays )
            IF ( BasinOut_freq==MEAN_YEARLY ) THEN
              DO jj = 1, BasinOutVars
                Basin_var_yearly(jj) = Basin_var_yearly(jj)/yeardays_dble
              ENDDO
            ENDIF
            IF ( last_day==ACTIVE ) THEN
              save_year = Nowyear
              save_month = Nowmonth
              save_day = Nowday
            ENDIF
            !WRITE ( Yearlyunit, Output_fmt3) Lastyear, (Basin_var_yearly(jj), jj=1, BasinOutVars)
            WRITE ( Yearlyunit, Output_fmt) save_year, save_month, save_day, (Basin_var_yearly(jj), jj=1, BasinOutVars)
            Basin_var_yearly = 0.0D0
            Yeardays = 0
            Lastyear = Nowyear
          ENDIF
        ENDIF
        Yeardays = Yeardays + 1
        save_year = Nowyear
        save_month = Nowmonth
        save_day = Nowday
      ELSEIF ( Monthly_flag==ACTIVE ) THEN
        ! check for last day of month and simulation
        IF ( Nowday==Modays(Nowmonth) ) THEN
          write_month = ACTIVE
        ELSEIF ( Nowyear==End_year ) THEN
          IF ( Nowmonth==End_month ) THEN
            IF ( Nowday==End_day ) write_month = ACTIVE
          ENDIF
        ENDIF
        Monthdays = Monthdays + 1
      ENDIF

      IF ( BasinOut_freq>MEAN_MONTHLY ) THEN
        DO jj = 1, BasinOutVars
          Basin_var_yearly(jj) = Basin_var_yearly(jj) + Basin_var_daily(jj)
        ENDDO
        RETURN
      ENDIF

      IF ( Monthly_flag==ACTIVE ) THEN
        monthdays_dble = DBLE( Monthdays )
        DO jj = 1, BasinOutVars
          Basin_var_monthly(jj) = Basin_var_monthly(jj) + Basin_var_daily(jj)
          IF ( write_month==ACTIVE ) THEN
            IF ( BasinOut_freq==MEAN_MONTHLY ) Basin_var_monthly(jj) = Basin_var_monthly(jj)/monthdays_dble
          ENDIF
        ENDDO
      ENDIF

      IF ( Daily_flag==ACTIVE ) WRITE ( Dailyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_daily(jj), jj=1,BasinOutVars)
      IF ( write_month==ACTIVE ) THEN
        WRITE ( Monthlyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_monthly(jj), jj=1,BasinOutVars)
        Monthdays = 0
        Basin_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE basin_summaryrun

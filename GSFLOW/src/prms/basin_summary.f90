!***********************************************************************
!     Output a set of declared basin variables as CSV file
!***********************************************************************
      MODULE PRMS_BASIN_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear, Dailyunit, Monthlyunit, Yearlyunit, Basin_var_type
      INTEGER, SAVE, ALLOCATABLE :: Nc_vars(:)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3
      CHARACTER(LEN=13), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Yeardays, Monthly_flag
      DOUBLE PRECISION, SAVE :: Monthdays
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Basin_var_daily(:), Basin_var_monthly(:), Basin_var_yearly(:)
! Control Parameters
      INTEGER, SAVE :: BasinOutVars, BasinOut_freq
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: BasinOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: BasinOutBaseFileName
      END MODULE PRMS_BASIN_SUMMARY

!     ******************************************************************
!     Basin results module
!     ******************************************************************
      SUBROUTINE basin_summary()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_BASIN_SUMMARY
      IMPLICIT NONE
! Functions
      EXTERNAL :: basin_summarydecl, basin_summaryinit, basin_summaryrun
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL basin_summaryrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL basin_summarydecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL basin_summaryinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Daily_flag==1 ) CLOSE ( Dailyunit )
        IF ( BasinOut_freq>4 ) CLOSE ( Yearlyunit )
        IF ( Monthly_flag==1 ) CLOSE ( Monthlyunit )
      ENDIF

      END SUBROUTINE basin_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE basin_summarydecl()
      USE PRMS_BASIN_SUMMARY
      USE PRMS_MODULE, ONLY: Model, Inputerror_flag
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_basin_summary
!***********************************************************************
      Version_basin_summary = 'basin_summary.f90 2018-04-05 14:02:00Z'
      CALL print_module(Version_basin_summary, 'Basin Output Summary        ', 90)
      MODNAME = 'basin_summary'

      IF ( control_integer(BasinOutVars, 'basinOutVars')/=0 ) BasinOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(BasinOut_freq, 'basinOut_freq')/=0 ) BasinOut_freq = 0

      IF ( BasinOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'ERROR, basin_summary requested with basinOutVars equal 0'
!          PRINT *, 'no basin_summary output is produced'
!          BasinOutON_OFF = 0
          Inputerror_flag = 1
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( BasinOutVar_names(BasinOutVars), Nc_vars(BasinOutVars) )
        BasinOutVar_names = ' '
        DO i = 1, BasinOutVars
          IF ( control_string_array(BasinOutVar_names(i), 'basinOutVar_names', i)/=0 ) CALL read_error(5, 'basinOutVar_names')
        ENDDO
        IF ( control_string(BasinOutBaseFileName, 'basinOutBaseFileName')/=0 ) CALL read_error(5, 'basinOutBaseFileName')
      ENDIF

      END SUBROUTINE basin_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE basin_summaryinit()
      USE PRMS_BASIN_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH, Start_year, Prms_warmup
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, size, dum, jj
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Start_year + Prms_warmup
      Lastyear = Begyr

      WRITE ( Output_fmt, 9001 ) BasinOutVars

      ierr = 0
      DO jj = 1, BasinOutVars
        Nc_vars(jj) = numchars(BasinOutVar_names(jj))
        Basin_var_type = getvartype(BasinOutVar_names(jj)(:Nc_vars(jj)), Basin_var_type )
        IF ( Basin_var_type/=3 ) THEN
          PRINT *, 'ERROR, invalid basin_summary variable:', BasinOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(BasinOutVar_names(jj)(:Nc_vars(jj)), dum )
        IF ( size/=1 ) THEN
          PRINT *, 'ERROR, invalid Basin_summary variable:', BasinOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only scalar variables are allowed'
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP
      ALLOCATE ( Basin_var_daily(BasinOutVars) )
      Basin_var_daily = 0.0D0

      Daily_flag = 0
      IF ( BasinOut_freq==1 .OR. BasinOut_freq==3 ) Daily_flag = 1

      Monthly_flag = 0
      IF ( BasinOut_freq==2 .OR. BasinOut_freq==3 .OR. BasinOut_freq==4 ) Monthly_flag = 1

      IF ( BasinOut_freq>4 ) THEN
        Yeardays = 0
        ALLOCATE ( Basin_var_yearly(BasinOutVars) )
        Basin_var_yearly = 0.0D0
        WRITE ( Output_fmt3, 9003 ) BasinOutVars
      ENDIF
      IF ( Monthly_flag==1 ) THEN
        Monthdays = 0.0D0
        ALLOCATE ( Basin_var_monthly(BasinOutVars) )
        Basin_var_monthly = 0.0D0
      ENDIF

      WRITE ( Output_fmt2, 9002 ) BasinOutVars

      IF ( Daily_flag==1 ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'.csv'
        !print *, fileName
        CALL PRMS_open_output_file(Dailyunit, fileName, 'xxx', 0, ios)
        IF ( ios/=0 ) STOP 'in basin_summary, daily'
        WRITE ( Dailyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ENDIF
      IF ( BasinOut_freq==5 ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_meanyearly.csv'
        CALL PRMS_open_output_file(Yearlyunit, fileName, 'xxx', 0, ios)
        IF ( ios/=0 ) STOP 'in basin_summary, mean yearly'
        WRITE ( Yearlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ELSEIF ( BasinOut_freq==6 ) THEN
        fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_yearly.csv'
        CALL PRMS_open_output_file(Yearlyunit, fileName, 'xxx', 0, ios)
        IF ( ios/=0 ) STOP 'in basin_summary, yearly'
        WRITE ( Yearlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ELSEIF ( Monthly_flag==1 ) THEN
        IF ( BasinOut_freq==4 ) THEN
          fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_meanmonthly.csv'
        ELSE
          fileName = BasinOutBaseFileName(:numchars(BasinOutBaseFileName))//'_monthly.csv'
        ENDIF
        !print *, fileName
        CALL PRMS_open_output_file(Monthlyunit, fileName, 'xxx', 0, ios)
        IF ( ios/=0 ) STOP 'in basin_summary, monthly'
        WRITE ( Monthlyunit, Output_fmt2 ) (BasinOutVar_names(jj)(:Nc_vars(jj)), jj=1, BasinOutVars)
      ENDIF

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I6,'('',''ES10.3))')
 9002 FORMAT ('("Date"',I0,'('', ''A))')
 9003 FORMAT ('(I4,', I0,'('',''ES10.3))')

      END SUBROUTINE basin_summaryinit

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
      SUBROUTINE basin_summaryrun()
      USE PRMS_BASIN_SUMMARY
      USE PRMS_MODULE, ONLY: Start_month, Start_day, End_year, End_month, End_day
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, DBLE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: jj, write_month, write_year, last_day
!***********************************************************************
      IF ( Begin_results==0 ) THEN
        IF ( Nowyear==Begyr .AND. Nowmonth==Start_month .AND. Nowday==Start_day ) THEN
          Begin_results = 1
        ELSE
          RETURN
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! need getvars for each variable (only can have short string)
      DO jj = 1, BasinOutVars
        IF ( getvar(MODNAME, BasinOutVar_names(jj)(:Nc_vars(jj)), 1, 'double', Basin_var_daily(jj))/=0 ) &
     &       CALL read_error(4, BasinOutVar_names(jj)(:Nc_vars(jj)))
      ENDDO

      write_month = 0
      write_year = 0
      IF ( BasinOut_freq>4 ) THEN
        last_day = 0
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = 1
        IF ( Lastyear/=Nowyear .OR. last_day==1 ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==1 ) THEN
            DO jj = 1, BasinOutVars
              IF ( BasinOut_freq==5 ) Basin_var_yearly(jj) = Basin_var_yearly(jj)/Yeardays
            ENDDO
            WRITE ( Yearlyunit, Output_fmt3) Lastyear, (Basin_var_yearly(jj), jj=1, BasinOutVars)
            Basin_var_yearly = 0.0D0
            Yeardays = 0
            Lastyear = Nowyear
          ENDIF
        ENDIF
        Yeardays = Yeardays + 1
      ELSEIF ( Monthly_flag==1 ) THEN
        ! check for last day of month and simulation
        IF ( Nowday==Modays(Nowmonth) ) THEN
          write_month = 1
        ELSEIF ( Nowyear==End_year ) THEN
          IF ( Nowmonth==End_month ) THEN
            IF ( Nowday==End_day ) write_month = 1
          ENDIF
        ENDIF
        Monthdays = Monthdays + 1.0D0
      ENDIF

      IF ( BasinOut_freq>4 ) THEN
        DO jj = 1, BasinOutVars
          Basin_var_yearly(jj) = Basin_var_yearly(jj) + Basin_var_daily(jj)
        ENDDO
        RETURN
      ENDIF

      IF ( Monthly_flag==1 ) THEN
        DO jj = 1, BasinOutVars
          Basin_var_monthly(jj) = Basin_var_monthly(jj) + Basin_var_daily(jj)
          IF ( write_month==1 ) THEN
            IF ( BasinOut_freq==4 ) Basin_var_monthly(jj) = Basin_var_monthly(jj)/Monthdays
          ENDIF
        ENDDO
      ENDIF

      IF ( Daily_flag==1 ) WRITE ( Dailyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_daily(jj), jj=1,BasinOutVars)
      IF ( write_month==1 ) THEN
        WRITE ( Monthlyunit, Output_fmt) Nowyear, Nowmonth, Nowday, (Basin_var_monthly(jj), jj=1,BasinOutVars)
        Monthdays = 0.0D0
        Basin_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE basin_summaryrun

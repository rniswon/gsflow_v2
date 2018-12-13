!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
      MODULE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:)
      REAL, SAVE, ALLOCATABLE :: Nhru_var_daily(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_dble(:, :)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3
      CHARACTER(LEN=12), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Double_vars, Yeardays, Monthly_flag
      DOUBLE PRECISION, SAVE :: Monthdays
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_monthly(:, :), Nhru_var_yearly(:, :)
! Declared Parameters
      INTEGER, SAVE :: Prms_warmup
! Control Parameters
      INTEGER, SAVE :: NhruOutVars, NhruOut_freq
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: NhruOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: NhruOutBaseFileName
      END MODULE PRMS_NHRU_SUMMARY

!     ******************************************************************
!     nhru results module
!     ******************************************************************
      SUBROUTINE nhru_summary()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_NHRU_SUMMARY
      IMPLICIT NONE
! Functions
      EXTERNAL :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
! Local Variables
      INTEGER :: i
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL nhru_summaryrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL nhru_summarydecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL nhru_summaryinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        DO i = 1, NhruOutVars
          IF ( Daily_flag==1 ) THEN
            IF ( Dailyunit(i)>0 ) CLOSE ( Dailyunit(i) )
          ENDIF
          IF ( NhruOut_freq>4 ) THEN
            IF ( Yearlyunit(i)>0 ) CLOSE ( Yearlyunit(i) )
          ENDIF
          IF ( Monthly_flag==1 ) THEN
            IF ( Monthlyunit(i)>0 ) CLOSE ( Monthlyunit(i) )
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE nhru_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE nhru_summarydecl()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Model, Inputerror_flag
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_nhru_summary
!***********************************************************************
      Version_nhru_summary = 'nhru_summary.f90 2017-11-20 12:59:00Z'
      CALL print_module(Version_nhru_summary, 'Nhru Output Summary         ', 90)
      MODNAME = 'nhru_summary'

      IF ( control_integer(NhruOutVars, 'nhruOutVars')/=0 ) NhruOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(NhruOut_freq, 'nhruOut_freq')/=0 ) NhruOut_freq = 0

      IF ( NhruOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'ERROR, nhru_summary requested with nhruOutVars equal 0'
          Inputerror_flag = 1
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( NhruOutVar_names(NhruOutVars), Nhru_var_type(NhruOutVars), Nc_vars(NhruOutVars) )
        NhruOutVar_names = ' '
        DO i = 1, NhruOutVars
          IF ( control_string_array(NhruOutVar_names(i), 'nhruOutVar_names', i)/=0 ) CALL read_error(5, 'nhruOutVar_names')
        ENDDO
        IF ( control_string(NhruOutBaseFileName, 'nhruOutBaseFileName')/=0 ) CALL read_error(5, 'nhruOutBaseFileName')
      ENDIF

! Declared Parameters
      IF ( declparam(MODNAME, 'prms_warmup', 'one', 'integer', &
     &     '1', '0', '12', &
     &     'Number of years to simulate before writing mapped results', &
     &     'Number of years to simulate before writing mapped results', &
     &     'years')/=0 ) CALL read_error(1, 'prms_warmup')

      END SUBROUTINE nhru_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nhru_summaryinit()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Inputerror_flag , MAXFILE_LENGTH, Start_year, End_year
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, size, dim, jj, j
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      Begyr = Start_year
      IF ( getparam(MODNAME, 'prms_warmup', 1, 'integer', Prms_warmup)/=0 ) CALL read_error(2, 'prms_warmup')
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Begyr + Prms_warmup
      IF ( Begyr>End_year ) THEN
        PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
        Inputerror_flag = 1
      ENDIF
      Lastyear = Begyr

      WRITE ( Output_fmt, 9001 ) Nhru

      Double_vars = 0
      ierr = 0
      DO jj = 1, NhruOutVars
        Nc_vars(jj) = numchars(NhruOutVar_names(jj))
        Nhru_var_type(jj) = getvartype(NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru_var_type(jj) )
        IF ( Nhru_var_type(jj)==3 ) Double_vars = 1
        IF ( Nhru_var_type(jj)/=2 .AND. Nhru_var_type(jj)/=3 ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only real or double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(NhruOutVar_names(jj)(:Nc_vars(jj)), dim )
        IF ( size/=Nhru ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only variables dimensioned by nhru, nssr, or ngw allowed'
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP
      IF ( Double_vars==1 ) THEN
        ALLOCATE ( Nhru_var_dble(Nhru, NhruOutVars) )
        Nhru_var_dble = 0.0D0
      ENDIF

      Daily_flag = 0
      IF ( NhruOut_freq==1 .OR. NhruOut_freq==3 ) THEN
        Daily_flag = 1
        ALLOCATE ( Dailyunit(NhruOutVars) )
        Dailyunit = 0
      ENDIF

      Monthly_flag = 0
      IF ( NhruOut_freq==2 .OR. NhruOut_freq==3 .OR. NhruOut_freq==4 ) Monthly_flag = 1

      IF ( NhruOut_freq>4 ) THEN
        Yeardays = 0
        ALLOCATE ( Nhru_var_yearly(Nhru, NhruOutVars), Yearlyunit(NhruOutVars) )
        Nhru_var_yearly = 0.0D0
        Yearlyunit = 0
        WRITE ( Output_fmt3, 9003 ) Nhru
      ELSEIF ( Monthly_flag==1 ) THEN
        Monthdays = 0.0D0
        ALLOCATE ( Nhru_var_monthly(Nhru, NhruOutVars), Monthlyunit(NhruOutVars) )
        Nhru_var_monthly = 0.0D0
        Monthlyunit = 0
      ENDIF

      WRITE ( Output_fmt2, 9002 ) Nhru
      ALLOCATE ( Nhru_var_daily(Nhru, NhruOutVars) )
      Nhru_var_daily = 0.0
      DO jj = 1, NhruOutVars
        IF ( Daily_flag==1 ) THEN
          fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'.csv'
          !print *, fileName
          CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nhru_summary'
          WRITE ( Dailyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
        ENDIF
        IF ( NhruOut_freq>4 ) THEN
          IF ( NhruOut_freq==5 ) THEN
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_meanyearly.csv'
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nhru_summary, mean yearly'
          ELSE  !IF ( NhruOut_freq==6 ) THEN
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_yearly.csv'
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nhru_summary, yearly'
          ENDIF
          WRITE ( Yearlyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
        ENDIF
        IF ( Monthly_flag==1 ) THEN
          IF ( NhruOut_freq==4 ) THEN
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))// &
     &                 '_meanmonthly.csv'
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nhru_summary, mean monthly'
          ELSE
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_monthly.csv'
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nhru_summary, monthly'
          ENDIF
          WRITE ( Monthlyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
        ENDIF
      ENDDO

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I6,'('',''ES10.3))')
 9002 FORMAT ('("Date "',I6,'('',''I6))')
 9003 FORMAT ('(I4,', I6,'('',''ES10.3))')

      END SUBROUTINE nhru_summaryinit

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
      SUBROUTINE nhru_summaryrun()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day, End_year, End_month, End_day
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, DBLE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: j, i, jj, write_month, write_year, last_day
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
      DO jj = 1, NhruOutVars
        IF ( Nhru_var_type(jj)==2 ) THEN
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'real', Nhru_var_daily(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
        ELSEIF ( Nhru_var_type(jj)==3 ) THEN  ! probably don't need double
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'double', Nhru_var_dble(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
        ENDIF
      ENDDO

      write_month = 0
      write_year = 0
      IF ( NhruOut_freq>4 ) THEN
        last_day = 0
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = 1
        IF ( Lastyear/=Nowyear .OR. last_day==1 ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==1 ) THEN
            DO jj = 1, NhruOutVars
              IF ( NhruOut_freq==5 ) THEN
                DO j = 1, Active_hrus
                  i = Hru_route_order(j)
                  Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj)/Yeardays
                ENDDO
              ENDIF
              WRITE ( Yearlyunit(jj), Output_fmt3) Lastyear, (Nhru_var_yearly(j,jj), j=1,Nhru)
            ENDDO
            Nhru_var_yearly = 0.0D0
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

      IF ( Double_vars==1 ) THEN
        DO jj = 1, NhruOutVars
          IF ( Nhru_var_type(jj)==3 ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_var_daily(i, jj) = SNGL( Nhru_var_dble(i, jj) )
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      IF ( NhruOut_freq>4 ) THEN
        DO jj = 1, NhruOutVars
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) + DBLE( Nhru_var_daily(i, jj) )
          ENDDO
        ENDDO
        RETURN
      ENDIF

      IF ( Monthly_flag==1 ) THEN
        DO jj = 1, NhruOutVars
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) + DBLE( Nhru_var_daily(i, jj) )
            IF ( write_month==1 ) THEN
              IF ( NhruOut_freq==4 ) Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj)/Monthdays
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      DO jj = 1, NhruOutVars
        IF ( Daily_flag==1 ) WRITE ( Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nhru_var_daily(j,jj), j=1,Nhru)
        IF ( write_month==1 ) WRITE ( Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nhru_var_monthly(j,jj), j=1,Nhru)
      ENDDO
      IF ( write_month==1 ) THEN
        Monthdays = 0.0D0
        Nhru_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE nhru_summaryrun

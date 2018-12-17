!***********************************************************************
!     Output a set of declared variables by subbasin in CSV format
!***********************************************************************
      MODULE PRMS_NSUB_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nsub_var_type(:), Nsub_var_size(:)
      REAL, SAVE, ALLOCATABLE :: Nhru_var_daily(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_dble(:, :), Nsub_var_dble(:, :), Nsub_var_daily(:, :)
      REAL, SAVE, ALLOCATABLE :: Nsub_var_single(:, :)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3
      CHARACTER(LEN=12), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Nhru_double_vars, Yeardays, Monthly_flag
      INTEGER, SAVE :: Nsub_single_vars, Nsub_vars, Nhru_vars
      DOUBLE PRECISION, SAVE :: Monthdays
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nsub_var_monthly(:, :), Nsub_var_yearly(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Sub_area(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Hru_subbasin(:)
! Control Parameters
      INTEGER, SAVE :: NsubOutVars, NsubOut_freq, NsubOut_format
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: NsubOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: NsubOutBaseFileName
      END MODULE PRMS_NSUB_SUMMARY

!     ******************************************************************
!     subbasin results module
!     ******************************************************************
      SUBROUTINE nsub_summary()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_NSUB_SUMMARY
      IMPLICIT NONE
! Functions
      EXTERNAL :: nsub_summarydecl, nsub_summaryinit, nsub_summaryrun
! Local Variables
      INTEGER :: i
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL nsub_summaryrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL nsub_summarydecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL nsub_summaryinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        DO i = 1, NsubOutVars
          IF ( Daily_flag==1 ) THEN
            IF ( Dailyunit(i)>0 ) CLOSE ( Dailyunit(i) )
          ENDIF
          IF ( NsubOut_freq>4 ) THEN
            IF ( Yearlyunit(i)>0 ) CLOSE ( Yearlyunit(i) )
          ENDIF
          IF ( Monthly_flag==1 ) THEN
            IF ( Monthlyunit(i)>0 ) CLOSE ( Monthlyunit(i) )
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE nsub_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE nsub_summarydecl()
      USE PRMS_NSUB_SUMMARY
      USE PRMS_MODULE, ONLY: Model, Inputerror_flag, Nhru, Nsub
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_nsub_summary
!***********************************************************************
      Version_nsub_summary = 'nsub_summary.f90 2018-05-25 13:36:00Z'
      CALL print_module(Version_nsub_summary, 'Subbasin Output Summary     ', 90)
      MODNAME = 'nsub_summary'

      IF ( control_integer(NsubOutVars, 'nsubOutVars')/=0 ) NsubOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(NsubOut_freq, 'nsubOut_freq')/=0 ) NsubOut_freq = 0
      ! 1 = ES10.3; 2 = F0.2; 3 = F0.3; 4 = F0.4; 5 = F0.5
      IF ( control_integer(NsubOut_format, 'nsubOut_format')/=0 ) NsubOut_format = 1

      ALLOCATE ( Nsub_var_size(NsubOutVars) )
      IF ( NsubOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'ERROR, nsub_summary requested with nsubOutVars equal 0'
          Inputerror_flag = 1
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( NsubOutVar_names(NsubOutVars), Nsub_var_type(NsubOutVars), Nc_vars(NsubOutVars) )
        NsubOutVar_names = ' '
        DO i = 1, NsubOutVars
          IF ( control_string_array(NsubOutVar_names(i), 'nsubOutVar_names', i)/=0 ) CALL read_error(5, 'nsubOutVar_names')
        ENDDO
        IF ( control_string(NsubOutBaseFileName, 'nsubOutBaseFileName')/=0 ) CALL read_error(5, 'nsubOutBaseFileName')
      ENDIF

      ALLOCATE ( Hru_subbasin(Nhru), Sub_area(Nsub) )
      IF ( declparam(MODNAME, 'hru_subbasin', 'nhru', 'integer', &
     &     '0', 'bounded', 'nsub', &
     &     'Index of subbasin assigned to each HRU', &
     &     'Index of subbasin assigned to each HRU', &
     &     'none')/=0 ) CALL read_error(1, 'hru_subbasin')

      END SUBROUTINE nsub_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nsub_summaryinit()
      USE PRMS_NSUB_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Nsub, Inputerror_flag, MAXFILE_LENGTH, Start_year, Prms_warmup
      USE PRMS_BASIN, ONLY: Hru_area_dble, DNEARZERO, Active_hrus, Hru_route_order
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, dum, jj, j, i, k
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Start_year + Prms_warmup
      Lastyear = Begyr

      IF ( NsubOut_format==1 ) THEN
        WRITE ( Output_fmt, 9001 ) Nsub
      ELSEIF ( NsubOut_format==2 ) THEN
        WRITE ( Output_fmt, 9007 ) Nsub
      ELSEIF ( NsubOut_format==3 ) THEN
        WRITE ( Output_fmt, 9006 ) Nsub
      ELSEIF ( NsubOut_format==4 ) THEN
        WRITE ( Output_fmt, 9005 ) Nsub
      ELSEIF ( NsubOut_format==5 ) THEN
        WRITE ( Output_fmt, 9012 ) Nsub
      ENDIF

      Nhru_double_vars = 0
      Nsub_single_vars = 0
      Nsub_vars = 0
      Nhru_vars = 0
      ierr = 0
      DO jj = 1, NsubOutVars
        Nc_vars(jj) = numchars(NsubOutVar_names(jj))
        Nsub_var_type(jj) = getvartype(NsubOutVar_names(jj)(:Nc_vars(jj)), Nsub_var_type(jj) )
        IF ( Nsub_var_type(jj)/=2 .AND. Nsub_var_type(jj)/=3 ) THEN
          PRINT *, 'ERROR, invalid nsub_summary variable:', NsubOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only real or double variables allowed'
          ierr = 1
        ENDIF
        Nsub_var_size(jj) = getvarsize(NsubOutVar_names(jj)(:Nc_vars(jj)), dum )
        IF ( Nsub_var_size(jj)==Nhru ) THEN
          Nhru_vars = 1
          IF ( Nsub_var_type(jj)==3 ) Nhru_double_vars = 1
        ELSEIF ( Nsub_var_size(jj)==Nsub ) THEN
          Nsub_vars = 1
          IF ( Nsub_var_type(jj)==2 ) Nsub_single_vars = 1
        ELSE
          PRINT *, 'ERROR, invalid nsub_summary variable:', NsubOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only variables dimensioned by nsub, nhru, nssr, or ngw are allowed'
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP

      IF ( Nhru_vars==1 ) THEN
        IF ( Nhru_double_vars==1 ) THEN
          ALLOCATE ( Nhru_var_dble(Nhru, NsubOutVars) )
          Nhru_var_dble = 0.0D0
        ENDIF
        ALLOCATE ( Nhru_var_daily(Nhru, NsubOutVars) )
        Nhru_var_daily = 0.0
      ENDIF

      ALLOCATE ( Nsub_var_dble(Nsub, NsubOutVars) )
      Nsub_var_dble = 0.0D0
      IF ( Nsub_vars==1 .AND. Nsub_single_vars==1 ) THEN
        ALLOCATE ( Nsub_var_single(Nsub, NsubOutVars) )
        Nsub_var_single = 0.0
      ENDIF

      Daily_flag = 0
      IF ( NsubOut_freq==1 .OR. NsubOut_freq==3 ) THEN
        Daily_flag = 1
        ALLOCATE ( Dailyunit(NsubOutVars) )
        Dailyunit = 0
        ALLOCATE ( Nsub_var_daily(Nsub, NsubOutVars) )
        Nsub_var_daily = 0.0D0
      ENDIF

      Monthly_flag = 0
      IF ( NsubOut_freq==2 .OR. NsubOut_freq==3 .OR. NsubOut_freq==4 ) Monthly_flag = 1

      IF ( NsubOut_freq>4 ) THEN
        Yeardays = 0
        ALLOCATE ( Nsub_var_yearly(Nsub, NsubOutVars), Yearlyunit(NsubOutVars) )
        Nsub_var_yearly = 0.0D0
        Yearlyunit = 0
        IF ( NsubOut_format==1 ) THEN
          WRITE ( Output_fmt3, 9003 ) Nsub
        ELSEIF ( NsubOut_format==2 ) THEN
          WRITE ( Output_fmt3, 9010 ) Nsub
        ELSEIF ( NsubOut_format==3 ) THEN
          WRITE ( Output_fmt3, 9009 ) Nsub
        ELSEIF ( NsubOut_format==4 ) THEN
          WRITE ( Output_fmt3, 9008 ) Nsub
        ELSEIF ( NsubOut_format==5 ) THEN
          WRITE ( Output_fmt3, 9011 ) Nsub
        ENDIF
      ENDIF
      IF ( Monthly_flag==1 ) THEN
        Monthdays = 0.0D0
        ALLOCATE ( Nsub_var_monthly(Nsub, NsubOutVars), Monthlyunit(NsubOutVars) )
        Nsub_var_monthly = 0.0D0
        Monthlyunit = 0
      ENDIF

      WRITE ( Output_fmt2, 9002 ) Nsub

      DO jj = 1, NsubOutVars
        IF ( Daily_flag==1 ) THEN
          fileName = NsubOutBaseFileName(:numchars(NsubOutBaseFileName))//NsubOutVar_names(jj)(:Nc_vars(jj))//'.csv'
          !print *, fileName
          CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nsub_summary, daily'
          WRITE ( Dailyunit(jj), Output_fmt2 ) (j, j=1,Nsub)
        ENDIF
        IF ( NsubOut_freq==5 ) THEN
          fileName = NsubOutBaseFileName(:numchars(NsubOutBaseFileName))//NsubOutVar_names(jj)(:Nc_vars(jj))//'_meanyearly.csv'
          CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nsub_summary, mean yearly'
          WRITE ( Yearlyunit(jj), Output_fmt2 ) (j, j=1,Nsub)
        ELSEIF ( NsubOut_freq==6 ) THEN
          fileName = NsubOutBaseFileName(:numchars(NsubOutBaseFileName))//NsubOutVar_names(jj)(:Nc_vars(jj))//'_yearly.csv'
          CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nsub_summary, yearly'
          WRITE ( Yearlyunit(jj), Output_fmt2 ) (j, j=1,Nsub)
        ELSEIF ( Monthly_flag==1 ) THEN
          IF ( NsubOut_freq==4 ) THEN
            fileName = NsubOutBaseFileName(:numchars(NsubOutBaseFileName))//NsubOutVar_names(jj)(:Nc_vars(jj))// &
     &                 '_meanmonthly.csv'
          ELSE
            fileName = NsubOutBaseFileName(:numchars(NsubOutBaseFileName))//NsubOutVar_names(jj)(:Nc_vars(jj))//'_monthly.csv'
          ENDIF
          !print *, fileName
          CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nsub_summary, monthly'
          WRITE ( Monthlyunit(jj), Output_fmt2 ) (j, j=1,Nsub)
        ENDIF
      ENDDO

      IF ( getparam(MODNAME, 'hru_subbasin', Nhru, 'integer', Hru_subbasin)/=0 ) CALL read_error(2, 'hru_subbasin')
      Sub_area = 0.0D0
      DO i = 1, Active_hrus
        j = Hru_route_order(i)
        k = Hru_subbasin(j)
        IF ( k>0 ) Sub_area(k) = Sub_area(k) + Hru_area_dble(j)
      ENDDO
      DO i = 1, Nsub
        IF ( Sub_area(i)<DNEARZERO ) THEN
          PRINT *, 'ERROR, subbasin:', i, ' does not include any HRUs'
          Inputerror_flag = 1
        ENDIF
      ENDDO

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('',''ES10.3))')
 9002 FORMAT ('("Date"',I0,'('', ''I0))')
 9003 FORMAT ('(I4,', I0,'('','',ES10.3))')
 9005 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.4))')
 9006 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.3))')
 9007 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.2))')
 9008 FORMAT ('(I4,', I0,'('','',F0.4))')
 9009 FORMAT ('(I4,', I0,'('','',F0.3))')
 9010 FORMAT ('(I4,', I0,'('','',F0.2))')
 9011 FORMAT ('(I4,', I0,'('','',F0.5))')
 9012 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.5))')

      END SUBROUTINE nsub_summaryinit

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
      SUBROUTINE nsub_summaryrun()
      USE PRMS_NSUB_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, Nsub, Start_month, Start_day, End_year, End_month, End_day
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area_dble
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Modays
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTRINSIC SNGL, DBLE
      INTEGER, EXTERNAL :: getvar
      EXTERNAL read_error
! Local Variables
      INTEGER :: j, i, jj, write_month, write_year, last_day, k
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
      DO jj = 1, NsubOutVars
        IF ( Nsub_var_type(jj)==2 ) THEN
          IF ( Nsub_var_size(jj)==Nhru ) THEN
            IF ( getvar(MODNAME, NsubOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'real', Nhru_var_daily(1, jj))/=0 ) &
     &           CALL read_error(4, NsubOutVar_names(jj)(:Nc_vars(jj)))
          ELSE
            IF ( getvar(MODNAME, NsubOutVar_names(jj)(:Nc_vars(jj)), Nsub, 'real', Nsub_var_single(1, jj))/=0 ) &
     &           CALL read_error(4, NsubOutVar_names(jj)(:Nc_vars(jj)))
          ENDIF
        ELSEIF ( Nsub_var_type(jj)==3 ) THEN
          IF ( Nsub_var_size(jj)==Nhru ) THEN
            IF ( getvar(MODNAME, NsubOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'double', Nhru_var_dble(1, jj))/=0 ) &
     &           CALL read_error(4, NsubOutVar_names(jj)(:Nc_vars(jj)))
          ELSE
            IF ( getvar(MODNAME, NsubOutVar_names(jj)(:Nc_vars(jj)), Nsub, 'double', Nsub_var_dble(1, jj))/=0 ) &
     &           CALL read_error(4, NsubOutVar_names(jj)(:Nc_vars(jj)))
          ENDIF
        ENDIF
      ENDDO

      write_month = 0
      write_year = 0
      IF ( NsubOut_freq>4 ) THEN
        last_day = 0
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = 1
        IF ( Lastyear/=Nowyear .OR. last_day==1 ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==1 ) THEN
            DO jj = 1, NsubOutVars
              IF ( Nsub_var_size(jj)==Nhru ) THEN
                DO k = 1, Nsub
                  IF ( NsubOut_freq==5 ) Nsub_var_yearly(k, jj) = Nsub_var_yearly(k, jj)/Yeardays
                  Nsub_var_yearly(k, jj) = Nsub_var_yearly(k, jj)/Sub_area(k)
                ENDDO
              ELSE
                IF ( NsubOut_freq==5 ) THEN
                  DO k = 1, Nsub
                    Nsub_var_yearly(k, jj) = Nsub_var_yearly(k, jj)/Yeardays
                  ENDDO
                ENDIF
              ENDIF
              WRITE ( Yearlyunit(jj), Output_fmt3) Lastyear, (Nsub_var_yearly(j,jj), j=1,Nsub)
            ENDDO
            Nsub_var_yearly = 0.0D0
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

      IF ( Nhru_double_vars==1 ) THEN
        DO jj = 1, NsubOutVars
          IF ( Nsub_var_type(jj)==3 ) THEN
            IF ( Nsub_var_size(jj)/=Nhru ) CYCLE
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_var_daily(i, jj) = SNGL( Nhru_var_dble(i, jj) )
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      IF ( Nsub_single_vars==1 ) THEN
        DO jj = 1, NsubOutVars
          IF ( Nsub_var_type(jj)==2 ) THEN
            IF ( Nsub_var_size(jj)/=Nsub ) CYCLE
            DO i = 1, Nsub
              Nsub_var_dble(i, jj) = DBLE( Nsub_var_single(i, jj) )
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      IF ( NsubOut_freq>4 ) THEN
        DO jj = 1, NsubOutVars
          IF ( Nsub_var_size(jj)==Nhru ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              k = Hru_subbasin(j)
              IF ( k>0 ) Nsub_var_yearly(k, jj) = Nsub_var_yearly(k, jj) + DBLE( Nhru_var_daily(i, jj) )*Hru_area_dble(i)
            ENDDO
          ELSE
            DO i = 1, Nsub
              Nsub_var_yearly(i, jj) = Nsub_var_yearly(i, jj) + Nsub_var_dble(i, jj)
            ENDDO
          ENDIF
        ENDDO
        RETURN
      ENDIF

      IF ( Monthly_flag==1 ) THEN
        DO jj = 1, NsubOutVars
          IF ( Nsub_var_size(jj)==Nhru ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              k = Hru_subbasin(j)
              IF ( k>0 ) Nsub_var_monthly(k, jj) = Nsub_var_monthly(k, jj) + DBLE( Nhru_var_daily(i, jj) )*Hru_area_dble(i)
            ENDDO
            DO k = 1, Nsub
              IF ( write_month==1 ) THEN
                IF ( NsubOut_freq==4 ) Nsub_var_monthly(k, jj) = Nsub_var_monthly(k, jj)/Monthdays/Sub_area(k)
              ENDIF
            ENDDO
          ELSE
            DO i = 1, Nsub
              Nsub_var_monthly(i, jj) = Nsub_var_monthly(i, jj) + Nsub_var_dble(i, jj)
              IF ( write_month==1 ) Nsub_var_monthly(i, jj) = Nsub_var_monthly(i, jj)/Monthdays
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      IF ( Daily_flag==1 .AND. Nhru_vars==1 ) THEN
        Nsub_var_daily = 0.0D0
        DO jj = 1, NsubOutVars
          IF ( Nsub_var_size(jj)==Nhru ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              k = Hru_subbasin(i)
              IF ( k>0 ) Nsub_var_daily(k, jj) = Nsub_var_daily(k, jj) + DBLE( Nhru_var_daily(i, jj) )*Hru_area_dble(i)
            ENDDO
            DO k = 1, Nsub
              Nsub_var_dble(k, jj) = Nsub_var_daily(k, jj)/Sub_area(k)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      DO jj = 1, NsubOutVars
        IF ( Daily_flag==1 ) WRITE ( Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nsub_var_dble(j,jj), j=1,Nsub)
        IF ( write_month==1 ) WRITE ( Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nsub_var_monthly(j,jj), j=1,Nsub)
      ENDDO
      IF ( write_month==1 ) THEN
        Monthdays = 0.0D0
        Nsub_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE nsub_summaryrun

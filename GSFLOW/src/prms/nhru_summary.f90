!***********************************************************************
!     Output a set of declared variables by HRU for use with R
!***********************************************************************
      MODULE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:), Nhru_var_int(:, :)
      REAL, SAVE, ALLOCATABLE :: Nhru_var_daily(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_dble(:, :)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3, Output_fmtint
      CHARACTER(LEN=48), SAVE :: Output_grid_fmt, Output_grid_fmtint, Output_date_fmt, Output_date_fmt3, Output_fmt3int
      CHARACTER(LEN=12), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Double_vars, Yeardays, Monthly_flag, Integer_vars
      DOUBLE PRECISION, SAVE :: Monthdays
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_monthly(:, :), Nhru_var_yearly(:, :)
! Paramters
      INTEGER, SAVE :: Nhru_out_ncol
      INTEGER, SAVE, ALLOCATABLE :: Nhm_id(:)
! Control Parameters
      INTEGER, SAVE :: NhruOutVars, NhruOut_freq, NhruOut_format
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
      USE PRMS_MODULE, ONLY: Model, Inputerror_flag, NhruOutON_OFF, Nhru
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_nhru_summary
!***********************************************************************
      Version_nhru_summary = 'nhru_summary.f90 2019-10-25 12:57:00Z'
      CALL print_module(Version_nhru_summary, 'Nhru Output Summary         ', 90)
      MODNAME = 'nhru_summary'

      IF ( control_integer(NhruOutVars, 'nhruOutVars')/=0 ) NhruOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(NhruOut_freq, 'nhruOut_freq')/=0 ) NhruOut_freq = 0
      ! 1 = ES10.3; 2 = F0.2; 3 = F0.3; 4 = F0.4; 5 = F0.5
      IF ( control_integer(NhruOut_format, 'nhruOut_format')/=0 ) NhruOut_format = 1

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
      IF ( NhruOutON_OFF==2 ) THEN
        ALLOCATE ( Nhm_id(Nhru) )
        IF ( declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &       '1', '1', '9999999', &
     &       'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &       'none') /= 0 ) CALL read_error(1, 'nhm_id')
      ENDIF

      IF ( declparam(MODNAME, 'nhru_out_ncol', 'one', 'integer', &
     &     '-1', '-1', '999999', &
     &     'Number of columns for each row of the CSV output', &
     &     'Number of columns for each row of the CSV output', &
     &     'none')/=0 ) CALL read_error(1, 'nhru_out_ncol')

      END SUBROUTINE nhru_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nhru_summaryinit()
      USE PRMS_NHRU_SUMMARY
      USE PRMS_MODULE, ONLY: Nhru, MAXFILE_LENGTH, Start_year, NhruOutON_OFF, Prms_warmup
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, size, dim, jj, j
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Start_year + Prms_warmup
      Lastyear = Begyr

      IF ( getparam(MODNAME, 'nhru_out_ncol', 1, 'integer', Nhru_out_ncol)/=0 ) CALL read_error(2, 'nhru_out_ncol')
      IF ( Nhru_out_ncol<1 ) Nhru_out_ncol = Nhru

      IF ( NhruOut_format==1 ) THEN
        WRITE ( Output_fmt, 9001 ) Nhru
        WRITE ( Output_grid_fmt, 9014 ) Nhru_out_ncol - 1
      ELSEIF ( NhruOut_format==2 ) THEN
        WRITE ( Output_fmt, 9007 ) Nhru
        WRITE ( Output_grid_fmt, 9017 ) Nhru_out_ncol - 1
      ELSEIF ( NhruOut_format==3 ) THEN
        WRITE ( Output_fmt, 9006 ) Nhru
        WRITE ( Output_grid_fmt, 9016 ) Nhru_out_ncol - 1
      ELSEIF ( NhruOut_format==4 ) THEN
        WRITE ( Output_fmt, 9005 ) Nhru
        WRITE ( Output_grid_fmt, 9015 ) Nhru_out_ncol - 1
      ELSEIF ( NhruOut_format==5 ) THEN
        WRITE ( Output_fmt, 9012 ) Nhru
        WRITE ( Output_grid_fmt, 9014 ) Nhru_out_ncol - 1
      ENDIF
      WRITE ( Output_fmtint, 9004 ) Nhru
      WRITE ( Output_grid_fmtint, 9018 ) Nhru_out_ncol - 1
      WRITE ( Output_date_fmt, 9013 )

      Double_vars = 0
      Integer_vars = 0
      ierr = 0
      DO jj = 1, NhruOutVars
        Nc_vars(jj) = numchars(NhruOutVar_names(jj))
        Nhru_var_type(jj) = getvartype(NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru_var_type(jj) )
        IF ( Nhru_var_type(jj)==3 ) Double_vars = 1
        IF ( Nhru_var_type(jj)==1 ) Integer_vars = 1
        IF ( Nhru_var_type(jj)/=2 .AND. Nhru_var_type(jj)/=3 .AND. Nhru_var_type(jj)/=1 ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only integer, real or double variables allowed'
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
      IF ( Integer_vars==1 ) THEN
        ALLOCATE ( Nhru_var_int(Nhru, NhruOutVars) )
        Nhru_var_int = 0
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
        IF ( NhruOut_format==1 ) THEN
          WRITE ( Output_fmt3, 9003 ) Nhru
        ELSEIF ( NhruOut_format==2 ) THEN
          WRITE ( Output_fmt3, 9010 ) Nhru
        ELSEIF ( NhruOut_format==3 ) THEN
          WRITE ( Output_fmt3, 9009 ) Nhru
        ELSEIF ( NhruOut_format==4 ) THEN
          WRITE ( Output_fmt3, 9008 ) Nhru
        ELSEIF ( NhruOut_format==5 ) THEN
          WRITE ( Output_fmt3, 9011 ) Nhru
        ENDIF
        WRITE ( Output_date_fmt3, "(A)" ) '(I0)'
        WRITE ( Output_fmt3int, 9019 ) Nhru_out_ncol - 1
      ELSEIF ( Monthly_flag==1 ) THEN
        Monthdays = 0.0D0
        ALLOCATE ( Nhru_var_monthly(Nhru, NhruOutVars), Monthlyunit(NhruOutVars) )
        Nhru_var_monthly = 0.0D0
        Monthlyunit = 0
      ENDIF

      IF ( NhruOutON_OFF==2 ) THEN
        IF ( getparam(MODNAME, 'nhm_id', Nhru, 'integer', Nhm_id)/=0 ) CALL read_error(2, 'nhm_id')
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
          IF ( NhruOutON_OFF==1 ) THEN
            WRITE ( Dailyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
          ELSE
            WRITE ( Dailyunit(jj), Output_fmt2 ) (Nhm_id(j), j=1,Nhru)
          ENDIF
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
          IF ( NhruOutON_OFF==1 ) THEN
            WRITE ( Yearlyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
          ELSE
            WRITE ( Yearlyunit(jj), Output_fmt2 ) (Nhm_id(j), j=1,Nhru)
          ENDIF
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
          IF ( NhruOutON_OFF==1 ) THEN
            WRITE ( Monthlyunit(jj), Output_fmt2 ) (j, j=1,Nhru)
          ELSE
            WRITE ( Monthlyunit(jj), Output_fmt2 ) (Nhm_id(j), j=1,Nhru)
          ENDIF
        ENDIF
      ENDDO

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',ES10.3))')
 9002 FORMAT ('("Date"',I0,'('', ''I0))')
 9003 FORMAT ('(I4,', I0,'('','',ES10.3))')
 9004 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',I0))')
 9005 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.4))')
 9006 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.3))')
 9007 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.2))')
 9008 FORMAT ('(I4,', I0,'('','',F0.4))')
 9009 FORMAT ('(I4,', I0,'('','',F0.3))')
 9010 FORMAT ('(I4,', I0,'('','',F0.2))')
 9011 FORMAT ('(I4,', I0,'('','',F0.5))')
 9012 FORMAT ('(I4, 2(''-'',I2.2),',I0,'('','',F0.5))')
 9013 FORMAT ('(I4, 2(''-'',I2.2))')
 9014 FORMAT ('(',I0,'(ES10.3,'',''),ES10.3)')
 9015 FORMAT ('(',I0,'(F0.4,'',''),F0.4)')
 9016 FORMAT ('(',I0,'(F0.3,'',''),F0.3)')
 9017 FORMAT ('(',I0,'(F0.2,'',''),F0.2)')
 9018 FORMAT ('(',I0,'(I0,'',''),I0)')
 9019 FORMAT ('(I4,', I0,'('','',I0),I0)')

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
        ELSEIF ( Nhru_var_type(jj)==3 ) THEN
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'double', Nhru_var_dble(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Nhru_var_daily(i, jj) = SNGL( Nhru_var_dble(i, jj) )
          ENDDO
        ELSEIF ( Nhru_var_type(jj)==1 ) THEN
          IF ( getvar(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, 'integer', Nhru_var_int(1, jj))/=0 ) &
     &         CALL read_error(4, NhruOutVar_names(jj)(:Nc_vars(jj)))
          IF ( NhruOut_freq>1 ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_var_daily(i, jj) = FLOAT( Nhru_var_int(i, jj) )
            ENDDO
          ENDIF
        ENDIF
        IF ( Daily_flag==1 ) THEN
          IF ( Nhru_var_type(jj)/=1 ) THEN
            IF ( Nhru_out_ncol==Nhru) THEN
              WRITE ( Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nhru_var_daily(j,jj), j=1,Nhru)
            ELSE
              WRITE ( Dailyunit(jj), Output_date_fmt) Nowyear, Nowmonth, Nowday
              WRITE ( Dailyunit(jj), Output_grid_fmt) (Nhru_var_daily(j,jj), j=1,Nhru)
            ENDIF
          ELSE
            IF ( Nhru_out_ncol==Nhru) THEN
              WRITE ( Dailyunit(jj), Output_fmtint) Nowyear, Nowmonth, Nowday, (Nhru_var_int(j,jj), j=1,Nhru)
            ELSE
              WRITE ( Dailyunit(jj), Output_date_fmt) Nowyear, Nowmonth, Nowday
              WRITE ( Dailyunit(jj), Output_grid_fmtint) (Nhru_var_int(j,jj), j=1,Nhru)
            ENDIF
          ENDIF
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
              IF ( Nhru_var_type(jj)/=1 ) THEN
                IF ( Nhru_out_ncol==Nhru) THEN
                  WRITE ( Yearlyunit(jj), Output_fmt3) Lastyear, (Nhru_var_yearly(j,jj), j=1,Nhru)
                ELSE
                  WRITE ( Yearlyunit(jj), Output_date_fmt3) Lastyear
                  WRITE ( Yearlyunit(jj), Output_grid_fmt) (Nhru_var_yearly(j,jj), j=1,Nhru)
                ENDIF
              ELSE
                DO i = 1, Nhru
                  Nhru_var_int(i, jj) = INT( Nhru_var_yearly(i, jj) )
                ENDDO
                IF ( Nhru_out_ncol==Nhru) THEN
                  WRITE ( Yearlyunit(jj), Output_fmt3int) Lastyear, (Nhru_var_int(j,jj), j=1,Nhru)
                ELSE
                  WRITE ( Yearlyunit(jj), Output_date_fmt3) Lastyear
                  WRITE ( Yearlyunit(jj), Output_grid_fmtint) (Nhru_var_int(j,jj), j=1,Nhru)
                ENDIF
              ENDIF
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

      IF ( write_month==1 ) THEN
        DO jj = 1, NhruOutVars
          IF ( Nhru_var_type(jj)/=1 ) THEN
            IF ( Nhru_out_ncol==Nhru) THEN
              WRITE ( Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, (Nhru_var_monthly(j,jj), j=1,Nhru)
            ELSE
              WRITE ( Monthlyunit(jj), Output_date_fmt) Nowyear, Nowmonth, Nowday
              WRITE ( Monthlyunit(jj), Output_grid_fmt) (Nhru_var_monthly(j,jj), j=1,Nhru)
            ENDIF
          ELSE
            DO i = 1, Nhru
              Nhru_var_int(i, jj) = INT( Nhru_var_monthly(i, jj) )
            ENDDO
            IF ( Nhru_out_ncol==Nhru) THEN
              WRITE ( Monthlyunit(jj), Output_fmtint) Nowyear, Nowmonth, Nowday, (Nhru_var_int(j,jj), j=1,Nhru)
            ELSE
              WRITE ( Monthlyunit(jj), Output_date_fmt) Nowyear, Nowmonth, Nowday
              WRITE ( Monthlyunit(jj), Output_grid_fmtint) (Nhru_var_int(j,jj), j=1,Nhru)
            ENDIF
          ENDIF
        ENDDO
        Monthdays = 0.0D0
        Nhru_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE nhru_summaryrun

!***********************************************************************
!     Output a set of declared variables by segment in CSV format
!***********************************************************************
      MODULE PRMS_NSEGMENT_SUMMARY
      USE PRMS_MODULE, ONLY: MAXFILE_LENGTH
      IMPLICIT NONE
! Module Variables
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nsegment_var_type(:)
      REAL, SAVE, ALLOCATABLE :: Nsegment_var_daily(:, :)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nsegment_var_dble(:, :)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmt3
      CHARACTER(LEN=16), SAVE :: MODNAME
      INTEGER, SAVE :: Daily_flag, Double_vars, Yeardays, Monthly_flag
      DOUBLE PRECISION, SAVE :: Monthdays
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nsegment_var_monthly(:, :), Nsegment_var_yearly(:, :)
! Control Parameters
      INTEGER, SAVE :: NsegmentOutVars, NsegmentOut_freq, Prms_warmup
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: NsegmentOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: NsegmentOutBaseFileName
      END MODULE PRMS_NSEGMENT_SUMMARY

!     ******************************************************************
!     nsegment results module
!     ******************************************************************
      SUBROUTINE nsegment_summary()
      USE PRMS_MODULE, ONLY: Process
      USE PRMS_NSEGMENT_SUMMARY
      IMPLICIT NONE
! Functions
      EXTERNAL :: nsegment_summarydecl, nsegment_summaryinit, nsegment_summaryrun
! Local Variables
      INTEGER :: i
!***********************************************************************
      IF ( Process(:3)=='run' ) THEN
        CALL nsegment_summaryrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL nsegment_summarydecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        CALL nsegment_summaryinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        DO i = 1, NsegmentOutVars
          IF ( Daily_flag==1 ) THEN
            IF ( Dailyunit(i)>0 ) CLOSE ( Dailyunit(i) )
          ENDIF
          IF ( NsegmentOut_freq>4 ) THEN
            IF ( Yearlyunit(i)>0 ) CLOSE ( Yearlyunit(i) )
          ENDIF
          IF ( Monthly_flag==1 ) THEN
            IF ( Monthlyunit(i)>0 ) CLOSE ( Monthlyunit(i) )
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE nsegment_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE nsegment_summarydecl()
      USE PRMS_NSEGMENT_SUMMARY
      USE PRMS_MODULE, ONLY: Model, Inputerror_flag
      IMPLICIT NONE
! Functions
      INTRINSIC CHAR
      INTEGER, EXTERNAL :: control_string_array, control_integer, control_string, declparam
      EXTERNAL read_error, print_module
! Local Variables
      INTEGER :: i
      CHARACTER(LEN=80), SAVE :: Version_nsegment_summary
!***********************************************************************
      Version_nsegment_summary = 'nsegment_summary.f90 2017-11-03 13:27:00Z'
      CALL print_module(Version_nsegment_summary, 'Nsegment Output Summary     ', 90)
      MODNAME = 'nsegment_summary'

      IF ( control_integer(NsegmentOutVars, 'nsegmentOutVars')/=0 ) NsegmentOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(NsegmentOut_freq, 'nsegmentOut_freq')/=0 ) NsegmentOut_freq = 0
      IF ( control_integer(Prms_warmup, 'prms_warmup')/=0 ) prms_warmup = 0

      IF ( NsegmentOutVars==0 ) THEN
        IF ( Model/=99 ) THEN
          PRINT *, 'ERROR, nsegment_summary requested with nsegmentOutVars equal 0'
          Inputerror_flag = 1
          RETURN
        ENDIF
      ELSE
        ALLOCATE ( NsegmentOutVar_names(NsegmentOutVars), Nsegment_var_type(NsegmentOutVars), Nc_vars(NsegmentOutVars) )
        NsegmentOutVar_names = ' '
        DO i = 1, NsegmentOutVars
          IF ( control_string_array(NsegmentOutVar_names(i), 'nsegmentOutVar_names', i)/=0 ) &
     &         CALL read_error(5, 'nsegmentOutVar_names')
        ENDDO
        IF ( control_string(NsegmentOutBaseFileName, 'nsegmentOutBaseFileName')/=0 ) CALL read_error(5, 'nsegmentOutBaseFileName')
      ENDIF

      END SUBROUTINE nsegment_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nsegment_summaryinit()
      USE PRMS_NSEGMENT_SUMMARY
      USE PRMS_MODULE, ONLY: Nsegment, MAXFILE_LENGTH, Start_year, End_year, Inputerror_flag
      IMPLICIT NONE
      INTRINSIC ABS
      INTEGER, EXTERNAL :: getvartype, numchars, getvarsize, getparam
      EXTERNAL read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: ios, ierr, size, jj, j
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
!***********************************************************************
      Begin_results = 1
      Begyr = Start_year
      IF ( Prms_warmup>0 ) Begin_results = 0
      Begyr = Begyr + Prms_warmup
      IF ( Begyr>End_year ) THEN
        PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
        Inputerror_flag = 1
      ENDIF
      Lastyear = Begyr

      WRITE ( Output_fmt, 9001 ) Nsegment

      Double_vars = 0
      ierr = 0
      DO jj = 1, NsegmentOutVars
        Nc_vars(jj) = numchars(NsegmentOutVar_names(jj))
        Nsegment_var_type(jj) = getvartype(NsegmentOutVar_names(jj)(:Nc_vars(jj)) )
        IF ( Nsegment_var_type(jj)==3 ) Double_vars = 1
        IF ( Nsegment_var_type(jj)/=2 .AND. Nsegment_var_type(jj)/=3 ) THEN
          PRINT *, 'ERROR, invalid nsegment_summary variable:', NsegmentOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only real or double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(NsegmentOutVar_names(jj)(:Nc_vars(jj)) )
        IF ( size/=Nsegment ) THEN
          PRINT *, 'ERROR, invalid nsegment_summary variable:', NsegmentOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only variables dimensioned by nsegment are allowed'
          ierr = 1
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP
      IF ( Double_vars==1 ) THEN
        ALLOCATE ( Nsegment_var_dble(Nsegment, NsegmentOutVars) )
        Nsegment_var_dble = 0.0D0
      ENDIF

      Daily_flag = 0
      IF ( NsegmentOut_freq==1 .OR. NsegmentOut_freq==3 ) THEN
        Daily_flag = 1
        ALLOCATE ( Dailyunit(NsegmentOutVars) )
        Dailyunit = 0
      ENDIF

      Monthly_flag = 0
      IF ( NsegmentOut_freq==2 .OR. NsegmentOut_freq==3 .OR. NsegmentOut_freq==4 ) Monthly_flag = 1

      IF ( NsegmentOut_freq>4 ) THEN
        Yeardays = 0
        ALLOCATE ( Nsegment_var_yearly(Nsegment, NsegmentOutVars), Yearlyunit(NsegmentOutVars) )
        Nsegment_var_yearly = 0.0D0
        Yearlyunit = 0
        WRITE ( Output_fmt3, 9003 ) Nsegment
      ENDIF
      IF ( Monthly_flag==1 ) THEN
        Monthdays = 0.0D0
        ALLOCATE ( Nsegment_var_monthly(Nsegment, NsegmentOutVars), Monthlyunit(NsegmentOutVars) )
        Nsegment_var_monthly = 0.0D0
        Monthlyunit = 0
      ENDIF

      WRITE ( Output_fmt2, 9002 ) Nsegment
      ALLOCATE ( Nsegment_var_daily(Nsegment, NsegmentOutVars) )
      Nsegment_var_daily = 0.0
      DO jj = 1, NsegmentOutVars
        IF ( Daily_flag==1 ) THEN
          fileName = NsegmentOutBaseFileName(:numchars(NsegmentOutBaseFileName))//NsegmentOutVar_names(jj)(:Nc_vars(jj))//'.csv'
          !print *, fileName
          CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', 0, ios)
          IF ( ios/=0 ) STOP 'in nsegment_summary, daily'
          WRITE ( Dailyunit(jj), Output_fmt2 ) (j, j=1,Nsegment)
        ENDIF
        IF ( NsegmentOut_freq>4 ) THEN
          IF ( NsegmentOut_freq==5 ) THEN
            fileName = NsegmentOutBaseFileName(:numchars(NsegmentOutBaseFileName))//NsegmentOutVar_names(jj)(:Nc_vars(jj))// &
      &                 '_meanyearly.csv'
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nsegment_summary, mean yearly'
          ELSE  !IF ( NsegmentOut_freq==6 ) THEN
            fileName = NsegmentOutBaseFileName(:numchars(NsegmentOutBaseFileName))//NsegmentOutVar_names(jj)(:Nc_vars(jj))// &
      &                 '_yearly.csv'
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nsegment_summary, yearly'
          ENDIF
          WRITE ( Yearlyunit(jj), Output_fmt2 ) (j, j=1,Nsegment)
        ENDIF
        IF ( Monthly_flag==1 ) THEN
          IF ( NsegmentOut_freq==4 ) THEN
            fileName = NsegmentOutBaseFileName(:numchars(NsegmentOutBaseFileName))//NsegmentOutVar_names(jj)(:Nc_vars(jj))// &
     &                 '_meanmonthly.csv'
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nsegment_summary, mean monthly'
          ELSE
            fileName = NsegmentOutBaseFileName(:numchars(NsegmentOutBaseFileName))//NsegmentOutVar_names(jj)(:Nc_vars(jj))// &
     &                  '_monthly.csv'
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', 0, ios)
            IF ( ios/=0 ) STOP 'in nsegment_summary, monthly'
          ENDIF
          WRITE ( Monthlyunit(jj), Output_fmt2 ) (j, j=1,Nsegment)
        ENDIF
      ENDDO

 9001 FORMAT ('(I4, 2(''-'',I2.2),',I6,'('',''ES10.3))')
 9002 FORMAT ('("Date "',I6,'('',''I6))')
 9003 FORMAT ('(I4,', I6,'('',''ES10.3))')

      END SUBROUTINE nsegment_summaryinit

!***********************************************************************
!     Output set of declared variables in CSV format
!***********************************************************************
      SUBROUTINE nsegment_summaryrun()
      USE PRMS_NSEGMENT_SUMMARY
      USE PRMS_MODULE, ONLY: Nsegment, Start_month, Start_day, End_year, End_month, End_day
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
      DO jj = 1, NsegmentOutVars
        IF ( Nsegment_var_type(jj)==2 ) THEN
          IF ( getvar(MODNAME, NsegmentOutVar_names(jj)(:Nc_vars(jj)), Nsegment, 'real', Nsegment_var_daily(1, jj))/=0 ) &
     &         CALL read_error(4, NsegmentOutVar_names(jj)(:Nc_vars(jj)))
        ELSEIF ( Nsegment_var_type(jj)==3 ) THEN  ! probably don't need double
          IF ( getvar(MODNAME, NsegmentOutVar_names(jj)(:Nc_vars(jj)), Nsegment, 'double', Nsegment_var_dble(1, jj))/=0 ) &
     &         CALL read_error(4, NsegmentOutVar_names(jj)(:Nc_vars(jj)))
        ENDIF
      ENDDO

      write_month = 0
      write_year = 0
      IF ( NsegmentOut_freq>4 ) THEN
        last_day = 0
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = 1
        IF ( Lastyear/=Nowyear .OR. last_day==1 ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==1 ) THEN
            DO jj = 1, NsegmentOutVars
              IF ( NsegmentOut_freq==5 ) THEN
                DO i = 1, Nsegment
                  Nsegment_var_yearly(i, jj) = Nsegment_var_yearly(i, jj)/Yeardays
                ENDDO
              ENDIF
              WRITE ( Yearlyunit(jj), Output_fmt3) Lastyear, (Nsegment_var_yearly(j,jj), j=1,Nsegment)
            ENDDO
            Nsegment_var_yearly = 0.0D0
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
        DO jj = 1, NsegmentOutVars
          IF ( Nsegment_var_type(jj)==3 ) THEN
            DO i = 1, Nsegment
              Nsegment_var_daily(i, jj) = SNGL( Nsegment_var_dble(i, jj) )
            ENDDO
          ENDIF
        ENDDO
      ENDIF

      IF ( NsegmentOut_freq>4 ) THEN
        DO jj = 1, NsegmentOutVars
          DO i = 1, Nsegment
            Nsegment_var_yearly(i, jj) = Nsegment_var_yearly(i, jj) + DBLE( Nsegment_var_daily(i, jj) )
          ENDDO
        ENDDO
        RETURN
      ENDIF

      IF ( Monthly_flag==1 ) THEN
        DO jj = 1, NsegmentOutVars
          DO i = 1, Nsegment
            Nsegment_var_monthly(i, jj) = Nsegment_var_monthly(i, jj) + DBLE( Nsegment_var_daily(i, jj) )
            IF ( write_month==1 ) THEN
              IF ( NsegmentOut_freq==4 ) Nsegment_var_monthly(i, jj) = Nsegment_var_monthly(i, jj)/Monthdays
            ENDIF
          ENDDO
        ENDDO
      ENDIF

      DO jj = 1, NsegmentOutVars
        IF ( Daily_flag==1 ) WRITE ( Dailyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
     &                               (Nsegment_var_daily(j,jj), j=1,Nsegment)
        IF ( write_month==1 ) WRITE ( Monthlyunit(jj), Output_fmt) Nowyear, Nowmonth, Nowday, &
     &                               (Nsegment_var_monthly(j,jj), j=1,Nsegment)
      ENDDO
      IF ( write_month==1 ) THEN
        Monthdays = 0.0D0
        Nsegment_var_monthly = 0.0D0
      ENDIF

      END SUBROUTINE nsegment_summaryrun

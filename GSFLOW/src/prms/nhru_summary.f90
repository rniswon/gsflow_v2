!***********************************************************************
!     Output a set of declared variables by HRU in CSV format
!***********************************************************************
      MODULE PRMS_NHRU_SUMMARY
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH, MEAN_MONTHLY
      IMPLICIT NONE
! Module Variables
      character(len=*), parameter :: MODDESC = 'Output Summary'
      character(len=*), parameter :: MODNAME = 'nhru_summary'
      character(len=*), parameter :: Version_nhru_summary = '2024-09-01'
      INTEGER, SAVE :: Begin_results, Begyr, Lastyear, nrows
      INTEGER, SAVE, ALLOCATABLE :: Dailyunit(:), Nc_vars(:), Nhru_var_type(:), Nhru_var_int(:, :)
      INTEGER, SAVE, ALLOCATABLE :: hru_ids(:), nhm_ids(:)
      REAL, SAVE, ALLOCATABLE :: Nhru_var_daily(:, :)
      REAL, SAVE, ALLOCATABLE :: real_values(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_dble(:, :), double_values(:)
      CHARACTER(LEN=48), SAVE :: Output_fmt, Output_fmt2, Output_fmtint !, Output_fmt3
      CHARACTER(LEN=48), SAVE :: Output_grid_fmt, Output_grid_fmtint, Output_date_fmt !, Output_date_fmt3, Output_fmt3int
      CHARACTER(LEN=48), SAVE :: Output_grid_last_fmt, Output_grid_last_intfmt
      INTEGER, SAVE :: Daily_flag, Double_vars, Yeardays, Monthly_flag, Integer_vars
      INTEGER, SAVE :: dates_next_year, dates_next_month, dates_next_day, selectDates_unit
      INTEGER, SAVE :: Monthdays, values_lastrow
      INTEGER :: save_year, save_month, save_day, last_day
      INTEGER, SAVE, ALLOCATABLE :: Monthlyunit(:), Yearlyunit(:)
      INTEGER, SAVE, ALLOCATABLE ::  Nhru_int_var_yearly(:,:), Nhru_int_var_monthly(:,:), integer_values(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Nhru_var_monthly(:, :), Nhru_var_yearly(:, :)
      CHARACTER(LEN=16), SAVE, ALLOCATABLE :: bin_var_names(:)
! Parameters
      INTEGER, SAVE, ALLOCATABLE :: Nhm_id(:)
! Control Parameters
      INTEGER, SAVE :: NhruOutVars, NhruOut_freq, NhruOut_format, NhruOutNcol, outputSelectDatesON_OFF 
      INTEGER, SAVE :: write_binary_nhru_flag
      CHARACTER(LEN=36), SAVE, ALLOCATABLE :: NhruOutVar_names(:)
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: NhruOutBaseFileName, selectDatesFileName
      END MODULE PRMS_NHRU_SUMMARY

!     ******************************************************************
!     nhru results module
!     ******************************************************************
      SUBROUTINE nhru_summary()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, RUN, DECL, INIT, CLEAN
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_NHRU_SUMMARY
      IMPLICIT NONE
! Functions
      EXTERNAL :: nhru_summarydecl, nhru_summaryinit, nhru_summaryrun
! Local Variables
      INTEGER :: i
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        CALL nhru_summaryrun()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL nhru_summarydecl()
      ELSEIF ( Process_flag==INIT ) THEN
        CALL nhru_summaryinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        DO i = 1, NhruOutVars
          IF ( Daily_flag==ACTIVE ) THEN
            IF ( Dailyunit(i)>0 ) CLOSE ( Dailyunit(i) )
          ENDIF
          IF ( NhruOut_freq>MEAN_MONTHLY ) THEN
            IF ( Yearlyunit(i)>0 ) CLOSE ( Yearlyunit(i) )
          ENDIF
          IF ( Monthly_flag==ACTIVE ) THEN
            IF ( Monthlyunit(i)>0 ) CLOSE ( Monthlyunit(i) )
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE nhru_summary

!***********************************************************************
!     declare parameters and variables
!***********************************************************************
      SUBROUTINE nhru_summarydecl()
      USE PRMS_CONSTANTS, ONLY: ERROR_control, DAILY, YEARLY, ACTIVE, OFF
      use PRMS_CONTROL_FILE, only: control_integer, control_string, control_string_array
      use PRMS_READ_PARAM_FILE, only: declparam
      USE PRMS_MODULE, ONLY: Nhru, NhruOutON_OFF
      USE PRMS_NHRU_SUMMARY
      use prms_utils, only: error_stop, print_module, read_error
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      CALL print_module(MODDESC, MODNAME, Version_nhru_summary)

      IF ( control_integer(NhruOutVars, 'nhruOutVars')/=0 ) NhruOutVars = 0
      ! 1 = daily, 2 = monthly, 3 = both, 4 = mean monthly, 5 = mean yearly, 6 = yearly total
      IF ( control_integer(NhruOut_freq, 'nhruOut_freq')/=0 ) NhruOut_freq = 0
      IF ( NhruOut_freq<DAILY .OR. NhruOut_freq>YEARLY ) CALL error_stop('invalid nhruOut_freq value', ERROR_control)
      ! 1 = ES10.3; 2 = F0.2; 3 = F0.3; 4 = F0.4; 5 = F0.5
      IF ( control_integer(NhruOut_format, 'nhruOut_format')/=0 ) NhruOut_format = 1
      IF ( NhruOut_format<1 .OR. NhruOut_format>5 ) CALL error_stop('invalid nhruOut_format value', ERROR_control)
      IF ( control_integer(NhruOutNcol, 'nhruOutNcol')/=0 ) NhruOutNcol = 0

      IF ( control_integer(outputSelectDatesON_OFF, 'outputSelectDatesON_OFF')/=0 ) outputSelectDatesON_OFF = OFF
      IF ( outputSelectDatesON_OFF==ACTIVE ) THEN
        IF ( control_string(selectDatesFileName, 'selectDatesFileName')/=0 ) CALL read_error(5, 'selectDatesFileName')
      ENDIF

      ! 0 = text; 1 = year, year month, or year month day then values; 2 = capitalized file names, flopy format
      ! format of file for flopy reader for write_binary_nhru_flag = 2
      IF ( control_integer(write_binary_nhru_flag, 'write_binary_nhru_flag')/=0 ) write_binary_nhru_flag = 0

      IF ( NhruOutVars==0 ) THEN
        CALL error_stop('nhru_summary requested with nhruOutVars equal 0', ERROR_control)
      ELSE
        ALLOCATE ( NhruOutVar_names(NhruOutVars), Nhru_var_type(NhruOutVars), Nc_vars(NhruOutVars) )
        NhruOutVar_names = ' '
        DO i = 1, NhruOutVars
          IF ( control_string_array(NhruOutVar_names(i), 'nhruOutVar_names', i)/=0 ) CALL read_error(5, 'nhruOutVar_names')
        ENDDO
        IF ( control_string(NhruOutBaseFileName, 'nhruOutBaseFileName')/=0 ) CALL read_error(5, 'nhruOutBaseFileName')
        IF ( write_binary_nhru_flag == 2 ) THEN
          ALLOCATE ( bin_var_names(NhruOutVars) )
          bin_var_names = ' '
        ENDIF
      ENDIF

! Declared Parameters
      IF ( NhruOutON_OFF==2 ) THEN
        ALLOCATE ( Nhm_id(Nhru) )
        IF ( declparam(MODNAME, 'nhm_id', 'nhru', 'integer', &
     &       '1', '1', '9999999', &
     &       'National Hydrologic Model HRU ID', 'National Hydrologic Model HRU ID', &
     &       'none') /= 0 ) CALL read_error(1, 'nhm_id')
      ENDIF

      END SUBROUTINE nhru_summarydecl

!***********************************************************************
!     Initialize module values
!***********************************************************************
      SUBROUTINE nhru_summaryinit()
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH, ERROR_open_out, &
     &    DAILY, MONTHLY, DAILY_MONTHLY, MEAN_MONTHLY, MEAN_YEARLY, YEARLY, ACTIVE, OFF, REAL_TYPE, DBLE_TYPE, INT_TYPE
      use PRMS_MMFAPI, only: getvartype, getvarsize
      use PRMS_READ_PARAM_FILE, only: getparam_int
      USE PRMS_MODULE, ONLY: Nhru, NhruOutON_OFF, Prms_warmup, Start_year, Start_month, Start_day, Inputerror_flag
      USE PRMS_NHRU_SUMMARY
      use prms_utils, only: error_stop, find_current_file_time, find_header_end, numchars, PRMS_open_output_file, read_error
      IMPLICIT NONE
      INTRINSIC :: MIN
      EXTERNAL :: write_header_date, to_upper
! Local Variables
      INTEGER :: ios, ierr, size, jj
      INTEGER :: nc
      CHARACTER(LEN=MAXFILE_LENGTH) :: fileName
      CHARACTER(LEN=4) ::file_suffix
!***********************************************************************
      Begin_results = ACTIVE
      IF ( Prms_warmup>0 ) Begin_results = OFF
      Begyr = Start_year + Prms_warmup
      Lastyear = Begyr

      IF ( outputSelectDatesON_OFF==ACTIVE ) THEN
        CALL find_header_end(selectDates_unit, selectDatesFileName, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(selectDates_unit, Start_year, Start_month, Start_day, &
     &                                dates_next_year, dates_next_month, dates_next_day)
        ELSE
          ierr = 1
        ENDIF
      ENDIF

      IF ( NhruOutNcol<1 .OR. write_binary_nhru_flag == 2 ) NhruOutNcol = Nhru
      nrows = Nhru / NhruOutNcol
      values_lastrow = Nhru - (NhruOutNcol * nrows)
      IF ( values_lastrow > 0 ) nrows = nrows + 1

      Output_fmt = ' '
      Output_grid_fmt = ' '
      Output_grid_last_fmt = ' '
      Output_grid_fmtint = ' '
      Output_date_fmt = ' '
      Output_fmt2 = ' '
      !Output_fmt3 = ' '
      !Output_date_fmt3 = ' '
      Output_fmtint = ' '
      !Output_fmt3int = ' '
      Output_grid_last_intfmt = ' '

      IF ( NhruOut_format==1 ) THEN
        WRITE ( Output_fmt, 9001 ) Nhru
        WRITE ( Output_grid_fmt, 9014 ) NhruOutNcol - 1
        IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_fmt, 9014 ) values_lastrow - 1
      ELSEIF ( NhruOut_format==2 ) THEN
        WRITE ( Output_fmt, 9007 ) Nhru
        WRITE ( Output_grid_fmt, 9017 ) NhruOutNcol - 1
        IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_fmt, 9017 ) values_lastrow - 1
      ELSEIF ( NhruOut_format==3 ) THEN
        WRITE ( Output_fmt, 9006 ) Nhru
        WRITE ( Output_grid_fmt, 9016 ) NhruOutNcol - 1
        IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_fmt, 9016 ) values_lastrow - 1
      ELSEIF ( NhruOut_format==4 ) THEN
        WRITE ( Output_fmt, 9005 ) Nhru
        WRITE ( Output_grid_fmt, 9015 ) NhruOutNcol - 1
        IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_fmt, 9015 ) values_lastrow - 1
      ELSEIF ( NhruOut_format==5 ) THEN
        WRITE ( Output_fmt, 9012 ) Nhru
        WRITE ( Output_grid_fmt, 9014 ) NhruOutNcol - 1
        IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_fmt, 9014 ) values_lastrow - 1
      ENDIF
      WRITE ( Output_fmtint, 9004 ) Nhru
      WRITE ( Output_grid_fmtint, 9018 ) NhruOutNcol - 1
      IF ( values_lastrow > 0 ) WRITE ( Output_grid_last_intfmt, 9018 ) values_lastrow - 1
      WRITE ( Output_date_fmt, 9013 )
      !print *, Output_grid_fmt, '  ', Output_date_fmt, '  ', Output_grid_last_fmt, '  ', Output_grid_last_intfmt

      Double_vars = OFF
      Integer_vars = OFF
      ierr = 0
      DO jj = 1, NhruOutVars
        Nc_vars(jj) = numchars(NhruOutVar_names(jj))
        Nhru_var_type(jj) = getvartype(NhruOutVar_names(jj)(:Nc_vars(jj)))
        IF ( Nhru_var_type(jj)==DBLE_TYPE ) Double_vars = ACTIVE
        IF ( Nhru_var_type(jj)==INT_TYPE ) Integer_vars = ACTIVE
        IF ( Nhru_var_type(jj)/=REAL_TYPE .AND. Nhru_var_type(jj)/=DBLE_TYPE .AND. Nhru_var_type(jj)/=INT_TYPE ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only integer, real or double variables allowed'
          ierr = 1
        ENDIF
        size = getvarsize(NhruOutVar_names(jj)(:Nc_vars(jj)))
        IF ( size/=Nhru ) THEN
          PRINT *, 'ERROR, invalid nhru_summary variable:', NhruOutVar_names(jj)(:Nc_vars(jj))
          PRINT *, '       only variables dimensioned by nhru, nssr, or ngw allowed'
          ierr = 1
        ENDIF
        IF ( write_binary_nhru_flag == 2 ) THEN
          nc = MIN( Nc_vars(jj), 16 )
          bin_var_names(jj) = NhruOutVar_names(jj)(:nc)
          CALL to_upper( bin_var_names(jj) )
        ENDIF
      ENDDO
      IF ( ierr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF
      IF ( Double_vars==ACTIVE ) THEN
        ALLOCATE ( Nhru_var_dble(Nhru, NhruOutVars) )
        Nhru_var_dble = 0.0D0
      ENDIF
      IF ( Integer_vars==ACTIVE ) THEN
        ALLOCATE ( Nhru_var_int(Nhru, NhruOutVars) )
        Nhru_var_int = 0
      ENDIF

      Daily_flag = OFF
      IF ( NhruOut_freq==DAILY .OR. NhruOut_freq==DAILY_MONTHLY ) THEN
        Daily_flag = ACTIVE
        ALLOCATE ( Dailyunit(NhruOutVars) )
        Dailyunit = 0
      ENDIF

      Monthly_flag = OFF
      IF ( NhruOut_freq==MONTHLY .OR. NhruOut_freq==DAILY_MONTHLY .OR. NhruOut_freq==MEAN_MONTHLY ) Monthly_flag = ACTIVE

      IF ( NhruOut_freq>MEAN_MONTHLY ) THEN
        Yeardays = 0
        ALLOCATE ( Nhru_var_yearly(Nhru, NhruOutVars), Yearlyunit(NhruOutVars) )
        Nhru_var_yearly = 0.0D0
        Yearlyunit = 0
        ALLOCATE ( Nhru_int_var_yearly(Nhru, NhruOutVars) )
        Nhru_int_var_yearly = 0
        !IF ( NhruOut_format==1 ) THEN ! yearly now writes nowyear, nowmonth, nowday instead of just year so daily, monthly and yearly use full date
        !  WRITE ( Output_fmt3, 9003 ) Nhru
        !ELSEIF ( NhruOut_format==2 ) THEN
        !  WRITE ( Output_fmt3, 9010 ) Nhru
        !ELSEIF ( NhruOut_format==3 ) THEN
        !  WRITE ( Output_fmt3, 9009 ) Nhru
        !ELSEIF ( NhruOut_format==4 ) THEN
        !  WRITE ( Output_fmt3, 9008 ) Nhru
        !ELSEIF ( NhruOut_format==5 ) THEN
        !  WRITE ( Output_fmt3, 9011 ) Nhru
        !ENDIF
        !WRITE ( Output_date_fmt3, "(A)" ) '(I0)'
        !WRITE ( Output_fmt3int, 9019 ) NhruOutNcol - 1
      ELSEIF ( Monthly_flag==ACTIVE ) THEN
        Monthdays = 0
        ALLOCATE ( Nhru_var_monthly(Nhru, NhruOutVars), Monthlyunit(NhruOutVars) )
        Nhru_var_monthly = 0.0D0
        Monthlyunit = 0
        ALLOCATE( Nhru_int_var_monthly(Nhru, NhruOutVars) )
        Nhru_int_var_monthly = 0
      ENDIF

      IF ( NhruOutON_OFF==2 ) THEN
        IF ( getparam_int(MODNAME, 'nhm_id', Nhru, Nhm_id)/=0 ) CALL read_error(2, 'nhm_id')
      ELSE
        ALLOCATE ( hru_ids(nhru) )
        DO jj = 1, nhru
          hru_ids(jj) = jj
        ENDDO
      ENDIF
      WRITE ( Output_fmt2, 9002 ) Nhru
      ALLOCATE ( Nhru_var_daily(Nhru, NhruOutVars) )
      Nhru_var_daily = 0.0
      file_suffix = '.csv'
      IF ( write_binary_nhru_flag > 0 ) file_suffix = '.bin'
      ALLOCATE( real_values(Nhru), integer_values(Nhru), double_values(Nhru) )
      real_values = 0.0
      double_values = 0.0D0
      integer_values = 0
      DO jj = 1, NhruOutVars
        IF ( Daily_flag==ACTIVE ) THEN
          fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//file_suffix
          CALL PRMS_open_output_file(Dailyunit(jj), fileName, 'xxx', write_binary_nhru_flag, ios)
          IF ( ios/=0 ) CALL error_stop('in nhru_summary, daily', ERROR_open_out)
          IF ( write_binary_nhru_flag /= 2 ) CALL write_header_date( Dailyunit(jj) )
        ENDIF
        IF ( NhruOut_freq>MEAN_MONTHLY ) THEN
          IF ( NhruOut_freq==MEAN_YEARLY ) THEN
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_meanyearly'//file_suffix
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', write_binary_nhru_flag, ios)
            IF ( ios/=0 ) CALL error_stop('in nhru_summary, mean yearly', ERROR_open_out)
          ELSE  !IF ( NhruOut_freq==YEARLY ) THEN
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_yearly'//file_suffix
            CALL PRMS_open_output_file(Yearlyunit(jj), fileName, 'xxx', write_binary_nhru_flag, ios)
            IF ( ios/=0 ) CALL error_stop('in nhru_summary, yearly', ERROR_open_out)
          ENDIF
          IF ( write_binary_nhru_flag /= 2 ) CALL write_header_date( Yearlyunit(jj) )
        ENDIF
        IF ( Monthly_flag==ACTIVE ) THEN
          IF ( NhruOut_freq==MEAN_MONTHLY ) THEN
            IF ( write_binary_nhru_flag == 0 ) THEN
              fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))// &
     &                   '_meanmonthly.csv'
            ELSE
              fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))// &
     &                   '_meanmonthly.bin'
            ENDIF
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', write_binary_nhru_flag, ios)
            IF ( ios/=0 ) CALL error_stop('in nhru_summary, mean monthly', ERROR_open_out)
          ELSE
            fileName = NhruOutBaseFileName(:numchars(NhruOutBaseFileName))//NhruOutVar_names(jj)(:Nc_vars(jj))//'_monthly'//file_suffix
            CALL PRMS_open_output_file(Monthlyunit(jj), fileName, 'xxx', write_binary_nhru_flag, ios)
            IF ( ios/=0 ) CALL error_stop('in nhru_summary, monthly', ERROR_open_out)
          ENDIF
          IF ( write_binary_nhru_flag /= 2 ) CALL write_header_date( Monthlyunit(jj) )
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
      USE PRMS_CONSTANTS, ONLY: DAILY, MEAN_YEARLY, ACTIVE, OFF, REAL_TYPE, DBLE_TYPE, INT_TYPE
      use PRMS_MMFAPI, only: getvar_dble, getvar_int, getvar_real
      USE PRMS_MODULE, ONLY: Nhru, Start_month, Start_day, End_year, End_month, End_day, Nowyear, Nowmonth, Nowday
      USE PRMS_NHRU_SUMMARY
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      USE PRMS_SET_TIME, ONLY: Modays
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions and subroutines
      INTRINSIC :: DBLE
      EXTERNAL :: write_CBH_values, read_event_date
      DOUBLE PRECISION :: yeardays_dble, monthdays_dble
! Local Variables
      INTEGER :: j, i, jj, write_month, write_date
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
      DO jj = 1, NhruOutVars
        IF ( Nhru_var_type(jj)==REAL_TYPE ) THEN
          CALL getvar_real(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, Nhru_var_daily(:, jj))
        ELSEIF ( Nhru_var_type(jj)==DBLE_TYPE ) THEN
          CALL getvar_dble(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, Nhru_var_dble(:, jj))
        ELSE !IF ( Nhru_var_type(jj)==INT_TYPE ) THEN
          CALL getvar_int(MODNAME, NhruOutVar_names(jj)(:Nc_vars(jj)), Nhru, Nhru_var_int(:, jj))
        ENDIF
        IF ( Daily_flag==ACTIVE ) THEN
          write_date = 1
          IF ( outputSelectDatesON_OFF==ACTIVE ) THEN
            write_date = 0
            IF ( Nowyear==dates_next_year .AND. Nowmonth==dates_next_month.AND. Nowday==dates_next_day ) write_date = 1
          ENDIF
          IF ( write_date==1 ) THEN
            IF ( Nhru_var_type(jj)==REAL_TYPE ) THEN
              DO j = 1, Active_hrus
                i = Hru_route_order(j)
                real_values(i) = Nhru_var_daily(i, jj)
              ENDDO
            ELSEIF ( Nhru_var_type(jj)==DBLE_TYPE ) THEN
              DO j = 1, Active_hrus
                i = Hru_route_order(j)
                double_values(i) = Nhru_var_dble(i, jj)
              ENDDO
            ELSE !IF ( Nhru_var_type(jj)==INT_TYPE )
              DO j = 1, Active_hrus
                i = Hru_route_order(j)
                integer_values(i) = Nhru_var_int(i, jj)
              ENDDO
            ENDIF
            CALL write_CBH_values(jj, Dailyunit(jj), Nhru_var_type(jj), 1)
          ENDIF
        ENDIF
      ENDDO
      IF ( Daily_flag==ACTIVE ) THEN
        IF ( outputSelectDatesON_OFF==ACTIVE ) THEN
          IF ( write_date==1 ) CALL read_event_date(selectDates_unit, dates_next_year, dates_next_month, dates_next_day)
        ENDIF
        IF ( Monthly_flag==OFF ) RETURN
      ENDIF

      IF ( NhruOut_freq>MEAN_MONTHLY ) THEN
        last_day = OFF
        IF ( Nowyear==End_year .AND. Nowmonth==End_month .AND. Nowday==End_day ) last_day = ACTIVE
        IF ( Lastyear/=Nowyear .OR. last_day==ACTIVE ) THEN
          IF ( (Nowmonth==Start_month .AND. Nowday==Start_day) .OR. last_day==ACTIVE ) THEN
            DO jj = 1, NhruOutVars
              IF ( NhruOut_freq==MEAN_YEARLY ) THEN
                IF ( Nhru_var_type(jj) /= INT_TYPE ) THEN
                  yeardays_dble = DBLE( Yeardays )
                  DO j = 1, Active_hrus
                    i = Hru_route_order(j)
                    double_values(i) = Nhru_var_yearly(i,jj) / yeardays_dble
                  ENDDO
                ELSE !IF ( Nhru_var_type(jj)==INT_TYPE ) THEN
                  DO j = 1, Active_hrus
                    i = Hru_route_order(j)
                    integer_values(i) = Nhru_int_var_yearly(i,jj) / Yeardays
                  ENDDO
                ENDIF
              ELSE
                IF ( Nhru_var_type(jj) /= INT_TYPE ) THEN
                  DO j = 1, Active_hrus
                    i = Hru_route_order(j)
                    double_values(i) = Nhru_var_yearly(i,jj)
                  ENDDO
                ELSE
                  DO j = 1, Active_hrus
                    i = Hru_route_order(j)
                    integer_values(i) = Nhru_int_var_yearly(i,jj)
                  ENDDO
                ENDIF
              ENDIF
              IF ( last_day==ACTIVE ) THEN
                save_year = Nowyear
                save_month = Nowmonth
                save_day = Nowday
              ENDIF
              CALL write_CBH_values(jj, Yearlyunit(jj), Nhru_var_type(jj), 3)
            ENDDO
            Nhru_var_yearly = 0.0D0
            Nhru_int_var_yearly = 0
            Yeardays = 0
            Lastyear = Nowyear
          ENDIF
        ENDIF
        DO jj = 1, NhruOutVars
          IF ( Nhru_var_type(jj)==REAL_TYPE ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) + DBLE( Nhru_var_daily(i, jj) )
            ENDDO
          ELSEIF ( Nhru_var_type(jj)==DBLE_TYPE ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_var_yearly(i, jj) = Nhru_var_yearly(i, jj) + Nhru_var_dble(i, jj)
            ENDDO
          ELSE !IF ( Nhru_var_type(jj)==INT_TYPE )
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              Nhru_int_var_yearly(i, jj) = Nhru_int_var_yearly(i, jj) + Nhru_var_int(i, jj)
            ENDDO
          ENDIF
        ENDDO
        Yeardays = Yeardays + 1
        save_year = Nowyear
        save_month = Nowmonth
        save_day = Nowday
        RETURN
      ENDIF

! monthly values
      write_month = OFF
      ! check for last day of month and simulation
      IF ( Nowday==Modays(Nowmonth) ) THEN
        write_month = ACTIVE
      ELSEIF ( Nowyear==End_year ) THEN
        IF ( Nowmonth==End_month ) THEN
          IF ( Nowday==End_day ) write_month = ACTIVE
        ENDIF
      ENDIF
      Monthdays = Monthdays + 1
      monthdays_dble = DBLE( Monthdays )
      DO jj = 1, NhruOutVars
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Nhru_var_type(jj)==REAL_TYPE ) THEN
            Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) + DBLE( Nhru_var_daily(i, jj) )
          ELSEIF ( Nhru_var_type(jj)==DBLE_TYPE ) THEN
            Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) + Nhru_var_dble(i, jj)
          ELSE !IF ( Nhru_var_type(jj)==INT_TYPE ) THEN
            Nhru_int_var_monthly(i, jj) = Nhru_int_var_monthly(i, jj) + Nhru_var_int(i, jj)
          ENDIF
          IF ( NhruOut_freq==MEAN_MONTHLY .AND. write_month==ACTIVE ) THEN
            IF ( Nhru_var_type(jj)/=INT_TYPE ) THEN
              Nhru_var_monthly(i, jj) = Nhru_var_monthly(i, jj) / monthdays_dble
            ELSE !IF ( Nhru_var_type(jj)==INT_TYPE ) THEN
              Nhru_int_var_monthly(i, jj) = Nhru_int_var_monthly(i, jj)/Monthdays
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      IF ( write_month==ACTIVE ) THEN
        DO jj = 1, NhruOutVars
          IF ( Nhru_var_type(jj)/=INT_TYPE ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              double_values(i) = Nhru_var_monthly(i, jj)
            ENDDO
          ELSE !IF ( Nhru_var_type(jj)==INT_TYPE ) THEN
            DO j = 1, Active_hrus
              i = Hru_route_order(j)
              integer_values(i) = Nhru_int_var_monthly(i, jj)
            ENDDO
          ENDIF
          CALL write_CBH_values(jj, Monthlyunit(jj), Nhru_var_type(jj), 2)
        ENDDO
        Monthdays = 0
        Nhru_var_monthly = 0.0D0
        Nhru_int_var_monthly = 0
      ENDIF

      END SUBROUTINE nhru_summaryrun

!*****************************
! Read event for a source type
!*****************************
      SUBROUTINE read_event_date(Iunit, Next_yr, Next_mo, Next_day)
      USE PRMS_MODULE, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF
      use prms_utils, only: is_eof
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit
      INTEGER, INTENT (INOUT) :: Next_yr, Next_mo, Next_day
! Local Variables
      INTEGER :: keep_reading
!*******************************************************************************
      IF ( Next_mo==0 ) RETURN ! already found end of file
      keep_reading = ACTIVE
      DO WHILE ( keep_reading==ACTIVE )
        IF ( Next_yr==Nowyear .AND. Next_mo==Nowmonth .AND. Next_day==Nowday ) THEN
          READ ( Iunit, * ) Next_yr, Next_mo, Next_day
          CALL is_eof(Iunit, Next_yr, Next_mo, Next_day)
          IF ( Next_mo==0 ) keep_reading = OFF
        ELSE
          keep_reading = OFF
        ENDIF
      ENDDO
      END SUBROUTINE read_event_date

!*****************************
! Write CBH date header
!*****************************
      SUBROUTINE write_header_date(Iunit)
      USE PRMS_CONSTANTS, ONLY: OFF
      USE PRMS_MODULE, ONLY: NhruOutON_OFF
      USE PRMS_NHRU_SUMMARY, ONLY: write_binary_nhru_flag, Output_fmt2, Nhm_id, hru_ids
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit
!*******************************************************************************
      IF ( NhruOutON_OFF==1 ) THEN
        IF ( write_binary_nhru_flag == OFF ) THEN
          WRITE ( Iunit, Output_fmt2 ) hru_ids
        ELSE
          WRITE ( Iunit ) 'Date', hru_ids
        ENDIF
      ELSE ! can only be write_binary_nhru_flag = 1
        IF ( write_binary_nhru_flag == OFF ) THEN
          WRITE ( Iunit, Output_fmt2 ) Nhm_id
        ELSE
          WRITE ( Iunit ) 'Date', Nhm_id
        ENDIF
      ENDIF

      END SUBROUTINE write_header_date

!*****************************
! Write CBH values
!*****************************
      SUBROUTINE write_CBH_values( ivar, iunit, var_type, outtype )
      USE PRMS_CONSTANTS, ONLY: INT_TYPE, REAL_TYPE, ACTIVE
      USE PRMS_MODULE, ONLY: Nhru, Nowyear, Nowmonth, Nowday, Timestep, GSFLOW_flag
      USE GSFMODFLOW, ONLY: KSTP, KPER
      USE GWFBASMODULE, ONLY: TOTIM, PERTIM
      USE PRMS_NHRU_SUMMARY
      IMPLICIT NONE
! Functions
      INTRINSIC :: FLOAT
! Arguments
      INTEGER, INTENT(IN) :: ivar, iunit, var_type, outtype ! 1 = daily; 2 = monthly; 3 = yearly
! Local Variables
      INTEGER :: j, jj, first, last, rows, year, month, day
!*******************************************************************************
      IF ( NhruOut_freq>MEAN_MONTHLY .AND. last_day == 0 ) THEN
        year = save_year
        month = save_month
        day = save_day
      ELSE
        year = Nowyear
        month = Nowmonth
        day = Nowday
      ENDIF
      IF ( NhruOutNcol == Nhru ) THEN
        IF ( write_binary_nhru_flag == 0 ) THEN
          IF ( var_type == INT_TYPE ) THEN
            WRITE ( iunit, Output_fmtint ) year, month, day, integer_values
          ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
            WRITE ( iunit, Output_fmt ) year, month, day, real_values
          ELSE !var_type = DBLE_TYPE or outtype = 2 or 3
            WRITE ( iunit, Output_fmt ) year, month, day, double_values
          ENDIF
        ELSEIF ( write_binary_nhru_flag == 1 ) THEN
          IF ( var_type == INT_TYPE ) THEN
            WRITE ( iunit ) year, month, day, integer_values
          ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
            WRITE ( iunit ) year, month, day, real_values
          ELSE !IF ( var_type = DBLE_TYPE ) THEN
            WRITE ( iunit, Output_fmt ) year, month, day, double_values
          ENDIF
        ELSE ! IF ( write_binary_nhru_flag == 2 ) THEN flopy format
          ! Record 1: KSTP,KPER,PERTIM,TOTIM,TEXT,NHRU,1,1
          !   where
          !   KSTP is the time step number;
          !   KPER is the stress period number;
          !   PERTIM is the time value for the current stress period
          !   TOTIM is the total simulation time;
          !   TEXT is a character string (character*16);
          !   DATA is HRU data of size (NHRU).
          ! Record 2: (DATA(N),N=1,NHRU)
          ! flopy format
          IF ( GSFLOW_flag == ACTIVE ) THEN
            WRITE ( iunit ) KSTP, KPER, PERTIM, TOTIM, bin_var_names(ivar), Nhru, 1, 1
          ELSE
            WRITE ( iunit ) year, month, day, FLOAT(Timestep), bin_var_names(ivar), Nhru, 1, 1
          ENDIF
          IF ( var_type /= INT_TYPE ) THEN
            IF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
              WRITE ( iunit ) real_values
            ELSE !IF ( var_type = DBLE_TYPE ) THEN
              WRITE ( iunit, Output_fmt ) double_values
            ENDIF
          ELSE
            WRITE ( iunit ) integer_values
          ENDIF
        ENDIF
      ELSE ! gridded
        IF ( write_binary_nhru_flag == 0 ) THEN
          IF ( outtype == 3 ) THEN
            WRITE ( iunit, '(I0)' ) Lastyear
          ELSE
            WRITE ( iunit, Output_date_fmt ) year, month, day
          ENDIF
          first = 1
          rows = nrows
          IF ( values_lastrow > 0 ) rows = rows - 1
          DO jj = 1, rows
            last = first + NhruOutNcol - 1
            IF ( var_type == INT_TYPE ) THEN
              WRITE ( iunit, Output_grid_fmtint ) (integer_values(j), j=first,last)
            ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
              WRITE ( iunit, Output_grid_fmt )  (real_values(j), j=first,last)
            ELSE !IF ( var_type = DBLE_TYPE ) THEN
              WRITE ( iunit, Output_grid_fmt )  (double_values(j), j=first,last)
            ENDIF
            first = last + 1
          ENDDO
          IF ( values_lastrow > 0 ) THEN
            IF ( var_type == INT_TYPE ) THEN
              WRITE ( iunit, Output_grid_last_intfmt ) (integer_values(j), j=first,last)
            ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
              WRITE ( iunit, Output_grid_last_fmt )  (real_values(j), j=first,last)
            ELSE !IF ( var_type = DBLE_TYPE ) THEN
              WRITE ( iunit, Output_grid_last_fmt )  (double_values(j), j=first,last)
            ENDIF
          ENDIF
        ELSE
          IF ( outtype == 3 ) THEN
            WRITE ( iunit ) Lastyear
          ELSE
            WRITE ( iunit ) year, month, day
          ENDIF
          first = 1
          rows = nrows
          IF ( values_lastrow > 0 ) rows = rows - 1
          DO jj = 1, rows
            last = first + NhruOutNcol - 1
            IF ( var_type == INT_TYPE ) THEN
              WRITE ( iunit ) (integer_values(j), j=first,last)
            ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
              WRITE ( iunit ) (real_values(j), j=first,last)
            ELSE
              WRITE ( iunit ) (double_values(j), j=first,last)
            ENDIF
            first = last + 1
          ENDDO
          IF ( values_lastrow > 0 ) THEN
            IF ( var_type == INT_TYPE ) THEN
              WRITE ( iunit ) (integer_values(j), j=first,last)
            ELSEIF ( outtype == 1 .AND. var_type == REAL_TYPE ) THEN
              WRITE ( iunit ) (real_values(j), j=first,last)
            ELSE
              WRITE ( iunit ) (double_values(j), j=first,last)
            ENDIF
          ENDIF
          WRITE ( iunit ) Lastyear
          IF ( var_type /= INT_TYPE ) THEN
            WRITE ( iunit ) (real_values(j), j=first,last)
          ELSE
            WRITE ( iunit ) (integer_values(j), j=first,last)
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE write_CBH_values

!*****************************
! Convert String to Upper Case
!*****************************
      SUBROUTINE to_upper( string )
      use prms_utils, only: error_stop, numchars
! Arguments
      CHARACTER(LEN=*), INTENT(INOUT) :: string
! Functions
      INTRINSIC :: ICHAR, CHAR
! Local Variables
      INTEGER :: idiff, nchars, i
!*******************************************************************************
      idiff = ICHAR('a') - ICHAR('A')
      nchars = numchars(string)
      DO i = 1, nchars
        IF ( string(i:i) >= 'a' .AND. string(i:i) <= 'z') string(i:i) = CHAR(ICHAR(string(i:i)) - idiff)
      ENDDO
      END SUBROUTINE to_upper

!***********************************************************************
! Read PRMS Data File
!***********************************************************************
      MODULE PRMS_DATA_FILE
        USE PRMS_CONSTANTS, ONLY: ERROR_open_in, ERROR_read
        INTEGER, SAVE :: Num_datafile_types, Num_datafile_columns, Datafile_unit
        CHARACTER(LEN=16), ALLOCATABLE, SAVE :: Data_varname(:)
        INTEGER, ALLOCATABLE, SAVE :: Data_varnum(:)
        REAL, ALLOCATABLE, SAVE :: Data_line_values(:)
        CHARACTER(LEN=1), ALLOCATABLE :: data_transfer(:)
      END MODULE PRMS_DATA_FILE

      SUBROUTINE read_prms_data_file
      USE PRMS_DATA_FILE
      USE PRMS_MODULE, ONLY: PRMS_output_unit, MAXFILE_LENGTH, EQULS, Print_debug, Starttime, Endtime
      IMPLICIT NONE
      ! Functions
      INTRINSIC LEN_TRIM, TRIM
      EXTERNAL read_error, write_outfile, PRMS_open_input_file, find_current_time, print_module, error_stop
      INTEGER, EXTERNAL :: control_string, numchars, check_data_values
      ! Local Variables
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Read Data File'
        character(len=*), parameter :: MODNAME = 'read_data_file'
        character(len=*), parameter :: Version_read_data_file = '2021-02-09'
      CHARACTER(LEN=MAXFILE_LENGTH) :: data_filename, data_line, dmy
      CHARACTER(LEN=80) :: line
      INTEGER n, ierr, ios, numchrs, length
      INTEGER startyr, startmo, startdy, starthr, startmn, startsec
      INTEGER endyr, endmo, enddy, endhr, endmn, endsec, num_vars
      REAL, ALLOCATABLE :: var(:)
!***********************************************************************
      CALL print_module(MODDESC, MODNAME, Version_read_data_file)

      IF ( control_string(data_filename, 'data_file')/=0 ) CALL read_error(5, 'data_file')
      CALL PRMS_open_input_file(Datafile_unit, data_filename, 'data_file', 0, ios)
      IF ( ios/=0 ) ERROR STOP ERROR_open_in
      CALL write_outfile(' ')
      CALL write_outfile(EQULS)
      CALL write_outfile('Using PRMS Data File: '//data_filename)
! Echo Data File Header and comment lines
      READ ( Datafile_unit, FMT='(A)', IOSTAT=ios ) line
      IF ( ios/=0 ) CALL read_error(13, 'title')
      CALL write_outfile('Title: '//TRIM(line))
      CALL write_outfile(EQULS)
      CALL write_outfile('Comment lines:')
      num_vars = 0
      DO
        READ ( Datafile_unit, FMT='(A)', IOSTAT=ios ) line
        IF ( ios==-1 ) CALL read_error(13, 'invalid Data File, end of file reached')
        IF ( ios/=0 ) CALL read_error(13, 'comment')
        IF ( line(:4)=='    ' ) CYCLE
        IF ( line(:2)=='//' .OR. line(:1)=='"' ) CALL write_outfile(TRIM(line))
        num_vars = num_vars + 1
        IF ( line(:4)=='####' ) EXIT
      ENDDO
      IF ( line(:4)/='####' ) CALL error_stop('invalid Data File, data section not found')
      CALL write_outfile(EQULS)
      CALL write_outfile('measured variables')

! read variables and number
      REWIND ( Datafile_unit )
      READ ( Datafile_unit, FMT='(A)' ) line ! skip first line
      ALLOCATE ( Data_varname(num_vars), Data_varnum(num_vars) )
      Data_varname = '                '
      Data_varnum = 0
      Num_datafile_columns = 0
      Num_datafile_types = 0
      ierr = 0
      DO
        READ ( Datafile_unit, FMT='(A)' ) line
        IF ( line(:4)=='    ' .OR. line(:2)=='//' .OR. line(:1)=='"' ) CYCLE
        IF ( line(:4)=='####' ) EXIT
        length = LEN_TRIM(line)
        CALL write_outfile(line(:length))
        numchrs = numchars(line(:length))
        READ ( line(numchrs+1:length), * ) n
        IF ( n==0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Varaible: ', line(:numchrs), ' ignored as number of values = 0'
          CYCLE
        ENDIF
        Num_datafile_types = Num_datafile_types + 1
        Data_varname(Num_datafile_types) = line(:numchrs)
        Data_varnum(Num_datafile_types) = n
        Num_datafile_columns = Num_datafile_columns + n
        ALLOCATE ( var(n) )
        CALL check_data_variables(Data_varname(Num_datafile_types), n, var, 0, ios)
        DEALLOCATE ( var )
        IF ( ios/=0 ) ierr = ios
      ENDDO
      IF ( ierr==1 ) ERROR STOP ERROR_read
      CALL write_outfile(EQULS)
      ALLOCATE ( Data_line_values(Num_datafile_columns) )

      READ ( Datafile_unit, *, IOSTAT=ios ) startyr, startmo, startdy, starthr, startmn, startsec
      IF ( ios/=0 ) CALL read_error(13, 'first data line')
      DO
        READ ( Datafile_unit, '(A)', IOSTAT=ios ) dmy
        IF ( ios==-1 ) EXIT ! found end of file
        IF ( dmy(:4)=='    ' ) THEN ! assume a blank line is the end of the Data File
          BACKSPACE Datafile_unit
          BACKSPACE Datafile_unit
          READ ( Datafile_unit, '(A)', IOSTAT=ios ) data_line
          EXIT
        ENDIF
        IF ( ios/=0 ) CALL read_error(13, 'data line')
        data_line = dmy
      ENDDO
      READ ( data_line, *, IOSTAT=ios ) endyr, endmo, enddy, endhr, endmn, endsec
      WRITE ( PRMS_output_unit, 10 ) ' Data File', startyr, startmo, startdy, starthr, startmn, startsec, endyr, endmo, &
     &                               enddy, endhr, endmn, endsec
 10   FORMAT ( A, ' time period:', I5.4, 2('/',I2.2), I3.2, 2(':', I2.2), ' to', I5.4, 2('/',I2.2), I3.2, 2(':', I2.2) )
      WRITE ( PRMS_output_unit, 10 ) 'Simulation', Starttime, Endtime
      CALL write_outfile(EQULS)

      ! check start and end times, if not valid stop with print
      ierr = 0
      IF ( Starttime(1)<startyr ) THEN
        ierr = 1
      ELSEIF ( Starttime(1)==startyr ) THEN
        IF ( Starttime(2)<startmo ) THEN
          ierr = 1
        ELSEIF ( Starttime(2)==startmo .AND. Starttime(3)<startdy ) THEN
          ierr = 1
        ENDIF
      ENDIF
      IF ( ierr==1 ) CALL error_stop('simulation time begins before Data File')

      ierr = 0
      IF ( Endtime(1)>endyr ) THEN
        ierr = 1
      ELSEIF ( Endtime(1)==endyr ) THEN
        IF ( Endtime(2)>endmo ) THEN
          ierr = 1
        ELSEIF ( Endtime(2)==endmo .AND. Endtime(3)>enddy ) THEN
          ierr = 1
        ENDIF
      ENDIF
      IF ( ierr==1 ) CALL error_stop('simulation end time exceeds Data File')
      
      ! read to start of data
      REWIND Datafile_unit
      DO
        READ ( Datafile_unit, FMT='(A)' ) data_line
        IF ( data_line(:4)=='####' ) EXIT
      ENDDO
      CALL find_current_time(Datafile_unit, Starttime(1), Starttime(2), Starttime(3), ios, 0)
      IF ( ios/=0 ) THEN
        PRINT *, 'End of file or error reading Data File to find the first simulation time step'
        PRINT *, 'Data File: ', data_filename
        ERROR STOP ERROR_read
      ENDIF

      END SUBROUTINE read_prms_data_file

!***********************************************************************
! Read PRMS Data File line
!***********************************************************************
      SUBROUTINE read_data_line()
      USE PRMS_DATA_FILE
      USE PRMS_MMFAPI
      USE PRMS_SET_TIME, ONLY: Nowtime
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRANSFER
      EXTERNAL read_error, check_data_variables
      INTEGER, EXTERNAL :: getvarnvals, getvar_id
      ! Local Variables
      INTEGER datatime(6), jj, ios, column_end, column, nvals, var_id
!***********************************************************************
      READ ( Datafile_unit, *, IOSTAT=ios ) datatime, (Data_line_values(jj),jj=1, Num_datafile_columns)
      IF ( ios/=0 ) THEN
        PRINT *, 'ERROR on:', Nowtime(1), Nowtime(2), Nowtime(3)
        IF ( ios/=0 ) THEN
          IF ( ios==-1 ) CALL read_error(13, 'hit end of file')
          CALL read_error(13, 'invalid line')
        ENDIF
      ENDIF
      IF ( datatime(1)/=Nowtime(1) .OR. datatime(2)/=Nowtime(2) .OR. datatime(3)/=Nowtime(3) ) THEN
        PRINT *, 'ERROR on: ', Nowtime(1), Nowtime(2), Nowtime(3), 'Data File date:', datatime(1), datatime(2), datatime(3)
        CALL read_error(13, 'data file date does not match time step date')
      ENDIF
      column_end = 0
      column = 1
      DO jj = 1, Num_datafile_types
        nvals = getvarnvals(Data_varname(jj))
        var_id = getvar_id(Data_varname(jj))
        column_end = column_end + nvals
        Variable_data(var_id)%values_real = TRANSFER(Data_line_values(column:column_end), Variable_data(var_id)%values_real)
        CALL check_data_variables(Data_varname(jj),nvals,Data_line_values(column:column_end),1,ios)
        IF ( ios/=0 ) THEN
          PRINT *, 'ERROR, Data File corrupted. Reading variable: ', Data_varname(jj)
          PRINT *, 'Date:', Nowtime(1), Nowtime(2), Nowtime(3)
          ERROR STOP ERROR_read
        ENDIF
        column = column + nvals
      ENDDO
      END SUBROUTINE read_data_line

!***********************************************************************
! check_data_variables - Check data variables and dimensions
!***********************************************************************
      SUBROUTINE check_data_variables(Varname, Numvalues, Values, Iflag, Iret)
      USE PRMS_CONSTANTS, ONLY: CFS2CMS_CONV
      USE PRMS_MODULE, ONLY: Ntemp, Nrain, Nsol, Nobs, Nevap
      USE PRMS_OBS, ONLY: Nlakeelev, Nwind, Nhumid, Nsnow, &
     &    Tmin, Tmax, Precip, Snowdepth, Runoff, Pan_evap, Wind_speed, Humidity, Solrad, &
     &    Gate_ht, Lake_elev, Rain_day, Runoff_units, Streamflow_cfs, Streamflow_cms
	  USE PRMS_CLIMATEVARS, ONLY: Ppt_zero_thresh
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      INTEGER, INTENT(IN) :: Numvalues, Iflag
      INTEGER, INTENT(OUT) :: Iret
      REAL, INTENT(IN) :: Values(Numvalues)
      ! Functions
      INTRINSIC DBLE
      ! Local Variables
      INTEGER ndim, i
!***********************************************************************
      Iret = 0
      IF ( Varname(:4)=='tmax' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Ntemp ) THEN
            Iret = -1
            ndim = Ntemp
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Tmax(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:4)=='tmin' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Ntemp ) THEN
            Iret = -1
            ndim = Ntemp
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Tmin(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:4)=='snow' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nsnow ) THEN
            Iret = -1
            ndim = Nsnow
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Snowdepth(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:6)=='precip' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nrain ) THEN
            Iret = -1
            ndim = Nrain
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Precip(i) = Values(i)
            IF ( Precip(i)<Ppt_zero_thresh ) Precip(i) = 0.0
          ENDDO
        ENDIF
      ELSEIF ( Varname(:6)=='runoff' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nobs ) THEN
            Iret = -1
            ndim = Nobs
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Runoff(i) = Values(i)
          ENDDO
          IF ( Runoff_units==1 ) THEN
            DO i = 1, Nobs
              Streamflow_cms(i) = DBLE( Runoff(i) )
              Streamflow_cfs(i) = Streamflow_cms(i)/CFS2CMS_CONV
            ENDDO
          ELSE
            DO i = 1, Nobs
              Streamflow_cfs(i) = DBLE( Runoff(i) )
              Streamflow_cms(i) = Streamflow_cfs(i)*CFS2CMS_CONV
            ENDDO
          ENDIF
        ENDIF
      ELSEIF ( Varname(:6)=='solrad' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nsol ) THEN
            Iret = -1
            ndim = Nsol
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Solrad(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:7)=='gate_ht' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nlakeelev ) THEN
            Iret = -1
            ndim = Nlakeelev
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Gate_ht(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:8)=='pan_evap' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nevap ) THEN
            Iret = -1
            ndim = Nevap
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Pan_evap(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:8)=='rain_day' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=1 ) THEN
            Iret = -1
            ndim = 1
          ENDIF
        ELSE
          Rain_day = Values(1)
        ENDIF
      ELSEIF ( Varname(:8)=='humidity' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nhumid ) THEN
            Iret = -1
            ndim = Nhumid
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Humidity(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:9)=='lake_elev' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nlakeelev ) THEN
            Iret = -1
            ndim = Nlakeelev
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Lake_elev(i) = Values(i)
          ENDDO
        ENDIF
      ELSEIF ( Varname(:10)=='wind_speed' ) THEN
        IF ( Iflag==0 ) THEN
          IF ( Numvalues/=Nwind ) THEN
            Iret = -1
            ndim = Nwind
          ENDIF
        ELSE
          DO i = 1, Numvalues
            Wind_speed(i) = Values(i)
          ENDDO
        ENDIF
      ELSE
        PRINT *, 'ERROR, data variable: ', Varname, ' is not valid'
        Iret = -2
      ENDIF
      IF ( Iret==-1 ) THEN
        PRINT *, 'ERROR, number of values for data variable: ', Varname
        PRINT *, 'does not equal dimension value; data values:', Numvalues, ' dimension:', ndim
      ENDIF
      END SUBROUTINE check_data_variables

!***********************************************************************
! read_data_file_line - Read next data line, check increment
!***********************************************************************
      SUBROUTINE read_data_file_line(Iret)
      USE PRMS_DATA_FILE
      USE PRMS_MODULE, ONLY: Start_year, Start_month, Start_day
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
      ! Functions
      INTEGER, EXTERNAL :: compute_julday
      EXTERNAL check_data_variables, read_error
      ! Arguments
      INTEGER, INTENT(OUT) :: Iret
      ! Local Variables
      INTEGER last_julday, now_julday, hr, mn, sec, start, i
      INTEGER, SAVE :: init
      DATA init/1/
!***********************************************************************
      Iret = 0
      IF ( init==1 ) THEN
        Nowyear = Start_year
        Nowmonth = Start_month
        Nowday = Start_day
      ELSE
        last_julday = compute_julday(Nowyear, Nowmonth, Nowday)
      ENDIF
      READ ( Datafile_unit, *, IOSTAT=Iret ) Nowyear, Nowmonth, Nowday, hr, mn, sec, (Data_line_values(i),i=1,Num_datafile_columns)
      IF ( Iret==0 ) THEN
        IF ( init==0 ) THEN
          now_julday = compute_julday(Nowyear, Nowmonth, Nowday)
          IF ( now_julday-last_julday/=1 ) THEN
            PRINT *, 'ERROR, Data File timestep not equal to 1 day on:', Nowyear, Nowmonth, Nowday
            PRINT *, '       timestep =', now_julday - last_julday
            STOP ERROR_read
          ENDIF
        ELSE
          init = 0
        ENDIF
        start = 1
        DO i = 1, Num_datafile_types
          CALL check_data_variables(Data_varname(i), Data_varnum(i), Data_line_values(start), 1, Iret)
          start = start + Data_varnum(i)
        ENDDO
      ELSE
        CALL read_error(13, 'measured variables')
      ENDIF
      END SUBROUTINE read_data_file_line

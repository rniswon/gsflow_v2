      MODULE PRMS_READ_PARAM_FILE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Read values from Parameter File'
         INTEGER, SAVE :: Param_unit, Read_parameters
      END MODULE PRMS_READ_PARAM_FILE

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_dimens
      USE PRMS_CONSTANTS, ONLY: MAXLINE_LENGTH
      USE PRMS_MODULE, ONLY: Print_debug, EQULS, Param_file
      USE PRMS_READ_PARAM_FILE
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      EXTERNAL read_error, PRMS_open_input_file, write_outfile, setdimension
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      CHARACTER(LEN=16) :: string, dimname
      CHARACTER(LEN=MAXLINE_LENGTH) :: line
      CHARACTER(LEN=24) :: dimstring
      INTEGER nchars, ios, dimen_value
!***********************************************************************
      CALL PRMS_open_input_file(Param_unit, Param_file, 'param_file', 0, ios)
      IF ( ios/=0 ) STOP
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile(EQULS)
        CALL write_outfile('Using PRMS Parameter File: '//Param_file)
      ENDIF

! Echo Parmeter File Header and comment lines
      READ ( Param_unit, FMT='(A)', IOSTAT=ios ) line
      IF ( ios/=0 ) CALL read_error(13, 'description')
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile('Description: '//TRIM(line))
        CALL write_outfile(EQULS)
        CALL write_outfile('Comment lines:')
      ENDIF

      ! Find start of dimensions section
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) line
        IF ( ios==-1 ) CALL read_error(13, 'end of file found before dimensions')
        IF ( ios/=0 ) CALL read_error(13, 'comment')
        IF ( line(:16)=='** Dimensions **' ) EXIT
        IF ( Print_debug>-1 ) CALL write_outfile(TRIM(line))
      ENDDO
      IF ( line(:16)/='** Dimensions **' ) CALL read_error(11, 'missing dimension section: '//TRIM(line))
      IF ( Print_debug>-1 ) THEN
        CALL write_outfile(EQULS)
        CALL write_outfile('Using dimensions    number')
      ENDIF

! Read all dimensions

      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) CALL read_error(13, 'end of file found before parameter section')
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension #### delimiter')
        IF ( string(:4)=='    ' ) CYCLE
        IF ( string(:2)=='//' ) CYCLE
        IF ( string=='** Parameters **' ) EXIT ! stop reading if end of dimensions section
        !IF ( string(:4)/='####' ) CALL read_error(11, 'missing dimension #### delimiter '//string)
        IF ( string(:4)/='####' ) THEN
          PRINT *, 'Warning, ignoring dimension line: ', string
          CYCLE
        ENDIF
        READ ( Param_unit, *, IOSTAT=ios ) dimname
        nchars = numchars(dimname)
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension name: '//dimname(:nchars))
        READ ( Param_unit, *, IOSTAT=ios ) dimen_value
        IF ( ios/=0 ) CALL read_error(11, 'missing dimension value')

        CALL setdimension(dimname, dimen_value)

        IF ( dimen_value==0 ) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Warning, dimension: ', dimname(:nchars), ' is not needed as value specified as 0'
        ENDIF
        IF ( Print_debug>-1 ) THEN
          WRITE ( dimstring, '(A,I8)' ) dimname, dimen_value
          CALL write_outfile(dimstring)
        ENDIF
      ENDDO
      IF ( Print_debug>-1 ) CALL write_outfile(EQULS)
      END SUBROUTINE read_parameter_file_dimens

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
      SUBROUTINE read_parameter_file_params
      USE PRMS_CONSTANTS, ONLY: MAXCONTROL_LENGTH
      USE PRMS_READ_PARAM_FILE
      USE PRMS_CONTROL_FILE, ONLY: Param_file_control_parameter_id
      USE PRMS_MMFAPI, ONLY: Control_parameter_data, Num_parameters, Parameter_data
      IMPLICIT NONE
      ! Functions
      EXTERNAL read_error, PRMS_open_input_file, setparam
      INTEGER, EXTERNAL :: numchars, getdim
      INTRINSIC :: TRIM
      ! Local Variables
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: string
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramstring
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: dim_string(2)
      INTEGER nchars, ios, num_dims, num_param_values, i, j, k, param_type, num, inum, numfiles, ii, duplicate, found
      INTEGER, ALLOCATABLE :: idmy(:)
      REAL, ALLOCATABLE :: dmy(:)
      !***********************************************************************
! Find parameter section
      REWIND ( Param_unit )
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) CALL read_error(11, 'end of file found before parameter section') ! found end of Parameter File
        IF ( string(:16)=='** Parameters **' ) EXIT ! stop reading if end of dimensions section
      ENDDO

! Read all parameters and verify
      numfiles = Control_parameter_data(Param_file_control_parameter_id)%numvals
      Read_parameters = 0
      DO k = 1, numfiles
        IF ( k>1 ) THEN
          CLOSE ( Param_unit )
          CALL PRMS_open_input_file(Param_unit, Control_parameter_data(Param_file_control_parameter_id)%values_character(k), &
     &                              'param_file', 0, ios)
        ENDIF
      DO
        READ ( Param_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of a Parameter File
        IF ( ios/=0 ) CALL read_error(11, 'missing parameter #### delimiter')
        IF ( string(:4)=='    ' ) CYCLE ! skip blank lines
        IF ( string(:2)=='//' ) CYCLE ! skip comment lines
        !IF ( string(:4)/='####' ) CALL read_error(11, 'missing parameter #### delimiter')
        IF ( string(:4)/='####' ) CYCLE
        READ ( Param_unit, '(A)', IOSTAT=ios ) paramstring ! parameter name
        IF ( ios/=0 ) CALL read_error(11, 'missing parameter name')
        nchars = numchars(paramstring)
        READ ( Param_unit, *, IOSTAT=ios ) num_dims
        IF ( ios/=0 ) CALL read_error(11, 'invalid number of dimensions: '//paramstring(:nchars))
        IF ( num_dims>2 ) CALL read_error(11, 'number of dimensions > 3: '//paramstring(:nchars))
        num = 1
        DO i = 1, num_dims
          READ ( Param_unit, '(A)', IOSTAT=ios ) dim_string(i)
          IF ( ios/=0 ) CALL read_error(11, 'invalid dimension for parameter: '//paramstring(:nchars))
          inum = getdim(dim_string(i))
          IF ( inum==-1 ) CALL read_error(11, TRIM(dim_string(i)))
          num = num*inum
        ENDDO
        READ ( Param_unit, *, IOSTAT=ios ) num_param_values
        IF ( ios/=0 ) CALL read_error(11, 'invalid number of parameter values: '//paramstring(:nchars))
!        IF ( num/=num_param_values ) CALL read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))
        READ ( Param_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(11, 'invalid parameter type '//paramstring(:nchars))
        IF ( param_type<1 .OR. param_type>3 ) CALL read_error(11, 'invalid parameter type: '//paramstring(:nchars))
        ! check to see if parameter already read
        duplicate = 0
        found = 0
        DO ii = 1, Num_parameters
          IF ( paramstring(:nchars)==TRIM(Parameter_data(ii)%param_name) ) THEN
            found = ii
            IF ( Parameter_data(ii)%read_flag==1 ) THEN
              PRINT '(/,3A)', 'WARNING, parameter: ', TRIM(paramstring), ' specified more than once'
              inum = MIN ( num_param_values, 5 )
              PRINT *, '        Ignoring previous value(s)'
              duplicate = ii
              EXIT
            ENDIF
          ENDIF
        ENDDO
        IF ( found==0 ) then
          PRINT '(/,3A)', 'Values for parameter: ', TRIM(paramstring), ' are ignored as the parameter is not used'
          CYCLE
        ENDIF
        IF ( param_type==1 ) THEN
          ALLOCATE ( idmy(num_param_values), dmy(1) )
          READ ( Param_unit, *, IOSTAT=ios ) (idmy(j),j=1,num_param_values)
          IF ( ios/=0 ) CALL read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
          IF ( duplicate>0 ) &
     &         PRINT '(A,5I8)', '         Using (up to 5 values printed):', (idmy(j), j=1, inum)
        ELSE
          ALLOCATE ( dmy(num_param_values), idmy(1) )
          READ ( Param_unit, *, IOSTAT=ios ) (dmy(j),j=1,num_param_values)
          IF ( ios/=0 ) CALL read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
          IF ( duplicate>0 ) &
     &         PRINT '(A,5F8.2)', '         Using (up to 5 values printed): ', (dmy(j), j=1, inum)
        ENDIF
        IF ( duplicate>0 ) PRINT *, ' '
        CALL setparam(paramstring(:nchars), num_param_values, param_type, num_dims, dim_string, dmy, idmy)
        Read_parameters = Read_parameters + 1
        Parameter_data(found)%read_flag = 1
        DEALLOCATE ( dmy, idmy )
      ENDDO
      ENDDO
      
      CLOSE ( param_unit )
      END SUBROUTINE read_parameter_file_params

!***********************************************************************
! Check for parameters declared but not in Parameter File
!***********************************************************************
      SUBROUTINE check_parameters
      USE PRMS_MMFAPI, ONLY: Num_parameters, Parameter_data
      IMPLICIT NONE
      ! Functions
      EXTERNAL read_error, PRMS_open_input_file, setparam
      INTEGER, EXTERNAL :: numchars, getdim
      INTRINSIC :: TRIM
      ! Local Variables
      INTEGER :: i
      !***********************************************************************
      DO i = 1, Num_parameters
        IF ( Parameter_data(i)%decl_flag==1 .AND. Parameter_data(i)%read_flag==0 ) THEN
          PRINT *, 'Parameter: ', TRIM(Parameter_data(i)%param_name), ' is not specified'
          IF ( Parameter_data(i)%data_flag==1 ) THEN
            PRINT *, '           Set to default value:', Parameter_data(i)%default_int
          ELSEIF ( Parameter_data(i)%data_flag==2 ) THEN
            PRINT *, '           Set to default value:', Parameter_data(i)%default_real
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE check_parameters

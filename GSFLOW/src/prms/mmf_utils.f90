!***********************************************************************
! DONE:
!  getdim, declfix, declmodule, decldim, declparam, getparam
!  control_string, control_integer, control_string_array, getvartype
!***********************************************************************
      MODULE PRMS_MMFAPI
        USE PRMS_MODULE, ONLY: MAXCONTROL_LENGTH, MAXFILE_LENGTH
        IMPLICIT NONE
        ! DANGER, DANGER, hard coded maximum number of paraemters and dimensions, DANGER, DANGER
        INTEGER, PARAMETER :: MAXDIMENSIONS = 50, MAXPARAMETERS = 240, MAXVARIABLES = 500
        INTEGER, SAVE :: Num_parameters, Num_dimensions, Num_variables  !, Total_parameters
 
        TYPE PRMS_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: param_name
             CHARACTER(LEN=MAXFILE_LENGTH) :: short_description, long_description
             INTEGER :: numvals, data_flag, decl_flag, read_flag, nchars, id_num
             INTEGER :: default_int, maximum_int, minimum_int, num_dimens
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: max_value, min_value, def_value, data_type
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: dimen_names, module_name, units
             REAL, POINTER :: values(:)
             INTEGER, POINTER :: int_values(:)
             REAL :: maximum, minimum, default_real
        END TYPE PRMS_parameter
        TYPE ( PRMS_parameter ), SAVE, ALLOCATABLE :: Parameter_data(:)

        TYPE PRMS_dimension
             CHARACTER(LEN=16) :: name
             INTEGER :: value, default, maximum, length
             CHARACTER(LEN=MAXFILE_LENGTH) :: description
        END TYPE PRMS_dimension
        TYPE ( PRMS_dimension ), SAVE, ALLOCATABLE :: Dimension_data(:)

        TYPE PRMS_variable
             CHARACTER(LEN=32) :: variable_name
             CHARACTER(LEN=256) :: description
             INTEGER :: numvals, data_flag, decl_flag, get_flag, var_name_nchars, id_num
             CHARACTER(LEN=12) :: data_type, dimen_names, module_name, units
             INTEGER, POINTER :: values_int(:)
             REAL, POINTER :: values_real(:)
             DOUBLE PRECISION, POINTER :: values_dble(:)
        END TYPE PRMS_variable
        TYPE ( PRMS_variable ), SAVE, ALLOCATABLE :: Variable_data(:)

      END MODULE PRMS_MMFAPI

!***********************************************************************
! Allocate and initialize parameter data base
! DANGER, DANGER, hard coded maximum number of paraemters, DANGER, DANGER
!***********************************************************************
      SUBROUTINE setup_params()
      USE PRMS_MMFAPI
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      ! allocate and store parameter data
      ALLOCATE ( Parameter_data(MAXPARAMETERS) ) ! allow for extra parameters being expected
      DO i = 1, MAXPARAMETERS
        Parameter_data(i)%param_name = ' '
        Parameter_data(i)%short_description = ' '
        Parameter_data(i)%long_description = ' '
        Parameter_data(i)%numvals = 0
        Parameter_data(i)%data_flag = 0
        Parameter_data(i)%decl_flag = 0
        Parameter_data(i)%read_flag = 0
        Parameter_data(i)%nchars = 0
        Parameter_data(i)%id_num = 0
        Parameter_data(i)%max_value = ' '
        Parameter_data(i)%min_value = ' '
        Parameter_data(i)%def_value = ' '
        Parameter_data(i)%data_type = ' '
        Parameter_data(i)%module_name = ' '
        Parameter_data(i)%units = ' '
        Parameter_data(i)%dimen_names = ' '
        Parameter_data(i)%maximum = 0.0
        Parameter_data(i)%minimum = 0.0
        Parameter_data(i)%default_real = 0.0
        Parameter_data(i)%maximum_int = 0
        Parameter_data(i)%minimum_int = 0
        Parameter_data(i)%default_int = 0
        Parameter_data(i)%num_dimens = 0
      ENDDO
      Num_parameters = 0

      END SUBROUTINE setup_params

!***********************************************************************
! Allocate and initialize dimension data base
! WARNING, hard coded, DANGER, DANGER
!***********************************************************************
      SUBROUTINE setup_dimens()
      USE PRMS_MMFAPI
      IMPLICIT NONE
! Local Variables
      INTEGER :: i
!***********************************************************************
      ! allocate and initialize dimension data
      ALLOCATE ( Dimension_data(MAXDIMENSIONS) ) ! allow for extra parameters being expected
      DO i = 1, MAXDIMENSIONS
        Dimension_data(i)%name = ' '
        Dimension_data(i)%value = 0
        Dimension_data(i)%default = 0
        Dimension_data(i)%maximum = 0
        Dimension_data(i)%description = ' '
      ENDDO
      Num_dimensions = 0

      END SUBROUTINE setup_dimens

!***********************************************************************
! declparam - set up memory for parameters
!***********************************************************************
      INTEGER FUNCTION declparam(Modname, Paramname, Dimenname, Datatype, &
     &                           Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Paramname, Dimenname, Datatype
      CHARACTER(LEN=*), INTENT(IN) :: Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units
      ! INTRINSIC
      INTRINSIC INDEX, TRIM
      ! Functions
      INTEGER, EXTERNAL :: numchars, isdeclared, getdim
      EXTERNAL :: check_parameters_declared, read_error, error_stop
      ! Local Variables
      INTEGER :: comma, ndimen, nval, nvals, nvals2, declared, numvalues, type_flag, iset, i, itemp
      REAL :: temp
      CHARACTER(LEN=MAXCONTROL_LENGTH) dimen1, dimen2
!***********************************************************************
      !!!!!!!!!!!! check to see if already in data structure
      ! doesn't check to see if declared the same, uses first values
      CALL check_parameters_declared(Paramname, Modname, declared)
      IF ( declared==1 ) RETURN

      ! current value of Num_parameters is the number that have been declared
      Num_parameters = Num_parameters + 1
      IF ( Num_parameters>MAXPARAMETERS ) STOP 'ERROR, hard-coded number of parameters exceeded, report to developers'

      Parameter_data(Num_parameters)%module_name = Modname
      Parameter_data(Num_parameters)%param_name = Paramname
      Parameter_data(Num_parameters)%dimen_names = Dimenname
      Parameter_data(Num_parameters)%data_type = Datatype
      Parameter_data(Num_parameters)%def_value = Defvalue
      Parameter_data(Num_parameters)%min_value = Minvalue
      Parameter_data(Num_parameters)%max_value = Maxvalue
      Parameter_data(Num_parameters)%short_description = Descshort
      Parameter_data(Num_parameters)%long_description = Desclong
      Parameter_data(Num_parameters)%units = Units

      Parameter_data(Num_parameters)%decl_flag = 1
      Parameter_data(Num_parameters)%nchars = numchars(Paramname)
      Parameter_data(Num_parameters)%id_num = Num_dimensions

      CALL set_data_type(Datatype, type_flag)
      IF ( type_flag<1 .OR. type_flag>2 ) CALL read_error(16, Paramname//': data type not implemented: '//Datatype)
      Parameter_data(Num_parameters)%data_flag = type_flag

      ! get dimension number of values
      dimen2 = ' '
      ndimen = numchars(Dimenname)
      comma = INDEX(Dimenname,',')
      IF ( comma==0 ) THEN
        dimen1 = Dimenname(:ndimen)
        Parameter_data(Num_parameters)%num_dimens = 1
      ELSE
        dimen1 = Dimenname(:(comma-1))
        dimen2 = Dimenname((comma+1):ndimen)
        Parameter_data(Num_parameters)%num_dimens = 2
      ENDIF
      numvalues = getdim(TRIM(dimen1))
      IF ( numvalues==-1 ) CALL read_error(11, TRIM(dimen1))
      IF ( comma>0 ) THEN
        nvals2 = getdim(TRIM(dimen2))
        IF ( nvals2==-1 ) CALL read_error(11, TRIM(dimen2))
        numvalues = numvalues * nvals2
      ENDIF
      Parameter_data(Num_parameters)%numvals = numvalues

      ! could add string and double
      IF ( type_flag==1 ) THEN
        READ ( defvalue, * ) itemp
        Parameter_data(Num_parameters)%default_int = itemp
        ALLOCATE ( Parameter_data(Num_parameters)%int_values(numvalues) )
        IF ( numvalues>50000 ) THEN
          DO i = 1, numvalues
            Parameter_data(Num_parameters)%int_values(i) = itemp
          ENDDO
        ELSE
          Parameter_data(Num_parameters)%int_values = itemp
        ENDIF
      ELSEIF ( type_flag==2 ) THEN
        READ ( defvalue, * ) temp
        Parameter_data(Num_parameters)%default_real = temp
        ALLOCATE ( Parameter_data(Num_parameters)%values(numvalues) )
        IF ( numvalues>50000 ) THEN
          DO i = 1, numvalues
            Parameter_data(Num_parameters)%values(i) = temp
          ENDDO
        ELSE
          Parameter_data(Num_parameters)%values = temp
        ENDIF
      ENDIF

      iset = 0
      nval = LEN(Minvalue)
      IF ( nval>6 ) THEN
        IF ( Minvalue(:7)=='bounded' ) iset = 1
      ENDIF

      IF ( iset==1 ) THEN
        IF ( type_flag==1 ) THEN  ! bounded parameters should all be integer
        nvals = getdim(TRIM(Maxvalue))
        IF ( nvals==-1 ) CALL read_error(11, Maxvalue)
          Parameter_data(Num_parameters)%maximum_int = nvals
          Parameter_data(Num_parameters)%minimum_int = Parameter_data(Num_parameters)%default_int
        ELSE
          CALL error_stop('bounded parameter not real type')
        ENDIF
      ELSE
        IF ( type_flag==1 ) THEN
          READ ( Maxvalue, * ) Parameter_data(Num_parameters)%maximum_int
          READ ( Minvalue, * ) Parameter_data(Num_parameters)%minimum_int
        ELSE
          READ ( Maxvalue, * ) Parameter_data(Num_parameters)%maximum
          READ ( Minvalue, * ) Parameter_data(Num_parameters)%minimum
        ENDIF
      ENDIF

      declparam = 0
      END FUNCTION declparam

!***********************************************************************
! Set data type flag
!***********************************************************************
      SUBROUTINE set_data_type(Data_type, Type_flag)
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Data_type
      INTEGER, INTENT(OUT) :: Type_flag
      ! Functions
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      INTEGER string_length
!***********************************************************************
      string_length = numchars( Data_type )
      IF ( string_length>3 .AND. Data_type(:4)=='real' ) THEN
        Type_flag = 2
      ELSEIF ( string_length>5 .AND. Data_type(:6)=='double' ) THEN
        Type_flag = 3
      ELSEIF ( string_length>5 .AND. Data_type(:6)=='string' ) THEN
        Type_flag = 4
      ELSEIF ( string_length>6 .AND. Data_type(:7)=='integer' ) THEN
        Type_flag = 1
      ELSE
        PRINT *, 'ERROR, invalid data type: ', Data_type
        PRINT *, '       valid values are real, double, string, integer'
        ERROR STOP -1
      ENDIF
      END SUBROUTINE set_data_type

!***********************************************************************
! check_parameters_declared - check for parameters being declared more than once
!***********************************************************************
      SUBROUTINE check_parameters_declared(Parmname, Modname, Iret)
      USE PRMS_MMFAPI
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Parmname, Modname
      INTEGER, INTENT(OUT) :: Iret
      ! Functions
      INTRINSIC TRIM
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      INTEGER i, nchars
!***********************************************************************
      Iret = 0
      nchars = numchars(Parmname)
      DO i = 1, Num_parameters
        IF ( nchars==Parameter_data(i)%nchars ) THEN
          IF ( Parmname(:nchars) == Parameter_data(i)%param_name(:nchars) ) THEN
            IF ( Parameter_data(i)%decl_flag==1 ) THEN
              IF ( Print_debug>-1 ) THEN
                PRINT *, 'Parameter: ', TRIM(Parmname), ' declared more than once'
                PRINT *, 'First declared by module: ', TRIM(Parameter_data(Num_parameters)%module_name)
                PRINT *, 'Also declared by module: ', TRIM(Modname)
                PRINT *, 'Model uses values based on first declare'
              ENDIF
              Iret = 1
            ENDIF
            EXIT
          ENDIF
        ENDIF
      ENDDO
      END SUBROUTINE check_parameters_declared

!***********************************************************************
! declvar - set up memory for variables
!***********************************************************************
      SUBROUTINE declvar(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units)
      USE PRMS_MMFAPI, ONLY: Variable_data, Num_variables, MAXVARIABLES
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Data_type, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      ! Functions
      INTEGER, EXTERNAL :: numchars
      EXTERNAL set_data_type, error_stop
      ! Local Variables
      INTEGER type_flag
      INTEGER, SAVE :: init
      DATA init/0/
!***********************************************************************
      IF ( init==0 ) THEN
        init = 1
        Num_variables = 0
        ALLOCATE ( Variable_data(MAXVARIABLES) ) ! don't know how many, need to read var_name file
      ENDIF
      ! need to declare parameters first, but don't know how many, know how many in Parameter File
      Num_variables = Num_variables + 1
      IF ( Num_variables>MAXVARIABLES ) THEN
        PRINT '(A,I0)', 'PRMS ERROR, maximum number of declared variables exceeded: ', MAXVARIABLES
        CALL error_stop('maximum number of declared variables exceeded')
      ENDIF
      Variable_data(Num_variables)%get_flag = 0
      Variable_data(Num_variables)%decl_flag = 1
      Variable_data(Num_variables)%variable_name = Varname
      Variable_data(Num_variables)%var_name_nchars = numchars(Varname)
      Variable_data(Num_variables)%description = Desc
      Variable_data(Num_variables)%units = Units
      Variable_data(Num_variables)%dimen_names = Dimenname
      Variable_data(Num_variables)%module_name = Modname
      Variable_data(Num_variables)%numvals = Numvalues
      Variable_data(Num_variables)%data_type = Data_type
      Variable_data(Num_variables)%id_num = Num_variables
      CALL set_data_type(Data_type, type_flag)
      IF ( type_flag<1 .OR. type_flag>3 ) THEN
        PRINT *, 'ERROR, data type not implemented: ', Data_type, ' Variable: ', &
     &           Varname(:Variable_data(Num_variables)%var_name_nchars)
        ERROR STOP -1
      ENDIF
      Variable_data(Num_variables)%data_flag = type_flag

      END SUBROUTINE declvar

!***********************************************************************
! declvar_dble - set up memory for double precision variables
!***********************************************************************
      SUBROUTINE declvar_dble(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
      USE PRMS_MMFAPI, ONLY: Num_variables, Variable_data
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Data_type, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      DOUBLE PRECISION, TARGET :: Values(*)
      ! Functions
      EXTERNAL declvar
!***********************************************************************
      CALL declvar( Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units )
      ALLOCATE ( Variable_data(Num_variables)%values_dble(Numvalues) )
      Variable_data(Num_variables)%values_dble => Values(:Numvalues)
      END SUBROUTINE declvar_dble

!***********************************************************************
! declvar_real - set up memory for real variables
!***********************************************************************
      SUBROUTINE declvar_real(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
      USE PRMS_MMFAPI, ONLY: Num_variables, Variable_data
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Data_type, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      REAL, TARGET :: Values(*)
      ! Functions
      EXTERNAL declvar
!***********************************************************************
      CALL declvar( Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units )
      ALLOCATE ( Variable_data(Num_variables)%values_real(Numvalues) )
      Variable_data(Num_variables)%values_real => Values(:Numvalues)
      END SUBROUTINE declvar_real

!***********************************************************************
! declvar_int - set up memory for integer variables
!***********************************************************************
      SUBROUTINE declvar_int(Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units, Values)
      USE PRMS_MMFAPI, ONLY: Num_variables, Variable_data
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Data_type, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, TARGET :: Values(*)
      ! Functions
      EXTERNAL declvar
!***********************************************************************
      CALL declvar( Modname, Varname, Dimenname, Numvalues, Data_type, Desc, Units )
      ALLOCATE ( Variable_data(Num_variables)%values_int(Numvalues) )
      Variable_data(Num_variables)%values_int => Values(:Numvalues)
      END SUBROUTINE declvar_int

!***********************************************************************
! getvar_dble - get double precision variable values
!***********************************************************************
      SUBROUTINE getvar_dble(Modname, Varname, Numvalues, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname
      INTEGER, INTENT(IN) :: Numvalues
      DOUBLE PRECISION, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTEGER, EXTERNAL :: find_variable
      ! Local Variables
      INTEGER :: var_id
!***********************************************************************
      var_id = find_variable(Modname, Varname, Numvalues, 'double')
      Values = Variable_data(var_id)%values_dble
      END SUBROUTINE getvar_dble

!***********************************************************************
! getvar_dble - get single precision variable values
!***********************************************************************
      SUBROUTINE getvar_real(Modname, Varname, Numvalues, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname
      INTEGER, INTENT(IN) :: Numvalues
      REAL, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTEGER, EXTERNAL :: find_variable
      ! Local Variables
      INTEGER :: var_id
!***********************************************************************
      var_id = find_variable(Modname, Varname, Numvalues, 'real')
      Values = Variable_data(var_id)%values_real
      END SUBROUTINE getvar_real

!***********************************************************************
! getvar_dble - get integer variable values
!***********************************************************************
      SUBROUTINE getvar_int(Modname, Varname, Numvalues, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTEGER, EXTERNAL :: find_variable
      ! Local Variables
      INTEGER :: var_id
!***********************************************************************
      var_id = find_variable(Modname, Varname, Numvalues, 'real')
      Values = Variable_data(var_id)%values_int
      END SUBROUTINE getvar_int

!***********************************************************************
! getvar - get variable values
!***********************************************************************
      INTEGER FUNCTION getvar(Modname, Varname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      REAL, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      !INTRINSIC TRANSFER
      INTEGER, EXTERNAL :: find_variable
      ! Local Variables
      INTEGER :: var_id, var_type
      INTEGER, ALLOCATABLE :: itemp(:)
      REAL, ALLOCATABLE :: temp(:)
      DOUBLE PRECISION, ALLOCATABLE :: dtemp(:)
!***********************************************************************
      var_id = find_variable(Modname, Varname, Numvalues, Data_type)
      var_type = Variable_data(var_id)%data_flag

      IF ( var_type==1 ) THEN
        ALLOCATE ( itemp(Numvalues) )
        itemp = Variable_data(var_id)%values_int
        Values = transfer(itemp, Values)
        DEALLOCATE ( itemp )
      ELSEIF ( var_type==2 ) THEN
        ALLOCATE ( temp(Numvalues) )
        temp = Variable_data(var_id)%values_real
        Values = transfer(temp, Values)
        DEALLOCATE ( temp )
      ELSEIF ( var_type==3 ) THEN
        ALLOCATE ( dtemp(Numvalues) )
        dtemp = Variable_data(var_id)%values_dble
        Values = transfer(dtemp, Values)
        DEALLOCATE ( dtemp )
      ENDIF
      

      !Values = TRANSFER(Variable_data(var_id)%values_dble,Values)
      !do i = 1, Numvalues
      !    !Values(i) = SNGL(Variable_data(var_id)%values_dble(i))
      !    Values(i) = temp(i)
      !    IF ( values(i)<0.0 ) values(i) = 0.0
      !    !print *, Variable_data(var_id)%values_dble(i)
      !ENDDO
      !values = temp

      getvar = 0
      END FUNCTION getvar

!***********************************************************************
! find_variable - find variable in data structure
!***********************************************************************
      INTEGER FUNCTION find_variable(Modname, Varname, Numvalues, Data_type)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: found, i, ierr
!***********************************************************************
      ierr = 0
      found = 0
      find_variable = 1
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          found = 1
          IF ( Variable_data(i)%numvals/=Numvalues ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, &
     &               ' number of values in getvar does not match declared number of values'
          ENDIF
          IF ( TRIM(Variable_data(i)%data_type)/=Data_type ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, ' data type does in getvar not match declared data type'
          ENDIF
          find_variable = i
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR in: ', Modname, ', Variable: ', Varname, ' not declared'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) ERROR STOP -1

      END FUNCTION find_variable

!***********************************************************************
! getvar_id - get variable index
!***********************************************************************
      INTEGER FUNCTION getvar_id(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvar_id = 1
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvar_id = Variable_data(i)%id_num
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR variable: ', Varname, ' not available'
      ERROR STOP -1
      END FUNCTION getvar_id

!***********************************************************************
! getvartype - get variable type
!***********************************************************************
      INTEGER FUNCTION getvartype(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvartype = 1
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvartype = Variable_data(i)%data_flag
          getvartype = getvartype
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR variable: ', Varname, ' not available'
      ERROR STOP -1
      END FUNCTION getvartype

!***********************************************************************
! getvarnvals - get variable number of values
!***********************************************************************
      INTEGER FUNCTION getvarnvals(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: i
!***********************************************************************
      getvarnvals = 1
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          getvarnvals = Variable_data(i)%numvals
          RETURN
        ENDIF
      ENDDO
      PRINT *, 'ERROR in: getvarnvals, Variable: ', Varname, ' not declared'
      ERROR STOP -1
      END FUNCTION getvarnvals

!***********************************************************************
! getparam - get parameter values
!***********************************************************************
      INTEGER FUNCTION getparam(Modname, Paramname, Numvalues, Data_type, Values)
      USE PRMS_MMFAPI
      USE PRMS_MODULE, ONLY: Parameter_check_flag
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Paramname, Data_type
      INTEGER, INTENT(IN) :: Numvalues
      ! values could be any data type
      REAL, INTENT(OUT) :: Values(Numvalues)
      ! Functions
      INTRINSIC TRIM
      EXTERNAL error_stop
      ! Local Variables
      INTEGER :: type_flag, found, param_id, i, ierr
!***********************************************************************
      Values = 0.0
      ierr = 0
      found = 0
      DO i = 1, Num_parameters
        IF ( Paramname==TRIM(Parameter_data(i)%param_name) ) THEN
          found = 1
          IF ( Parameter_data(i)%numvals/=Numvalues ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &               ' number of values in getparam does not match declared number of values'
          ENDIF
          IF ( TRIM(Parameter_data(i)%data_type)/=Data_type ) THEN
            ierr = 1
            PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' data type does in getparam not match declared data type'
          ENDIF
          param_id = i
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) ERROR STOP -1

      type_flag = Parameter_data(param_id)%data_flag

      IF ( type_flag==3 ) THEN
        CALL getvalues_dbl(param_id, Numvalues, Values)
      ELSEIF ( type_flag==2 ) THEN
        IF ( Parameter_check_flag==1 ) THEN
          DO i = 1, Numvalues
            IF ( Parameter_data(param_id)%values(i) > Parameter_data(param_id)%maximum ) THEN
              PRINT *, 'WARNING, value > maximum value for parameter: ', Paramname, '; index:', param_id
              PRINT *, '         value:', Parameter_data(param_id)%values(i), '; maximum value:', Parameter_data(param_id)%maximum
            ENDIF
            IF ( Parameter_data(param_id)%values(i) < Parameter_data(param_id)%minimum ) THEN
              PRINT *, 'WARNING, value < minimum value for parameter: ', Paramname, '; index:', param_id
              PRINT *, '         value:', Parameter_data(param_id)%values(i), '; minimum value:', Parameter_data(param_id)%minimum
            ENDIF
          ENDDO
        ENDIF
        Values = Parameter_data(param_id)%values
      ELSEIF ( type_flag==1 ) THEN
        CALL getvalues_int(param_id, Numvalues, Values)
      ELSE
        PRINT *, 'Paramname: ', Paramname, ' type: ', type_flag
        CALL error_stop('Parameter type not implemented')
      ENDIF

      getparam = 0
      END FUNCTION getparam

!***********************************************************************
! getvar_values_int - get values from variable data structure
!***********************************************************************
!      SUBROUTINE getvar_values_int(param_id, Numvalues, Values)
!      USE PRMS_MMFAPI
!      IMPLICIT NONE
!      ! Arguments
!      INTEGER, INTENT(IN) :: param_id, Numvalues
!      INTEGER, INTENT(OUT) :: Values(Numvalues)
!!***********************************************************************
!      values = Variable_data(param_id)%values_int
!      END SUBROUTINE getvar_values_int

!***********************************************************************
! getvar_values_real - get values from variable data structure
!***********************************************************************
!      SUBROUTINE getvar_values_real(param_id, Numvalues, Values)
!      USE PRMS_MMFAPI
!      IMPLICIT NONE
!      ! Arguments
!      INTEGER, INTENT(IN) :: param_id, Numvalues
!      REAL, INTENT(OUT) :: Values(Numvalues)
!!***********************************************************************
!      values = Variable_data(param_id)%values_real
!      END SUBROUTINE getvar_values_real

!***********************************************************************
! getvalues_int - get values from parameter data structure
!***********************************************************************
      SUBROUTINE getvalues_int(param_id, Numvalues, Values)
      USE PRMS_MMFAPI, ONLY: Parameter_data
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: param_id, Numvalues
      INTEGER, INTENT(OUT) :: Values(Numvalues)
!***********************************************************************
      Values = Parameter_data(param_id)%int_values
      END SUBROUTINE getvalues_int

!***********************************************************************
! getvalues_dbl - get values from parameter data structure
!***********************************************************************
      SUBROUTINE getvalues_dbl(param_id, Numvalues, Values)
      USE PRMS_MMFAPI, ONLY: Parameter_data
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: param_id, Numvalues
      DOUBLE PRECISION, INTENT(OUT) :: Values(Numvalues)
!***********************************************************************
      values = Parameter_data(param_id)%values
      END SUBROUTINE getvalues_dbl

!***********************************************************************
! timestep_hours - time step increment in hours
!***********************************************************************
      DOUBLE PRECISION FUNCTION deltim()
      IMPLICIT NONE
      ! Functions
!***********************************************************************
      !deltim = lisfunction() ! need to make routine to get time step increment
      deltim = 24.0D0
      END FUNCTION deltim

!***********************************************************************
! dattim - get start, end, or current date and time
!***********************************************************************
      SUBROUTINE dattim(String, Datetime)
      USE PRMS_MODULE, ONLY: Endtime, Starttime
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday, Julian_day_absolute
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: String
      INTEGER, INTENT(OUT) :: Datetime(6)
      EXTERNAL compute_gregorian, error_stop
      ! Local variable
      INTEGER string_length
!***********************************************************************
      Datetime = 0
      string_length = LEN(String)
      IF ( String(:3)=='end' ) THEN
        Datetime = Endtime
      ELSEIF ( String(:3)=='now' ) THEN
        CALL compute_gregorian(Julian_day_absolute, Nowyear, Nowmonth, Nowday)
        Datetime(1) = Nowyear
        Datetime(2) = Nowmonth
        Datetime(3) = Nowday
        ! Datetime = LIS function
      ELSEIF ( string_length>4 ) THEN
        IF ( String(:5)=='start' ) THEN
          Datetime = Starttime
        ELSE
          CALL error_stop('invalid call to dattim')
        ENDIF
      ENDIF
      END SUBROUTINE dattim

!***********************************************************************
! decldim
! declare dimensions and set values in dimension data structure
!***********************************************************************
      INTEGER FUNCTION decldim(Dimname, Defval, Maxval, Desc)
      USE PRMS_MMFAPI, ONLY: MAXDIMENSIONS, Num_dimensions, Dimension_data
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Defval, Maxval
      CHARACTER(LEN=*), INTENT(IN) :: Dimname, Desc
      ! Functions
      INTEGER, EXTERNAL :: numchars
      EXTERNAL error_stop
!***********************************************************************
      Num_dimensions = Num_dimensions + 1
      IF ( Num_dimensions>MAXDIMENSIONS ) CALL error_stop('hard-coded number of dimensions exceeded, report to developers')
      Dimension_data(Num_dimensions)%name = Dimname
      Dimension_data(Num_dimensions)%default = Defval
      Dimension_data(Num_dimensions)%maximum = Maxval
      Dimension_data(Num_dimensions)%value = Defval
      Dimension_data(Num_dimensions)%length = numchars(Dimname)
      Dimension_data(Num_dimensions)%description = Desc

      decldim = 0
      END FUNCTION decldim

!***********************************************************************
! declfix
! declare fixed dimensions and set values in dimension data structure
!***********************************************************************
      INTEGER FUNCTION declfix(Dimname, Defval, Maxval, Desc)
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Defval, Maxval
      CHARACTER(LEN=*), INTENT(IN) :: Dimname, Desc
      ! Functions
      INTEGER, EXTERNAL :: decldim
      EXTERNAL :: setdimension
!***********************************************************************
      declfix = decldim(Dimname, Defval, Maxval, Desc)
      CALL setdimension(Dimname, Defval)
      END FUNCTION declfix

!***********************************************************************
! setdimension
! set dimension value in data structure based on value in Paramter File
!***********************************************************************
      SUBROUTINE setdimension(Dimname, Dim)
      USE PRMS_MMFAPI, ONLY: Num_dimensions, Dimension_data
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Dim
      CHARACTER(LEN=*), INTENT(IN) :: Dimname
      ! Functions
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      INTEGER i, nchars, nlen
!***********************************************************************
      nchars = numchars(Dimname)
      DO i = 1, Num_dimensions
        nlen = Dimension_data(i)%length
        IF ( nchars==nlen ) THEN
          IF ( Dimname==Dimension_data(i)%name(:nlen) ) THEN
            Dimension_data(i)%value = Dim
            EXIT
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE setdimension

!***********************************************************************
! getdim - get dimension number
!***********************************************************************
      INTEGER FUNCTION getdim(Dimname)
      USE PRMS_MMFAPI, ONLY: Num_dimensions, Dimension_data
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Dimname
      ! Functions
      INTEGER, EXTERNAL :: numchars
      ! Local Variables
      INTEGER i, nchars, nlen
!***********************************************************************
      getdim = -1
      nchars = numchars(Dimname)
      DO i = 1, Num_dimensions
        nlen = Dimension_data(i)%length
        IF ( nchars==nlen ) THEN
          IF ( Dimname==Dimension_data(i)%name(:nlen) ) THEN
            getdim = Dimension_data(i)%value
            EXIT
          ENDIF
        ENDIF
      ENDDO

      END FUNCTION getdim

!***********************************************************************
! control_integer
! control parameters are read, this sets integer values stored in the
! data base and checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_integer(Parmval, Paramname)
      USE PRMS_CONTROL_FILE, ONLY: Num_control_parameters, Control_parameter_data, Max_num_control_parameters
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      INTEGER, INTENT(OUT) :: Parmval
      ! Functions
      INTRINSIC :: TRIM
      EXTERNAL error_stop
      ! Local Variables
      INTEGER :: i, found
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_int(1)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        Num_control_parameters = Num_control_parameters + 1
        IF ( Num_control_parameters > Max_num_control_parameters ) CALL error_stop('exceeded maximum number of control parameters')
        PRINT *, 'WARNING, control parameter not in Control File: ', TRIM(Paramname), ', set to 0'
        Control_parameter_data(Num_control_parameters)%read_flag = 2 ! set to default
        Control_parameter_data(Num_control_parameters)%data_type = 1
        Control_parameter_data(Num_control_parameters)%numvals = 1
        Control_parameter_data(Num_control_parameters)%name = paramname
        Control_parameter_data(Num_control_parameters)%values_int(1) = 0 !???
      ENDIF

      control_integer = 0
      END FUNCTION control_integer

!***********************************************************************
! control_integer_array
! control parameters are read are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_integer_array(Parmval, Array_index, Paramname)
      USE PRMS_CONTROL_FILE, ONLY: Control_parameter_data, Num_control_parameters
      IMPLICIT NONE
      ! Arguments
      ! Array_index and Parmval not used, only used with MMF
      INTEGER, INTENT(IN) :: Array_index
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      INTEGER, INTENT(OUT) :: Parmval
      ! Functions
      EXTERNAL error_stop
      ! Local Variables
      INTEGER :: found, i
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_int(Array_index)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        PRINT *, 'invalid array control parameter: ', TRIM(Paramname)
        CALL error_stop('execution terminated')
      ENDIF

      control_integer_array = 0
      END FUNCTION control_integer_array

!***********************************************************************
! control_string
! control parameters are read are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_string(Parmval, Paramname)
      USE PRMS_CONTROL_FILE, ONLY: Num_control_parameters, Control_parameter_data, Max_num_control_parameters
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: TRIM
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      CHARACTER(LEN=*), INTENT(OUT) :: Parmval
      ! Local Variables
      INTEGER :: found, i
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_character(1)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        Num_control_parameters = Num_control_parameters + 1
        PRINT *, 'WARNING, control parameter not in Control File: ', TRIM(Paramname), ', set to blank'
        Control_parameter_data(Num_control_parameters)%read_flag = 2 ! set to default
        Control_parameter_data(Num_control_parameters)%data_type = 4
        Control_parameter_data(Num_control_parameters)%numvals = 1
        Control_parameter_data(Num_control_parameters)%name = paramname
        Control_parameter_data(Num_control_parameters)%values_int(1) = ' '
      ENDIF

      control_string = 0
      END FUNCTION control_string

!***********************************************************************
! control_string_array
! control parameters are read are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION control_string_array(Parmval, Paramname, Array_index)
      USE PRMS_CONTROL_FILE, ONLY: Control_parameter_data, Num_control_parameters
      IMPLICIT NONE
      ! Arguments
      ! Array_index and Parmval not used, only used with MMF
      INTEGER, INTENT(IN) :: Array_index
      CHARACTER(LEN=*), INTENT(IN) :: Paramname
      CHARACTER(LEN=*), INTENT(OUT) :: Parmval
      ! Functions
      EXTERNAL error_stop
      ! Local Variables
      INTEGER :: found, i
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          Parmval = Control_parameter_data(i)%values_character(Array_index)
          found = i
          EXIT
        ENDIF
      ENDDO
      IF ( found==0 ) THEN
        PRINT *, 'invalid array control parameter: ', TRIM(Paramname)
        CALL error_stop('execution terminated')
      ENDIF

      control_string_array = 0
      END FUNCTION control_string_array

!***********************************************************************
! getparamstring
! control parameters are read and verified this
! function checks to be sure a required parameter has a value (read or default)
!***********************************************************************
      INTEGER FUNCTION getparamstring(Module_name, Paramname, Numvalues, Data_type, Array_index, String)
      USE PRMS_MMFAPI, ONLY: Num_parameters
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Module_name, Paramname, Data_type
      INTEGER, INTENT(IN) :: Numvalues, Array_index
      CHARACTER(LEN=*), INTENT(OUT) :: String
      ! Functions
      INTRINSIC INDEX
      ! Local Variables
      INTEGER nchars, nchars_param, type_flag, num_values, i, j
      CHARACTER(LEN=16) :: dimenname
!***********************************************************************
      String = ' '
      ! Modname
      nchars_param = INDEX( Paramname, ' ') - 1
      ! Paramname(:nchars_param)
      nchars = INDEX( Dimenname, ' ') - 1
      num_values = -2
      IF ( num_values/=Numvalues ) THEN
        PRINT *, 'ERROR, number of values does not equal values for the dimension'
        PRINT *, '       parameter: ', Dimenname(:nchars), ' dimension value:', num_values
        PRINT *, '       dimension: ', Paramname(:nchars_param), ' number of values:', Numvalues
        ERROR STOP -1
      ENDIF
      nchars = INDEX( Data_type, ' ') - 1
      ! Data_type(:nchars)
      CALL set_data_type(Data_type, type_flag)

      DO j = 1, Num_parameters
          DO i = 1, Numvalues
            IF ( type_flag==1 ) THEN
            ELSEIF ( type_flag==2 ) THEN
            ELSEIF ( type_flag==3 ) THEN
            ELSEIF ( type_flag==4 ) THEN
            ENDIF
          ENDDO
        EXIT
      ENDDO

      getparamstring = 0
      END FUNCTION getparamstring

!***********************************************************************
! setparam - set real or integer parameter values read from Parameter File
!***********************************************************************
      SUBROUTINE setparam(Paramname, Numvalues, Data_type, Num_dims, Dim_string, Values, Ivalues)
      USE PRMS_MMFAPI
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Numvalues, Data_type, Num_dims, Ivalues(*)
      CHARACTER(LEN=*), INTENT(IN) :: Paramname, Dim_string(Num_dims)
      REAL, INTENT(IN) :: Values(*)
      ! Functions
      INTRINSIC TRIM, INDEX
      ! Local Variables
      INTEGER :: found, i, ii, j, k, ierr, iflg, comma, nvals
      CHARACTER(LEN=MAXCONTROL_LENGTH) dimen1
!***********************************************************************
      ierr = 0
      found = 0
      DO i = 1, Num_parameters
        IF ( Paramname==TRIM(Parameter_data(i)%param_name) ) THEN
          found = i
          IF ( Parameter_data(i)%data_flag/=Data_type ) THEN
            ierr = 1
            PRINT *, 'ERROR, Parameter: ', Paramname, ' data type does not match declared data type'
          ENDIF
          IF ( Parameter_data(i)%numvals==Numvalues ) THEN
            IF ( Data_type==2 ) THEN
              DO j = 1, Numvalues
                Parameter_data(found)%values(j) = Values(j)
              ENDDO
            ELSE
              DO j = 1, Numvalues
                Parameter_data(found)%int_values(j) = Ivalues(j)
              ENDDO
            ENDIF
          ELSE ! check for flexible dimension
            IF ( Numvalues==1 ) THEN ! set all values to single value
              IF ( Data_type==2 ) THEN
                DO j = 1, Parameter_data(found)%numvals
                  Parameter_data(found)%values(j) = Values(1)
                ENDDO
              ELSE
                DO j = 1, Parameter_data(found)%numvals
                  Parameter_data(found)%int_values(j) = Ivalues(1)
                ENDDO
              ENDIF
            ELSE
              nvals = Parameter_data(found)%numvals / 12
              IF ( nvals*12/=Parameter_data(found)%numvals) THEN
                iflg = 0
                IF ( Num_dims==1 .AND. TRIM(Dim_string(1))=='nmonths' ) iflg = 1
                IF ( Num_dims==2 ) THEN
                  IF ( TRIM(Dim_string(2))=='nmonths' ) iflg = 1
                ENDIF
                IF ( iflg==1 ) THEN
                  PRINT *, 'ERROR, parameter not evenly divisible by 12'
                  PRINT *, '       number of parameter values expected:', Parameter_data(i)%numvals
                  PRINT *, '       number of parameter values specified:', Numvalues
                  ERROR STOP -1
                ENDIF
              ENDIF
              comma = INDEX(Parameter_data(found)%dimen_names,',')
              IF ( comma==0 ) THEN
                dimen1 = TRIM(Parameter_data(found)%dimen_names)
              ELSE
                dimen1 = Parameter_data(found)%dimen_names(:(comma-1))
              ENDIF

              ! DANGER, messy IF's
              iflg = 0
              IF ( Numvalues==12 .AND. Nhru/=12 .AND. Num_dims==1 .AND. TRIM(Dim_string(1))=='nmonths' ) iflg = 2 ! set monthly
              IF ( Numvalues==Nhru .AND. Num_dims==1 .AND. TRIM(Dim_string(1))/='nmonths' ) iflg = 3 ! set nhru, nmonths

              k = 0
              IF ( iflg==3 ) THEN ! 12 sets of nhru values
                DO j = 1, 12
                  DO ii = 1, nvals
                    k = k + 1
                    IF ( Data_type==2 ) THEN
                      Parameter_data(found)%values(k) = Values(ii)
                    ELSE
                      Parameter_data(found)%int_values(k) = Ivalues(ii)
                    ENDIF
                  ENDDO
                ENDDO
              ELSEIF ( iflg==2 ) THEN ! dim sets of 12
                DO j = 1, 12
                  DO ii = 1, nvals
                    k = k + 1
                    IF ( Data_type==2 ) THEN
                      Parameter_data(found)%values(k) = Values(j)
                    ELSE
                      Parameter_data(found)%int_values(k) = Ivalues(j)
                    ENDIF
                  ENDDO
                ENDDO
              ELSE
!                print *, '??? not sure this can happen'
!                DO ii = 1, nvals
!                  DO j = 1, 12
!                    k = k + 1
!                    IF ( Data_type==2 ) THEN
!                      Parameter_data(found)%values(k) = Values(ii)
!                    ELSE
!                      Parameter_data(found)%int_values(k) = Ivalues(ii)
!                    ENDIF
!                  ENDDO
!                ENDDO
                !!!!!! add parameter expansion !!!!!!!!!! for nsub
                ierr = 1
                PRINT *, 'ERROR, Parameter: ', Paramname, ' number of values in getparam does not match declared number of values'
              ENDIF
            ENDIF
          ENDIF
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR, Parameter: ', Paramname, ' not declared'
        ierr = 1
      ENDIF
      IF ( ierr==1 ) ERROR STOP -1
 
      END SUBROUTINE setparam

!***********************************************************************
! getvarsize - return the number of values for a parameter
!***********************************************************************
      INTEGER FUNCTION getvarsize(Varname)
      USE PRMS_MMFAPI
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Varname
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: found, i
!***********************************************************************
      found = 0
      DO i = 1, Num_variables
        IF ( Varname==TRIM(Variable_data(i)%variable_name) ) THEN
          found = i
          getvarsize = Variable_data(i)%numvals
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) THEN
        PRINT *, 'ERROR, Variable: ', Varname, ' not declared'
        ERROR STOP -1
      ENDIF
 
      END FUNCTION getvarsize

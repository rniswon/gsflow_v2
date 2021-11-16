!***********************************************************************
! interfaces and types
!***********************************************************************
      MODULE PRMS_MMFAPI
        USE PRMS_CONSTANTS, ONLY: MAXCONTROL_LENGTH, MAXDESCRIPTION_LENGTH, ERROR_var, MAXFILE_LENGTH
        IMPLICIT NONE
        ! DANGER, DANGER, hard coded maximum number of paraemters and dimensions, DANGER, DANGER
        INTEGER, PARAMETER :: MAXDIMENSIONS = 48, MAXPARAMETERS = 256, MAXVARIABLES = 512
        INTEGER, PARAMETER :: Max_num_control_parameters = 250
        INTEGER, SAVE :: Num_parameters, Num_dimensions, Num_variables, Num_control_parameters  !, Total_parameters

        TYPE PRMS_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: param_name
             CHARACTER(LEN=MAXDESCRIPTION_LENGTH) :: short_description, long_description
             INTEGER :: numvals, data_flag, decl_flag, read_flag, nchars, id_num, scalar_flag
             INTEGER :: default_int, maximum_int, minimum_int, num_dimens, num_dim1, num_dim2
             CHARACTER(LEN=16) :: max_value, min_value, def_value, data_type
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: dimen_names, module_name, units
             INTEGER, POINTER :: values_int_0d ! Scalars
             INTEGER, POINTER :: values_int_1d(:)
             INTEGER, POINTER :: values_int_2d(:, :)
             REAL, POINTER :: values_real_0d
             REAL, POINTER :: values_real_1d(:)
             REAL, POINTER :: values_real_2d(:, :)
             REAL :: maximum, minimum, default_real
        END TYPE PRMS_parameter
        TYPE ( PRMS_parameter ), SAVE, ALLOCATABLE :: Parameter_data(:)

        TYPE PRMS_dimension
             CHARACTER(LEN=16) :: name
             INTEGER :: value, default, maximum, length
             CHARACTER(LEN=MAXDESCRIPTION_LENGTH) :: description
        END TYPE PRMS_dimension
        TYPE ( PRMS_dimension ), SAVE, ALLOCATABLE :: Dimension_data(:)

        TYPE PRMS_variable
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: variable_name
             CHARACTER(LEN=MAXDESCRIPTION_LENGTH) :: description
             INTEGER :: numvals, data_flag, decl_flag, get_flag, var_name_nchars, id_num, dim_flag
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: data_type, dimen_names, module_name, units
             INTEGER, POINTER :: values_int_0d ! Scalars
             INTEGER, POINTER :: values_int_1d(:)
             INTEGER, POINTER :: values_int_2d(:, :)
             REAL, POINTER :: values_real_0d
             REAL, POINTER :: values_real_1d(:)
             REAL, POINTER :: values_real_2d(:, :)
             DOUBLE PRECISION, POINTER :: values_dble_0d
             DOUBLE PRECISION, POINTER :: values_dble_1d(:)
             DOUBLE PRECISION, POINTER :: values_dble_2d(:,:)
        END TYPE PRMS_variable
        TYPE ( PRMS_variable ), SAVE, ALLOCATABLE :: Variable_data(:)

        ! read_flag: 0 = not set, 1 = set from control file, 2 = set to default
        TYPE PRMS_control_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: name
             INTEGER :: numvals, read_flag, data_type, index, allocate_flag
             INTEGER, ALLOCATABLE :: values_int(:)
             REAL, ALLOCATABLE :: values_real(:)
             CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: values_character(:)
        END TYPE PRMS_control_parameter
        TYPE ( PRMS_control_parameter ), SAVE, ALLOCATABLE :: Control_parameter_data(:)

      END MODULE PRMS_MMFAPI

MODULE PRMS_MMFSUBS
  interface declvar_subs
    SUBROUTINE declvar_dble(Modname, Varname, Dimenname, Numvalues, Desc, Units, Value)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      DOUBLE PRECISION, TARGET :: Value
    end subroutine

    SUBROUTINE declvar_dble_1d(Modname, Varname, Dimenname, Numvalues, Desc, Units, Values)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      DOUBLE PRECISION, TARGET :: Values(Numvalues)
    end subroutine

    SUBROUTINE declvar_real_0d(Modname, Varname, Dimenname, Numvalues, Desc, Units, Value)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      REAL, TARGET :: Value
    end subroutine

    SUBROUTINE declvar_real(Modname, Varname, Dimenname, Numvalues, Desc, Units, Values)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      REAL, TARGET :: Values(Numvalues)
    end subroutine

    SUBROUTINE declvar_real_2d(Modname, Varname, Dimenname, Dim1, Dim2, Desc, Units, Values)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Dim1, Dim2
      REAL, TARGET :: Values(Dim1, Dim2)
    end subroutine

    SUBROUTINE declvar_int_0d(Modname, Varname, Dimenname, Numvalues, Desc, Units, Value)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, TARGET :: Value
    end subroutine

    SUBROUTINE declvar_int(Modname, Varname, Dimenname, Numvalues, Desc, Units, Values)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, TARGET :: Values(Numvalues)
    end subroutine

    SUBROUTINE declvar_int_2d(Modname, Varname, Dimenname, Dim1, Dim2, Desc, Units, Values)
      CHARACTER(LEN=*), INTENT(IN) :: Modname, Varname, Dimenname, Desc, Units
      INTEGER, INTENT(IN) :: Dim1, Dim2
      INTEGER, TARGET :: Values(Dim1, Dim2)
    end subroutine

  end interface

END MODULE PRMS_MMFSUBS

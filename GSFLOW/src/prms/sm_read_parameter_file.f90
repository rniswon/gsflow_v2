submodule(PRMS_READ_PARAM_FILE) sm_read_parameter_file
  implicit none
  integer, save :: Param_unit, Read_parameters
contains

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
  module subroutine read_parameter_file_dimens()
    use PRMS_CONSTANTS, only: MAXLINE_LENGTH, DEBUG_less
    use PRMS_MODULE, only: Print_debug, EQULS, Param_file
    use prms_utils, only: numchars, PRMS_open_input_file, read_error, write_outfile
    implicit none
      ! Functions
    intrinsic :: TRIM
    ! Local Variables
    character(LEN=16) :: string, dimname
    character(LEN=MAXLINE_LENGTH) :: line
    character(LEN=24) :: dimstring
    integer nchars, ios, dimen_value
    !***********************************************************************
    call PRMS_open_input_file(Param_unit, Param_file, 'param_file', 0, ios)
    if (ios /= 0) stop
    if (Print_debug > DEBUG_less) then
      call write_outfile(EQULS)
      call write_outfile('Using PRMS Parameter File: '//Param_file)
    end if

    ! Echo Parmeter File Header and comment lines
    read (Param_unit, FMT='(A)', IOSTAT=ios) line
    if (ios /= 0) call read_error(13, 'description')
    if (Print_debug > DEBUG_less) then
      call write_outfile('Description: '//trim(line))
      call write_outfile(EQULS)
      call write_outfile('Comment lines:')
    end if

    ! Find start of dimensions section
    do
      read (Param_unit, '(A)', IOSTAT=ios) line
      if (ios == -1) call read_error(13, 'end of file found before dimensions')
      if (ios /= 0) call read_error(13, 'comment')
      if (line(:16) == '** Dimensions **') exit
      if (Print_debug > DEBUG_less) call write_outfile(trim(line))
    end do
    if (line(:16) /= '** Dimensions **') call read_error(11, 'missing dimension section: '//trim(line))
    if (Print_debug > DEBUG_less) then
      call write_outfile(EQULS)
      call write_outfile('Using dimensions    number')
    end if

    ! Read all dimensions

    do
      read (Param_unit, '(A)', IOSTAT=ios) string
      if (ios == -1) call read_error(13, 'end of file found before parameter section')
      if (ios /= 0) call read_error(11, 'missing dimension #### delimiter')
      if (string(:4) == '    ') cycle
      if (string(:2) == '//') cycle
      if (string == '** Parameters **') exit ! stop reading if end of dimensions section
      !IF ( string(:4)/='####' ) CALL read_error(11, 'missing dimension #### delimiter '//string)
      if (string(:4) /= '####') then
        print *, 'Warning, ignoring dimension line: ', string
        cycle
      end if
      read (Param_unit, *, IOSTAT=ios) dimname
      nchars = numchars(dimname)
      if (ios /= 0) call read_error(11, 'missing dimension name: '//dimname(:nchars))
      read (Param_unit, *, IOSTAT=ios) dimen_value
      if (ios /= 0) call read_error(11, 'missing dimension value')

      call setdimension(dimname, dimen_value)

      if (dimen_value == 0) then
        if (Print_debug > DEBUG_less) print *, 'Warning, dimension: ', dimname(:nchars), ' is not needed, value specified = 0'
      end if
      if (Print_debug > DEBUG_less) then
        write (dimstring, '(A,I8)') dimname, dimen_value
        call write_outfile(dimstring)
      end if
    end do
    if (Print_debug > DEBUG_less) call write_outfile(EQULS)
  end subroutine read_parameter_file_dimens

!***********************************************************************
! Read Parameter File Dimensions
!***********************************************************************
  module subroutine read_parameter_file_params()
    use PRMS_CONSTANTS, only: MAXCONTROL_LENGTH, DEBUG_less
    use PRMS_MODULE, only: Print_debug
    use PRMS_CONTROL_FILE, only: Control_parameter_data, Param_file_control_parameter_id
    use prms_utils, only: PRMS_open_input_file, read_error
    implicit none
    ! Functions
    intrinsic :: trim, scan
    ! Local Variables
    character(LEN=MAXCONTROL_LENGTH) :: string
    character(LEN=MAXCONTROL_LENGTH) :: paramstring
    character(LEN=MAXCONTROL_LENGTH) :: dim_string(2)
    integer nchars, ios, num_dims, num_param_values, i, j, k, param_type, num, inum, numfiles, ii, duplicate, found
    integer, allocatable :: idmy(:)
    real, allocatable :: dmy(:)
    character(LEN=16), allocatable :: cdmy(:)
    !***********************************************************************
    ! Find parameter section
    rewind (Param_unit)
    do
      read (Param_unit, '(A)', IOSTAT=ios) string
      if (ios == -1) call read_error(11, 'end of file found before parameter section') ! found end of Parameter File
      if (string(:16) == '** Parameters **') exit ! stop reading if end of dimensions section
    end do

    ! Read all parameters and verify
    numfiles = Control_parameter_data(Param_file_control_parameter_id)%numvals
    Read_parameters = 0
    do k = 1, numfiles

      if (k > 1) then
        close (Param_unit)
        call PRMS_open_input_file(Param_unit, Control_parameter_data(Param_file_control_parameter_id)%values_character(k), &
                                  'param_file', 0, ios)
      end if

      do
        read (Param_unit, '(A)', IOSTAT=ios) string
        if (ios == -1) exit ! found end of a Parameter File
        if (ios /= 0) call read_error(11, 'missing parameter #### delimiter')
        if (string(:4) == '    ') cycle ! skip blank lines
        if (string(:2) == '//') cycle ! skip comment lines
        !IF ( string(:4)/='####' ) CALL read_error(11, 'missing parameter #### delimiter')
        if (string(:4) /= '####') cycle

        read (Param_unit, '(A)', IOSTAT=ios) paramstring ! parameter name
        if (ios /= 0) call read_error(11, 'missing parameter name')

        nchars = scan(paramstring, ' ') - 1
        read (Param_unit, *, IOSTAT=ios) num_dims
        if (ios /= 0) call read_error(11, 'invalid number of dimensions: '//paramstring(:nchars))
        if (num_dims > 2) call read_error(11, 'number of dimensions > 3: '//paramstring(:nchars))

        num = 1
        do i = 1, num_dims
          read (Param_unit, '(A)', IOSTAT=ios) dim_string(i)
          if (ios /= 0) call read_error(11, 'invalid dimension for parameter: '//paramstring(:nchars))
          inum = getdim(dim_string(i))
          if (inum == -1) call read_error(11, trim(dim_string(i)))
          num = num * inum
        end do

        read (Param_unit, *, IOSTAT=ios) num_param_values
        if (ios /= 0) call read_error(11, 'invalid number of parameter values: '//paramstring(:nchars))
        !        IF ( num/=num_param_values ) CALL read_error(11, 'invalid number of parameter values based on specified dimensions '//paramstring(:nchars))

        read (Param_unit, *, IOSTAT=ios) param_type
        if (ios /= 0) call read_error(11, 'invalid parameter type '//paramstring(:nchars))
        if (param_type < 1 .or. param_type > 4 .or. param_type == 3) &
            call read_error(11, 'invalid parameter type: '//paramstring(:nchars))

        ! check to see if parameter already read
        duplicate = 0
        found = 0
        do ii = 1, Num_parameters
          if (paramstring(:nchars) == trim(Parameter_data(ii)%param_name)) then
            found = ii
            if (Parameter_data(ii)%read_flag == 1) then
              print '(/,3A)', 'WARNING, parameter: ', trim(paramstring), ' specified more than once'
              inum = min(num_param_values, 5)
              print *, '        Ignoring previous value(s)'
              duplicate = ii
              exit
            end if
          end if
        end do
        if (found == 0) then
          if (Print_debug > DEBUG_less) print '(3A)', 'WARNING, parameter: ', trim(paramstring(:nchars)), &
                                              ' is ignored as it is not used'
          cycle
        end if

        if (param_type == 1) then
          allocate (idmy(num_param_values), dmy(1), cdmy(1))
          read (Param_unit, *, IOSTAT=ios) (idmy(j), j=1, num_param_values)
          if (ios /= 0) call read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
          if (duplicate > 0) &
    &         print '(A,5I8)', '         Using (up to 5 values printed):', (idmy(j), j=1, inum)
        elseif (param_type == 2) then
          allocate (dmy(num_param_values), idmy(1), cdmy(1))
          read (Param_unit, *, IOSTAT=ios) (dmy(j), j=1, num_param_values)
          if (ios /= 0) call read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
          if (duplicate > 0) &
    &         print '(A,5F8.2)', '         Using (up to 5 values printed): ', (dmy(j), j=1, inum)
        else  ! param_type = 4
          allocate (cdmy(num_param_values), idmy(1), dmy(1))
          read (Param_unit, *, IOSTAT=ios) (cdmy(j), j=1, num_param_values)
          if (ios /= 0) call read_error(11, 'incorrect number of parameter values: '//paramstring(:nchars))
          if (duplicate > 0) &
    &         print '(A,5F8.2)', '         Using (up to 5 values printed): ', (cdmy(j), j=1, inum)
        end if
        if (duplicate > 0) print *, ' '
        call setparam(paramstring(:nchars), num_param_values, param_type, num_dims, dim_string, dmy, idmy, cdmy)
        Read_parameters = Read_parameters + 1
        Parameter_data(found)%read_flag = 1
        deallocate (dmy, idmy, cdmy)
      end do
    end do

    close (param_unit)
  end subroutine read_parameter_file_params

!***********************************************************************
! Check for parameters declared but not in Parameter File
!***********************************************************************
  module subroutine check_parameters()
    implicit none
    ! Functions
    intrinsic :: trim
    ! Local Variables
    integer :: i
    !***********************************************************************
    print *, ' '
    do i = 1, Num_parameters
      if (Parameter_data(i)%decl_flag == 1 .and. Parameter_data(i)%read_flag == 0) then
        print *, 'WARNING, parameter: ', trim(Parameter_data(i)%param_name), ' is not specified'
        if (Parameter_data(i)%data_flag == 1) then
          print *, '         Set to default value:', Parameter_data(i)%default_int
        elseif (Parameter_data(i)%data_flag == 2) then
          print *, '         Set to default value:', Parameter_data(i)%default_real
        end if
      end if
    end do

  end subroutine check_parameters

!***********************************************************************
! Allocate and initialize parameter data base
! DANGER, DANGER, hard coded maximum number of paraemters, DANGER, DANGER
!***********************************************************************
  module subroutine setup_params()
    implicit none
    ! Local Variables
    integer :: i
    !***********************************************************************
    ! allocate and store parameter data
    allocate (Parameter_data(MAXPARAMETERS)) ! allow for extra parameters being expected
    do i = 1, MAXPARAMETERS
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
      Parameter_data(i)%num_dim1 = 0
      Parameter_data(i)%num_dim2 = 0
      Parameter_data(i)%scalar_flag = 0
    end do
    Num_parameters = 0

  end subroutine setup_params

!***********************************************************************
! Allocate and initialize dimension data base
! WARNING, hard coded, DANGER, DANGER
!***********************************************************************
  module subroutine setup_dimens()
    implicit none
    ! Local Variables
    integer :: i
    !***********************************************************************
    ! allocate and initialize dimension data
    allocate (Dimension_data(MAXDIMENSIONS)) ! allow for hard-coded maximum dimensions
    do i = 1, MAXDIMENSIONS
      Dimension_data(i)%name = ' '
      Dimension_data(i)%value = 0
      Dimension_data(i)%default = 0
      Dimension_data(i)%maximum = 0
      Dimension_data(i)%description = ' '
    end do
    Num_dimensions = 0

  end subroutine setup_dimens

!***********************************************************************
! declparam - set up memory for parameters
!***********************************************************************
  integer module function declparam(Modname, Paramname, Dimenname, Datatype, &
                                    Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units)
    use PRMS_CONSTANTS, only: ERROR_param
    use PRMS_MODULE, only: Ndepl
    use PRMS_MMFAPI, only: set_data_type
    use prms_utils, only: error_stop, numchars, read_error
    implicit none
    ! Arguments
    character(LEN=*), intent(IN) :: Modname, Paramname, Dimenname, Datatype
    character(LEN=*), intent(IN) :: Defvalue, Minvalue, Maxvalue, Descshort, Desclong, Units
    ! INTRINSIC
    intrinsic :: INDEX, TRIM
    ! Functions
    ! INTEGER, EXTERNAL :: isdeclared

    ! Local Variables
    integer :: comma, j, ndimen, nval, nvals, nvals2, declared, numvalues, type_flag, iset, i, itemp
    real :: temp
    character(LEN=16) :: dimen1, dimen2, ctemp
    !***********************************************************************
    declparam = 0
    !!!!!!!!!!!! check to see if already in data structure
    ! doesn't check to see if declared the same, uses first values
    call check_parameters_declared(Paramname, Modname, declared)
    if (declared == 1) return

    ! current value of Num_parameters is the number that have been declared
    Num_parameters = Num_parameters + 1
    if (Num_parameters > MAXPARAMETERS) stop 'ERROR, hard-coded number of parameters exceeded, report to developers'

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
    Parameter_data(Num_parameters)%num_dim1 = 0
    Parameter_data(Num_parameters)%num_dim2 = 0
    Parameter_data(Num_parameters)%scalar_flag = 0

    call set_data_type(Datatype, type_flag)
    if (type_flag < 1 .or. type_flag > 4 .or. type_flag == 3) &
        call read_error(16, Paramname//': data type not implemented: '//Datatype)
    Parameter_data(Num_parameters)%data_flag = type_flag

    ! get dimension number of values
    dimen2 = ' '
    ndimen = numchars(Dimenname)
    comma = index(Dimenname, ',')
    if (comma == 0) then
      dimen1 = Dimenname(:ndimen)
      Parameter_data(Num_parameters)%num_dimens = 1
    else
      dimen1 = Dimenname(:(comma - 1))
      dimen2 = Dimenname((comma + 1):ndimen)
      Parameter_data(Num_parameters)%num_dimens = 2
    end if

    if (dimen1(:3) == 'one') Parameter_data(Num_parameters)%scalar_flag = 1

    numvalues = getdim(trim(dimen1))
    Parameter_data(Num_parameters)%num_dim1 = numvalues

    if (numvalues == -1) call read_error(11, trim(dimen1))
    if (comma > 0) then
      nvals2 = getdim(trim(dimen2))
      if (nvals2 == -1) call read_error(11, trim(dimen2))
      Parameter_data(Num_parameters)%num_dim2 = nvals2
      numvalues = numvalues * nvals2
    end if
    Parameter_data(Num_parameters)%numvals = numvalues

    ! could add string and double
    if (type_flag == 1) then
      read (defvalue, *) itemp
      Parameter_data(Num_parameters)%default_int = itemp
      if (Parameter_data(Num_parameters)%num_dimens == 1) then
        if (Parameter_data(Num_parameters)%scalar_flag == 1) then
          allocate (Parameter_data(Num_parameters)%values_int_0d)
          Parameter_data(Num_parameters)%values_int_0d = itemp
        else
          allocate (Parameter_data(Num_parameters)%values_int_1d(numvalues))
          do i = 1, numvalues
            Parameter_data(Num_parameters)%values_int_1d(i) = itemp
          end do
        end if
      else
        allocate (Parameter_data(Num_parameters)%values_int_2d(Parameter_data(Num_parameters)%num_dim1, nvals2))
        do i = 1, Parameter_data(Num_parameters)%num_dim1
          do j = 1, nvals2
            Parameter_data(Num_parameters)%values_int_2d(i, j) = itemp
          end do
        end do
      end if
    elseif (type_flag == 2) then
      read (defvalue, *) temp
      Parameter_data(Num_parameters)%default_real = temp
      if (Parameter_data(Num_parameters)%num_dimens == 1) then
        if (Parameter_data(Num_parameters)%scalar_flag == 1) then
          allocate (Parameter_data(Num_parameters)%values_real_0d)
          Parameter_data(Num_parameters)%values_real_0d = temp
        else
          if (trim(dimen1) == 'ndeplval') then
            ! Special case to handle snarea_curve
            allocate (Parameter_data(Num_parameters)%values_real_2d(11, Ndepl))
            do i = 1, Ndepl
              do j = 1, 11
                Parameter_data(Num_parameters)%values_real_2d(j, i) = temp
              end do
            end do
          else
            allocate (Parameter_data(Num_parameters)%values_real_1d(numvalues))
            do i = 1, numvalues
              Parameter_data(Num_parameters)%values_real_1d(i) = temp
            end do
          end if
        end if
      else
        allocate (Parameter_data(Num_parameters)%values_real_2d(Parameter_data(Num_parameters)%num_dim1, nvals2))
        do i = 1, Parameter_data(Num_parameters)%num_dim1
          do j = 1, nvals2
            Parameter_data(Num_parameters)%values_real_2d(i, j) = temp
          end do
        end do
      end if
    elseif (type_flag == 4) then ! only allow for a 1-D array
      read (defvalue, *) ctemp
      Parameter_data(Num_parameters)%def_char = ctemp
      if (Parameter_data(Num_parameters)%num_dimens == 1) then
        if (Parameter_data(Num_parameters)%scalar_flag == 1) then
          allocate (Parameter_data(Num_parameters)%values_char_0d)
          Parameter_data(Num_parameters)%values_char_0d = ctemp
        else
          allocate (Parameter_data(Num_parameters)%values_char_1d(numvalues))
          do i = 1, numvalues
            Parameter_data(Num_parameters)%values_char_1d(i) = ctemp
          end do
        end if
      !else
      !  allocate (Parameter_data(Num_parameters)%values_char_2d(Parameter_data(Num_parameters)%num_dim1, nvals2))
      !  do i = 1, Parameter_data(Num_parameters)%num_dim1
      !    do j = 1, nvals2
      !      Parameter_data(Num_parameters)%values_char_2d(i, j) = ctemp
      !    end do
      !  end do
      else
          print *, 'ERROR, string parameters must be an 1-D array', Paramname
          stop
      end if
    end if

    iset = 0
    nval = len(Minvalue)
    if (nval > 6) then
      if (Minvalue(:7) == 'bounded') iset = 1
    end if

    if (iset == 1) then
      if (type_flag == 1) then  ! bounded parameters should all be integer
        nvals = getdim(trim(Maxvalue))
        if (nvals == -1) call read_error(11, Maxvalue)
        Parameter_data(Num_parameters)%maximum_int = nvals
        Parameter_data(Num_parameters)%minimum_int = Parameter_data(Num_parameters)%default_int
      else
        call error_stop('bounded parameter must be integer type', ERROR_param)
      end if
    else
      if (type_flag == 1) then
        read (Maxvalue, *) Parameter_data(Num_parameters)%maximum_int
        read (Minvalue, *) Parameter_data(Num_parameters)%minimum_int
      else
        read (Maxvalue, *) Parameter_data(Num_parameters)%maximum
        read (Minvalue, *) Parameter_data(Num_parameters)%minimum
      end if
    end if

    declparam = 0
  end function declparam

!***********************************************************************
! check_parameters_declared - check for parameters being declared more than once
!***********************************************************************
  module subroutine check_parameters_declared(Parmname, Modname, Iret)
    use PRMS_CONSTANTS, only: DEBUG_less
    use PRMS_MODULE, only: Print_debug
    use prms_utils, only: numchars
    implicit none
    ! Arguments
    character(LEN=*), intent(IN) :: Parmname, Modname
    integer, intent(OUT) :: Iret
    ! Functions
    intrinsic :: TRIM

    ! Local Variables
    integer :: i, nchars
    !***********************************************************************
    Iret = 0
    nchars = numchars(Parmname)
    do i = 1, Num_parameters
      if (nchars == Parameter_data(i)%nchars) then
        if (Parmname(:nchars) == Parameter_data(i)%param_name(:nchars)) then
          if (Parameter_data(i)%decl_flag == 1) then
            if (Print_debug > DEBUG_less) then
              print *, 'Parameter: ', trim(Parmname), ' declared more than once'
              print *, 'First declared by module: ', trim(Parameter_data(Num_parameters)%module_name)
              print *, 'Also declared by module: ', trim(Modname)
              print *, 'Model uses values based on first declare'
            end if
            Iret = 1
          end if
          exit
        end if
      end if
    end do
  end subroutine check_parameters_declared

!***********************************************************************
! getparam_real_0d - get real scalar parameter values
!***********************************************************************
  module function getparam_real_0d(Modname, Paramname, Numvalues, Values) result(res)
    use PRMS_CONSTANTS, only: ERROR_param
    use PRMS_MODULE, only: Parameter_check_flag
    implicit none
    ! Arguments
    integer :: res  ! Function result
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    ! values could be any data type
    real, intent(OUT) :: Values
    ! Functions
    intrinsic :: TRIM

    ! Local Variables
    integer :: found, param_id, i, ierr
    !***********************************************************************
    Values = 0.0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' number of values in getparam_real_0d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'real') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' data type does in getparam_real_0d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    if (Parameter_check_flag == 1) then
      if (Parameter_data(param_id)%values_real_0d > Parameter_data(param_id)%maximum) then
        print '(/,3A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id
        print '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_0d, '; maximum value: ', &
 &                               Parameter_data(param_id)%maximum
      end if
      if (Parameter_data(param_id)%values_real_0d < Parameter_data(param_id)%minimum) then
        print '(/,3A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id
        print '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_0d, '; minimum value: ', &
 &                               Parameter_data(param_id)%minimum
      end if
    end if
    Values = Parameter_data(param_id)%values_real_0d

    res = 0
  end function getparam_real_0d

!***********************************************************************
! getparam_real - get real 1-dimensional parameter values
!***********************************************************************
  module function getparam_real_1d(Modname, Paramname, Numvalues, Values) result(res)
    use prms_constants, only: ERROR_param
    use PRMS_MODULE, only: Parameter_check_flag !, Hru_type
    implicit none
    ! Arguments
    integer :: res  ! Function result
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    ! values could be any data type
    real, intent(OUT) :: Values(:)
    ! Functions
    intrinsic :: TRIM
    ! Local Variables
    integer :: found, param_id, i, ierr
    !***********************************************************************
    Values = 0.0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' number of values in getparam_real_1d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'real') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' data type does in getparam_real_1d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    if (Parameter_check_flag == 1) then
      do i = 1, Numvalues
        !if ( Hru_type(i)==0 .OR. Hru_type(i)==2 ) CYCLE
        if ( Parameter_data(param_id)%values_real_1d(i) > Parameter_data(param_id)%maximum ) then
          print '(/,3A,I0,A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id, '; HRU: ', i
          print '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_1d(i), '; maximum value: ', &
     &                             Parameter_data(param_id)%maximum
        end if
        if (Parameter_data(param_id)%values_real_1d(i) < Parameter_data(param_id)%minimum) then
          print '(/,3A,I0,A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id, '; HRU: ', i
          print '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_1d(i), '; minimum value: ', &
     &                             Parameter_data(param_id)%minimum
        end if
      end do
    end if
    Values = Parameter_data(param_id)%values_real_1d

    res = 0
  end function getparam_real_1d

!***********************************************************************
! getparam_real_2d - get real parameter values
!***********************************************************************
  module function getparam_real_2d(Modname, Paramname, Numvalues, Values) result(res)
    use PRMS_CONSTANTS, only: ERROR_param
    use PRMS_MODULE, only: Parameter_check_flag
    implicit none
    ! Arguments
    integer :: res  ! Function result
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    ! values could be any data type
    real, intent(OUT) :: Values(:, :)
    ! Functions
    intrinsic :: TRIM

    ! Local Variables
    integer :: found, param_id, i, ierr, j
    !***********************************************************************
    Values = 0.0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' number of values in getparam_real_2d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'real') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' data type does in getparam_real_2d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    IF ( Parameter_check_flag==1 ) THEN
      DO i = 1, Parameter_data(param_id)%num_dim1
        DO j = 1, Parameter_data(param_id)%num_dim2
          IF ( Parameter_data(param_id)%values_real_2d(i,j) > Parameter_data(param_id)%maximum ) THEN
            PRINT '(/,3A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id
            PRINT '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_2d(i,j), '; maximum value: ', &
     &                               Parameter_data(param_id)%maximum
          ENDIF
          IF ( Parameter_data(param_id)%values_real_2d(i,j) < Parameter_data(param_id)%minimum ) THEN
            PRINT '(/,3A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id
            PRINT '(A,F0.5,A,F0.5)', '         value: ', Parameter_data(param_id)%values_real_2d(i,j), '; minimum value: ', &
     &                               Parameter_data(param_id)%minimum
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    Values = Parameter_data(param_id)%values_real_2d

    res = 0
  end function getparam_real_2d

!***********************************************************************
! getparam_int_0d - get integer scalar parameter values
!***********************************************************************
  module function getparam_int_0d(Modname, Paramname, Numvalues, Values) result(res)
    USE PRMS_CONSTANTS, ONLY: ERROR_param
    USE PRMS_MODULE, ONLY: Parameter_check_flag
    implicit none
    ! Arguments
    integer :: res
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    integer, intent(OUT) :: Values
    ! Functions
    intrinsic :: TRIM
    ! Local Variables
    integer :: found, param_id, i, ierr
    !***********************************************************************
    Values = 0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' number of values in getparam_int_0d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'integer') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' data type does in getparam_int_0d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    if (Parameter_check_flag == 1) then
      if (Parameter_data(param_id)%values_int_0d > Parameter_data(param_id)%maximum_int) then
        print '(/,3A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id
        print '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_0d, '; maximum value: ', &
 &                             Parameter_data(param_id)%maximum_int
      end if
      if (Parameter_data(param_id)%values_int_0d < Parameter_data(param_id)%minimum_int) then
        print '(/,3A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id
        print '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_0d, '; minimum value: ', &
 &                             Parameter_data(param_id)%minimum_int
      end if
    end if

    Values = Parameter_data(param_id)%values_int_0d
    ! CALL getvalues_int(param_id, Numvalues, Values)

    res = 0
  end function getparam_int_0d

!***********************************************************************
! getparam_int_1d - get 1-dimensional integer parameter values
!***********************************************************************
  module function getparam_int_1d(Modname, Paramname, Numvalues, Values) result(res)
    USE PRMS_CONSTANTS, ONLY: ERROR_param
    USE PRMS_MODULE, ONLY: Parameter_check_flag
    implicit none
    ! Arguments
    integer :: res
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    integer, intent(OUT) :: Values(:)
    ! Functions
    intrinsic :: TRIM
    ! Local Variables
    integer :: found, param_id, i, ierr
    !***********************************************************************
    Values = 0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' number of values in getparam_int_1d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'integer') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' data type does in getparam_int_1d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    if (Parameter_check_flag == 1) then
      do i = 1, Numvalues
        if (Parameter_data(param_id)%values_int_1d(i) > Parameter_data(param_id)%maximum_int) then
          print '(/,3A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id
          print '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_1d(i), '; maximum value: ', &
   &                             Parameter_data(param_id)%maximum_int
        end if
        if (Parameter_data(param_id)%values_int_1d(i) < Parameter_data(param_id)%minimum_int) then
          print '(/,3A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id
          print '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_1d(i), '; minimum value: ', &
   &                             Parameter_data(param_id)%minimum_int
        end if
      end do
    end if

    Values = Parameter_data(param_id)%values_int_1d
    ! CALL getvalues_int(param_id, Numvalues, Values)

    res = 0
  end function getparam_int_1d

!***********************************************************************
! getparam_int_2d - get integer 2-dimensional parameter values
!***********************************************************************
  module function getparam_int_2d(Modname, Paramname, Numvalues, Values) result(res)
    USE PRMS_CONSTANTS, ONLY: ERROR_param
    USE PRMS_MODULE, ONLY: Parameter_check_flag
    implicit none
    ! Arguments
    integer :: res
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    integer, intent(OUT) :: Values(:, :)
    ! Functions
    intrinsic :: TRIM
    ! Local Variables
    integer :: found, param_id, i, j, ierr
    !***********************************************************************
    Values = 0
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' number of values in getparam_int_2d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'integer') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
     &             ' data type does in getparam_int_2d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    IF ( Parameter_check_flag==1 ) THEN
      DO i = 1, Parameter_data(param_id)%num_dim1
        DO j = 1, Parameter_data(param_id)%num_dim2
          IF ( Parameter_data(param_id)%values_int_2d(i,j) > Parameter_data(param_id)%maximum_int ) THEN
            PRINT '(/,3A,I0)', 'WARNING, value > maximum value for parameter: ', Paramname, '; index: ', param_id
            PRINT '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_2d(i,j), '; maximum value: ', &
     &                             Parameter_data(param_id)%maximum_int
          ENDIF
          IF ( Parameter_data(param_id)%values_int_2d(i,j) < Parameter_data(param_id)%minimum_int ) THEN
            PRINT '(/,3A,I0)', 'WARNING, value < minimum value for parameter: ', Paramname, '; index: ', param_id
            PRINT '(A,F0.5,A,I0)', '         value: ', Parameter_data(param_id)%values_int_2d(i,j), '; minimum value: ', &
     &                             Parameter_data(param_id)%minimum_int
          ENDIF
        ENDDO
      ENDDO
    ENDIF

    Values = Parameter_data(param_id)%values_int_2d
    ! CALL getvalues_int(param_id, Numvalues, Values)

    res = 0
  end function getparam_int_2d

!***********************************************************************
! decldim
! declare dimensions and set values in dimension data structure
!***********************************************************************
  integer module function decldim(Dimname, Defval, Maxval, Desc)
    use PRMS_CONSTANTS, only: ERROR_dim
    use prms_utils, only: error_stop, numchars
    implicit none
    ! Arguments
    integer, intent(IN) :: Defval, Maxval
    character(LEN=*), intent(IN) :: Dimname, Desc
    !***********************************************************************
    Num_dimensions = Num_dimensions + 1
    if (Num_dimensions > MAXDIMENSIONS) call error_stop('hard-coded number of dimensions exceeded, report to developers', &
   &     ERROR_dim)
    Dimension_data(Num_dimensions)%name = Dimname
    Dimension_data(Num_dimensions)%default = Defval
    Dimension_data(Num_dimensions)%maximum = Maxval
    Dimension_data(Num_dimensions)%value = Defval
    Dimension_data(Num_dimensions)%length = numchars(Dimname)
    Dimension_data(Num_dimensions)%description = Desc

    decldim = 0
  end function decldim

!***********************************************************************
! declfix
! declare fixed dimensions and set values in dimension data structure
!***********************************************************************
  integer module function declfix(Dimname, Defval, Maxval, Desc)
    implicit none
    ! Arguments
    integer, intent(IN) :: Defval, Maxval
    character(LEN=*), intent(IN) :: Dimname, Desc
    !***********************************************************************
    declfix = decldim(Dimname, Defval, Maxval, Desc)
    call setdimension(Dimname, Defval)
  end function declfix

!***********************************************************************
! setdimension
! set dimension value in data structure based on value in Parameter File
!***********************************************************************
  module subroutine setdimension(Dimname, Dim)
    use prms_utils, only: numchars
    implicit none
    ! Arguments
    integer, intent(IN) :: Dim
    character(LEN=*), intent(IN) :: Dimname
    ! Local Variables
    integer :: i, nchars, nlen
    !***********************************************************************
    nchars = numchars(Dimname)
    do i = 1, Num_dimensions
      nlen = Dimension_data(i)%length
      if (nchars == nlen) then
        if (Dimname == Dimension_data(i)%name(:nlen)) then
          Dimension_data(i)%value = Dim
          exit
        end if
      end if
    end do

  end subroutine setdimension

!***********************************************************************
! getdim - get dimension number
!***********************************************************************
  integer module function getdim(Dimname)
    use prms_utils, only: numchars
    implicit none
    ! Arguments
    character(LEN=*), intent(IN) :: Dimname

    ! Local Variables
    integer :: i, nchars, nlen
    !***********************************************************************
    getdim = -1
    nchars = numchars(Dimname)
    do i = 1, Num_dimensions
      nlen = Dimension_data(i)%length
      if (nchars == nlen) then
        if (Dimname == Dimension_data(i)%name(:nlen)) then
          getdim = Dimension_data(i)%value
          exit
        end if
      end if
    end do

  end function getdim

!***********************************************************************
! getparam_string - get 1-dimensional string parameter values
!***********************************************************************
  integer module function getparam_string(Modname, Paramname, Numvalues, Values)
    use PRMS_CONSTANTS, only: ERROR_param
    use PRMS_MMFAPI, only: set_data_type
    implicit none
    ! Arguments
    character(LEN=*), intent(IN) :: Modname, Paramname
    integer, intent(IN) :: Numvalues
    character(LEN=16), intent(OUT) :: Values(:)
    ! Functions
    intrinsic :: TRIM
    ! Local Variables
    integer :: found, param_id, i, ierr
    !***********************************************************************
    Values = ' '
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = 1
        if (Parameter_data(i)%numvals /= Numvalues) then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' number of values in getparam_char_1d does not match declared number of values'
        end if
        if (trim(Parameter_data(i)%data_type) /= 'string') then
          ierr = 1
          print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, &
   &               ' data type does in getparam_char_1d not match declared data type'
        end if
        param_id = i
        exit
      end if
    end do

    if (found == 0) then
      print *, 'ERROR in: ', Modname, ', Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

    Values = Parameter_data(param_id)%values_char_1d
    getparam_string = 0

    end function getparam_string

!***********************************************************************
! setparam - set real or integer parameter values read from Parameter File
!***********************************************************************
  module subroutine setparam(Paramname, Numvalues, Data_type, Num_dims, Dim_string, Values, Ivalues, Cvalues)
    use PRMS_CONSTANTS, ONLY: ERROR_param
    use PRMS_MODULE, only: Ndepl, Nhru, Nsub, Nmonths
    implicit none
    ! Arguments
    integer, intent(IN) :: Numvalues, Data_type, Num_dims, Ivalues(:)
    character(LEN=*), intent(IN) :: Paramname, Dim_string(Num_dims)
    real, intent(IN) :: Values(:)
    character(LEN=16), intent(IN) :: Cvalues(:)
    ! Functions
    intrinsic :: TRIM, INDEX
    ! Local Variables
    integer :: found, i, ii, j, jj, k, ierr
    integer :: nmonths_flag, nsub_flag, nhru_flag
!***********************************************************************
    ierr = 0
    found = 0
    do i = 1, Num_parameters
      if (Paramname == trim(Parameter_data(i)%param_name)) then
        found = i
        if (Parameter_data(i)%data_flag /= Data_type) then
          ierr = 1
          print *, 'ERROR, Parameter: ', Paramname, ' data type does not match declared data type'
        end if

        nmonths_flag = 0
        nsub_flag = 0
        nhru_flag = 0
        if (Num_dims == 1) then
          if (trim(Dim_string(1)) == 'nmonths') nmonths_flag = 1
          if (trim(Dim_string(1)) == 'nsub') nsub_flag = 1
          if (trim(Dim_string(1)) == 'nhru') nhru_flag = 1
        endif

        if (Parameter_data(found)%numvals == Numvalues) then
          if (Data_type == 2) then
            if (Parameter_data(found)%scalar_flag == 1) then
              Parameter_data(found)%values_real_0d = Values(1)
            elseif (Parameter_data(found)%num_dimens == 1) then
              if (trim(Parameter_data(found)%param_name) == 'snarea_curve') then
                k = 0
                do ii = 1, Ndepl
                  do j = 1, 11
                    k = k + 1
                    Parameter_data(found)%values_real_2d(j,ii) = Values(k)
                  enddo
                enddo
!                Parameter_data(found)%values_real_2d = reshape(Values, (/11, Ndepl/))
              else
                !do j = 1, Numvalues
                !  Parameter_data(found)%values_real_1d(j) = Values(j)
                !end do
                Parameter_data(found)%values_real_1d = Values
              end if
            else ! 2d
              k = 0
              do j = 1, Parameter_data(found)%num_dim2
                do jj = 1, Parameter_data(found)%num_dim1
                  k = k + 1
                  Parameter_data(found)%values_real_2d(jj, j) = Values(k)
                end do
              end do
            end if
          elseif (Data_type == 1) then
            if (Parameter_data(found)%scalar_flag == 1) then
              Parameter_data(found)%values_int_0d = Ivalues(1)
            elseif (Parameter_data(found)%num_dimens == 1) then
              do j = 1, Numvalues
                Parameter_data(found)%values_int_1d(j) = Ivalues(j)
              end do
            else ! 2d
              k = 0
              do jj = 1, Parameter_data(found)%num_dim2
                do j = 1, Parameter_data(found)%num_dim1
                  k = k + 1
                  Parameter_data(found)%values_int_2d(j, jj) = Ivalues(k)
                end do
              end do
            end if
          else  ! Data_type == 4 
            if (Parameter_data(found)%scalar_flag == 1) then
              Parameter_data(found)%values_char_0d = Cvalues(1)
            elseif (Parameter_data(found)%num_dimens == 1) then
              do j = 1, Numvalues
                Parameter_data(found)%values_char_1d(j) = Cvalues(j)
              end do
            else ! 2d
              PRINT *, 'ERROR, character parameters cannot be two dimensional'
              ierr = 1
              !k = 0
              !do jj = 1, Parameter_data(found)%num_dim2
              !  do j = 1, Parameter_data(found)%num_dim1
              !    k = k + 1
              !    Parameter_data(found)%values_char_2d(j, jj) = Cvalues(k)
              !  end do
              !end do
            end if
          end if
        else ! check for flexible dimension
          if (Numvalues == 1) then ! set all values to single value
            if (Data_type == 2) then
              if (Parameter_data(found)%num_dimens == 1) then
                !do j = 1, Parameter_data(found)%num_dim1
                !  Parameter_data(found)%values_real_1d(j) = Values(1)
                !end do
                Parameter_data(found)%values_real_1d = Values(1)
              else ! 2d
                !do j = 1, Parameter_data(found)%num_dim2
                !  do jj = 1, Parameter_data(found)%num_dim1
                !    Parameter_data(found)%values_real_2d(jj, j) = Values(1)
                !  end do
                !end do
                Parameter_data(found)%values_real_2d = Values(1)
              end if
            elseif (Data_type == 1) then ! data_type 1
              if (Parameter_data(found)%num_dimens == 1) then
                !do j = 1, Parameter_data(found)%num_dim1
                !  Parameter_data(found)%values_int_1d(j) = Ivalues(1)
                !end do
                Parameter_data(found)%values_int_1d = Ivalues(1)
              else ! 2d
                !do j = 1, Parameter_data(found)%num_dim1
                !  do jj = 1, Parameter_data(found)%num_dim2
                !    Parameter_data(found)%values_int_2d(j, jj) = Ivalues(1)
                !  end do
                !end do
                Parameter_data(found)%values_int_2d = Ivalues(1)
              end if
            elseif (Data_type == 4) then ! data_type 4
              if (Parameter_data(found)%num_dimens == 1) then
                !do j = 1, Parameter_data(found)%num_dim1
                !  Parameter_data(found)%values_char_1d(j) = Cvalues(1)
                !end do
                Parameter_data(found)%values_char_1d = Cvalues(1)
              !else ! no 2d character parameters
              !  Parameter_data(found)%values_char_2d = Cvalues(1)
              end if
            end if
          else ! WARNING: not checking for character parameters or double
            ! first dimension is nhru, nssr, or ngw, passing a different dimension
            if (nmonths_flag == 1) then
              do j = 1, Nmonths
                do ii = 1, Parameter_data(found)%num_dim1
                  if (Data_type == 2) then
                    Parameter_data(found)%values_real_2d(ii, j) = Values(j)
                  else
                    Parameter_data(found)%values_int_2d(ii, j) = Ivalues(j)
                  end if
                end do
              end do
            else if (nsub_flag == 1) then
              do j = 1, Nsub
                do ii = 1, Parameter_data(found)%num_dim1
                  if (Data_type == 2) then
                    Parameter_data(found)%values_real_2d(ii, j) = Values(j)
                  else
                    Parameter_data(found)%values_int_2d(ii, j) = Ivalues(j)
                  end if
                end do
              end do
            else if (nhru_flag == 1) then
              do j = 1, Nhru
                do ii = 1, Parameter_data(found)%num_dim2
                  if (Data_type == 2) then
                    Parameter_data(found)%values_real_2d(j, ii) = Values(j)
                  else
                    Parameter_data(found)%values_int_2d(j, ii) = Ivalues(j)
                  end if
                end do
              end do
            else
              print *, 'ERROR, Parameter: ', Paramname, ' number of values in getparam does not match declared number of values'
              print *, '       number of parameter values expected:', Parameter_data(found)%numvals
              print *, '       number of parameter values specified:', Numvalues
              ierr = 1
            end if
          end if
        end if
      end if
    end do

    if (found == 0) then
      print *, 'ERROR, Parameter: ', Paramname, ' not declared'
      ierr = 1
    end if
    if (ierr == 1) ERROR stop ERROR_param

  end subroutine setparam

end submodule

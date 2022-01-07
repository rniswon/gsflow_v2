!***********************************************************************
! Read Control File
!***********************************************************************
module PRMS_CONTROL_FILE
  use PRMS_CONSTANTS, only: MAXCONTROL_LENGTH, MAXFILE_LENGTH

  character(len=*), parameter :: MODDESC = 'Read Control File'
  character(len=*), parameter :: MODNAME = 'read_control_file'
  integer, parameter :: Max_num_control_parameters = 256 ! WARNING, hard coded, DANGER, DANGER
  character(LEN=MAXFILE_LENGTH), save :: Data_file, Var_init_file, Ani_out_file
  character(LEN=MAXFILE_LENGTH), save :: Executable_desc, Executable_model, Var_save_file
  character(LEN=MAXFILE_LENGTH) :: Control_file, Ani_output_file, Control_description
  integer, save :: PlotsON_OFF, Num_control_parameters, Glacier_flag, Stream_temp_shade_flag
  integer, save :: AniOutON_OFF, NaniOutVars, NdispGraphs, DispGraphsBuffSize, Param_file_control_parameter_id
  integer, save :: NumdispVar_names, NumdispVar_elements
  integer, save :: Canopy_transferON_OFF, Soilzone_transferON_OFF, Consumed_transferON_OFF
  integer, save :: Dyn_sro_to_dprst_flag, Dyn_sro_to_imperv_flag
  real, save :: Initial_deltat
  character(LEN=MAXCONTROL_LENGTH), allocatable, save :: param_file_names(:)
  character(LEN=MAXCONTROL_LENGTH), allocatable, save :: dispVar_element(:), dispVar_names(:)
  ! read_flag: 0 = not set, 1 = set from control file, 2 = set to default

  type PRMS_control_parameter
    character(LEN=MAXCONTROL_LENGTH) :: name
    integer :: numvals, read_flag, data_type, index, allocate_flag
    integer, allocatable :: values_int(:)
    real, allocatable :: values_real(:)
    character(LEN=MAXFILE_LENGTH), allocatable :: values_character(:)
  end type PRMS_control_parameter

  type(PRMS_control_parameter), save, allocatable :: Control_parameter_data(:)

  interface
    module subroutine read_control_file()
    end subroutine

    module subroutine setup_cont()
    end subroutine

    module subroutine get_control_filename()
    end subroutine

    module subroutine get_control_arguments()
    end subroutine

    module subroutine set_control_parameter(Paramname, Numvalues, Paramval_int, Paramval_real, Paramval_char) ! allow arrays
      character(LEN=MAXCONTROL_LENGTH), intent(IN) :: Paramname
      integer, intent(IN) :: Numvalues
      integer, intent(IN) :: Paramval_int(Numvalues)
      real, intent(IN) :: Paramval_real(Numvalues)
      character(LEN=MAXFILE_LENGTH), intent(IN) :: Paramval_char(Numvalues)
    end subroutine

    integer module function control_integer(Parmval, Paramname)
      character(LEN=*), intent(IN) :: Paramname
      integer, intent(OUT) :: Parmval
    end function

    integer module function control_integer_array(Parmval, Array_index, Paramname)
      integer, intent(IN) :: Array_index
      character(LEN=*), intent(IN) :: Paramname
      integer, intent(OUT) :: Parmval
    end function

    integer module function control_string(Parmval, Paramname)
      character(LEN=*), intent(IN) :: Paramname
      character(LEN=*), intent(OUT) :: Parmval
    end function

    integer module function control_string_array(Parmval, Paramname, Array_index)
      character(LEN=*), intent(IN) :: Paramname
      character(LEN=*), intent(OUT) :: Parmval
      integer, intent(IN) :: Array_index
    end function
  end interface
end module PRMS_CONTROL_FILE

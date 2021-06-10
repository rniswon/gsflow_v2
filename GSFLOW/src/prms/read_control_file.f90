!***********************************************************************
! Read Control File
!***********************************************************************
      MODULE PRMS_CONTROL_FILE
        USE PRMS_CONSTANTS, ONLY: MAXCONTROL_LENGTH, MAXFILE_LENGTH, INT_TYPE, REAL_TYPE, CHAR_TYPE, ERROR_control
        USE PRMS_MODULE, ONLY: Print_debug, EQULS, statsON_OFF, &
     &      Init_vars_from_file, Save_vars_to_file, Parameter_check_flag, Param_file, Model_output_file, &
     &      Precip_module, Temp_module, Et_module, Srunoff_module, Solrad_module, Gwr_swale_flag, &
     &      Strmflow_module, Transp_module, Soilzone_module, Print_debug, Dprst_flag, Subbasin_flag, Frozen_flag, &
     &      CsvON_OFF, MapOutON_OFF, Model_mode, Orad_flag, Endtime, Starttime, Snow_cbh_flag, Stream_temp_flag, &
     &      Cascade_flag, Cascadegw_flag, Prms_warmup, Humidity_cbh_flag, Windspeed_cbh_flag, Strmtemp_humidity_flag, &
     &      Gwflow_cbh_flag, NhruOutON_OFF, NsubOutON_OFF, BasinOutON_OFF, Dyn_imperv_flag, Dyn_dprst_flag, Dyn_intcp_flag, &
     &      Dyn_covtype_flag, Dyn_potet_flag, Dyn_transp_flag, Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, &
     &      Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Dyn_fallfrost_flag, NsegmentOutON_OFF, &
     &      Dyn_springfrost_flag, Dyn_snareathresh_flag, Dyn_covden_flag, Segment_transferON_OFF, Gwr_transferON_OFF, &
     &      Lake_transferON_OFF, External_transferON_OFF, Dprst_transferON_OFF, BasinOutON_OFF, &
     &      Snarea_curve_flag, Soilzone_aet_flag, Gsflow_output_file, &
     &      Csv_output_file, selectDatesFileName, outputSelectDatesON_OFF, Gsf_rpt, Rpt_days, &
     &      Dprst_transfer_water_use, Dprst_add_water_use, PRMS_land_iteration_flag, &
     &      Agriculture_soil_flag, Agriculture_canopy_flag, Dyn_ag_frac_flag, &
     &      mappingFileName, xyFileName, PET_ag_module, AET_module
        USE PRMS_CLIMATE_HRU, ONLY: AET_cbh_file, PET_cbh_file
        USE GSFMODFLOW, ONLY: Modflow_name, Modflow_time_zero
        USE PRMS_CLIMATE_HRU, ONLY: Precip_day, Tmax_day, Tmin_day, Potet_day, Transp_day, Swrad_day, &
     &      Cbh_check_flag, Cbh_binary_flag, Windspeed_day, Humidity_day
        USE PRMS_MAP_RESULTS, ONLY: NmapOutVars, MapOutVar_names
        USE PRMS_NHRU_SUMMARY, ONLY: NhruOutVars, NhruOut_freq, NhruOutBaseFileName, NhruOutVar_names, NhruOut_format, NhruOutNcol
        USE PRMS_NSUB_SUMMARY, ONLY: NsubOutVars, NsubOut_freq, NsubOutBaseFileName, NsubOutVar_names, NsubOut_format
        USE PRMS_BASIN_SUMMARY, ONLY: BasinOutVars, BasinOut_freq, BasinOutBaseFileName, BasinOutVar_names
        USE PRMS_NSEGMENT_SUMMARY, ONLY: NsegmentOutVars, NsegmentOut_freq, NsegmentOutBaseFileName, &
     &      NsegmentOutVar_names, NsegmentOut_format
        USE PRMS_DYNAMIC_PARAM_READ, ONLY: imperv_frac_dynamic, imperv_stor_dynamic, dprst_depth_dynamic, dprst_frac_dynamic, &
     &      wrain_intcp_dynamic, srain_intcp_dynamic, snow_intcp_dynamic, covtype_dynamic, &
     &      potetcoef_dynamic, transpbeg_dynamic, transpend_dynamic, Ag_frac_dynamic, &
     &      soilmoist_dynamic, soilrechr_dynamic, radtrncf_dynamic, &
     &      fallfrost_dynamic, springfrost_dynamic, transp_on_dynamic, snareathresh_dynamic, &
     &      covden_sum_dynamic, covden_win_dynamic, sro2dprst_perv_dyn, sro2dprst_imperv_dyn, Dynamic_param_log_file
        USE PRMS_GLACR, ONLY: Mbinit_flag
        USE PRMS_PRECIP_MAP, ONLY: Precip_map_file
        USE PRMS_TEMP_MAP, ONLY: Tmax_map_file, Tmin_map_file
        character(len=*), parameter :: MODDESC = 'Read Control File'
        character(len=*), parameter :: MODNAME = 'read_control_file'
        INTEGER, PARAMETER :: Max_num_control_parameters = 2020 ! WARNING, hard coded, DANGER, DANGER
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Data_file, Var_init_file, Stat_var_file, Ani_out_file
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Executable_desc, Executable_model, Var_save_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: Control_file, Ani_output_file, Control_description
        INTEGER, SAVE :: PlotsON_OFF, Num_control_parameters, Glacier_flag, Stream_temp_shade_flag
        INTEGER, SAVE :: NstatVars, AniOutON_OFF, NaniOutVars, NdispGraphs, DispGraphsBuffSize, Param_file_control_parameter_id
        INTEGER, SAVE :: Num_statvar_elements, Num_statvar_names, NumdispVar_names, NumdispVar_elements
        INTEGER, SAVE :: Canopy_transferON_OFF, Soilzone_transferON_OFF, Consumed_transferON_OFF
        INTEGER, SAVE :: Dyn_sro_to_dprst_flag, Dyn_sro_to_imperv_flag
        REAL, SAVE :: Initial_deltat
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: statVar_element(:), statVar_names(:), param_file_names(:)
        CHARACTER(LEN=MAXCONTROL_LENGTH), ALLOCATABLE, SAVE :: dispVar_element(:), dispVar_names(:)
        ! read_flag: 0 = not set, 1 = set from control file, 2 = set to default
        TYPE PRMS_control_parameter
             CHARACTER(LEN=MAXCONTROL_LENGTH) :: name
             INTEGER :: numvals, read_flag, data_type, index, allocate_flag
             INTEGER, ALLOCATABLE :: values_int(:)
             REAL, ALLOCATABLE :: values_real(:)
             CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: values_character(:)
        END TYPE PRMS_control_parameter
        TYPE ( PRMS_control_parameter ), SAVE, ALLOCATABLE :: Control_parameter_data(:)
      END MODULE PRMS_CONTROL_FILE

      SUBROUTINE read_control_file()
      USE PRMS_CONTROL_FILE
      USE PRMS_MODULE, ONLY: Print_debug, Model_output_file, Model_control_file
      IMPLICIT NONE
      ! Functions
      INTRINSIC TRIM
      INTEGER, EXTERNAL :: numchars
      EXTERNAL read_error, set_control_parameter, PRMS_open_input_file, write_outfile, PRMS_open_output_file !, print_module
      ! Local Variables
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramname
      CHARACTER(LEN=4) :: string
      INTEGER nchars, ios, numvalues, param_type, control_unit, j
      INTEGER, ALLOCATABLE :: int_parameter_values(:)
      CHARACTER(LEN=MAXFILE_LENGTH), ALLOCATABLE :: parameter_values(:)
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: paramstring
      REAL, ALLOCATABLE :: real_parameter_values(:)
!***********************************************************************

      ! control filename cannot include blanks
      CALL get_control_filename(Model_control_file, nchars)
      CALL PRMS_open_input_file(control_unit, Model_control_file, 'model_control_file', 0, ios)
      IF ( ios/=0 ) CALL read_error(10, TRIM(Model_control_file))
      ! read header
      READ (control_unit, '(A)', IOSTAT=ios ) Control_description
      IF ( ios/=0 ) CALL read_error(12, TRIM(Model_control_file))

      CALL setup_cont() ! set default control parameter values

      ! Read all Control Parameters
      DO
        READ ( control_unit, '(A)', IOSTAT=ios ) string
        IF ( ios==-1 ) EXIT ! found end of Control File
        IF ( ios/=0 ) CALL read_error(12, 'missing #### delimiter')
        IF ( string(:4)/='####' ) CYCLE ! skip until delimiter found, such as blank of // comment lines
        READ ( control_unit, '(A)', IOSTAT=ios ) paramname ! parameter name
        IF ( ios/=0 ) CALL read_error(5, 'missing parameter name')
        READ ( control_unit, *, IOSTAT=ios ) numvalues
        IF ( ios/=0 ) CALL read_error(5, 'invalid number of values: '//TRIM(paramname) )
        READ ( control_unit, *, IOSTAT=ios ) param_type
        IF ( ios/=0 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        IF ( param_type<1 .OR. param_type>4 .OR. param_type==3 ) CALL read_error(5, 'invalid parameter type: '//TRIM(paramstring) )
        ALLOCATE ( int_parameter_values(numvalues), real_parameter_values(numvalues), parameter_values(numvalues) )
        IF ( param_type==1 ) THEN
          READ ( Control_unit, *, IOSTAT=ios ) (int_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid integer value: '//TRIM(paramname) )
        ELSEIF ( param_type==4 ) THEN
          DO j = 1, numvalues
            READ ( Control_unit, '(A)', IOSTAT=ios ) parameter_values(j)
            IF ( ios/=0 ) CALL read_error(5, 'invalid character value: '//TRIM(paramname) )
          ENDDO
        ELSE
          READ ( Control_unit, *, IOSTAT=ios ) (real_parameter_values(j),j=1,numvalues)
          IF ( ios/=0 ) CALL read_error(5, 'invalid real value: '//TRIM(paramname) )
        ENDIF
        CALL set_control_parameter(paramname, numvalues, int_parameter_values, real_parameter_values, parameter_values)
        DEALLOCATE ( int_parameter_values, real_parameter_values, parameter_values )
      ENDDO
      ! reset control parameters based on command line
      CLOSE ( control_unit )

      END SUBROUTINE read_control_file

!***********************************************************************
! setup_cont - Set control parameter value defaults
!***********************************************************************
      SUBROUTINE setup_cont()
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Local Variables
      INTEGER i, numvalues
!***********************************************************************
      Num_control_parameters = Max_num_control_parameters
      ! allocate and store parameter data
      ALLOCATE ( Control_parameter_data(Num_control_parameters) )
      DO i = 1, Num_control_parameters
        Control_parameter_data(i)%read_flag = 2 ! 2 = set to default; 1 = read from Control File
        Control_parameter_data(i)%data_type = INT_TYPE ! 1 = integer, 2 = real, 4 = string
        Control_parameter_data(i)%allocate_flag = 0 ! set to 1 if allocatable
        Control_parameter_data(i)%numvals = 1
        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
        Control_parameter_data(i)%index = i
        ALLOCATE ( Control_parameter_data(i)%values_int(1) )
        ALLOCATE ( Control_parameter_data(i)%values_real(1) )
        ALLOCATE ( Control_parameter_data(i)%values_character(1) )
        Control_parameter_data(i)%values_int(1) = 0
        Control_parameter_data(i)%values_real(1) = 0.0
        Control_parameter_data(i)%values_character(1) = ' '
      ENDDO

!      DO i = Num_control_parameters+1, Num_control_parameters+20
!        Control_parameter_data(i)%read_flag = 0 ! 0 means not set
!        Control_parameter_data(i)%data_type = 0 ! 1 = integer, 2 = real, 4 = string
!        Control_parameter_data(i)%numvals = 0
!        Control_parameter_data(i)%name = ' '
        ! WARNING, parameter index is set based on order defaults defined
!        Control_parameter_data(i)%index = i
!      ENDDO

      ! assign default value for integer flags
      ! note: default value for all parameters set to 0, only need to reset if other than 0
      numvalues = 1
      i = 1
      Control_parameter_data(i)%name = 'print_debug'
      Print_debug = 0
      i = i + 1
      Control_parameter_data(i)%name = 'parameter_check_flag'
      Parameter_check_flag = 1
      Control_parameter_data(i)%values_int(1) = Parameter_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_flag'
      Dprst_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'cascade_flag'
      Cascade_flag = 1
      Control_parameter_data(i)%values_int(1) = Cascade_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cascadegw_flag'
      Cascadegw_flag = 1
      Control_parameter_data(i)%values_int(1) = Cascadegw_flag
      i = i + 1
      Control_parameter_data(i)%name = 'save_vars_to_file'
      Save_vars_to_file = 0
      i = i + 1
      Control_parameter_data(i)%name = 'init_vars_from_file'
      Init_vars_from_file = 0
      i = i + 1
      Control_parameter_data(i)%name = 'frozen_flag'
      Frozen_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'glacier_flag'
      Glacier_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'stream_temp_flag'
      Stream_temp_flag = 0
	  i = i + 1
      Control_parameter_data(i)%name = 'strmtemp_humidity_flag'
      Strmtemp_humidity_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'stream_temp_shade_flag'
      Stream_temp_shade_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'snarea_curve_flag'
      Snarea_curve_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'soilzone_aet_flag'
      Soilzone_aet_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'orad_flag'
      Orad_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'subbasin_flag'
      Subbasin_flag = 1
      Control_parameter_data(i)%values_int(1) = Subbasin_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_check_flag'
      Cbh_check_flag = 1
      Control_parameter_data(i)%values_int(1) = Cbh_check_flag
      i = i + 1
      Control_parameter_data(i)%name = 'cbh_binary_flag'
      Cbh_binary_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'gwr_swale_flag'
      Gwr_swale_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'snow_cbh_flag'
      Snow_cbh_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'gwflow_cbh_flag'
       Gwflow_cbh_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'humidity_cbh_flag'
      Humidity_cbh_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'windspeed_cbh_flag'
      Windspeed_cbh_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_transfer_water_use'
      Dprst_transfer_water_use = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_add_water_use'
      Dprst_add_water_use = 0
      i = i + 1
      Control_parameter_data(i)%name = 'PRMS_land_iteration_flag'
      PRMS_land_iteration_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'agriculture_soil_flag'
      Agriculture_soil_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'agriculture_canopy_flag'
      Agriculture_canopy_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_ag_frac_flag'
      Dyn_ag_frac_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'mbinit_flag'
      Mbinit_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'outputSelectDatesON_OFF'
      outputSelectDatesON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutON_OFF'
      NhruOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOut_freq'
      NhruOut_freq = 1
      Control_parameter_data(i)%values_int(1) = NhruOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutNcol'
	  NhruOutNcol = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOut_format'
      NhruOut_format = 1
      Control_parameter_data(i)%values_int(1) = NhruOut_format
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVars'
      NhruOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutON_OFF'
      NsubOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutVars'
      NsubOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutON_OFF'
      BasinOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutVars'
      BasinOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOutON_OFF'
      NsegmentOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOutVars'
      NsegmentOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'statsON_OFF'
      StatsON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'csvON_OFF'
      CsvON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'aniOutON_OFF'
      AniOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'mapOutON_OFF'
      MapOutON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nstatVars'
      NstatVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nmapOutVars'
      NmapOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'naniOutVars'
      NaniOutVars = 0
      i = i + 1
      Control_parameter_data(i)%name = 'ndispGraphs'
      NdispGraphs = 0
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOut_freq'
      NsubOut_freq = 1
      Control_parameter_data(i)%values_int(1) = NsubOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOut_format'
      NsubOut_format = 1
      Control_parameter_data(i)%values_int(1) = NsubOut_format
      i = i + 1
      Control_parameter_data(i)%name = 'basinOut_freq'
      BasinOut_freq = 1
      Control_parameter_data(i)%values_int(1) = BasinOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOut_freq'
      NsegmentOut_freq = 1
      Control_parameter_data(i)%values_int(1) = NsegmentOut_freq
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOut_format'
      NsegmentOut_format = 1
      Control_parameter_data(i)%values_int(1) = NsegmentOut_format
      i = i + 1
      Control_parameter_data(i)%name = 'prms_warmup'
      Prms_warmup = 0
      Control_parameter_data(i)%values_int(1) = Prms_warmup
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_imperv_flag'
      Dyn_imperv_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_intcp_flag'
      Dyn_intcp_flag = 0
       i = i + 1
      Control_parameter_data(i)%name = 'dyn_covden_flag'
      Dyn_covden_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dispGraphsBuffSize'
      DispGraphsBuffSize = 50
      Control_parameter_data(i)%values_int(1) = DispGraphsBuffSize
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_transp_flag'
      Dyn_transp_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_transp_on_flag'
      Dyn_transp_on_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro2dprst_perv_flag'
      Dyn_sro2dprst_perv_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro2dprst_imperv_flag'
      Dyn_sro2dprst_imperv_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_covtype_flag'
      Dyn_covtype_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_fallfrost_flag'
      Dyn_fallfrost_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_springfrost_flag'
      Dyn_springfrost_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_potet_flag'
      Dyn_potet_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_soil_flag'
      Dyn_soil_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_radtrncf_flag'
      Dyn_radtrncf_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_snareathresh_flag'
      Dyn_snareathresh_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro_to_dprst_flag'
      Dyn_sro_to_dprst_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_sro_to_imperv_flag'
      Dyn_sro_to_imperv_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dyn_dprst_flag'
      Dyn_dprst_flag = 0
      i = i + 1
      Control_parameter_data(i)%name = 'segment_transferON_OFF'
      Segment_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'gwr_transferON_OFF'
      Gwr_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'external_transferON_OFF'
      External_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'consumed_transferON_OFF'
      Consumed_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'lake_transferON_OFF'
      Lake_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_transferON_OFF'
      Dprst_transferON_OFF = 0
      i = i + 1
      Control_parameter_data(i)%name = 'soilzone_transferON_OFF'
      Soilzone_transferON_OFF = 0
      i = i + 1      
      Control_parameter_data(i)%name = 'canopy_transferON_OFF'
      Canopy_transferON_OFF = 0
      i = i + 1 

      ! parameters that get allocated if in Control File
      Control_parameter_data(i)%name = 'mapOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'statVar_element'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'statVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'aniOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'dispVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'dispVar_plot'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'dispVar_element'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutVar_names'
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      i = i + 1

      Control_parameter_data(i)%name = 'gsf_rpt'
      Gsf_rpt = 1
      Control_parameter_data(i)%values_int(1) = Gsf_rpt
      i = i + 1
      Control_parameter_data(i)%name = 'rpt_days'
      Rpt_days = 7
      Control_parameter_data(i)%values_int(1) = Rpt_days
      i = i + 1

      !!!! add dynamic and water use

! floating point parameters
      Control_parameter_data(i)%name = 'initial_deltat'
      Initial_deltat = 24.0
      Control_parameter_data(i)%values_real(1) = Initial_deltat
      Control_parameter_data(i)%data_type = REAL_TYPE
      i = i + 1

      ! assign default value for character parameters
      Control_parameter_data(i)%name = 'selectDatesFileName'
      selectDatesFileName = 'dates'
      Control_parameter_data(i)%values_character(1) = selectDatesFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'model_mode'
      Model_mode = 'GSFLOW5'
      Control_parameter_data(i)%values_character(1) = Model_mode
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'executable_desc'
      Executable_desc = 'GSFLOW with MODSIM, MFNWT, and PRMS-5'
      Control_parameter_data(i)%values_character(1) = Executable_desc
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'executable_model'
      Executable_model = 'gsflow'
      Control_parameter_data(i)%values_character(1) = Executable_model
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'precip_module'
      Precip_module = 'precip_1sta'
      Control_parameter_data(i)%values_character(1) = Precip_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'temp_module'
      Temp_module = 'temp_1sta'
      Control_parameter_data(i)%values_character(1) = Temp_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'solrad_module'
      Solrad_module = 'ddsolrad'
      Control_parameter_data(i)%values_character(1) = Solrad_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'et_module'
      Et_module = 'potet_jh'
      Control_parameter_data(i)%values_character(1) = Et_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'srunoff_module'
      Srunoff_module = 'srunoff_smidx'
      Control_parameter_data(i)%values_character(1) = Srunoff_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'strmflow_module'
      Strmflow_module = 'strmflow'
      Control_parameter_data(i)%values_character(1) = Strmflow_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'transp_module'
      Transp_module = 'transp_tindex'
      Control_parameter_data(i)%values_character(1) = Transp_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'data_file'
      Data_file = 'prms.data'
      Control_parameter_data(i)%values_character(1) = Data_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'param_file'
      Param_file = 'prms.params'
      Control_parameter_data(i)%values_character(1) = Param_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      Control_parameter_data(i)%allocate_flag = 1 ! need to allocate
      Param_file_control_parameter_id = i
      i = i + 1
      Control_parameter_data(i)%name = 'model_output_file'
      Model_output_file = 'prms.out'
      Control_parameter_data(i)%values_character(1) = Model_output_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'mappingFileName'
      mappingFileName = 'MODSIM.map'
      Control_parameter_data(i)%values_character(1) = mappingFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'xyFileName'
      xyFileName = 'MODSIM.xy'
      Control_parameter_data(i)%values_character(1) = xyFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'csv_output_file'
      Csv_output_file = 'prms_summary.csv'
      Control_parameter_data(i)%values_character(1) = Csv_output_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'var_save_file'
      Var_save_file = 'prms_ic.out'
      Control_parameter_data(i)%values_character(1) = Var_save_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'var_init_file'
      Var_init_file = 'prms_ic.in'
      Control_parameter_data(i)%values_character(1) = Var_init_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'stat_var_file'
      Stat_var_file = 'statvar.out'
      Control_parameter_data(i)%values_character(1) = Stat_var_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'ani_output_file'
      Ani_output_file = 'animation.out'
      Control_parameter_data(i)%values_character(1) = Ani_output_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'nhruOutBaseFileName'
      NhruOutBaseFileName = 'nhruout_path'
      Control_parameter_data(i)%values_character(1) = NhruOutBaseFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'nsubOutBaseFileName'
      NsubOutBaseFileName = 'nsubout_path'
      Control_parameter_data(i)%values_character(1) = NsubOutBaseFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'basinOutBaseFileName'
      BasinOutBaseFileName = 'basinout_path'
      Control_parameter_data(i)%values_character(1) = BasinOutBaseFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'nsegmentOutBaseFileName'
      NsubOutBaseFileName = 'nsegmentout_path'
      Control_parameter_data(i)%values_character(1) = NsegmentOutBaseFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'tmax_day'
      Tmax_day = 'tmax_day'
      Control_parameter_data(i)%values_character(1) = Tmax_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'tmin_day'
      Tmin_day = 'tmin_day'
      Control_parameter_data(i)%values_character(1) = Tmin_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'precip_day'
      Precip_day = 'precip_day'
      Control_parameter_data(i)%values_character(1) = Precip_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'swrad_day'
      Swrad_day = 'swrad_day'
      Control_parameter_data(i)%values_character(1) = Swrad_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'potet_day'
      Potet_day = 'potet_day'
      Control_parameter_data(i)%values_character(1) = Potet_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'transp_day'
      Transp_day = 'transp_day'
      Control_parameter_data(i)%values_character(1) = Transp_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'windspeed_day'
      Windspeed_day = 'windspeed_day'
      Control_parameter_data(i)%values_character(1) = Windspeed_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'humidity_day'
      Humidity_day = 'humidity_day'
      Control_parameter_data(i)%values_character(1) = Humidity_day
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'dynamic_param_log_file'
      Dynamic_param_log_file = 'dynamic_parameter.out'
      Control_parameter_data(i)%values_character(1) = Dynamic_param_log_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      ! GSFLOW parameters
      Control_parameter_data(i)%name = 'modflow_name'
      Modflow_name = 'modflow.nam'
      Control_parameter_data(i)%values_character(1) = Modflow_name
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'gsflow_output_file'
      Gsflow_output_file = 'gsflow.out'
      Control_parameter_data(i)%values_character(1) = Gsflow_output_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_depth_dynamic'
      Dprst_depth_dynamic = 'dyndprst_depth'
      Control_parameter_data(i)%values_character(1) = Dprst_depth_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'dprst_frac_dynamic'
      Dprst_frac_dynamic = 'dyndprst_frac'
      Control_parameter_data(i)%values_character(1) = Dprst_frac_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'snow_intcp_dynamic'
      Snow_intcp_dynamic = 'dynsnowintcp'
      Control_parameter_data(i)%values_character(1) = Snow_intcp_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'srain_intcp_dynamic'
      Srain_intcp_dynamic = 'dynsrainintcp'
      Control_parameter_data(i)%values_character(1) = Srain_intcp_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'wrain_intcp_dynamic'
      Wrain_intcp_dynamic = 'dynwrainintcp'
      Control_parameter_data(i)%values_character(1) = Wrain_intcp_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'imperv_frac_dynamic'
      Imperv_frac_dynamic = 'dynimperv_frac'
      Control_parameter_data(i)%values_character(1) = Imperv_frac_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'imperv_stor_dynamic'
      Imperv_stor_dynamic = 'dynimperv_stor'
      Control_parameter_data(i)%values_character(1) = Imperv_stor_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'covtype_dynamic'
      Covtype_dynamic = 'dyncovtype'
      Control_parameter_data(i)%values_character(1) = Covtype_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'covden_sum_dynamic'
      Covden_sum_dynamic = 'dyncovden_sum'
      Control_parameter_data(i)%values_character(1) = Covden_sum_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'covden_win_dynamic'
      Covden_win_dynamic = 'dyncovden_win'
      Control_parameter_data(i)%values_character(1) = Covden_win_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'potetcoef_dynamic'
      Potetcoef_dynamic = 'dynpotetcoef'
      Control_parameter_data(i)%values_character(1) = Potetcoef_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'transpbeg_dynamic'
      Transpbeg_dynamic = 'dyntranspbeg'
      Control_parameter_data(i)%values_character(1) = Transpbeg_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'transpend_dynamic'
      Transpend_dynamic = 'dyntranspend'
      Control_parameter_data(i)%values_character(1) = Transpend_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'fallfrost_dynamic'
      Fallfrost_dynamic = 'dynfallfrost'
      Control_parameter_data(i)%values_character(1) = Fallfrost_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'springfrost_dynamic'
      Springfrost_dynamic = 'dynspringfrost'
      Control_parameter_data(i)%values_character(1) = Springfrost_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'soilrechr_dynamic'
      Soilrechr_dynamic = 'dynsoilrechr'
      Control_parameter_data(i)%values_character(1) = Soilrechr_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'soilmoist_dynamic'
      Soilmoist_dynamic = 'dynsoilmoist'
      Control_parameter_data(i)%values_character(1) = Soilmoist_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'radtrncf_dynamic'
      Radtrncf_dynamic = 'dynradtrncf'
      Control_parameter_data(i)%values_character(1) = Radtrncf_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'sro2dprst_perv_dynamic'
      Sro2dprst_perv_dyn = 'dynsro2dprst_perv'
      Control_parameter_data(i)%values_character(1) = Sro2dprst_perv_dyn
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'sro2dprst_imperv_dynamic'
      Sro2dprst_imperv_dyn = 'dynsro2dprst_imperv'
      Control_parameter_data(i)%values_character(1) = Sro2dprst_imperv_dyn
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'transp_on_dynamic'
      Transp_on_dynamic = 'dyntranspon'
      Control_parameter_data(i)%values_character(1) = Transp_on_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'PET_ag_module'
      PET_ag_module = 'none'
      Control_parameter_data(i)%values_character(1) = PET_ag_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'AET_module'
      AET_module = 'none'
      Control_parameter_data(i)%values_character(1) = AET_module
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'PET_cbh_file'
      PET_cbh_file = 'PET_external.cbh'
      Control_parameter_data(i)%values_character(1) = PET_cbh_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'AET_cbh_file'
      AET_cbh_file = 'AET_external.cbh'
      Control_parameter_data(i)%values_character(1) = AET_cbh_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'ag_frac_dynamic'
      Ag_frac_dynamic = 'ag_frac.dyn'
      Control_parameter_data(i)%values_character(1) = Ag_frac_dynamic
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'dynamic_param_log_file'
      Dynamic_param_log_file = 'dynamic_param.log'
      Control_parameter_data(i)%values_character(1) = Dynamic_param_log_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'selectDatesFileName'
      SelectDatesFileName = 'print_dates.dat'
      Control_parameter_data(i)%values_character(1) = SelectDatesFileName
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'precip_map_file'
      Precip_map_file = 'precip.map'
      Control_parameter_data(i)%values_character(1) = Precip_map_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'tmax_map_file'
      Tmax_map_file = 'tmax.map'
      Control_parameter_data(i)%values_character(1) = Tmax_map_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
      Control_parameter_data(i)%name = 'tmin_map_file'
      Tmin_map_file = 'tmin.map'
      Control_parameter_data(i)%values_character(1) = Tmin_map_file
      Control_parameter_data(i)%data_type = CHAR_TYPE
      i = i + 1
!      Control_parameter_data(i)%name = 'pkwater_equiv_day'
!      Pkwater_equiv_day = 'pkwater_equiv.day'
!      Control_parameter_data(i)%values_character(1) = Pkwater_equiv_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
!      Control_parameter_data(i)%name = 'pk_depth_day'
!      Pk_depth_day = 'pk_depth.day'
!      Control_parameter_data(i)%values_character(1) = Pk_depth_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
!      Control_parameter_data(i)%name = 'snow_evap_day'
!      Snow_evap_day = 'snow_evap.day'
!      Control_parameter_data(i)%values_character(1) = Snow_evap_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
!      Control_parameter_data(i)%name = 'snowcov_area_day'
!      Snowcov_area_day = 'snowcov_area.day'
!      Control_parameter_data(i)%values_character(1) = Snowcov_area_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
!      Control_parameter_data(i)%name = 'snowmelt_day'
!      Snowmelt_day = 'snowmelt.day'
!      Control_parameter_data(i)%values_character(1) = Snowmelt_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
!      Control_parameter_data(i)%name = 'gwres_flow_day'
!      Gwres_flow_day = 'gwres_flow.day'
!      Control_parameter_data(i)%values_character(1) = Gwres_flow_day
!      Control_parameter_data(i)%data_type = CHAR_TYPE
!      i = i + 1
          
      ! time arrays
      Control_parameter_data(i)%name = 'start_time'
      DEALLOCATE ( Control_parameter_data(i)%values_int )
      ALLOCATE ( Control_parameter_data(i)%values_int(6) )
      Starttime(1) = 2000
      Starttime(2) = 10
      Starttime(3) = 1
      Starttime(4) = 0
      Starttime(5) = 0
      Starttime(6) = 0
      Control_parameter_data(i)%values_int = Starttime
      Control_parameter_data(i)%numvals = 6
      i = i + 1
      Control_parameter_data(i)%name = 'end_time'
      DEALLOCATE ( Control_parameter_data(i)%values_int )
      ALLOCATE ( Control_parameter_data(i)%values_int(6) )
      Endtime(1) = 2001
      Endtime(2) = 9
      Endtime(3) = 30
      Endtime(4) = 0
      Endtime(5) = 0
      Endtime(6) = 0
      Control_parameter_data(i)%values_int = Endtime
      Control_parameter_data(i)%numvals = 6
      i = i + 1

      ! GSFLOW parameters
      Control_parameter_data(i)%name = 'modflow_time_zero'
      DEALLOCATE ( Control_parameter_data(i)%values_int )
      ALLOCATE ( Control_parameter_data(i)%values_int(6) )
      Control_parameter_data(i)%values_int(1) = -999 
      Modflow_time_zero(1) = -999 ! set to negative, so default can be set to start_time in code
      Modflow_time_zero(2) = 10
      Modflow_time_zero(3) = 1
      Modflow_time_zero(4) = 0
      Modflow_time_zero(5) = 0
      Modflow_time_zero(6) = 0
      Control_parameter_data(i)%values_int = Modflow_time_zero
      Control_parameter_data(i)%numvals = 6

      Num_control_parameters = i
      
      END SUBROUTINE setup_cont

!***********************************************************************
! Get Control File from command line or user interaction.
!***********************************************************************
      SUBROUTINE get_control_filename()
      USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH, ERROR_control
      USE PRMS_MODULE, ONLY: Print_debug, EQULS, Model_control_file
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, GET_COMMAND, TRIM
      EXTERNAL :: error_stop
      ! Local Variables
      CHARACTER(LEN=MAXFILE_LENGTH) command_line_arg, command_line
      LOGICAL :: exists
      INTEGER :: status, nchars, numargs
!***********************************************************************
! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
! This routine expects the Control File name to be the first argument, if present
      CALL GET_COMMAND(command_line)
!      print *, 'Command line: ', TRIM(command_line)
      numargs = COMMAND_ARGUMENT_COUNT()
      IF ( Print_debug>-1 ) PRINT '(/,A)', EQULS
      CALL GET_COMMAND_ARGUMENT(1, command_line_arg, nchars, status)
      IF ( status/=0 ) THEN
        WRITE ( *,'(/,A)' ) 'Enter the name of the PRMS Control File or quit:'
        READ ( *, '(A)' ) Model_control_file
        IF ( Model_control_file(:4)=='quit' .OR. Model_control_file(:4)=='QUIT' ) ERROR STOP ERROR_control
      ELSE
        IF ( TRIM(command_line_arg)=='-C' ) THEN
          CALL GET_COMMAND_ARGUMENT(2, Model_control_file, nchars, status)
          IF ( status/=0 ) CALL error_stop('bad argment value after -C argument', ERROR_control)
        ELSE
          Model_control_file = TRIM(command_line_arg)
        ENDIF
      ENDIF

      INQUIRE ( FILE=TRIM(Model_control_file), EXIST=exists )
      IF ( .NOT.exists ) THEN
        WRITE ( *,'(/,A)' ) 'Control File does not exist, file name: '//TRIM(Model_control_file)
        PRINT *, 'Note: Control File names cannot include spaces'
        ERROR STOP -2
      ENDIF

      END SUBROUTINE get_control_filename

!***********************************************************************
! Get Control File set arguments from command line.
!***********************************************************************
      SUBROUTINE get_control_arguments()
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, MAXFILE_LENGTH, ERROR_control
      USE PRMS_CONTROL_FILE, ONLY: PlotsON_OFF, Num_control_parameters, Control_parameter_data
      USE PRMS_MODULE, ONLY: Print_debug, EQULS
      IMPLICIT NONE
      ! Functions
      INTRINSIC :: GET_COMMAND_ARGUMENT, COMMAND_ARGUMENT_COUNT, GET_COMMAND, TRIM
      EXTERNAL :: error_stop
      ! Local Variables
      CHARACTER(LEN=MAXFILE_LENGTH) command_line_arg, command_line
      INTEGER :: status, i, j, nchars, numargs, index, param_type, num_param_values
!***********************************************************************
! Subroutine GET_COMMAND_ARGUMENT may not be available with all compilers-it is a Fortran 2003 routine
! This routine expects the Control File name to be the first argument, if present
      CALL GET_COMMAND(command_line)
      numargs = COMMAND_ARGUMENT_COUNT()
      PlotsON_OFF = 0
      i = 0
      DO WHILE ( i < numargs )
        i = i + 1
        CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
        IF ( status/=0 ) CALL error_stop('setting control parameters from command line', ERROR_control)
        IF ( TRIM(command_line_arg)=='-C' ) THEN
          i = i + 2
          CYCLE
        ELSE
          IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
          IF ( i==1 ) CYCLE
          IF ( TRIM(command_line_arg)=='-rtg' ) THEN
            PlotsON_OFF = 1
          ELSEIF ( TRIM(command_line_arg)=='-set' ) THEN
            ! find control file parameter and reset it, need type and number of values
            i = i + 1
            CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
            IF ( status/=0 ) CALL error_stop('bad argment value after -set argument', ERROR_control)
            IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
            index = 0
            DO j = 1, Num_control_parameters
              IF ( TRIM(command_line_arg)==TRIM(Control_parameter_data(j)%name) ) THEN
                param_type = Control_parameter_data(j)%data_type
                num_param_values = Control_parameter_data(j)%numvals
                index = j
                EXIT
              ENDIF
            ENDDO
            IF ( index==0 ) CALL error_stop('control parameter argument not found', ERROR_control)
            DO j = 1, num_param_values
              i = i + 1
              CALL GET_COMMAND_ARGUMENT(i, command_line_arg, nchars, status)
              IF ( status/=0 ) CALL error_stop('bad value after -set argument', ERROR_control)
              IF ( Print_debug>-1 ) PRINT *, 'PRMS command line argument,', i, ': ', TRIM(command_line_arg)
              IF ( param_type==1 ) THEN
                READ ( command_line_arg, *, IOSTAT=status ) Control_parameter_data(index)%values_int(j)
                IF ( status/=0 ) CALL error_stop('reading integer command line argument', ERROR_control)
              ELSEIF ( param_type==4 ) THEN
                Control_parameter_data(index)%values_character(j) = command_line_arg
              ELSEIF ( param_type==2 ) THEN
                READ ( command_line_arg, * ) Control_parameter_data(index)%values_real(j)
              ELSE
                CALL error_stop('control parameter type not implemented', ERROR_control)
              ENDIF
            ENDDO
          ELSE
            CALL error_stop('command line argument invalid', ERROR_control)
          ENDIF
        ENDIF
      ENDDO

      IF ( Print_debug>DEBUG_less ) PRINT '(A)', EQULS
      END SUBROUTINE get_control_arguments

!***********************************************************************
! check control parameter if in Control File
!***********************************************************************
      SUBROUTINE set_control_parameter(Paramname, Numvalues, Paramval_int, Paramval_real, Paramval_char) ! allow arrays
      USE PRMS_CONSTANTS, ONLY: ERROR_control
      USE PRMS_CONTROL_FILE
      IMPLICIT NONE
      ! Arguments
      CHARACTER(LEN=MAXCONTROL_LENGTH), INTENT(IN) :: Paramname
      INTEGER, INTENT(IN) :: Numvalues
      INTEGER, INTENT(IN) :: Paramval_int(Numvalues)
      REAL, INTENT(IN) :: Paramval_real(Numvalues)
      CHARACTER(LEN=MAXFILE_LENGTH), INTENT(IN) :: Paramval_char(Numvalues)
      ! Functions
      INTRINSIC :: TRIM
      EXTERNAL :: error_stop
      ! Local Variables
      INTEGER :: i, j, found, dtype
!***********************************************************************
      found = 0
      DO i = 1, Num_control_parameters
        IF ( TRIM(Paramname)==TRIM(Control_parameter_data(i)%name) ) THEN
          found = i
          dtype = Control_parameter_data(i)%data_type
          Control_parameter_data(i)%numvals = Numvalues
          IF ( Control_parameter_data(i)%allocate_flag == 1 ) THEN ! one of variably sized parameters
            IF ( dtype==1 ) THEN
              DEALLOCATE ( Control_parameter_data(i)%values_int )
              ALLOCATE ( Control_parameter_data(i)%values_int(Numvalues) )
            ELSEIF ( dtype==4 ) THEN
              DEALLOCATE ( Control_parameter_data(i)%values_character )
              ALLOCATE ( Control_parameter_data(i)%values_character(Numvalues) )
            ELSE
              CALL error_stop('allocatable control parameter that is real', ERROR_control)
            ENDIF
          ENDIF
          Control_parameter_data(i)%read_flag = 1
          IF ( dtype==1 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_int(j) = Paramval_int(j)
            ENDDO
          ELSEIF ( dtype==4 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_character(j) = Paramval_char(j)
            ENDDO
          ELSE !IF ( dtype==2 ) THEN
            DO j = 1, Numvalues
              Control_parameter_data(i)%values_real(j) = Paramval_real(j)
            ENDDO
          ENDIF
          EXIT
        ENDIF
      ENDDO

      IF ( found==0 ) PRINT *, 'WARNING, control parameter not used: ', TRIM(Paramname), ', ignored'

      END SUBROUTINE set_control_parameter

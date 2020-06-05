!***********************************************************************
!     declare the dimensions
!***********************************************************************
      SUBROUTINE setdims()
      USE PRMS_MODULE
      USE GLOBAL, ONLY: NSTP, NPER, ISSFLG
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, control_integer_array
      INTEGER, EXTERNAL :: control_string, control_integer
      INTEGER, EXTERNAL :: gsfdecl, gsfinit, gsfrun, gsfclean, compute_julday, check_dims
      EXTERNAL :: read_error, PRMS_open_output_file, PRMS_open_input_file, check_module_names, module_error
      EXTERNAL :: read_control_file, setup_dimens, read_parameter_file_dimens, get_control_arguments, setup_params
! Local Variables
      ! Maximum values are no longer limits
! Local Variables
      INTEGER :: idim, iret, j
      INTEGER :: test
!***********************************************************************
      Inputerror_flag = 0

      CALL read_control_file()
      CALL get_control_arguments()

      ! debug print flag:
      ! -1=quiet - reduced screen output
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 ) Print_debug = 0
      IF ( Print_debug>-1 ) PRINT 3
    3 FORMAT (//, 26X, 'U.S. Geological Survey', /, 8X, &
     &        'Coupled Groundwater and Surface-water FLOW model (GSFLOW)', /, &
     &        25X, 'Version 2.2.0 05/01/2020', //, &
     &        '    An integration of the Precipitation-Runoff Modeling System (PRMS)', /, &
     &        '    and the Modular Groundwater Model (MODFLOW-NWT and MODFLOW-2005)', /)

      IF ( control_integer(Parameter_check_flag, 'parameter_check_flag')/=0 ) Parameter_check_flag = 1

      IF ( control_string(Model_mode, 'model_mode')/=0 ) CALL read_error(5, 'model_mode')
      PRMS4_flag = 1
      IF ( Model_mode(:5)=='PRMS5' .OR. Model_mode(:7)=='GSFLOW5' ) PRMS4_flag = 0
      PRMS_flag = 1
      GSFLOW_flag = 0
      ! Model (0=GSFLOW; 1=PRMS; 2=MODFLOW)
      IF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:5)=='DAILY' ) THEN
        Model = PRMS
      ELSEIF ( Model_mode(:6)=='GSFLOW' .OR. Model_mode(:4)=='    ') THEN
        Model = GSFLOW
        GSFLOW_flag = 1
      ELSEIF ( Model_mode(:7)=='MODFLOW' ) THEN
        Model = MODFLOW
        PRMS_flag = 0
      ELSEIF ( Model_mode(:5)=='FROST' ) THEN
        Model = 29
      ELSEIF ( Model_mode(:13)=='WRITE_CLIMATE' ) THEN
        Model = 24
      ELSEIF ( Model_mode(:7)=='CLIMATE' ) THEN
        Model = 26
      ELSEIF ( Model_mode(:5)=='POTET' ) THEN
        Model = 27
      ELSEIF ( Model_mode(:9)=='TRANSPIRE' ) THEN
        Model = 28
      ELSEIF ( Model_mode(:7)=='CONVERT' ) THEN ! can be CONVERT4 or CONVERT5 or CONVERT (=CONVERT5)
        Model = 25
      ELSEIF ( Model_mode(:13)=='DOCUMENTATION' ) THEN
        Model = DOCUMENTATION
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid model_mode value: ', Model_mode
        Inputerror_flag = 1
      ENDIF

      ! get simulation start_time and end_time
      Starttime = -1
      DO j = 1, 6
        IF ( control_integer_array(Starttime(j), j, 'start_time')/=0 ) THEN
          PRINT *, 'ERROR, start_time, index:', j, 'value: ', Starttime(j)
          Inputerror_flag = 1
        ENDIF
      ENDDO
      Start_year = Starttime(1)
      IF ( Start_year<0 ) THEN
        PRINT *, 'ERROR, control parameter start_time must be specified'
        Inputerror_flag = 1
      ENDIF
      Start_month = Starttime(2)
      Start_day = Starttime(3)
      Nowyear = Start_year
      Nowmonth = Start_month
      Nowday = Start_day
      Endtime = -1
      DO j = 1, 6
        IF ( control_integer_array(Endtime(j), j, 'end_time')/=0 ) THEN
          PRINT *, 'ERROR, end_time, index:', j, 'value: ', Endtime(j)
          Inputerror_flag = 1
        ENDIF
      ENDDO
      End_year = Endtime(1)
      IF ( End_year<0 ) THEN
        PRINT *, 'ERROR, control parameter start_time must be specified'
        Inputerror_flag = 1
      ENDIF
      End_month = Endtime(2)
      End_day = Endtime(3)

      startday = compute_julday(Start_year, Start_month, Start_day)
      endday = compute_julday(End_year, End_month, End_day)
      Number_timesteps = endday - startday + 1

      IF ( control_integer(Init_vars_from_file, 'init_vars_from_file')/=0 ) Init_vars_from_file = 0
      IF ( control_integer(Save_vars_to_file, 'save_vars_to_file')/=0 ) Save_vars_to_file = 0

      IF ( Model==MODFLOW ) THEN
! for MODFLOW-only simulations
        Kper_mfo = 1
        mf_timestep = 1
        mf_nowtime = startday
        test = gsfdecl()
        IF ( test/=0 ) CALL module_error(MODNAME, 'declare', test)
        test = gsfinit()
        IF ( test/=0 ) CALL module_error(MODNAME, 'initialize', test)
        PRINT *, ' '
        If ( ISSFLG(Kper_mfo) == 1 .and. nper == 1) THEN
        ELSE
          Process_flag = 0
          DO WHILE ( Kper_mfo<=Nper )
            test = gsfrun()
            IF ( test/=0 ) CALL module_error(MODNAME, 'run', test)
            IF ( mf_timestep==NSTP(Kper_mfo) ) THEN
                Kper_mfo = Kper_mfo + 1
                mf_timestep = 0
            ENDIF
            mf_timestep = mf_timestep + 1
            mf_nowtime = mf_nowtime + 1
          ENDDO
        ENDIF
        test = gsfclean()
        IF ( test/=0 ) CALL module_error(MODNAME, 'clean', test)
        STOP
      ENDIF

      CALL setup_dimens()

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'model_output_file')
      IF ( Print_debug>-2 ) THEN
        CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
        IF ( iret/=0 ) Inputerror_flag = 1
      ENDIF
      IF ( control_string(Param_file, 'param_file')/=0 ) CALL read_error(5, 'param_file')

      ! Check for restart files
      IF ( Init_vars_from_file>0 ) THEN
        IF ( control_string(Var_init_file, 'var_init_file')/=0 ) CALL read_error(5, 'var_init_file')
        CALL PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        IF ( iret/=0 ) Inputerror_flag = 1
      ENDIF
      IF ( Save_vars_to_file==1 ) THEN
        IF ( control_string(Var_save_file, 'var_save_file')/=0 ) CALL read_error(5, 'var_save_file')
      ENDIF

      Temp_module = ' '
      IF ( control_string(Temp_module, 'temp_module')/=0 ) CALL read_error(5, 'temp_module')
      Precip_module = ' '
      IF ( control_string(Precip_module, 'precip_module')/=0 ) CALL read_error(5, 'precip_module')
      Transp_module = ' '
      IF ( control_string(Transp_module, 'transp_module')/=0 ) CALL read_error(5, 'transp_module')
      Et_module = ' '
      IF ( control_string(Et_module, 'et_module')/=0 ) CALL read_error(5, 'et_module')
      Srunoff_module = ' '
      IF ( control_string(Srunoff_module, 'srunoff_module')/=0 ) CALL read_error(5, 'srunoff_module')
      Solrad_module = ' '
      IF ( control_string(Solrad_module, 'solrad_module')/=0 ) CALL read_error(5, 'solrad_module')
      Strmflow_module = 'strmflow'
      IF ( control_string(Strmflow_module, 'strmflow_module')/=0 ) CALL read_error(5, 'strmflow_module')

      IF ( Parameter_check_flag>0 ) CALL check_module_names(Inputerror_flag)

      Climate_precip_flag = 0
      Climate_temp_flag = 0
      Climate_transp_flag = 0
      Climate_potet_flag = 0
      Climate_swrad_flag = 0

      IF ( Precip_module(:11)=='precip_1sta' .OR. Precip_module(:11)=='precip_prms') THEN
        Precip_flag = 1
      ELSEIF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = 2
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = 3
      ELSEIF ( Precip_module(:8)=='ide_dist' ) THEN
        Precip_flag = 5
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = 7
        Climate_precip_flag = 1
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = 6
      ELSEIF ( Precip_module(:15)=='precip_temp_grid' ) THEN
        Precip_flag = 9
      ELSE
        PRINT '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
      ENDIF
      Precip_combined_flag = 0
      IF ( Precip_flag==1 .OR. Precip_flag==2 ) Precip_combined_flag = 1

      IF ( Temp_module(:9)=='temp_1sta' ) THEN
        Temp_flag = 1
      ELSEIF ( Temp_module(:9)=='temp_laps' ) THEN
        Temp_flag = 2
      ELSEIF ( Temp_module(:10)=='temp_dist2' ) THEN
        Temp_flag = 3
      ELSEIF ( Temp_module(:8)=='ide_dist' ) THEN
        Temp_flag = 5
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = 7
        Climate_temp_flag = 1
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = 6
      ELSEIF ( Temp_module(:8)=='temp_sta' ) THEN
        Temp_flag = 8
      ELSEIF ( Temp_module(:15)=='precip_temp_grid' ) THEN
        Temp_flag = 9
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
      ENDIF
      Temp_combined_flag = 0
      IF ( Temp_flag==1 .OR. Temp_flag==2 .OR. Temp_flag==8 ) Temp_combined_flag = 1

      IF ( Transp_module(:13)=='transp_tindex' ) THEN
        Transp_flag = 1
      ELSEIF ( Transp_module(:12)=='transp_frost' ) THEN
        Transp_flag = 2
      ELSEIF ( Transp_module(:11)=='climate_hru' ) THEN
        Transp_flag = 3
        Climate_transp_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid transp_module value: ', Transp_module
        Inputerror_flag = 1
      ENDIF

      IF ( Et_module(:8)=='potet_jh' ) THEN
        Et_flag = 1
      ELSEIF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = 2
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = 7
        Climate_potet_flag = 1
      ELSEIF ( Et_module(:8)=='potet_hs' ) THEN
        Et_flag = 10
      ELSEIF ( Et_module(:12)=='potet_pm_sta' ) THEN
        Et_flag = 6
      ELSEIF ( Et_module(:8)=='potet_pm' ) THEN
        Et_flag = 11
      ELSEIF ( Et_module(:8)=='potet_pt' ) THEN
        Et_flag = 5
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = 4
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
      ENDIF

      ! stream_temp
      IF ( control_integer(Stream_temp_flag, 'stream_temp_flag')/=0 ) Stream_temp_flag = 0
      ! 0 = CBH File; 1 = specified constant; 2 = Stations
      IF ( control_integer(Strmtemp_humidity_flag, 'strmtemp_humidity_flag')/=0 ) Strmtemp_humidity_flag = 0

      IF ( control_integer(Snarea_curve_flag, 'snarea_curve_flag')/=0 ) Snarea_curve_flag = 0
      IF ( control_integer(Soilzone_aet_flag, 'soilzone_aet_flag')/=0 ) Soilzone_aet_flag = 0

      Humidity_cbh_flag = 0
      Windspeed_cbh_flag = 0
      IF ( Et_flag==11 .OR. Et_flag==5 .OR. (Stream_temp_flag==1 .AND. Strmtemp_humidity_flag==0) ) Humidity_cbh_flag = 1
      IF ( Et_flag==11 ) Windspeed_cbh_flag = 1

      IF ( Srunoff_module(:13)=='srunoff_smidx' ) THEN
        Sroff_flag = 1
      ELSEIF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = 2
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid srunoff_module value: ', Srunoff_module
        Inputerror_flag = 1
      ENDIF

      Soilzone_module = 'soilzone'

      IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = 0
      IF ( Solrad_module(:8)=='ddsolrad' ) THEN
        Solrad_flag = 1
      ELSEIF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = 7
        Climate_swrad_flag = 1
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
      ENDIF

      IF ( control_integer(Snow_cbh_flag, 'snow_cbh_flag')/=0 ) Snow_cbh_flag = 0
      IF ( control_integer(Gwflow_cbh_flag, 'gwflow_cbh_flag')/=0 ) Gwflow_cbh_flag = 0

      Climate_hru_flag = 0
      IF ( Climate_temp_flag==1 .OR. Climate_precip_flag==1 .OR. Climate_potet_flag==1 .OR. &
     &     Climate_swrad_flag==1 .OR. Climate_transp_flag==1 .OR. &
     &     Humidity_cbh_flag==1 .OR. Windspeed_cbh_flag==1 .OR. &
     &     Gwflow_cbh_flag==1 .OR. Snow_cbh_flag==1 ) Climate_hru_flag = 1

      Muskingum_flag = 0
      IF ( Strmflow_module(:15)=='strmflow_in_out' ) THEN
        Strmflow_flag = 5
      ELSEIF ( Strmflow_module(:14)=='muskingum_lake' ) THEN
        Strmflow_flag = 3
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value: ', Strmflow_module
        Inputerror_flag = 1
      ELSEIF ( Strmflow_module(:8)=='strmflow' ) THEN
        Strmflow_flag = 1
      ELSEIF ( Strmflow_module(:14)=='muskingum_mann' ) THEN
        Strmflow_flag = 7
        Muskingum_flag = 1
      ELSEIF ( Strmflow_module(:9)=='muskingum' ) THEN
        Strmflow_flag = 4
        Muskingum_flag = 1
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value: ', Strmflow_module
        Inputerror_flag = 1
      ENDIF

! cascade dimensions
      IF ( decldim('ncascade', 0, MAXDIM, &
     &     'Number of HRU links for cascading flow')/=0 ) CALL read_error(7, 'ncascade')
      IF ( decldim('ncascdgw', 0, MAXDIM, &
     &     'Number of GWR links for cascading flow')/=0 ) CALL read_error(7, 'ncascdgw')

! nsegment dimension
      IF ( decldim('nsegment', 0, MAXDIM, 'Number of stream-channel segments')/=0 ) CALL read_error(7, 'nsegment')

! subbasin dimensions
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 ) Subbasin_flag = 1
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')/=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 ) Dprst_flag = 0
      ! 0 = off, 1 = on, 2 = lauren version
      IF ( control_integer(CsvON_OFF, 'csvON_OFF')/=0 ) CsvON_OFF = 0

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')/=0 ) MapOutON_OFF = 0
      idim = 0
      IF ( GSFLOW_flag==1 .OR. MapOutON_OFF==1 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM, &
     &     'Number of unique intersections between HRUs and spatial units of a target map for mapped results')/=0 ) &
     &     CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', 0, MAXDIM, &
     &     'Number of spatial units in the target map for mapped results')/=0 ) CALL read_error(7, 'ngwcell')
      IF ( decldim('nreach', idim, MAXDIM, 'Number of reaches on all stream segments')/=0 ) CALL read_error(7, 'nreach')

      IF ( control_integer(Frozen_flag, 'frozen_flag')/=0 ) Frozen_flag = 0
      IF ( control_integer(Dyn_imperv_flag, 'dyn_imperv_flag')/=0 ) Dyn_imperv_flag = 0
      IF ( control_integer(Dyn_intcp_flag, 'dyn_intcp_flag')/=0 ) Dyn_intcp_flag = 0
      IF ( control_integer(Dyn_covden_flag, 'dyn_covden_flag')/=0 ) Dyn_covden_flag = 0
      IF ( control_integer(Dyn_dprst_flag, 'dyn_dprst_flag')/=0 ) Dyn_dprst_flag = 0
      IF ( control_integer(Dyn_potet_flag, 'dyn_potet_flag')/=0 ) Dyn_potet_flag = 0
      IF ( control_integer(Dyn_covtype_flag, 'dyn_covtype_flag')/=0 ) Dyn_covtype_flag = 0
      IF ( control_integer(Dyn_transp_flag, 'dyn_transp_flag')/=0 ) Dyn_transp_flag = 0
      IF ( control_integer(Dyn_soil_flag, 'dyn_soil_flag')/=0 ) Dyn_soil_flag = 0
      IF ( control_integer(Dyn_radtrncf_flag, 'dyn_radtrncf_flag')/=0 ) Dyn_radtrncf_flag = 0
      IF ( control_integer(Dyn_sro2dprst_perv_flag, 'dyn_sro2dprst_perv_flag')/=0 ) Dyn_sro2dprst_perv_flag = 0
      IF ( control_integer(Dyn_sro2dprst_imperv_flag, 'dyn_sro2dprst_imperv_flag')/=0 ) Dyn_sro2dprst_imperv_flag = 0
      IF ( control_integer(Dyn_fallfrost_flag, 'dyn_fallfrost_flag')/=0 ) Dyn_fallfrost_flag = 0
      IF ( control_integer(Dyn_springfrost_flag, 'dyn_springfrost_flag')/=0 ) Dyn_springfrost_flag = 0
      IF ( control_integer(Dyn_snareathresh_flag, 'dyn_snareathresh_flag')/=0 ) Dyn_snareathresh_flag = 0
      IF ( control_integer(Dyn_transp_on_flag, 'dyn_transp_on_flag')/=0 ) Dyn_transp_on_flag = 0
      Dynamic_flag = 0
      IF ( Dyn_imperv_flag/=0 .OR. Dyn_intcp_flag/=0 .OR. Dyn_covden_flag/=0 .OR. Dyn_dprst_flag/=0 .OR. &
     &     Dyn_potet_flag/=0 .OR. Dyn_covtype_flag/=0 .OR. Dyn_transp_flag/=0 .OR. Dyn_soil_flag /=0 .OR. &
     &     Dyn_radtrncf_flag/=0 .OR. Dyn_sro2dprst_perv_flag/=0 .OR. Dyn_sro2dprst_imperv_flag/=0 .OR. &
     &     Dyn_fallfrost_flag/=0 .OR. Dyn_springfrost_flag/=0 .OR. Dyn_snareathresh_flag/=0 .OR. &
     &     Dyn_transp_on_flag/=0 ) Dynamic_flag = 1
      IF ( control_integer(Gwr_transferON_OFF, 'gwr_transferON_OFF')/=0) Gwr_transferON_OFF = 0
      IF ( control_integer(External_transferON_OFF, 'external_transferON_OFF')/=0 ) External_transferON_OFF = 0
      IF ( control_integer(Dprst_transferON_OFF, 'dprst_transferON_OFF')/=0 ) Dprst_transferON_OFF = 0
      IF ( control_integer(Segment_transferON_OFF, 'segment_transferON_OFF')/=0 ) Segment_transferON_OFF = 0
      IF ( control_integer(Lake_transferON_OFF, 'lake_transferON_OFF')/=0 ) Lake_transferON_OFF = 0
      IF ( control_integer(Gwr_swale_flag, 'gwr_swale_flag')/=0 ) Gwr_swale_flag = 0

! nhru_summary
      IF ( control_integer(NhruOutON_OFF, 'nhruOutON_OFF')/=0 ) NhruOutON_OFF = 0

! nsub_summary
      IF ( control_integer(NsubOutON_OFF, 'nsubOutON_OFF')/=0 ) NsubOutON_OFF = 0

! basin_summary
      IF ( control_integer(BasinOutON_OFF, 'basinOutON_OFF')/=0 ) BasinOutON_OFF = 0

! nsegment_summary
      IF ( control_integer(NsegmentOutON_OFF, 'nsegmentOutON_OFF')/=0 ) NsegmentOutON_OFF = 0

      IF ( control_integer(Prms_warmup, 'prms_warmup')/=0 ) Prms_warmup = 0
      IF ( NhruOutON_OFF>0 .OR. NsubOutON_OFF>0 .OR. BasinOutON_OFF>0 .OR. NsegmentOutON_OFF>0 ) THEN
        IF ( Start_year+Prms_warmup>End_year ) THEN ! change to start full date ???
          PRINT *, 'ERROR, prms_warmup > than simulation time period:', Prms_warmup
          Inputerror_flag = 1
        ENDIF
      ENDIF

! cascade
      ! if cascade_flag = 2, use hru_segment parameter for cascades, ncascade=ncascdgw=nhru (typical polygon HRUs)
      IF ( control_integer(Cascade_flag, 'cascade_flag')/=0 ) Cascade_flag = 1
      ! if cascadegw_flag = 2, use same cascades as HRUs
      IF ( control_integer(Cascadegw_flag, 'cascadegw_flag')/=0 ) Cascadegw_flag = 1

! spatial units
      IF ( decldim('ngw', 1, MAXDIM, 'Number of GWRs')/=0 ) CALL read_error(7, 'ngw')
      IF ( decldim('nhru', 1, MAXDIM, 'Number of HRUs')/=0 ) CALL read_error(7, 'nhru')
      IF ( decldim('nssr', 1, MAXDIM, 'Number of subsurface reservoirs')/=0 ) CALL read_error(7, 'nssr')
      IF ( decldim('nlake', 0, MAXDIM, 'Number of lakes')/=0 ) CALL read_error(7, 'nlake')
      IF ( decldim('nlake_hrus', 0, MAXDIM, 'Number of lake HRUs')/=0 ) CALL read_error(7, 'nlake_hrus')
      IF ( decldim('npoigages', 0, MAXDIM, 'Number of POI gages')/=0 ) CALL read_error(7, 'npoigages')

! Time-series data stations, need to know if in Data File
      IF ( decldim('nrain', 0, MAXDIM, 'Number of precipitation-measurement stations')/=0 ) CALL read_error(7, 'nrain')
      IF ( decldim('nsol', 0, MAXDIM, 'Number of solar-radiation measurement stations')/=0 ) CALL read_error(7, 'nsol')
      IF ( decldim('ntemp', 0, MAXDIM, 'Number of air-temperature-measurement stations')/=0 ) CALL read_error(7, 'ntemp')
      IF ( decldim('nobs', 0, MAXDIM, 'Number of streamflow-measurement stations')/=0 ) CALL read_error(7, 'nobs')
      IF ( decldim('nevap', 0, MAXDIM, 'Number of pan-evaporation data sets')/=0 ) CALL read_error(7, 'nevap')
      IF ( decldim('nratetbl', 0, MAXDIM, 'Number of rating-table data sets for lake elevations') &
     &     /=0 ) CALL read_error(7, 'nratetbl')
      IF ( decldim('nsnow', 0, MAXDIM, 'Number of snow-depth-measurement stations')/=0 ) CALL read_error(7, 'nsnow')

! depletion curves
      IF ( decldim('ndepl', 1, MAXDIM, 'Number of snow-depletion curves')/=0 ) CALL read_error(7, 'ndelp')
      IF ( decldim('ndeplval', 11, MAXDIM, 'Number of values in all snow-depletion curves (set to ndepl*11)')/=0 ) &
     &     CALL read_error(7, 'ndelplval')

! water-use
      IF ( decldim('nwateruse', 0, MAXDIM, 'Number of water-use data sets')/=0 ) CALL read_error(7, 'nwateruse')
      IF ( decldim('nexternal', 0, MAXDIM, &
     &      'Number of external water-use sources or destinations')/=0 ) CALL read_error(7, 'nexternal')
      IF ( decldim('nconsumed', 0, MAXDIM, 'Number of consumptive water-use destinations')/=0 ) CALL read_error(7, 'nconsumed')

! fixed dimensions
      IF ( declfix('ndays', 366, 366, 'Maximum number of days in a year ')/=0 ) CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')/=0 ) CALL read_error(7, 'nmonths')
      IF ( declfix('one', 1, 1, 'Number of values for scaler array')/=0 ) CALL read_error(7, 'one')

      IF ( Inputerror_flag==1 ) PRINT '(//,A,/,A,/ )', '**FIX input errors in your Control File to continue**', &
     &     'NOTE: some errors may be due to use of defalut values'

      CALL setup_params()
      CALL read_parameter_file_dimens()
      IF ( check_dims()/=0 ) STOP -1

      END SUBROUTINE setdims

!***********************************************************************
!     Get and check consistency of dimensions with flags
!***********************************************************************
      INTEGER FUNCTION check_dims()
      USE PRMS_MODULE
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getdim
      EXTERNAL :: check_dimens
!***********************************************************************

      Nhru = getdim('nhru')
      IF ( Nhru==-1 ) CALL read_error(7, 'nhru')

      Nssr = getdim('nssr')
      IF ( Nssr==-1 ) CALL read_error(7, 'nssr')

      Ngw = getdim('ngw')
      IF ( Ngw==-1 ) CALL read_error(7, 'ngw')

      Ntemp = getdim('ntemp')
      IF ( Ntemp==-1 ) CALL read_error(6, 'ntemp')

      Nrain = getdim('nrain')
      IF ( Nrain==-1 ) CALL read_error(6, 'nrain')

      Nsol = getdim('nsol')
      IF ( Nsol==-1 ) CALL read_error(6, 'nsol')

      Nsnow = getdim('nsnow')
      IF ( Nsnow==-1 ) CALL read_error(6, 'nsnow')

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

      Nevap = getdim('nevap')
      IF ( Nevap==-1 ) CALL read_error(6, 'nevap')

      Ncascade = getdim('ncascade')
      IF ( Ncascade==-1 ) CALL read_error(7, 'ncascade')
      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw==-1 ) CALL read_error(7, 'ncascdgw')
      IF ( Cascade_flag==2 ) THEN
        Ncascade = Nhru
        Cascadegw_flag = 2
      ENDIF
      IF ( Cascadegw_flag==2 ) Ncascdgw = Ncascade
      IF ( Ncascade==0 ) Cascade_flag = 0
      IF ( Ncascdgw==0 .OR. GSFLOW_flag==1 .OR. Model==MODFLOW ) Cascadegw_flag = 0
      IF ( (Cascade_flag>0 .OR. Cascadegw_flag>0) .AND. Model/=25 ) THEN ! don't call if model_mode = CONVERT
        Call_cascade = 1
      ELSE
        Call_cascade = 0
      ENDIF
      IF ( GSFLOW_flag==1 .AND. Call_cascade==0 ) THEN
        PRINT *, 'ERROR, GSFLOW requires that PRMS cascade routing is active'
        Inputerror_flag = 1
      ENDIF

      Nwateruse = getdim('nwateruse')
      IF ( Nwateruse==-1 ) CALL read_error(7, 'nwateruse')

      Nexternal = getdim('nexternal')
      IF ( Nexternal==-1 ) CALL read_error(6, 'nexternal')

      Nconsumed = getdim('nconsumed')
      IF ( Nconsumed==-1 ) CALL read_error(6, 'nconsumed')

      Npoigages = getdim('npoigages')
      IF ( Npoigages==-1 ) CALL read_error(6, 'npoigages')

      Nlake = getdim('nlake')
      IF ( Nlake==-1 ) CALL read_error(7, 'nlake')

      Nlake_hrus = getdim('nlake_hrus')
      IF ( Nlake_hrus==-1 ) CALL read_error(7, 'nlake_hrus')
      IF ( Nlake>0 .AND. Nlake_hrus==0 ) Nlake_hrus = Nlake

      Ndepl = getdim('ndepl')
      IF ( Ndepl==-1 ) CALL read_error(7, 'ndepl')

      Ndeplval = getdim('ndeplval')
      IF ( Ndeplval==-1 ) CALL read_error(7, 'ndeplval')

      Nsub = getdim('nsub')
      IF ( Nsub==-1 ) CALL read_error(7, 'nsub')
      ! default = 1, turn off if no subbasins
      IF ( Subbasin_flag==1 .AND. Nsub==0 ) Subbasin_flag = 0

      Nsegment = getdim('nsegment')
      IF ( Nsegment==-1 ) CALL read_error(7, 'nsegment')

      Nhrucell = getdim('nhrucell')
      IF ( Nhrucell==-1 ) CALL read_error(6, 'nhrucell')

      Ngwcell = getdim('ngwcell')
      IF ( Ngwcell==-1 ) CALL read_error(6, 'ngwcell')

      Nratetbl = getdim('nratetbl')
      IF ( Nratetbl==-1 ) CALL read_error(6, 'nratetbl')

      Water_use_flag = 0
      IF ( Nwateruse>0 ) THEN
        IF ( Segment_transferON_OFF==1 .OR. Gwr_transferON_OFF==1 .OR. External_transferON_OFF==1 .OR. &
     &       Dprst_transferON_OFF==1 .OR. Lake_transferON_OFF==1 .OR. Nconsumed>0 .OR. Nwateruse>0 ) Water_use_flag = 1
      ENDIF

      IF ( Segment_transferON_OFF==1 .OR. Gwr_transferON_OFF==1 .OR. External_transferON_OFF==1 .OR. &
     &     Dprst_transferON_OFF==1 .OR. Lake_transferON_OFF==1 .OR. Nconsumed>0 ) THEN
        IF ( Dprst_transferON_OFF==1 .AND. Dprst_flag==0 ) THEN
          PRINT *, 'ERROR, specified water-use event based dprst input and have dprst inactive'
          Inputerror_flag = 1
        ENDIF
        IF ( Lake_transferON_OFF==1 .AND. Strmflow_flag/=3 ) THEN
          PRINT *, 'ERROR, specified water-use event based lake input and have lake simulation inactive'
          Inputerror_flag = 1
        ENDIF
      ENDIF

      Stream_order_flag = 0
      IF ( Nsegment>0 .AND. Strmflow_flag>1 .AND. Model/=0 ) THEN
        Stream_order_flag = 1 ! strmflow_in_out, muskingum, muskingum_lake, muskingum_mann
      ENDIF

      IF ( Nsegment<1 .AND. Model/=DOCUMENTATION ) THEN
        IF ( Stream_order_flag==1 .OR. Call_cascade==1 ) THEN
          PRINT *, 'ERROR, streamflow and cascade routing require nsegment > 0, specified as:', Nsegment
          Inputerror_flag = 1
        ENDIF
      ENDIF

      Lake_route_flag = 0
      IF ( Nlake>0 .AND. Strmflow_flag==3 .AND. Model==PRMS ) Lake_route_flag = 1 ! muskingum_lake

      IF ( Stream_temp_flag>0 .AND. Stream_order_flag==0 ) THEN
        PRINT *, 'ERROR, stream temperature computation requires streamflow routing, thus strmflow_module'
        PRINT *, '       must be set to strmflow_in_out, muskingum, muskingum_mann, or muskingum_lake'
        Inputerror_flag = 1
      ENDIF

      IF ( NsubOutON_OFF==1 .AND. Nsub==0 ) THEN
        NsubOutON_OFF = 0
        IF ( Print_debug>-1 ) PRINT *, 'WARNING, nsubOutON_OFF = 1 and nsub = 0, thus nsub_summary not used'
      ENDIF

      IF ( NsegmentOutON_OFF==1 .AND. Nsegment==0 ) THEN
        NsegmentOutON_OFF = 0
        IF ( Print_debug>-1 ) PRINT *, 'WARNING, nsegmentOutON_OFF = 1 and nsegment = 0, thus nsegment_summary not used'
      ENDIF

      IF ( Model==DOCUMENTATION .OR. Parameter_check_flag>0 ) CALL check_dimens()

      check_dims = Inputerror_flag
      END FUNCTION check_dims

!***********************************************************************
!     Check consistency of dimensions with flags
!***********************************************************************
      SUBROUTINE check_dimens()
      USE PRMS_MODULE
      IMPLICIT NONE
!***********************************************************************
      IF ( Nhru==0 .OR. Nssr==0 .OR. Ngw==0 ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must be > 0: nhru=', Nhru, ', nssr=', Nssr, ', ngw=', Ngw
        Inputerror_flag = 1
      ELSEIF ( Nssr/=Nhru .OR. Ngw/=Nhru ) THEN
        PRINT *, 'ERROR, nhru, nssr, and ngw must equal: nhru=', Nhru, ', nssr=', Nssr, ', ngw=', Ngw
        Inputerror_flag = 1
      ENDIF
      IF ( Ndepl==0 ) THEN
        PRINT *, 'ERROR, ndepl must be > 0: ndepl=', Ndepl
        Inputerror_flag = 1
      ENDIF
      IF ( Ndeplval/=Ndepl*11 ) THEN
        PRINT *, 'ERROR, ndeplval must be = ndepl*11: ndeplval:', Ndeplval, ', ndepl=', Ndepl
        Inputerror_flag = 1
      ENDIF

      IF ( Model==DOCUMENTATION ) THEN
        IF ( Ntemp==0 ) Ntemp = 1
        IF ( Nrain==0 ) Nrain = 1
        IF ( Nlake==0 ) Nlake = 1
        IF ( Nlake_hrus==0 ) Nlake_hrus = 1
        IF ( Nsol==0 ) Nsol = 1
        IF ( Nobs==0 ) Nobs = 1
        IF ( Ncascade==0 ) Ncascade = 1
        IF ( Ncascdgw==0 ) Ncascdgw = 1
        IF ( Nsub==0 ) Nsub = 1
        IF ( Nevap==0 ) Nevap = 1
        IF ( Nhrucell==0 ) Nhrucell = 1
        IF ( Ngwcell==0 ) Ngwcell = 1
        IF ( Nsegment==0 ) Nsegment = 1
        IF ( Nratetbl==0 ) Nratetbl = 4
        IF ( Nwateruse==0 ) Nwateruse = 1
        IF ( Nexternal==0 ) Nexternal = 1
        IF ( Nconsumed==0 ) Nconsumed = 1
        IF ( Npoigages==0 ) Npoigages = 1
        Subbasin_flag = 1
        Cascade_flag = 1
        Cascadegw_flag = 1
        Call_cascade = 1
        Stream_order_flag = 1
        Climate_hru_flag = 1
        Lake_route_flag = 1
        Water_use_flag = 1
        Segment_transferON_OFF = 1
        Gwr_transferON_OFF = 1
        External_transferON_OFF = 1
        Dprst_transferON_OFF = 1
        Lake_transferON_OFF = 1
      ENDIF

      END SUBROUTINE check_dimens

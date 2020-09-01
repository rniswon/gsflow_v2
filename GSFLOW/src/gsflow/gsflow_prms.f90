!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
    USE ISO_FORTRAN_ENV
    USE PRMS_CONSTANTS, ONLY: MODFLOW, MAX_DAYS_PER_YEAR, DEBUG_minimum, DEBUG_less, DEBUG_WB, &
   &    RUN, DECL, INIT, SETDIMENS, CLEAN, ON, OFF, ERROR_dim, ERROR_open_out, ERROR_param, ERROR_restart, &
   &    ERROR_modflow, PRMS, GSFLOW, CASCADE_NORMAL, CASCADE_HRU_SEGMENT, CASCADE_OFF, &
   &    CASCADEGW_SAME, CASCADEGW_OFF, &
   &    xyz_dist_module, ide_dist_module, temp_dist2_module, temp_map_module, precip_dist2_module, &
   &    DOCUMENTATION, MAXDIM, MAXFILE_LENGTH, MAXCONTROL_LENGTH, &
   &    potet_jh_module, potet_hamon_module, potet_pan_module, potet_pt_module, potet_pm_sta_module, &
   &    potet_pm_module, potet_hs_module, strmflow_muskingum_lake_module, strmflow_in_out_module, &
   &    strmflow_noroute_module, strmflow_muskingum_mann_module, &
   &    strmflow_muskingum_module, precip_1sta_module, precip_laps_module, &
   &    climate_hru_module, precip_map_module, temp_1sta_module, temp_laps_module, temp_sta_module, &
   &    smidx_module, carea_module
      IMPLICIT NONE
      character(LEN=*), parameter :: &
     &  EQULS = '===================================================================='
    character(len=*), parameter :: MODDESC = 'PRMS Computation Order'
    character(len=11), parameter :: MODNAME = 'gsflow_prms'
    character(len=*), parameter :: PRMS_versn = '2020-09-01'
    character(len=*), parameter :: PRMS_VERSION = 'Version 5.2.0 09/01/2020'
      CHARACTER(LEN=8), SAVE :: Process
! Dimensions
      INTEGER, SAVE :: Nratetbl, Nwateruse, Nexternal, Nconsumed, Npoigages, Ncascade, Ncascdgw
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nhrucell, Nlake, Ngwcell, Nlake_hrus, NLAKES_MF
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Ndepl, Nobs, Nevap, Ndeplval
! Global
      INTEGER, SAVE :: Model, Process_flag, Call_cascade
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag, Climate_potet_flag, Climate_transp_flag
      INTEGER, SAVE :: Lake_route_flag, Strmflow_flag, Stream_order_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, Climate_hru_flag, Climate_swrad_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag, Muskingum_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep
      INTEGER, SAVE :: Humidity_cbh_flag, Windspeed_cbh_flag
      INTEGER, SAVE :: Grid_flag, PRMS_flag, GSFLOW_flag, PRMS4_flag
      INTEGER, SAVE :: Kper_mfo, Kkstp_mfo
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Dynamic_flag, Water_use_flag, Prms_warmup
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      INTEGER, SAVE :: Diversion2soil_flag, Have_lakes, Soilzone_add_water_use
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
!   Declared Variables
      INTEGER, SAVE :: Kkiter
      REAL, SAVE, ALLOCATABLE :: Hru_ag_irr(:)    !Ag irrigation added to HRU
!   Declared Parameters
      INTEGER, SAVE :: Mxsziter
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 9=precip_temp_map
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=temp_sta; 9=precip_temp_map
! Control parameters
      INTEGER, SAVE :: Starttime(6), Endtime(6), Modflow_time_zero(6)
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Orad_flag, Cascade_flag, Cascadegw_flag
      INTEGER, SAVE :: NhruOutON_OFF, Gwr_swale_flag, NsubOutON_OFF, BasinOutON_OFF, NsegmentOutON_OFF
      INTEGER, SAVE :: Stream_temp_flag, Strmtemp_humidity_flag, Stream_temp_shade_flag
      INTEGER, SAVE :: Snarea_curve_flag, Soilzone_aet_flag, statsON_OFF
      INTEGER, SAVE :: Snow_cbh_flag, Gwflow_cbh_flag, Frozen_flag, Glacier_flag
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Model_output_file, Var_init_file, Var_save_file
      CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Csv_output_file, Model_control_file, Param_file
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Temp_module, Srunoff_module, Et_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Strmflow_module, Transp_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Model_mode, Precip_module, Solrad_module
      CHARACTER(LEN=MAXCONTROL_LENGTH), SAVE :: Modflow_name
      CHARACTER(LEN=8), SAVE :: Soilzone_module
      INTEGER, SAVE :: Dyn_imperv_flag, Dyn_intcp_flag, Dyn_covden_flag, Dyn_covtype_flag, Dyn_transp_flag, Dyn_potet_flag
      INTEGER, SAVE :: Dyn_soil_flag, Dyn_radtrncf_flag, Dyn_dprst_flag,  Dprst_transferON_OFF
      INTEGER, SAVE :: Dyn_snareathresh_flag, Dyn_transp_on_flag
      INTEGER, SAVE :: Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Dyn_fallfrost_flag, Dyn_springfrost_flag
      INTEGER, SAVE :: Gwr_transferON_OFF, External_transferON_OFF, Segment_transferON_OFF, Lake_transferON_OFF
      END MODULE PRMS_MODULE

!***********************************************************************
      INTEGER FUNCTION call_modules(Arg)
      USE PRMS_MODULE
      IMPLICIT NONE
! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Arg
! Functions
      INTRINSIC :: DATE_AND_TIME, INT
      INTEGER, EXTERNAL :: check_dims, basin, climateflow, prms_time !, setup
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date, routing
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt, potet_pm
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow
      INTEGER, EXTERNAL :: srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, write_climate_hru
      INTEGER, EXTERNAL :: strmflow_in_out, muskingum, muskingum_lake, numchars
      INTEGER, EXTERNAL :: water_use_read, dynamic_param_read, potet_pm_sta
      INTEGER, EXTERNAL :: stream_temp, glacr
      EXTERNAL :: module_error, print_module, PRMS_open_output_file, precip_temp_map
      EXTERNAL :: call_modules_restart, water_balance, basin_summary, nsegment_summary
      EXTERNAL :: prms_summary, nhru_summary, module_doc, convert_params, read_error, nsub_summary, error_stop
      INTEGER, EXTERNAL :: gsflow_modflow, gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      INTEGER, EXTERNAL :: declparam, getparam, declvar
! Local Variables
      INTEGER :: i, iret, nc
!***********************************************************************
      call_modules = 1

      Process = Arg

      IF ( Process(:3)=='run' ) THEN
        Process_flag = RUN !(0=run, 1=declare, 2=init, 3=clean, 4=setdims)
        Soilzone_add_water_use = OFF

      ELSEIF ( Process(:4)=='decl' ) THEN
        CALL DATE_AND_TIME(VALUES=Elapsed_time_start)
        Execution_time_start = Elapsed_time_start(5)*3600 + Elapsed_time_start(6)*60 + &
     &                         Elapsed_time_start(7) + Elapsed_time_start(8)*0.001

        Process_flag = DECL

        IF ( PRMS_flag==ON ) THEN ! PRMS is active (GSFLOW or PRMS)
          IF ( check_dims()/=0 ) ERROR STOP ERROR_dim
        ENDIF

        IF ( Print_debug>DEBUG_minimum ) THEN
          PRINT 10, PRMS_VERSION
          WRITE ( PRMS_output_unit, 10 ) PRMS_VERSION
        ENDIF
  10  FORMAT (/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
  15  FORMAT (/, 8X, 'Process',  12X, 'Available Modules', /, 68('-'), /, &
     &        '  Basin Definition: basin', /, &
     &        '    Cascading Flow: cascade', /, &
     &        '  Time Series Data: obs, water_use_read, dynamic_param_read', /, &
     &        '   Potet Solar Rad: soltab', /, &
     &        '  Temperature Dist: temp_1sta, temp_laps, temp_dist2, climate_hru,', /, &
     &        '                    temp_map', /, &
     &        '       Precip Dist: precip_1sta, precip_laps, precip_dist2,', /, &
     &        '                    climate_hru, precip_map', /, &
     &        'Temp & Precip Dist: xyz_dist, ide_dist', /, &
     &        '    Solar Rad Dist: ccsolrad, ddsolrad, climate_hru', /, &
     &        'Transpiration Dist: transp_tindex, climate_hru, transp_frost', /, &
     &        '      Potential ET: potet_hamon, potet_jh, potet_pan, climate_hru,', /, &
     &        '                    potet_hs, potet_pt, potet_pm, potet_pm_sta', /, &
     &        '      Interception: intcp', /, &
     &        'Snow & Glacr Dynam: snowcomp, glacr', /, &
     &        '    Surface Runoff: srunoff_smidx, srunoff_carea', /, &
     &        '         Soil Zone: soilzone', /, &
     &        '       Groundwater: gwflow', /, &
     &        'Streamflow Routing: strmflow, strmflow_in_out, muskingum,', /, &
     &        '                    muskingum_lake, muskingum_mann', /, &
     &        'Stream Temperature: stream_temp', /, &
     &        '    Output Summary: basin_sum, subbasin, map_results, prms_summary,', /, &
     &        '                    nhru_summary, nsub_summary, water_balance', /, &
     &        '                    basin_summary, nsegment_summary', /, &
     &        '     Preprocessing: write_climate_hru, frost_date', /, 68('-'))
  16  FORMAT (//, 4X, 'Active modules listed in the order in which they are called', //, 8X, 'Process', 20X, &
     &        'Module', 9X, 'Version Date', /, A)

        Diversion2soil_flag = OFF
        IF ( GSFLOW_flag==ON ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF

        IF ( Print_debug>DEBUG_minimum ) THEN
          PRINT 15
          PRINT 9002
          WRITE ( PRMS_output_unit, 15 )
          PRINT 16, EQULS(:62)
          WRITE ( PRMS_output_unit, 16 ) EQULS(:62)
        ENDIF
        CALL print_module(MODDESC, MODNAME, PRMS_versn)

        IF ( GSFLOW_flag==ON .OR. Model==DOCUMENTATION ) THEN
          IF ( declvar(MODNAME, 'KKITER', 'one', 1, 'integer', &
     &         'Current iteration in GSFLOW simulation', 'none', KKITER)/=0 ) CALL read_error(3, 'KKITER')
          IF ( declparam(MODNAME, 'mxsziter', 'one', 'integer', &
     &         '0', '0', '5000', &
     &         'Maximum number of iterations soilzone states are computed', &
     &         'Maximum number of iterations soilzone states are computed', &
     &         'none')/=0 ) CALL read_error(1, 'mxsziter')
          ALLOCATE ( Gvr_cell_pct(Nhrucell) )
          IF ( Nhru/=Nhrucell ) THEN
            IF ( declparam(MODNAME, 'gvr_cell_pct', 'nhrucell', 'real', &
     &           '0.0', '0.0', '1.0', &
     &           'Proportion of the grid cell associated with each GVR', &
     &           'Proportion of the grid cell area associated with each gravity reservoir', &
     &           'decimal fraction')/=0 ) CALL read_error(1, 'gvr_cell_pct')
          ENDIF
          ALLOCATE ( Gvr_cell_id(Nhrucell) )
          IF ( declparam(MODNAME, 'gvr_cell_id', 'nhrucell', 'integer', &
     &         '-1', '-1', '999999999', &
     &         'Corresponding grid cell id associated with each GVR', &
     &         'Index of the grid cell associated with each gravity reservoir', &
     &         'none')/=0 ) CALL read_error(1, 'gvr_cell_id')
            ! Allocate variable for adding irrigation water to HRU from AG Package, always declare for now
            ALLOCATE ( Hru_ag_irr(Nhru) )
            IF ( Diversion2soil_flag==ON ) THEN
              IF ( declvar(MODNAME, 'hru_ag_irr', 'nhru', Nhru, 'real', &
     &             'Irrigation added to soilzone from MODFLOW wells', 'inches', Hru_ag_irr)/=0 ) &
     &             CALL read_error(3, 'hru_ag_irr')
            ENDIF
            Hru_ag_irr = 0.0
        ENDIF

        Have_lakes = OFF ! set for modes when MODFLOW is not active
        Kkiter = 1 ! set for PRMS-only mode

        Timestep = 0
        IF ( Init_vars_from_file>0 ) CALL call_modules_restart(1)

      ELSEIF ( Process(:4)=='init' ) THEN
        Process_flag = INIT

        Grid_flag = OFF
        IF ( Nhru==Nhrucell ) Grid_flag = ON
        IF ( GSFLOW_flag==ON ) THEN
          IF ( Nhru==Nhrucell ) THEN
            Gvr_cell_pct = 1.0
          ELSE
            IF ( getparam(MODNAME, 'gvr_cell_pct', Nhrucell, 'real', &
     &           Gvr_cell_pct)/=0 ) CALL read_error(2, 'gvr_cell_pct')
          ENDIF
          IF ( getparam(MODNAME, 'mxsziter', 1, 'integer', Mxsziter)/=0 ) CALL read_error(2, 'mxsziter')
          IF ( getparam(MODNAME, 'gvr_cell_id', Nhrucell, 'integer', &
     &         Gvr_cell_id)/=0 ) CALL read_error(2, 'gvr_cell_id')
          IF ( Gvr_cell_id(1)==-1 ) THEN
            IF ( Nhru==Nhrucell ) THEN
              IF ( Print_debug>DEBUG_less ) THEN
                PRINT *, 'WARNING, HRUs are assumed to be numbered from upper left corner'
                PRINT *, '         gvr_cell_id values are set to 1 through nhru'
              ENDIF
              DO i = 1, Nhrucell
                Gvr_cell_id(i) = i
              ENDDO
            ELSE
              CALL error_stop('gvr_cell_id must be specified', ERROR_param)
            ENDIF
          ENDIF
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
          IF ( Have_lakes==1 .AND. Nlake/=NLAKES_MF ) THEN
            PRINT *, 'ERROR, NLAKES not equal to Nlake'
            PRINT *, '       NLAKES=', NLAKES_MF, '; Nlake=', Nlake
            ERROR STOP ERROR_modflow
          ENDIF
!          IF ( Nsegment/=NSS ) THEN
!            PRINT *, 'ERROR, NSS not equal to nsegment'
!            PRINT *, '       NSS=', NSS, '; nsegment=', Nsegment
!            STOP
!          ENDIF
        ENDIF

        nc = numchars(Model_control_file)
        IF ( Print_debug>DEBUG_less ) PRINT 9004, 'Using Control File: ', Model_control_file(:nc)
        IF ( Print_debug>DEBUG_minimum ) WRITE ( PRMS_output_unit, 9004 ) 'Using Control File: ', Model_control_file(:nc)

        nc = numchars(Param_file)
        IF ( Print_debug>DEBUG_less ) PRINT 9004, 'Using Parameter File: ', Param_file(:nc)
        IF ( Print_debug>DEBUG_minimum ) WRITE ( PRMS_output_unit, 9004 ) 'Using Parameter File: ', Param_file(:nc)

        IF ( Init_vars_from_file>0 ) THEN
          nc = numchars(Var_init_file)
          IF ( Print_debug>DEBUG_less ) PRINT 9004, 'Using var_init_file: ', Var_init_file(:nc)
        ENDIF
        IF ( Save_vars_to_file==ON ) THEN
          nc = numchars(Var_save_file)
          IF ( Print_debug>DEBUG_less ) PRINT 9004, 'Using var_save_file: ', Var_save_file(:nc)
        ENDIF

        IF ( Print_debug>DEBUG_minimum ) THEN
          nc = numchars(Model_output_file)
          PRINT 9004, 'Writing PRMS Water Budget File: ', Model_output_file(:nc)
        ENDIF

      ELSEIF ( Process(:7)=='setdims' ) THEN
        Process_flag = SETDIMENS

      ELSE  !IF ( Process(:5)=='clean' ) THEN
        Process_flag = CLEAN

        IF ( Init_vars_from_file>0 ) CLOSE ( Restart_inunit )
        IF ( Save_vars_to_file==ON ) THEN
          CALL PRMS_open_output_file(Restart_outunit, Var_save_file, 'var_save_file', 1, iret)
          IF ( iret/=0 ) ERROR STOP ERROR_open_out
          CALL call_modules_restart(0)
        ENDIF
        IF ( Model==GSFLOW ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF
      ENDIF

      IF ( Model==DOCUMENTATION ) THEN
        IF ( Process_flag==SETDIMENS .OR. Process_flag==DECL ) THEN
          Init_vars_from_file = 0 ! make sure this is set so all variables and parameters are declared
          CALL module_doc()
          call_modules = 0
          RETURN
        ELSE
          STOP 0
        ENDIF
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=RUN .AND. PRMS_flag==ON ) THEN
        call_modules = basin()
        IF ( call_modules/=0 ) CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==ON ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 ) CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 ) CALL module_error('climateflow', Arg, call_modules)

        call_modules = soltab()
        IF ( call_modules/=0 ) CALL module_error('soltab', Arg, call_modules)

!        call_modules = setup()
!        IF ( call_modules/=0 ) CALL module_error('setup', Arg, call_modules)
      ENDIF

      call_modules = prms_time()
      IF ( call_modules/=0 ) CALL module_error('prms_time', Arg, call_modules)

      call_modules = obs()
      IF ( call_modules/=0 ) CALL module_error('obs', Arg, call_modules)

      IF ( Water_use_flag==ON ) THEN
        call_modules = water_use_read()
        IF ( call_modules/=0 ) CALL module_error('water_use_read', Arg, call_modules)
      ENDIF

      IF ( Dynamic_flag==ON ) THEN
        call_modules = dynamic_param_read()
        IF ( call_modules/=0 ) CALL module_error('dynamic_param_read', Arg, call_modules)
      ENDIF

      IF ( Climate_hru_flag==ON ) THEN
        call_modules = climate_hru()
        IF ( call_modules/=0 ) CALL module_error('climate_hru', Arg, call_modules)
      ENDIF

      IF ( Climate_temp_flag==OFF ) THEN
        IF ( Temp_combined_flag==ON ) THEN
          call_modules = temp_1sta_laps()
        ELSEIF ( Temp_flag==xyz_dist_module ) THEN
          call_modules = xyz_dist()
        ELSEIF ( Temp_flag==temp_dist2_module ) THEN
          call_modules = temp_dist2()
        ELSEIF ( Temp_flag==ide_dist_module ) THEN
          call_modules = ide_dist()
        ELSE !IF ( Temp_flag==temp_map_module ) THEN ! may be a problem, temp needs to be first ??? rsr
          CALL precip_temp_map()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Temp_module, Arg, call_modules)
      ENDIF

      IF ( Climate_precip_flag==OFF ) THEN
        IF ( Precip_combined_flag==ON ) THEN
          call_modules = precip_1sta_laps()
        ELSEIF ( Precip_flag==precip_dist2_module ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

      IF ( Model==26 ) THEN
        IF ( Process_flag==RUN ) RETURN
      ENDIF

! frost_date is a pre-process module
      IF ( Model==29 ) THEN
        call_modules = frost_date()
        IF ( call_modules/=0 ) CALL module_error('frost_date', Arg, call_modules)
        IF ( Process_flag==RUN ) RETURN
        IF ( Process_flag==CLEAN ) STOP 0
      ENDIF

      IF ( Climate_swrad_flag==0 ) THEN
        IF ( Solrad_flag==1 ) THEN
          call_modules = ddsolrad()
        ELSE !IF ( Solrad_flag==2 ) THEN
          call_modules = ccsolrad()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Solrad_module, Arg, call_modules)
      ENDIF

      IF ( Transp_flag==1 ) THEN
        call_modules = transp_tindex()
      ELSEIF ( Transp_flag==2 ) THEN
        call_modules = transp_frost()
      ENDIF
      IF ( call_modules/=0 ) CALL module_error(Transp_module, Arg, call_modules)

      IF ( Model==28 ) THEN
        IF ( Process_flag==RUN ) RETURN
      ENDIF

      IF ( Climate_potet_flag==0 ) THEN
        IF ( Et_flag==potet_jh_module ) THEN
          call_modules = potet_jh()
        ELSEIF ( Et_flag==potet_hamon_module ) THEN
          call_modules = potet_hamon()
        ELSEIF ( Et_flag==potet_pan_module ) THEN
          call_modules = potet_pan()
        ELSEIF ( Et_flag==potet_pt_module ) THEN
          call_modules = potet_pt()
        ELSEIF ( Et_flag==potet_pm_sta_module ) THEN
          call_modules = potet_pm_sta()
        ELSEIF ( Et_flag==potet_pm_module ) THEN
          call_modules = potet_pm()
        ELSE !IF ( Et_flag==potet_hs_module ) THEN
          call_modules = potet_hs()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Et_module, Arg, call_modules)
      ENDIF

      IF ( Model==24 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 ) CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==RUN ) RETURN
      ENDIF

      IF ( Model==27 ) THEN
        IF ( Process_flag==RUN ) RETURN
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 ) CALL module_error('intcp', Arg, call_modules)

      ! rsr, need to do something if snow_cbh_flag=1
      call_modules = snowcomp()
      IF ( call_modules/=0 ) CALL module_error('snowcomp', Arg, call_modules)

      IF ( Glacier_flag==ON ) THEN
        call_modules = glacr()
        IF ( call_modules/=0 ) CALL module_error('glacr', Arg, call_modules)
      ENDIF

      call_modules = srunoff()
      IF ( call_modules/=0 ) CALL module_error(Srunoff_module, Arg, call_modules)

! for PRMS-only simulations
      IF ( Model==PRMS ) THEN
        call_modules = soilzone()
        IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)

        ! rsr, need to do something if gwflow_cbh_flag=1
        call_modules = gwflow()
        IF ( call_modules/=0 ) CALL module_error('gwflow', Arg, call_modules)

        IF ( Stream_order_flag==ON ) THEN
          call_modules = routing()
          IF ( call_modules/=0 ) CALL module_error('routing', Arg, call_modules)
        ENDIF

        IF ( Strmflow_flag==strmflow_noroute_module ) THEN
          call_modules = strmflow()
        ELSEIF ( Muskingum_flag==ON ) THEN ! muskingum = 4; muskingum_mann = 7
          call_modules = muskingum()
        ELSEIF ( Strmflow_flag==strmflow_in_out_module ) THEN
          call_modules = strmflow_in_out()
        ELSEIF ( Strmflow_flag==strmflow_muskingum_lake_module ) THEN
          call_modules = muskingum_lake()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Strmflow_module, Arg, call_modules)

        IF ( Stream_temp_flag==ON ) call_modules = stream_temp()

        IF ( Print_debug>DEBUG_minimum ) THEN
          call_modules = basin_sum()
          IF ( call_modules/=0 ) CALL module_error('basin_sum', Arg, call_modules)
        ENDIF

        IF ( Print_debug==DEBUG_WB ) CALL water_balance()

! for GSFLOW simulations
      ELSEIF ( GSFLOW_flag==ON ) THEN

        IF ( Process_flag==RUN ) THEN
          call_modules = gsflow_modflow()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
        ELSE !IF ( Process_flag/=RUN ) THEN

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
          call_modules = soilzone()
          IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)

          call_modules = gsflow_prms2mf()
          IF ( call_modules/=0 ) CALL module_error('gsflow_prms2mf', Arg, call_modules)

          call_modules = gsflow_mf2prms()
          IF ( call_modules/=0 ) CALL module_error('gsflow_mf2prms', Arg, call_modules)
        ENDIF

        call_modules = gsflow_budget()
        IF ( call_modules/=0 ) CALL module_error('gsflow_budget', Arg, call_modules)

        call_modules = gsflow_sum()
        IF ( call_modules/=0 ) CALL module_error('gsflow_sum', Arg, call_modules)
      ENDIF

      IF ( MapOutON_OFF>0 ) THEN
        call_modules = map_results()
        IF ( call_modules/=0 ) CALL module_error('map_results', Arg, call_modules)
      ENDIF

      IF ( Subbasin_flag==ON ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 ) CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( NhruOutON_OFF>OFF ) CALL nhru_summary()

      IF ( NsubOutON_OFF==ON ) CALL nsub_summary()

      IF ( BasinOutON_OFF==ON ) CALL basin_summary()

      IF ( NsegmentOutON_OFF>OFF ) CALL nsegment_summary()

      IF ( CsvON_OFF>OFF .AND. Model==PRMS ) CALL prms_summary()

      IF ( Process_flag==RUN ) THEN
        RETURN
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Model==PRMS ) THEN
          CALL DATE_AND_TIME(VALUES=Elapsed_time_end)
          Execution_time_end = Elapsed_time_end(5)*3600 + Elapsed_time_end(6)*60 + &
     &                         Elapsed_time_end(7) + Elapsed_time_end(8)*0.001
          Elapsed_time = Execution_time_end - Execution_time_start
          Elapsed_time_minutes = INT(Elapsed_time/60.0)
          IF ( Print_debug>DEBUG_less ) THEN
            PRINT 9001
            PRINT 9003, 'start', (Elapsed_time_start(i),i=1,3), (Elapsed_time_start(i),i=5,7)
            PRINT 9003, 'end  ', (Elapsed_time_end(i),i=1,3), (Elapsed_time_end(i),i=5,7)
            PRINT '(A,I5,A,F6.2,A,/)', 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                 Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
          ENDIF
          IF ( Print_debug>DEBUG_minimum ) &
     &         WRITE ( PRMS_output_unit,'(A,I5,A,F6.2,A,/)') 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                                       Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
        ENDIF
        IF ( Print_debug>DEBUG_minimum ) CLOSE ( PRMS_output_unit )
        IF ( Save_vars_to_file==ON ) CLOSE ( Restart_outunit )
        STOP 0
      ELSEIF ( Process_flag==DECL ) THEN
        IF ( Print_debug>DEBUG_minimum ) THEN
          PRINT '(A)', EQULS(:62)
          WRITE ( PRMS_output_unit, '(A)' ) EQULS(:62)
        ENDIF
        IF ( Model==25 ) CALL convert_params()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Inputerror_flag==1 ) THEN
          PRINT '(//,A,//,A,/,A,/,A)', '**Fix input errors in your Parameter File to continue**', &
     &          '  Set control parameter parameter_check_flag to 0 after', &
     &          '  all parameter values are valid.'
          PRINT '(/,A,/,A,/,A,/,A,/,A,/)', &
     &          'If input errors are related to paramters used for automated', &
     &          'calibration processes, with CAUTION, set control parameter', &
     &          'parameter_check_flag to 0. After calibration set the', &
     &          'parameter_check_flag to 1 to verify that those calibration', &
     &          'parameters have valid and compatible values.'
        ENDIF
        IF ( Parameter_check_flag==2 ) STOP 0
        IF ( Inputerror_flag==1 ) ERROR STOP ERROR_param
        IF ( Model==25 ) THEN
          CALL convert_params()
          STOP 0
        ENDIF
        IF ( Print_debug>DEBUG_minimum ) &
     &       PRINT 4, 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, End_month, End_day, EQULS
      ENDIF

    4 FORMAT (/, 2(A, I5, 2('/',I2.2)), //, A, /)
 9001 FORMAT (/, 26X, 27('='), /, 26X, 'Normal completion of GSFLOW', /, 26X, 27('='), /)
 9002 FORMAT (//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', /, 74('='))
 9003 FORMAT ('Execution ', A, ' date and time (yyyy/mm/dd hh:mm:ss)', I5, 2('/',I2.2), I3, 2(':',I2.2), /)
 9004 FORMAT (/, 2A)

      END FUNCTION call_modules

!***********************************************************************
!     declare the dimensions
!***********************************************************************
      INTEGER FUNCTION setdims()
      USE PRMS_MODULE
      USE GLOBAL, ONLY: NSTP, NPER, ISSFLG
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: decldim, declfix, call_modules, control_integer_array, control_file_name
      INTEGER, EXTERNAL :: control_string, control_integer, gsflow_modflow
      EXTERNAL :: read_error, PRMS_open_output_file, PRMS_open_input_file, check_module_names, module_error
! Local Variables
      ! Maximum values are no longer limits
! Local Variables
      INTEGER :: idim, iret, j
      INTEGER :: test, mf_timestep
!***********************************************************************
      setdims = 1

      Inputerror_flag = 0

      ! debug print flag:
      ! -1=quiet - reduced screen output (DEBUG_less)
      ! 0=none; 1=water balances; 2=basin;
      ! 4=basin_sum; 5=soltab; 7=soil zone;
      ! 9=snowcomp; 13=cascade; 14=subbasin tree
      IF ( control_integer(Print_debug, 'print_debug')/=0 ) Print_debug = 0
      IF ( Print_debug>DEBUG_less ) PRINT 3
    3 FORMAT (//, 26X, 'U.S. Geological Survey', /, 8X, &
     &        'Coupled Groundwater and Surface-water FLOW model (GSFLOW)', /, &
     &        25X, 'Version 2.2.0 09/01/2020', //, &
     &        '    An integration of the Precipitation-Runoff Modeling System (PRMS)', /, &
     &        '    and the Modular Groundwater Model (MODFLOW-NWT and MODFLOW-2005)', /)

      IF ( control_integer(Parameter_check_flag, 'parameter_check_flag')/=0 ) Parameter_check_flag = 1

      IF ( control_string(Model_mode, 'model_mode')/=0 ) CALL read_error(5, 'model_mode')
      PRMS4_flag = ON
      IF ( Model_mode(:5)=='PRMS5' .OR. Model_mode(:7)=='GSFLOW5' .OR. Model_mode(:7)=='gsflow5' ) PRMS4_flag = OFF
      PRMS_flag = ON
      GSFLOW_flag = OFF
      ! Model (0=GSFLOW; 1=PRMS; 2=MODFLOW)
      IF ( Model_mode(:4)=='PRMS' .OR. Model_mode(:5)=='DAILY' )THEN
        Model = PRMS
      ELSEIF ( Model_mode(:6)=='GSFLOW' .OR. Model_mode(:4)=='    ' .OR. Model_mode(:4)=='gsflow' ) THEN
        Model = GSFLOW
        GSFLOW_flag = ON
      ELSEIF ( Model_mode(:7)=='MODFLOW' .OR. Model_mode(:7)=='modflow' ) THEN
        Model = MODFLOW
        PRMS_flag = OFF
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

      IF ( control_integer(Init_vars_from_file, 'init_vars_from_file')/=0 ) Init_vars_from_file = 0
      IF ( control_integer(Save_vars_to_file, 'save_vars_to_file')/=0 ) Save_vars_to_file = 0

      IF ( Model==MODFLOW ) THEN
! for MODFLOW-only simulations
        Kper_mfo = 1
        mf_timestep = 1
        Process_flag = DECL
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'declare', test)
        Process_flag = INIT
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'initialize', test)
        PRINT *, ' '
        If ( ISSFLG(Kper_mfo) == 1 .and. nper == 1) THEN
        ELSE
          Process_flag = RUN
          DO WHILE ( Kper_mfo<=Nper )
            test = gsflow_modflow()
            IF ( test/=0 ) CALL module_error(MODNAME, 'run', test)
            IF ( mf_timestep==NSTP(Kper_mfo) ) THEN
                Kper_mfo = Kper_mfo + 1
                mf_timestep = 0
            ENDIF
            mf_timestep = mf_timestep + 1
          ENDDO
        ENDIF
        Process_flag = CLEAN
        test = gsflow_modflow()
        IF ( test/=0 ) CALL module_error(MODNAME, 'clean', test)
        STOP
      ENDIF

      ! Open PRMS module output file
      IF ( control_string(Model_output_file, 'model_output_file')/=0 ) CALL read_error(5, 'model_output_file')
      IF ( Print_debug>DEBUG_minimum ) THEN
        CALL PRMS_open_output_file(PRMS_output_unit, Model_output_file, 'model_output_file', 0, iret)
        IF ( iret/=0 ) Inputerror_flag = 1
      ENDIF
      IF ( control_file_name(Model_control_file)/=0 ) CALL read_error(5, 'control_file_name')
      IF ( control_string(Param_file, 'param_file')/=0 ) CALL read_error(5, 'param_file')

      ! Check for restart files
      IF ( Init_vars_from_file>0 ) THEN
        IF ( control_string(Var_init_file, 'var_init_file')/=0 ) CALL read_error(5, 'var_init_file')
        CALL PRMS_open_input_file(Restart_inunit, Var_init_file, 'var_init_file', 1, iret)
        IF ( iret/=0 ) Inputerror_flag = 1
      ENDIF
      IF ( Save_vars_to_file==ON ) THEN
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

      Climate_precip_flag = OFF
      Climate_temp_flag = OFF
      Climate_transp_flag = OFF
      Climate_potet_flag = OFF
      Climate_swrad_flag = OFF

      IF ( Precip_module(:11)=='precip_1sta' .OR. Precip_module(:11)=='precip_prms') THEN
        Precip_flag = precip_1sta_module
      ELSEIF ( Precip_module(:11)=='precip_laps' ) THEN
        Precip_flag = precip_laps_module
      ELSEIF ( Precip_module(:12)=='precip_dist2' ) THEN
        Precip_flag = precip_dist2_module
      ELSEIF ( Precip_module(:8)=='ide_dist' ) THEN
        Precip_flag = ide_dist_module
      ELSEIF ( Precip_module(:11)=='climate_hru' ) THEN
        Precip_flag = climate_hru_module
        Climate_precip_flag = 1
      ELSEIF ( Precip_module(:8)=='xyz_dist' ) THEN
        Precip_flag = xyz_dist_module
      ELSEIF ( Precip_module(:15)=='precip_temp_map' ) THEN
        Precip_flag = precip_map_module
      ELSE
        PRINT '(/,2A)', 'ERROR: invalid precip_module value: ', Precip_module
        Inputerror_flag = 1
      ENDIF
      Precip_combined_flag = 0
      IF ( Precip_flag==precip_1sta_module .OR. Precip_flag==precip_laps_module ) Precip_combined_flag = 1

      IF ( Temp_module(:9)=='temp_1sta' ) THEN
        Temp_flag = temp_1sta_module
      ELSEIF ( Temp_module(:9)=='temp_laps' ) THEN
        Temp_flag = temp_laps_module
      ELSEIF ( Temp_module(:10)=='temp_dist2' ) THEN
        Temp_flag = temp_dist2_module
      ELSEIF ( Temp_module(:8)=='ide_dist' ) THEN
        Temp_flag = ide_dist_module
      ELSEIF ( Temp_module(:11)=='climate_hru' ) THEN
        Temp_flag = climate_hru_module
        Climate_temp_flag = ON
      ELSEIF ( Temp_module(:8)=='xyz_dist' ) THEN
        Temp_flag = xyz_dist_module
      ELSEIF ( Temp_module(:8)=='temp_sta' ) THEN
        Temp_flag = temp_sta_module
      ELSEIF ( Temp_module(:15)=='precip_temp_map' ) THEN
        Temp_flag = temp_map_module
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid temp_module value: ', Temp_module
        Inputerror_flag = 1
      ENDIF
      Temp_combined_flag = OFF
      IF ( Temp_flag==temp_1sta_module .OR. Temp_flag==temp_laps_module .OR. Temp_flag==temp_sta_module ) Temp_combined_flag = 1

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
        Et_flag = potet_jh_module
      ELSEIF ( Et_module(:11)=='potet_hamon' ) THEN
        Et_flag = potet_hamon_module
      ELSEIF ( Et_module(:11)=='climate_hru' ) THEN
        Et_flag = climate_hru_module
        Climate_potet_flag = ON
      ELSEIF ( Et_module(:8)=='potet_hs' ) THEN
        Et_flag = potet_hs_module
      ELSEIF ( Et_module(:12)=='potet_pm_sta' ) THEN
        Et_flag = potet_pm_sta_module
      ELSEIF ( Et_module(:8)=='potet_pm' ) THEN
        Et_flag = potet_pm_module
      ELSEIF ( Et_module(:8)=='potet_pt' ) THEN
        Et_flag = potet_pt_module
      ELSEIF ( Et_module(:9)=='potet_pan' ) THEN
        Et_flag = potet_pan_module
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid et_module value: ', Et_module
        Inputerror_flag = 1
      ENDIF

      ! stream_temp
      IF ( control_integer(Stream_temp_flag, 'stream_temp_flag')/=0 ) Stream_temp_flag = OFF
      ! 0 = CBH File; 1 = specified constant; 2 = Stations
      IF ( control_integer(Strmtemp_humidity_flag, 'strmtemp_humidity_flag')/=0 ) Strmtemp_humidity_flag = OFF

      IF ( control_integer(Snarea_curve_flag, 'snarea_curve_flag')/=0 ) Snarea_curve_flag = OFF
      IF ( control_integer(Soilzone_aet_flag, 'soilzone_aet_flag')/=0 ) Soilzone_aet_flag = OFF

      Humidity_cbh_flag = OFF
      Windspeed_cbh_flag = OFF
      IF ( Et_flag==potet_pm_module .OR. Et_flag==potet_pt_module .OR. &
     &     (Stream_temp_flag==ON .AND. Strmtemp_humidity_flag==OFF) ) Humidity_cbh_flag = ON
      IF ( Et_flag==potet_pm_module ) Windspeed_cbh_flag = ON

      IF ( Srunoff_module(:13)=='srunoff_smidx' ) THEN
        Sroff_flag = smidx_module
      ELSEIF ( Srunoff_module(:13)=='srunoff_carea' ) THEN
        Sroff_flag = carea_module
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid srunoff_module value: ', Srunoff_module
        Inputerror_flag = 1
      ENDIF

      Soilzone_module = 'soilzone'

      IF ( control_integer(Orad_flag, 'orad_flag')/=0 ) Orad_flag = OFF
      IF ( Solrad_module(:8)=='ddsolrad' ) THEN
        Solrad_flag = 1
      ELSEIF ( Solrad_module(:11)=='climate_hru' ) THEN
        Solrad_flag = climate_hru_module
        Climate_swrad_flag = ON
      ELSEIF ( Solrad_module(:8)=='ccsolrad' ) THEN
        Solrad_flag = 2
      ELSE
        PRINT '(/,2A)', 'ERROR, invalid solrad_module value: ', Solrad_module
        Inputerror_flag = 1
      ENDIF

      IF ( control_integer(Snow_cbh_flag, 'snow_cbh_flag')/=0 ) Snow_cbh_flag = 0
      IF ( control_integer(Gwflow_cbh_flag, 'gwflow_cbh_flag')/=0 ) Gwflow_cbh_flag = 0

      Climate_hru_flag = OFF
      IF ( Climate_temp_flag==ON .OR. Climate_precip_flag==ON .OR. Climate_potet_flag==ON .OR. &
     &     Climate_swrad_flag==ON .OR. Climate_transp_flag==ON .OR. &
     &     Humidity_cbh_flag==ON .OR. Windspeed_cbh_flag==ON .OR. &
     &     Gwflow_cbh_flag==1 .OR. Snow_cbh_flag==1 ) Climate_hru_flag = ON

      Muskingum_flag = OFF
      IF ( Strmflow_module(:15)=='strmflow_in_out' ) THEN
        Strmflow_flag = strmflow_in_out_module
      ELSEIF ( Strmflow_module(:14)=='muskingum_lake' ) THEN
        Strmflow_flag = strmflow_muskingum_lake_module
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        PRINT '(/,2A)', 'ERROR, invalid strmflow_module value: ', Strmflow_module
        Inputerror_flag = 1
      ELSEIF ( Strmflow_module(:8)=='strmflow' ) THEN
        Strmflow_flag = strmflow_noroute_module
      ELSEIF ( Strmflow_module(:14)=='muskingum_mann' ) THEN
        Strmflow_flag = strmflow_muskingum_mann_module
        Muskingum_flag = ON
      ELSEIF ( Strmflow_module(:9)=='muskingum' ) THEN
        Strmflow_flag = strmflow_muskingum_module
        Muskingum_flag = ON
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
      IF ( control_integer(Subbasin_flag, 'subbasin_flag')/=0 ) Subbasin_flag = ON
      IF ( decldim('nsub', 0, MAXDIM, 'Number of internal subbasins')/=0 ) CALL read_error(7, 'nsub')

      IF ( control_integer(Dprst_flag, 'dprst_flag')/=0 ) Dprst_flag = OFF
      ! 0 = off, 1 = on, 2 = lauren version
      IF ( control_integer(CsvON_OFF, 'csvON_OFF')/=0 ) CsvON_OFF = OFF

! map results dimensions
      IF ( control_integer(MapOutON_OFF, 'mapOutON_OFF')/=0 ) MapOutON_OFF = OFF
      idim = 0
      IF ( GSFLOW_flag==ON .OR. MapOutON_OFF>0 ) idim = 1
      IF ( decldim('nhrucell', idim, MAXDIM, &
     &     'Number of unique intersections between HRUs and spatial units of a target map for mapped results')/=0 ) &
     &     CALL read_error(7, 'nhrucell')
      IF ( decldim('ngwcell', 0, MAXDIM, &
     &     'Number of spatial units in the target map for mapped results')/=0 ) CALL read_error(7, 'ngwcell')
      IF ( decldim('nreach', idim, MAXDIM, 'Number of reaches on all stream segments')/=0 ) CALL read_error(7, 'nreach')

      IF ( control_integer(Glacier_flag, 'glacier_flag')/=0 ) Glacier_flag = 0
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
      ! if cascade_flag = 2 (CASCADE_HRU_SEGMENT), use hru_segment parameter for cascades, ncascade=ncascdgw=nhru (typical polygon HRUs)
      IF ( control_integer(Cascade_flag, 'cascade_flag')/=0 ) Cascade_flag = CASCADE_NORMAL
      ! if cascadegw_flag = 2 (CASCADEGW_SAME), use same cascades as HRUs
      IF ( control_integer(Cascadegw_flag, 'cascadegw_flag')/=0 ) Cascadegw_flag = CASCADE_NORMAL

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
      IF ( declfix('ndays', MAX_DAYS_PER_YEAR, MAX_DAYS_PER_YEAR, 'Maximum number of days in a year ')/=0 ) &
     &     CALL read_error(7, 'ndays')
      IF ( declfix('nmonths', 12, 12, 'Number of months in a year')/=0 ) CALL read_error(7, 'nmonths')
      IF ( declfix('one', 1, 1, 'Number of values for scaler array')/=0 ) CALL read_error(7, 'one')

      IF ( call_modules('setdims')/=0 ) Inputerror_flag = 1

      IF ( Inputerror_flag==1 ) PRINT '(//,A,/,A,/ )', '**FIX input errors in your Control File to continue**', &
     &     'NOTE: some errors may be due to use of defalut values'

      setdims = Inputerror_flag
      END FUNCTION setdims

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

      Nobs = getdim('nobs')
      IF ( Nobs==-1 ) CALL read_error(6, 'nobs')

      Nevap = getdim('nevap')
      IF ( Nevap==-1 ) CALL read_error(6, 'nevap')

      Ncascade = getdim('ncascade')
      IF ( Ncascade==-1 ) CALL read_error(7, 'ncascade')
      Ncascdgw = getdim('ncascdgw')
      IF ( Ncascdgw==-1 ) CALL read_error(7, 'ncascdgw')
      IF ( Cascade_flag==CASCADE_HRU_SEGMENT ) THEN
        Ncascade = Nhru
        Cascadegw_flag = CASCADEGW_SAME
      ENDIF
      IF ( Cascadegw_flag==CASCADEGW_SAME ) Ncascdgw = Ncascade
      IF ( Ncascade==0 ) Cascade_flag = CASCADE_OFF
      IF ( Ncascdgw==0 .OR. GSFLOW_flag==ON .OR. Model==MODFLOW ) Cascadegw_flag = CASCADEGW_OFF
      IF ( (Cascade_flag>CASCADE_OFF .OR. Cascadegw_flag>CASCADEGW_OFF) .AND. Model/=25 ) THEN ! don't call if model_mode = CONVERT
        Call_cascade = ON
      ELSE
        Call_cascade = OFF
      ENDIF
      IF ( Model==GSFLOW .AND. Call_cascade==OFF ) THEN
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

      Water_use_flag = OFF
      IF ( Nwateruse>0 ) THEN
        IF ( Segment_transferON_OFF==1 .OR. Gwr_transferON_OFF==1 .OR. External_transferON_OFF==1 .OR. &
     &       Dprst_transferON_OFF==1 .OR. Lake_transferON_OFF==1 .OR. Nconsumed>0 .OR. Nwateruse>0 ) Water_use_flag = 1
      ENDIF

      IF ( Segment_transferON_OFF==ON .OR. Gwr_transferON_OFF==ON .OR. External_transferON_OFF==ON .OR. &
     &     Dprst_transferON_OFF==ON .OR. Lake_transferON_OFF==ON .OR. Nconsumed>0 ) THEN
        IF ( Dprst_transferON_OFF==ON .AND. Dprst_flag==OFF ) THEN
          PRINT *, 'ERROR, specified water-use event based dprst input and have dprst inactive'
          Inputerror_flag = 1
        ENDIF
        IF ( Lake_transferON_OFF==ON .AND. Strmflow_flag==strmflow_muskingum_lake_module ) THEN
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
      IF ( Nlake>0 .AND. Strmflow_flag==3 .AND. GSFLOW_flag==OFF ) Lake_route_flag = 1 ! muskingum_lake

      IF ( Stream_temp_flag>0 .AND. Stream_order_flag==0 ) THEN
        PRINT *, 'ERROR, stream temperature computation requires streamflow routing, thus strmflow_module'
        PRINT *, '       must be set to strmflow_in_out, muskingum, muskingum_mann, or muskingum_lake'
        Inputerror_flag = 1
      ENDIF

      IF ( NsubOutON_OFF==1 .AND. Nsub==0 ) THEN
        NsubOutON_OFF = 0
        IF ( Print_debug>DEBUG_less ) PRINT *, 'WARNING, nsubOutON_OFF = 1 and nsub = 0, thus nsub_summary not used'
      ENDIF

      IF ( NsegmentOutON_OFF==1 .AND. Nsegment==0 ) THEN
        NsegmentOutON_OFF = 0
        IF ( Print_debug>DEBUG_less ) PRINT *, 'WARNING, nsegmentOutON_OFF = 1 and nsegment = 0, thus nsegment_summary not used'
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
        Subbasin_flag = ON
        Cascade_flag = CASCADE_NORMAL
        Cascadegw_flag = CASCADE_NORMAL
        Call_cascade = ON
        Stream_order_flag = ON
        Climate_hru_flag = ON
        Lake_route_flag = ON
        Water_use_flag = ON
        Segment_transferON_OFF = ON
        Gwr_transferON_OFF = ON
        External_transferON_OFF = ON
        Dprst_transferON_OFF = ON
        Lake_transferON_OFF = ON
      ENDIF

      END SUBROUTINE check_dimens

!**********************************************************************
!     Module documentation
!**********************************************************************
      SUBROUTINE module_doc()
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: basin, climateflow, prms_time
      INTEGER, EXTERNAL :: cascade, obs, soltab, transp_tindex
      INTEGER, EXTERNAL :: transp_frost, frost_date, routing
      INTEGER, EXTERNAL :: temp_1sta_laps, temp_dist2
      INTEGER, EXTERNAL :: precip_1sta_laps, climate_hru
      INTEGER, EXTERNAL :: precip_dist2, xyz_dist, ide_dist
      INTEGER, EXTERNAL :: ddsolrad, ccsolrad
      INTEGER, EXTERNAL :: potet_pan, potet_jh, potet_hamon, potet_hs, potet_pt, potet_pm
      INTEGER, EXTERNAL :: intcp, snowcomp, gwflow, srunoff, soilzone
      INTEGER, EXTERNAL :: strmflow, subbasin, basin_sum, map_results, strmflow_in_out
      INTEGER, EXTERNAL :: write_climate_hru, muskingum, muskingum_lake
      INTEGER, EXTERNAL :: stream_temp
      EXTERNAL :: nhru_summary, prms_summary, water_balance, nsub_summary, basin_summary, nsegment_summary
      INTEGER, EXTERNAL :: dynamic_param_read, water_use_read, potet_pm_sta, glacr !, setup
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      EXTERNAL :: precip_temp_map
! Local variable
      INTEGER :: test
!**********************************************************************
      test = basin()
      test = cascade()
      test = climateflow()
      test = soltab()
!      test = setup()
      test = prms_time()
      test = obs()
      test = water_use_read()
      test = dynamic_param_read()
      test = temp_1sta_laps()
      test = temp_dist2()
      test = xyz_dist()
      test = ide_dist()
      CALL precip_temp_map()
      test = climate_hru()
      test = precip_1sta_laps()
      test = precip_dist2()
      test = ddsolrad()
      test = ccsolrad()
      test = transp_tindex()
      test = frost_date()
      test = transp_frost()
      test = potet_jh()
      test = potet_hamon()
      test = potet_pan()
      test = potet_hs()
      test = potet_pt()
      test = potet_pm()
      test = potet_pm_sta()
      test = write_climate_hru()
      test = intcp()
      test = snowcomp()
      test = srunoff()
      test = glacr()
      test = soilzone()
      test = gsflow_prms2mf()
      test = gsflow_mf2prms()
      test = gsflow_budget()
      test = gsflow_sum()
      test = gwflow()
      test = routing()
      test = strmflow()
      test = strmflow_in_out()
      test = muskingum()
      test = muskingum_lake()
      test = stream_temp()
      test = basin_sum()
      test = map_results()
      CALL nhru_summary()
      CALL nsub_summary()
      CALL basin_summary()
      CALL nsegment_summary()
      CALL prms_summary()
      CALL water_balance()
      test = subbasin()

      PRINT 9001
 9001 FORMAT (//, ' All available modules have been called.', /, &
     &        ' All parameters have been declared.', /, &
     &        ' Note, no simulation was computed.', /)

      END SUBROUTINE module_doc

!***********************************************************************
!     check module names
!***********************************************************************
      SUBROUTINE check_module_names(Inputerror_flag)
      USE PRMS_MODULE, ONLY: Temp_module, Precip_module, Et_module, Solrad_module, &
     &    Transp_module, Srunoff_module, Strmflow_module
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(INOUT) :: Inputerror_flag
!***********************************************************************
      IF ( Temp_module(:14)=='temp_1sta_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_1sta_prms to temp_1sta'
        Temp_module = 'temp_1sta'
      ELSEIF ( Temp_module(:14)=='temp_laps_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_laps_prms to temp_laps'
        Temp_module = 'temp_laps'
      ELSEIF ( Temp_module(:15)=='temp_dist2_prms' ) THEN
        PRINT *, 'WARNING, deprecated temp_module value, change temp_dist2_prms to temp_dist2'
        Temp_module = 'temp_dist2'
      ELSEIF ( Temp_module(:9)=='temp_2sta' ) THEN
        PRINT *, 'ERROR, module temp_2sta_prms not available, use a different temp_module'
        Inputerror_flag = 1
      ENDIF

      IF ( Precip_module(:11)=='precip_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_prms to precip_1sta'
        Precip_module = 'precip_1sta'
      ELSEIF ( Precip_module(:16)=='precip_laps_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_laps_prms to precip_laps'
        Precip_module = 'precip_laps'
      ELSEIF ( Precip_module(:17)=='precip_dist2_prms' ) THEN
        PRINT *, 'WARNING, deprecated precip_module value, change precip_dist2_prms to precip_dist2'
        Precip_module = 'precip_dist2'
      ENDIF

      IF ( Temp_module(:8)=='ide_dist' .AND. Precip_module(:8)/='ide_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for temp_module,', &
     &        'it also must be specified for precip_module: ', Precip_module
        Inputerror_flag = 1
      ELSEIF ( Precip_module(:8)=='ide_dist' .AND. Temp_module(:8)/='ide_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if ide_dist is specified for precip_module,', &
     &        'it also must be specified for temp_module: ', Temp_module
        Inputerror_flag = 1
      ELSEIF ( Temp_module(:8)=='xyz_dist' .AND. Precip_module(:8)/='xyz_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for temp_module,', &
     &        'it also must be specified for precip_module: ', Precip_module
        Inputerror_flag = 1
      ELSEIF ( Precip_module(:8)=='xyz_dist' .AND. Temp_module(:8)/='xyz_dist') THEN
        PRINT '(/,A,/,2A)', 'ERROR, if xyz_dist is specified for precip_module,', &
     &        'it also must be specified for temp_module: ', Temp_module
        Inputerror_flag = 1
      ENDIF

      IF ( Transp_module(:18)=='transp_tindex_prms' ) THEN
        PRINT *, 'WARNING, deprecated transp_module value, change transp_tindex_prms to transp_tindex'
        Transp_module = 'transp_tindex'
      ENDIF

      IF ( Et_module(:13)=='potet_jh_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_jh_prms to potet_jh'
        Et_module = 'potet_jh'
      ELSEIF ( Et_module(:14)=='potet_pan_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_pan_prms to potet_pan'
        Et_module = 'potet_pan'
      ELSEIF ( Et_module(:15)=='potet_epan_prms' ) THEN
        PRINT *, 'ERROR, deprecated et_module value, change potet_epan_prms to potet_pan'
        Inputerror_flag = 1
      ELSEIF ( Et_module(:20)=='potet_hamon_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_hamon_hru_prms to potet_hamon_hru'
        Et_module = 'potet_hamon'
      ELSEIF ( Et_module(:16)=='potet_hamon_prms' ) THEN
        PRINT *, 'WARNING, deprecated et_module value, change potet_hamon_prms to potet_hamon'
        Et_module = 'potet_hamon'
      ENDIF

      IF ( Solrad_module(:17)=='ddsolrad_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ddsolrad_hru_prms to ddsolrad'
        Solrad_module = 'ddsolrad'
      ELSEIF ( Solrad_module(:17)=='ccsolrad_hru_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ccsolrad_hru_prms to ccsolrad'
        Solrad_module = 'ccsolrad'
      ELSEIF ( Solrad_module(:13)=='ddsolrad_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ddsolrad_prms to ddsolrad'
        Solrad_module = 'ddsolrad'
      ELSEIF ( Solrad_module(:13)=='ccsolrad_prms' ) THEN
        PRINT *, 'WARNING, deprecated solrad_module value, change ccsolrad_prms to ccsolrad'
        Solrad_module = 'ccsolrad'
      ENDIF

      IF ( Srunoff_module(:18)=='srunoff_carea_prms' ) THEN
        PRINT *, 'WARNING, deprecated srunoff_module value, change srunoff_carea_prms to srunoff_carea'
        Srunoff_module = 'srunoff_carea'
      ELSEIF ( Srunoff_module(:18)=='srunoff_smidx_prms' ) THEN
        PRINT *, 'WARNING, deprecated srunoff_module value, change srunoff_smidx_prms to srunoff_smidx'
        Srunoff_module = 'srunoff_smidx'
      ENDIF

      IF ( Strmflow_module(:13)=='strmflow_prms' ) THEN
        PRINT *, 'WARNING, deprecated strmflow_module value, change strmflow_prms to strmflow'
        Strmflow_module = 'strmflow'
      ELSEIF ( Strmflow_module(:13)=='strmflow_lake' ) THEN
        PRINT *, 'ERROR, module strmflow_lake not available, use a different strmflow_module, such as muskingum_lake'
        Inputerror_flag = 1
      ENDIF

      END SUBROUTINE check_module_names

!***********************************************************************
!     call_modules_restart - write or read restart file
!***********************************************************************
      SUBROUTINE call_modules_restart(In_out)
      USE PRMS_MODULE
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart, check_restart_dimen
      ! Functions
      INTRINSIC TRIM
      ! Local Variables
      INTEGER :: nhru_test, dprst_test, nsegment_test, temp_test, et_test, ierr, time_step
      INTEGER :: cascade_test, cascdgw_test, nhrucell_test, nlake_test, transp_test, start_time(6), end_time(6)
      CHARACTER(LEN=MAXCONTROL_LENGTH) :: model_test
      CHARACTER(LEN=11) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Timestep, Nhru, Dprst_flag, Nsegment, Temp_flag, Et_flag, &
     &          Cascade_flag, Cascadegw_flag, Nhrucell, Nlake, Transp_flag, Model_mode
        WRITE ( Restart_outunit ) Starttime, Endtime
      ELSE
        ierr = 0
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) time_step, nhru_test, dprst_test, nsegment_test, temp_test, et_test, &
     &         cascade_test, cascdgw_test, nhrucell_test, nlake_test, transp_test, model_test
        READ ( Restart_inunit ) start_time, end_time
        IF ( Print_debug>DEBUG_minimum ) PRINT 4, EQULS, 'Simulation time period of Restart File:', &
     &       start_time(1), start_time(2), start_time(3), ' -', end_time(1), end_time(2), end_time(3), &
     &       'Last time step of simulation: ', time_step, EQULS
    4   FORMAT (/, A, /, 2(A, I5, 2('/',I2.2)), /, A, I0, /, A, /)
        IF ( TRIM(Model_mode)/=TRIM(model_test) ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model_mode=', model_test
          PRINT *, '       Current model has model_mode=', Model_mode, ' they must be equal'
          ierr = 1
        ENDIF
        CALL check_restart_dimen('nhru', nhru_test, Nhru, ierr)
        CALL check_restart_dimen('nhrucell', nhrucell_test, Nhrucell, ierr)
        CALL check_restart_dimen('nlake', nlake_test, Nlake, ierr)
        IF ( Dprst_flag/=dprst_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with dprst_flag=', dprst_test
          PRINT *, '       Current model has dprst_flag=', Dprst_flag, ' they must be equal'
          ierr = 1
        ENDIF
        IF ( Cascade_flag/=cascade_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with cascade_flag=', cascade_test
          PRINT *, '       Current model has cascade_flag=', Cascade_flag, ' they must be equal'
          ierr = 1
        ENDIF
        IF ( Cascadegw_flag/=cascdgw_test ) THEN
          PRINT *, 'ERROR, Initial Conditions File saved for model with cascadegw_flag=', cascdgw_test
          PRINT *, '       Current model has cascadegw_flag=', Cascadegw_flag, ' they must be equal'
          ierr = 1
        ENDIF
        CALL check_restart_dimen('nsegment', nsegment_test, Nsegment, ierr)
        ! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=temp_sta
        IF ( Temp_flag/=temp_test ) THEN
          IF ( Temp_flag<4 .OR. temp_test<4 ) THEN
            PRINT *, 'ERROR, Initial Conditions File saved for model with different temperature module'
            PRINT *, '       than current model, cannot switch to/from temp_1sta, temp_laps, or temp_dist2'
            ierr = 1
          ENDIF
        ENDIF
        IF ( Et_flag/=et_test ) THEN
          IF ( Et_flag==4 .OR. et_test==4 ) THEN
            PRINT *, 'ERROR, Cannot switch to/from potet_pan module for restart simulations'
            ierr = 1
          ENDIF
        ENDIF
        IF ( Transp_flag/=transp_test ) THEN
          IF ( Transp_flag==1 .OR. transp_test==1 ) THEN
            PRINT *, 'ERROR, Cannot switch to/from transp_tindex module for restart simulations'
            ierr = 1
          ENDIF
        ENDIF
        IF ( ierr==1 ) ERROR STOP ERROR_restart
      ENDIF
      END SUBROUTINE call_modules_restart

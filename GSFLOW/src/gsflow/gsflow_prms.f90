!***********************************************************************
! Defines the computational sequence, valid modules, and dimensions
!***********************************************************************
      MODULE PRMS_MODULE
      IMPLICIT NONE
      INTEGER, PARAMETER :: MAXFILE_LENGTH = 256, MAXCONTROL_LENGTH = 32
      INTEGER, PARAMETER :: MAXDIM = 500
      CHARACTER(LEN=68), PARAMETER :: &
     &  EQULS = '===================================================================='
      CHARACTER(LEN=11), PARAMETER :: MODNAME = 'gsflow_prms'
      CHARACTER(LEN=24), PARAMETER :: PRMS_VERSION = 'Version 5.2.0 06/04/2020'
      CHARACTER(LEN=8), SAVE :: Process, Arg
      ! model_mode
      INTEGER, PARAMETER :: GSFLOW = 0, PRMS = 1, MODFLOW = 2
      INTEGER, PARAMETER :: DOCUMENTATION = 99
      CHARACTER(LEN=80), SAVE :: PRMS_versn
! Dimensions
      INTEGER, SAVE :: Ncascade, Ncascdgw
      INTEGER, SAVE :: Nhru, Nssr, Ngw, Nsub, Nhrucell, Nlake, Ngwcell, Nlake_hrus
      INTEGER, SAVE :: Ntemp, Nrain, Nsol, Nsegment, Ndepl, Nobs, Nevap, Ndeplval
      INTEGER, SAVE :: Nsnow, NLAKES_MF
! Global
      INTEGER, SAVE :: Model, Process_flag, Call_cascade, call_modules
      INTEGER, SAVE :: Start_year, Start_month, Start_day, End_year, End_month, End_day
      INTEGER, SAVE :: Transp_flag, Sroff_flag, Solrad_flag, Et_flag
      INTEGER, SAVE :: Climate_temp_flag, Climate_precip_flag, Climate_potet_flag, Climate_transp_flag
      INTEGER, SAVE :: Lake_route_flag, Nratetbl, Strmflow_flag, Stream_order_flag
      INTEGER, SAVE :: Temp_flag, Precip_flag, Climate_hru_flag, Climate_swrad_flag
      INTEGER, SAVE :: Precip_combined_flag, Temp_combined_flag, Muskingum_flag
      INTEGER, SAVE :: Inputerror_flag, Timestep
      INTEGER, SAVE :: Humidity_cbh_flag, Windspeed_cbh_flag
      INTEGER, SAVE :: Grid_flag, PRMS_flag, GSFLOW_flag
      INTEGER, SAVE :: PRMS_output_unit, Restart_inunit, Restart_outunit
      INTEGER, SAVE :: Dynamic_flag, Water_use_flag, Nwateruse, Nexternal, Nconsumed, Npoigages, Prms_warmup
      INTEGER, SAVE :: Elapsed_time_start(8), Elapsed_time_end(8), Elapsed_time_minutes
      INTEGER, SAVE :: PRMS4_flag, Kper_mfo, Kkstp_mfo, Diversion2soil_flag
      INTEGER, SAVE :: mf_timestep, startday, endday, mf_nowtime, Number_timesteps, Have_lakes
      REAL, SAVE :: Execution_time_start, Execution_time_end, Elapsed_time
      CHARACTER(LEN=80), SAVE :: Version_read_control_file, Version_read_parameter_file
!   Declared Variables
      INTEGER, SAVE :: Kkiter
      REAL, SAVE, ALLOCATABLE :: Hru_ag_irr(:)    !Ag irrigation added to HRU
!   Declared Parameters
      INTEGER, SAVE :: Mxsziter
      INTEGER, SAVE, ALLOCATABLE :: Gvr_cell_id(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_cell_pct(:)
! Precip_flag (1=precip_1sta; 2=precip_laps; 3=precip_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 9=precip_temp_grid
! Temp_flag (1=temp_1sta; 2=temp_laps; 3=temp_dist2; 5=ide_dist; 6=xyz_dist; 7=climate_hru; 8=temp_sta; 9=precip_temp_grid
! Control parameters
      INTEGER, SAVE :: Starttime(6), Endtime(6), Modflow_time_zero(6)
      INTEGER, SAVE :: Print_debug, MapOutON_OFF, CsvON_OFF, Dprst_flag, Subbasin_flag, Parameter_check_flag
      INTEGER, SAVE :: Init_vars_from_file, Save_vars_to_file, Orad_flag, Cascade_flag, Cascadegw_flag
      INTEGER, SAVE :: NhruOutON_OFF, Gwr_swale_flag, NsubOutON_OFF, BasinOutON_OFF, NsegmentOutON_OFF
      INTEGER, SAVE :: Stream_temp_flag, Strmtemp_humidity_flag, Stream_temp_shade_flag
      INTEGER, SAVE :: Snarea_curve_flag, Soilzone_aet_flag, statsON_OFF
      INTEGER, SAVE :: Snow_cbh_flag, Gwflow_cbh_flag, Frozen_flag
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
      PROGRAM gsflow
      USE PRMS_MODULE
      IMPLICIT NONE
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
      INTEGER, EXTERNAL :: stream_temp
      EXTERNAL :: module_error, print_module, PRMS_open_output_file, precip_temp_grid
      EXTERNAL :: call_modules_restart, water_balance, basin_summary, nsegment_summary
      EXTERNAL :: prms_summary, nhru_summary, module_doc, convert_params, read_error, nsub_summary
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      INTEGER, EXTERNAL :: declparam, getparam, gsfdecl, gsfinit, gsfrun, gsfclean
      EXTERNAL :: setdims, declvar_int, declvar_real, check_parameters, read_prms_data_file, error_stop
! Local Variables
      INTEGER :: i, iret, nc
!***********************************************************************
      call_modules = 1

! (0=run, 1=declare, 2=init, 3=clean, 4=setdims)
      Process_flag = -1

      DO
      IF ( Process_flag==0 ) THEN
        Arg = 'run'
      ELSEIF ( Process_flag==-1 ) THEN
        Arg = 'setdims'
        Process_flag = 4
          CALL DATE_AND_TIME(VALUES=Elapsed_time_start)
          Execution_time_start = Elapsed_time_start(5)*3600 + Elapsed_time_start(6)*60 + &
     &                           Elapsed_time_start(7) + Elapsed_time_start(8)*0.001
          PRMS_versn = 'gsflow_prms.f90 2020-06-17 10:00:00Z'
        ! Note, MODFLOW-only doesn't leave setdims
        CALL setdims()
      ELSEIF ( Process_flag==1 ) THEN  ! after setdims finished
        Arg = 'decl'
      ELSEIF ( Process_flag==2 ) THEN ! after declare finished
        Arg = 'init'
      ELSE ! after last timestep
        Arg = 'clean'
      ENDIF
      Process = Arg

      IF ( Process_flag==0 ) THEN
      ELSEIF ( Process_flag==1 ) THEN
        IF ( Print_debug>-2 ) THEN
          PRINT 10, PRMS_VERSION
          WRITE ( PRMS_output_unit, 10 ) PRMS_VERSION
        ENDIF
  10  FORMAT (/, 15X, 'Precipitation-Runoff Modeling System (PRMS)', /, 23X, A)
      15  FORMAT (/, 8X, 'Process',  12X, 'Available Modules', /, 68('-'), /, &
     &        '  Basin Definition: basin', /, &
     &        '    Cascading Flow: cascade', /, &
     &        '  Time Series Data: obs, water_use_read, dynamic_param_read', /, &
     &        '   Potet Solar Rad: soltab', /, &
     &        '  Temperature Dist: temp_1sta, temp_laps, temp_dist2, climate_hru', /, &
     &        '       Precip Dist: precip_1sta, precip_laps, precip_dist2,', /, &
     &        '                    climate_hru', /, &
     &        'Temp & Precip Dist: xyz_dist, ide_dist, precip_temp_grid', /, &
     &        '    Solar Rad Dist: ccsolrad, ddsolrad, climate_hru', /, &
     &        'Transpiration Dist: transp_tindex, climate_hru, transp_frost', /, &
     &        '      Potential ET: potet_hamon, potet_jh, potet_pan, climate_hru,', /, &
     &        '                    potet_hs, potet_pt, potet_pm, potet_pm_sta', /, &
     &        '      Interception: intcp', /, &
     &        '     Snow Dynamics: snowcomp', /, &
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
  16  FORMAT (//, 4X, 'Active modules listed in the order in which they are called', //, 8X, 'Process', 19X, &
     &        'Module', 16X, 'Version Date', /, A)

        Diversion2soil_flag = 0
        IF ( GSFLOW_flag==1 ) THEN
          call_modules = gsfdecl()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF

        IF ( Print_debug>-2 ) THEN
          PRINT 15
          PRINT 9002
          WRITE ( PRMS_output_unit, 15 )
          PRINT 16, EQULS
          WRITE ( PRMS_output_unit, 16 ) EQULS
        ENDIF
        CALL print_module(PRMS_versn, 'GSFLOW Computation Order    ', 90)
        CALL print_module(Version_read_control_file, 'Read Control File           ', 90)
        CALL print_module(Version_read_parameter_file, 'Read Parameter File         ', 90)
        CALL read_prms_data_file()

        IF ( GSFLOW_flag==1 .OR. Model==DOCUMENTATION ) THEN
          CALL declvar_int(MODNAME, 'KKITER', 'one', 1, &
     &         'Current iteration in GSFLOW simulation', 'none', KKITER)
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
        ENDIF

        Have_lakes = 0 ! set for modes when MODFLOW is not active
        Kkiter = 1 ! set for PRMS-only mode

        Timestep = 0
        IF ( Init_vars_from_file>0 ) CALL call_modules_restart(1)
        IF ( Model==DOCUMENTATION ) THEN
          Init_vars_from_file = 0 ! make sure this is set so all variables and parameters are declared
          CALL module_doc()
          STOP 0
        ENDIF

      ELSEIF ( Process_flag==2 ) THEN ! after declare finished

        Grid_flag = 0
        IF ( Nhru==Nhrucell ) Grid_flag = 1
        IF ( GSFLOW_flag==1 ) THEN
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
              IF ( Print_debug>-1 ) THEN
                PRINT *, 'WARNING, HRUs are assumed to be numbered from upper left corner'
                PRINT *, '         gvr_cell_id values are set to 1 through nhru'
              ENDIF
              DO i = 1, Nhrucell
                Gvr_cell_id(i) = i
              ENDDO
            ELSE
              CALL error_stop('gvr_cell_id must be specified')
            ENDIF
          ENDIF
          call_modules = gsfinit()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
          IF ( Diversion2soil_flag==1 ) THEN
            ! Allocate variable for adding irrigation water to HRU from AG Package
            ALLOCATE ( Hru_ag_irr(Nhru) )
            CALL declvar_real(MODNAME, 'hru_ag_irr', 'nhru', Nhru, &
     &           'Irrigation added to soilzone from MODFLOW wells', 'inches', Hru_ag_irr)
            Hru_ag_irr = 0.0
          ENDIF
          IF ( Have_lakes==1 .AND. Nlake/=NLAKES_MF ) THEN
            PRINT *, 'ERROR, NLAKES not equal to Nlake'
            PRINT *, '       NLAKES=', NLAKES_MF, '; Nlake=', Nlake
            ERROR STOP -1
          ENDIF
!          IF ( Nsegment/=NSS_MF ) THEN
!            PRINT *, 'ERROR, NSS not equal to nsegment'
!            PRINT *, '       NSS=', NSS_MF, '; nsegment=', Nsegment
!            ERROR STOP -1
!          ENDIF
        ENDIF

        nc = numchars(Model_control_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using Control File: ', Model_control_file(:nc)
        IF ( Print_debug>-2 ) WRITE ( PRMS_output_unit, 9004 ) 'Using Control File: ', Model_control_file(:nc)

        nc = numchars(Param_file)
        IF ( Print_debug>-1 ) PRINT 9004, 'Using Parameter File: ', Param_file(:nc)
        IF ( Print_debug>-2 ) WRITE ( PRMS_output_unit, 9004 ) 'Using Parameter File: ', Param_file(:nc)

        IF ( Init_vars_from_file>0 ) THEN
          nc = numchars(Var_init_file)
          IF ( Print_debug>-1 ) PRINT 9004, 'Using var_init_file: ', Var_init_file(:nc)
        ENDIF
        IF ( Save_vars_to_file==1 ) THEN
          nc = numchars(Var_save_file)
          IF ( Print_debug>-1 ) PRINT 9004, 'Using var_save_file: ', Var_save_file(:nc)
        ENDIF

        IF ( Print_debug>-2 ) THEN
          nc = numchars(Model_output_file)
          PRINT 9004, 'Writing PRMS Water Budget File: ', Model_output_file(:nc)
        ENDIF

      ELSEIF ( Process_flag==3 ) THEN
        Arg = 'clean'
        Process_flag = 3

        IF ( Init_vars_from_file>0 ) CLOSE ( Restart_inunit )
        IF ( Save_vars_to_file==1 ) THEN
          CALL PRMS_open_output_file(Restart_outunit, Var_save_file, 'var_save_file', 1, iret)
          IF ( iret/=0 ) ERROR STOP -1
          CALL call_modules_restart(0)
        ENDIF
        IF ( GSFLOW_flag==1 ) THEN
          call_modules = gsfclean()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)
        ENDIF
      ENDIF

! All modules must be called for setdims, declare, initialize, and cleanup
      IF ( Process_flag/=0 .AND. PRMS_flag==1 ) THEN
        call_modules = basin()
        IF ( call_modules/=0 ) CALL module_error('basin', Arg, call_modules)

        IF ( Call_cascade==1 ) THEN
          call_modules = cascade()
          IF ( call_modules/=0 ) CALL module_error('cascade', Arg, call_modules)
        ENDIF

        call_modules = climateflow()
        IF ( call_modules/=0 ) CALL module_error('climateflow', Arg, call_modules)

        call_modules = soltab()
        IF ( call_modules/=0 ) CALL module_error('soltab', Arg, call_modules)

        call_modules = obs()
        IF ( call_modules/=0 ) CALL module_error('obs', Arg, call_modules)

!        call_modules = setup()
!        IF ( call_modules/=0 ) CALL module_error('setup', Arg, call_modules)
      ENDIF

      call_modules = prms_time()
      IF ( call_modules/=0 ) CALL module_error('prms_time', Arg, call_modules)

      IF ( Water_use_flag==1 ) THEN
        call_modules = water_use_read()
        IF ( call_modules/=0 ) CALL module_error('water_use_read', Arg, call_modules)
      ENDIF

      IF ( Dynamic_flag==1 ) THEN
        call_modules = dynamic_param_read()
        IF ( call_modules/=0 ) CALL module_error('dynamic_param_read', Arg, call_modules)
      ENDIF

      IF ( Climate_hru_flag==1 ) THEN
        call_modules = climate_hru()
        IF ( call_modules/=0 ) CALL module_error('climate_hru', Arg, call_modules)
      ENDIF

      IF ( Climate_temp_flag==0 ) THEN
        IF ( Temp_combined_flag==1 ) THEN
          call_modules = temp_1sta_laps()
        ELSEIF ( Temp_flag==6 ) THEN
          call_modules = xyz_dist()
        ELSEIF ( Temp_flag==3 ) THEN
          call_modules = temp_dist2()
        ELSEIF ( Temp_flag==5 ) THEN
          call_modules = ide_dist()
        ELSE !IF ( Temp_flag==9 ) THEN
          CALL precip_temp_grid()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Temp_module, Arg, call_modules)
      ENDIF

      IF ( Climate_precip_flag==0 ) THEN
        IF ( Precip_combined_flag==1 ) THEN
          call_modules = precip_1sta_laps()
        ELSEIF ( Precip_flag==3 ) THEN
          call_modules = precip_dist2()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Precip_module, Arg, call_modules)
      ENDIF

      IF ( Model==26 ) THEN
        IF ( Process_flag==0 ) CYCLE
      ENDIF

! frost_date is a pre-process module
      IF ( Model==29 ) THEN
        call_modules = frost_date()
        IF ( call_modules/=0 ) CALL module_error('frost_date', Arg, call_modules)
        IF ( Process_flag==0 ) CYCLE
        IF ( Process_flag==3 ) STOP 0
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
        IF ( Process_flag==0 ) CYCLE
      ENDIF

      IF ( Climate_potet_flag==0 ) THEN
        IF ( Et_flag==1 ) THEN
          call_modules = potet_jh()
        ELSEIF ( Et_flag==2 ) THEN
          call_modules = potet_hamon()
        ELSEIF ( Et_flag==4 ) THEN
          call_modules = potet_pan()
        ELSEIF ( Et_flag==5 ) THEN
          call_modules = potet_pt()
        ELSEIF ( Et_flag==6 ) THEN
          call_modules = potet_pm_sta()
        ELSEIF ( Et_flag==11 ) THEN
          call_modules = potet_pm()
        ELSE !IF ( Et_flag==10 ) THEN
          call_modules = potet_hs()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Et_module, Arg, call_modules)
      ENDIF

      IF ( Model==24 ) THEN
        call_modules = write_climate_hru()
        IF ( call_modules/=0 ) CALL module_error('write_climate_hru', Arg, call_modules)
        IF ( Process_flag==0 ) CYCLE
      ENDIF

      IF ( Model==27 ) THEN
        IF ( Process_flag==0 ) CYCLE
      ENDIF

      call_modules = intcp()
      IF ( call_modules/=0 ) CALL module_error('intcp', Arg, call_modules)

      ! rsr, need to do something if snow_cbh_flag=1
      call_modules = snowcomp()
      IF ( call_modules/=0 ) CALL module_error('snowcomp', Arg, call_modules)

! for PRMS-only simulations
      IF ( Model==PRMS ) THEN
        call_modules = srunoff()
        IF ( call_modules/=0 ) CALL module_error(Srunoff_module, Arg, call_modules)

        call_modules = soilzone()
        IF ( call_modules/=0 ) CALL module_error(Soilzone_module, Arg, call_modules)

        ! rsr, need to do something if gwflow_cbh_flag=1
        call_modules = gwflow()
        IF ( call_modules/=0 ) CALL module_error('gwflow', Arg, call_modules)

        IF ( Stream_order_flag==1 ) THEN
          call_modules = routing()
          IF ( call_modules/=0 ) CALL module_error('routing', Arg, call_modules)
        ENDIF

        IF ( Strmflow_flag==1 ) THEN
          call_modules = strmflow()
        ELSEIF ( Muskingum_flag==1 ) THEN ! muskingum = 4; muskingum_mann = 7
          call_modules = muskingum()
        ELSEIF ( Strmflow_flag==5 ) THEN
          call_modules = strmflow_in_out()
        ELSEIF ( Strmflow_flag==3 ) THEN
          call_modules = muskingum_lake()
        ENDIF
        IF ( call_modules/=0 ) CALL module_error(Strmflow_module, Arg, call_modules)

        IF ( Stream_temp_flag==1 ) call_modules = stream_temp()

        IF ( Print_debug>-2 ) THEN
          call_modules = basin_sum()
          IF ( call_modules/=0 ) CALL module_error('basin_sum', Arg, call_modules)
        ENDIF

        IF ( Print_debug==1 ) CALL water_balance()

! for GSFLOW simulations
      ELSEIF ( GSFLOW_flag==1 ) THEN

        IF ( Process_flag==0 ) THEN
          call_modules = gsfrun()
          IF ( call_modules/=0 ) CALL module_error(MODNAME, Arg, call_modules)

! The following modules are in the MODFLOW iteration loop
! (contained in gsflow_modflow.f).
! They still need to be called for declare, initialize and cleanup
        ELSE !IF ( Process_flag/=0 ) THEN

! SOILZONE for GSFLOW is in the MODFLOW iteration loop,
! only call for declare, initialize, and cleanup.
          call_modules = srunoff()
          IF ( call_modules/=0 ) CALL module_error(Srunoff_module, Arg, call_modules)

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

      IF ( Subbasin_flag==1 ) THEN
        call_modules = subbasin()
        IF ( call_modules/=0 ) CALL module_error('subbasin', Arg, call_modules)
      ENDIF

      IF ( NhruOutON_OFF>0 ) CALL nhru_summary()

      IF ( NsubOutON_OFF==1 ) CALL nsub_summary()

      IF ( BasinOutON_OFF==1 ) CALL basin_summary()

      IF ( NsegmentOutON_OFF>0 ) CALL nsegment_summary()

      IF ( CsvON_OFF>0 .AND. Model==PRMS ) CALL prms_summary()

      IF ( Process_flag==0 ) THEN
        IF ( Timestep==Number_timesteps ) Process_flag = 3
      ELSEIF ( Process_flag==4 ) THEN
        Process_flag = 1  ! next is declare
      ELSEIF ( Process_flag==3 ) THEN
        IF ( Model==PRMS ) THEN
          CALL DATE_AND_TIME(VALUES=Elapsed_time_end)
          Execution_time_end = Elapsed_time_end(5)*3600 + Elapsed_time_end(6)*60 + &
     &                         Elapsed_time_end(7) + Elapsed_time_end(8)*0.001
          Elapsed_time = Execution_time_end - Execution_time_start
          Elapsed_time_minutes = INT(Elapsed_time/60.0)
          IF ( Print_debug>-1 ) THEN
            PRINT 9001
            PRINT 9003, 'start', (Elapsed_time_start(i),i=1,3), (Elapsed_time_start(i),i=5,7)
            PRINT 9003, 'end  ', (Elapsed_time_end(i),i=1,3), (Elapsed_time_end(i),i=5,7)
            PRINT '(A,I5,A,F6.2,A,/)', 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                 Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
          ENDIF
          IF ( Print_debug>-2 ) &
     &         WRITE ( PRMS_output_unit,'(A,I5,A,F6.2,A,/)') 'Execution elapsed time', Elapsed_time_minutes, ' minutes', &
     &                                                       Elapsed_time - Elapsed_time_minutes*60.0, ' seconds'
        ENDIF
        IF ( Print_debug>-2 ) CLOSE ( PRMS_output_unit )
        IF ( Save_vars_to_file>0 ) CLOSE ( Restart_outunit )
        STOP 0
      ELSEIF ( Process_flag==1 ) THEN
        IF ( Print_debug>-2 ) THEN
          PRINT '(A)', EQULS
          WRITE ( PRMS_output_unit, '(A)' ) EQULS
        ENDIF
        CALL read_parameter_file_params()
        IF ( Print_debug>-2 ) PRINT '(A)', EQULS
        IF ( Model==25 ) CALL convert_params()
        Process_flag = 2  ! next is init
      ELSEIF ( Process_flag==2 ) THEN
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
        IF ( Inputerror_flag==1 ) ERROR STOP -1
        IF ( Model==25 ) THEN
          CALL convert_params()
          STOP 0
        ENDIF
        IF ( Print_debug>-2 ) &
     &       PRINT 4, 'Simulation time period:', Start_year, Start_month, Start_day, ' -', End_year, End_month, End_day, EQULS
        Process_flag = 0  ! next is run
      ENDIF

      ENDDO

    4 FORMAT (/, 2(A, I5, 2('/',I2.2)), //, A, /)
 9001 FORMAT (/, 26X, 27('='), /, 26X, 'Normal completion of GSFLOW', /, 26X, 27('='), /)
 9002 FORMAT (//, 74('='), /, 'Please give careful consideration to fixing all ERROR and WARNING messages', /, 74('='))
 9003 FORMAT ('Execution ', A, ' date and time (yyyy/mm/dd hh:mm:ss)', I5, 2('/',I2.2), I3, 2(':',I2.2), /)
 9004 FORMAT (/, 2A)

      END PROGRAM gsflow

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
      INTEGER, EXTERNAL :: dynamic_param_read, water_use_read, potet_pm_sta !, setup
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms, gsflow_budget, gsflow_sum
      EXTERNAL :: precip_temp_grid
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
      CALL precip_temp_grid()
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
        IF ( Print_debug>-2 ) PRINT 4, EQULS, 'Simulation time period of Restart File:', &
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
        IF ( ierr==1 ) ERROR STOP -3
      ENDIF
      END SUBROUTINE call_modules_restart

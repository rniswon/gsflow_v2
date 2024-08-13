!***********************************************************************
! Read and makes available land surface and soil dynamic parameters
! These parameters can be input for any date within the simulation time
! period. Associated states with each parameter are adjusted.
!***********************************************************************
      MODULE PRMS_DYNAMIC_SOIL_PARAM_READ
        USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Time Series Data'
        character(len=*), parameter :: MODNAME = 'dynamic_soil_param_read'
        character(len=*), parameter :: Version_dynamic_soil_param_read = '2024-08-09'
        INTEGER, SAVE :: Imperv_frac_unit, Imperv_next_yr, Imperv_next_mo, Imperv_next_day, Imperv_frac_flag
        INTEGER, SAVE :: Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day, Imperv_stor_unit
        INTEGER, SAVE :: Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day, Soil_rechr_unit
        INTEGER, SAVE :: Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day, Soil_moist_unit
        INTEGER, SAVE :: Ag_soil_rechr_next_yr, Ag_soil_rechr_next_mo, Ag_soil_rechr_next_day, Ag_soil_rechr_unit
        INTEGER, SAVE :: Ag_soil_moist_next_yr, Ag_soil_moist_next_mo, Ag_soil_moist_next_day, Ag_soil_moist_unit
        INTEGER, SAVE :: Ag_frac_next_yr, Ag_frac_next_mo, Ag_frac_next_day, Ag_frac_unit, ag_frac_flag
        INTEGER, SAVE :: Ag_soilmoist_flag, Ag_soilrechr_flag
        INTEGER, SAVE :: Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day, Dprst_depth_unit, Dprst_depth_flag
        INTEGER, SAVE :: Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day, Dprst_frac_unit, Dprst_frac_flag
        INTEGER, SAVE :: Soilmoist_flag, Soilrechr_flag, Output_unit
        INTEGER, SAVE, ALLOCATABLE :: Updated_hrus(:)
        REAL, SAVE, ALLOCATABLE :: Temp(:), temp_imperv_frac(:), temp_sm_frac(:), temp_dprst_frac(:), temp_ag_frac(:)
        REAL, SAVE, ALLOCATABLE :: Soil_rechr_max_frac(:)
! Control Parameters
        CHARACTER(LEN=MAXFILE_LENGTH) :: imperv_frac_dynamic, imperv_stor_dynamic, dprst_depth_dynamic, dprst_frac_dynamic
        CHARACTER(LEN=MAXFILE_LENGTH) :: soilmoist_dynamic, soilrechr_dynamic, dynamic_soil_param_log_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: ag_soilmoist_dynamic, ag_soilrechr_dynamic, ag_frac_dynamic
      END MODULE PRMS_DYNAMIC_SOIL_PARAM_READ

!***********************************************************************
!     Main dynamic parameter routine
!***********************************************************************
      INTEGER FUNCTION dynamic_soil_param_read()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_DYNAMIC_SOIL_PARAM_READ, ONLY: MODDESC, MODNAME, Version_dynamic_soil_param_read
      use prms_utils, only: print_module
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: dynsoilparamrun, dynsoilparaminit
!***********************************************************************
      dynamic_soil_param_read = 0

      IF ( Process_flag==RUN ) THEN
        dynamic_soil_param_read = dynsoilparamrun()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME(:18), Version_dynamic_soil_param_read)
      ELSEIF ( Process_flag==INIT ) THEN
        dynamic_soil_param_read = dynsoilparaminit()
      ENDIF

      END FUNCTION dynamic_soil_param_read

!***********************************************************************
!     dynsoilparaminit - open files, read to start time, initialize flags and arrays
!***********************************************************************
      INTEGER FUNCTION dynsoilparaminit()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, ERROR_dynamic, DEBUG_minimum
      use PRMS_CONTROL_FILE, only: control_string
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Start_year, Start_month, Start_day, &
     &    Dyn_imperv_flag, Dyn_dprst_flag, Dyn_soil_flag, Dprst_flag, PRMS4_flag, Dyn_ag_soil_flag, AG_flag, Dyn_ag_frac_flag
      USE PRMS_DYNAMIC_SOIL_PARAM_READ
      use prms_utils, only: error_stop, find_current_file_time, find_header_end, PRMS_open_output_file, read_error, numchars
      IMPLICIT NONE
! Local Variables
      INTEGER :: year, month, day, istop, ierr
!***********************************************************************
      dynsoilparaminit = 0

      year = Start_year
      month = Start_month
      day = Start_day

      ! dyn_imperv_flag: 1 or 3, hru_percent_imperv; 2 or 3, imperv_stor_max
      ! dyn_dprst_flag: 1 or 3, dprst_frac; 2 or 3, dprst_depth_avg
      ! dyn_soil_flag: 1 or 3, soil_moist_max; 2 or 3, soil_rechr_max
      ! dyn_ag_soil_flag: 1 or 3, ag_soil_moist_max; 2 or 3, ag_soil_rechr_max
      ! dyn_ag_frac_flag: 1 = ag_frac
      ALLOCATE ( Temp(Nhru), Updated_hrus(Nhru) )
      IF ( Dyn_imperv_flag==1 .OR. Dyn_imperv_flag==3 .OR. Dyn_dprst_flag==1 .OR. Dyn_dprst_flag==3 .OR. &
           Dyn_ag_frac_flag==ACTIVE ) THEN
        ALLOCATE ( temp_imperv_frac(Nhru) )
        IF ( Dprst_flag==ACTIVE ) ALLOCATE ( temp_dprst_frac(Nhru) )
        IF ( AG_flag==ACTIVE ) ALLOCATE ( Temp_ag_frac(Nhru) )
      ENDIF

      Imperv_frac_flag = OFF
      istop = 0
      ierr = 0
      IF ( Dyn_imperv_flag==1 .OR. Dyn_imperv_flag==3 ) THEN
        IF ( control_string(imperv_frac_dynamic, 'imperv_frac_dynamic')/=0 ) CALL read_error(5, 'imperv_frac_dynamic')
        CALL find_header_end(Imperv_frac_unit, imperv_frac_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Imperv_frac_unit, year, month, day, Imperv_next_yr, Imperv_next_mo, Imperv_next_day)
          Imperv_frac_flag = ACTIVE
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_imperv_flag>1 ) THEN
        IF ( control_string(imperv_stor_dynamic, 'imperv_stor_dynamic')/=0 ) CALL read_error(5, 'imperv_stor_dynamic')
        CALL find_header_end(Imperv_stor_unit, imperv_stor_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Imperv_stor_unit, year, month, day, &
     &                                Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Dprst_frac_flag = OFF
      Dprst_depth_flag = OFF
      IF ( Dprst_flag==ACTIVE ) THEN
        IF ( Dyn_dprst_flag==1 .OR. Dyn_dprst_flag==3 ) THEN
          IF ( control_string(dprst_frac_dynamic, 'dprst_frac_dynamic')/=0 ) CALL read_error(5, 'dprst_frac_dynamic')
          CALL find_header_end(Dprst_frac_unit, dprst_frac_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Dprst_frac_unit, year, month, day, &
     &                                  Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day)
            Dprst_frac_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF

        IF ( Dyn_dprst_flag==2 .OR. Dyn_dprst_flag==3 ) THEN
          IF ( control_string(dprst_depth_dynamic, 'dprst_depth_dynamic')/=0 ) CALL read_error(5, 'dprst_depth_dynamic')
          CALL find_header_end(Dprst_depth_unit, dprst_depth_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Dprst_depth_unit, year, month, day, &
     &                                  Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day)
            Dprst_depth_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      ag_frac_flag = OFF
      IF ( AG_flag==ACTIVE ) THEN
        IF ( Dyn_ag_frac_flag==ACTIVE ) THEN
          IF ( control_string(ag_frac_dynamic, 'ag_frac_dynamic')/=0 ) CALL read_error(5, 'ag_frac_dynamic')
          CALL find_header_end(Ag_frac_unit, ag_frac_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Ag_frac_unit, year, month, day, &
     &                                  Ag_frac_next_yr, Ag_frac_next_mo, Ag_frac_next_day)
            ag_frac_flag = ACTIVE
          ELSE
!              print *, ierr, 'ag_frac header'
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Soilrechr_flag = OFF
      IF ( Dyn_soil_flag>1 ) THEN
        Soilrechr_flag = ACTIVE
        IF ( PRMS4_flag==OFF ) ALLOCATE ( Soil_rechr_max_frac(Nhru) )
        IF ( control_string(soilrechr_dynamic, 'soilrechr_dynamic')/=0 ) CALL read_error(5, 'soilrechr_dynamic')
        CALL find_header_end(Soil_rechr_unit, soilrechr_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Soil_rechr_unit, year, month, day, &
     &                                Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Soilmoist_flag = OFF
      IF ( Dyn_soil_flag==1 .OR. Dyn_soil_flag==3 ) THEN
        Soilmoist_flag = ACTIVE
        IF ( control_string(soilmoist_dynamic, 'soilmoist_dynamic')/=0 ) CALL read_error(5, 'soilmoist_dynamic')
        CALL find_header_end(Soil_moist_unit, soilmoist_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Soil_moist_unit, year, month, day, &
     &                                Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Ag_soilrechr_flag = OFF
      IF ( Dyn_ag_soil_flag>1 ) THEN
        Ag_soilrechr_flag = ACTIVE
        IF ( control_string(ag_soilrechr_dynamic, 'ag_soilrechr_dynamic')/=0 ) CALL read_error(5, 'ag_soilrechr_dynamic')
        CALL find_header_end(Ag_soil_rechr_unit, ag_soilrechr_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Ag_soil_rechr_unit, year, month, day, &
     &                                Ag_soil_rechr_next_yr, Ag_soil_rechr_next_mo, Ag_soil_rechr_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Ag_soilmoist_flag = OFF
      IF ( Dyn_ag_soil_flag==1 .OR. Dyn_ag_soil_flag==3 ) THEN
        Ag_soilmoist_flag = ACTIVE
        IF ( control_string(ag_soilmoist_dynamic, 'ag_soilmoist_dynamic')/=0 ) CALL read_error(5, 'ag_soilmoist_dynamic')
        CALL find_header_end(Ag_soil_moist_unit, Ag_soilmoist_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Ag_soil_moist_unit, year, month, day, &
     &                                Ag_soil_moist_next_yr, Ag_soil_moist_next_mo, Ag_soil_moist_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Print_debug>DEBUG_minimum ) THEN
        IF ( control_string(dynamic_soil_param_log_file, 'dynamic_soil_param_log_file')/=0 ) &
             CALL read_error(5, 'dynamic_soil_param_log_file')
        CALL PRMS_open_output_file(Output_unit, dynamic_soil_param_log_file, 'dynamic_soil_param_log_file', 0, ierr)
        PRINT '(/,A,/,A,/)', 'A summary of dynamic parameter events are written to file:', &
     &                       dynamic_soil_param_log_file(:numchars(dynamic_soil_param_log_file))
      ENDIF

      IF ( istop==1 .OR. ierr/=0 ) CALL error_stop('in dynamic_soil_param_read initialize procedure', ERROR_dynamic)

      END FUNCTION dynsoilparaminit

!***********************************************************************
!     dynsoilparamrun - Read and set dynamic parameters
!***********************************************************************
      INTEGER FUNCTION dynsoilparamrun()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, ERROR_dynamic, LAKE, CLOSEZERO, CANOPY
      USE PRMS_MODULE, ONLY: Nhru, Nowyear, Nowmonth, Nowday, Dyn_imperv_flag, Dprst_flag, PRMS4_flag, &
     &    AG_flag, Hru_type, GSFLOW_flag, Ag_package, irrigation_apply_flag
      USE PRMS_DYNAMIC_SOIL_PARAM_READ
      USE PRMS_BASIN, ONLY: Hru_area, Dprst_clos_flag, gsflow_ag_area, gsflow_ag_frac, &
     &    Hru_frac_imperv, Hru_frac_perv, Hru_imperv, Hru_perv, Hru_frac_dprst, Dprst_open_flag, &
     &    Dprst_area_max, Dprst_area_open_max, Dprst_area_clos_max, Dprst_frac_open, &
     &    Hru_area_dble, Dprst_area, Active_hrus, Hru_route_order, Basin_area_inv, Imperv_flag, Ag_area, Ag_frac
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Imperv_stor, Sat_threshold, &
     &    Soil_rechr_max, Soil_moist_max, Imperv_stor_max, Dprst_vol_open, Dprst_vol_clos, Ssres_stor, &
     &    Slow_stor, Pref_flow_stor, Basin_soil_moist, Basin_ssstor, Hru_impervstor, Dprst_stor_hru, &
     &    Soil_zone_max, Soil_lower_stor_max, &
     &    Ag_soil_moist, Ag_soil_rechr, Ag_soil_moist_max, Ag_soil_rechr_max, Ag_soil_rechr_max_frac, &
     &    Basin_ag_soil_moist, Basin_ag_soil_rechr
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_basin_soil_moist, It0_soil_rechr, &
                               It0_ssres_stor, It0_basin_ssstor, It0_imperv_stor, It0_hru_impervstor, &
                               It0_ag_soil_moist, It0_ag_soil_rechr, It0_dprst_vol_open, It0_dprst_vol_clos, &
                               It0_dprst_stor_hru, It0_slow_stor
      USE PRMS_SRUNOFF, ONLY:  Dprst_depth_avg, Op_flow_thres, Dprst_vol_open_max, Dprst_vol_clos_max, &
     &    Dprst_vol_thres_open, Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac
      USE PRMS_SOILZONE, ONLY: Replenish_frac
      USE PRMS_SOILZONE_AG, ONLY: Ag_soil_lower_stor_max, Ag_replenish_frac
      use prms_utils, only: is_eof, error_stop
      IMPLICIT NONE
! Functions
      INTRINSIC :: SNGL, DBLE
      EXTERNAL :: write_dynoutput, write_dynparam, write_dynparam_int
      EXTERNAL :: write_dynparam_potet
! Local Variables
      INTEGER :: i, j, istop, check_dprst_depth_flag, check_sm_max_flag
      INTEGER :: ios, check_fractions, check_imperv, check_dprst_frac
      INTEGER :: it0_sm_flag, it0_grav_flag, it0_dprst_flag
      INTEGER :: check_ag_max_flag, adjust_dprst_fractions, adjust_imperv_fractions, check_ag_frac, it0_imperv_flag
      REAL :: harea, frac_imperv, tmp, frac_dprst, frac_ag, to_slow_stor, frac_perv
      CHARACTER(LEN=30), PARAMETER :: FMT1 = '(I5, 2("/",I2.2))'
!***********************************************************************
      dynsoilparamrun = 0
      istop = 0

      ! leave current impervious storage amount alone as it will be taking care of later in current timestep
      IF ( Dyn_imperv_flag>1 ) THEN
        IF ( Imperv_stor_next_mo/=0 ) THEN
          IF ( Imperv_stor_next_yr==Nowyear .AND. Imperv_stor_next_mo==Nowmonth .AND. Imperv_stor_next_day==Nowday ) THEN
            READ ( Imperv_stor_unit, *, IOSTAT=ios ) Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day, Temp
            if (ios /= 0) call error_stop('reading impervious storage dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Imperv_stor_max, 'imperv_stor_max')
            CALL is_eof(Imperv_stor_unit, Imperv_stor_next_yr, Imperv_stor_next_mo, Imperv_stor_next_day)
          ENDIF
        ENDIF
      ENDIF

      check_dprst_depth_flag = OFF
      IF ( Dprst_depth_flag==ACTIVE ) THEN
        IF ( Dprst_depth_next_mo/=0 ) THEN
          IF ( Dprst_depth_next_yr==Nowyear .AND. Dprst_depth_next_mo==Nowmonth .AND. Dprst_depth_next_day==Nowday ) THEN
            READ ( Dprst_depth_unit, *, IOSTAT=ios ) Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day, Temp
            if (ios /= 0) call error_stop('reading depression storage dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Dprst_depth_avg, 'dprst_depth_avg')
            CALL is_eof(Dprst_depth_unit, Dprst_depth_next_yr, Dprst_depth_next_mo, Dprst_depth_next_day)
            check_dprst_depth_flag = ACTIVE
          ENDIF
        ENDIF
      ENDIF

      check_sm_max_flag = OFF
      ! leave current soil_moist storage amount alone as it will be taking care of later in current timestep
      IF ( Soilmoist_flag==ACTIVE ) THEN
        IF ( Soil_moist_next_mo/=0 ) THEN
          IF ( Soil_moist_next_yr==Nowyear .AND. Soil_moist_next_mo==Nowmonth .AND. Soil_moist_next_day==Nowday ) THEN
            READ ( Soil_moist_unit, *, IOSTAT=ios ) Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day, Temp
            if (ios /= 0) call error_stop('reading soil_moist_max dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_moist_max, 'soil_moist_max')
            CALL is_eof(Soil_moist_unit, Soil_moist_next_yr, Soil_moist_next_mo, Soil_moist_next_day)
            check_sm_max_flag = ACTIVE
          ENDIF
        ENDIF
      ENDIF
      ! leave current soil_rechr storage amount alone as it will be taking care of later in current timestep
      IF ( Soilrechr_flag==ACTIVE ) THEN
        IF ( Soil_rechr_next_mo/=0 ) THEN
          IF ( Soil_rechr_next_yr==Nowyear .AND. Soil_rechr_next_mo==Nowmonth .AND. Soil_rechr_next_day==Nowday ) THEN
            READ ( Soil_rechr_unit, *, IOSTAT=ios ) Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day, Temp
            if (ios /= 0) call error_stop('reading soil_rechr_max dynamic parameter file', ERROR_dynamic)
            IF ( PRMS4_flag==ACTIVE ) THEN
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_rechr_max, 'soil_rechr_max')
            ELSE
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Soil_rechr_max_frac, 'soil_rechr_max_frac')
            ENDIF
            CALL is_eof(Soil_rechr_unit, Soil_rechr_next_yr, Soil_rechr_next_mo, Soil_rechr_next_day)
            check_sm_max_flag = ACTIVE
          ENDIF
        ENDIF
      ENDIF

! Ag soil parameters
      check_ag_max_flag = OFF
      IF ( Ag_soilrechr_flag==ACTIVE ) THEN
        IF ( Ag_soil_rechr_next_mo/=0 ) THEN
          IF ( Ag_soil_rechr_next_yr==Nowyear .AND. Ag_soil_rechr_next_mo==Nowmonth .AND. Ag_soil_rechr_next_day==Nowday ) THEN
            READ ( Ag_soil_rechr_unit, *, IOSTAT=ios ) Ag_soil_rechr_next_yr, Ag_soil_rechr_next_mo, Ag_soil_rechr_next_day, Temp
            if (ios /= 0) call error_stop('reading ag_soil_rechr_frac dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Ag_soil_rechr_max_frac, 'ag_soil_rechr_max_frac')
            CALL is_eof(Ag_soil_rechr_unit, Ag_soil_rechr_next_yr, Ag_soil_rechr_next_mo, Ag_soil_rechr_next_day)
            check_ag_max_flag = ACTIVE
          ENDIF
        ENDIF
      ENDIF
! leave current ag_soil_moist storage amount alone as it will be taking care of later in current timestep
      IF ( Ag_soilmoist_flag==ACTIVE ) THEN
        IF ( Ag_soil_moist_next_mo/=0 ) THEN
          IF ( Ag_soil_moist_next_yr==Nowyear .AND. Ag_soil_moist_next_mo==Nowmonth .AND. Ag_soil_moist_next_day==Nowday ) THEN
            READ ( Ag_soil_moist_unit, *, IOSTAT=ios ) Ag_soil_moist_next_yr, Ag_soil_moist_next_mo, Ag_soil_moist_next_day, Temp
            if (ios /= 0) call error_stop('reading ag_soil_moist_max dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Ag_soil_moist_max, 'ag_soil_moist_max')
            CALL is_eof(Ag_soil_moist_unit, Ag_soil_moist_next_yr, Ag_soil_moist_next_mo, Ag_soil_moist_next_day)
            check_ag_max_flag = ACTIVE
          ENDIF
        ENDIF
      ENDIF

      IF ( check_sm_max_flag==ACTIVE .OR. check_ag_max_flag==ACTIVE .OR. check_dprst_depth_flag==ACTIVE ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Hru_type(i)==LAKE ) CYCLE ! skip lake HRUs
          IF ( check_sm_max_flag==ACTIVE ) THEN
            IF ( Soil_moist_max(i)<0.001 .AND. AG_flag==OFF ) THEN
              PRINT 9001, 'soil_moist_max', 0.001, i, Soil_moist_max(i), 0.001
              Soil_moist_max(i) = 0.001
            ENDIF
            IF ( PRMS4_flag==OFF ) Soil_rechr_max(i) = Soil_moist_max(i)*Soil_rechr_max_frac(i)
            IF ( Soil_rechr_max(i)<0.00001 ) THEN
              PRINT 9001, 'soil_rechr_max', 0.00001, i, Soil_rechr_max(i), 0.00001
              Soil_rechr_max(i) = 0.00001
            ENDIF
            IF ( Soil_rechr_max(i)>Soil_moist_max(i) ) THEN
              istop = 1
              PRINT 9002, Soil_rechr_max(i), Soil_moist_max(i), i
              CYCLE
            ENDIF
            Soil_lower_stor_max(i) = Soil_moist_max(i) - Soil_rechr_max(i)
          ENDIF

          IF ( check_ag_max_flag==ACTIVE ) THEN
            IF ( Ag_soil_moist_max(i)<0.001 ) THEN
              PRINT 9001, 'ag_soil_moist_max', 0.001, i, Ag_soil_moist_max(i), 0.001
              Ag_soil_moist_max(i) = 0.001
            ENDIF
            Ag_soil_rechr_max(i) = Ag_soil_moist_max(i) * Ag_soil_rechr_max_frac(i)
            IF ( Ag_soil_rechr_max(i)<0.00001 ) THEN
              PRINT 9001, 'ag_soil_rechr_max', 0.00001, i, Ag_soil_rechr_max(i), 0.00001
              Ag_soil_rechr_max(i) = 0.00001
            ENDIF
            IF ( Ag_soil_rechr_max(i)>Ag_soil_moist_max(i) ) THEN
              istop = 1
              PRINT 9003, Ag_soil_rechr_max(i), Ag_soil_moist_max(i), i
              CYCLE
            ENDIF
            Ag_soil_lower_stor_max(i) = Ag_soil_moist_max(i) - Ag_soil_rechr_max(i)
          ENDIF

          IF ( check_dprst_depth_flag==ACTIVE ) THEN
            Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
            Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )
            Dprst_vol_thres_open(i) = Dprst_vol_open_max(i)*DBLE(Op_flow_thres(i))
          ENDIF
        ENDDO
      ENDIF

      !*******************
      check_fractions = OFF
      check_imperv = OFF
      check_dprst_frac = OFF
      check_ag_frac = OFF
      it0_sm_flag = OFF
      it0_grav_flag = OFF
      it0_dprst_flag = OFF
      it0_imperv_flag = OFF
      !*******************

      IF ( Imperv_frac_flag==ACTIVE ) THEN
        IF ( Imperv_next_mo/=0 ) THEN
          IF ( Imperv_next_yr==Nowyear .AND. Imperv_next_mo==Nowmonth .AND. Imperv_next_day==Nowday ) THEN
            READ ( Imperv_frac_unit, *, iostat=ios ) Imperv_next_yr, Imperv_next_mo, Imperv_next_day, temp_imperv_frac
            if (ios /= 0) call error_stop('reading impervious dynamic parameter file', ERROR_dynamic)
            ! temp_imperv_frac has new values, Hru_percent_imperv has old values
            CALL write_dynoutput(Output_unit, Nhru, Updated_hrus, temp_imperv_frac, Hru_frac_imperv, 'hru_percent_imperv')
            ! temp_imperv_frac has new values with negative values set to the old value
            CALL is_eof(Imperv_frac_unit, Imperv_next_yr, Imperv_next_mo, Imperv_next_day)
            check_imperv = ACTIVE
            check_fractions = ACTIVE
            Imperv_flag = OFF
          ENDIF
        ENDIF
      ENDIF

      IF ( Dprst_frac_flag==ACTIVE ) THEN
        Dprst_clos_flag = OFF
        Dprst_open_flag = OFF
        IF ( Dprst_frac_next_mo/=0 ) THEN
          IF ( Dprst_frac_next_yr==Nowyear .AND. Dprst_frac_next_mo==Nowmonth .AND. Dprst_frac_next_day==Nowday ) THEN
            READ ( Dprst_frac_unit, * ) Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day, temp_dprst_frac
            CALL write_dynoutput(Output_unit, Nhru, Updated_hrus, temp_dprst_frac, Hru_frac_dprst, 'dprst_frac')
            CALL is_eof(Dprst_frac_unit, Dprst_frac_next_yr, Dprst_frac_next_mo, Dprst_frac_next_day)
            check_dprst_frac = ACTIVE
            check_fractions = ACTIVE
          ENDIF
        ENDIF
      ENDIF

      IF ( ag_frac_flag==ACTIVE ) THEN
        IF ( Ag_frac_next_mo/=0 ) THEN
          IF ( Ag_frac_next_yr==Nowyear .AND. Ag_frac_next_mo==Nowmonth .AND. Ag_frac_next_day==Nowday ) THEN
            READ ( Ag_frac_unit, *, IOSTAT=ios ) Ag_frac_next_yr, Ag_frac_next_mo, Ag_frac_next_day, temp_ag_frac
            if (ios /= 0) call error_stop('reading agricultural fraction dynamic parameter file', ERROR_dynamic)
            CALL write_dynoutput(Output_unit, Nhru, Updated_hrus, temp_ag_frac, Ag_frac, 'ag_frac')
            CALL is_eof(Ag_frac_unit, Ag_frac_next_yr, Ag_frac_next_mo, Ag_frac_next_day)
            check_ag_frac = ACTIVE
            check_fractions = ACTIVE
          ENDIF
        ENDIF
      ENDIF

      IF ( check_fractions==ACTIVE ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Hru_type(i)==LAKE ) CYCLE ! skip lake HRUs
          harea = Hru_area(i)
          to_slow_stor = 0.0
          adjust_dprst_fractions = OFF
          adjust_imperv_fractions = OFF

          ! temp_imperv_frac has new values with negative values set to the old value, Hru_frac_imperv has old values
          IF ( check_imperv==ACTIVE ) THEN
            frac_imperv = temp_imperv_frac(i)
          ELSE
            frac_imperv = Hru_frac_imperv(i)
          ENDIF

          frac_dprst = 0.0
          IF ( check_dprst_frac==ACTIVE ) THEN
            ! temp_dprst_frac has new values with negative values set to the old value, Dprst_frac has old values
            frac_dprst = temp_dprst_frac(i)
          ELSE
            IF ( Dprst_flag==ACTIVE ) frac_dprst = Hru_frac_dprst(i)
          ENDIF

          IF ( frac_imperv+frac_dprst > 0.999 ) THEN
            istop = 1
            PRINT *, 'ERROR, fraction impervious + fraction dprst > 0.999 for HRU:', i
            PRINT *, '       fraction impervious + dprst:', frac_imperv + frac_dprst
            PRINT *, '       hru_percent_imperv:', frac_imperv, '; frac_dprst:', frac_dprst
            CYCLE
          ENDIF

! use ag_frac as input, adjust other fractions if needed
          frac_ag = 0.0
          IF ( AG_flag==ACTIVE ) frac_ag = Ag_frac(i)
          IF ( check_ag_frac==ACTIVE ) THEN
            ! temp_ag_frac has new values with negative values set to the old value, Ag_frac has old values
            frac_ag = temp_ag_frac(i)
            if ( frac_ag>0.0 ) then
              IF ( frac_ag>1.0 ) THEN
                PRINT '(A,I0,A,F0.6)', 'WARNING, dynamic ag_frac > 1.0, set to 1.0 for HRU: ', i, ', frac_ag: ', frac_ag
                frac_ag = 1.0
              ENDIF
              IF ( frac_ag<0.0001 ) THEN
                PRINT '(A,I0,A,F0.6)', 'WARNING, dynamic ag_frac > 0.0 and < 0.0001, set to 0.0 for HRU: ', i, &
                    ', ag_frac: ', frac_ag
                frac_ag = 0.0
              ENDIF
            endif
            !if ( Ag_frac(i)>0.0 .and. .not.(frac_ag>0.0) ) then
            !  print *, 'ag_frac issue', Ag_frac(i), frac_ag, i
            !endif
            IF ( Ag_soil_moist(i)>0.0 ) THEN
              IF ( frac_ag > 0.0 ) THEN
                IF ( frac_ag < Ag_frac(i) ) THEN
                  ! keep same ag depth, send excess to GVR storage
                  to_slow_stor = Ag_soil_moist(i) * (Ag_frac(i) - frac_ag)
                  PRINT '(A,F0.4,A,F0.4)', ' WARNING, dynamic agriculture fraction reduced from: ', Ag_frac(i), ' to:', frac_ag
                  PRINT *, '         ag_soil_moist unchanged'
                  PRINT *, '         excess water added to slow storage of GVR:', to_slow_stor
                  PRINT FMT1, Nowyear, Nowmonth, Nowday
                ELSEIF ( Ag_frac(i) < frac_ag ) THEN
                  ! adjust ag depth
                  Ag_soil_moist(i) = Ag_soil_moist(i)*Ag_frac(i)/frac_ag
                  Ag_soil_rechr(i) = Ag_soil_rechr(i)*Ag_frac(i)/frac_ag
                ENDIF
              ELSE
                tmp = Ag_soil_moist(i)*Ag_frac(i)
                PRINT *, 'WARNING, dynamic agriculture fraction changed to 0 when ag_soil_moist > 0'
                PRINT '(A,F0.4,A,F0.4)', '          old value: ', Ag_frac(i), '; storage added to slow storage of GVR: ', tmp
                PRINT FMT1, Nowyear, Nowmonth, Nowday
                to_slow_stor = to_slow_stor + tmp
                Ag_soil_moist(i) = 0.0
                Ag_soil_rechr(i) = 0.0
              ENDIF
            ENDIF
            Ag_frac(i) = frac_ag
            Ag_area(i) = frac_ag * Hru_area(i)

            tmp = 1.0 - (frac_imperv + frac_dprst + frac_ag)
            IF ( frac_ag < 0.0001 .AND. tmp < 0.0001 ) THEN
              istop = 1
              PRINT *, 'ERROR, pervious fraction or agriculture pervious fraction must be >= 0.0001 for HRU:', i
              PRINT *, '       pervious fraction:', tmp, '; agriculture fraction:', frac_ag
              CYCLE
            ENDIF

            ! check sum of imperv, ag, and dprst if either are updated!!!!!!
            IF ( tmp < 0.0 ) THEN
              print *, 'Sum of impervious, surface depression storage, and agriculture fractions > 1'
              print *, '  for HRU:', i
              print *, '  impervious:', frac_imperv, '; dprst:', frac_dprst, '; agriculture:', frac_ag
              print *, '  agriculture fraction is retained, dprst fraction is reduced,'
              print *, '  if necessary impervious'
              IF ( frac_dprst>0.0 ) THEN
                adjust_dprst_fractions = ACTIVE
                frac_dprst = frac_dprst + tmp ! reduce frac_dprst first, then imperv_frac if needed
              ENDIF
              IF ( frac_dprst<0.0 ) THEN
                IF ( frac_imperv>0.0 ) frac_imperv = frac_imperv + frac_dprst
                IF ( frac_imperv<0.0 ) THEN
                  IF ( frac_imperv<CLOSEZERO ) THEN
                    PRINT *, 'ERROR adjusting impervious and dprst fractions'
                    istop = 1
                    EXIT
                  ENDIF
                ENDIF
                adjust_imperv_fractions = ACTIVE
                frac_dprst = 0.0
              ENDIF
!              print *, 'adjusted values: impervious:', frac_imperv, '; dprst:', frac_dprst, '; agriculture:', frac_ag
            ENDIF
          ENDIF

          IF ( check_imperv==ACTIVE .OR. adjust_imperv_fractions==ACTIVE ) THEN
            IF ( Imperv_stor(i)>0.0 ) THEN
              IF ( frac_imperv>0.0 ) THEN
                Imperv_stor(i) = Imperv_stor(i)*Hru_frac_imperv(i)/frac_imperv
                it0_imperv_flag = ACTIVE
              ELSE
                tmp = Imperv_stor(i)*Hru_frac_imperv(i)
                PRINT *, 'WARNING, impervious changed to 0 in dynamic parameter module with impervious storage > 0'
                PRINT *, '         this storage is added to slow storage of gravity reservoir:', tmp, '; HRU: ', i
                PRINT FMT1, Nowyear, Nowmonth, Nowday
                to_slow_stor = to_slow_stor + tmp
                Imperv_stor(i) = 0.0
                it0_imperv_flag = ACTIVE
              ENDIF
            ENDIF
            Hru_impervstor(i) = Imperv_stor(i)*frac_imperv
            Hru_frac_imperv(i) = frac_imperv
            Hru_imperv(i) = harea*frac_imperv
            IF ( frac_imperv > 0.0 ) Imperv_flag = ACTIVE
          ENDIF

          ! adjust dprst volume if dprst_frac > and frac_dprst = 0, otherwise leave volume adjust for srunoff
          ! volume to groundwater ?? now volume to pervious, if no pervious??
          IF ( Dprst_flag==ACTIVE ) THEN
            IF ( check_dprst_frac==ACTIVE .OR. adjust_dprst_fractions==ACTIVE ) THEN
              IF ( .NOT.(Dprst_depth_avg(i)>0.0) .AND. frac_dprst>0.0 ) THEN
                istop = 1
                PRINT *, 'ERROR, dprst_frac>0 and dprst_depth_avg==0 for HRU:', i, '; dprst_frac:', frac_dprst
                CYCLE
              ENDIF
              ! CAUTION: other DPRST parameters need to have valid values as related to any dynamic parameter updates
              tmp = SNGL( Dprst_vol_open(i) + Dprst_vol_clos(i) )
              IF ( tmp > 0.0 ) THEN
                IF ( .NOT.(frac_dprst)>0.0 .AND. tmp>0.0 ) THEN
                  tmp = ( tmp / Dprst_area(i) ) * Hru_frac_dprst(i)
                  PRINT *, 'WARNING, dprst_frac reduced to 0 with storage > 0 in dynamic parameter module'
                  PRINT *, '         storage added to slow storage of the gravity reservoir:', tmp, '; HRU: ', i
                  PRINT FMT1, Nowyear, Nowmonth, Nowday
                  to_slow_stor = to_slow_stor + tmp
                  Dprst_vol_open(i) = 0.0D0
                  Dprst_vol_clos(i) = 0.0D0
                  it0_dprst_flag = ACTIVE
                ENDIF
                IF ( frac_dprst>0.0 ) THEN
                  ! update variables as dprst could have gone from positive value to 0 and not get updated in srunoff
                  IF ( Dprst_vol_open_max(i)>0.0D0 ) THEN
                    Dprst_vol_open_frac(i) = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
                  ELSE
                    Dprst_vol_open_frac(i) = 0.0
                  ENDIF
                  IF ( Dprst_vol_clos_max(i)>0.0D0 ) THEN
                    Dprst_vol_clos_frac(i) = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
                  ELSE
                    Dprst_vol_clos_frac(i) = 0.0
                  ENDIF
                  IF ( Dprst_vol_open_max(i)+Dprst_vol_clos_max(i)>0.0D0 ) THEN
                    Dprst_vol_frac(i) = SNGL( (Dprst_vol_open(i)+Dprst_vol_clos(i))/(Dprst_vol_open_max(i)+Dprst_vol_clos_max(i)) )
                  ELSE
                    Dprst_vol_frac(i) = 0.0
                  ENDIF
                ENDIF
              ENDIF
              Hru_frac_dprst(i) = frac_dprst
              Dprst_area_max(i) = frac_dprst*harea
              Dprst_area_open_max(i) = Dprst_area_max(i)*Dprst_frac_open(i)
              Dprst_area_clos_max(i) = Dprst_area_max(i) - Dprst_area_open_max(i)
              IF ( Dprst_area_clos_max(i)>0.0 ) Dprst_clos_flag = ACTIVE
              IF ( Dprst_area_open_max(i)>0.0 ) Dprst_open_flag = ACTIVE
            ENDIF
          ENDIF

          frac_perv = 1.0 - (frac_imperv + frac_dprst + frac_ag)

          IF ( frac_perv<0.0 ) THEN
            IF ( frac_perv < -CLOSEZERO ) THEN
              PRINT *, 'ERROR, pervious < 0 for HRU:', i
              PRINT *, '       pervious fraction equals 1.0 - hru_percent_imperv - dprst_frac - ag_frac'
              PRINT *, '       pervious fraction:', frac_perv
              PRINT *, '       impervious fraction:', Hru_frac_imperv(i)
              IF ( Dprst_flag==ACTIVE ) PRINT *, '       depression storage fraction:', Hru_frac_dprst(i)
              istop = 1
              CYCLE
            ENDIF
          ELSEIF ( frac_perv<0.00001 .AND. AG_flag==OFF ) THEN
            PRINT *, 'ERROR, pervious fraction must be >= 0.00001 for HRU:', i
            PRINT *, '       pervious fraction is 1.0 - hru_percent_imperv - dprst_frac'
            PRINT *, '       pervious fraction:', frac_perv
            PRINT *, '       impervious fraction:', Hru_frac_imperv(i)
            IF ( Dprst_flag==ACTIVE ) PRINT *, '       depression storage fraction:', Hru_frac_dprst(i)
            istop = 1
            CYCLE
          ENDIF

          IF ( frac_perv>0.0 ) THEN
            ! adjust pervious area and capillary storage for dynamic parameters
            IF ( Hru_frac_perv(i) /= frac_perv ) THEN
              tmp = Hru_frac_perv(i)/frac_perv
              Soil_moist(i) = Soil_moist(i)*tmp
              Soil_rechr(i) = Soil_rechr(i)*tmp
              it0_sm_flag = ACTIVE
            ENDIF
          ELSE
            IF ( Hru_frac_perv(i) > 0.0 ) THEN
              to_slow_stor = to_slow_stor + Soil_moist(i) * Hru_frac_perv(i)
              Soil_moist(i) = 0.0
              Soil_rechr(i) = 0.0
              it0_sm_flag = ACTIVE
            ENDIF
            frac_perv = 0.0
          ENDIF
          Hru_frac_perv(i) = frac_perv
          Hru_perv(i) = harea * frac_perv
          if ( to_slow_stor>0.0 ) then
!            print *, 'to_slow_stor', to_slow_stor
            Slow_stor(i) = Slow_stor(i) + to_slow_stor
            it0_grav_flag = ACTIVE
          endif

        ENDDO
        if ( istop==1 ) then
          PRINT FMT1, Nowyear, Nowmonth, Nowday
          ERROR STOP ERROR_dynamic
        endif

        IF ( it0_sm_flag == ACTIVE ) THEN
          Basin_soil_moist = 0.0D0
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*Hru_perv(i) )
          ENDDO
          Basin_soil_moist = Basin_soil_moist * Basin_area_inv
          It0_soil_moist = Soil_moist
          It0_soil_rechr = Soil_rechr
          It0_basin_soil_moist = Basin_soil_moist
        ENDIF
        IF ( it0_grav_flag == ACTIVE ) THEN
          Basin_ssstor = 0.0D0
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
            Basin_ssstor = Basin_ssstor + DBLE( Ssres_stor(i)*Hru_area(i) )
          ENDDO
          Basin_ssstor = Basin_ssstor * Basin_area_inv
          It0_ssres_stor = Ssres_stor
          It0_basin_ssstor = Basin_ssstor
          It0_slow_stor = Slow_stor
        ENDIF
        IF ( check_ag_frac==ACTIVE ) THEN
          Basin_ag_soil_moist = 0.0D0
          Basin_ag_soil_rechr = 0.0D0
          DO j = 1, Active_hrus
            i = Hru_route_order(j)
            Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(i)*Ag_area(i) )
            Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(i)*Ag_area(i) )
          ENDDO
          Basin_ag_soil_moist = Basin_ag_soil_moist * Basin_area_inv
          Basin_ag_soil_rechr = Basin_ag_soil_rechr * Basin_area_inv
          It0_ag_soil_moist = Ag_soil_moist
          It0_ag_soil_rechr = Ag_soil_rechr
        ENDIF
        IF ( it0_dprst_flag==ACTIVE ) THEN
          It0_dprst_vol_open = Dprst_vol_open
          It0_dprst_vol_clos = Dprst_vol_clos
          Dprst_stor_hru = (Dprst_vol_open+Dprst_vol_clos) / Hru_area_dble
          It0_dprst_stor_hru = Dprst_stor_hru
        ENDIF
        IF ( it0_imperv_flag==ACTIVE ) THEN
          It0_imperv_stor = Imperv_stor
          It0_hru_impervstor = Hru_impervstor
        ENDIF
        IF ( Ag_package==ACTIVE ) THEN
          IF ( irrigation_apply_flag == CANOPY ) THEN
              gsflow_ag_area = Hru_area ! apply irrigation to canopy, which adjusts for covden
              gsflow_ag_frac = 1.0
          ELSEIF ( AG_flag == ACTIVE .AND. irrigation_apply_flag == 3 ) THEN
              gsflow_ag_area = Ag_area ! apply irrigation to ag area in soilzone_ag module
              gsflow_ag_frac = Ag_frac
          ELSE
              gsflow_ag_area = Hru_perv ! apply irrigation to pervious area in soilzone module
              gsflow_ag_frac = Hru_frac_perv
          ENDIF
        ENDIF
      ENDIF

      IF ( check_sm_max_flag==ACTIVE .OR. check_fractions==ACTIVE ) THEN
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          IF ( Hru_type(i)==LAKE ) CYCLE ! skip lake
          Soil_zone_max(i) = Sat_threshold(i) + Soil_moist_max(i)*Hru_frac_perv(i)
          IF ( GSFLOW_flag==ACTIVE .AND. Soil_moist_max(i)>0.0 ) Replenish_frac(i) = Soil_rechr_max(i)/Soil_moist_max(i)
          IF ( AG_flag==ACTIVE ) THEN
            IF ( GSFLOW_flag==ACTIVE .AND. Ag_soil_moist_max(i)>0.0 ) &
                 Ag_replenish_frac(i) = ag_soil_rechr_max(i)/Ag_soil_moist_max(i)
          ENDIF
        ENDDO
      ENDIF

 9001 FORMAT (/, 'WARNING, dynamic parameter', A, ' <', F0.7, ' for HRU: ', I0, /, 9X, 'value: ', F0.7, ' set to ', F0.7)
 9002 FORMAT (/, 'ERROR, dynamic parameter causes soil_rechr_max: ', F0.7, ' > soil_moist_max: ', F0.7, ' for HRU: ', I0)
 9003 FORMAT (/, 'ERROR, dynamic parameter causes ag_soil_rechr_max: ', F0.7, ' > ag_soil_moist_max: ', F0.7, ' for HRU: ', I0)

      END FUNCTION dynsoilparamrun

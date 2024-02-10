!***********************************************************************
! Read and makes available dynamic parameters cov_type, rad_trncf,
! transp_beg, transp_end, covden_sum, covden_win, fall_frost, spring_frost
! sro_to_dprst, sro_to_imperv, snarea_thresh, jh_coef, jh_coef_hru,
! pm_n_coef, pm_d_coef, pt_alpha, hs_krs, hamon_coef, potet_cbh_adj,
! wrain_intcp, srain_intcp, snow_intcp by HRU from pre-processed files.
! These parameters can be input for any date within the simulation time
! period. Associated states with each parameter are adjusted in module computed.
!***********************************************************************
      MODULE PRMS_DYNAMIC_PARAM_READ
        USE PRMS_CONSTANTS, ONLY: MAXFILE_LENGTH
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Time Series Data'
        character(len=*), parameter :: MODNAME = 'dynamic_param_read'
        character(len=*), parameter :: Version_dynamic_param_read = '2023-11-01'
        INTEGER, SAVE :: Wrain_intcp_unit, Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day
        INTEGER, SAVE :: Srain_intcp_unit, Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day
        INTEGER, SAVE :: Snow_intcp_unit, Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day
        INTEGER, SAVE :: Transp_event_unit, Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day
        INTEGER, SAVE :: Covtype_unit, Covtype_next_yr, Covtype_next_mo, Covtype_next_day
        INTEGER, SAVE :: Covden_sum_unit, Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day, Covden_sum_flag
        INTEGER, SAVE :: Covden_win_unit, Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day, Covden_win_flag
        INTEGER, SAVE :: Potetcoef_unit, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day
        INTEGER, SAVE :: Transpbeg_unit, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day, Transpbeg_flag
        INTEGER, SAVE :: Transpend_unit, Transpend_next_yr, Transpend_next_mo, Transpend_next_day, Transpend_flag
        INTEGER, SAVE :: Fallfrost_unit, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day, Fallfrost_flag
        INTEGER, SAVE :: Springfrost_unit, Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day, Springfrost_flag
        INTEGER, SAVE :: Rad_trncf_unit, Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day
        INTEGER, SAVE :: Sro_to_dprst_unit, Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day
        INTEGER, SAVE :: Sro_to_imperv_unit, Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day
        INTEGER, SAVE :: Wrainintcp_flag, Srainintcp_flag, Snowintcp_flag, Output_unit
        INTEGER, SAVE :: Snarea_thresh_unit, Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day
        INTEGER, SAVE, ALLOCATABLE :: Itemp(:), Updated_hrus(:)
        REAL, SAVE, ALLOCATABLE :: Temp(:), Potet_coef(:, :)
! Control Parameters
        CHARACTER(LEN=MAXFILE_LENGTH) :: wrain_intcp_dynamic, srain_intcp_dynamic, snow_intcp_dynamic, covtype_dynamic
        CHARACTER(LEN=MAXFILE_LENGTH) :: potetcoef_dynamic, transpbeg_dynamic, transpend_dynamic
        CHARACTER(LEN=MAXFILE_LENGTH) :: radtrncf_dynamic, dynamic_param_log_file
        CHARACTER(LEN=MAXFILE_LENGTH) :: fallfrost_dynamic, springfrost_dynamic, transp_on_dynamic, snareathresh_dynamic
        CHARACTER(LEN=MAXFILE_LENGTH) :: covden_sum_dynamic, covden_win_dynamic, sro2dprst_perv_dyn, sro2dprst_imperv_dyn
      END MODULE PRMS_DYNAMIC_PARAM_READ

!***********************************************************************
!     Main dynamic parameter routine
!***********************************************************************
      INTEGER FUNCTION dynamic_param_read()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_DYNAMIC_PARAM_READ, ONLY: MODDESC, MODNAME, Version_dynamic_param_read
      use prms_utils, only: print_module
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: dynparamrun, dynparaminit
!***********************************************************************
      dynamic_param_read = 0

      IF ( Process_flag==RUN ) THEN
        dynamic_param_read = dynparamrun()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_dynamic_param_read)
      ELSEIF ( Process_flag==INIT ) THEN
        dynamic_param_read = dynparaminit()
      ENDIF

      END FUNCTION dynamic_param_read

!***********************************************************************
!     dynparaminit - open files, read to start time, initialize flags and arrays
!***********************************************************************
      INTEGER FUNCTION dynparaminit()
      USE PRMS_CONSTANTS, ONLY: Nmonths, ACTIVE, OFF, ERROR_dynamic, DEBUG_minimum
      use PRMS_CONTROL_FILE, only: control_string
      USE PRMS_MODULE, ONLY: Nhru, Print_debug, Start_year, Start_month, Start_day, &
     &    Dyn_intcp_flag, Dyn_covden_flag, &
     &    Dyn_covtype_flag, Dyn_potet_flag, Dyn_transp_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, &
     &    Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, Transp_flag, Dyn_fallfrost_flag, &
     &    Dyn_springfrost_flag, Dyn_snareathresh_flag
      USE PRMS_DYNAMIC_PARAM_READ
      use prms_utils, only: error_stop, find_current_file_time, find_header_end, PRMS_open_output_file, read_error, numchars
      IMPLICIT NONE
! Local Variables
      INTEGER :: year, month, day, istop, ierr
!***********************************************************************
      dynparaminit = 0

      year = Start_year
      month = Start_month
      day = Start_day

      ALLOCATE ( Temp(Nhru), Itemp(Nhru), Updated_hrus(Nhru) )

      istop = 0
      Wrainintcp_flag = OFF
      Srainintcp_flag = OFF
      Snowintcp_flag = OFF
      IF ( Dyn_intcp_flag>OFF ) THEN
        IF ( Dyn_intcp_flag==1 .OR. Dyn_intcp_flag==3 .OR. Dyn_intcp_flag==5 .OR. Dyn_intcp_flag==7 ) THEN
          Wrainintcp_flag = ACTIVE
          IF ( control_string(wrain_intcp_dynamic, 'wrain_intcp_dynamic')/=0 ) CALL read_error(5, 'wrain_intcp_dynamic')
          CALL find_header_end(Wrain_intcp_unit, wrain_intcp_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Wrain_intcp_unit, year, month, day, &
     &                                  Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Dyn_intcp_flag==2 .OR. Dyn_intcp_flag==3 .OR. Dyn_intcp_flag==6 .OR. Dyn_intcp_flag==7 ) THEN
          Srainintcp_flag = ACTIVE
          IF ( control_string(srain_intcp_dynamic, 'srain_intcp_dynamic')/=0 ) CALL read_error(5, 'srain_intcp_dynamic')
          CALL find_header_end(Srain_intcp_unit, srain_intcp_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Srain_intcp_unit, year, month, day, &
     &                                  Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Dyn_intcp_flag>3 ) THEN
          Snowintcp_flag = ACTIVE
          IF ( control_string(snow_intcp_dynamic, 'snow_intcp_dynamic')/=0 ) CALL read_error(5, 'snown_intcp_dynamic')
          CALL find_header_end(Snow_intcp_unit, snow_intcp_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Snow_intcp_unit, year, month, day, &
     &                                  Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day)
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Covden_win_flag = OFF
      Covden_sum_flag = OFF
      IF ( Dyn_covden_flag==1 .OR. Dyn_covden_flag==3 ) THEN
        IF ( control_string(covden_sum_dynamic, 'covden_sum_dynamic')/=0 ) CALL read_error(5, 'covden_sum_dynamic')
        CALL find_header_end(Covden_sum_unit, covden_sum_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covden_sum_unit, year, month, day, Covden_sum_next_yr, Covden_sum_next_mo,Covden_sum_next_day)
          Covden_sum_flag = ACTIVE
        ELSE
          istop = 1
        ENDIF
      ENDIF
      IF ( Dyn_covden_flag==2 .OR. Dyn_covden_flag==3 ) THEN
        IF ( control_string(covden_win_dynamic, 'covden_win_dynamic')/=0 ) CALL read_error(5, 'covden_win_dynamic')
        CALL find_header_end(Covden_win_unit, covden_win_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covden_win_unit, year, month, day, Covden_win_next_yr, Covden_win_next_mo,Covden_win_next_day)
          Covden_win_flag = ACTIVE
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_covtype_flag==ACTIVE ) THEN
        IF ( control_string(covtype_dynamic, 'covtype_dynamic')/=0 ) CALL read_error(5, 'covtype_dynamic')
        CALL find_header_end(Covtype_unit, covtype_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Covtype_unit, year, month, day, Covtype_next_yr, Covtype_next_mo, Covtype_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_potet_flag>OFF ) THEN
        ALLOCATE ( Potet_coef(Nhru,Nmonths) )
        IF ( control_string(potetcoef_dynamic, 'potetcoef_dynamic')/=0 ) CALL read_error(5, 'potetcoef_dynamic')
        CALL find_header_end(Potetcoef_unit, potetcoef_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Potetcoef_unit, year, month, day, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      Transpbeg_flag = OFF
      IF ( Dyn_transp_flag==1 .OR. Dyn_transp_flag==3 ) THEN
        IF ( Transp_flag/=1 ) THEN
          PRINT *, 'ERROR, transp_beg input as dynamic parameter but transp_module not transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(transpbeg_dynamic, 'transpbeg_dynamic')/=0 ) CALL read_error(5, 'transpbeg_dynamic')
          CALL find_header_end(Transpbeg_unit, transpbeg_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Transpbeg_unit, year, month, day, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day)
            Transpbeg_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Transpend_flag = OFF
      IF ( Dyn_transp_flag>1 ) THEN
        IF ( Transp_flag/=1 ) THEN
          PRINT *, 'ERROR, transp_end input as dynamic parameter but transp_module not transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(transpend_dynamic, 'transpend_dynamic')/=0 ) CALL read_error(5, 'transpend_dynamic')
          CALL find_header_end(Transpend_unit, transpend_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Transpend_unit, year, month, day, Transpend_next_yr, Transpend_next_mo, Transpend_next_day)
            Transpend_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Fallfrost_flag = OFF
      IF ( Dyn_fallfrost_flag==ACTIVE ) THEN
        IF ( Transp_flag==1 ) THEN
          PRINT *, 'ERROR, fall_frost input as dynamic parameter but transp_module set to transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(fallfrost_dynamic, 'fallfrost_dynamic')/=0 ) CALL read_error(5, 'fallfrost_dynamic')
          CALL find_header_end(Fallfrost_unit, fallfrost_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Fallfrost_unit, year, month, day, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day)
            Fallfrost_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      Springfrost_flag = OFF
      IF ( Dyn_springfrost_flag==ACTIVE ) THEN
        IF ( Transp_flag==1 ) THEN
          PRINT *, 'ERROR, spring_frost input as dynamic parameter but transp_module set to transp_tindex'
          istop = 1
        ELSE
          IF ( control_string(springfrost_dynamic, 'springfrost_dynamic')/=0 ) CALL read_error(5, 'springfrost_dynamic')
          CALL find_header_end(Springfrost_unit, springfrost_dynamic, ierr)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(Springfrost_unit, year, month, day, Springfrost_next_yr, Springfrost_next_mo, &
     &                                  Springfrost_next_day)
            Springfrost_flag = ACTIVE
          ELSE
            istop = 1
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_radtrncf_flag==ACTIVE ) THEN
        IF ( control_string(radtrncf_dynamic, 'radtrncf_dynamic')/=0 ) CALL read_error(5, 'radtrncf_dynamic')
        CALL find_header_end(Rad_trncf_unit, radtrncf_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Rad_trncf_unit, year, month, day, &
     &                                Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_snareathresh_flag==ACTIVE ) THEN
        IF ( control_string(snareathresh_dynamic, 'snareathresh_dynamic')/=0 ) CALL read_error(5, 'snareathresh_dynamic')
        CALL find_header_end(Snarea_thresh_unit, snareathresh_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Snarea_thresh_unit, year, month, day, &
     &                                Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_perv_flag==ACTIVE ) THEN
        IF ( control_string(sro2dprst_perv_dyn, 'sro2dprst_perv_dynamic')/=0 ) CALL read_error(5, 'sro2dprst_perv_dynamic')
        CALL find_header_end(Sro_to_dprst_unit, sro2dprst_perv_dyn, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Sro_to_dprst_unit, year, month, day, &
     &                                Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_imperv_flag==ACTIVE ) THEN
        IF ( control_string(sro2dprst_imperv_dyn, 'sro2dprst_imperv_dynamic')/=0 ) CALL read_error(5, 'sro2dprst_imperv_dynamic')
        CALL find_header_end(Sro_to_imperv_unit, sro2dprst_imperv_dyn, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Sro_to_imperv_unit, year, month, day, &
     &                                Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Dyn_transp_on_flag==ACTIVE ) THEN
        IF ( control_string(transp_on_dynamic, 'transp_on_dynamic')/=0 ) CALL read_error(5, 'transp_on_dynamic')
        CALL find_header_end(Transp_event_unit, transp_on_dynamic, ierr)
        IF ( ierr==0 ) THEN
          CALL find_current_file_time(Transp_event_unit, year, month, day, Transp_event_next_yr, Transp_event_next_mo, &
     &                                Transp_event_next_day)
        ELSE
          istop = 1
        ENDIF
      ENDIF

      IF ( Print_debug>DEBUG_minimum ) THEN
        IF ( control_string(dynamic_param_log_file, 'dynamic_param_log_file')/=0 ) CALL read_error(5, 'dynamic_param_log_file')
        CALL PRMS_open_output_file(Output_unit, dynamic_param_log_file, 'dynamic_param_log_file', 0, ierr)
        PRINT '(/,A,/,A)', 'A summary of dynamic parameter events are written to file:', &
     &                     dynamic_param_log_file(:numchars(dynamic_param_log_file))
      ENDIF

      IF ( istop==1 .OR. ierr/=0 ) CALL error_stop('in dynamic_param_read initialize procedure', ERROR_dynamic)

      END FUNCTION dynparaminit

!***********************************************************************
!     dynparamrun - Read and set dynamic parameters
!***********************************************************************
      INTEGER FUNCTION dynparamrun()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, ERROR_dynamic, LAKE, &
     &    potet_jh_module, potet_pan_module, potet_hamon_module, potet_hs_module, &
     &    potet_pt_module, potet_pm_module, climate_hru_module
      USE PRMS_MODULE, ONLY: Nhru, Nowyear, Nowmonth, Nowday, &
     &    Dyn_covtype_flag, Dyn_potet_flag, Dyn_radtrncf_flag, Dyn_transp_on_flag, &
     &    Dyn_sro2dprst_perv_flag, Dyn_sro2dprst_imperv_flag, &
     &    Dyn_snareathresh_flag, Et_flag
      USE PRMS_DYNAMIC_PARAM_READ
      USE PRMS_BASIN, ONLY: Cov_type, Covden_win, Covden_sum
      USE PRMS_CLIMATEVARS, ONLY: Transp_on, Epan_coef
      USE PRMS_POTET_JH, ONLY: Jh_coef, Jh_coef_hru
      USE PRMS_POTET_PM, ONLY: Pm_n_coef, Pm_d_coef
      USE PRMS_POTET_PT, ONLY: Pt_alpha
      USE PRMS_POTET_HS, ONLY: Hs_krs
      USE PRMS_POTET_HAMON, ONLY: Hamon_coef
      USE PRMS_CLIMATE_HRU, ONLY: Potet_cbh_adj
      USE PRMS_TRANSP_TINDEX, ONLY: Transp_beg, Transp_end
      USE PRMS_TRANSP_FROST, ONLY: Fall_frost, Spring_frost
      USE PRMS_INTCP, ONLY: Wrain_intcp, Srain_intcp, Snow_intcp
      USE PRMS_SNOW, ONLY: Rad_trncf, Snarea_thresh
      USE PRMS_SRUNOFF, ONLY: Sro_to_dprst_perv, Sro_to_dprst_imperv
      use prms_utils, only: is_eof, error_stop
      IMPLICIT NONE
! Functions
      EXTERNAL :: write_dynoutput, write_dynparam, write_dynparam_int
      EXTERNAL :: write_dynparam_potet
! Local Variables
      INTEGER :: i, ios
!***********************************************************************
      dynparamrun = 0

      ! leave any interception storage unchanged, it will be evaporated based on new values in intcp module
      IF ( Wrainintcp_flag==ACTIVE ) THEN
        IF ( Wrain_intcp_next_mo/=0 ) THEN
          IF ( Wrain_intcp_next_yr==Nowyear .AND. Wrain_intcp_next_mo==Nowmonth .AND. Wrain_intcp_next_day==Nowday ) THEN
            READ ( Wrain_intcp_unit, *, IOSTAT=ios ) Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day, Temp
            if (ios /= 0) call error_stop('reading wrain_intcp dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Wrain_intcp, 'wrain_intcp')
            CALL is_eof(Wrain_intcp_unit, Wrain_intcp_next_yr, Wrain_intcp_next_mo, Wrain_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Srainintcp_flag==ACTIVE ) THEN
        IF ( Srain_intcp_next_mo/=0 ) THEN
          IF ( Srain_intcp_next_yr==Nowyear .AND. Srain_intcp_next_mo==Nowmonth .AND. Srain_intcp_next_day==Nowday ) THEN
            READ ( Srain_intcp_unit, *, IOSTAT=ios ) Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day, Temp
            if (ios /= 0) call error_stop('reading srain_intcp dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Srain_intcp, 'srain_intcp')
            CALL is_eof(Srain_intcp_unit, Srain_intcp_next_yr, Srain_intcp_next_mo, Srain_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Snowintcp_flag==ACTIVE ) THEN
        IF ( Snow_intcp_next_mo/=0 ) THEN
          IF ( Snow_intcp_next_yr==Nowyear .AND. Snow_intcp_next_mo==Nowmonth .AND. Snow_intcp_next_day==Nowday ) THEN
            READ ( Snow_intcp_unit, *, IOSTAT=ios ) Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day, Temp
            if (ios /= 0) call error_stop('reading snow_intcp dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Snow_intcp, 'snow_intcp')
            CALL is_eof(Snow_intcp_unit, Snow_intcp_next_yr, Snow_intcp_next_mo, Snow_intcp_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Covden_sum_flag==ACTIVE ) THEN
        IF ( Covden_sum_next_mo/=0 ) THEN
          IF ( Covden_sum_next_yr==Nowyear .AND. Covden_sum_next_mo==Nowmonth .AND. Covden_sum_next_day==Nowday ) THEN
            READ ( Covden_sum_unit, *, IOSTAT=ios ) Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day, Temp
            if (ios /= 0) call error_stop('reading covden_sum dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Covden_sum, 'covden_sum')
            CALL is_eof(Covden_sum_unit, Covden_sum_next_yr, Covden_sum_next_mo, Covden_sum_next_day)
          ENDIF
        ENDIF
      ENDIF
      IF ( Covden_win_flag==ACTIVE ) THEN
        IF ( Covden_win_next_mo/=0 ) THEN
          IF ( Covden_win_next_yr==Nowyear .AND. Covden_win_next_mo==Nowmonth .AND. Covden_win_next_day==Nowday ) THEN
            READ ( Covden_win_unit, *, IOSTAT=ios ) Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day, Temp
            if (ios /= 0) call error_stop('reading covden_win dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Covden_win, 'covden_win')
            CALL is_eof(Covden_win_unit, Covden_win_next_yr, Covden_win_next_mo, Covden_win_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_covtype_flag==ACTIVE ) THEN
        IF ( Covtype_next_mo/=0 ) THEN
          IF ( Covtype_next_yr==Nowyear .AND. Covtype_next_mo==Nowmonth .AND. Covtype_next_day==Nowday ) THEN
            READ ( Covtype_unit, *, IOSTAT=ios ) Covtype_next_yr, Covtype_next_mo, Covtype_next_day, Itemp
            if (ios /= 0) call error_stop('reading cov_type dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Cov_type, 'cov_type')
            Cov_type = Itemp
            CALL is_eof(Covtype_unit, Covtype_next_yr, Covtype_next_mo, Covtype_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_potet_flag>0 ) THEN  ! fix so only current month is updated
        IF ( Potetcoef_next_mo/=0 ) THEN
          IF ( Potetcoef_next_yr==Nowyear .AND. Potetcoef_next_mo==Nowmonth .AND. Potetcoef_next_day==Nowday ) THEN
            READ ( Potetcoef_unit, *, IOSTAT=ios ) Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day, Temp
            if (ios /= 0) call error_stop('reading potet coefficient dynamic parameter file', ERROR_dynamic)
            IF ( Et_flag==potet_jh_module .AND. Dyn_potet_flag/=1 ) THEN ! allow values to be < 0.0 for potet_jh_hru parameter
              CALL write_dynparam_potet(Output_unit, Nhru, Updated_hrus, Temp, Potet_coef(:,Nowmonth), 'potet_coef')
            ELSE
              CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Potet_coef(:,Nowmonth), 'potet_coef')
            ENDIF
            CALL is_eof(Potetcoef_unit, Potetcoef_next_yr, Potetcoef_next_mo, Potetcoef_next_day)

            IF ( Et_flag==potet_jh_module ) THEN ! potet_jh
              IF ( Dyn_potet_flag==1 ) THEN
                Jh_coef = Potet_coef
              ELSE
                DO i = 1, Nhru
                  Jh_coef_hru(i) = Potet_coef(i,Nowmonth)
                ENDDO
              ENDIF
            ELSEIF ( Et_flag==climate_hru_module ) THEN
              Potet_cbh_adj = Potet_coef
            ELSEIF ( Et_flag==potet_pm_module ) THEN
              IF ( Dyn_potet_flag==1 ) THEN
                Pm_n_coef = Potet_coef
              ELSE
                Pm_d_coef = Potet_coef
              ENDIF
            ELSEIF ( Et_flag==potet_pt_module ) THEN
              Pt_alpha = Potet_coef
            ELSEIF ( Et_flag==potet_hs_module ) THEN
              Hs_krs = Potet_coef
            ELSEIF ( Et_flag==potet_hamon_module ) THEN
              Hamon_coef = Potet_coef
            ELSEIF ( Et_flag==potet_pan_module ) THEN
              Epan_coef = Potet_coef
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF ( Transpbeg_flag==ACTIVE ) THEN
        IF ( Transpbeg_next_mo/=0 ) THEN
          IF ( Transpbeg_next_yr==Nowyear .AND. Transpbeg_next_mo==Nowmonth .AND. Transpbeg_next_day==Nowday ) THEN
            READ ( Transpbeg_unit, *, IOSTAT=ios ) Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day, Itemp
            if (ios /= 0) call error_stop('reading transp_beg dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_beg, 'transp_beg')
            CALL is_eof(Transpbeg_unit, Transpbeg_next_yr, Transpbeg_next_mo, Transpbeg_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Transpend_flag==ACTIVE ) THEN
        IF ( Transpend_next_mo/=0 ) THEN
          IF ( Transpend_next_yr==Nowyear .AND. Transpend_next_mo==Nowmonth .AND. Transpend_next_day==Nowday ) THEN
            READ ( Transpend_unit, *, IOSTAT=ios ) Transpend_next_yr, Transpend_next_mo, Transpend_next_day, Itemp
            if (ios /= 0) call error_stop('reading transp_end dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_end, 'transp_end')
            CALL is_eof(Transpend_unit, Transpend_next_yr, Transpend_next_mo, Transpend_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Fallfrost_flag==ACTIVE ) THEN
        IF ( Fallfrost_next_mo/=0 ) THEN
          IF ( Fallfrost_next_yr==Nowyear .AND. Fallfrost_next_mo==Nowmonth .AND. Fallfrost_next_day==Nowday ) THEN
            READ ( Fallfrost_unit, *, IOSTAT=ios ) Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day, Itemp
            if (ios /= 0) call error_stop('reading fall_frost dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Fall_frost, 'fall_frost')
            CALL is_eof(Fallfrost_unit, Fallfrost_next_yr, Fallfrost_next_mo, Fallfrost_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Springfrost_flag==ACTIVE ) THEN
        IF ( Springfrost_next_mo/=0 ) THEN
          IF ( Springfrost_next_yr==Nowyear .AND. Springfrost_next_mo==Nowmonth .AND. Springfrost_next_day==Nowday ) THEN
            READ ( Springfrost_unit, *, IOSTAT=ios ) Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day, Itemp
            if (ios /= 0) call error_stop('reading spring_frost dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Spring_frost, 'spring_frost')
            CALL is_eof(Springfrost_unit, Springfrost_next_yr, Springfrost_next_mo, Springfrost_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_radtrncf_flag==ACTIVE ) THEN
        IF ( Rad_trncf_next_mo/=0 ) THEN
          IF ( Rad_trncf_next_yr==Nowyear .AND. Rad_trncf_next_mo==Nowmonth .AND. Rad_trncf_next_day==Nowday ) THEN
            READ ( Rad_trncf_unit, *, IOSTAT=ios ) Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day, Temp
            if (ios /= 0) call error_stop('reading rad_trnch dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Rad_trncf, 'rad_trncf')
            CALL is_eof(Rad_trncf_unit, Rad_trncf_next_yr, Rad_trncf_next_mo, Rad_trncf_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_snareathresh_flag==ACTIVE ) THEN
        IF ( Snarea_thresh_next_mo/=0 ) THEN
          IF ( Snarea_thresh_next_yr==Nowyear .AND. Snarea_thresh_next_mo==Nowmonth .AND. Snarea_thresh_next_day==Nowday ) THEN
            READ ( Snarea_thresh_unit, *, IOSTAT=ios ) Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day, Temp
            if (ios /= 0) call error_stop('reading snarea_thresh dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Snarea_thresh, 'snarea_thresh')
            CALL is_eof(Snarea_thresh_unit, Snarea_thresh_next_yr, Snarea_thresh_next_mo, Snarea_thresh_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_perv_flag==ACTIVE ) THEN
        IF ( Sro_to_dprst_next_mo/=0 ) THEN
          IF ( Sro_to_dprst_next_yr==Nowyear .AND. Sro_to_dprst_next_mo==Nowmonth .AND. Sro_to_dprst_next_day==Nowday ) THEN
            READ ( Sro_to_dprst_unit, *, IOSTAT=ios ) Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day, Temp
            if (ios /= 0) call error_stop('reading sro_to_dprst dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Sro_to_dprst_perv, 'sro_to_dprst_perv')
            CALL is_eof(Sro_to_dprst_unit, Sro_to_dprst_next_yr, Sro_to_dprst_next_mo, Sro_to_dprst_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_sro2dprst_imperv_flag==ACTIVE ) THEN
        IF ( Sro_to_imperv_next_mo/=0 ) THEN
          IF ( Sro_to_imperv_next_yr==Nowyear .AND. Sro_to_imperv_next_mo==Nowmonth .AND. Sro_to_imperv_next_day==Nowday ) THEN
            READ ( Sro_to_imperv_unit, *, IOSTAT=ios ) Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day, Temp
            if (ios /= 0) call error_stop('reading sro_to_imperv dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam(Output_unit, Nhru, Updated_hrus, Temp, Sro_to_dprst_imperv, 'sro_to_dprst_imperv')
            CALL is_eof(Sro_to_imperv_unit, Sro_to_imperv_next_yr, Sro_to_imperv_next_mo, Sro_to_imperv_next_day)
          ENDIF
        ENDIF
      ENDIF

      IF ( Dyn_transp_on_flag==ACTIVE ) THEN
        IF ( Transp_event_next_mo/=0 ) THEN
          IF ( Transp_event_next_yr==Nowyear .AND. Transp_event_next_mo==Nowmonth .AND. Transp_event_next_day==Nowday ) THEN
            READ ( Transp_event_unit, *, IOSTAT=ios ) Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day, Itemp
            if (ios /= 0) call error_stop('reading transp_on dynamic parameter file', ERROR_dynamic)
            CALL write_dynparam_int(Output_unit, Nhru, Updated_hrus, Itemp, Transp_on, 'transp_on_event')
            CALL is_eof(Transp_event_unit, Transp_event_next_yr, Transp_event_next_mo, Transp_event_next_day)
          ENDIF
        ENDIF
      ENDIF

      END FUNCTION dynparamrun

!***********************************************************************
!     Values are read in, Parm are last, Values are updated or old
!***********************************************************************
      SUBROUTINE write_dynoutput(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, DEBUG_less
      USE PRMS_MODULE, ONLY: Print_debug, Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Param(Dim)
      REAL, INTENT(INOUT) :: Values(Dim) ! dynamic values with old non-updated
      INTEGER, INTENT(OUT) :: Updated_hrus(Dim)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO i = 1, Dim
        IF ( Values(i)<0.0 ) THEN
          Values(i) = Param(i)
        ELSEIF ( Values(i)/=Param(i) ) THEN
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>DEBUG_minimum ) THEN
        WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
        IF ( Print_debug>DEBUG_less ) THEN
          WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
          WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
        ENDIF
      ENDIF
      END SUBROUTINE write_dynoutput

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam_int(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, DEBUG_less
      USE PRMS_MODULE, ONLY: Print_debug, Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      INTEGER, INTENT(IN) :: Values(Dim)
      INTEGER, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Dim)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO i = 1, Dim
        IF ( Values(i)<0 ) CYCLE
        IF ( Values(i)/=Param(i) ) THEN
          num = num + 1
          Updated_hrus(num) = i
          Param(i) = Values(i)
        ENDIF
      ENDDO
      IF ( Print_debug>DEBUG_minimum ) THEN
        WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
        IF ( Print_debug>DEBUG_less ) THEN
          WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
          WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
        ENDIF
      ENDIF
      END SUBROUTINE write_dynparam_int

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, DEBUG_less
      USE PRMS_MODULE, ONLY: Print_debug, Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Values(Dim)
      REAL, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Dim)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO i = 1, Dim
        IF ( Values(i)<0.0 ) CYCLE
        IF ( Values(i)/=Param(i) ) THEN
          Param(i) = Values(i)
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>DEBUG_minimum ) THEN
        WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
        IF ( Print_debug>DEBUG_less ) THEN
          WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
          WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
        ENDIF
      ENDIF
      END SUBROUTINE write_dynparam

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
!      SUBROUTINE write_dynparam_dble(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
!      USE PRMS_MODULE, ONLY: Print_debug, Nowyear, Nowmonth, Nowday
!      IMPLICIT NONE
! Arguments
!      INTEGER, INTENT(IN) :: Output_unit, Dim
!      REAL, INTENT(IN) :: Values(Dim)
!      DOUBLE PRECISION, INTENT(INOUT) :: Param(Dim)
!      INTEGER, INTENT(OUT) :: Updated_hrus(Dim)
!      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Functions
!      INTRINSIC :: DBLE, SNGL
! Local Variables
!      INTEGER i, num
!***********************************************************************
!      Updated_hrus = 0
!      num = 0
!      DO i = 1, Dim
!        IF ( Values(i)<0.0 ) CYCLE
!        IF ( Values(i)/=SNGL(Param(i)) ) THEN
!          Param(i) = DBLE( Values(i) )
!          num = num + 1
!          Updated_hrus(num) = i
!        ENDIF
!      ENDDO
!      IF ( Print_debug>DEBUG_minimum ) THEN
!        WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
!        IF ( Print_debug>DEBUG_less ) THEN
!          WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
!          WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
!        ENDIF
!      ENDIF
!      END SUBROUTINE write_dynparam_dble

!***********************************************************************
!     Values are read in, Parm are are updated or old
!***********************************************************************
      SUBROUTINE write_dynparam_potet(Output_unit, Dim, Updated_hrus, Values, Param, Param_name)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, DEBUG_less
      USE PRMS_MODULE, ONLY: Print_debug, Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Output_unit, Dim
      REAL, INTENT(IN) :: Values(Dim)
      REAL, INTENT(INOUT) :: Param(Dim)
      INTEGER, INTENT(OUT) :: Updated_hrus(Dim)
      CHARACTER(LEN=*), INTENT(IN) :: Param_name
! Local Variables
      INTEGER i, num
!***********************************************************************
      Updated_hrus = 0
      num = 0
      DO i = 1, Dim
        IF ( Values(i)/=Param(i) ) THEN
          Param(i) = Values(i)
          num = num + 1
          Updated_hrus(num) = i
        ENDIF
      ENDDO
      IF ( Print_debug>DEBUG_minimum ) THEN
        WRITE ( Output_unit, '(/,3A,I4,2("/",I2.2))' ) 'Parameter ', Param_name, ' updated on: ', Nowyear, Nowmonth, Nowday
        IF ( Print_debug>DEBUG_less ) THEN
          WRITE ( Output_unit, '(/,A,I5,2("/",I2.2))' ) 'List of updated HRUs; Date:', Nowyear, Nowmonth, Nowday
          WRITE ( Output_unit, '(20I7)' ) (Updated_hrus(i), i=1,num)
        ENDIF
      ENDIF
      END SUBROUTINE write_dynparam_potet

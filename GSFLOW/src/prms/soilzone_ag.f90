!***********************************************************************
! Computes inflows to and outflows from soil zone of each HRU and
! includes inflows from infiltration, groundwater, and upslope HRUs,
! and outflows to gravity drainage, interflow, and surface runoff to
! downslope HRUs; merge of smbal_prms and ssflow_prms with enhancements
!
! Daily accounting for soil zone;
!    adds infiltration
!    computes et
!    computes recharge of soil zone
!    computes interflow to stream or cascade
!    adjusts storage in soil zone
!    sends dunnian runoff to stream or cascade by adding to sroff
!    computes drainage to groundwater
!***********************************************************************
! FORTRAN module for soilzone_ag
!***********************************************************************
      MODULE PRMS_SOILZONE_AG

      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC_AG = 'Soilzone Computations'
      character(len=11), parameter :: MODNAME_AG = 'soilzone_ag'
      character(len=*), parameter :: Version_soilzone_ag = '2021-10-28'
      INTEGER, SAVE :: Soil_iter, HRU_id
      DOUBLE PRECISION, SAVE :: Basin_ag_soil_to_gw, Basin_ag_up_max, Basin_ag_gvr2sm
      DOUBLE PRECISION, SAVE :: Basin_ag_actet, Last_ag_soil_moist, Basin_ag_soil_rechr
      !DOUBLE PRECISION, SAVE :: Basin_ag_ssstor, Basin_ag_recharge, Basin_ag_ssflow
      REAL, SAVE, ALLOCATABLE :: Ag_soil_to_gw(:), Ag_soil_to_ssr(:), Ag_dunnian(:), Ag_replenish_frac(:)
      REAL, SAVE, ALLOCATABLE :: It0_ag_soil_rechr(:), It0_ag_soil_moist(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_slow_flow(:), Ag_ssres_in(:), Ag_water_maxin(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_ssr_to_gw(:), Ag_slow_stor(:), Ag_recharge(:)
      !REAL, SAVE, ALLOCATABLE :: Ag_ssres_stor(:), Ag_ssres_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ag_upslope_dunnian(:)
!   Agriculture Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_saturated(:)
      DOUBLE PRECISION, SAVE :: Basin_agwaterin
      REAL, SAVE, ALLOCATABLE :: Ag_hortonian(:), Unused_ag_et(:), Ag_soil2gvr(:), Ag_soilwater_deficit(:)
      REAL, SAVE, ALLOCATABLE :: Ag_actet(:), Ag_irrigation_add(:), Ag_gvr2sm(:), Ag_irrigation_add_vol(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_lower(:), Ag_soil_lower_stor_max(:), Ag_potet_rechr(:), Ag_potet_lower(:)
      INTEGER, SAVE :: total_iters, iter_nonconverge
      real, save :: unsatisfied_big
      ! parameters
! have covden a monthly, later
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_type(:) !, Ag_crop_type(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soilwater_deficit_min(:), Ag_covden_sum(:,:), Ag_covden_win(:,:)
!      REAL, SAVE, ALLOCATABLE :: Ag_sat_threshold(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_rechr_max_frac(:) ! Ag_crop_coef later, will specify PET
      !REAL, SAVE, ALLOCATABLE :: Ag_snowinfil_max(:), Ag_ssstor_init_frac(:)
      INTEGER, SAVE :: max_soilzone_ag_iter
      REAL, SAVE :: soilzone_aet_converge

      END MODULE PRMS_SOILZONE_AG

!***********************************************************************
!     Main soilzone_ag routine
!***********************************************************************
      INTEGER FUNCTION soilzone_ag()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, OFF, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
! Functions
      INTEGER, EXTERNAL :: szdecl, szinit, szrun_ag, szdecl_ag, szinit_ag
      EXTERNAL :: soilzone_restart_ag
!***********************************************************************
      soilzone_ag = 0

      IF ( Process_flag==RUN ) THEN
        soilzone_ag = szrun_ag()
      ELSEIF ( Process_flag==DECL ) THEN
        soilzone_ag = szdecl()
        soilzone_ag = szdecl_ag()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL soilzone_restart_ag(READ_INIT)
        soilzone_ag = szinit()
        soilzone_ag = szinit_ag()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL soilzone_restart_ag(SAVE_INIT)
      ENDIF

      END FUNCTION soilzone_ag

!***********************************************************************
!     szdecl_ag - set up parameters for agriculture soil zone computations
!   Declared Parameters
!     sat_threshold, ssstor_init_frac fastcoef_lin, fastcoef_sq
!     ssr2gw_rate, ssr2gw_exp, soil2gw_max, soil_type, ag_soil_type, ag_soilwater_deficit_min
!     soil_rechr_max_frac, soil_rechr_init_frac, soil_moist_max, soil_moist_init_frac
!     pref_flow_den, slowcoef_lin, cov_type
!     hru_area, slowcoef_sq, gvr_hru_id
!***********************************************************************
      INTEGER FUNCTION szdecl_ag()
      USE PRMS_CONSTANTS, ONLY: OFF, ACTIVE, DOCUMENTATION, PRMS_AG, MONTHS_PER_YEAR
      USE PRMS_MODULE, ONLY: Nhru, Cascade_flag, GSFLOW_flag, Model
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, getdim
      EXTERNAL :: read_error, print_module, PRMS_open_module_file, error_stop, declvar_dble, declvar_real, declvar_int
!***********************************************************************
      szdecl_ag = 0

      total_iters = 0

      CALL print_module(MODDESC_AG, MODNAME_AG, Version_soilzone_ag)

! Agriculture variables and parameters
      ALLOCATE ( Ag_soil_to_gw(Nhru), Ag_soil_to_ssr(Nhru) )
      ALLOCATE ( Ag_dunnian(Nhru) )
      IF ( Cascade_flag>OFF ) ALLOCATE ( Ag_upslope_dunnian(Nhru) )

      ALLOCATE ( Ag_actet(Nhru) )
      CALL declvar_real(MODNAME, 'ag_actet', 'nhru', Nhru, &
     &     'Actual ET for agriculture reservoir for each HRU', &
     &     'inches', Ag_actet)

      ALLOCATE ( Unused_ag_et(Nhru) )
      CALL declvar_real(MODNAME, 'unused_ag_et', 'nhru', Nhru, &
     &     'Actual ET for agriculture reservoir for each HRU', &
     &     'inches', Unused_ag_et)

      ALLOCATE (  Ag_hortonian(Nhru) )
      CALL declvar_real(MODNAME, 'ag_hortonian', 'nhru', Nhru, &
     &     'Hortonian surface runoff that flows to the stream network from the agricultural fraction of each HRU', &
     &     'inches', Ag_hortonian)

      ALLOCATE (  Ag_soil2gvr(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soil2gvr', 'nhru', Nhru, &
     &     'Excess capillary water that flows to the gravity reservoir from the agricultural fraction of each HRU', &
     &     'inches', Ag_soil2gvr)

      CALL declvar_dble(MODNAME, 'basin_agwaterin', 'one', 1, &
     &     'Basin area-weighted average infiltration,'// &
     &     ' cascading interflow and Dunnian flow added to agriculture reservoir storage', &
     &     'inches', Basin_agwaterin)

      ALLOCATE ( Ag_irrigation_add(Nhru) )
      CALL declvar_real(MODNAME, 'ag_irrigation_add', 'nhru', Nhru, &
     &     'Irrigation water added to agriculture fraction when ag_actet < PET_external for each HRU', &
     &     'inches', Ag_irrigation_add)

      ALLOCATE ( Ag_soilwater_deficit(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soilwater_deficit', 'nhru', Nhru, &
     &     'Soil-water deficit of agriculture fraction for each HRU', &
     &     'inches', Ag_soilwater_deficit)

      ALLOCATE ( Ag_irrigation_add_vol(Nhru) )
      CALL declvar_real(MODNAME, 'ag_irrigation_add_vol', 'nhru', Nhru, &
     &     'Irrigation water added to agriculture fraction when ag_actet < PET_external for each HRU', &
     &     'acre-inches', Ag_irrigation_add_vol)

      ALLOCATE ( Ag_soil_lower(Nhru), Ag_soil_lower_stor_max(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soil_lower', 'nhru', Nhru, &
     &   'Storage in the lower zone of the agriculture'// &
     &   ' reservoir that is only available for transpiration for each HRU', &
     &   'inches', Ag_soil_lower)

      IF ( GSFLOW_flag==ACTIVE .OR. MODEL==PRMS_AG .OR. Model==DOCUMENTATION ) THEN
        ALLOCATE ( Ag_gvr2sm(Nhru), Ag_replenish_frac(Nhru) )
        CALL declvar_real(MODNAME, 'ag_gvr2sm', 'nhru', Nhru, &
     &       'Gravity flow to irrigated soil replenishment for each HRU', &
     &       'inches', Ag_gvr2sm)
      ENDIF

      ALLOCATE ( Ag_potet_lower(Nhru) ) !, Ag_water_maxin(Nhru) )
      CALL declvar_real(MODNAME, 'ag_potet_lower', 'nhru', Nhru, &
     &     'Potential ET in the lower zone of the agriculture reservoir for each HRU', &
     &     'inches', Ag_potet_lower)

      ALLOCATE ( Ag_potet_rechr(Nhru) )
      CALL declvar_real(MODNAME, 'ag_potet_rechr', 'nhru', Nhru, &
     &     'Potential ET in the recharge zone of the agriculture reservoir for each HRU', &
     &     'inches', Ag_potet_rechr)

      ALLOCATE ( Ag_soil_saturated(Nhru) )
      CALL declvar_int(MODNAME, 'ag_soil_saturated', 'nhru', Nhru, &
     &     'Flag set if infiltration saturates capillary reservoir (0=no, 1=yes)', &
     &     'none', Ag_soil_saturated)
      Ag_soil_saturated = OFF

      IF ( declparam(MODNAME, 'max_soilzone_ag_iter', 'one', 'integer', &
     &     '10', '1', '9999', &
     &     'Maximum number of iterations to optimize computed AET and input AET', &
     &     'Maximum number of iterations to optimize computed AET and input AET', &
     &     'none')/=0 ) CALL read_error(1, 'max_soilzone_ag_iter')

      IF ( declparam(MODNAME, 'soilzone_aet_converge', 'one', 'real', &
     &     '0.01', '0.0', '1.0', &
     &     'Convergence criteria to iterate computed AET compared to input AET', &
     &     'Convergence criteria to iterate computed AET compared to input AET', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'soilzone_aet_converge')

      ALLOCATE ( Ag_soil_type(Nhru) )
      IF ( declparam(MODNAME, 'ag_soil_type', 'nhru', 'integer', &
     &     '2', '1', '3', &
     &     'Agriculture soil type', 'Soil type of agriculture in each HRU (1=sand; 2=loam; 3=clay)', &
     &     'none')/=0 ) CALL read_error(1, 'ag_soil_type')

      ALLOCATE ( Ag_soilwater_deficit_min(Nhru) )
      IF ( declparam(MODNAME, 'ag_soilwater_deficit_min', 'nhru', 'real', &
     &     '0.0', '0.0', '1.0', &
     &     'Minimum soil-water deficit to begin agriculture irrigaition', &
     &     'Minimum soil-water deficit fraction to begin agriculture irrigaition', &
     &     'fraction')/=0 ) CALL read_error(1, 'ag_soilwater_deficit_min')

     ! ALLOCATE ( Ag_sat_threshold(Nhru) )
     ! IF ( declparam(MODNAME, 'ag_sat_threshold', 'nhru', 'real', &
     !&     '999.0', '0.00001', '999.0', &
     !&     'Soil saturation threshold, above field-capacity threshold of agriculture reservoir', &
     !&     'Water holding capacity of the gravity and preferential-'// &
     !&     'flow reservoirs; difference between field capacity and'// &
     !&     ' total soil saturation for each HRU', &
     !&     'inches')/=0 ) CALL read_error(1, 'ag_sat_threshold')

!      ALLOCATE ( Ag_crop_type(Nhru) ) ! find Mastin's code on different crops
!      IF ( declparam(MODNAME, 'ag_crop_type', 'nhru', 'integer', &
!     &     '3', '0', '4', &
!     &     'Agriculture cover type designation for each HRU', &
!     &     'Vegetation cover type for agriculture in each HRU (0=none;'// &
!     &     ' 1=grasses; 2=grain; 3=trees; 4=vegetable)', &
!     &     'none')/=0 ) CALL read_error(1, 'ag_crop_type')

        ! use existing covden_sum, covden_win
      ALLOCATE ( Ag_covden_sum(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'ag_covden_sum', 'nhru,nmonths', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Summer vegetation cover density for agriculture crop type', &
     &     'Summer vegetation cover density for the agriculture crop type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'ag_covden_sum')

      ALLOCATE ( Ag_covden_win(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'ag_covden_win', 'nhru,nmonths', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Winter vegetation cover density for crop type', &
     &     'Winter vegetation cover density for the crop type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'ag_covden_win')

      END FUNCTION szdecl_ag

!***********************************************************************
!     szinit_ag - Initialize soilzone module - get parameter values,
!                 set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, LAKE, GLACIER, INACTIVE, OFF, MONTHS_PER_YEAR
      USE PRMS_MODULE, ONLY: Ag_package, Init_vars_from_file, Nhru, Agriculture_soilzone_flag
      USE PRMS_SOILZONE, ONLY: MODNAME, Iter_aet, Soil2gw_max
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Hru_type, Basin_area_inv, Ag_area, Covden_win, Covden_sum
      USE PRMS_FLOWVARS, ONLY: Basin_ag_soil_moist, Ag_soil_moist, Ag_soil_rechr, Ag_soil_moist_max, Ag_soil_rechr_max
      IMPLICIT NONE
! Functions
      EXTERNAL :: init_basin_vars, checkdim_bounded_limits, error_stop
      INTEGER, EXTERNAL :: getparam_int, getparam_real
      INTRINSIC :: MIN, DBLE
! Local Variables
      INTEGER :: ihru
!***********************************************************************
      szinit_ag = 0

!??? figure out what to save in restart file ???
      IF ( getparam_int(MODNAME, 'max_soilzone_ag_iter', 1, max_soilzone_ag_iter)/=0 ) &
     &     CALL read_error(2, 'max_soilzone_ag_iter')
      IF ( getparam_real(MODNAME, 'soilzone_aet_converge', 1, soilzone_aet_converge)/=0 ) &
     &     CALL read_error(2, 'soilzone_aet_converge')
      IF ( getparam_int(MODNAME, 'ag_soil_type', Nhru, Ag_soil_type)/=0 ) CALL read_error(2, 'ag_soil_type')
      IF ( getparam_real(MODNAME, 'ag_soilwater_deficit_min', Nhru, Ag_soilwater_deficit_min)/=0 ) &
     &     CALL read_error(2, 'ag_soilwater_deficit_min')
!      IF ( getparam_int(MODNAME, 'ag_crop_type', Nhru, Ag_crop_type)/=0 ) CALL read_error(2, 'ag_crop_type')
      IF ( getparam_real(MODNAME, 'ag_covden_sum', Nhru*MONTHS_PER_YEAR, Ag_covden_sum)/=0 ) CALL read_error(2, 'ag_covden_sum')
      IF ( Ag_covden_sum(1,1)<0.0 ) Ag_covden_sum = Covden_sum
      IF ( getparam_real(MODNAME, 'ag_covden_win', Nhru*MONTHS_PER_YEAR, Ag_covden_win)/=0 ) CALL read_error(2, 'ag_covden_win')
      IF ( Ag_covden_win(1,1)<0.0 ) Ag_covden_win = Covden_win
      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) Ag_soil_lower = 0.0
      Basin_agwaterin = 0.0D0
      Basin_ag_soil_to_gw = 0.0D0
      Basin_ag_actet = 0.0D0
      Basin_ag_gvr2sm = 0.0D0
      ! dimensioned nhru
      Ag_soil_to_gw = 0.0
      Ag_soil_to_ssr = 0.0
      Ag_dunnian = 0.0
      Ag_hortonian = 0.0
      Ag_soil2gvr = 0.0
      IF ( Iter_aet==ACTIVE ) THEN
        Ag_irrigation_add = 0.0
        Ag_irrigation_add_vol = 0.0
        Unused_ag_et = 0.0
        Ag_soilwater_deficit = 0.0
      ENDIF
      Ag_soil_lower_stor_max = 0.0
      Ag_potet_lower = 0.0
      Ag_potet_rechr = 0.0
      Basin_ag_soil_moist = 0.0D0
      Basin_ag_soil_rechr = 0.0D0
      Ag_replenish_frac = 0.0
      Ag_gvr2sm = 0.0
      DO ihru = 1, Nhru
        ! make sure LAKE, INACTIVE, GLACIER have agriculture values of 0
        IF ( Hru_type(ihru)==LAKE .OR. Hru_type(ihru)==INACTIVE .OR. Hru_type(ihru)==GLACIER ) Ag_area(ihru) = 0.0
        IF ( Ag_area(ihru)>0.0 ) THEN
          IF ( Soil2gw_max(ihru)>0.0 ) THEN
            print *, 'soil2gw_max>0 for ag_frac, HRU:', ihru, Soil2gw_max(ihru)
            !Soil2gw_max(ihru) = 0.0
          ENDIF
          Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(ihru)*Ag_area(ihru) )
          Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(ihru)*Ag_area(ihru) )
        ELSE
          Ag_soil_moist_max(ihru) = 0.0
          Ag_soil_rechr_max(ihru) = 0.0
          Ag_soil_moist(ihru) = 0.0
          Ag_soil_rechr(ihru) = 0.0
        ENDIF
        IF ( Ag_soil_moist_max(ihru)>0.0 ) Ag_replenish_frac(ihru) = Ag_soil_rechr_max(ihru)/Ag_soil_moist_max(ihru)
      ENDDO
      Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
      Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv
      Last_ag_soil_moist = Basin_ag_soil_moist

      IF ( Ag_package==ACTIVE .OR. Agriculture_soilzone_flag==ACTIVE ) ALLOCATE ( It0_ag_soil_rechr(Nhru), It0_ag_soil_moist(Nhru) )
      Ag_actet = 0.0

      Soil_iter = 1
      iter_nonconverge = 0
      unsatisfied_big = 0.0

      END FUNCTION szinit_ag

!***********************************************************************
!     szrun_ag - Does soil water balance for each HRU, adds in infiltration
!                then computes actual et and apportions remainder between
!                recharge of soil moisture, soil storage available for
!                interflow, excess routed to stream,
!                and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, NEARZERO, LAND, LAKE, SWALE, GLACIER, &
     &    DEBUG_less, DEBUG_WB, ERROR_param, CASCADE_OFF
      USE PRMS_MODULE, ONLY: Nlake, Print_debug, Dprst_flag, Cascade_flag, GSFLOW_flag, &
     &    Kkiter, Frozen_flag, Soilzone_add_water_use, Hru_ag_irr, Ag_package, Call_cascade, PRMS_land_iteration_flag, &
     &    Soilzone_aet_flag, Nowmonth, Nowyear, Nowday, Iter_aet_flag, Agriculture_soilzone_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Hru_type, Hru_perv, Hru_frac_perv, Hru_storage, &
     &    Hru_route_order, Active_hrus, Basin_area_inv, Hru_area, &
     &    Lake_hru_id, Cov_type, Numlake_hrus, Hru_area_dble, Ag_frac, Ag_area, Ag_cov_type
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Transp_on, Potet, Basin_potet, Basin_transp_on
! WARNING!!! Sroff, Basin_sroff, and Strm_seg_in can be updated
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_actet, Hru_actet, &
     &    Ssres_flow, Soil_to_gw, Basin_soil_to_gw, Ssr_to_gw, &
     &    Soil_to_ssr, Basin_lakeevap, Basin_perv_et, Basin_swale_et, &
     &    Sroff, Soil_moist_max, Infil, Soil_rechr_max, Ssres_in, &
     &    Basin_soil_moist, Basin_ssstor, Slow_stor, Slow_flow, Pkwater_equiv, &
     &    Ssres_stor, Soil_moist, Sat_threshold, Soil_rechr, Basin_sroff, Basin_lake_stor, &
     &    Ag_soil_rechr, Ag_soil_moist, Ag_soil_rechr_max, Ag_soil_moist_max, Basin_ag_soil_moist
      USE PRMS_INTCP, ONLY: Hru_intcpstor !, Net_apply
      USE PRMS_SRUNOFF, ONLY: Hru_impervstor, Dprst_stor_hru
      USE PRMS_WATER_USE, ONLY: Soilzone_gain, Soilzone_gain_hru
      USE PRMS_CLIMATE_HRU, ONLY: AET_external, PET_external
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_INTCP, ONLY: Hru_intcpevap
      USE PRMS_SNOW, ONLY: Snowcov_area, Snow_evap
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Strm_seg_in, Dprst_evap_hru, Dprst_seep_hru, Frozen, Infil_ag
      IMPLICIT NONE
! Functions
      INTRINSIC :: MIN, ABS, MAX, SNGL, DBLE
      EXTERNAL :: compute_soilmoist, compute_szactet, compute_cascades, compute_gravflow
      EXTERNAL :: compute_interflow, compute_gwflow, init_basin_vars, print_date, check_gvr_sm
! Local Variables
      INTEGER :: i, k, update_potet, compute_lateral, perv_on_flag
      REAL :: dunnianflw, interflow, perv_area, harea
      REAL :: dnslowflow, dnpreflow, dndunn, availh2o, avail_potet, hruactet, ag_hruactet
      REAL :: gvr_maxin, topfr, depth !, tmp
      REAL :: dunnianflw_pfr, dunnianflw_gvr, pref_flow_maxin
      REAL :: perv_frac, capacity, capwater_maxin, ssresin
      REAL :: cap_upflow_max, unsatisfied_et, pervactet, prefflow, ag_water_maxin
      REAL :: ag_upflow_max, ag_capacity, excess, agfrac, ag_soil2gw, ag_avail_potet, ag_potet
      REAL :: ag_AETtarget, ag_avail_targetAET, cap_ag_water_maxin
      DOUBLE PRECISION :: gwin, frac
      INTEGER :: cfgi_frozen_hru
      INTEGER :: num_hrus_ag_iter, ag_on_flag, keep_iterating, add_estimated_irrigation
!***********************************************************************
      szrun_ag = 0

      IF ( Print_debug==DEBUG_WB ) THEN
        Soil_moist_ante = Soil_moist
        Ssres_stor_ante = Ssres_stor
        Last_soil_moist = Basin_soil_moist
        Last_ssstor = Basin_ssstor
      ENDIF

! It0 variables used with MODFLOW integration to save iteration states.
      IF ( GSFLOW_flag==ACTIVE .OR. PRMS_land_iteration_flag==ACTIVE .OR. Iter_aet==ACTIVE ) THEN
        IF ( Kkiter>1 ) THEN
          Ssres_stor = It0_ssres_stor
          Slow_stor = It0_slow_stor
          IF ( GSFLOW_flag==ACTIVE ) Gravity_stor_res = It0_gravity_stor_res
          IF ( Pref_flag==ACTIVE ) Pref_flow_stor = It0_pref_flow_stor
          IF ( Nlake>0 ) Potet = It0_potet
        ELSE
          It0_ssres_stor = Ssres_stor
          It0_slow_stor = Slow_stor
          IF ( GSFLOW_flag==ACTIVE ) THEN
            It0_gravity_stor_res = Gravity_stor_res
            Gw2sm_grav = 0.0
          ENDIF
          IF ( Pref_flag==ACTIVE ) It0_pref_flow_stor = Pref_flow_stor
          IF ( Nlake>0 ) It0_potet = Potet
        ENDIF
      ENDIF
      IF ( GSFLOW_flag==ACTIVE .AND. PRMS_land_iteration_flag==OFF ) THEN
        IF ( Kkiter>1 ) THEN
          ! states saved in srunoff when PRMS_land_iteration_flag = ACTIVE
          Soil_rechr = It0_soil_rechr
          Soil_moist = It0_soil_moist
          ! computed in srunoff
          Sroff = It0_sroff
          IF ( Call_cascade==ACTIVE ) Strm_seg_in = It0_strm_seg_in
        ELSE
          It0_soil_rechr = Soil_rechr
          It0_soil_moist = Soil_moist
          It0_sroff = Sroff
          IF ( Call_cascade==ACTIVE ) It0_strm_seg_in = Strm_seg_in
        ENDIF
      ENDIF
      IF ( GSFLOW_flag==ACTIVE ) Sm2gw_grav = 0.0
      IF ( Kkiter>1 ) THEN
        Basin_soil_moist = It0_basin_soil_moist
        Basin_ssstor = It0_basin_ssstor
      ELSE
        It0_basin_soil_moist = Basin_soil_moist
        It0_basin_ssstor = Basin_ssstor
      ENDIF

      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Upslope_interflow = 0.0D0
        Upslope_dunnianflow = 0.0D0
        IF ( Numlake_hrus>0 ) THEN
          Lakein_sz = 0.0D0
          Basin_lakeinsz = 0.0D0
        ENDIF
      ENDIF

      IF ( Iter_aet==ACTIVE ) THEN
        Ag_irrigation_add = 0.0
        Ag_irrigation_add_vol = 0.0
        Unused_ag_et = 0.0
        Ag_soilwater_deficit = 0.0
        It0_ag_soil_moist = Ag_soil_moist
        It0_ag_soil_rechr = Ag_soil_rechr
        Ag_gvr2sm = 0.0
      ENDIF
      keep_iterating = ACTIVE
      Soil_iter = 1
      DO WHILE ( keep_iterating==ACTIVE )
        IF ( Iter_aet==ACTIVE .AND. Soil_iter>1 ) THEN
          Ag_soil_moist = It0_ag_soil_moist
          Ag_soil_rechr = It0_ag_soil_rechr
          Ag_gvr2sm = 0.0
          Ssres_stor = It0_ssres_stor
          Slow_stor = It0_slow_stor
          IF ( GSFLOW_flag==ACTIVE ) Gravity_stor_res = It0_gravity_stor_res
          IF ( Pref_flag==ACTIVE ) Pref_flow_stor = It0_pref_flow_stor
          IF ( Nlake>0 ) Potet = It0_potet
        ENDIF
      Basin_ag_soil_moist = 0.0D0
      !Basin_ag_soil_to_gw = 0.0D0
      Basin_ag_up_max = 0.0D0
      Basin_ag_actet = 0.0D0
      Basin_ag_gvr2sm = 0.0D0
      Basin_agwaterin = 0.0D0
      CALL init_basin_vars()
      gwin = 0.0D0
      ! Soil_to_gw and Soil_to_ssr for whole HRU
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
!      Snowevap_aet_frac = 0.0
      ! gravity reservoir variables for whole HRU
      Ssr_to_gw = 0.0
      Slow_flow = 0.0
      Ssres_flow = 0.0
      Cap_waterin = 0.0
      Soil_saturated = OFF
      update_potet = OFF
      add_estimated_irrigation = OFF
      num_hrus_ag_iter = 0
      IF ( Soilzone_add_water_use==ACTIVE ) Soilzone_gain_hru = 0.0
      IF ( Ag_package==ACTIVE ) Ag_soil_saturated = OFF
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        HRU_id = i
!        if (i==36) write(531,*) 'a', Ag_soil_moist(i), It0_ag_soil_moist(i), ag_soil_rechr(i), iter_aet, Soil_iter, Infil_ag(i)

        hruactet = Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i)
        IF ( Dprst_flag==ACTIVE ) hruactet = hruactet + Dprst_evap_hru(i)
        harea = Hru_area(i)

        IF ( Hru_type(i)==LAKE ) THEN ! lake or reservoir
          !WARNING, RSR, if hru_actet>water in lake, then budget error
          hruactet = (Potet(i) - hruactet)*Lake_evap_adj(Nowmonth,Lake_hru_id(i))
          IF ( hruactet>Potet(i) ) THEN
            PRINT *, 'WARNING, lake evap > potet, for HRU:', i, ' potential ET increased to adjusted lake ET'
            PRINT *, hruactet, Potet(i), hruactet - Potet(i)
            Potet(i) = hruactet ! this could be a problem when it happens
            update_potet = ACTIVE
          ENDIF
          Unused_potet(i) = Potet(i) - hruactet
          Basin_actet = Basin_actet + DBLE( hruactet*harea )
          Basin_lakeevap = Basin_lakeevap + DBLE( hruactet*harea )
          Basin_lakeprecip = Basin_lakeprecip + DBLE( Hru_ppt(i)*harea )
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            ! if lake HRU doesn't cascade, should we limit ET to
            !  water entering the HRU to this point (no gwflow yet)
            Lakein_sz(i) = Upslope_interflow(i) + Upslope_dunnianflow(i)
            Basin_lakeinsz = Basin_lakeinsz + Lakein_sz(i)*Hru_area_dble(i)
          ENDIF
          Hru_actet(i) = hruactet
          CYCLE
        ENDIF

        !Hru_type can be 1 (land) or 3 (swale) or 4 (glacier)
        compute_lateral = OFF ! swale
        IF ( Hru_type(i)==LAND .OR. Hru_type(i)==GLACIER ) compute_lateral = ACTIVE
        perv_area = Hru_perv(i)
        perv_frac = Hru_frac_perv(i)
        perv_on_flag = OFF
        IF ( perv_area>0.0 ) perv_on_flag = ACTIVE
        ag_water_maxin = 0.0
        ag_on_flag = OFF
        IF ( Ag_area(i)>0.0 ) THEN
          ag_on_flag = ACTIVE
          agfrac = Ag_frac(i)
          ag_water_maxin = Infil_ag(i)
!          if (i == 3056 .AND. ag_water_maxin>0.0 ) then
!            write(877,*) i, kkiter, ag_area(i), ag_frac(i), Infil_ag(i), ag_water_maxin, 'infil_ag'
!            endif
        ENDIF

        avail_potet = Potet(i) - hruactet
        IF ( avail_potet<0.0 ) THEN
            print *, 'avail_potet<0', avail_potet, Potet(i), Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i), hruactet
            avail_potet = 0.0
            hruactet = Potet(i)
        endif
!        Snowevap_aet_frac(i) = 0.0

        !Hru_type can be 1 (land) or 3 (swale) or 4 (glacier)

!******Add infiltration to soil and compute excess
        dunnianflw = 0.0
        dunnianflw_pfr = 0.0
        dunnianflw_gvr = 0.0
        interflow = 0.0

!******Add infiltration to soil and compute excess
        !infil_tot is the depth in whole HRU
        !capillary reservoir for pervious area
        !agriculture reservoir for irrigated area
        !preferential flow reservoir for whole HRU
        !gravity reservoir for whole HRU
        !upslope flow for whole HRU

!******if cascading flow available from upslope cascades
!****** add soil excess (Dunnian flow) to infiltration
        ! infil for pervious portion of HRU
        ! infil_ag for pervious and agriculture portion of HRU
        capwater_maxin = Infil(i)

        IF ( Agriculture_soilzone_flag==ACTIVE ) THEN
          IF ( Hru_ag_irr(i)>0.0 ) THEN ! Hru_ag_irr is in inches-acres over ag area
            IF ( ag_on_flag==OFF ) THEN
              PRINT *, 'ag_frac=0.0 for HRU:', i
              CALL error_stop('AG Package irrigation specified and ag_frac=0.0', ERROR_param)
            ENDIF
            ag_water_maxin = ag_water_maxin + Hru_ag_irr(i) / Ag_area(i)
          ENDIF
        ENDIF
        IF ( Iter_aet==ACTIVE ) ag_water_maxin = ag_water_maxin + Ag_irrigation_add(i) ! units of inches over Ag_area
        cap_ag_water_maxin = 0.0
        IF ( Soilzone_add_water_use==ACTIVE ) THEN
          IF ( Soilzone_gain(i)>0.0 ) THEN
            IF ( perv_on_flag==OFF ) THEN
              PRINT *, 'perv_area=0.0 for HRU:', i
              CALL error_stop('soilzone gain specified and perv_area=0.0', ERROR_param)
            ENDIF
            Soilzone_gain_hru(i) = Soilzone_gain(i)/perv_area/SNGL(Cfs_conv) ! ??? is this harea
            cap_ag_water_maxin = Soilzone_gain_hru(i)
          ENDIF
        ENDIF
        IF ( ag_on_flag==ACTIVE ) THEN
          Ag_soilwater_deficit(i) = Ag_soil_moist_max(i) - Ag_soil_moist(i)
          excess = ag_water_maxin + Ag_soil_moist(i) - Ag_soil_moist_max(i)
          IF ( excess>0.0 .OR. Ag_irrigation_add(i)>0.0 ) THEN
            if(Ag_soil_moist(i)>Ag_soil_moist_max(i)) then
!           IF (i==36) WRITE(531,333) Nowyear, Nowmonth, Nowday, i, excess, ag_water_maxin, &
!     &                                   Ag_soil_moist(i), Ag_soil_moist_max(i), Ag_irrigation_add(i)
endif
            Ag_soil_moist(i) = Ag_soil_moist(i) - excess
            Ag_hortonian(i) = excess
            Sroff(i) = Sroff(i) + excess
            ag_water_maxin = ag_water_maxin - excess
 333 format (I4, 2(', ',i3), ', ', I0, 5(', ',F0.5))
          ENDIF
        ENDIF
        capwater_maxin = capwater_maxin + cap_ag_water_maxin

        cfgi_frozen_hru = OFF
        !Frozen is HRU variable that says if frozen gravity reservoir
        ! For CFGI all inflow is assumed to be Dunnian Flow when frozen
        IF ( Frozen_flag==ACTIVE ) THEN
          IF ( Frozen(i)==ACTIVE ) THEN
            IF ( Hru_type(i)==SWALE ) THEN
              PRINT *, 'ERROR, a swale HRU cannot be frozen for CFGI, HRU:', i
              ERROR STOP ERROR_param
            ENDIF
            cfgi_frozen_hru = ACTIVE
          ENDIF
        ENDIF

        ! compute preferential flow and storage, and any dunnian flow
        ! pref_flow for whole HRU
! ??? should cascading flow go to preferential flow fraction ???
        prefflow = 0.0
        dunnianflw_pfr = 0.0
        IF ( Pref_flow_infil_frac(i)>0.0 ) THEN
          pref_flow_maxin = 0.0
          Pref_flow_infil(i) = 0.0
          IF ( capwater_maxin>0.0 ) THEN
            ! pref_flow for whole HRU
            pref_flow_maxin = capwater_maxin*Pref_flow_infil_frac(i)
            capwater_maxin = capwater_maxin - pref_flow_maxin
            pref_flow_maxin = pref_flow_maxin*perv_frac
          ENDIF
          IF ( ag_water_maxin>0.0 .AND. ag_on_flag==ACTIVE ) THEN
            pref_flow_maxin = ag_water_maxin*Pref_flow_infil_frac(i)
            ag_water_maxin = ag_water_maxin - pref_flow_maxin
            pref_flow_maxin = pref_flow_maxin*agfrac
          ENDIF
          IF ( pref_flow_maxin>0.0 ) THEN
            IF ( cfgi_frozen_hru==ACTIVE ) THEN
              dunnianflw_pfr = pref_flow_maxin
            ELSE
              ! compute contribution to preferential-flow reservoir storage
              Pref_flow_stor(i) = Pref_flow_stor(i) + pref_flow_maxin
              dunnianflw_pfr = MAX( 0.0, Pref_flow_stor(i)-Pref_flow_max(i) )
            ENDIF
            IF ( dunnianflw_pfr>0.0 ) THEN
              Basin_dunnian_pfr = Basin_dunnian_pfr + DBLE( dunnianflw_pfr*harea )
              Pref_flow_stor(i) = Pref_flow_max(i)
            ENDIF
            Pref_flow_infil(i) = pref_flow_maxin - dunnianflw_pfr
            Basin_pref_flow_infil = Basin_pref_flow_infil + DBLE( Pref_flow_infil(i)*harea )
          ENDIF
          Pfr_dunnian_flow(i) = dunnianflw_pfr
        ENDIF

        IF ( Cascade_flag>CASCADE_OFF ) THEN
          IF ( ag_on_flag==ACTIVE ) THEN
            ag_upflow_max = SNGL(Upslope_dunnianflow(i)+Upslope_interflow(i))/agfrac
            Basin_ag_up_max = Basin_ag_up_max + ag_upflow_max*Ag_area(i)
            ag_water_maxin = ag_water_maxin + ag_upflow_max
          ENDIF
          IF ( perv_on_flag==ACTIVE ) THEN
!            Cap_upflow_max(i) = SNGL(Upslope_dunnianflow(i)+Upslope_interflow(i))/perv_frac
!            capwater_maxin = capwater_maxin + Cap_upflow_max(i)
!            Basin_cap_up_max = Basin_cap_up_max + DBLE( Cap_upflow_max(i)*perv_area )
            cap_upflow_max = SNGL(Upslope_dunnianflow(i)+Upslope_interflow(i))/perv_frac
            capwater_maxin = capwater_maxin + cap_upflow_max
            Basin_cap_up_max = Basin_cap_up_max + DBLE( cap_upflow_max*perv_area )
          ENDIF
        ENDIF
        IF ( perv_on_flag==ACTIVE ) THEN
          Cap_infil_tot(i) = capwater_maxin*perv_frac
          Basin_cap_infil_tot = Basin_cap_infil_tot + DBLE( Cap_infil_tot(i)*harea )
        ENDIF

!******Add infiltration to soil and compute excess
        gvr_maxin = 0.0
        Cap_waterin(i) = capwater_maxin

        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( perv_on_flag==ACTIVE ) THEN
            ! call even if capwater_maxin = 0, just in case soil_moist now > Soil_moist_max
            IF ( capwater_maxin+Soil_moist(i)>0.0 ) THEN
              CALL compute_soilmoist(Cap_waterin(i), Soil_moist_max(i), &
     &             Soil_rechr_max(i), Soil2gw_max(i), gvr_maxin, &
     &             Soil_moist(i), Soil_rechr(i), Soil_to_gw(i), perv_frac)
              Cap_waterin(i) = Cap_waterin(i)*perv_frac
              Basin_capwaterin = Basin_capwaterin + DBLE( Cap_waterin(i)*harea )
            ENDIF
          ENDIF
          IF ( ag_on_flag==ACTIVE ) THEN
 !if(i==36) write(531,*)              'c',ag_water_maxin,Ag_soil_moist(i)
            IF ( ag_water_maxin+Ag_soil_moist(i)>0.0 ) THEN
              ag_soil2gw = 0.0
              CALL compute_soilmoist(ag_water_maxin, Ag_soil_moist_max(i), &
     &             Ag_soil_rechr_max(i), Soil2gw_max(i), Ag_soil2gvr(i), &
     &             Ag_soil_moist(i), Ag_soil_rechr(i), ag_soil2gw, agfrac)
! if(i==36) write(531,*)              'd',ag_water_maxin,Ag_soil_moist(i), Ag_soil2gvr(i), ag_soil2gw
              ag_water_maxin = ag_water_maxin*agfrac
!              Ag_water_maxin(i) = ag_water_maxin
              Basin_agwaterin = Basin_agwaterin + DBLE( ag_water_maxin*harea )
              Soil_to_gw(i) = Soil_to_gw(i) + ag_soil2gw
              gvr_maxin = gvr_maxin + Ag_soil2gvr(i)
            ENDIF
          ENDIF
        ENDIF
        Basin_soil_to_gw = Basin_soil_to_gw + DBLE( Soil_to_gw(i)*harea )
        Basin_sm2gvr_max = Basin_sm2gvr_max + DBLE( gvr_maxin*harea )
        Soil_to_ssr(i) = gvr_maxin

! compute slow interflow and ssr_to_gw
        topfr = 0.0
        ag_capacity = 0.0
        IF ( ag_on_flag==ACTIVE ) ag_capacity = (Ag_soil_moist_max(i) - Ag_soil_moist(i))*agfrac
        IF ( GSFLOW_flag==ACTIVE ) THEN
          ! capacity for whole HRU
          capacity = (Soil_moist_max(i) - Soil_moist(i))*perv_frac
          CALL compute_gravflow(i, capacity, Slowcoef_lin(i), &
     &                          Slowcoef_sq(i), Ssr2gw_rate(i), Ssr2gw_exp(i), &
     &                          gvr_maxin, Pref_flow_thrsh(i), topfr, &
     &                          Ssr_to_gw(i), Slow_flow(i), Slow_stor(i), &
     &                          Gvr2sm(i), Soil_to_gw(i), gwin, compute_lateral, &
     &                          ag_capacity, ag_on_flag)
          ! adjust soil moisture with replenish amount
          IF ( Gvr2sm(i)>0.0 ) THEN
            IF ( perv_on_flag==ACTIVE ) THEN
              Soil_moist(i) = Soil_moist(i) + Gvr2sm(i)/perv_frac ! ??? could this be bigger than soil_moist_max ??? (add to Dunnian)
!              IF ( Soil_moist(i)>Soil_moist_max(i) ) PRINT *, 'CAP sm>max', Soil_moist(i), Soil_moist_max(i), i
              IF ( Soilzone_aet_flag==ACTIVE ) THEN
                Soil_lower(i) = MIN( Soil_lower_stor_max(i), Soil_moist(i) - Soil_rechr(i) + Gvr2sm(i)/perv_frac )
                Soil_rechr(i) = Soil_moist(i) - Soil_lower(i)
!                excess = MAX( 0.0, Soil_lower(i) - Soil_lower_stor_max(i) )
!                if ( abs(soil_lower(i) + soil_rechr(i) - soil_moist(i))>NEARZERO ) THEN
!                  print *, 'excess', excess, Soil_lower_stor_max(i), soil_lower(i) + soil_rechr(i)-soil_moist(i)
!                  print *, soil_lower(i), soil_rechr(i), soil_moist(i)
!                endif
              ELSE
                Soil_rechr(i) = MIN( Soil_rechr_max(i), Soil_rechr(i) + Gvr2sm(i)/perv_frac*Replenish_frac(i) )
              ENDIF
            ENDIF
            Basin_gvr2sm = Basin_gvr2sm + DBLE( Gvr2sm(i)*harea )
!          ELSEIF ( Gvr2sm(i)<-NEARZERO ) THEN
!            PRINT *, 'negative gvr2sm, HRU:', i, Gvr2sm(i)
!            Gvr2sm(i) = 0.0
          ENDIF
          IF ( ag_on_flag==ACTIVE ) THEN
            IF ( Ag_gvr2sm(i)>0.0 ) THEN
              Ag_soil_moist(i) = Ag_soil_moist(i) + Ag_gvr2sm(i)/agfrac
!if(i==36) write(531,*) 'ag_gvr2sm 1', ag_gvr2sm(i), agfrac, Ag_soil_moist(i)
!              IF ( Ag_soil_moist(i)>Ag_soil_moist_max(i) ) &
!     &             PRINT *, 'AG sm>max', Ag_soil_moist(i), Ag_soil_moist_max(i), i
              Ag_soil_rechr(i) = MIN( Ag_soil_rechr_max(i), Ag_soil_rechr(i) + (Ag_gvr2sm(i)/agfrac)*Ag_replenish_frac(i))
              Ag_soil_lower(i) = MIN( Ag_soil_lower_stor_max(i), Ag_soil_rechr(i) )
!              excess = MAX( 0.0, Ag_soil_lower(i) + Ag_soil_rechr(i) - Ag_soil_moist(i) )
!              if ( excess>0.0 ) print *, 'excess 2', excess, &
!     &             Ag_soil_lower(i), Ag_soil_rechr(i), Ag_soil_moist(i), Ag_soil_moist_max(i)
              Basin_ag_gvr2sm = Basin_ag_gvr2sm + DBLE( Ag_gvr2sm(i)*harea )
            ELSE
              Ag_soil_rechr(i) = MIN( Ag_soil_rechr_max(i), Ag_soil_rechr(i) )
            ENDIF
          ENDIF
          Grav_gwin(i) = SNGL( gwin )
          Basin_sz_gwin = Basin_sz_gwin + gwin*Hru_area_dble(i)
        ELSE
          availh2o = Slow_stor(i) + gvr_maxin
          IF ( compute_lateral==ACTIVE ) THEN
            topfr = MAX( 0.0, availh2o-Pref_flow_thrsh(i) )
            ssresin = gvr_maxin - topfr
            Slow_stor(i) = availh2o - topfr
            ! compute slow contribution to interflow, if any
            IF ( Slow_stor(i)>0.0 ) &
     &           CALL compute_interflow(Slowcoef_lin(i), Slowcoef_sq(i), &
     &                                  ssresin, Slow_stor(i), Slow_flow(i))
!if (i==36) print '(2(i0,1X),3(F0.5,1X),3i5)', i, hrus_iterating(i), Slow_stor(i), Slow_flow(i), ssresin, Nowyear, Nowmonth, Nowday
          ELSEIF ( Hru_type(i)==SWALE ) THEN
            Slow_stor(i) = availh2o
          ENDIF
          IF ( Slow_stor(i)>0.0 .AND. ag_capacity>0.0 ) THEN
            frac = 1.0D0
            depth = 0.0
            CALL check_gvr_sm(ag_capacity, Slow_stor(i), frac, Ag_gvr2sm(i), depth)
            IF ( Ag_gvr2sm(i)>0.0 ) THEN
              Ag_soil_moist(i) = Ag_soil_moist(i) + Ag_gvr2sm(i)/agfrac
!if(i==36) write(531,*) 'ag_gvr2sm 2', ag_gvr2sm(i), agfrac, Ag_soil_moist(i)
!              IF ( Ag_soil_moist(i)>Ag_soil_moist_max(i) ) &
!     &             PRINT *, 'AG sm>max', Ag_soil_moist(i), Ag_soil_moist_max(i), i
              Ag_soil_rechr(i) = MIN( Ag_soil_rechr_max(i), Ag_soil_rechr(i) + (Ag_gvr2sm(i)/agfrac)*Ag_replenish_frac(i))
              Ag_soil_lower(i) = MIN( Ag_soil_lower_stor_max(i), Ag_soil_rechr(i) )
!              excess = Ag_soil_lower(i) + Ag_soil_rechr(i) - Ag_soil_moist(i)
!              if ( excess>NEARZERO ) print *, 'excess', excess, Ag_soil_lower(i) &
!      &        + Ag_soil_rechr(i)-Ag_soil_moist(i), Ag_soil_lower(i), Ag_soil_rechr(i), Ag_soil_moist(i), i
              Basin_ag_gvr2sm = Basin_ag_gvr2sm + DBLE( Ag_gvr2sm(i)*harea )
            ELSE
              Ag_soil_rechr(i) = MIN( Ag_soil_rechr_max(i), Ag_soil_rechr(i) )
            ENDIF
          ENDIF
          IF ( Slow_stor(i)>0.0 .AND. Ssr2gw_rate(i)>0.0 ) &
       &       CALL compute_gwflow(Ssr2gw_rate(i), Ssr2gw_exp(i), Ssr_to_gw(i), Slow_stor(i))
        ENDIF

        ! compute contribution to Dunnian flow from PFR, if any
        IF ( Pref_flow_den(i)>0.0 ) THEN
          availh2o = Pref_flow_stor(i) + topfr
          dunnianflw_gvr = MAX( 0.0, availh2o-Pref_flow_max(i) )
          IF ( dunnianflw_gvr>0.0 ) THEN
            topfr = topfr - dunnianflw_gvr
            IF ( topfr<0.0 ) THEN
!              IF ( topfr<-NEARZERO .AND. Print_debug>DEBUG_less ) PRINT *, 'gvr2pfr<0', topfr, dunnianflw_gvr, &
!     &             Pref_flow_max(i), Pref_flow_stor(i), gvr_maxin
              topfr = 0.0
            ENDIF
          ENDIF
          Pref_flow_in(i) = Pref_flow_infil(i) + topfr
          Pref_flow_stor(i) = Pref_flow_stor(i) + topfr
          IF ( Pref_flow_stor(i)>0.0 ) &
     &         CALL compute_interflow(Fastcoef_lin(i), Fastcoef_sq(i), &
     &                                Pref_flow_in(i), Pref_flow_stor(i), prefflow)
          Basin_pref_stor = Basin_pref_stor + DBLE( Pref_flow_stor(i)*harea )
!          Pfr_stor_frac(i) = Pref_flow_stor(i)/Pref_flow_max(i)
!          Basin_pfr_stor_frac = Basin_pfr_stor_frac + DBLE( Pfr_stor_frac(i)*harea )
          Basin_pfr_stor_frac = Basin_pfr_stor_frac + DBLE( Pref_flow_stor(i)/Pref_flow_max(i)*harea )
        ELSEIF ( compute_lateral==ACTIVE ) THEN
          dunnianflw_gvr = topfr  !?? is this right
        ENDIF
        Gvr2pfr(i) = topfr

        Basin_sm2gvr = Basin_sm2gvr + DBLE( Soil_to_ssr(i)*harea )
        Basin_dunnian_gvr = Basin_dunnian_gvr + DBLE( dunnianflw_gvr*harea )
        Basin_sz2gw = Basin_sz2gw + DBLE( Ssr_to_gw(i)*harea )

!******Compute actual evapotranspiration
        Snow_free(i) = 1.0 - Snowcov_area(i)
        Potet_rechr(i) = 0.0
        Potet_lower(i) = 0.0
        pervactet = 0.0
        IF ( Soil_moist(i)>0.0 .AND. cfgi_frozen_hru==OFF ) THEN
          CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i), Transp_on(i), Cov_type(i), &
     &                         Soil_type(i), Soil_moist(i), Soil_rechr(i), pervactet, avail_potet, &
     &                         Snow_free(i), Potet_rechr(i), Potet_lower(i), &
     &                         Potet(i), perv_frac, Soil_saturated(i))
          ! sanity check
!          IF ( pervactet>avail_potet ) THEN
!            Soil_moist(i) = Soil_moist(i) + pervactet - avail_potet
!            pervactet = avail_potet
!            PRINT *, 'perv_et problem', pervactet, Avail_potet
!          ENDIF
        ENDIF
        ! sanity check
!        IF ( Soil_moist(i)<0.0 ) THEN
!          IF ( Print_debug>-1 ) PRINT *, i, Soil_moist(i), ' negative'
!          IF ( pervactet>=ABS(Soil_moist(i)) ) THEN
!            pervactet = pervactet + Soil_moist(i)
!            Soil_moist(i) = 0.0
!          ENDIF
!          IF ( Soil_moist(i)<-NEARZERO ) THEN
!            IF ( Print_debug>-1 ) PRINT *, 'HRU:', i, ' soil_moist<0.0', Soil_moist(i)
!          ENDIF
!          Soil_moist(i) = 0.0
!        ENDIF
        hruactet = hruactet + pervactet*perv_frac
        ag_hruactet = 0.0
        IF ( ag_on_flag==ACTIVE ) THEN
          IF ( Iter_aet_flag==ACTIVE .AND. Transp_on(i)==ACTIVE ) THEN
              !soilwater_deficit = MAX( sz_deficit_param, ag_soil_moist(i)/ag_soil_moist_max(i) ) irrigation happens at a deficit threshold
            ag_AETtarget = AET_external(i) ! ??? rsr, should this be PET target
          ELSE
            ag_AETtarget = Potet(i)
          ENDIF

          ag_avail_targetAET = ag_AETtarget - hruactet
          IF ( Ag_soil_moist(i)>0.0 .AND. cfgi_frozen_hru==OFF ) THEN
!            print *, Ag_soil_moist(i), Ag_soil_rechr(i)
!if (i==36)  write(531,*) 'ee',ag_AETtarget, hruactet, AET_external(i), Ag_actet(i), ag_avail_targetAET, Ag_soil_moist(i)
            CALL compute_szactet(Ag_soil_moist_max(i), Ag_soil_rechr_max(i), Transp_on(i), Ag_cov_type(i), &
     &                           Ag_soil_type(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_actet(i), ag_avail_targetAET, & !?? instead of ag_avail_potet use AET_external
     &                           Snow_free(i), Ag_potet_rechr(i), Ag_potet_lower(i), &
     &                           ag_AETtarget, agfrac, Ag_soil_saturated(i))
!if (i==36)  write(531,*) 'e',ag_AETtarget, hruactet, AET_external(i), Ag_actet(i), ag_avail_targetAET, Ag_soil_moist(i)
!if (i==36) write(531,*) 'f',Ag_soil_moist_max(i), Ag_soil_rechr_max(i), Transp_on(i), Ag_cov_type(i), &
!     &                           Ag_soil_type(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_actet(i), ag_avail_targetAET, & !?? instead of ag_avail_potet use AET_external
!     &                           Snow_free(i), Ag_potet_rechr(i), Ag_potet_lower(i), &
!     &                           ag_AETtarget, agfrac, Ag_soil_saturated(i)
            ! sanity checks
            IF ( Iter_aet_flag==ACTIVE  .AND. Transp_on(i)==ACTIVE ) THEN
              IF ( Ag_actet(i)-ag_AETtarget>NEARZERO ) THEN
                PRINT *, 'ag_actet problem', Ag_actet(i), ag_avail_potet, agfrac, AET_external(i), ag_potet, i, hruactet
                PRINT *, ag_AETtarget-Ag_actet(i), Ag_soil_moist(i), Ag_soil_rechr(i)
              ENDIF
            ENDIF
            ag_hruactet = Ag_actet(i)*agfrac
                    
!        avail_potet = ag_AETtarget - (hruactet + ag_hruactet)
!                      if (i == 3056 .and. Net_apply(i)>0.0 ) then
!            write(877,*) i, kkiter, avail_potet, ag_hruactet, Ag_actet(i), ag_AETtarget, Ag_soilwater_deficit(i), Ag_soil_saturated(i), Infil_ag(i), sroff(i), 'aet'
!            write(877,*) hruactet, Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i), Dprst_evap_hru(i), pervactet
!            endif
          ENDIF
        ENDIF
!        Perv_avail_et(i) = avail_potet

        Hru_actet(i) = hruactet + ag_hruactet
        avail_potet = Potet(i) - Hru_actet(i)
        ! sanity check
        IF ( avail_potet<0.0 ) THEN
          IF ( Print_debug>-1 ) THEN
            IF ( avail_potet<-NEARZERO ) THEN
              PRINT *, 'hru_actet>potet', i, hruactet, &
     &                 Nowmonth, Nowday, Hru_actet(i), Potet(i), avail_potet, ag_hruactet, &
     &                 pervactet*perv_frac, perv_frac, agfrac
              PRINT *, 'hruactet', hruactet, Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i), &
     &                 Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
            ENDIF
          ENDIF
!          Hru_actet(i) = Potet(i)
!          IF ( perv_on_flag==ACTIVE ) THEN
!            tmp = avail_potet/perv_frac
!            pervactet = pervactet + tmp
!            Soil_moist(i) = Soil_moist(i) - tmp
!            Soil_rechr(i) = Soil_rechr(i) - tmp
!            IF ( Soil_rechr(i)<0.0 ) Soil_rechr(i) = 0.0
!            IF ( Soil_moist(i)<0.0 ) Soil_moist(i) = 0.0
!          ENDIF
        ENDIF
        Perv_actet(i) = pervactet

! soil_moist & soil_rechr multiplied by perv_area instead of harea
        Soil_lower(i) = Soil_moist(i) - Soil_rechr(i)
        Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*perv_area )
        Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(i)*perv_area )
        Basin_perv_et = Basin_perv_et + DBLE( Perv_actet(i)*perv_area )
        IF ( ag_on_flag==ACTIVE ) THEN
          Ag_soil_lower(i) = Ag_soil_moist(i) - Ag_soil_rechr(i)
!          if ( Ag_soil_lower(i)<0.0 ) print *, 'soil_lower', Ag_soil_lower(i), i
          Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(i)*Ag_area(i) )
          Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(i)*Ag_area(i) )
          Basin_ag_actet = Basin_ag_actet + DBLE( Ag_actet(i)*Ag_area(i) )
        ENDIF

! if HRU cascades,
! compute interflow and excess flow to each HRU or stream
        IF ( compute_lateral==ACTIVE ) THEN
          interflow = Slow_flow(i) + prefflow
!          Interflow_max(i) = interflow
          Basin_interflow_max = Basin_interflow_max + interflow*harea
          dunnianflw = dunnianflw_gvr + dunnianflw_pfr
          Dunnian_flow(i) = dunnianflw
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            IF ( Ncascade_hru(i)>0 ) THEN
              dnslowflow = 0.0
              dnpreflow = 0.0
              dndunn = 0.0
              IF ( interflow+dunnianflw>0.0 ) THEN
                CALL compute_cascades(i, Ncascade_hru(i), Slow_flow(i), &
     &                                prefflow, Dunnian_flow(i), dnslowflow, &
     &                                dnpreflow, dndunn)
                Basin_dninterflow = Basin_dninterflow + DBLE( (dnslowflow+dnpreflow)*harea )
                Basin_dndunnianflow = Basin_dndunnianflow + DBLE( dndunn*harea )
              ENDIF
              Hru_sz_cascadeflow(i) = dnslowflow + dnpreflow + dndunn
!              Cascade_interflow(i) = dnslowflow + dnpreflow
!              Cascade_dunnianflow(i) = dndunn
              Basin_dncascadeflow = Basin_dncascadeflow + DBLE( Hru_sz_cascadeflow(i)*harea )
            ENDIF
          ENDIF

! treat pref_flow as interflow
          Ssres_flow(i) = Slow_flow(i)
          IF ( Pref_flow_den(i)>0.0 ) THEN
            Pref_flow(i) = prefflow
            Ssres_flow(i) = Ssres_flow(i) + prefflow
            Basin_prefflow = Basin_prefflow + DBLE( prefflow*harea )
            Basin_gvr2pfr = Basin_gvr2pfr + DBLE( Gvr2pfr(i)*harea )
          ENDIF
          Basin_ssflow = Basin_ssflow + DBLE( Ssres_flow(i)*harea )
          Basin_slowflow = Basin_slowflow + DBLE( Slow_flow(i)*harea )

! treat dunnianflw as surface runoff to streams
          Sroff(i) = Sroff(i) + Dunnian_flow(i)
          Basin_sroff = Basin_sroff + DBLE( Sroff(i)*harea )
          Basin_dunnian = Basin_dunnian + DBLE( Dunnian_flow(i)*harea )
          Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)

        ELSE ! for swales
          availh2o = Slow_stor(i) - Sat_threshold(i)
          Swale_actet(i) = 0.0
          IF ( availh2o>0.0 ) THEN ! if ponding, as storage > sat_threshold
            unsatisfied_et = Potet(i) - Hru_actet(i)
            IF ( unsatisfied_et>0.0 ) THEN
              availh2o = MIN ( availh2o, unsatisfied_et )
              Swale_actet(i) = availh2o
              Hru_actet(i) = Hru_actet(i) + Swale_actet(i)
              Slow_stor(i) = Slow_stor(i) - Swale_actet(i)
              Basin_swale_et = Basin_swale_et + DBLE( Swale_actet(i)*harea )
            ENDIF
            IF ( Print_debug==7 ) THEN
              IF ( Slow_stor(i)>Swale_limit(i) ) THEN
                WRITE ( DBGUNT, * ) 'Swale ponding, HRU:', i, &
     &                  ' gravity reservoir is 3*sat_threshold', Slow_stor(i), Sat_threshold(i)
                CALL print_date(DBGUNT)
              ENDIF
            ENDIF
          ENDIF
          Ssres_stor(i) = Slow_stor(i)
        ENDIF

        IF ( Soil_lower_stor_max(i)>0.0 ) Soil_lower_ratio(i) = Soil_lower(i)/Soil_lower_stor_max(i)
!        Soil_rechr_ratio(i) = Soil_rechr(i)/Soil_rechr_max(i)
        Ssres_in(i) = Soil_to_ssr(i) + Pref_flow_infil(i) + SNGL( gwin )
        Basin_ssin = Basin_ssin + DBLE( Ssres_in(i)*harea )
        Basin_ssstor = Basin_ssstor + DBLE( Ssres_stor(i)*harea )
        Basin_slstor = Basin_slstor + DBLE( Slow_stor(i)*harea )
        Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_frac
        IF ( ag_on_flag==ACTIVE ) Soil_moist_tot(i) = Soil_moist_tot(i) + Ag_soil_moist(i)*agfrac
        Basin_soil_moist_tot = Basin_soil_moist_tot + DBLE( Soil_moist_tot(i)*harea )
        IF ( perv_on_flag==ACTIVE ) THEN
!          Soil_moist_frac(i) = Soil_moist_tot(i)/Soil_zone_max(i)
!          Cpr_stor_frac(i) = Soil_moist(i)/Soil_moist_max(i)
!          Basin_cpr_stor_frac = Basin_cpr_stor_frac + Cpr_stor_frac(i)*perv_area
!          Basin_sz_stor_frac = Basin_sz_stor_frac + Soil_moist_frac(i)*harea
          Basin_cpr_stor_frac = Basin_cpr_stor_frac + Soil_moist(i)/Soil_moist_max(i)*perv_area
!          Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac + Soil_rechr_ratio(i)*perv_area
          Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac + Soil_rechr(i)/Soil_rechr_max(i)*perv_area
        ENDIF
        IF ( Pref_flow_thrsh(i)>0.0 ) THEN
!          Gvr_stor_frac(i) = Slow_stor(i)/Pref_flow_thrsh(i)
!          Basin_gvr_stor_frac = Basin_gvr_stor_frac + Gvr_stor_frac(i)*harea
          Basin_gvr_stor_frac = Basin_gvr_stor_frac + Slow_stor(i)/Pref_flow_thrsh(i)*harea
        ENDIF
        Basin_sz_stor_frac = Basin_sz_stor_frac + Soil_moist_tot(i)/Soil_zone_max(i)*harea
        Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac + Soil_lower_ratio(i)*perv_area
        Recharge(i) = Soil_to_gw(i) + Ssr_to_gw(i)
        IF ( Dprst_flag==1 ) Recharge(i) = Recharge(i) + SNGL( Dprst_seep_hru(i) )
        Basin_recharge = Basin_recharge + DBLE( Recharge(i)*harea )
        Grav_dunnian_flow(i) = dunnianflw_gvr
        Unused_potet(i) = Potet(i) - Hru_actet(i)
        IF ( ag_on_flag==ACTIVE ) THEN
          IF ( Iter_aet_flag==ACTIVE .AND.Transp_on(i)==ACTIVE ) THEN
          !IF ( Iter_aet_flag==ACTIVE ) THEN
            !agriculture_external(i)
            !IF ( Unused_potet(i)>0.0 ) THEN
            unsatisfied_et = ag_AETtarget - Ag_actet(i)
            if ( unsatisfied_et<0.0 ) print *, unsatisfied_et, i, 'unsat', soilzone_aet_converge, ag_AETtarget, Ag_actet(i)
            IF ( unsatisfied_et>soilzone_aet_converge ) THEN
              IF ( .NOT.(Ag_soilwater_deficit(i)>Ag_soilwater_deficit_min(i)) ) &
     &             Ag_irrigation_add(i) = Ag_irrigation_add(i) + unsatisfied_et
              keep_iterating = ACTIVE
              add_estimated_irrigation = ACTIVE
              num_hrus_ag_iter = num_hrus_ag_iter + 1
              IF ( unsatisfied_et>unsatisfied_big ) unsatisfied_big = unsatisfied_et
            ELSE
              IF ( ag_AETtarget<Ag_actet(i) ) THEN
                PRINT *, 'WARNING, external agriculture available target AET from CBH File < computeted AET', i, &
     &                Nowyear, Nowmonth, Nowday, num_hrus_ag_iter
                PRINT '(4(A,F0.6))', '         AET_external: ', ag_AETtarget, '; ag_actet: ', &
     &                Ag_actet(i), ' PET_external: ', PET_external(i), ' AET_external: ', AET_external(i)
                print *, Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
              ENDIF
            ENDIF
          ELSE
!print *, i, AET_external(i), Ag_actet(i), unsatisfied_et, Ag_irrigation_add(i) 
          ENDIF
          Unused_ag_et(i) = ag_AETtarget - Ag_actet(i)
          Unused_potet(i) = Unused_potet(i) - Unused_ag_et(i)
!          if ( i==36) print *, Ag_irrigation_add(i), i, num_hrus_ag_iter, transp_on(i)
        ENDIF
        Basin_actet = Basin_actet + DBLE( Hru_actet(i)*harea )
!        IF ( Hru_actet(i)>0.0 ) Snowevap_aet_frac(i) = Snow_evap(i)/Hru_actet(i)
        Hru_storage(i) = DBLE( Soil_moist_tot(i) + Hru_intcpstor(i) + Hru_impervstor(i) ) + Pkwater_equiv(i)
        IF ( Dprst_flag==ACTIVE ) Hru_storage(i) = Hru_storage(i) + Dprst_stor_hru(i)
      ENDDO ! end HRU loop

      Soil_iter = Soil_iter + 1
      IF ( Iter_aet==OFF .OR. Basin_transp_on==OFF ) keep_iterating = OFF
      IF ( Soil_iter>max_soilzone_ag_iter .OR. add_estimated_irrigation==OFF ) keep_iterating = OFF
!      if (ag_actet(i)>potet(i)) print *, 'ag_actet>potet', ag_actet(i), potet(i)
!      if (unused_potet(i)<0.00) print *, 'unused', unused_potet(i), potet(i)
!      if ( hru_actet(i)>potet(i)) print *, 'hru_actet', hru_actet(i), potet(i)
      ENDDO ! end iteration while loop
      Soil_iter = Soil_iter - 1
      IF ( Iter_aet==ACTIVE ) Ag_irrigation_add_vol = Ag_irrigation_add*Ag_area
!      IF ( num_hrus_ag_iter>0 ) print '(2(A,I0))', 'number of hrus still iterating on AET: ', &
!     &     num_hrus_ag_iter
!      if ( soil_iter==max_soilzone_ag_iter ) iter_nonconverge = iter_nonconverge + 1
!      print *, 'iterations: ', Soil_iter, '; nonconverged', iter_nonconverge
      total_iters = total_iters + Soil_iter
!      print *, NOWTIME, unsatisfied_big, unsatisfied_big/basin_potet, total_iters

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_swale_et = Basin_swale_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv
      IF ( Nlake>0 ) THEN
        Basin_lakeevap = Basin_lakeevap*Basin_area_inv
        Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
        Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv
        Basin_lake_stor = Basin_lake_stor + Basin_lakeprecip - Basin_lakeevap
      ENDIF
      IF ( Pref_flag==ACTIVE ) THEN
        Basin_pref_stor = Basin_pref_stor*Basin_area_inv
        Basin_pref_flow_infil = Basin_pref_flow_infil*Basin_area_inv
        Basin_prefflow = Basin_prefflow*Basin_area_inv
        Basin_dunnian_pfr = Basin_dunnian_pfr*Basin_area_inv
        Basin_pfr_stor_frac = Basin_pfr_stor_frac*Basin_area_inv
      ENDIF
      IF ( Iter_aet==ACTIVE ) THEN
        Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
        Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv
        Basin_agwaterin = Basin_agwaterin*Basin_area_inv
      ENDIF
      Basin_dunnian_gvr = Basin_dunnian_gvr*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_ssflow = Basin_ssflow*Basin_area_inv
      Basin_interflow_max = Basin_interflow_max*Basin_area_inv
      Basin_sz2gw = Basin_sz2gw*Basin_area_inv
      Basin_ssin = Basin_ssin*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_dunnian = Basin_dunnian*Basin_area_inv
      Basin_sm2gvr = Basin_sm2gvr*Basin_area_inv
      Basin_sm2gvr_max = Basin_sm2gvr_max*Basin_area_inv
      Basin_capwaterin = Basin_capwaterin*Basin_area_inv
      Basin_cap_infil_tot = Basin_cap_infil_tot*Basin_area_inv
      Basin_cap_up_max = Basin_cap_up_max*Basin_area_inv
      Basin_dninterflow = Basin_dninterflow*Basin_area_inv
      Basin_dndunnianflow = Basin_dndunnianflow*Basin_area_inv
      Basin_dncascadeflow = Basin_dncascadeflow*Basin_area_inv
      Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_recharge = Basin_recharge*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_sz_gwin = Basin_sz_gwin*Basin_area_inv
      Basin_cpr_stor_frac = Basin_cpr_stor_frac*Basin_area_inv
      Basin_gvr_stor_frac = Basin_gvr_stor_frac*Basin_area_inv
      Basin_sz_stor_frac = Basin_sz_stor_frac*Basin_area_inv
      Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac*Basin_area_inv
      Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac*Basin_area_inv
      IF ( update_potet==ACTIVE ) THEN
        Basin_potet = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
      ENDIF

      END FUNCTION szrun_ag
!***********************************************************************
!     compute interflow and flow to groundwater reservoir
!***********************************************************************
      SUBROUTINE compute_gravflow(Ihru, Capacity, Slowcoef_lin, &
     &           Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp, Gvr_maxin, &
     &           Pref_flow_thrsh, Gvr2pfr, Ssr_to_gw, &
     &           Slow_flow, Slow_stor, Gvr2sm, Soil_to_gw, Gwin, Compute_lateral, &
     &           Ag_capacity, Ag_on_flag)
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, ACTIVE
      USE PRMS_MODULE, ONLY: Dprst_flag, Print_debug, Dprst_flag, Print_debug
      USE PRMS_SOILZONE, ONLY: Gravity_stor_res, Sm2gw_grav, Hru_gvr_count, Hru_gvr_index, &
     &    Gw2sm_grav, Gvr_hru_pct_adjusted
      USE PRMS_SOILZONE_AG, ONLY: Ag_gvr2sm
      USE PRMS_SRUNOFF, ONLY: Dprst_seep_hru
      IMPLICIT NONE
! Functions
      INTRINSIC :: MAX, DBLE, SNGL
      EXTERNAL :: check_gvr_sm, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Compute_lateral, Ag_on_flag
      REAL, INTENT(IN) :: Slowcoef_lin, Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp
      REAL, INTENT(IN) :: Pref_flow_thrsh, Soil_to_gw, Gvr_maxin
      REAL, INTENT(INOUT) :: Capacity, Ag_capacity
      REAL, INTENT(OUT) :: Ssr_to_gw, Slow_stor, Slow_flow, Gvr2pfr, Gvr2sm
      DOUBLE PRECISION, INTENT(OUT) :: Gwin
! Local Variables
      INTEGER :: j, igvr
      REAL :: perc, slowflow, extra_water, gvrin_actual, depth, input
      DOUBLE PRECISION :: topfr, slflow, togw, slowstor, frac
!***********************************************************************
      !Capacity is for whole HRU
      !Soil_to_gw is for whole HRU
      !TO DO
! use VKS as a function of slope (vector analysis) instead of coef_lin
! coef_lin for pref_flow needs to be VKS lateral times a factor
! change slow to interflow
! in init, set an array dimensioned by nhrucell to vks*mfl_to_inch

      Gwin = 0.0D0
      Gvr2sm = 0.0
      topfr = 0.0D0
      slflow = 0.0D0
      togw = 0.0D0
      slowstor = 0.0D0
      DO j = 1, Hru_gvr_count(Ihru)
        igvr = Hru_gvr_index(j, Ihru)
        frac = Gvr_hru_pct_adjusted(igvr)
        Gwin = Gwin + DBLE( Gw2sm_grav(igvr) )*frac
        input = Gvr_maxin + Gw2sm_grav(igvr)
        depth = Gravity_stor_res(igvr) + input
        IF ( depth>0.0 ) THEN
          IF ( Capacity>0.0 ) CALL check_gvr_sm(Capacity, depth, frac, Gvr2sm, input)
          IF ( Ag_on_flag==ACTIVE .AND. Ag_capacity>0.0 ) CALL check_gvr_sm(Ag_capacity, depth, frac, Ag_gvr2sm(igvr), input)
        ENDIF

        IF ( Compute_lateral==ACTIVE ) THEN
          extra_water = MAX( 0.0, depth-Pref_flow_thrsh )
          IF ( extra_water>0.0 ) THEN
            !compute contribution to preferential-flow reservoir storage
            topfr = topfr + DBLE( extra_water )*frac
            depth = Pref_flow_thrsh
          ENDIF
          gvrin_actual = MAX(0.0, input-extra_water)

! compute contribution to slow interflow, if any
          IF ( depth>0.0 ) THEN
            CALL compute_interflow(Slowcoef_lin, Slowcoef_sq, gvrin_actual, depth, slowflow)
            slflow = slflow + DBLE( slowflow )*frac
          ENDIF
        ENDIF

! compute flow to groundwater, if any
        IF ( depth>0.0 ) THEN
          IF ( Ssr2gw_rate>0.0 ) THEN
! use VKS instead of rate  ???????????????
            perc = Ssr2gw_rate*(depth**Ssr2gw_exp)
            IF ( perc<0.0 ) THEN
              perc = 0.0
            ELSEIF ( perc>depth ) THEN
              perc = depth
            ENDIF
            depth = depth - perc
            Sm2gw_grav(igvr) = perc
            togw = togw + DBLE( perc )*frac
          ENDIF
!        ELSE ! GVRs can go negative if flux change in MODFLOW final iteration decreases, so don't set to 0
!          if(depth<0.0) print *, 'depth<0', depth, ihru
!          depth = 0.0
        ENDIF

        Gravity_stor_res(igvr) = depth
        slowstor = slowstor + DBLE(depth)*frac

! add any direct recharge from soil infiltration
        Sm2gw_grav(igvr) = Sm2gw_grav(igvr) + Soil_to_gw
        IF ( Dprst_flag==1 ) Sm2gw_grav(igvr) = Sm2gw_grav(igvr) + SNGL( Dprst_seep_hru(Ihru) )

      ENDDO ! end loop of GVRs in the HRU

      Gvr2pfr = SNGL( topfr )
      Slow_flow = SNGL( slflow )
      Ssr_to_gw = SNGL( togw )
      Slow_stor = SNGL( slowstor )
      IF ( Slow_stor>Pref_flow_thrsh ) THEN
        IF ( Print_debug>DEBUG_less .AND. Compute_lateral==ACTIVE ) &
     &       PRINT *, 'slow_stor > thrsh', Slow_stor, Pref_flow_thrsh, ' HRU:', Ihru
      ENDIF

      END SUBROUTINE compute_gravflow

!***********************************************************************
!     soilzone_restart_ag - write or read soilzone_ag restart file
!***********************************************************************
      SUBROUTINE soilzone_restart_ag(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, GSFLOW_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Function
      EXTERNAL :: check_restart
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_soil_rechr, Basin_slstor, Basin_soil_moist_tot, Basin_pref_stor
        WRITE ( Restart_outunit ) Pref_flow_stor
        IF ( GSFLOW_flag==ACTIVE ) WRITE ( Restart_outunit ) Gravity_stor_res
        WRITE ( Restart_outunit ) Ag_soil_lower
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_soil_rechr, Basin_slstor, Basin_soil_moist_tot, Basin_pref_stor
        READ ( Restart_inunit ) Pref_flow_stor
        IF ( GSFLOW_flag==ACTIVE ) READ ( Restart_inunit ) Gravity_stor_res
        READ ( Restart_inunit ) Ag_soil_lower
      ENDIF
      END SUBROUTINE soilzone_restart_ag

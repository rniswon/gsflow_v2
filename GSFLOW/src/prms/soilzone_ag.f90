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
!    sends capillary inflow for frozen HRUs to sroff and hortonian_flow
!    allows dprst seepage and evaporation when frozen
!    allows impervious evaporation when frozen
!    computes drainage to groundwater
!***********************************************************************
! FORTRAN module for soilzone_ag
!***********************************************************************
      MODULE PRMS_SOILZONE_AG
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC_AG = 'Soilzone Computations'
      character(len=11), parameter :: MODNAME_AG = 'soilzone_ag'
      character(len=*), parameter :: Version_soilzone_ag = '2024-12-10'
      INTEGER, SAVE :: Soil_iter !, HRU_id
      DOUBLE PRECISION, SAVE :: Basin_ag_soil_to_gw, Basin_ag_up_max, Basin_perv_to_gw
      DOUBLE PRECISION, SAVE :: Basin_ag_actet, Basin_ag_soil_rechr, Basin_ag_gvr2sm, Basin_ag_cap_infil_tot
      REAL, SAVE, ALLOCATABLE :: Ag_replenish_frac(:), Ag_cap_infil_tot(:), Ag_water_in(:)
!      REAL, SAVE, ALLOCATABLE :: Ag_water_maxin(:)
      INTEGER, SAVE :: iter_nonconverge
!   Pervious Declared Variables
      REAL, SAVE, ALLOCATABLE :: perv_soil_to_gw(:), perv_soil_to_gvr(:)
!   Agriculture Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_saturated(:)
      DOUBLE PRECISION, SAVE :: Basin_ag_irrigation_add
      REAL, SAVE, ALLOCATABLE :: Unused_ag_et(:), ag_soil_to_gvr(:), Ag_soilwater_deficit(:)
      REAL, SAVE, ALLOCATABLE :: Ag_actet(:), Ag_irrigation_add(:), Ag_irrigation_add_vol(:)
      REAL, SAVE, ALLOCATABLE :: ag_soil_to_gw(:), hru_ag_actet(:), Ag_hortonian(:), ag_AET_external_vol(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil_lower(:), Ag_soil_lower_stor_max(:), Ag_potet_rechr(:), Ag_potet_lower(:)
!      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Ag_upslope_dunnian(:)
      REAL, SAVE, ALLOCATABLE :: Gvr2ag(:), Gvr_maxin(:)
      real, save :: unsatisfied_big
      ! parameters
! have covden a monthly, later
      INTEGER, SAVE, ALLOCATABLE :: Ag_soil_type(:) !, Ag_crop_type(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soilwater_deficit_min(:), Ag_covden_sum(:), Ag_covden_win(:)
      REAL, SAVE, ALLOCATABLE :: Ag_soil2gw_max(:) ! Ag_crop_coef later, will specify PET
      INTEGER, SAVE :: max_soilzone_ag_iter
      REAL, SAVE :: soilzone_aet_converge

      END MODULE PRMS_SOILZONE_AG

!***********************************************************************
!     Main soilzone_ag routine
!***********************************************************************
      INTEGER FUNCTION soilzone_ag()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, OFF, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
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
      USE PRMS_CONSTANTS, ONLY: OFF, ACTIVE
      use PRMS_MMFAPI, only: declvar_dble, declvar_int, declvar_real
      use PRMS_READ_PARAM_FILE, only: declparam
      USE PRMS_MODULE, ONLY: Nhru, Iter_aet_flag, Nhrucell, GSFLOW_flag !, Cascade_flag
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      use prms_utils, only: error_stop, print_module, PRMS_open_module_file, read_error
      IMPLICIT NONE
!***********************************************************************
      szdecl_ag = 0

      CALL print_module(MODDESC_AG, MODNAME_AG, Version_soilzone_ag)

      ALLOCATE ( perv_soil_to_gw(Nhru) )
      CALL declvar_real(MODNAME, 'perv_soil_to_gw', 'nhru', Nhru, &
     &     'Direct recharge from pervious capillary reservoir to groundwater reservior for each HRU', &
     &     'inches', perv_soil_to_gw)

      ALLOCATE ( perv_soil_to_gvr(Nhru) )
      CALL declvar_real(MODNAME, 'perv_soil_to_gvr', 'nhru', Nhru, &
     &     'Excess pervious capillary water that flows to the gravity reservoir of each HRU', &
     &     'inches', perv_soil_to_gvr)

! Agriculture variables and parameters
      ALLOCATE ( ag_soil_to_gw(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soil_to_gw', 'nhru', Nhru, &
     &     'Direct recharge from capillary reservoir for the irrigated area to groundwater reservoir for each HRU', &
     &     'inches', ag_soil_to_gw)

!      IF ( Cascade_flag>OFF ) THEN
!        ALLOCATE ( Ag_upslope_dunnian(Nhru) )
!        CALL declvar_dble(MODNAME, 'ag_upslope_dunnian', 'nhru', Nhru, &
!     &       'Cascading Dunnian surface runoff that'// &
!     &       ' flows to the agriculture capillary reservoir of each downslope HRU for each upslope HRU', &
!     &       'inches', Ag_upslope_dunnian)
!      ENDIF

      ALLOCATE ( Ag_actet(Nhru) )
      CALL declvar_real(MODNAME, 'ag_actet', 'nhru', Nhru, &
     &     'Actual ET in capillary reservoir for irrigated fraction of each HRU', &
     &     'inches', Ag_actet)

      ALLOCATE ( hru_ag_actet(Nhru) )
      CALL declvar_real(MODNAME, 'hru_ag_actet', 'nhru', Nhru, &
     &     'Actual ET for irrigated area portion of capillary reservoir averaged over irrigated area of HRU', &
     &     'inches', hru_ag_actet)

      ALLOCATE ( Unused_ag_et(Nhru) )
      CALL declvar_real(MODNAME, 'unused_ag_et', 'nhru', Nhru, &
     &     'Actual ET for agriculture capillary reservoir for each HRU', &
     &     'inches', Unused_ag_et)

      ALLOCATE ( Ag_hortonian(Nhru) )
      CALL declvar_real(MODNAME, 'ag_hortonian', 'nhru', Nhru, &
     &     'Hortonian surface runoff that flows to the stream network from the irrigated area of each HRU', &
     &     'inches', Ag_hortonian)

      ALLOCATE ( ag_soil_to_gvr(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soil_to_gvr', 'nhru', Nhru, &
     &     'Excess capillary water that flows to the gravity reservoir from the irrigated area of each HRU', &
     &     'inches', ag_soil_to_gvr)

      CALL declvar_dble(MODNAME, 'Basin_ag_irrigation_add', 'one', 1, &
     &     'Basin area-weighted average irrigation estimate', &
     &     'inches', Basin_ag_irrigation_add)

      ALLOCATE ( Ag_irrigation_add(Nhru) )
      CALL declvar_real(MODNAME, 'ag_irrigation_add', 'nhru', Nhru, &
     &     'Irrigation water added to irrigated area when ag_actet < AET_external for each HRU', &
     &     'inches', Ag_irrigation_add)

      ALLOCATE ( Ag_soilwater_deficit(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soilwater_deficit', 'nhru', Nhru, &
     &     'Soil-water deficit of agriculture fraction for each HRU', &
     &     'inches', Ag_soilwater_deficit)

      ALLOCATE ( Ag_irrigation_add_vol(Nhru) )
      CALL declvar_real(MODNAME, 'ag_irrigation_add_vol', 'nhru', Nhru, &
     &     'Irrigation water added to irrigated area when ag_actet < AET_external for each HRU', &
     &     'acre-inches', Ag_irrigation_add_vol)

      ALLOCATE ( ag_AET_external_vol(Nhru) )
      CALL declvar_real(MODNAME, 'ag_AET_external_vol', 'nhru', Nhru, &
     &     'External actual evapotranspiration used as a target for simulating actual ET during'// &
     &     ' transpiration days for the irrigated fraction of each HRU', &
     &     'acre-inches', ag_AET_external_vol)

      ALLOCATE ( Ag_soil_lower(Nhru), Ag_soil_lower_stor_max(Nhru) )
      CALL declvar_real(MODNAME, 'ag_soil_lower', 'nhru', Nhru, &
     &   'Storage in the lower zone of the irrigated area that is only available for transpiration for each HRU', &
     &   'inches', Ag_soil_lower)

      IF ( GSFLOW_flag==ACTIVE ) THEN
        ALLOCATE ( Gvr2ag(Nhru), Ag_replenish_frac(Nhru) )
        CALL declvar_real(MODNAME, 'gvr2ag', 'nhru', Nhru, &
     &       'Gravity flow to the capillary reservoir replenishment for irrigated area of each HRU', &
     &       'inches', Gvr2ag)
        ALLOCATE ( Gvr_maxin(Nhrucell) )
        CALL declvar_real(MODNAME, 'gvr_maxin', 'nhrucell', Nhru, &
     &       'Maximum inflow to gravity reservoirs', &
     &       'inches', Gvr_maxin)
      ENDIF

      ALLOCATE ( Ag_potet_lower(Nhru) ) !, Ag_water_maxin(Nhru) )
      CALL declvar_real(MODNAME, 'ag_potet_lower', 'nhru', Nhru, &
     &     'Potential ET in the lower zone of the agriculture reservoir for the irrigated fraction of each HRU', &
     &     'inches', Ag_potet_lower)

      ALLOCATE ( Ag_potet_rechr(Nhru) )
      CALL declvar_real(MODNAME, 'ag_potet_rechr', 'nhru', Nhru, &
     &     'Potential ET in the recharge zone of the agriculture reservoir for the irrigated fraction of each HRU', &
     &     'inches', Ag_potet_rechr)

      ALLOCATE ( Ag_soil_saturated(Nhru) )
      CALL declvar_int(MODNAME, 'ag_soil_saturated', 'nhru', Nhru, &
     &     'Flag set if infiltration saturates capillary reservoir for the irrigated area of the HRU (0=no, 1=yes)', &
     &     'none', Ag_soil_saturated)

      ALLOCATE ( Ag_cap_infil_tot(Nhru), Ag_water_in(Nhru) )
      CALL declvar_real(MODNAME, 'ag_water_in', 'nhru', Nhru, &
     &     'Total water into the capillary reservoir in the irrigated area for each HRU', &
     &     'inches', Ag_water_in)

      IF ( Iter_aet_flag==ACTIVE ) THEN
        IF ( declparam(MODNAME, 'max_soilzone_ag_iter', 'one', 'integer', &
     &       '10', '1', '9999', &
     &       'Maximum number of iterations to optimize computed AET and input AET', &
     &       'Maximum number of iterations to optimize computed AET and input AET', &
     &       'none')/=0 ) CALL read_error(1, 'max_soilzone_ag_iter')

        IF ( declparam(MODNAME, 'soilzone_aet_converge', 'one', 'real', &
     &       '0.00001', '0.0', '1.0', &
     &       'Convergence criteria to iterate computed AET compared to input AET', &
     &       'Convergence criteria to iterate computed AET compared to input AET', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'soilzone_aet_converge')

        ALLOCATE ( Ag_soilwater_deficit_min(Nhru) )
        IF ( declparam(MODNAME, 'ag_soilwater_deficit_min', 'nhru', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Minimum soil-water deficit to begin agriculture irrigation', &
     &       'Minimum soil-water deficit fraction to begin agriculture irrigation', &
     &       'fraction')/=0 ) CALL read_error(1, 'ag_soilwater_deficit_min')
      ENDIF

      ALLOCATE ( Ag_soil_type(Nhru) )
      IF ( declparam(MODNAME, 'ag_soil_type', 'nhru', 'integer', &
     &     '-1', '-1', '3', &
     &     'Agriculture soil type', 'Soil type of agriculture fraction in each HRU (1=sand; 2=loam; 3=clay)', &
     &     'none')/=0 ) CALL read_error(1, 'ag_soil_type')

!      ALLOCATE ( Ag_crop_type(Nhru) ) ! find Mastin's code on different crops
!      IF ( declparam(MODNAME, 'ag_crop_type', 'nhru', 'integer', &
!     &     '3', '0', '4', &
!     &     'Agriculture cover type designation for each HRU', &
!     &     'Vegetation cover type for agriculture in each HRU (0=none;'// &
!     &     ' 1=grasses; 2=grain; 3=trees; 4=vegetable)', &
!     &     'none')/=0 ) CALL read_error(1, 'ag_crop_type')

      ALLOCATE ( Ag_covden_sum(Nhru) )
      IF ( declparam(MODNAME, 'ag_covden_sum', 'nhru', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Summer vegetation cover density for agriculture crop type', &
     &     'Summer vegetation cover density for the agriculture crop type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'ag_covden_sum')

      ALLOCATE ( Ag_covden_win(Nhru) )
      IF ( declparam(MODNAME, 'ag_covden_win', 'nhru', 'real', &
     &     '-1.0', '-1.0', '1.0', &
     &     'Winter vegetation cover density for crop type', &
     &     'Winter vegetation cover density for the crop type in each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'ag_covden_win')

      ALLOCATE ( Ag_soil2gw_max(Nhru) )
      IF ( declparam(MODNAME, 'ag_soil2gw_max', 'nhru', 'real', &
     &     '-1.0', '-1.0', '5.0', &
     &     'Maximum value for agriculture capillary reservoir excess to groundwater storage', &
     &     'Maximum amount of the agriculture capillary reservoir excess that'// &
     &     ' is routed directly to groundwater storage for each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'ag_soil2gw_max')

      END FUNCTION szdecl_ag

!***********************************************************************
!     szinit_ag - Initialize soilzone module - get parameter values,
!                 set initial values and check parameter values
!***********************************************************************
      INTEGER FUNCTION szinit_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, LAKE, INACTIVE, GLACIER
      use PRMS_READ_PARAM_FILE, only: getparam_int, getparam_real
      USE PRMS_MODULE, ONLY: Nhru, Init_vars_from_file, Hru_type, GSFLOW_flag, Iter_aet_flag
      USE PRMS_SOILZONE, ONLY: MODNAME, Soil2gw_max, Soil_type
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Ag_area, Covden_win, Covden_sum
      USE PRMS_FLOWVARS, ONLY: Ag_soil_moist, Ag_soil_rechr, Ag_soil_moist_max, Ag_soil_rechr_max
      use prms_utils, only: checkdim_bounded_limits, error_stop, read_error
      IMPLICIT NONE
! Functions
      EXTERNAL :: init_basin_vars
      INTRINSIC :: MIN, DBLE
! Local Variables
      INTEGER :: ihru
!***********************************************************************
      szinit_ag = 0

!??? figure out what to save in restart file ???
      IF ( Iter_aet_flag==ACTIVE ) THEN
        IF ( getparam_int(MODNAME, 'max_soilzone_ag_iter', 1, max_soilzone_ag_iter)/=0 ) &
     &       CALL read_error(2, 'max_soilzone_ag_iter')
        IF ( getparam_real(MODNAME, 'soilzone_aet_converge', 1, soilzone_aet_converge)/=0 ) &
     &       CALL read_error(2, 'soilzone_aet_converge')
        IF ( getparam_real(MODNAME, 'ag_soilwater_deficit_min', Nhru, Ag_soilwater_deficit_min)/=0 ) &
     &       CALL read_error(2, 'ag_soilwater_deficit_min')
        Ag_irrigation_add = 0.0
        Ag_soilwater_deficit = 0.0
      ENDIF
      IF ( getparam_int(MODNAME, 'ag_soil_type', Nhru, Ag_soil_type)/=0 ) CALL read_error(2, 'ag_soil_type')
      IF ( Ag_soil_type(1) == -1 ) THEN
        print *, 'WARNING, ag_soil_type not specified, substituting soil_type'
        Ag_soil_type = Soil_type
      ENDIF
!      IF ( getparam_int(MODNAME, 'ag_crop_type', Nhru, Ag_crop_type)/=0 ) CALL read_error(2, 'ag_crop_type')
      IF ( getparam_real(MODNAME, 'ag_covden_sum', Nhru, Ag_covden_sum)/=0 ) CALL read_error(2, 'ag_covden_sum')
      IF ( Ag_covden_sum(1)<0.0 ) THEN
        print *, 'WARNING, ag_covden_sum not specified, substituting covden_sum'
        Ag_covden_sum = Covden_sum
      ENDIF
      IF ( getparam_real(MODNAME, 'ag_covden_win', Nhru, Ag_covden_win)/=0 ) CALL read_error(2, 'ag_covden_win')
      IF ( Ag_covden_win(1)<0.0 ) THEN
        print *, 'WARNING, ag_covden_win not specified, substituting covden_win'
        Ag_covden_win = Covden_win
      ENDIF
      IF ( getparam_real(MODNAME, 'ag_soil2gw_max', Nhru, Ag_soil2gw_max)/=0 ) CALL read_error(2, 'ag_soil2gw_max')
      IF ( Ag_soil2gw_max(1)<0.0 ) THEN
        print *, 'WARNING, ag_soil2gw_max not specified, substituting soil2gw_max'
        Ag_soil2gw_max = Soil2gw_max
      ENDIF
      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==5 ) Ag_soil_lower = 0.0
      Basin_ag_irrigation_add = 0.0D0
! initialize all HRU values in case dynamic ag frac
      hru_ag_actet = 0.0
      perv_soil_to_gvr = 0.0
      IF ( GSFLOW_flag==ACTIVE ) Ag_replenish_frac = 0.0
      DO ihru = 1, Nhru
        ! make sure LAKE, INACTIVE, GLACIER have agriculture values of 0
        IF ( Hru_type(ihru)==LAKE .OR. Hru_type(ihru)==INACTIVE .OR. Hru_type(ihru)==GLACIER ) Ag_area(ihru) = 0.0
        IF ( .not.(Ag_area(ihru)>0.0) ) THEN
          Ag_soil_moist(ihru) = 0.0
          Ag_soil_rechr(ihru) = 0.0
        ENDIF
        IF ( GSFLOW_flag==ACTIVE ) THEN
          IF ( Ag_soil_moist_max(ihru)>0.0 ) Ag_replenish_frac(ihru) = Ag_soil_rechr_max(ihru)/Ag_soil_moist_max(ihru)
        ENDIF
        Ag_soil_lower_stor_max(ihru) = Ag_soil_moist_max(ihru) - Ag_soil_rechr_max(ihru)
      ENDDO

      IF ( GSFLOW_flag==ACTIVE ) Gvr2ag = 0.0
      iter_nonconverge = 0

      END FUNCTION szinit_ag

!***********************************************************************
!     szrun_ag - Does soil water balance for each HRU, adds in infiltration
!                then computes actual et and apportions remainder between
!                recharge of soil moisture, soil storage available for
!                interflow, excess routed to stream,
!                and groundwater reservoirs
!***********************************************************************
      INTEGER FUNCTION szrun_ag()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, LAKE, SWALE, &
     &    DEBUG_less, CASCADE_OFF, ERROR_param, MODSIM_PRMS, MODSIM_PRMS_LOOSE, NEARZERO !, CLOSEZERO
      USE PRMS_MODULE, ONLY: Print_debug, Dprst_flag, Cascade_flag, Nlake, &
     &    Frozen_flag, Soilzone_add_water_use, Nowmonth, GSFLOW_flag, Hru_ag_irr, Ag_package, PRMS_land_iteration_flag, &
     &    Soilzone_aet_flag, Hru_type, timestep_start_flag, Model, Dprst_ag_gain, &
     &    activeHru_inactiveCell_flag, activeHru_inactiveCell, Iter_aet_flag, irrigation_apply_flag, Nhru, Nowyear, Nowday
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      USE PRMS_BASIN, ONLY: Hru_perv, Hru_frac_perv, Hru_storage, &
     &    Hru_route_order, Active_hrus, Basin_area_inv, Hru_area, &
     &    Ag_frac, Ag_area, Ag_cov_type, Ag_area_total, &
     &    Lake_hru_id, Cov_type, Hru_area_dble, gsflow_ag_area
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Transp_on, Potet, Basin_potet, Basin_transp_on
! WARNING!!! sroff and basin_sroff can be updated due to Dunnian flow
! WARNING!!! Strm_seg_in can be updated
! WARNING!!! if frozen sroff and basin_sroff can be updated
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_actet, Hru_actet, &
     &    Ssres_flow, Soil_to_gw, Basin_soil_to_gw, Ssr_to_gw, Pref_flow_stor, Ssr_to_prmsgw, &
     &    Soil_to_ssr, Basin_lakeevap, Basin_perv_et, Basin_swale_et, &
     &    Sroff, Soil_moist_max, Infil, Soil_rechr_max, Ssres_in, Soil_zone_max, &
     &    Basin_soil_moist, Basin_ssstor, Slow_stor, Slow_flow, Pkwater_equiv, &
     &    Soil_lower_stor_max, Ssres_stor, Soil_moist, Sat_threshold, Soil_rechr, Basin_sroff, Basin_lake_stor, &
     &    Dprst_stor_hru, Hru_impervstor, Gravity_stor_res, Snowcov_area, Snow_evap, Strm_seg_in, Hru_intcpstor, &
     &    gsflow_ag_actet, Basin_soil_to_prmsgw, Soil_to_prmsgw, &
     &    Ag_soil_rechr, Ag_soil_moist, Ag_soil_rechr_max, Ag_soil_moist_max, Basin_ag_soil_moist
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_soil_rechr, It0_ssres_stor, It0_slow_stor, &
                               It0_ag_soil_rechr, It0_ag_soil_moist, &
                               It0_pref_flow_stor, It0_gravity_stor_res, It0_potet
      USE GSFMODSIM2PRMS, ONLY: HRU_diversion
      USE PRMS_WATER_USE, ONLY: Soilzone_gain, Soilzone_gain_hru
      USE PRMS_CLIMATE_HRU, ONLY: AET_external, PET_external
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_INTCP, ONLY: Hru_intcpevap
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Dprst_evap_hru, Dprst_seep_hru, Frozen, Hru_sroffp, Hortonian_flow, Infil_ag
      use prms_utils, only: print_date, error_stop
      IMPLICIT NONE
! Functions
      INTRINSIC :: MIN, ABS, MAX, SNGL, DBLE
      EXTERNAL :: compute_soilmoist, compute_szactet, compute_cascades, compute_gravflow_ag
      EXTERNAL :: compute_interflow, compute_gwflow, init_basin_vars
! Local Variables
      INTEGER :: i, k, update_potet, compute_lateral, j, igvr
      REAL :: dunnianflw, interflow, perv_area, harea
      DOUBLE PRECISION :: dnslowflow, dnpreflow, dndunn
      REAL :: availh2o, avail_potet, hruactet
      REAL :: topfr !, tmp
      REAL :: dunnianflw_pfr, dunnianflw_gvr, pref_flow_maxin
      REAL :: perv_frac, capwater_maxin, ssresin, dunnianflw_frz, capacity
      REAL :: cap_upflow_max, unsatisfied_et, pervactet, prefflow, ag_water_maxin, cap_pref_flow_maxin
      REAL :: ag_upflow_max, ag_capacity, agfrac, ag_avail_potet, ag_potet, unsatisfied_ag_et
      REAL :: ag_AETtarget, ag_avail_targetAET, agactet, ag_pref_flow_maxin, ag_hruactet
      REAL :: upflow_max, ag_portion, perv_portion, agarea, unsatisfied_max !, max_irrigation
      DOUBLE PRECISION :: gwin
      INTEGER :: cfgi_frozen_hru
      INTEGER :: num_hrus_ag_iter, ag_on_flag, keep_iterating, add_estimated_irrigation, perv_on_flag
!***********************************************************************
      szrun_ag = 0

! It0 variables used with MODFLOW integration to save iteration states.
      IF ( timestep_start_flag == ACTIVE ) THEN
        IF ( GSFLOW_flag==ACTIVE ) THEN
          IF ( Ag_package == ACTIVE ) THEN
            IF ( Dprst_flag == ACTIVE ) Dprst_ag_gain = 0.0
          ENDIF
          Gw2sm_grav = 0.0 ! dimension nhrucell
          IF ( Nlake>0 ) It0_potet = Potet
        ENDIF
        IF ( PRMS_land_iteration_flag==OFF .OR.  Iter_aet_flag==ACTIVE ) THEN
          ! computed in srunoff
          It0_sroff = Sroff
          It0_hru_sroffp = Hru_sroffp
          It0_hortonian_flow = Hortonian_flow
          IF ( Cascade_flag>CASCADE_OFF ) It0_strm_seg_in = Strm_seg_in
          IF ( Iter_aet_flag==ACTIVE ) THEN
            Ag_irrigation_add = 0.0
            Ag_irrigation_add_vol = 0.0
            ag_AET_external_vol = 0.0
          ENDIF
        ENDIF
      ENDIF
      keep_iterating = ACTIVE
      Soil_iter = 1

! ***************************************
      DO WHILE ( keep_iterating==ACTIVE )
! ***************************************
      IF ( timestep_start_flag == OFF ) THEN
        Soil_moist = It0_soil_moist
        Soil_rechr = It0_soil_rechr
        Ssres_stor = It0_ssres_stor
        Slow_stor = It0_slow_stor
        IF ( Pref_flag==ACTIVE ) Pref_flow_stor = It0_pref_flow_stor
        IF ( Nlake > 0 ) Potet = It0_potet
        Gravity_stor_res = It0_gravity_stor_res
        IF ( PRMS_land_iteration_flag==OFF .OR. Iter_aet_flag==ACTIVE ) THEN
          ! computed in srunoff
          Sroff = It0_sroff
          Hru_sroffp = It0_hru_sroffp
          Hortonian_flow = It0_hortonian_flow
          IF ( Cascade_flag>CASCADE_OFF ) Strm_seg_in = It0_strm_seg_in
        ENDIF
        Ag_soil_moist = It0_ag_soil_moist
        Ag_soil_rechr = It0_ag_soil_rechr
      ENDIF
      IF ( timestep_start_flag == ACTIVE ) timestep_start_flag = OFF
      IF ( GSFLOW_flag==ACTIVE ) Sm2gw_grav = 0.0
      !max_irrigation = 0.0
      Ag_actet = 0.0
      IF ( activeHru_inactiveCell_flag == ACTIVE ) THEN
        Basin_soil_to_prmsgw = 0.0D0
        Basin_sz2gwprms = 0.0D0
        Soil_to_prmsgw = 0.0
        Ssr_to_prmsgw = 0.0
      ENDIF

      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Upslope_interflow = 0.0D0
        Upslope_dunnianflow = 0.0D0
        Hru_sz_cascadeflow = 0.0D0
        Hru_dunnian_cascadeflow = 0.0D0
        Hru_interflow_cascadeflow = 0.0D0
        IF ( Nlake>0 ) THEN
          Lakein_sz = 0.0D0
          Basin_lakeinsz = 0.0D0
        ENDIF
        Basin_dninterflow = 0.0D0
        Basin_dndunnianflow = 0.0D0
        Basin_dncascadeflow = 0.0D0
      ENDIF

      IF ( Nlake>0 ) THEN
        Basin_lakeprecip = 0.0D0
        Basin_lakeevap = 0.0D0
      ENDIF
      CALL init_basin_vars()
      gwin = 0.0D0
      ! Soil_to_gw and Soil_to_ssr for whole HRU
      Soil_to_gw = 0.0
      Soil_to_ssr = 0.0
      ! gravity reservoir variables for whole HRU
      Ssr_to_gw = 0.0
      Slow_flow = 0.0
      Ssres_flow = 0.0
      Soil_saturated = OFF
      Potet_rechr = 0.0
      Potet_lower = 0.0
      Snow_free = 1.0 - Snowcov_area
      IF ( Pref_flag==ACTIVE ) THEN
        Basin_pref_flow_infil = 0.0D0
        Basin_dunnian_pfr = 0.0D0
        Basin_gvr2pfr = 0.0D0
        Basin_prefflow = 0.0D0
        Basin_pref_stor = 0.0D0
        Basin_pfr_stor_frac = 0.0D0
        Pfr_dunnian_flow = 0.0
        Pref_flow_infil = 0.0
        Pref_flow_in = 0.0
      ENDIF
      update_potet = OFF
      IF ( Soilzone_add_water_use==ACTIVE ) Soilzone_gain_hru = 0.0
      Basin_ag_soil_moist = 0.0D0
      Basin_ag_soil_rechr = 0.0D0
      Basin_ag_soil_to_gw = 0.0D0
      Basin_ag_up_max = 0.0D0
      Basin_ag_actet = 0.0D0
      Basin_ag_gvr2sm = 0.0D0
      Basin_ag_cap_infil_tot = 0.0D0
      Basin_perv_to_gw = 0.D0
      perv_soil_to_gw = 0.0
! initialize all HRU values in case dynamic ag frac
      Ag_soil_saturated = OFF
      ag_soil_to_gvr = 0.0
      ag_soil_to_gw = 0.0
      Ag_hortonian = 0.0
      Unused_ag_et = 0.0
      IF ( Iter_aet_flag==ACTIVE ) Ag_soilwater_deficit = 0.0
      Ag_cap_infil_tot = 0.0
      Ag_water_in = 0.0
      Ag_potet_lower = 0.0
      Ag_potet_rechr = 0.0
      Ag_soil_lower = 0.0
      unsatisfied_big = 0.0
      add_estimated_irrigation = OFF
      num_hrus_ag_iter = 0

! ***************************************
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
! ***************************************

        !HRU_id = i
        hruactet = Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i)
        IF ( Dprst_flag==ACTIVE ) hruactet = hruactet + Dprst_evap_hru(i)
        harea = Hru_area(i)
        agfrac = Ag_frac(i)

        IF ( Hru_type(i)==LAKE ) THEN ! lake or reservoir
          !WARNING, RSR, if hru_actet>water in lake, then budget error
          hruactet = (Potet(i) - hruactet)*Lake_evap_adj(Nowmonth,Lake_hru_id(i))
          IF ( hruactet>Potet(i) ) THEN
            IF ( Print_debug > DEBUG_less ) THEN
              PRINT *, 'WARNING, lake evap > potet, for HRU:', i, ' potential ET increased to adjusted lake ET'
              PRINT *, hruactet, Potet(i), hruactet - Potet(i)
            ENDIF
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
        compute_lateral = ACTIVE
        IF ( Hru_type(i)==SWALE ) compute_lateral = OFF
        perv_area = Hru_perv(i)
        perv_frac = Hru_frac_perv(i)
        perv_on_flag = OFF
        IF ( perv_area>0.0 ) perv_on_flag = ACTIVE
        agarea = Ag_area(i)
        ag_on_flag = OFF
        IF ( agarea>0.0 ) ag_on_flag = ACTIVE

        avail_potet = Potet(i) - hruactet
        IF ( avail_potet<0.0 ) THEN
!          IF ( avail_potet<-CLOSEZERO ) &
!               print *, 'avail_potet<0', i, avail_potet, Potet(i), Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i), hruactet
          avail_potet = 0.0
          hruactet = Potet(i)
        ENDIF

        dunnianflw = 0.0
        dunnianflw_pfr = 0.0
        dunnianflw_gvr = 0.0
        dunnianflw_frz = 0.0
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
        ! infil_ag for agriculture portion of HRU
        capwater_maxin = Infil(i)
        ag_water_maxin = Infil_ag(i)

        IF ( irrigation_apply_flag == 3 ) THEN
          IF ( Hru_ag_irr(i)>0.0 ) THEN ! Hru_ag_irr is in acre-inches over ag area
            if ( gsflow_ag_area(i)>0.0 ) then
              ag_water_maxin = ag_water_maxin + Hru_ag_irr(i) / gsflow_ag_area(i)
            else ! needed in case dynamic ag_frac causes a problem
              PRINT *, 'ag_frac=0.0 for HRU:', i, Nowyear, Nowmonth, Nowday
              CALL error_stop('AG Package irrigation specified and ag_frac=0.0', ERROR_param)
            endif
          ENDIF
        ENDIF

!!!caution, soilzone module adds irrigation to pervious area, soilzone_ag adds to ag area
        IF ( Model==MODSIM_PRMS .OR. Model==MODSIM_PRMS_LOOSE ) THEN
          IF ( HRU_diversion(i)>0.0 ) THEN
            IF ( ag_on_flag==OFF ) THEN
              PRINT *, 'ag_frac=0.0 for HRU:', i
              CALL error_stop('MODSIM diversion specified and ag_frac=0.0', ERROR_param)
            ENDIF
            ag_water_maxin = ag_water_maxin + HRU_diversion(i) / agarea
          ENDIF
        ENDIF

        IF ( Iter_aet_flag==ACTIVE ) ag_water_maxin = ag_water_maxin + Ag_irrigation_add(i) ! units of inches over Ag_area
        IF ( Soilzone_add_water_use==ACTIVE ) THEN
          IF ( Soilzone_gain(i)>0.0 ) THEN
            IF ( perv_on_flag==OFF ) CALL error_stop('pervious area must be > 0 for soilzone transfer', ERROR_param)
            Soilzone_gain_hru(i) = (Soilzone_gain(i)/perv_area)/SNGL(Cfs_conv) ! ??? is this harea
            IF ( ag_on_flag==ACTIVE ) THEN
              Soilzone_gain_hru(i) = (Soilzone_gain(i)/agarea)/SNGL(Cfs_conv)
              ag_water_maxin = ag_water_maxin + Soilzone_gain_hru(i)
            ELSE
              Soilzone_gain_hru(i) = (Soilzone_gain(i)/perv_area)/SNGL(Cfs_conv)
              capwater_maxin = capwater_maxin + Soilzone_gain_hru(i)
            ENDIF
          ENDIF
        ENDIF

        cfgi_frozen_hru = OFF
        !Frozen is HRU variable that says if frozen gravity reservoir
        ! For CFGI all inflow is assumed to be Dunnian Flow when frozen
        IF ( Frozen_flag==ACTIVE ) THEN
          IF ( Frozen(i)==ACTIVE ) THEN
!            IF ( compute_lateral==OFF ) THEN
!              PRINT *, 'ERROR, a swale HRU cannot be frozen for CFGI, HRU:', i
!              ERROR STOP ERROR_param
!            ENDIF
            cfgi_frozen_hru = ACTIVE
          ENDIF
        ENDIF

        ! compute preferential flow and storage, and any dunnian flow, no cascading dunnian added
        ! pref_flow for whole HRU
! ??? should cascading flow go to preferential flow fraction ???
        prefflow = 0.0
        IF ( Pref_flag == ACTIVE ) THEN
          IF ( Pref_flow_infil_frac(i)>0.0 ) THEN
            pref_flow_maxin = 0.0
            ag_pref_flow_maxin = 0.0
            IF ( ag_water_maxin>0.0 ) THEN
              ag_pref_flow_maxin = ag_water_maxin*Pref_flow_infil_frac(i)
              ag_water_maxin = ag_water_maxin - ag_pref_flow_maxin
              ag_pref_flow_maxin = ag_pref_flow_maxin*agfrac
            ENDIF
            cap_pref_flow_maxin = 0.0
            IF ( capwater_maxin>0.0 ) THEN
              ! pref_flow for whole HRU
              cap_pref_flow_maxin = capwater_maxin*Pref_flow_infil_frac(i)
              capwater_maxin = capwater_maxin - cap_pref_flow_maxin
              cap_pref_flow_maxin = cap_pref_flow_maxin*perv_frac
              pref_flow_maxin = cap_pref_flow_maxin + ag_pref_flow_maxin
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
              Pfr_dunnian_flow(i) = dunnianflw_pfr
            ENDIF
          ENDIF
        ENDIF

        IF ( Cascade_flag>CASCADE_OFF ) THEN
          upflow_max = SNGL(Upslope_dunnianflow(i)+Upslope_interflow(i))
          IF ( upflow_max > 0.0 ) THEN
            IF ( perv_on_flag==ACTIVE ) THEN
              perv_portion = perv_frac / (perv_frac + agfrac)
              cap_upflow_max = upflow_max * perv_portion / perv_frac
              capwater_maxin = capwater_maxin + cap_upflow_max
              Basin_cap_up_max = Basin_cap_up_max + DBLE( cap_upflow_max*perv_area )
            ENDIF
            IF ( ag_on_flag==ACTIVE ) THEN
              ag_portion = agfrac / (perv_frac + agfrac)
              ag_upflow_max = upflow_max * ag_portion / agfrac
              ag_water_maxin = ag_water_maxin + ag_upflow_max
              Basin_ag_up_max = Basin_ag_up_max + DBLE( ag_upflow_max*agarea )
            ENDIF
          ENDIF
        ENDIF

        IF ( perv_on_flag==ACTIVE ) THEN
          Cap_infil_tot(i) = capwater_maxin*perv_frac
          Basin_cap_infil_tot = Basin_cap_infil_tot + DBLE( Cap_infil_tot(i)*harea )
        ENDIF
        IF ( ag_on_flag==ACTIVE ) THEN
          Ag_cap_infil_tot(i) = ag_water_maxin*agfrac
          Basin_ag_cap_infil_tot = Basin_ag_cap_infil_tot + DBLE( Ag_cap_infil_tot(i)*harea )
        ENDIF

!******Add infiltration to soil and compute excess
        perv_soil_to_gvr(i) = 0.0

        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( perv_on_flag==ACTIVE ) THEN
            ! call even if capwater_maxin = 0, just in case soil_moist now > Soil_moist_max
            IF ( capwater_maxin+Soil_moist(i)>0.0 ) THEN
              CALL compute_soilmoist(capwater_maxin, Soil_moist_max(i), &
     &             Soil_rechr_max(i), Soil2gw_max(i), perv_soil_to_gvr(i), &
     &             Soil_moist(i), Soil_rechr(i), perv_soil_to_gw(i), perv_frac)
            ENDIF
          ENDIF
          IF ( ag_on_flag==ACTIVE ) THEN
            IF ( ag_water_maxin+Ag_soil_moist(i)>0.0 ) THEN
!              if ( Ag_soil_moist(i)<Ag_soil_rechr(i) ) print *, 'AG1 soilrechr, before', i, &
!     &             Ag_soil_moist(i)-Ag_soil_rechr(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i),perv_soil_to_gvr(i)
              CALL compute_soilmoist(ag_water_maxin, Ag_soil_moist_max(i), &
     &                               Ag_soil_rechr_max(i), Soil2gw_max(i), ag_soil_to_gvr(i), &
     &                               Ag_soil_moist(i), Ag_soil_rechr(i), ag_soil_to_gw(i), agfrac)
              !if ( Ag_soil_moist(i)< Ag_soil_rechr(i)) print *, 'AG1 soilrechr, after', i, &
              !     Ag_soil_moist(i)-Ag_soil_rechr(i), Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              Ag_water_in(i) = ag_water_maxin
            ENDIF
          ENDIF
          Soil_to_gw(i) = perv_soil_to_gw(i) + ag_soil_to_gw(i)
          Soil_to_ssr(i) = perv_soil_to_gvr(i) + ag_soil_to_gvr(i)
          IF ( activeHru_inactiveCell_flag == ACTIVE ) THEN
            IF ( activeHru_inactiveCell(i) == ACTIVE ) THEN
              Soil_to_prmsgw(i) = Soil_to_gw(i)
              Soil_to_gw(i) = 0.0
            ENDIF
            Basin_soil_to_prmsgw = Basin_soil_to_prmsgw + DBLE( Soil_to_prmsgw(i)*harea )
          ELSE
            Basin_soil_to_gw = Basin_soil_to_gw + DBLE( Soil_to_gw(i)*harea )
          ENDIF
          Basin_sm2gvr_max = Basin_sm2gvr_max + DBLE( Soil_to_ssr(i)*harea )
          Basin_perv_to_gw = Basin_perv_to_gw + DBLE ( perv_soil_to_gw(i)*harea )
        ELSE
          IF ( compute_lateral==ACTIVE ) THEN
            Sroff(i) = Sroff(i) + capwater_maxin
            Basin_sroff = Basin_sroff + DBLE( Sroff(i)*harea )
            IF ( perv_on_flag==ACTIVE ) THEN
              dunnianflw_frz = capwater_maxin
              capwater_maxin = 0.0
            ENDIF
            IF ( ag_on_flag==ACTIVE ) THEN
              dunnianflw_frz = dunnianflw_frz + Ag_water_in(i)
              Ag_water_in(i) = 0.0
            ENDIF
          ELSE
            IF ( perv_on_flag==ACTIVE ) THEN
              Soil_moist(i) = Soil_moist(i) + capwater_maxin*perv_frac
              Soil_rechr(i) = Soil_rechr(i) + capwater_maxin*perv_frac
            ENDIF
            IF ( ag_on_flag==ACTIVE ) THEN
              Ag_soil_moist(i) = Ag_soil_moist(i) + ag_water_maxin
              Ag_soil_rechr(i) = Ag_soil_rechr(i) + ag_water_maxin
            ENDIF
          ENDIF
        ENDIF
        Cap_waterin(i) = capwater_maxin*perv_frac
        Basin_capwaterin = Basin_capwaterin + DBLE( Cap_waterin(i)*harea )

! compute slow interflow and ssr_to_gw
        topfr = 0.0
        IF ( GSFLOW_flag==ACTIVE .AND. activeHru_inactiveCell(i) == OFF ) THEN
          ! capacity for whole HRU
          capacity = (Soil_moist_max(i) - Soil_moist(i))*perv_frac
          ag_capacity = (Ag_soil_moist_max(i) - Ag_soil_moist(i))*agfrac
          CALL compute_gravflow_ag(i, capacity, Slowcoef_lin(i), &
     &                          Slowcoef_sq(i), Ssr2gw_rate(i), Ssr2gw_exp(i), &
     &                          Soil_to_ssr(i), Pref_flow_thrsh(i), topfr, &
     &                          Ssr_to_gw(i), Slow_flow(i), Slow_stor(i), &
     &                          Gvr2sm(i), Soil_to_gw(i), gwin, compute_lateral, ag_capacity, Gvr2ag(i))
          ! adjust soil moisture with replenish amount
          IF ( Gvr2sm(i)>0.0 ) THEN
            IF ( perv_on_flag==ACTIVE ) THEN
              Soil_moist(i) = Soil_moist(i) + Gvr2sm(i)/perv_frac ! ??? could this be bigger than soil_moist_max ??? (add to Dunnian)
!            IF ( Soil_moist(i)>Soil_moist_max(i) ) PRINT *, 'CAP sm>max', Soil_moist(i), Soil_moist_max(i), i
              IF ( Soilzone_aet_flag==ACTIVE ) THEN
                Soil_lower(i) = MIN( Soil_lower_stor_max(i), Soil_moist(i) - Soil_rechr(i) + Gvr2sm(i)/perv_frac )
                Soil_rechr(i) = MIN( Soil_rechr_max(i), Soil_moist(i) - Soil_lower(i) )
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
          Grav_gwin(i) = SNGL( gwin )
          Basin_sz_gwin = Basin_sz_gwin + gwin*Hru_area_dble(i)
          IF ( Gvr2ag(i)>0.0 ) THEN
            Ag_soil_moist(i) = Ag_soil_moist(i) + Gvr2ag(i)/agfrac
            !IF ( Ag_soil_moist(i)>Ag_soil_moist_max(i) ) THEN
              !IF ( ABS(Ag_soil_moist(i)-Ag_soil_moist_max(i)) > NEARZERO ) &
              !  PRINT *, 'AG sm>max', Ag_soil_moist(i), Ag_soil_moist_max(i), i, Gvr2ag(i)/agfrac, &
              !           Gvr2ag(i),agfrac,ag_capacity
              !Ag_soil_moist(i) = Ag_soil_moist_max(i)
            !ENDIF
            IF ( Soilzone_aet_flag==ACTIVE ) THEN
              Ag_soil_lower(i) = MIN( Ag_soil_lower_stor_max(i), Ag_soil_moist(i) - (Ag_soil_rechr(i) + Gvr2ag(i)/agfrac) )
              Soil_rechr(i) = MIN( Soil_rechr_max(i), Soil_moist(i) - Ag_soil_lower(i) )
            ELSE
              Ag_soil_rechr(i) = MIN( Ag_soil_rechr_max(i), Ag_soil_rechr(i) + (Gvr2ag(i)/agfrac)*Ag_replenish_frac(i) )
            ENDIF
            Basin_ag_gvr2sm = Basin_ag_gvr2sm + DBLE( Gvr2ag(i)*harea )
          ENDIF
        ELSE
          availh2o = Slow_stor(i) + Soil_to_ssr(i)
          IF ( compute_lateral==ACTIVE ) THEN
            IF ( Pref_flag==ACTIVE ) topfr = MAX( 0.0, availh2o-Pref_flow_thrsh(i) )
            ssresin = Soil_to_ssr(i) - topfr
            Slow_stor(i) = availh2o - topfr
            ! compute slow contribution to interflow, if any
            IF ( Slow_stor(i)>0.0 ) &
     &           CALL compute_interflow(Slowcoef_lin(i), Slowcoef_sq(i), &
     &                                  ssresin, Slow_stor(i), Slow_flow(i))
          ELSE ! compute_lateral==OFF
            Slow_stor(i) = availh2o
          ENDIF
          IF ( Slow_stor(i)>0.0 .AND. Ssr2gw_rate(i)>0.0 ) THEN
            CALL compute_gwflow(Ssr2gw_rate(i), Ssr2gw_exp(i), Ssr_to_gw(i), Slow_stor(i))
            IF ( activeHru_inactiveCell(i) == ACTIVE ) THEN
              Ssr_to_prmsgw(i) = Ssr_to_gw(i)
              Ssr_to_gw(i) = 0.0
              Basin_sz2gwprms = Basin_sz2gwprms + DBLE( Ssr_to_prmsgw(i)*harea )
            ENDIF
          ENDIF
        ENDIF

        ! compute contribution to Dunnian flow from PFR, if any; if frozen or swale don't compute Dunnian
        IF ( Pref_flag==ACTIVE ) THEN
          IF ( Pref_flow_max(i)>0.0 ) THEN
            IF ( cfgi_frozen_hru==OFF ) THEN
              availh2o = Pref_flow_stor(i) + topfr
              dunnianflw_gvr = MAX( 0.0, availh2o-Pref_flow_max(i) )
              IF ( dunnianflw_gvr>0.0 ) THEN
                topfr = topfr - dunnianflw_gvr
                IF ( topfr<0.0 ) THEN
!                  IF ( topfr<-NEARZERO .AND. Print_debug>DEBUG_less ) PRINT *, 'gvr2pfr<0', topfr, dunnianflw_gvr, &
!     &                 Pref_flow_max(i), Pref_flow_stor(i), Soil_to_ssr(i)
                  topfr = 0.0
                ENDIF
              ENDIF
              Pref_flow_in(i) = Pref_flow_infil(i) + topfr
              Pref_flow_stor(i) = Pref_flow_stor(i) + topfr
              IF ( Pref_flow_stor(i)>0.0 ) &
     &             CALL compute_interflow(Fastcoef_lin(i), Fastcoef_sq(i), &
     &                                    Pref_flow_in(i), Pref_flow_stor(i), prefflow)
            ELSE ! add water from slow storage to preferential flow storage when frozen, pref_flow_stor can be > pref_flow_max
              Pref_flow_in(i) = Pref_flow_infil(i) + topfr
              Pref_flow_stor(i) = Pref_flow_stor(i) + topfr
            ENDIF
            Basin_pref_stor = Basin_pref_stor + DBLE( Pref_flow_stor(i)*harea )
            Basin_pfr_stor_frac = Basin_pfr_stor_frac + DBLE( Pref_flow_stor(i)/Pref_flow_max(i)*harea )
          ENDIF
        ELSEIF ( .not.(Pref_flow_max(i)>0.0) ) THEN
          IF ( compute_lateral==ACTIVE ) dunnianflw_gvr = topfr  !?? is this right
        ENDIF
        Gvr2pfr(i) = topfr

        Basin_sm2gvr = Basin_sm2gvr + DBLE( Soil_to_ssr(i)*harea )
        Basin_dunnian_gvr = Basin_dunnian_gvr + DBLE( dunnianflw_gvr*harea )
        Basin_sz2gw = Basin_sz2gw + DBLE( Ssr_to_gw(i)*harea )

!******Compute actual evapotranspiration
        pervactet = 0.0
        agactet = 0.0
        ag_hruactet = 0.0
        unsatisfied_ag_et = 0.0

        IF ( cfgi_frozen_hru==OFF ) THEN
          IF ( ag_on_flag==ACTIVE ) THEN
            IF ( Iter_aet_flag==ACTIVE ) THEN
              !soilwater_deficit = MAX( sz_deficit_param, ag_soil_moist(i)/ag_soil_moist_max(i) ) irrigation happens at a deficit threshold
              ag_AETtarget = AET_external(i)
            ELSE
              ag_AETtarget = Potet(i)
            ENDIF

            ! assume canopy interception evaporation is accounted for in intcp module, so subtract here
            ! assume impervious, snow, and dprst evap not in ag fraction
            ! assume ag fraction only contains irrigated land with vegetation
            ag_avail_targetAET = ag_AETtarget - Hru_intcpevap(i)
            IF ( ag_avail_targetAET < 0.0 ) ag_avail_targetAET = 0.0

            IF ( ag_avail_targetAET>0.0 ) THEN
              !if ( Ag_soil_moist(i) < Ag_soil_rechr(i) ) print *, 'AG1 szactet, before', i, Ag_soil_moist(i)-Ag_soil_rechr(i), &
              !     Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              !if (.not.(ag_soil_moist_max(i)>0) ) then
              !  print *, 'ag soil max', i, ag_soil_moist_max(i), ag_frac(i), ag_soil_moist(i)
              !  print *, Nowyear, Nowmonth, Nowday
              !  ag_soil_moist_max(i) = soil_moist_max(i)
              !endif
              CALL compute_szactet(Ag_soil_moist_max(i), Ag_soil_rechr_max(i), Transp_on(i), Ag_cov_type(i), &
     &                             Ag_soil_type(i), Ag_soil_moist(i), Ag_soil_rechr(i), agactet, ag_avail_targetAET, & !?? instead of ag_avail_potet use AET_external
     &                             Snow_free(i), Ag_potet_rechr(i), Ag_potet_lower(i), &
     &                             ag_AETtarget, agfrac, Ag_soil_saturated(i), i, 1)
              !if ( Ag_soil_moist(i)< Ag_soil_rechr(i)) print *, 'AG1 szactet, after', i, Ag_soil_moist(i)-Ag_soil_rechr(i), &
                    !Ag_soil_moist(i), Ag_soil_rechr(i), Ag_soil_moist_max(i), Ag_soil_rechr_max(i)
              ! sanity check
              IF ( Iter_aet_flag==ACTIVE ) THEN
                IF ( agactet-ag_AETtarget>NEARZERO ) THEN
                  PRINT *, 'ag_actet issue', agactet, ag_avail_potet, agfrac, AET_external(i), ag_potet, i, hruactet
                  PRINT *, ag_AETtarget-agactet, Ag_soil_moist(i), Ag_soil_rechr(i)
                ENDIF
                IF ( ag_AETtarget<agactet ) THEN
                  PRINT *, 'WARNING, external agriculture available target AET from CBH File < computed AET', i, &
     &                     Nowyear, Nowmonth, Nowday, num_hrus_ag_iter
                  PRINT '(4(A,F0.6))', '         AET_external: ', ag_AETtarget, '; ag_actet: ', &
     &                  agactet, ' PET_external: ', PET_external(i), ' AET_external: ', AET_external(i)
                  print *, Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
                ENDIF
              ENDIF
              ag_hruactet = agactet*agfrac
            ENDIF
            unsatisfied_ag_et = ag_avail_targetAET - agactet
            !if ( unsatisfied_ag_et<0.0 ) print *, 'unused ag et problem', unsatisfied_ag_et, i, soilzone_aet_converge, ag_AETtarget, agactet
            Unused_ag_et(i) = unsatisfied_ag_et
            ! assume canopy interception is accounted for in intcp module, so add here
            Ag_actet(i) = agactet + Hru_intcpevap(i)
          ENDIF

          avail_potet = Potet(i) - hruactet - ag_hruactet
!          IF ( avail_potet<0.0 ) THEN
!            IF ( avail_potet<-CLOSEZERO ) &
!                 print *, 'AG avail_potet<0', i, avail_potet, Potet(i), Hru_impervevap(i), &
!                          Hru_intcpevap(i), Snow_evap(i), hruactet, ag_hruactet, agactet, agfrac, ag_AETtarget
!            avail_potet = 0.0
!          ENDIF
          IF ( Soil_moist(i)>0.0 .AND. avail_potet>0.0 ) THEN
            CALL compute_szactet(Soil_moist_max(i), Soil_rechr_max(i), Transp_on(i), Cov_type(i), &
     &                           Soil_type(i), Soil_moist(i), Soil_rechr(i), pervactet, avail_potet, &
     &                           Snow_free(i), Potet_rechr(i), Potet_lower(i), &
     &                           Potet(i), perv_frac, Soil_saturated(i), i, 0)
          ENDIF
        ENDIF

        hru_perv_actet(i) = pervactet * perv_frac
        hru_ag_actet(i) = ag_hruactet
        Hru_actet(i) = hruactet + hru_perv_actet(i) + ag_hruactet
        IF ( Ag_package==ACTIVE ) gsflow_ag_actet(i) = Ag_actet(i)
        Perv_actet(i) = pervactet

! soil_moist & soil_rechr multiplied by perv_area instead of harea
        Soil_lower(i) = Soil_moist(i) - Soil_rechr(i)
        Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(i)*perv_area )
        Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(i)*perv_area )
        Basin_perv_et = Basin_perv_et + DBLE( Perv_actet(i)*perv_area )

! if HRU cascades,
! compute interflow and excess flow to each HRU or stream
        IF ( compute_lateral==ACTIVE ) THEN
          interflow = Slow_flow(i) + prefflow
          Basin_interflow_max = Basin_interflow_max + interflow*harea
          dunnianflw = dunnianflw_gvr + dunnianflw_pfr + dunnianflw_frz
          Dunnian_flow(i) = dunnianflw
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            IF ( Ncascade_hru(i)>0 ) THEN
              IF ( interflow+dunnianflw>cascade_min ) THEN
                dnslowflow = 0.0D0
                dnpreflow = 0.0D0
                dndunn = 0.0D0
                CALL compute_cascades(i, Ncascade_hru(i), Slow_flow(i), &
     &                                prefflow, Dunnian_flow(i), dnslowflow, &
     &                                dnpreflow, dndunn)
                Basin_dninterflow = Basin_dninterflow + (dnslowflow+dnpreflow)*Hru_area_dble(i)
                Basin_dndunnianflow = Basin_dndunnianflow + dndunn*Hru_area_dble(i)
                Hru_sz_cascadeflow(i) = dnslowflow + dnpreflow + dndunn
                Hru_dunnian_cascadeflow(i) = dndunn
                Hru_interflow_cascadeflow(i) = dnslowflow + dnpreflow
                Basin_dncascadeflow = Basin_dncascadeflow + Hru_sz_cascadeflow(i)*Hru_area_dble(i)
              ENDIF
            ENDIF
          ENDIF

! treat pref_flow as interflow
          Ssres_flow(i) = Slow_flow(i)
          IF ( Pref_flag == ACTIVE ) THEN
            IF ( Pref_flow_max(i)>0.0 ) THEN
              Pref_flow(i) = prefflow
              Ssres_flow(i) = Ssres_flow(i) + prefflow
              Basin_prefflow = Basin_prefflow + DBLE( prefflow*harea )
              Basin_gvr2pfr = Basin_gvr2pfr + DBLE( Gvr2pfr(i)*harea )
            ENDIF
          ENDIF
          Basin_ssflow = Basin_ssflow + DBLE( Ssres_flow(i)*harea )
          Basin_slowflow = Basin_slowflow + DBLE( Slow_flow(i)*harea )

! treat dunnianflw as surface runoff to streams
          Sroff(i) = Sroff(i) + Dunnian_flow(i)
          Basin_sroff = Basin_sroff + DBLE( Sroff(i)*harea )
          Basin_dunnian = Basin_dunnian + DBLE( Dunnian_flow(i)*harea )
          Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)

        ELSE ! for swales ! note, swales don't have PFR
          availh2o = Slow_stor(i) - Sat_threshold(i)
          Swale_actet(i) = 0.0
          IF ( availh2o>0.0 ) THEN ! if ponding, as storage > sat_threshold
            unsatisfied_et = Potet(i) - Hru_actet(i)
            IF ( unsatisfied_et>0.0 ) THEN
              availh2o = MIN ( availh2o, unsatisfied_et )
              Swale_actet(i) = availh2o
              Hru_actet(i) = Hru_actet(i) + Swale_actet(i)
              Slow_stor(i) = Slow_stor(i) - Swale_actet(i)
              IF ( GSFLOW_flag==ACTIVE ) THEN
                 DO j = 1, Hru_gvr_count(i)
                    igvr = Hru_gvr_index(j, i)
                    Gravity_stor_res(igvr) = Gravity_stor_res(igvr) - Swale_actet(i)
                 ENDDO
              ENDIF
              Basin_swale_et = Basin_swale_et + DBLE( Swale_actet(i)*harea )
            ENDIF
            IF ( Print_debug==7 ) THEN
              IF ( Slow_stor(i)>Swale_limit(i) ) THEN
                WRITE ( DBGUNT, * ) 'Swale ponding, HRU:', i, &
     &                    ' gravity reservoir is 3*sat_threshold', Slow_stor(i), Sat_threshold(i)
                CALL print_date(DBGUNT)
              ENDIF
            ENDIF
          ENDIF
          Ssres_stor(i) = Slow_stor(i)
        ENDIF

        IF ( Soil_lower_stor_max(i)>0.0 ) Soil_lower_ratio(i) = Soil_lower(i)/Soil_lower_stor_max(i)
        Ssres_in(i) = Soil_to_ssr(i) + Pref_flow_infil(i) + SNGL( gwin )
        Basin_ssin = Basin_ssin + DBLE( Ssres_in(i)*harea )
        Basin_ssstor = Basin_ssstor + DBLE( Ssres_stor(i)*harea )
        Basin_slstor = Basin_slstor + DBLE( Slow_stor(i)*harea )
        Unused_potet(i) = Potet(i) - Hru_actet(i)
        ! sanity check
!        IF ( Unused_potet(i)<-CLOSEZERO ) THEN
!          IF ( Print_debug>DEBUG_less ) THEN
!            IF ( avail_potet<-NEARZERO ) THEN
!              PRINT *, 'hru_actet>potet', i, Nowmonth, Nowday, Unused_potet(i)
!              PRINT *, Hru_actet(i), Potet(i), ag_hruactet, pervactet*perv_frac, perv_frac, agfrac
!              print *, 'potet', Potet(i), ag_avail_targetAET
!              PRINT *, 'hru_actet', Hru_actet(i), Hru_impervevap(i) + Hru_intcpevap(i) + Snow_evap(i), &
!     &                 Hru_impervevap(i), Hru_intcpevap(i), Snow_evap(i)
!            ENDIF
!          ENDIF
!          Hru_actet(i) = Potet(i)
!          Unused_potet(i) = 0.0
!          IF ( perv_on_flag==ACTIVE ) THEN
!            tmp = Unused_potet(i)/perv_frac
!            pervactet = pervactet + tmp
!            Soil_moist(i) = Soil_moist(i) - tmp
!            Soil_rechr(i) = Soil_rechr(i) - tmp
!            IF ( Soil_rechr(i)<0.0 ) Soil_rechr(i) = 0.0
!            IF ( Soil_moist(i)<0.0 ) Soil_moist(i) = 0.0
!          ENDIF
!        ENDIF
!        if (perv_actet(i)<0.0) then
!            print *, i, 'perv_actet', perv_actet(i), potet(i), Unused_potet(i), hru_actet(i), agactet, hruactet
!            print *, soil_moist(i), soil_moist_max(i), soil_rechr(i), soil_rechr_max(i), soil_lower(i), perv_frac
!            print *, grav_gwin(i), gvr2sm(i)
!            endif
        Soil_moist_tot(i) = Ssres_stor(i) + Soil_moist(i)*perv_frac + Ag_soil_moist(i)*agfrac
        Basin_soil_moist_tot = Basin_soil_moist_tot + DBLE( Soil_moist_tot(i)*harea )
        IF ( perv_on_flag==ACTIVE ) THEN
          Basin_cpr_stor_frac = Basin_cpr_stor_frac + DBLE( Soil_moist(i)/Soil_moist_max(i)*perv_area )
          Basin_sz_stor_frac = Basin_sz_stor_frac + DBLE( Soil_moist_tot(i)/Soil_zone_max(i)*harea )
          Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac + DBLE( Soil_lower_ratio(i)*perv_area )
          IF ( Soil_rechr_max(i)>0.0 ) Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac + &
     &         DBLE( Soil_rechr(i)/Soil_rechr_max(i)*perv_area )
        ENDIF
        Recharge(i) = Soil_to_gw(i) + Ssr_to_gw(i)
        IF ( Dprst_flag==1 ) Recharge(i) = Recharge(i) + SNGL( Dprst_seep_hru(i) )
        Basin_recharge = Basin_recharge + DBLE( Recharge(i)*harea )
        Grav_dunnian_flow(i) = dunnianflw_gvr
        IF ( ag_on_flag==ACTIVE ) THEN
          Ag_soil_lower(i) = Ag_soil_moist(i) - Ag_soil_rechr(i)
          Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(i)*agarea )
          Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(i)*agarea )
          Basin_ag_actet = Basin_ag_actet + DBLE( Ag_actet(i)*agarea )
          IF ( Iter_aet_flag==ACTIVE .AND. Transp_on(i)==ACTIVE ) THEN
          !IF ( Iter_aet_flag==ACTIVE ) THEN
            !agriculture_external(i)
            !IF ( Unused_potet(i)>0.0 ) THEN
            IF ( unsatisfied_ag_et>soilzone_aet_converge ) THEN
              Ag_soilwater_deficit(i) = (Ag_soil_moist_max(i) - Ag_soil_moist(i)) / Ag_soil_moist_max(i)
              IF ( Ag_soilwater_deficit(i)>Ag_soilwater_deficit_min(i) ) THEN
                IF ( unsatisfied_ag_et > ag_soil_moist_max(i) ) THEN
                  !print *, 'unsat>max', i, unsatisfied_ag_et, ag_soil_moist_max(i)
                  unsatisfied_max = ag_soil_moist_max(i) ! temporary fix, need better values for ag_soil_moist_max
                ELSE
                  unsatisfied_max = unsatisfied_ag_et
                  IF ( Soil_iter > 20 ) unsatisfied_max = unsatisfied_max + unsatisfied_ag_et ! this doubles small values so solution converges faster
                  keep_iterating = ACTIVE
                  add_estimated_irrigation = ACTIVE
                  num_hrus_ag_iter = num_hrus_ag_iter + 1
                ENDIF
                Ag_irrigation_add(i) = Ag_irrigation_add(i) + unsatisfied_max
                !if ( Ag_irrigation_add(i)>2.5 ) then
                !    print *, 'large irrigation', i, Ag_irrigation_add(i), ag_AETtarget, unsatisfied_ag_et, ag_soil_moist(i), ag_soil_rechr(i), soil_iter
                !endif
!                IF ( Ag_irrigation_add(i)>max_irrigation ) max_irrigation = Ag_irrigation_add(i)
                IF ( unsatisfied_max>unsatisfied_big ) unsatisfied_big = unsatisfied_max
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        Basin_actet = Basin_actet + DBLE( Hru_actet(i)*harea )
        Hru_storage(i) = DBLE( Soil_moist_tot(i) + Hru_intcpstor(i) + Hru_impervstor(i) ) + Pkwater_equiv(i)
        IF ( Dprst_flag==ACTIVE ) Hru_storage(i) = Hru_storage(i) + Dprst_stor_hru(i)

! ***************************************
      ENDDO ! end HRU loop
! ***************************************

      Soil_iter = Soil_iter + 1
      IF ( Iter_aet_flag==OFF .OR. Basin_transp_on==OFF ) keep_iterating = OFF
      IF ( Soil_iter>max_soilzone_ag_iter .OR. add_estimated_irrigation==OFF ) keep_iterating = OFF
!      if (ag_actet(i)>potet(i)) print *, 'ag_actet>potet', ag_actet(i), potet(i)
!      if (unused_potet(i)<0.00) print *, 'unused', unused_potet(i), potet(i)
!      if ( hru_actet(i)>potet(i)) print *, 'hru_actet', hru_actet(i), potet(i)

! ***************************************
      ENDDO ! end iteration while loop
! ***************************************

      Soil_iter = Soil_iter - 1
      IF ( Iter_aet_flag == ACTIVE ) THEN
        IF ( Print_debug>-1 ) PRINT '(/,I5,2("/",I0), 2(A,I0),A,I0,/)', Nowyear, Nowmonth, Nowday, &
                                                                        ' AET iterations: ', soil_iter
        !print '(a,F0.5)', '; maximum irrigation:', max_irrigation
        Basin_ag_irrigation_add = 0.0D0
        IF ( Basin_transp_on == ACTIVE ) THEN
          Ag_irrigation_add_vol = Ag_irrigation_add*Ag_area
          ag_AET_external_vol = ag_AET_external_vol*Ag_area
          do i = 1, Nhru ! temporary to put mask in nhru_summary file
            if (ag_frac(i)>0.0 ) then
                Basin_ag_irrigation_add = Basin_ag_irrigation_add + DBLE( Ag_irrigation_add_vol(i) )
                if ( ag_irrigation_add(i)>0.0)then
                    if (ag_soil_moist_max(i)-ag_soil_moist(i) < 0.0001 ) then
                        print *, 'ag soil full', i, ag_soil_moist_max(i), ag_soil_moist(i), ag_irrigation_add(i), &
                                 Ag_soilwater_deficit(i)
                        print *, ag_actet(i), aet_external(i)
                    endif
                endif
            endif
          enddo
          Basin_ag_irrigation_add = Basin_ag_irrigation_add / Ag_area_total
        ENDIF
        IF ( num_hrus_ag_iter > 0 ) print '(/,2(A,I0))', 'number of hrus still iterating on AET: ', num_hrus_ag_iter
        if ( Soil_iter == max_soilzone_ag_iter ) then
           iter_nonconverge = iter_nonconverge + 1
           print '(/,A,I0,2("/",I0))', 'WARNING, ag AET did not converge due to max_soilzone_ag_iter: ', Nowyear, Nowmonth, Nowday
           print '(A,F0.4,2(A,I0))', '         largest AET-ag_actet: ', unsatisfied_big, '; iterations: ', Soil_iter, &
                                    '; number of non-convergence: ', iter_nonconverge
           print '(A,F0.6,A,I0)', '         convergence criteria: ', soilzone_aet_converge, &
                                  '; maximum iterations: ', max_soilzone_ag_iter
        ENDIF
      ENDIF
      Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
      Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv
      Basin_ag_actet = Basin_ag_actet*Basin_area_inv
      Basin_perv_to_gw = Basin_perv_to_gw*Basin_area_inv
      Basin_ag_gvr2sm = Basin_ag_gvr2sm*Basin_area_inv
      Basin_ag_cap_infil_tot = Basin_ag_cap_infil_tot*Basin_area_inv
      Basin_actet = Basin_actet*Basin_area_inv
      Basin_perv_et = Basin_perv_et*Basin_area_inv
      Basin_swale_et = Basin_swale_et*Basin_area_inv
      Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
      Basin_soil_to_gw = Basin_soil_to_gw*Basin_area_inv
      IF ( activeHru_inactiveCell_flag == ACTIVE ) THEN
        Basin_soil_to_prmsgw = Basin_soil_to_prmsgw*Basin_area_inv
        Basin_sz2gwprms = Basin_sz2gwprms*Basin_area_inv
      ENDIF
      Basin_soil_moist = Basin_soil_moist*Basin_area_inv
      Basin_soil_moist_tot = Basin_soil_moist_tot*Basin_area_inv
      IF ( Nlake>0 ) THEN
        Basin_lakeevap = Basin_lakeevap*Basin_area_inv
        Basin_lakeprecip = Basin_lakeprecip*Basin_area_inv
        Basin_lake_stor = Basin_lake_stor + Basin_lakeprecip - Basin_lakeevap
      ENDIF
      IF ( Pref_flag==ACTIVE ) THEN
        Basin_pref_stor = Basin_pref_stor*Basin_area_inv
        Basin_pref_flow_infil = Basin_pref_flow_infil*Basin_area_inv
        Basin_prefflow = Basin_prefflow*Basin_area_inv
        Basin_dunnian_pfr = Basin_dunnian_pfr*Basin_area_inv
        Basin_pfr_stor_frac = Basin_pfr_stor_frac*Basin_area_inv
        Basin_gvr2pfr = Basin_gvr2pfr*Basin_area_inv
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
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Basin_dninterflow = Basin_dninterflow*Basin_area_inv
        Basin_dndunnianflow = Basin_dndunnianflow*Basin_area_inv
        Basin_dncascadeflow = Basin_dncascadeflow*Basin_area_inv
        Basin_lakeinsz = Basin_lakeinsz*Basin_area_inv
        Basin_cap_up_max = Basin_cap_up_max*Basin_area_inv
      ENDIF
      Basin_slowflow = Basin_slowflow*Basin_area_inv
      Basin_recharge = Basin_recharge*Basin_area_inv
      Basin_gvr2sm = Basin_gvr2sm*Basin_area_inv
      Basin_sz_gwin = Basin_sz_gwin*Basin_area_inv
      Basin_cpr_stor_frac = Basin_cpr_stor_frac*Basin_area_inv
      Basin_sz_stor_frac = Basin_sz_stor_frac*Basin_area_inv
      Basin_soil_lower_stor_frac = Basin_soil_lower_stor_frac*Basin_area_inv
      Basin_soil_rechr_stor_frac = Basin_soil_rechr_stor_frac*Basin_area_inv
      IF ( update_potet==ACTIVE ) THEN ! need when lakes present
        Basin_potet = 0.0D0
        DO k = 1, Active_hrus
          i = Hru_route_order(k)
          Basin_potet = Basin_potet + DBLE( Potet(i)*Hru_area(i) )
        ENDDO
        Basin_potet = Basin_potet*Basin_area_inv
      ENDIF

      END FUNCTION szrun_ag

!***********************************************************************
!     compute storage of GVRs, replenishment of capillary reservoir,
!     excess storage to preferential flow reservoir, interflow and
!     flow to groundwater reservoir for GSFLOW
!***********************************************************************
      SUBROUTINE compute_gravflow_ag(Ihru, Capacity, Slowcoef_lin, &
     &           Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp, Gvr_maxin, &
     &           Pref_flow_thrsh, Gvr2pfr, Ssr_to_gw, &
     &           Slow_flow, Slow_stor, Gvr2sm, Soil_to_gw, Gwin, Compute_lateral, &
     &           Ag_capacity, Gvr2ag)
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, ACTIVE, CLOSEZERO
      USE PRMS_MODULE, ONLY: Dprst_flag, Print_debug
      USE PRMS_FLOWVARS, ONLY: Gravity_stor_res
      USE PRMS_SOILZONE, ONLY: Sm2gw_grav, Hru_gvr_count, Hru_gvr_index, &
     &    Gw2sm_grav, Gvr_hru_pct_adjusted
      USE PRMS_SRUNOFF, ONLY: Dprst_seep_hru
      IMPLICIT NONE
! Functions
      INTRINSIC :: MAX, DBLE, SNGL
      EXTERNAL :: check_gvr_sm, compute_interflow
! Arguments
      INTEGER, INTENT(IN) :: Ihru, Compute_lateral
      REAL, INTENT(IN) :: Slowcoef_lin, Slowcoef_sq, Ssr2gw_rate, Ssr2gw_exp
      REAL, INTENT(IN) :: Pref_flow_thrsh, Soil_to_gw, Gvr_maxin
      REAL, INTENT(INOUT) :: Capacity, Ag_capacity
      REAL, INTENT(OUT) :: Ssr_to_gw, Slow_stor, Slow_flow, Gvr2pfr, Gvr2sm, Gvr2ag
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
      Gvr2ag = 0.0
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
        IF ( depth > 0.0 .AND. Ag_capacity > 0.0 ) CALL check_gvr_sm(Ag_capacity, depth, frac, Gvr2ag, input)
        IF ( depth>0.0 .AND. Capacity>0.0 ) CALL check_gvr_sm(Capacity, depth, frac, Gvr2sm, input)

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
        IF ( depth>CLOSEZERO ) THEN
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

      END SUBROUTINE compute_gravflow_ag

!***********************************************************************
!     soilzone_restart_ag - write or read soilzone_ag restart file
!***********************************************************************
      SUBROUTINE soilzone_restart_ag(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, GSFLOW_flag, text_restart_flag
      USE PRMS_FLOWVARS, ONLY: Gravity_stor_res
      USE PRMS_SOILZONE
      USE PRMS_SOILZONE_AG
      use prms_utils, only: check_restart
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Local Variable
      CHARACTER(LEN=8) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        IF ( text_restart_flag==OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          IF ( GSFLOW_flag==ACTIVE ) WRITE ( Restart_outunit ) Gravity_stor_res
          WRITE ( Restart_outunit ) Ag_soil_lower
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          IF ( GSFLOW_flag==ACTIVE ) WRITE ( Restart_outunit, * ) Gravity_stor_res
          WRITE ( Restart_outunit, * ) Ag_soil_lower
        ENDIF
      ELSE
        IF ( text_restart_flag==OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          IF ( GSFLOW_flag==ACTIVE ) READ ( Restart_inunit ) Gravity_stor_res
          READ ( Restart_inunit ) Ag_soil_lower
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          IF ( GSFLOW_flag==ACTIVE ) READ ( Restart_inunit, * ) Gravity_stor_res
          READ ( Restart_inunit, * ) Ag_soil_lower
        ENDIF
      ENDIF
      END SUBROUTINE soilzone_restart_ag

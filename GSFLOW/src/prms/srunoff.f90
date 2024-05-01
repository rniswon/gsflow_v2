!***********************************************************************
! Computes surface runoff and infiltration for each HRU using a
! non-linear (smidx) and linear (carea) variable-source-area method
! Combinded smidx and carea modules 3/12/2013
!
!     version: 2.1 combined with srunoff_cfgi.f from M.C.Mastin
!          includes glacr_melt for HRU's w/ glaciers,IE: hru_type(HRU#)=4
!          modified 6/98 by JJ Vaccaro
!      MODIFIED 5/00 by JJ Vaccaro to account for irrigation application
!      MODIFIED 9/04 by M.C.Mastin to include a Continuous Frozen Ground
!         Index (cfgi) that shuts off infiltration once the index reaches
!         a threshold value (parameter cfgi_thrshld). A daily decay of
!         the index is defined by cfgi_decay parameter.
!     version: 2.2 added cascading flow for infiltration and runoff
!
! includes glacrb_melt for HRUs with glaciers and frozen ground under glaciers 08/2020
! rsr, 10/1/2008 added Vaccaro code
! rsr, 10/21/2008 added frozen ground code
! rsr, 10/30/2008 added depression storage code
! rsr, 04/11/2011 changed so dprst_area to be a parameter (does not change)
! rsr, 07/1/2013 combined smidx and carea into one module
!***********************************************************************
      MODULE PRMS_SRUNOFF
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Surface Runoff'
      character(LEN=13), save :: MODNAME
      character(len=*), parameter :: Version_srunoff = '2024-04-04'
      INTEGER, SAVE :: Ihru
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_thres_open(:), Dprst_in(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:), Dprst_vol_clos_max(:)
      REAL, SAVE, ALLOCATABLE :: Carea_dif(:)
      REAL, SAVE :: Srp, Sri, Sra, Perv_frac, Imperv_frac, Hruarea_imperv, Hruarea, Sroff_ag
      DOUBLE PRECISION, SAVE :: Hruarea_dble, Basin_apply_sroff, Basin_cfgi_sroff
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_sroff_down, Basin_sroff_upslope
      DOUBLE PRECISION, SAVE :: Basin_sroffi, Basin_sroffp
      DOUBLE PRECISION, SAVE :: Basin_imperv_stor, Basin_imperv_evap, Basin_infil
      DOUBLE PRECISION, SAVE :: Basin_hortonian, Basin_hortonian_lakes, Basin_contrib_fraction
      DOUBLE PRECISION, SAVE :: basin_ag_contrib_fraction, Basin_infil_ag
      REAL, SAVE, ALLOCATABLE :: Contrib_fraction(:), Imperv_evap(:)
      REAL, SAVE, ALLOCATABLE :: ag_contrib_fraction(:), Infil_ag(:)
      REAL, SAVE, ALLOCATABLE :: Hru_sroffp(:), Hru_sroffi(:) !, Hru_sroffa(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Hortonian_flow(:), Hru_impervevap(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hortonian_lakes(:), Hru_hortn_cascflow(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Smidx_coef(:), Smidx_exp(:)
      REAL, SAVE, ALLOCATABLE :: Carea_min(:), Carea_max(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Op_flow_thres(:), Sro_to_dprst_perv(:) !, Sro_to_dprst_ag(:)
      REAL, SAVE, ALLOCATABLE :: Va_clos_exp(:), Va_open_exp(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_flow_coef(:), Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:), Dprst_seep_rate_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_depth_avg(:), Sro_to_dprst_imperv(:), Dprst_et_coef(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl !, Basin_dprst_hortonian_lakes
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_sroff_hru(:), Dprst_seep_hru(:)
!      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_dprst_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_insroff_hru(:), Dprst_evap_hru(:)
      DOUBLE PRECISION, ALLOCATABLE :: Dprst_vol_frac(:), Dprst_vol_open_frac(:), Dprst_vol_clos_frac(:)
!   Declared Parameters for Frozen Ground
      REAL, SAVE :: Cfgi_thrshld, Cfgi_decay
!   Declared Variables for Frozen Ground
      REAL, SAVE, ALLOCATABLE :: Cfgi(:), Cfgi_prev(:)
      INTEGER, SAVE, ALLOCATABLE :: Frozen(:)
      END MODULE PRMS_SRUNOFF

!***********************************************************************
!     Main srunoff routine
!***********************************************************************
      INTEGER FUNCTION srunoff()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN, ACTIVE, READ_INIT, SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Init_vars_from_file, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: srunoffdecl, srunoffinit, srunoffrun
      EXTERNAL :: srunoff_restart
!***********************************************************************
      srunoff = 0

      IF ( Process_flag==RUN ) THEN
        srunoff = srunoffrun()
      ELSEIF ( Process_flag==DECL ) THEN
        srunoff = srunoffdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL srunoff_restart(READ_INIT)
        srunoff = srunoffinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL srunoff_restart(SAVE_INIT)
      ENDIF

      END FUNCTION srunoff

!***********************************************************************
!     srunoffdecl - set up parameters for surface runoff computations
!   Declared Parameters
!     smidx_coef, smidx_exp, carea_max, imperv_stor_max, snowinfil_max
!     hru_area, soil_moist_max, soil_rechr_max, carea_min
!     cfgi_thrshld, cfgi_decay
!***********************************************************************
      INTEGER FUNCTION srunoffdecl()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, smidx_module, carea_module, CASCADE_OFF
      use PRMS_MMFAPI, only: declvar_dble, declvar_int, declvar_real
      use PRMS_READ_PARAM_FILE, only: declparam
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Init_vars_from_file, &
     &    Dprst_flag, Cascade_flag, Sroff_flag, PRMS4_flag, Frozen_flag, AG_flag
      USE PRMS_SRUNOFF
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
!***********************************************************************
      srunoffdecl = 0

      IF ( Sroff_flag==smidx_module ) THEN
        MODNAME = 'srunoff_smidx'
      ELSE
        MODNAME = 'srunoff_carea'
      ENDIF
      CALL print_module(MODDESC, MODNAME, Version_srunoff)

      CALL declvar_dble(MODNAME, 'basin_imperv_evap', 'one', 1, &
     &     'Basin area-weighted average evaporation from impervious area', &
     &     'inches', Basin_imperv_evap)

      CALL declvar_dble(MODNAME, 'basin_imperv_stor', 'one', 1, &
     &     'Basin area-weighted average storage on impervious area', &
     &     'inches', Basin_imperv_stor)

      CALL declvar_dble(MODNAME, 'basin_infil', 'one', 1, &
     &     'Basin area-weighted average infiltration to the capillary reservoirs', &
     &     'inches', Basin_infil)

      CALL declvar_dble(MODNAME, 'basin_hortonian', 'one', 1, &
     &     'Basin area-weighted average Hortonian runoff', &
     &     'inches', Basin_hortonian)

      CALL declvar_dble(MODNAME, 'basin_contrib_fraction', 'one', 1, &
     &     'Basin area-weighted average contributing area of the pervious area of each HRU', &
     &     'decimal fraction', Basin_contrib_fraction)

      ALLOCATE ( Contrib_fraction(Nhru) )
      CALL declvar_real(MODNAME, 'contrib_fraction', 'nhru', Nhru, &
     &     'Contributing area of each HRU pervious area', &
     &     'decimal fraction', Contrib_fraction)

      ALLOCATE ( Hru_impervevap(Nhru) )
      CALL declvar_real(MODNAME, 'hru_impervevap', 'nhru', Nhru, &
     &     'HRU area-weighted average evaporation from impervious area for each HRU', &
     &     'inches', Hru_impervevap)

      ALLOCATE ( Imperv_evap(Nhru) )
      CALL declvar_real(MODNAME, 'imperv_evap', 'nhru', Nhru, &
     &     'Evaporation from impervious area for each HRU', &
     &     'inches', Imperv_evap)

      CALL declvar_dble(MODNAME, 'basin_sroffi', 'one', 1, &
     &     'Basin area-weighted average surface runoff from impervious areas', &
     &     'inches', Basin_sroffi)

      CALL declvar_dble(MODNAME, 'basin_sroffp', 'one', 1, &
     &     'Basin area-weighted average surface runoff from pervious areas', &
     &     'inches', Basin_sroffp)

      ALLOCATE ( Hru_sroffp(Nhru) )
      CALL declvar_real(MODNAME, 'hru_sroffp', 'nhru', Nhru, &
     &     'HRU area-weighted average surface runoff from pervious areas flowing out of each HRU', &
     &     'inches', Hru_sroffp)

      ALLOCATE ( Hru_sroffi(Nhru) )
      CALL declvar_real(MODNAME, 'hru_sroffi', 'nhru', Nhru, &
     &     'HRU area-weighted average surface runoff from impervious areas flowing out of each HRU', &
     &     'inches', Hru_sroffi)

     ! ALLOCATE ( Hru_sroffa(Nhru) )
     ! CALL declvar_real(MODNAME, 'hru_sroffa', 'nhru', Nhru, &
     !&     'HRU area-weighted average surface runoff from net application water for pervious area for each HRU', &
     !&     'inches', Hru_sroffa)

! Depression storage variables
      IF ( Dprst_flag==ACTIVE ) THEN
        CALL declvar_dble(MODNAME, 'basin_dprst_sroff', 'one', 1, &
     &       'Basin area-weighted average surface runoff from open surface-depression storage', &
     &       'inches', Basin_dprst_sroff)

        CALL declvar_dble(MODNAME, 'basin_dprst_evap', 'one', 1, &
     &       'Basin area-weighted average evaporation from surface-depression storage', &
     &       'inches', Basin_dprst_evap)

        CALL declvar_dble(MODNAME, 'basin_dprst_seep', 'one', 1, &
     &       'Basin area-weighted average seepage from surface-depression storage', &
     &       'inches', Basin_dprst_seep)

        CALL declvar_dble(MODNAME, 'basin_dprst_volop', 'one', 1, &
     &       'Basin area-weighted average storage volume in open surface depressions', &
     &       'inches', Basin_dprst_volop)

        CALL declvar_dble(MODNAME, 'basin_dprst_volcl', 'one', 1, &
     &       'Basin area-weighted average storage volume in closed surface depressions', &
     &       'inches', Basin_dprst_volcl)

        ALLOCATE ( Dprst_sroff_hru(Nhru) )
        CALL declvar_dble(MODNAME, 'dprst_sroff_hru', 'nhru', Nhru, &
     &       'Surface runoff from open surface-depression storage for each HRU', &
     &       'inches', Dprst_sroff_hru)

        ALLOCATE ( Dprst_insroff_hru(Nhru) )
        CALL declvar_real(MODNAME, 'dprst_insroff_hru', 'nhru', Nhru, &
     &       'Surface runoff from pervious and impervious portions into open and closed surface-depression storage for each HRU', &
     &       'inches', Dprst_insroff_hru)

        ALLOCATE ( Dprst_area_open(Nhru) )
        CALL declvar_real(MODNAME, 'dprst_area_open', 'nhru', Nhru, &
     &       'Surface area of open surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_open)

        ALLOCATE ( Dprst_area_clos(Nhru) )
        CALL declvar_real(MODNAME, 'dprst_area_clos', 'nhru', Nhru, &
     &       'Surface area of closed surface depressions based on storage volume for each HRU', &
     &       'acres', Dprst_area_clos)

!        ALLOCATE ( Upslope_dprst_hortonian(Nhru) )
!        CALL declvar_dble(MODNAME, 'upslope_dprst_hortonian', 'nhru', Nhru, &
!     &       'Upslope surface-depression spillage and interflow for each HRU',   &
!     &       'inches', Upslope_dprst_hortonian)

        ALLOCATE ( Dprst_seep_hru(Nhru) )
        CALL declvar_dble(MODNAME, 'dprst_seep_hru', 'nhru', Nhru, &
     &       'Seepage from surface-depression storage to associated GWR for each HRU', &
     &       'inches', Dprst_seep_hru)

        ALLOCATE ( Dprst_evap_hru(Nhru) )
        CALL declvar_real(MODNAME, 'dprst_evap_hru', 'nhru', Nhru, &
     &       'Evaporation from surface-depression storage for each HRU', &
     &       'inches', Dprst_evap_hru)

        ALLOCATE ( Dprst_vol_open_frac(Nhru) )
        CALL declvar_dble(MODNAME, 'dprst_vol_open_frac', 'nhru', Nhru, &
     &      'Fraction of open surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_open_frac)

        ALLOCATE ( Dprst_vol_clos_frac(Nhru) )
        CALL declvar_dble(MODNAME, 'dprst_vol_clos_frac', 'nhru', Nhru, &
     &      'Fraction of closed surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_clos_frac)

        ALLOCATE ( Dprst_vol_frac(Nhru) )
        CALL declvar_dble(MODNAME, 'dprst_vol_frac', 'nhru', Nhru, &
     &      'Fraction of surface-depression storage of the maximum storage for each HRU', &
     &      'decimal fraction', Dprst_vol_frac)

        ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru), Dprst_vol_thres_open(Nhru), Dprst_in(Nhru) )
      ENDIF

      ALLOCATE ( Hortonian_flow(Nhru) )
      CALL declvar_real(MODNAME, 'hortonian_flow', 'nhru', Nhru, &
     &     'Hortonian surface runoff reaching stream network for each HRU', &
     &     'inches', Hortonian_flow)

! cascading variables and parameters
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        ALLOCATE ( Upslope_hortonian(Nhru) )
        CALL declvar_dble(MODNAME, 'upslope_hortonian', 'nhru', Nhru, &
     &       'Hortonian surface runoff received from upslope HRUs', &
     &       'inches', Upslope_hortonian)

        CALL declvar_dble(MODNAME, 'basin_sroff_down', 'one', 1, &
     &       'Basin area-weighted average of cascading surface runoff', &
     &       'inches', Basin_sroff_down)

        CALL declvar_dble(MODNAME, 'basin_sroff_upslope', 'one', 1, &
     &       'Basin area-weighted average of cascading surface runoff received from upslope HRUs', &
     &       'inches', Basin_sroff_upslope)

        ALLOCATE ( Hru_hortn_cascflow(Nhru) )
        CALL declvar_dble(MODNAME, 'hru_hortn_cascflow', 'nhru', Nhru, &
     &       'Cascading Hortonian surface runoff leaving each HRU', &
     &       'inches', Hru_hortn_cascflow)

        IF ( Nlake>0 ) THEN
          CALL declvar_dble(MODNAME, 'basin_hortonian_lakes', 'one', 1, &
     &         'Basin area-weighted average Hortonian surface runoff to lakes', &
     &         'inches', Basin_hortonian_lakes)

          ALLOCATE ( Hortonian_lakes(Nhru) )
          CALL declvar_dble(MODNAME, 'hortonian_lakes', 'nhru', Nhru, &
     &         'Surface runoff to lakes for each HRU', &
     &         'inches', Hortonian_lakes)
        ENDIF
      ENDIF

! frozen ground variables and parameters
      ALLOCATE ( Frozen(Nhru) )
      IF ( Frozen_flag==ACTIVE ) THEN
        CALL declvar_int(MODNAME, 'frozen', 'nhru', Nhru, &
     &       'Flag for frozen ground (0=no; 1=yes)', &
     &       'none', Frozen)

        ALLOCATE ( Cfgi(Nhru) )
        CALL declvar_real(MODNAME, 'cfgi', 'nhru', Nhru, &
     &       'Continuous Frozen Ground Index', &
     &       'index', Cfgi)

        ALLOCATE ( Cfgi_prev(Nhru) )
        CALL declvar_real(MODNAME, 'cfgi_prev', 'nhru', Nhru, &
     &       'Continuous Frozen Ground Index from previous day', &
     &       'index', Cfgi_prev)

        IF ( declparam(MODNAME, 'cfgi_decay', 'one', 'real', &
     &       '0.97', '0.01', '1.0', &
     &       'CFGI daily decay of index; value of 1.0 is no decay', &
     &       'CFGI daily decay of index; value of 1.0 is no decay', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'cfgi_decay')

        IF ( declparam(MODNAME, 'cfgi_thrshld', 'one', 'real', &
     &       '52.55', '1.0', '500.0', &
     &       'CFGI threshold value indicating frozen soil', &
     &       'CFGI threshold value indicating frozen soil', &
     &       'index')/=0 ) CALL read_error(1, 'cfgi_thrshld')
      ENDIF

! Declare parameters
      IF ( Sroff_flag==smidx_module ) THEN
        ALLOCATE ( Smidx_coef(Nhru) )
        IF ( declparam(MODNAME, 'smidx_coef', 'nhru', 'real', &
     &       '0.005', '0.0', '1.0', &
     &       'Coefficient in contributing area computations', &
     &       'Coefficient in non-linear contributing area algorithm for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'smidx_coef')
        ALLOCATE ( Smidx_exp(Nhru) )
        IF ( declparam(MODNAME, 'smidx_exp', 'nhru', 'real', &
     &       '0.3', '0.0', '5.0', &
     &       'Exponent in contributing area computations', &
     &       'Exponent in non-linear contributing area algorithm for each HRU', &
     &       '1.0/inch')/=0 ) CALL read_error(1, 'smidx_exp')
      ENDIF

      IF ( Sroff_flag==carea_module ) THEN
        ALLOCATE ( Carea_min(Nhru), Carea_dif(Nhru) )
        IF ( declparam(MODNAME, 'carea_min', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Minimum contributing area', &
     &       'Minimum possible area contributing to surface runoff expressed as a portion of the area for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'carea_min')
      ENDIF

      ALLOCATE ( Carea_max(Nhru) )
      IF ( declparam(MODNAME, 'carea_max', 'nhru', 'real', &
     &     '0.6', '0.0', '1.0', &
     &     'Maximum contributing area', &
     &     'Maximum possible area contributing to surface runoff expressed as a portion of the HRU area', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'carea_max')

! Depression Storage parameters:
      IF ( Dprst_flag==ACTIVE ) THEN
        ALLOCATE ( Dprst_depth_avg(Nhru) )
        IF ( declparam(MODNAME, 'dprst_depth_avg', 'nhru', 'real', &
     &       '132.0', '0.0', '500.0', &
     &       'Average depth of surface depressions at maximum storage capacity', &
     &       'Average depth of surface depressions at maximum storage capacity', &
     &       'inches')/=0 ) CALL read_error(1, 'dprst_depth_avg')

        ALLOCATE ( Dprst_flow_coef(Nhru) )
        IF ( declparam(MODNAME, 'dprst_flow_coef', 'nhru', 'real', &
     &       '0.05', '0.00001', '0.5', &
     &       'Coefficient in linear flow routing equation for open surface depressions', &
     &       'Coefficient in linear flow routing equation for open surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_flow_coef')

        ALLOCATE ( Dprst_seep_rate_open(Nhru) )
        IF ( declparam(MODNAME, 'dprst_seep_rate_open', 'nhru', 'real', &
     &       '0.02', '0.0', '0.2', &
     &       'Coefficient used in linear seepage flow equation for open surface depressions', &
     &       'Coefficient used in linear seepage flow equation for'// &
     &       ' open surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_open')

        ALLOCATE ( Dprst_seep_rate_clos(Nhru) )
        IF ( declparam(MODNAME, 'dprst_seep_rate_clos', 'nhru', 'real', &
     &       '0.02', '0.0', '0.2', &
     &       'Coefficient used in linear seepage flow equation for closed surface depressions', &
     &       'Coefficient used in linear seepage flow equation for'// &
     &       ' closed surface depressions for each HRU', &
     &       'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_clos')

        ALLOCATE ( Op_flow_thres(Nhru) )
        IF ( declparam(MODNAME, 'op_flow_thres', 'nhru', 'real', &
     &       '1.0', '0.01', '1.0', &
     &       'Fraction of open depression storage above which surface runoff occurs for each timestep', &
     &       'Fraction of open depression storage above'// &
     &       ' which surface runoff occurs; any water above'// &
     &       ' maximum open storage capacity spills as surface runoff', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'op_flow_thres')

        ALLOCATE ( Sro_to_dprst_perv(Nhru) )
        IF ( PRMS4_flag==ACTIVE ) THEN
          IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real', &
     &         '0.2', '0.0', '1.0', &
     &         'Fraction of pervious surface runoff that flows into surface-depression storage', &
     &         'Fraction of pervious surface runoff that'// &
     &         ' flows into surface-depression storage; the remainder'// &
     &         ' flows to the stream network for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst')
        ENDIF
        IF ( PRMS4_flag==OFF ) THEN
          IF ( declparam(MODNAME, 'sro_to_dprst_perv', 'nhru', 'real', &
     &         '0.2', '0.0', '1.0', &
     &         'Fraction of pervious surface runoff that flows into surface-depression storage', &
     &         'Fraction of pervious surface runoff that'// &
     &         ' flows into surface-depression storage; the remainder'// &
     &         ' flows to the stream network for each HRU', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_perv')
        ENDIF

        ALLOCATE ( Sro_to_dprst_imperv(Nhru) )
        IF ( declparam(MODNAME, 'sro_to_dprst_imperv', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of impervious surface runoff that flows into surface-depression storage', &
     &       'Fraction of impervious surface runoff that'// &
     &       ' flows into surface-depression storage; the remainder'// &
     &       ' flows to the stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_imperv')

        ALLOCATE ( Dprst_et_coef(Nhru) )
        IF ( declparam(MODNAME, 'dprst_et_coef', 'nhru', 'real', &
     &       '1.0', '0.5', '1.5', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_et_coef')

        IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
          ALLOCATE ( Dprst_frac_init(Nhru) )
          IF ( declparam(MODNAME, 'dprst_frac_init', 'nhru', 'real', &
     &         '0.5', '0.0', '1.0', &
     &         'Fraction of maximum storage that contains water at the start of a simulation', &
     &         'Fraction of maximum surface-depression storage that'// &
     &         ' contains water at the start of a simulation', &
     &         'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_init')
        ENDIF

        ALLOCATE ( Va_open_exp(Nhru) )
        IF ( declparam(MODNAME, 'va_open_exp', 'nhru', 'real', &
     &       '0.001', '0.0001', '10.0', &
     &       'Coefficient in the exponential equation to compute'// &
     &       ' current surface area of open surface-depression storage', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that open'// &
     &       ' depressions are full to compute current surface area for each HRU;'// &
     &       ' 0.001 is an approximate cylinder; 1.0 is a cone', &
     &       'none')/=0 ) CALL read_error(1, 'va_open_exp')

        ALLOCATE ( Va_clos_exp(Nhru) )
        IF ( declparam(MODNAME, 'va_clos_exp', 'nhru', 'real', &
     &       '0.001', '0.0001', '10.0', &
     &       'Coefficient in the exponential equation to compute'// &
     &       ' current surface area of closed surface-depression storage', &
     &       'Coefficient in the exponential equation relating'// &
     &       ' maximum surface area to the fraction that closed'// &
     &       ' depressions are full to compute current surface area for each HRU;'// &
     &       ' 0.001 is an approximate cylinder; 1.0 is a cone', &
     &       'none')/=0 ) CALL read_error(1, 'va_clos_exp')
      ENDIF

      ALLOCATE ( Infil_ag(Nhru) )
      IF ( AG_flag==ACTIVE ) THEN
        CALL declvar_dble(MODNAME, 'basin_infil_ag', 'one', 1, &
     &       'Basin area-weighted average infiltration to the agriculture capillary reservoirs', &
     &       'inches', Basin_infil_ag)

        CALL declvar_real(MODNAME, 'infil_ag', 'nhru', Nhru, &
     &       'Infiltration to the agriculture reservoirs for each HRU', &
     &       'inches', Infil_ag)
        CALL declvar_dble(MODNAME, 'basin_ag_contrib_fraction', 'one', 1, &
     &       'Basin area-weighted average contributing area of the agriculture area of each HRU', &
     &       'decimal fraction', basin_ag_contrib_fraction)
        ALLOCATE ( ag_contrib_fraction(Nhru) )
        CALL declvar_real(MODNAME, 'ag_contrib_fraction', 'nhru', Nhru, &
     &       'Contributing area of each HRU agriculture area', &
     &       'decimal fraction', ag_contrib_fraction)
!       ALLOCATE ( Sro_to_dprst_ag(Nhru) )
!       IF ( declparam(MODNAME, 'sro_to_dprst_ag', 'nhru', 'real', &
!     &      '0.2', '0.0', '1.0', &
!     &      'Fraction of agricultural surface runoff that flows into surface-depression storage', &
!     &      'Fraction of agricultural surface runoff that flows into'// &
!     &      ' surface-depression storage; the remainder flows to a stream network for each HRU', &
!     &      'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_ag')
      ENDIF

      END FUNCTION srunoffdecl

!***********************************************************************
!     srunoffinit - Initialize srunoff module - get parameter values
!***********************************************************************
      INTEGER FUNCTION srunoffinit()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, smidx_module, carea_module, CASCADE_OFF
      use PRMS_READ_PARAM_FILE, only: getparam_real
      USE PRMS_MODULE, ONLY: Nhru, Nlake, Init_vars_from_file, &
     &    Dprst_flag, Cascade_flag, Sroff_flag, Frozen_flag, Parameter_check_flag, AG_flag
      USE PRMS_SRUNOFF
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order
      use prms_utils, only: read_error
      USE PRMS_FLOWVARS, ONLY: Hru_impervstor, Soil_moist_max
      IMPLICIT NONE
! Functions
      EXTERNAL :: dprst_init
! Local Variables
      INTEGER :: i, j, k, num_hrus
      REAL :: frac
!***********************************************************************
      srunoffinit = 0

      Imperv_evap = 0.0
      Hortonian_flow = 0.0
      Hru_sroffi = 0.0
      Hru_sroffp = 0.0
      Hru_impervevap = 0.0
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Hru_hortn_cascflow = 0.0D0
        IF ( Nlake>0 ) Hortonian_lakes = 0.0D0
      ENDIF

      Basin_sroffi = 0.0D0
      Basin_sroffp = 0.0D0
      Basin_imperv_evap = 0.0D0
      Basin_dprst_sroff = 0.0D0
      Basin_dprst_evap = 0.0D0
      Basin_dprst_seep = 0.0D0
      Basin_sroff_upslope = 0.0D0
      Basin_sroff_down = 0.0D0
      Basin_hortonian_lakes = 0.0D0
      Basin_contrib_fraction = 0.0D0
      IF ( Init_vars_from_file==OFF ) THEN
        Basin_imperv_stor = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Hru_impervstor = 0.0
        Frozen = OFF
        IF ( Frozen_flag==ACTIVE ) THEN
          Cfgi = 0.0
          Cfgi_prev = 0.0
        ENDIF
      ENDIF
      Sroff_ag = 0.0

      IF ( getparam_real(MODNAME, 'carea_max', Nhru, Carea_max)/=0 ) CALL read_error(2, 'carea_max')

      IF ( Sroff_flag==smidx_module ) THEN
! Smidx parameters
        IF ( getparam_real(MODNAME, 'smidx_coef', Nhru, Smidx_coef)/=0 ) CALL read_error(2, 'smidx_coef')
        IF ( getparam_real(MODNAME, 'smidx_exp', Nhru, Smidx_exp)/=0 ) CALL read_error(2, 'smidx_exp')
        IF ( Parameter_check_flag>0 ) THEN
          num_hrus = 0
          DO i = 1, Nhru
            frac = Smidx_coef(i)*10**(Soil_moist_max(i)*Smidx_exp(i))
            k = 0
            IF ( frac>2.0 ) k = 1
            IF ( frac>Carea_max(i)*2.0 ) k = k + 2
            IF ( k>0 ) THEN
              num_hrus = num_hrus + 1
              WRITE (*, '(/,A,F10.4)') 'WARNING, Contributing area based on smidx parameters and soil_moist_max:', frac
              IF ( k==1 .OR. k==3 ) PRINT *, 'Maximum contributing area > 200%'
              IF ( k>1 ) PRINT *, 'Maximum contributing area > carea_max:', Carea_max(i)
              PRINT *, 'HRU:', i, '; soil_moist_max:', Soil_moist_max(i)
              PRINT *, 'smidx_coef:', Smidx_coef(i), '; smidx_exp:', Smidx_exp(i)
            ENDIF
          ENDDO
          IF ( num_hrus>0 ) THEN
            WRITE (*, '(/,A,/,9X,A,/,9X,A,I7,/,9X,A,/,9X,A,/)') &
     &             'WARNING, maximum contributing area based on smidx coefficents and', &
     &             'soil_moist_max are > 200% of the HRU area and/or > 2*carea_max', &
     &             'number of HRUs for which this condition exists:', num_hrus, &
     &             'This means the smidx parameters are insensitive and', &
     &             'carea_max very sensitive for those HRUs'
          ENDIF
        ENDIF

      ELSE !IF ( Sroff_flag==carea_module ) THEN
! Carea parameters
        IF ( getparam_real(MODNAME, 'carea_min', Nhru, Carea_min)/=0 ) CALL read_error(2, 'carea_min')
        Carea_dif = 0.0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          Carea_dif(i) = Carea_max(i) - Carea_min(i)
        ENDDO
      ENDIF

! Depression Storage parameters and variables:
      IF ( Dprst_flag==ACTIVE ) CALL dprst_init()

! Frozen soil parameters
      IF ( Frozen_flag==ACTIVE ) THEN
        IF ( getparam_real(MODNAME, 'cfgi_thrshld', 1, Cfgi_thrshld)/=0 ) CALL read_error(2, 'cfgi_thrshld')
        IF ( getparam_real(MODNAME, 'cfgi_decay', 1, Cfgi_decay)/=0 ) CALL read_error(2, 'cfgi_decay')
      ENDIF

! Agriculture variables
      IF ( AG_flag==ACTIVE ) ag_contrib_fraction = 0.0
      basin_ag_contrib_fraction = 0.0D0
      Infil_ag = 0.0 ! always allocated as passed to subroutine
      Basin_infil_ag = 0.0D0

      END FUNCTION srunoffinit

!***********************************************************************
!     srunoffrun - Computes surface runoff using contributing area
!                  computations using antecedent soil moisture.
!***********************************************************************
      INTEGER FUNCTION srunoffrun()
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ACTIVE, OFF, LAND, LAKE, GLACIER, CASCADE_OFF, SWALE, ZERO_SNOWPACK, CLOSEZERO
      USE PRMS_MODULE, ONLY: Dprst_flag, Cascade_flag, Call_cascade, Frozen_flag, Glacier_flag, &
     &    PRMS_land_iteration_flag, Kkiter, AG_flag, Hru_type, Ag_Package
      USE PRMS_SRUNOFF
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, &
     &    Hru_perv, Hru_imperv, Hru_frac_imperv, Hru_frac_perv, &
     &    Dprst_area_max, Hru_area, Basin_area_inv, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Hru_area_dble, Imperv_flag, Ag_area
      USE PRMS_CLIMATEVARS, ONLY: Potet, Tavgc
      USE PRMS_FLOWVARS, ONLY: Sroff, Infil, Imperv_stor, Pkwater_equiv, Dprst_vol_open, Dprst_vol_clos, &
     &    Imperv_stor_max, Snowinfil_max, Basin_sroff, Glacier_frac, Hru_impervstor, Dprst_stor_hru, &
     &    Dprst_total_open_in, Dprst_total_open_out, Dprst_total_clos_in, Dprst_total_clos_out, &
     &    Soil_moist, Soil_rechr, Strm_seg_in, &
     &    Ag_soil_moist, Ag_soil_rechr, Pk_depth, Snowcov_area, Snow_evap, Snowmelt, Pptmix_nopack
      USE PRMS_IT0_VARS, ONLY: It0_dprst_vol_open, It0_dprst_vol_clos, It0_imperv_stor, It0_soil_moist, &
                               It0_soil_rechr, It0_ag_soil_moist, It0_ag_soil_rechr, It0_hru_impervstor
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Net_rain, Net_snow, Net_ppt, Hru_intcpevap, Net_apply
      USE PRMS_SNOW, ONLY: Glacrb_melt, Pk_precip
      IMPLICIT NONE
! Functions
      INTRINSIC :: SNGL, DBLE
      EXTERNAL :: imperv_et, compute_infil, run_cascade_sroff, dprst_comp, compute_infil_ag_glcr
! Local Variables
      INTEGER :: i, k, dprst_chk, frzen, active_glacier, lateral_flow_flag, perv_on, ag_on
      REAL :: srunoff, avail_et, perv_area, availh2o_total, waterin, runoff, glacier_free
      DOUBLE PRECISION :: hru_sroff_down, cfgi_sroff, upslope
      REAL :: cfgi_k, depth_cm !frozen ground
      REAL :: glcrmltb, temp ! glaciers
!***********************************************************************
      srunoffrun = 0

! It0 variables used with MODFLOW integration and PRMS iteration to save states.
      IF ( Kkiter>1 ) THEN
        IF ( PRMS_land_iteration_flag>0 .OR. Ag_Package==ACTIVE ) THEN
          IF ( PRMS_land_iteration_flag>0 ) THEN
            Imperv_stor = It0_imperv_stor
            Soil_moist = It0_soil_moist
            Soil_rechr = It0_soil_rechr
          ENDIF
          IF ( Dprst_flag==ACTIVE ) THEN
            Dprst_vol_open = It0_dprst_vol_open
            Dprst_vol_clos = It0_dprst_vol_clos
          ENDIF
          IF ( AG_flag==ACTIVE ) THEN
            Ag_soil_moist = It0_ag_soil_moist
            Ag_soil_rechr = It0_ag_soil_rechr
          ENDIF
          Hru_impervstor = It0_hru_impervstor
        ENDIF
      ENDIF

      Basin_sroffp = 0.0D0
      Basin_sroff = 0.0D0
      Basin_infil = 0.0D0
      Basin_hortonian = 0.0D0
      Basin_contrib_fraction = 0.0D0
      Basin_cfgi_sroff = 0.0D0
      Basin_apply_sroff = 0.0D0

      IF ( Call_cascade==ACTIVE ) Strm_seg_in = 0.0D0
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Basin_sroff_down = 0.0D0
        Basin_sroff_upslope = 0.0D0
        Basin_hortonian_lakes = 0.0D0
        Upslope_hortonian = 0.0D0
      ENDIF

      IF ( Dprst_flag==ACTIVE ) THEN
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
        Dprst_seep_hru = 0.0D0 ! initialize HRU variables in case of dynamic parameters, assume won't turn all dprst off
        Dprst_sroff_hru = 0.0D0
        Dprst_evap_hru = 0.0
        Dprst_in = 0.0D0
        Dprst_stor_hru = 0.0D0
        Dprst_insroff_hru = 0.0
        Dprst_total_open_in = 0.0D0
        Dprst_total_open_out = 0.0D0
        Dprst_total_clos_in = 0.0D0
        Dprst_total_clos_out = 0.0D0
      ENDIF

      ! initialize HRU variables in case of dynamic parameters
      !Hru_sroffa = 0.0
      IF ( Imperv_flag == ACTIVE ) THEN
        Basin_sroffi = 0.0D0
        Basin_imperv_evap = 0.0D0
        Basin_imperv_stor = 0.0D0
        Hru_sroffi = 0.0
        Imperv_evap = 0.0
        Hru_impervevap = 0.0
      ENDIF
      Contrib_fraction = 0.0
      Hru_sroffp = 0.0

      dprst_chk = 0
      Infil = 0.0
      ag_on = OFF
      perv_on = ACTIVE
      IF ( AG_flag==ACTIVE ) THEN
        Infil_ag = 0.0
        Basin_infil_ag = 0.0D0
        basin_ag_contrib_fraction = 0.0D0
        ag_contrib_fraction = 0.0
      ENDIF

      DO k = 1, Active_hrus
        i = Hru_route_order(k)

        Hruarea = Hru_area(i)
        Hruarea_dble = Hru_area_dble(i)
        Imperv_frac = Hru_frac_imperv(i)
        upslope = 0.0D0
        IF ( Cascade_flag>CASCADE_OFF ) upslope = Upslope_hortonian(i)
        Ihru = i
        runoff = 0.0
        glcrmltb = 0.0 ! glacier
        active_glacier = OFF ! not a glacier or unglaciated glacier HRU
        glacier_free = 1.0
        IF ( Glacier_flag==ACTIVE ) THEN ! should frozen ground be triggered if glacier_frac > 0.75 (maybe a parameter)?
          IF ( Hru_type(i)==GLACIER ) THEN
            glcrmltb = Glacrb_melt(i)
            IF ( Glacier_frac(i)>0.0 ) active_glacier = ACTIVE
            glacier_free = 1.0 - Glacier_frac(i)
          ENDIF
        ENDIF

        IF ( Hru_type(i)==LAKE ) THEN
! HRU is a lake
!     eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          Hortonian_lakes(i) = upslope
          Basin_hortonian_lakes = Basin_hortonian_lakes + upslope*Hruarea_dble
          CYCLE
        ENDIF

        perv_area = Hru_perv(i)
        Perv_frac = Hru_frac_perv(i)
        IF ( AG_flag==ACTIVE ) THEN
          ag_on = OFF
          IF ( Ag_area(i)>0.0 ) ag_on = ACTIVE
          perv_on = OFF
          IF ( perv_area>0.0 ) perv_on = ACTIVE
          Sroff_ag = 0.0
        ENDIF
        Srp = 0.0
        Sri = 0.0
        Sra = 0.0
        lateral_flow_flag = ACTIVE
        IF ( Hru_type(i)==SWALE ) lateral_flow_flag = OFF
        Hruarea_imperv = Hru_imperv(i)

        avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i)

        waterin = SNGL( upslope ) + glcrmltb + Net_apply(i) !; net_apply = 0 if no application

        IF ( Pptmix_nopack(i) == ACTIVE ) waterin = waterin + Net_rain(i)

!******If precipitation on snowpack all water available to the surface is considered to be snowmelt
!******If there is no snowpack and no precip,then check for melt from last of snowpack.
!******If rain/snow mix with no antecedent snowpack, compute snowmelt portion of runoff.

        IF ( Snowmelt(i)>0.0 ) THEN
          waterin = waterin + Snowmelt(i)

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.
        ELSEIF ( Pkwater_equiv(i)<ZERO_SNOWPACK ) THEN

!      If no snowmelt and no snowpack but there was net snow then
!      snowpack was small and was lost to sublimation.
          IF ( .not.(Net_snow(i)>0.0) .AND. Net_rain(i)>0.0 ) THEN
            IF ( .not.(Pk_precip(i)>0.0) ) waterin = waterin + Net_rain(i)
          ENDIF
        ENDIF
        availh2o_total = waterin * glacier_free ! new water for glacier_free area

        frzen = OFF
        IF ( Frozen_flag==ACTIVE ) THEN
          IF ( Tavgc(i)>0.0 ) THEN
            cfgi_k = 0.5
          ELSE
            cfgi_k = 0.08
          ENDIF
          depth_cm = SNGL(Pk_depth(i))*2.54 !depth of snow cover averaged over HRU
          Cfgi(i) = Cfgi_decay*Cfgi_prev(i) - Tavgc(i)*( 2.71828**(-0.4*cfgi_k*depth_cm) )
          IF ( active_glacier==1 ) THEN
            Cfgi(i) = 0.0 !if glacier over, want ground completely unfrozen, or below threshold, infiltration
            IF ( Glacier_frac(i)<1.0 ) Cfgi(i) = Cfgi_thrshld ! glacier with some open fraction
          ENDIF
          IF ( Cfgi(i)<0.0 ) Cfgi(i) = 0.0
          Cfgi_prev(i) = Cfgi(i)
          IF ( Cfgi(i)>=Cfgi_thrshld ) THEN
            frzen = 1
            IF ( lateral_flow_flag == ACTIVE ) THEN ! if swale, add water to storage of impervious and dprst and infiltraion for pervious
              cfgi_sroff = DBLE( availh2o_total ) * Hruarea_dble ! all water runs off, but still compute components below
              Basin_cfgi_sroff = Basin_cfgi_sroff + cfgi_sroff
            ENDIF
          ENDIF
          Frozen(i) = frzen
        ENDIF

!******Impervious area computations
        IF ( Hruarea_imperv > 0.0 ) THEN
          IF ( frzen == OFF ) THEN
            Imperv_stor(i) = Imperv_stor(i) + availh2o_total
            IF ( lateral_flow_flag == ACTIVE ) THEN
              IF ( Imperv_stor(i) > Imperv_stor_max(i) ) THEN
                Sri = Imperv_stor(i) - Imperv_stor_max(i)
                Imperv_stor(i) = Imperv_stor_max(i)
              ENDIF
            ENDIF
          ELSE
            IF ( lateral_flow_flag == ACTIVE ) THEN
              Sri = availh2o_total
            ELSE
              Imperv_stor(i) = Imperv_stor(i) + availh2o_total
              ! for swales water added to storage
            ENDIF
          ENDIF
        ENDIF

!******Compute runoff for pervious, agriculture, and depression storage area, only if not frozen ground
        IF ( frzen==OFF ) THEN
          IF ( active_glacier==OFF .AND. ag_on==OFF ) THEN
            CALL compute_infil(Net_rain(i), Net_ppt(i), Snowmelt(i), Pk_precip(i), Net_apply(i), &
     &                         Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), lateral_flow_flag)
          ELSE ! glacier and/or with AG
            temp = Snowmelt(i) + glcrmltb !Snowmelt or 0.0
            CALL compute_infil_ag_glcr(Net_rain(i), Net_ppt(i), temp, Pk_precip(i), Net_apply(i), &
     &                         Snowinfil_max(i), Net_snow(i), Pkwater_equiv(i), Infil(i), lateral_flow_flag, &
     &                         glacier_free, perv_on, ag_on, Infil_ag(i))
          ENDIF
        ELSE ! frozen ground
          IF ( lateral_flow_flag == ACTIVE ) THEN
            IF ( perv_on==ACTIVE ) Srp = availh2o_total
            IF ( ag_on==ACTIVE ) Sroff_ag = availh2o_total + Infil_ag(i)
          ELSE
            IF ( perv_on==ACTIVE ) Infil(i) = availh2o_total
            IF ( ag_on==ACTIVE ) Infil_ag(i) = availh2o_total
          ENDIF
        ENDIF
        ! ** ADD in water from irrigation application and water-use transfer for pervious portion - sra (if any)
        IF ( Sra > 0.0 ) THEN
          Srp = Srp + Sra
          Basin_apply_sroff = Basin_apply_sroff + DBLE( Sra*perv_area )
        ENDIF

!******Compute runoff for depression storage area
        IF ( Dprst_flag==ACTIVE ) THEN
          dprst_chk = OFF
          IF ( Dprst_area_max(i)>0.0 ) THEN
            dprst_chk = ACTIVE
!           ******Compute the depression storage component
!           only call if total depression surface area for each HRU is > 0.0
            CALL dprst_comp(Dprst_vol_clos(i), Dprst_area_clos_max(i), Dprst_area_clos(i), &
     &                      Dprst_vol_open_max(i), Dprst_vol_open(i), Dprst_area_open_max(i), Dprst_area_open(i), &
     &                      Dprst_sroff_hru(i), Dprst_seep_hru(i), Sro_to_dprst_perv(i), Sro_to_dprst_imperv(i), &
     &                      Dprst_evap_hru(i), avail_et, availh2o_total, Dprst_in(i), frzen, lateral_flow_flag)
            runoff = runoff + SNGL( Dprst_sroff_hru(i)*Hruarea_dble )
          ENDIF
        ENDIF

!       **********************************************************

        srunoff = 0.0
        IF ( lateral_flow_flag==ACTIVE .AND. active_glacier==OFF ) THEN ! could be a glacier-capable HRU with no ice
          ! Srp, Sri, and Sroff_ag can be reduced in dprst_comp, so compute here
          ! Sroff_ag, Sri and Srp = 0 for swales
          runoff = runoff + Srp*perv_area + Sri*Hruarea_imperv
          IF ( ag_on == ACTIVE ) THEN
    !        apply_sroff = DBLE( Sroff_ag*Ag_area(i) ) ! may want apply_sroff be a declared variable
            Basin_apply_sroff = Basin_apply_sroff + DBLE( Sroff_ag*Ag_area(i) )
            runoff = runoff + Sroff_ag*Ag_area(i)
          ENDIF
          srunoff = runoff / Hruarea

!******Compute HRU weighted average (to units of inches/dt)
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            hru_sroff_down = 0.0D0
            IF ( srunoff>CLOSEZERO ) THEN
              IF ( Ncascade_hru(i)>0 ) CALL run_cascade_sroff(Ncascade_hru(i), srunoff, hru_sroff_down)
              Hru_hortn_cascflow(i) = hru_sroff_down
              !IF ( Hru_hortn_cascflow(i)<0.0D0 ) Hru_hortn_cascflow(i) = 0.0D0
              !IF ( Upslope_hortonian(i)<0.0D0 ) Upslope_hortonian(i) = 0.0D0
              Basin_sroff_upslope = Basin_sroff_upslope + Upslope_hortonian(i)*Hruarea_dble
              Basin_sroff_down = Basin_sroff_down + hru_sroff_down*Hruarea_dble
            ELSE
              Hru_hortn_cascflow(i) = 0.0D0
            ENDIF
          ENDIF
        ENDIF
        Hru_sroffp(i) = Srp*Perv_frac
        Basin_sroffp = Basin_sroffp + DBLE( Srp*Perv_area )

        Basin_infil = Basin_infil + DBLE( Infil(i)*perv_area )
        Basin_contrib_fraction = Basin_contrib_fraction + DBLE( Contrib_fraction(i)*perv_area )
        IF ( ag_on == ACTIVE ) THEN
          Basin_infil_ag = Basin_infil_ag + DBLE( Infil_ag(i)*Ag_area(i) )
          basin_ag_contrib_fraction = basin_ag_contrib_fraction + DBLE( ag_contrib_fraction(i)*Ag_area(i) )
        ENDIF

!******Compute evaporation from impervious area, even if frozen ground
        IF ( Hruarea_imperv>0.0 ) THEN
          IF ( Imperv_stor(i)>0.0 ) THEN
            CALL imperv_et(Imperv_stor(i), Potet(i), Imperv_evap(i), Snowcov_area(i), avail_et)
            Hru_impervevap(i) = Imperv_evap(i)*Imperv_frac
            !IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
            avail_et = avail_et - Hru_impervevap(i)
            IF ( avail_et<0.0 ) THEN
               ! sanity check
!              IF ( avail_et<-NEARZERO ) PRINT*, 'avail_et<0 in srunoff imperv', i, Nowmonth, Nowday, avail_et
              Hru_impervevap(i) = Hru_impervevap(i) + avail_et
              IF ( Hru_impervevap(i)<0.0 ) Hru_impervevap(i) = 0.0
              Imperv_evap(i) = Hru_impervevap(i)/Imperv_frac
              Imperv_stor(i) = Imperv_stor(i) - avail_et/Imperv_frac
              avail_et = 0.0
            ENDIF
            Basin_imperv_evap = Basin_imperv_evap + DBLE( Hru_impervevap(i)*Hruarea )
            Hru_impervstor(i) = Imperv_stor(i)*Imperv_frac
            Basin_imperv_stor = Basin_imperv_stor + DBLE(Imperv_stor(i)*Hruarea_imperv )
          ENDIF
          Hru_sroffi(i) = Sri*Imperv_frac
          Basin_sroffi = Basin_sroffi + DBLE( Sri*Hruarea_imperv )
        ENDIF

        IF ( dprst_chk==ACTIVE ) Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hruarea_dble

        Sroff(i) = srunoff
        Hortonian_flow(i) = srunoff
        Basin_hortonian = Basin_hortonian + DBLE( srunoff*Hruarea )
        Basin_sroff = Basin_sroff + DBLE( srunoff*Hruarea )
      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      !rsr, should be land_area???
      Basin_sroff = Basin_sroff*Basin_area_inv
      IF ( Imperv_flag == ACTIVE ) THEN
        Basin_imperv_evap = Basin_imperv_evap*Basin_area_inv
        Basin_imperv_stor = Basin_imperv_stor*Basin_area_inv
        Basin_sroffi = Basin_sroffi*Basin_area_inv
      ENDIF
      Basin_infil = Basin_infil*Basin_area_inv
      ! doesn't include CFGI runoff
      Basin_sroffp = Basin_sroffp*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv
      Basin_contrib_fraction = Basin_contrib_fraction*Basin_area_inv
      IF ( AG_flag==ACTIVE ) THEN
        Basin_infil_ag = Basin_infil_ag*Basin_area_inv
        basin_ag_contrib_fraction = basin_ag_contrib_fraction*Basin_area_inv
      ENDIF
      Basin_apply_sroff = Basin_apply_sroff*Basin_area_inv
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      ENDIF

      IF ( Dprst_flag==ACTIVE ) THEN
        Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
        Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
        Basin_dprst_evap = Basin_dprst_evap*Basin_area_inv
        Basin_dprst_seep = Basin_dprst_seep*Basin_area_inv
        Basin_dprst_sroff = Basin_dprst_sroff*Basin_area_inv
      ENDIF

      END FUNCTION srunoffrun

!***********************************************************************
!      Subroutine to compute evaporation from impervious area at
!      potential ET rate up to available ET
!***********************************************************************
      SUBROUTINE imperv_et(Imperv_stor, Potet, Imperv_evap, Sca, Avail_et)
      USE PRMS_SRUNOFF, ONLY: Imperv_frac
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Potet, Sca, Avail_et
      REAL, INTENT(INOUT) :: Imperv_stor, Imperv_evap
!***********************************************************************
      IF ( Sca<1.0 ) THEN
        IF ( Potet<Imperv_stor ) THEN
          Imperv_evap = Potet*(1.0-Sca)
        ELSE
          Imperv_evap = Imperv_stor*(1.0-Sca)
        ENDIF
        IF ( Imperv_evap*Imperv_frac>Avail_et ) Imperv_evap = Avail_et/Imperv_frac
        Imperv_stor = Imperv_stor - Imperv_evap
      ENDIF
      !rsr, sanity check
!      IF ( Imperv_stor<0.0 ) THEN
!        PRINT *, 'imperv_stor<0', Imperv_stor
!        Imperv_stor = 0.0
!      ENDIF

      END SUBROUTINE imperv_et

!***********************************************************************
!     Compute infiltration
!***********************************************************************
      SUBROUTINE compute_infil(Net_rain, Net_ppt, Snowmelt, Pk_precip, Net_apply, &
     &                         Snowinfil_max, Net_snow, Pkwater_equiv, Infil, hru_flag)
      USE PRMS_CONSTANTS, ONLY: ACTIVE, CASCADE_OFF
      USE PRMS_MODULE, ONLY: Cascade_flag
      USE PRMS_SRUNOFF, ONLY: Upslope_hortonian, Ihru, Srp, Sra
      USE PRMS_FLOWVARS, ONLY: Pptmix_nopack
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: hru_flag ! LAND (later flow enabled, i.e., not a swale)
      REAL, INTENT(IN) :: Net_rain, Net_ppt, Pk_precip, Net_apply
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Infil
! Functions
      INTRINSIC :: SNGL
      EXTERNAL :: perv_comp, check_capacity
! Local Variables
      REAL :: avail_water
!***********************************************************************
! irrigation application just like infiltration
      IF ( Net_apply>0.0 ) THEN
        Infil = Net_apply
        IF ( hru_flag==1 ) CALL perv_comp(Net_apply, Net_apply, Infil, Sra)
      ENDIF

! compute runoff from cascading Hortonian flow
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        avail_water = SNGL( Upslope_hortonian(Ihru) )
        IF ( avail_water>0.0 ) THEN
          Infil = Infil + avail_water
          IF ( hru_flag==1 ) CALL perv_comp(avail_water, avail_water, Infil, Srp)
        ENDIF
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack(Ihru)==ACTIVE ) THEN
        Infil = Infil + Net_rain
        IF ( hru_flag==1 ) CALL perv_comp(Net_rain, Net_rain, Infil, Srp)
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN ! includes glacier melt, if any
        Infil = Infil + Snowmelt
        IF ( hru_flag==1 ) THEN
          IF ( Pkwater_equiv>0.0D0 .OR. .not.(Net_ppt-Net_snow>0.0) ) THEN
!******Pervious area computations
            CALL check_capacity(Snowinfil_max, Infil)
!******Snowmelt occurred and depleted the snowpack
          ELSE
            CALL perv_comp(Snowmelt, Net_ppt, Infil, Srp)
          ENDIF
        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( .not.(Pkwater_equiv>0.0D0) ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( .not.(Net_snow>0.0) .AND. Net_rain>0.0 .AND. .not.(Pk_precip>0.0) ) THEN
! no snow, some rain
          Infil = Infil + Net_rain
          IF ( hru_flag==1 ) CALL perv_comp(Net_rain, Net_rain, Infil, Srp)
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.

      ELSEIF ( Infil>0.0 ) THEN
        IF ( hru_flag==1 ) CALL check_capacity(Snowinfil_max, Infil)
      ENDIF

      END SUBROUTINE compute_infil

!***********************************************************************
!     Compute infiltration with active glacier and/or agriculture
!***********************************************************************
      SUBROUTINE compute_infil_ag_glcr(Net_rain, Net_ppt, Snowmelt, Pk_precip, Net_apply, &
     &                                 Snowinfil_max, Net_snow, Pkwater_equiv, Infil, hru_flag, glacier_free, &
     &                                 Perv_on, Ag_on, Infil_ag)
      USE PRMS_CONSTANTS, ONLY: LAND, ACTIVE, CASCADE_OFF
      USE PRMS_MODULE, ONLY: Cascade_flag
      USE PRMS_SRUNOFF, ONLY: Upslope_hortonian, Ihru, Srp, Sra, Sroff_ag
      USE PRMS_FLOWVARS, ONLY: Pptmix_nopack
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: hru_flag ! LAND or GLACIER HRU (lateral flow enabled)
      INTEGER, INTENT(IN) :: Perv_on, Ag_on
      REAL, INTENT(IN) :: Net_rain, Net_ppt, Pk_precip, Net_apply
      REAL, INTENT(IN) :: Snowmelt, Snowinfil_max, Net_snow, glacier_free
      DOUBLE PRECISION, INTENT(IN) :: Pkwater_equiv
      REAL, INTENT(INOUT) :: Infil, Infil_ag
! Functions
      INTRINSIC :: SNGL
      EXTERNAL :: perv_comp, check_capacity, ag_comp, check_capacity_ag
! Local Variables
      REAL :: avail_water, avail_net_ppt
!***********************************************************************
! irrigation application for pervious and agriculture areas (just like infiltration)
      IF ( Net_apply>0.0 ) THEN
        avail_water = Net_apply * glacier_free
        IF ( Perv_on==ACTIVE ) Infil = avail_water
        IF ( Ag_on==ACTIVE ) Infil_ag = avail_water
        IF ( hru_flag==1 ) THEN
          IF ( Perv_on==ACTIVE ) CALL perv_comp(avail_water, avail_water, Infil, Sra)
          IF ( Ag_on==ACTIVE ) CALL ag_comp(avail_water, avail_water, Infil_ag, Sroff_ag)
!          apply_sroff = Sra + Sroff_ag ! may want apply_sroff be a declared variable
        ENDIF
      ENDIF

! compute runoff from cascading Hortonian flow
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        avail_water = SNGL( Upslope_hortonian(Ihru) ) * glacier_free ! likely no cascades to glacier HRUs
        IF ( avail_water>0.0 ) THEN
          IF ( Perv_on==ACTIVE ) Infil = Infil + avail_water
          IF ( Ag_on==ACTIVE ) Infil_ag = Infil_ag + avail_water
          IF ( hru_flag==1 ) THEN
            IF ( Perv_on==ACTIVE ) CALL perv_comp(avail_water, avail_water, Infil, Srp)
            IF ( Ag_on==ACTIVE ) CALL ag_comp(avail_water, avail_water, Infil_ag, Sroff_ag)
          ENDIF
        ENDIF
      ENDIF

!******if rain/snow event with no antecedent snowpack,
!******compute the runoff from the rain first and then proceed with the
!******snowmelt computations

      IF ( Pptmix_nopack(Ihru)==ACTIVE ) THEN
        avail_water = Net_rain * glacier_free
        IF ( Perv_on==ACTIVE ) Infil = Infil + avail_water
        IF ( Ag_on==ACTIVE ) Infil_ag = Infil_ag + avail_water
        IF ( hru_flag==1 ) THEN
          IF ( Perv_on==ACTIVE ) CALL perv_comp(avail_water, avail_water, Infil, Srp)
          IF ( Ag_on==ACTIVE ) CALL ag_comp(avail_water, avail_water, Infil_ag, Sroff_ag)
        ENDIF
      ENDIF

!******If precipitation on snowpack, all water available to the surface is
!******considered to be snowmelt, and the snowmelt infiltration
!******procedure is used.  If there is no snowpack and no precip,
!******then check for melt from last of snowpack.  If rain/snow mix
!******with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt>0.0 ) THEN ! includes glacier melt, if any
        avail_water = Snowmelt * glacier_free
        IF ( Perv_on==ACTIVE ) Infil = Infil + avail_water
        IF ( Ag_on==ACTIVE ) Infil_ag = Infil_ag + avail_water
        IF ( hru_flag==1 ) THEN
          IF ( Pkwater_equiv>0.0D0 .OR. .not.(Net_ppt-Net_snow>0.0) ) THEN
!******Pervious area computations
            IF ( Perv_on==ACTIVE ) CALL check_capacity(Snowinfil_max, Infil)
!******agriculture area computations
            IF ( Ag_on==ACTIVE ) CALL check_capacity_ag(Snowinfil_max, Infil_ag)
!******Snowmelt occurred and depleted the snowpack
          ELSE
            avail_net_ppt = Net_ppt * glacier_free
            IF ( Perv_on==ACTIVE ) CALL perv_comp(avail_water, avail_net_ppt, Infil, Srp)
            IF ( Ag_on==ACTIVE ) CALL ag_comp(avail_water, avail_net_ppt, Infil_ag, Sroff_ag)
          ENDIF
        ENDIF

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.

      ELSEIF ( .not.(Pkwater_equiv>0.0D0) ) THEN

!       If no snowmelt and no snowpack but there was net snow then
!       snowpack was small and was lost to sublimation.

        IF ( .not.(Net_snow>0.0) .AND. Net_rain>0.0 .AND. .not.(Pk_precip>0.0) ) THEN
! no snow, some rain
          avail_water = Net_rain * glacier_free
          IF ( Perv_on==ACTIVE ) Infil = Infil + avail_water
          IF ( Ag_on==ACTIVE ) Infil_ag = Infil_ag + avail_water
          IF ( hru_flag==1 ) THEN
            IF ( Perv_on==ACTIVE ) CALL perv_comp(avail_water, avail_water, Infil, Srp)
            IF ( Ag_on==ACTIVE ) CALL ag_comp(avail_water, avail_water, Infil_ag, Sroff_ag)
          ENDIF
        ENDIF

!***** Snowpack exists, check to see if infil exceeds maximum daily
!***** snowmelt infiltration rate. Infil results from rain snow mix
!***** on a snowfree surface.

      ELSE
        IF ( hru_flag==1 ) THEN
          IF ( Infil>0.0 ) CALL check_capacity(Snowinfil_max, Infil)
          IF ( Infil_ag>0.0 ) CALL check_capacity_ag(Snowinfil_max, Infil_ag)
        ENDIF
      ENDIF

      END SUBROUTINE compute_infil_ag_glcr

!***********************************************************************
      SUBROUTINE perv_comp(Pptp, Ptc, Infil, Srp)
      USE PRMS_CONSTANTS, ONLY: smidx_module
      USE PRMS_MODULE, ONLY: Sroff_flag
      USE PRMS_SRUNOFF, ONLY: Ihru, Smidx_coef, Smidx_exp, &
     &    Carea_max, Carea_min, Carea_dif, Contrib_fraction
      USE PRMS_FLOWVARS, ONLY: Soil_rechr_max
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_soil_rechr
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Pptp, Ptc
      REAL, INTENT(INOUT) :: Infil, Srp
! Local Variables
      REAL :: smidx, srpp, ca_fraction
!***********************************************************************
!******Pervious area computations
      IF ( Sroff_flag==smidx_module ) THEN
        ! antecedent soil_moist
        smidx = It0_soil_moist(Ihru) + (0.5*Ptc)
        IF ( smidx>25.0) THEN
          ca_fraction = Carea_max(Ihru)
        ELSE
          ca_fraction = Smidx_coef(Ihru)*10.0**(Smidx_exp(Ihru)*smidx)
        ENDIF
      ELSE
        ! antecedent soil_rechr
        ca_fraction = Carea_min(Ihru) + Carea_dif(Ihru)*(It0_soil_rechr(Ihru)/Soil_rechr_max(Ihru))
      ENDIF
      IF ( ca_fraction>Carea_max(Ihru) ) THEN
        ca_fraction = Carea_max(Ihru)
      ELSEIF ( .not.(ca_fraction>0.0) ) THEN
        ca_fraction = 0.0
      ENDIF
      srpp = ca_fraction*Pptp
      Contrib_fraction(Ihru) = ca_fraction
      IF ( srpp<0.0 ) THEN
!        PRINT *, 'negative srp', srpp
        srpp = 0.0
      ENDIF
      Infil = Infil - srpp
      Srp = Srp + srpp
      !IF ( Srp < 0.0 ) Srp = 0.0

      END SUBROUTINE perv_comp

!***********************************************************************
      SUBROUTINE ag_comp(Pptp, Ptc, Infil_ag, Sroff_ag)
      USE PRMS_CONSTANTS, ONLY: smidx_module
      USE PRMS_SRUNOFF, ONLY: Ihru, Smidx_coef, Smidx_exp, Carea_max, Carea_min, Carea_dif, ag_contrib_fraction
      USE PRMS_MODULE, ONLY: Sroff_flag
      USE PRMS_FLOWVARS, ONLY: Ag_soil_rechr_max
      USE PRMS_IT0_VARS, ONLY: It0_ag_soil_moist, It0_ag_soil_rechr
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Pptp, Ptc
      REAL, INTENT(INOUT) :: Infil_ag, Sroff_ag
! Local Variables
      REAL :: smidx, srpp, ca_fraction
!***********************************************************************
!******Pervious area computations
      IF ( Sroff_flag==smidx_module ) THEN
        ! antecedent ag_soil_moist
        smidx = It0_ag_soil_moist(Ihru) + (0.5*Ptc)
        IF ( smidx>25.0) THEN
          ca_fraction = Carea_max(Ihru)
        ELSE
          ca_fraction = Smidx_coef(Ihru)*10.0**(Smidx_exp(Ihru)*smidx)
        ENDIF
      ELSE
        ! antecedent ag_soil_rechr
        ca_fraction = Carea_min(Ihru) + Carea_dif(Ihru)*(It0_ag_soil_rechr(Ihru)/Ag_soil_rechr_max(Ihru))
      ENDIF
      IF ( ca_fraction>Carea_max(Ihru) ) THEN
        ca_fraction = Carea_max(Ihru)
      ELSEIF ( .not.(ca_fraction>0.0) ) THEN
        ca_fraction = 0.0
      ENDIF
      srpp = ca_fraction*Pptp
      ag_contrib_fraction(Ihru) = ca_fraction
      IF ( srpp<0.0 ) THEN
!        PRINT *, 'negative srp', srpp
        srpp = 0.0
      ENDIF
      Infil_ag = Infil_ag - srpp
      Sroff_ag = Sroff_ag + srpp
      !IF ( Sroff_ag < 0.0 ) Sroff_ag = 0.0

      END SUBROUTINE ag_comp

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_sroff(Ncascade_hru, Runoff, Hru_sroff_down)
!      USE PRMS_BASIN, ONLY: NEARZERO
!      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Ihru, Upslope_hortonian
      USE PRMS_FLOWVARS, ONLY: Strm_seg_in
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt, Cascade_area
      IMPLICIT NONE
! Functions
      INTRINSIC :: IABS, DBLE
! Arguments
      INTEGER, INTENT(IN) :: Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff
      DOUBLE PRECISION, INTENT(INOUT) :: Hru_sroff_down
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_hortonian(j) = Upslope_hortonian(j) + DBLE( Runoff*Hru_down_fracwt(k, Ihru) )
          Hru_sroff_down = Hru_sroff_down + DBLE( Runoff*Hru_down_frac(k,Ihru) )

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          Strm_seg_in(j) = Strm_seg_in(j) + DBLE( Runoff*Cascade_area(k, Ihru) )*Cfs_conv
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - SNGL( Hru_sroff_down )

      END SUBROUTINE run_cascade_sroff

!***********************************************************************
! fill soil to soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity(Snowinfil_max, Infil)
      USE PRMS_FLOWVARS, ONLY: Soil_moist_max, Soil_moist
      USE PRMS_SRUNOFF, ONLY: Ihru, Srp
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Snowinfil_max
      REAL, INTENT(INOUT) :: Infil
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Soil_moist_max(Ihru) - Soil_moist(Ihru)
      excess = Infil - capacity
      IF ( excess>Snowinfil_max ) THEN
        Srp = Srp + excess - Snowinfil_max
        Infil = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity

!***********************************************************************
! fill soil to ag_soil_moist_max, if more than capacity restrict
! infiltration by snowinfil_max, with excess added to runoff
!***********************************************************************
      SUBROUTINE check_capacity_ag(Snowinfil_max, Infil_ag)
      USE PRMS_FLOWVARS, ONLY: Ag_soil_moist_max, Ag_soil_moist
      USE PRMS_SRUNOFF, ONLY: Ihru, Sroff_ag
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Snowinfil_max
      REAL, INTENT(INOUT) :: Infil_ag
! Local Variables
      REAL :: capacity, excess
!***********************************************************************
      capacity = Ag_soil_moist_max(Ihru) - Ag_soil_moist(Ihru)
      excess = Infil_ag - capacity
      IF ( excess>Snowinfil_max ) THEN
        Sroff_ag = Sroff_ag + excess - Snowinfil_max
        Infil_ag = Snowinfil_max + capacity
      ENDIF

      END SUBROUTINE check_capacity_ag

!***********************************************************************
! Initialize depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_init()
      USE PRMS_SRUNOFF
      USE PRMS_CONSTANTS, ONLY: ACTIVE
      use PRMS_READ_PARAM_FILE, only: getparam_real
      USE PRMS_MODULE, ONLY: Init_vars_from_file, Nhru, PRMS4_flag, Inputerror_flag !, AG_flag
      USE PRMS_BASIN, ONLY: Dprst_clos_flag, Hru_frac_dprst, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv, &
     &    Hru_area_dble, Active_hrus, Hru_route_order, Dprst_open_flag
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open, Dprst_vol_clos, Dprst_stor_hru
      use prms_utils, only: read_error
      IMPLICIT NONE
! Functions
      INTRINSIC :: EXP, LOG, DBLE, SNGL
! Local Variables
      INTEGER :: i, j
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
        IF ( getparam_real(MODNAME, 'dprst_frac_init', Nhru, Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
      ENDIF
      IF ( getparam_real(MODNAME, 'dprst_flow_coef', Nhru, Dprst_flow_coef)/=0 ) CALL read_error(2, 'dprst_flow_coef')
      IF ( Dprst_open_flag==ACTIVE ) THEN
        IF ( getparam_real(MODNAME, 'dprst_seep_rate_open', Nhru, Dprst_seep_rate_open)/=0 )  &
     &       CALL read_error(2, 'dprst_seep_rate_open')
        IF ( getparam_real(MODNAME, 'va_open_exp', Nhru, Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
        IF ( getparam_real(MODNAME, 'op_flow_thres', Nhru, Op_flow_thres)/=0 ) CALL read_error(2, 'op_flow_thres')
      ELSE
        Dprst_seep_rate_open = 0.0
        Va_open_exp = 0.0
        Op_flow_thres = 0.0
      ENDIF
      IF ( PRMS4_flag==ACTIVE ) THEN
        IF ( getparam_real(MODNAME, 'sro_to_dprst', Nhru, Sro_to_dprst_perv)/=0 ) CALL read_error(2, 'sro_to_dprst')
      ELSE
        IF ( getparam_real(MODNAME, 'sro_to_dprst_perv', Nhru, Sro_to_dprst_perv)/=0 ) CALL read_error(2, 'sro_to_dprst_perv')
      ENDIF
      IF ( getparam_real(MODNAME, 'sro_to_dprst_imperv', Nhru, Sro_to_dprst_imperv)/=0 ) &
     &     CALL read_error(2, 'sro_to_dprst_imperv')
      IF ( getparam_real(MODNAME, 'dprst_depth_avg', Nhru, Dprst_depth_avg)/=0 ) CALL read_error(2, 'dprst_depth_avg')
      IF ( getparam_real(MODNAME, 'dprst_et_coef', Nhru, Dprst_et_coef)/=0 ) CALL read_error(2, 'dprst_et_coef')
      IF ( Dprst_clos_flag==ACTIVE ) THEN
        IF ( getparam_real(MODNAME, 'dprst_seep_rate_clos', Nhru, Dprst_seep_rate_clos)/=0 ) &
     &       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam_real(MODNAME, 'va_clos_exp', Nhru, Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
      ELSE
        Dprst_seep_rate_clos = 0.0
        Va_clos_exp = 0.0
      ENDIF
      Dprst_area_open = 0.0
      Dprst_area_clos = 0.0
!      IF ( AG_flag==ACTIVE ) THEN
!        IF ( getparam_real(MODNAME, 'sro_to_dprst_ag', Nhru, Sro_to_dprst_ag)/=0 ) &
!     &       CALL read_error(2, 'sro_to_dprst_ag')
!      ENDIF
      Dprst_stor_hru = 0.0D0
      Dprst_vol_thres_open = 0.0D0
      Dprst_vol_open_max = 0.0D0
      Dprst_vol_clos_max = 0.0D0
      Dprst_vol_frac = 0.0D0
      Dprst_vol_open_frac = 0.0D0
      Dprst_vol_clos_frac = 0.0D0
      Basin_dprst_volop = 0.0D0
      Basin_dprst_volcl = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)

        IF ( Hru_frac_dprst(i)>0.0 ) THEN
          IF ( Dprst_depth_avg(i)==0.0 ) THEN
            PRINT *, 'ERROR, dprst_frac>0 and dprst_depth_avg==0 for HRU:', i, '; dprst_frac:', Hru_frac_dprst(i)
            Inputerror_flag = 1
            CYCLE
          ENDIF
!         calculate open and closed volumes (acre-inches) of depression storage by HRU
!         Dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
          IF ( Dprst_clos_flag==ACTIVE ) Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
          IF ( Dprst_open_flag==ACTIVE ) Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )

!         calculate the initial open and closed depression storage volume:
          IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
            IF ( Dprst_open_flag==ACTIVE ) Dprst_vol_open(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_open_max(i)
            IF ( Dprst_clos_flag==ACTIVE ) Dprst_vol_clos(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_clos_max(i)
          ENDIF

!         threshold volume is calculated as the % of maximum open
!         depression storage above which flow occurs *  total open depression storage volume
          Dprst_vol_thres_open(i) = DBLE(Op_flow_thres(i))*Dprst_vol_open_max(i)

!         initial open and closed storage volume as fraction of total open and closed storage volume

!         Open depression surface area for each HRU:
          IF ( Dprst_vol_open(i)>0.0D0 ) THEN
            open_vol_r = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
            IF ( .not.(open_vol_r>0.0) ) THEN
              frac_op_ar = 0.0
            ELSEIF ( open_vol_r>1.0 ) THEN
              frac_op_ar = 1.0
            ELSE
              frac_op_ar = EXP(Va_open_exp(i)*LOG(open_vol_r))
            ENDIF
          ENDIF

!         Closed depression surface area for each HRU:
          IF ( Dprst_vol_clos(i)>0.0D0 ) THEN
            clos_vol_r = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
            IF ( .not.(clos_vol_r>0.0) ) THEN
              frac_cl_ar = 0.0
            ELSEIF ( clos_vol_r>1.0 ) THEN
              frac_cl_ar = 1.0
            ELSE
              frac_cl_ar = EXP(Va_clos_exp(i)*LOG(clos_vol_r))
            ENDIF
          ENDIF

!         calculate the basin open and closed depression storage volumes
          Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
          Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
          Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hru_area_dble(i)
          IF ( Dprst_vol_open_max(i)>0.0D0 ) Dprst_vol_open_frac(i) = Dprst_vol_open(i)/Dprst_vol_open_max(i)
          IF ( Dprst_vol_clos_max(i)>0.0D0 ) Dprst_vol_clos_frac(i) = Dprst_vol_clos(i)/Dprst_vol_clos_max(i)
          if (Dprst_vol_open_max(i)+Dprst_vol_clos_max(i) > 0.0 ) then  !RGN added to avoid divide by zero 1/20/2022
            Dprst_vol_frac(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/(Dprst_vol_open_max(i)+Dprst_vol_clos_max(i))
          end if
        ENDIF
      ENDDO
      Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
      Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) DEALLOCATE ( Dprst_frac_init )

      END SUBROUTINE dprst_init

!***********************************************************************
!     Compute depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_comp(Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
     &           Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, Dprst_area_open, &
     &           Dprst_sroff_hru, Dprst_seep_hru, Sro_to_dprst_perv, Sro_to_dprst_imperv, Dprst_evap_hru, &
     &           Avail_et, Availh2o_total, Dprst_in, frozen_flag, lateral_flow_flag)
      USE PRMS_CONSTANTS, ONLY: ERROR_water_use, NEARZERO, OFF, ACTIVE ! , DNEARZERO, DEBUG_less
      USE PRMS_MODULE, ONLY: Dprst_add_water_use, Dprst_transfer_water_use, &
     &    Nowyear, Nowmonth, Nowday, Dprst_ag_gain, Dprst_ag_transfer, Ag_package !, Print_debug
      USE PRMS_SRUNOFF, ONLY: Srp, Sri, Ihru, Perv_frac, Imperv_frac, Hruarea, Dprst_et_coef, &
     &    Dprst_seep_rate_open, Dprst_seep_rate_clos, Va_clos_exp, Va_open_exp, Dprst_flow_coef, &
     &    Dprst_vol_thres_open, Dprst_vol_clos_max, Dprst_insroff_hru, &
     &    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff, &
     &    Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac, Hruarea_dble, Sroff_ag
      USE PRMS_BASIN, ONLY: Dprst_frac_open, Dprst_frac_clos, Ag_frac
      USE PRMS_WATER_USE, ONLY: Dprst_transfer, Dprst_gain
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Dprst_stor_hru, Dprst_total_open_in, Dprst_total_open_out, &
     &    Dprst_total_clos_in, Dprst_total_clos_out, Snowcov_area
      IMPLICIT NONE
! Functions
      INTRINSIC :: EXP, LOG, MAX, DBLE, SNGL
! Arguments
      INTEGER, INTENT(IN) :: frozen_flag, lateral_flow_flag
      REAL, INTENT(IN) :: Dprst_area_open_max, Dprst_area_clos_max, Availh2o_total
      REAL, INTENT(IN) :: Sro_to_dprst_perv, Sro_to_dprst_imperv
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_open, Dprst_vol_clos, Dprst_in
      REAL, INTENT(INOUT) :: Avail_et, Dprst_evap_hru
      REAL, INTENT(OUT) :: Dprst_area_open, Dprst_area_clos
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_sroff_hru, Dprst_seep_hru
! Local Variables
      REAL :: inflow, dprst_avail_et
      REAL :: dprst_srp, dprst_sri
      REAL :: dprst_srp_open, dprst_srp_clos, dprst_sri_open, dprst_sri_clos
      REAL :: dprst_sra_open, dprst_sra_clos, dprst_sra
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r, unsatisfied_et
      REAL :: tmp, dprst_evap_open, dprst_evap_clos
      DOUBLE PRECISION :: seep_open, seep_clos, tmp1
      DOUBLE PRECISION :: open_in, open_out, clos_in, clos_out
!***********************************************************************
      inflow = Availh2o_total
      IF ( Dprst_add_water_use==ACTIVE ) THEN
        IF ( Dprst_gain(Ihru)>0.0 ) inflow = inflow + Dprst_gain(Ihru) / SNGL( Cfs_conv )
      ENDIF
      IF ( Ag_package==ACTIVE ) inflow = inflow + Dprst_ag_gain(Ihru) ! gain in acre-inches

      IF ( Dprst_area_open_max>0.0 ) THEN
        Dprst_in = DBLE( inflow*Dprst_area_open_max ) ! acre-inches
        Dprst_vol_open = Dprst_vol_open + Dprst_in
      ENDIF
      open_in = Dprst_in

      clos_in = 0.0D0
      IF ( Dprst_area_clos_max>0.0 ) THEN
        tmp1 = DBLE( inflow*Dprst_area_clos_max ) ! acre-inches
        clos_in = tmp1
        Dprst_vol_clos = Dprst_vol_clos + tmp1
        Dprst_in = Dprst_in + tmp1
      ENDIF
      Dprst_in = Dprst_in/Hruarea_dble ! inches over HRU

      ! add any pervious surface runoff fraction to depressions
      dprst_srp = 0.0
      dprst_sri = 0.0
      dprst_sra = 0.0
      IF ( Srp>0.0 ) THEN
        tmp = Srp*Perv_frac*Sro_to_dprst_perv*Hruarea
        IF ( Dprst_area_open_max>0.0 ) THEN
          dprst_srp_open = tmp*Dprst_frac_open(Ihru) ! acre-inches
          open_in = open_in + DBLE( dprst_srp_open )
          dprst_srp = dprst_srp_open/Hruarea
          Dprst_vol_open = Dprst_vol_open + DBLE( dprst_srp_open )
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          dprst_srp_clos = tmp*Dprst_frac_clos(Ihru)
          clos_in = clos_in + DBLE( dprst_srp_clos )
          dprst_srp = dprst_srp + dprst_srp_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + DBLE( dprst_srp_clos )
        ENDIF
        Srp = Srp - dprst_srp/Perv_frac
        IF ( Srp<0.0 ) THEN
          !IF ( Srp<-NEARZERO ) PRINT *, 'dprst srp<0.0', Srp, dprst_srp
          ! may need to adjust dprst_srp and volumes
          Srp = 0.0
        ENDIF
      ENDIF

      IF ( Sri>0.0 ) THEN
        tmp = Sri*Imperv_frac*Sro_to_dprst_imperv*Hruarea
        IF ( Dprst_area_open_max>0.0 ) THEN
          dprst_sri_open = tmp*Dprst_frac_open(Ihru)
          open_in = open_in + DBLE( dprst_sri_open )
          dprst_sri = dprst_sri_open/Hruarea
          Dprst_vol_open = Dprst_vol_open + DBLE( dprst_sri_open )
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          dprst_sri_clos = tmp*Dprst_frac_clos(Ihru)
          clos_in = clos_in + DBLE( dprst_sri_clos )
          dprst_sri = dprst_sri + dprst_sri_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + DBLE( dprst_sri_clos )
        ENDIF
        Sri = Sri - dprst_sri/Imperv_frac
        IF ( Sri<0.0 ) THEN
          !IF ( Sri<-NEARZERO ) PRINT *, 'dprst sri<0.0', Sri, dprst_sri
          ! may need to adjust dprst_sri and volumes
          Sri = 0.0
        ENDIF
      ENDIF
      Dprst_insroff_hru(Ihru) = dprst_srp + dprst_sri

      ! add any agriculture surface runoff fraction to depressions
      IF ( Sroff_ag>0.0 ) THEN
        tmp = Sroff_ag*Ag_frac(Ihru)*Sro_to_dprst_perv*Hruarea
        IF ( Dprst_area_open_max>0.0 ) THEN
          dprst_sra_open = tmp*Dprst_frac_open(Ihru) ! acre-inches
          open_in = open_in + DBLE( dprst_sra_open )
          dprst_sra = dprst_sra_open/Hruarea
          Dprst_vol_open = Dprst_vol_open + DBLE( dprst_sra_open )
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          dprst_sra_clos = tmp*Dprst_frac_clos(Ihru)
          clos_in = clos_in + DBLE( dprst_sra_clos )
          dprst_sra = dprst_sra + dprst_sra_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + DBLE( dprst_sra_clos )
        ENDIF
        Sroff_ag = Sroff_ag - dprst_sra/Ag_frac(Ihru)
        IF ( Sroff_ag<0.0 ) THEN
          !IF ( Sroff_ag<-NEARZERO ) PRINT *, 'dprst sra<0.0', Sroff_ag, dprst_sra
          ! may need to adjust dprst_sra and volumes
          Sroff_ag = 0.0
        ENDIF
        Dprst_insroff_hru(Ihru) = Dprst_insroff_hru(Ihru) + dprst_sra
      ENDIF

      open_out = 0.0D0
      clos_out = 0.0D0
      IF ( Dprst_transfer_water_use==ACTIVE ) THEN
        IF ( Dprst_area_open_max>0.0 ) THEN
          IF ( Dprst_transfer(Ihru)>0.0 ) THEN
            IF ( SNGL(Dprst_vol_open*Cfs_conv)<Dprst_transfer(Ihru) ) THEN
              PRINT *, 'ERROR, not enough storage for transfer from open surface-depression storage:', &
     &                  Ihru, ' Date:', Nowyear, Nowmonth, Nowday
              PRINT *, '       storage: ', Dprst_vol_open, '; transfer: ', Dprst_transfer(Ihru)/Cfs_conv
              ERROR STOP ERROR_water_use
            ENDIF
            open_out = DBLE( Dprst_transfer(Ihru)*Dprst_area_open_max ) / Cfs_conv
            Dprst_vol_open = Dprst_vol_open - open_out
          ENDIF
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          IF ( Dprst_transfer(Ihru)>0.0 ) THEN
            IF ( SNGL(Dprst_area_clos_max*Cfs_conv)<Dprst_transfer(Ihru) ) THEN
              PRINT *, 'ERROR, not enough storage for transfer from closed surface-depression storage:', &
     &                  Ihru, ' Date:', Nowyear, Nowmonth, Nowday
              PRINT *, '       storage: ', Dprst_vol_clos, '; transfer: ', Dprst_transfer(Ihru)/Cfs_conv
              ERROR STOP ERROR_water_use
            ENDIF
            clos_out = clos_out + DBLE( Dprst_transfer(Ihru)*Dprst_area_clos_max ) / Cfs_conv
            Dprst_vol_clos = Dprst_vol_clos - DBLE( Dprst_transfer(Ihru)*Dprst_area_clos_max ) / Cfs_conv
          ENDIF
        ENDIF
      ENDIF

      IF ( Ag_package==ACTIVE ) THEN
        open_out = open_out + DBLE( Dprst_ag_transfer(Ihru) )
        Dprst_vol_open = Dprst_vol_open - DBLE( Dprst_ag_transfer(Ihru) ) ! ag transfer in acre-inches
      ENDIF

!     Open depression surface area for each HRU:
      Dprst_area_open = 0.0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        open_vol_r = SNGL( Dprst_vol_open/Dprst_vol_open_max )
        IF ( .not.(open_vol_r>0.0 ) ) THEN
          frac_op_ar = 0.0
        ELSEIF ( open_vol_r>1.0 ) THEN
          frac_op_ar = 1.0
        ELSE
          frac_op_ar = EXP(Va_open_exp(Ihru)*LOG(open_vol_r))
        ENDIF
        Dprst_area_open = Dprst_area_open_max*frac_op_ar
        IF ( Dprst_area_open>Dprst_area_open_max ) Dprst_area_open = Dprst_area_open_max
        IF ( .not.(Dprst_area_open>0.0) ) Dprst_area_open = 0.0
      ENDIF

!     Closed depression surface area for each HRU:
      IF ( Dprst_area_clos_max>0.0 ) THEN
        Dprst_area_clos = 0.0
        IF ( Dprst_vol_clos>0.0D0 ) THEN
          clos_vol_r = SNGL( Dprst_vol_clos/Dprst_vol_clos_max(Ihru) )
          IF ( .not.(clos_vol_r>0.0) ) THEN
            frac_cl_ar = 0.0
          ELSEIF ( clos_vol_r>1.0 ) THEN
            frac_cl_ar = 1.0
          ELSE
            frac_cl_ar = EXP(Va_clos_exp(Ihru)*LOG(clos_vol_r))
          ENDIF
          Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
          IF ( Dprst_area_clos>Dprst_area_clos_max ) Dprst_area_clos = Dprst_area_clos_max
          IF ( .not.(Dprst_area_clos>0.0) ) Dprst_area_clos = 0.0
        ENDIF
      ENDIF

      ! evaporate water from depressions based on snowcov_area
      ! dprst_evap_open & dprst_evap_clos = acre-inches on the HRU
      unsatisfied_et = Avail_et
      dprst_avail_et = (Potet(Ihru)*(1.0-Snowcov_area(Ihru)))*Dprst_et_coef(Ihru)
      Dprst_evap_hru = 0.0
      IF ( dprst_avail_et>0.0 ) THEN
        dprst_evap_open = 0.0
        dprst_evap_clos = 0.0
        IF ( Dprst_area_open>0.0 ) THEN
          dprst_evap_open = MIN(Dprst_area_open*dprst_avail_et, SNGL(Dprst_vol_open))
          IF ( dprst_evap_open/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>DEBUG_less ) THEN
            !  PRINT *, 'Warning, open dprst evaporation > available ET, HRU:, ', Ihru, &
!    &                  unsatisfied_et, dprst_evap_open*DBLE(Dprst_frac_open(Ihru))
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_open = unsatisfied_et*Hruarea
          ENDIF
          !IF ( dprst_evap_open>SNGL(Dprst_vol_open) ) print *, '>', dprst_evap_open, dprst_vol_open
          IF ( dprst_evap_open>SNGL(Dprst_vol_open) ) dprst_evap_open = SNGL( Dprst_vol_open )
          unsatisfied_et = unsatisfied_et - dprst_evap_open/Hruarea
          open_out = open_out + DBLE( dprst_evap_open )
          Dprst_vol_open = Dprst_vol_open - DBLE( dprst_evap_open )
        ENDIF
        IF ( Dprst_area_clos>0.0 ) THEN
          dprst_evap_clos = MIN(Dprst_area_clos*dprst_avail_et, SNGL(Dprst_vol_clos))
          IF ( dprst_evap_clos/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>DEBUG_less ) THEN
            !  PRINT *, 'Warning, closed dprst evaporation > available ET, HRU:, ', Ihru, &
!      &                 unsatisfied_et, dprst_evap_clos*Dprst_frac_clos(Ihru)
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_clos = unsatisfied_et*Hruarea
          ENDIF
          IF ( dprst_evap_clos>SNGL(Dprst_vol_clos) ) dprst_evap_clos = SNGL( Dprst_vol_clos )
          clos_out = clos_out + DBLE( dprst_evap_clos )
          Dprst_vol_clos = Dprst_vol_clos - DBLE( dprst_evap_clos )
        ENDIF
        Dprst_evap_hru = (dprst_evap_open + dprst_evap_clos)/Hruarea
      ENDIF

      ! compute seepage, allow seepage even if frozen soil
      IF ( Dprst_vol_open>0.0D0 ) THEN
        seep_open = Dprst_vol_open*DBLE( Dprst_seep_rate_open(Ihru) )
        open_out = open_out + seep_open
        Dprst_vol_open = Dprst_vol_open - seep_open
        IF ( Dprst_vol_open<0.0D0 ) THEN
!          IF ( Dprst_vol_open<-DNEARZERO ) PRINT *, 'negative dprst_vol_open:', Dprst_vol_open, ' HRU:', Ihru
          seep_open = seep_open + Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
        Dprst_seep_hru = seep_open/Hruarea_dble
      ENDIF

      ! compute open surface runoff, only if not a swale and not frozen
      IF ( lateral_flow_flag == ACTIVE .AND. frozen_flag == OFF ) THEN
        IF ( Dprst_vol_open>0.0D0 ) THEN
          Dprst_sroff_hru = MAX( 0.0D0, Dprst_vol_open-Dprst_vol_open_max )
          Dprst_sroff_hru = Dprst_sroff_hru + &
     &                      MAX( 0.0D0, (Dprst_vol_open-Dprst_sroff_hru-Dprst_vol_thres_open(Ihru))*DBLE(Dprst_flow_coef(Ihru)) )
          open_out = open_out + Dprst_sroff_hru
          Dprst_vol_open = Dprst_vol_open - Dprst_sroff_hru
          Dprst_sroff_hru = Dprst_sroff_hru/Hruarea_dble
          ! sanity checks
          IF ( Dprst_vol_open<0.0D0 ) THEN
!            IF ( Dprst_vol_open<-DNEARZERO ) PRINT *, 'issue, dprst_vol_open<0.0', Dprst_vol_open
            Dprst_vol_open = 0.0D0
          ENDIF
        ENDIF
      ENDIF

      IF ( Dprst_area_clos_max>0.0 ) THEN
        IF ( Dprst_area_clos>NEARZERO ) THEN
          seep_clos = Dprst_vol_clos*DBLE( Dprst_seep_rate_clos(Ihru) )
          clos_out = clos_out + seep_clos
          Dprst_vol_clos = Dprst_vol_clos - seep_clos
          IF ( Dprst_vol_clos<0.0D0 ) THEN
!            IF ( Dprst_vol_clos<-DNEARZERO ) PRINT *, 'issue, dprst_vol_clos<0.0', Dprst_vol_clos
            seep_clos = seep_clos + Dprst_vol_clos
            Dprst_vol_clos = 0.0D0
          ENDIF
          Dprst_seep_hru = Dprst_seep_hru + seep_clos/Hruarea_dble
        ENDIF
      ENDIF

      Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open
      Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos
      Basin_dprst_evap = Basin_dprst_evap + DBLE( Dprst_evap_hru*Hruarea )
      Basin_dprst_seep = Basin_dprst_seep + Dprst_seep_hru*Hruarea_dble
      Basin_dprst_sroff = Basin_dprst_sroff + Dprst_sroff_hru*Hruarea_dble
      Avail_et = Avail_et - Dprst_evap_hru
      IF ( Dprst_vol_open_max>0.0D0 ) Dprst_vol_open_frac(Ihru) = Dprst_vol_open/Dprst_vol_open_max
      IF ( Dprst_vol_clos_max(Ihru)>0.0D0 ) Dprst_vol_clos_frac(Ihru) = Dprst_vol_clos/Dprst_vol_clos_max(Ihru)
      if (Dprst_vol_open_max+Dprst_vol_clos_max(Ihru) > 0.0D0 ) then  !RGN added to avoid divide by zero 1/20/2022
        Dprst_vol_frac(Ihru) = (Dprst_vol_open+Dprst_vol_clos)/(Dprst_vol_open_max+Dprst_vol_clos_max(Ihru))
      end if
      Dprst_stor_hru(Ihru) = (Dprst_vol_open+Dprst_vol_clos)/Hruarea_dble
      Dprst_total_open_in(Ihru) = open_in
      Dprst_total_open_out(Ihru) = open_out
      Dprst_total_clos_in(Ihru) = clos_in
      Dprst_total_clos_out(Ihru) = clos_out

      END SUBROUTINE dprst_comp

!***********************************************************************
!     srunoff_restart - write or read srunoff restart file
!***********************************************************************
      SUBROUTINE srunoff_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, Dprst_flag, Frozen_flag, text_restart_flag
      USE PRMS_FLOWVARS, ONLY: Dprst_stor_hru, Hru_impervstor
      USE PRMS_SRUNOFF
      use prms_utils, only: check_restart
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
    IF ( In_out==SAVE_INIT ) THEN
      IF ( text_restart_flag==OFF ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_imperv_stor, Basin_dprst_volop, Basin_dprst_volcl
        WRITE ( Restart_outunit ) Hru_impervstor
        IF ( Dprst_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit ) Dprst_area_open
          WRITE ( Restart_outunit ) Dprst_area_clos
          WRITE ( Restart_outunit ) Dprst_stor_hru
          WRITE ( Restart_outunit ) Dprst_vol_thres_open
        ENDIF
        IF ( Frozen_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit ) Frozen
          WRITE ( Restart_outunit ) Cfgi
          WRITE ( Restart_outunit ) Cfgi_prev
        ENDIF
      ELSE
        WRITE ( Restart_outunit, * ) MODNAME
        WRITE ( Restart_outunit, * ) Basin_imperv_stor, Basin_dprst_volop, Basin_dprst_volcl
        WRITE ( Restart_outunit, * ) Hru_impervstor
        IF ( Dprst_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit, * ) Dprst_area_open
          WRITE ( Restart_outunit, * ) Dprst_area_clos
          WRITE ( Restart_outunit, * ) Dprst_stor_hru
          WRITE ( Restart_outunit, * ) Dprst_vol_thres_open
        ENDIF
        IF ( Frozen_flag==ACTIVE ) THEN
          WRITE ( Restart_outunit, * ) Frozen
          WRITE ( Restart_outunit, * ) Cfgi
          WRITE ( Restart_outunit, * ) Cfgi_prev
        ENDIF
      ENDIF
    ELSE
      IF ( text_restart_flag==OFF ) THEN
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_imperv_stor, Basin_dprst_volop, Basin_dprst_volcl
        READ ( Restart_inunit ) Hru_impervstor
        IF ( Dprst_flag==ACTIVE ) THEN
          READ ( Restart_inunit ) Dprst_area_open
          READ ( Restart_inunit ) Dprst_area_clos
          READ ( Restart_inunit ) Dprst_stor_hru
          READ ( Restart_inunit ) Dprst_vol_thres_open
        ENDIF
        IF ( Frozen_flag==ACTIVE ) THEN ! could be problem for restart
          READ ( Restart_inunit ) Frozen
          READ ( Restart_inunit ) Cfgi
          READ ( Restart_inunit ) Cfgi_prev
        ENDIF
      ELSE
        READ ( Restart_inunit, * ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit, * ) Basin_imperv_stor, Basin_dprst_volop, Basin_dprst_volcl
        READ ( Restart_inunit, * ) Hru_impervstor
        IF ( Dprst_flag==ACTIVE ) THEN
          READ ( Restart_inunit, * ) Dprst_area_open
          READ ( Restart_inunit, * ) Dprst_area_clos
          READ ( Restart_inunit, * ) Dprst_stor_hru
          READ ( Restart_inunit, * ) Dprst_vol_thres_open
        ENDIF
        IF ( Frozen_flag==ACTIVE ) THEN ! could be problem for restart
          READ ( Restart_inunit, * ) Frozen
          READ ( Restart_inunit, * ) Cfgi
          READ ( Restart_inunit, * ) Cfgi_prev
        ENDIF
      ENDIF
    ENDIF
    END SUBROUTINE srunoff_restart

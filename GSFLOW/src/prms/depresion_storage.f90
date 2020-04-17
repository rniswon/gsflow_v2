!***********************************************************************
! Computes surface depression storage and flows for each HRU
!***********************************************************************
      MODULE PRMS_DPRST
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=17), SAVE :: MODNAME
      INTEGER, SAVE :: Ihru
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_thres_open(:), Dprst_in(:), Dprst_stor_ante(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_vol_open_max(:), Dprst_vol_clos_max(:)
      REAL, SAVE :: Perv_frac, Imperv_frac, Hruarea_imperv, Hruarea
      DOUBLE PRECISION, SAVE :: Hruarea_dble, Basin_dprst_hortonian_lakes
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: It0_dprst_vol_open(:), It0_dprst_vol_clos(:)
!   Declared Parameters for Depression Storage
      REAL, SAVE, ALLOCATABLE :: Op_flow_thres(:), Sro_to_dprst_perv(:)
      REAL, SAVE, ALLOCATABLE :: Va_clos_exp(:), Va_open_exp(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_flow_coef(:), Dprst_frac_init(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_seep_rate_open(:), Dprst_seep_rate_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_depth_avg(:), Sro_to_dprst_imperv(:), Dprst_et_coef(:)
!   Declared Variables for Depression Storage
      DOUBLE PRECISION, SAVE :: Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep
      DOUBLE PRECISION, SAVE :: Basin_dprst_volop, Basin_dprst_volcl
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Dprst_stor_hru(:), Dprst_sroff_hru(:), Dprst_seep_hru(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Upslope_dprst_hortonian(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_area_open(:), Dprst_area_clos(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_insroff_hru(:), Dprst_evap_hru(:)
      REAL, SAVE, ALLOCATABLE :: Dprst_vol_frac(:), Dprst_vol_open_frac(:), Dprst_vol_clos_frac(:)
      END MODULE PRMS_DPRST

!***********************************************************************
!     Main depresion_storage routine
!***********************************************************************
      INTEGER FUNCTION depresion_storage()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: dprst_decl, dprst_init, dprst_run
      EXTERNAL :: dprst_restart
!***********************************************************************
      depresion_storage = 0

      IF ( Process(:3)=='run' ) THEN
        depresion_storage = dprst_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        depresion_storage = dprst_decl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file>0 ) CALL dprst_restart(1)
        depresion_storage = dprst_init()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL dprst_restart(0)
      ENDIF

      END FUNCTION depresion_storage

!***********************************************************************
!     dprst_decl - set up parameters for surface depression computations
!***********************************************************************
      INTEGER FUNCTION dprst_decl()
      USE PRMS_DPRST
      USE PRMS_MODULE, ONLY: Model, Nhru, Print_debug, DOCUMENTATION, Init_vars_from_file, &
     &    PRMS4_flag, GSFLOW_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam
      EXTERNAL read_error, print_module, declvar_dble, declvar_real, declvar_int
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_dprst
!***********************************************************************
      dprst_decl = 0

      Version_dprst = 'depresion_storage.f90 2020-04-17 14:26:00Z'
      MODNAME = 'depresion_storage'
      Version_dprst = MODNAME//'.f90 '//Version_dprst(13:80)
      CALL print_module(Version_dprst, 'Surface Depresion Storage   ', 90)

! Depression storage variables
      CALL declvar_dble(MODNAME, 'basin_dprst_sroff', 'one', 1, 'double', &
     &     'Basin area-weighted average surface runoff from open surface-depression storage', &
     &     'inches', Basin_dprst_sroff)

      CALL declvar_dble(MODNAME, 'basin_dprst_evap', 'one', 1, 'double', &
     &     'Basin area-weighted average evaporation from surface-depression storage', &
     &     'inches', Basin_dprst_evap)

      CALL declvar_dble(MODNAME, 'basin_dprst_seep', 'one', 1, 'double', &
     &     'Basin area-weighted average seepage from surface-depression storage', &
     &     'inches', Basin_dprst_seep)

      CALL declvar_dble(MODNAME, 'basin_dprst_volop', 'one', 1, 'double', &
     &     'Basin area-weighted average storage volume in open surface depressions', &
     &     'inches', Basin_dprst_volop)

      CALL declvar_dble(MODNAME, 'basin_dprst_volcl', 'one', 1, 'double', &
     &     'Basin area-weighted average storage volume in closed surface depressions', &
     &     'inches', Basin_dprst_volcl)

      ALLOCATE ( Dprst_sroff_hru(Nhru) )
      CALL declvar_dble(MODNAME, 'dprst_sroff_hru', 'nhru', Nhru, 'double', &
     &     'Surface runoff from open surface-depression storage for each HRU', &
     &     'inches', Dprst_sroff_hru)

      ALLOCATE ( Dprst_insroff_hru(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_insroff_hru', 'nhru', Nhru, 'real', &
     &     'Surface runoff from pervious and impervious portions into open and closed surface-depression storage for each HRU', &
     &     'inches', Dprst_insroff_hru)

      ALLOCATE ( Dprst_area_open(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_area_open', 'nhru', Nhru, 'real', &
     &     'Surface area of open surface depressions based on storage volume for each HRU', &
     &     'acres', Dprst_area_open)

      ALLOCATE ( Dprst_area_clos(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_area_clos', 'nhru', Nhru, 'real', &
     &     'Surface area of closed surface depressions based on storage volume for each HRU', &
     &     'acres', Dprst_area_clos)

      ALLOCATE ( Upslope_dprst_hortonian(Nhru) )
      CALL declvar_dble(MODNAME, 'upslope_dprst_hortonian', 'nhru', Nhru, 'double', &
     &     'Upslope surface-depression spillage and interflow for each HRU', &
     &     'inches', Upslope_dprst_hortonian)

      ALLOCATE ( Dprst_stor_hru(Nhru) )
      CALL declvar_dble(MODNAME, 'dprst_stor_hru', 'nhru', Nhru, 'double', &
     &     'Surface-depression storage for each HRU', &
     &     'inches', Dprst_stor_hru)

      ALLOCATE ( Dprst_seep_hru(Nhru) )
      CALL declvar_dble(MODNAME, 'dprst_seep_hru', 'nhru', Nhru, 'double', &
     &     'Seepage from surface-depression storage to associated GWR for each HRU', &
     &     'inches', Dprst_seep_hru)

      ALLOCATE ( Dprst_evap_hru(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_evap_hru', 'nhru', Nhru, 'real', &
     &     'Evaporation from surface-depression storage for each HRU', &
     &     'inches', Dprst_evap_hru)

      ALLOCATE ( Dprst_vol_open_frac(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_vol_open_frac', 'nhru', Nhru, 'real', &
     &    'Fraction of open surface-depression storage of the maximum storage for each HRU', &
     &    'decimal fraction', Dprst_vol_open_frac)

      ALLOCATE ( Dprst_vol_clos_frac(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_vol_clos_frac', 'nhru', Nhru, 'real', &
     &    'Fraction of closed surface-depression storage of the maximum storage for each HRU', &
     &    'decimal fraction', Dprst_vol_clos_frac)

      ALLOCATE ( Dprst_vol_frac(Nhru) )
      CALL declvar_real(MODNAME, 'dprst_vol_frac', 'nhru', Nhru, 'real', &
     &    'Fraction of surface-depression storage of the maximum storage for each HRU', &
     &    'decimal fraction', Dprst_vol_frac)

      ALLOCATE ( Dprst_vol_open_max(Nhru), Dprst_vol_clos_max(Nhru), Dprst_vol_thres_open(Nhru), Dprst_in(Nhru) )
      IF ( GSFLOW_flag==1 ) ALLOCATE ( It0_dprst_vol_open(Nhru), It0_dprst_vol_clos(Nhru) )

! Declare parameters
      ALLOCATE ( Dprst_depth_avg(Nhru) )
      IF ( declparam(MODNAME, 'dprst_depth_avg', 'nhru', 'real', &
     &     '132.0', '0.0', '500.0', &
     &     'Average depth of surface depressions at maximum storage capacity', &
     &     'Average depth of surface depressions at maximum storage capacity', &
     &     'inches')/=0 ) CALL read_error(1, 'dprst_depth_avg')

      ALLOCATE ( Dprst_flow_coef(Nhru) )
      IF ( declparam(MODNAME, 'dprst_flow_coef', 'nhru', 'real', &
     &     '0.05', '0.00001', '0.5', &
     &     'Coefficient in linear flow routing equation for open surface depressions', &
     &     'Coefficient in linear flow routing equation for open surface depressions for each HRU', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'dprst_flow_coef')

      ALLOCATE ( Dprst_seep_rate_open(Nhru) )
      IF ( declparam(MODNAME, 'dprst_seep_rate_open', 'nhru', 'real', &
     &     '0.02', '0.0', '0.2', &
     &     'Coefficient used in linear seepage flow equation for open surface depressions', &
     &     'Coefficient used in linear seepage flow equation for'// &
     &     ' open surface depressions for each HRU', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_open')

      ALLOCATE ( Dprst_seep_rate_clos(Nhru) )
      IF ( declparam(MODNAME, 'dprst_seep_rate_clos', 'nhru', 'real', &
     &     '0.02', '0.0', '0.2', &
     &     'Coefficient used in linear seepage flow equation for closed surface depressions', &
     &     'Coefficient used in linear seepage flow equation for'// &
     &     ' closed surface depressions for each HRU', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'dprst_seep_rate_clos')

      ALLOCATE ( Op_flow_thres(Nhru) )
      IF ( declparam(MODNAME, 'op_flow_thres', 'nhru', 'real', &
     &     '1.0', '0.01', '1.0', &
     &     'Fraction of open depression storage above which surface runoff occurs for each timestep', &
     &     'Fraction of open depression storage above'// &
     &     ' which surface runoff occurs; any water above'// &
     &     ' maximum open storage capacity spills as surface runoff', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'op_flow_thres')

      ALLOCATE ( Sro_to_dprst_perv(Nhru) )
      IF ( PRMS4_flag==1 .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(MODNAME, 'sro_to_dprst', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of pervious surface runoff that flows into surface-depression storage', &
     &       'Fraction of pervious surface runoff that'// &
     &       ' flows into surface-depression storage; the remainder'// &
     &       ' flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst')
      ENDIF
      IF ( PRMS4_flag==0 .OR. Model==DOCUMENTATION ) THEN
        IF ( declparam(MODNAME, 'sro_to_dprst_perv', 'nhru', 'real', &
     &       '0.2', '0.0', '1.0', &
     &       'Fraction of pervious surface runoff that flows into surface-depression storage', &
     &       'Fraction of pervious surface runoff that'// &
     &       ' flows into surface-depression storage; the remainder'// &
     &       ' flows to a stream network for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_perv')
      ENDIF

      ALLOCATE ( Sro_to_dprst_imperv(Nhru) )
      IF ( declparam(MODNAME, 'sro_to_dprst_imperv', 'nhru', 'real', &
     &     '0.2', '0.0', '1.0', &
     &     'Fraction of impervious surface runoff that flows into surface-depression storage', &
     &     'Fraction of impervious surface runoff that'// &
     &     ' flows into surface-depression storage; the remainder'// &
     &     ' flows to a stream network for each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'sro_to_dprst_imperv')

      ALLOCATE ( Dprst_et_coef(Nhru) )
      IF ( declparam(MODNAME, 'dprst_et_coef', 'nhru', 'real', &
     &     '1.0', '0.5', '1.5', &
     &     'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &     'Fraction of unsatisfied potential evapotranspiration to apply to surface-depression storage', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'dprst_et_coef')

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
        ALLOCATE ( Dprst_frac_init(Nhru) )
        IF ( declparam(MODNAME, 'dprst_frac_init', 'nhru', 'real', &
     &       '0.5', '0.0', '1.0', &
     &       'Fraction of maximum storage that contains water at the start of a simulation', &
     &       'Fraction of maximum surface-depression storage that'// &
     &       ' contains water at the start of a simulation', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'dprst_frac_init')
      ENDIF

      ALLOCATE ( Va_open_exp(Nhru) )
      IF ( declparam(MODNAME, 'va_open_exp', 'nhru', 'real', &
     &     '0.001', '0.0001', '10.0', &
     &     'Coefficient in the exponential equation to compute'// &
     &     ' current surface area of open surface-depression storage', &
     &     'Coefficient in the exponential equation relating'// &
     &     ' maximum surface area to the fraction that open'// &
     &     ' depressions are full to compute current surface area for each HRU;'// &
     &     ' 0.001 is an approximate cylinder; 1.0 is a cone', &
     &     'none')/=0 ) CALL read_error(1, 'va_open_exp')

      ALLOCATE ( Va_clos_exp(Nhru) )
      IF ( declparam(MODNAME, 'va_clos_exp', 'nhru', 'real', &
     &     '0.001', '0.0001', '10.0', &
     &     'Coefficient in the exponential equation to compute'// &
     &     ' current surface area of closed surface-depression storage', &
     &     'Coefficient in the exponential equation relating'// &
     &     ' maximum surface area to the fraction that closed'// &
     &     ' depressions are full to compute current surface area for each HRU;'// &
     &     ' 0.001 is an approximate cylinder; 1.0 is a cone', &
     &     'none')/=0 ) CALL read_error(1, 'va_clos_exp')

      IF ( Print_debug==1 ) ALLOCATE ( Dprst_stor_ante(Nhru) )

      END FUNCTION dprst_decl

!***********************************************************************
!     Initialize depression storage module - get parameter values
!***********************************************************************
      INTEGER FUNCTION dprst_init()
      USE PRMS_DPRST
      USE PRMS_MODULE, ONLY: Init_vars_from_file, Print_debug
      IMPLICIT NONE
!***********************************************************************
      dprst_init = 0

      IF ( Init_vars_from_file==0 ) THEN
        Basin_dprst_sroff = 0.0D0
        Basin_dprst_evap = 0.0D0
        Basin_dprst_seep = 0.0D0
        Basin_dprst_volop = 0.0D0
        Basin_dprst_volcl = 0.0D0
      ENDIF

      CALL dprst_initialize()

      IF ( Print_debug==1 ) Dprst_stor_ante = Dprst_stor_hru

    END FUNCTION dprst_init

!***********************************************************************
!     dprst_run - Computes surface depression storages and fluxes
!***********************************************************************
      INTEGER FUNCTION dprst_run()
      USE PRMS_DPRST
      USE PRMS_MODULE, ONLY: Print_debug, Frozen_flag, GSFLOW_flag, Kkiter, &
     &    Cascade_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, &
     &    Hru_imperv, Hru_percent_imperv, Active_area, Dprst_clos_flag, Dprst_open_flag, &
     &    Dprst_area_max, Hru_area, Hru_type, Basin_area_inv, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Hru_area_dble
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open, Dprst_vol_clos, Sroff
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_SNOW, ONLY: Snow_evap
      USE PRMS_INTCP, ONLY: Net_rain, Hru_intcpevap, Intcp_changeover
      USE PRMS_SRUNOFF, ONLY: Hru_impervevap, Frozen, Basin_hortonian, Basin_hortonian_lakes, Basin_sroff, &
     &    Basin_sroff_down, Basin_sroff_upslope, Hortonian_flow, Hortonian_lakes, Hru_hortn_cascflow
      IMPLICIT NONE
      INTRINSIC SNGL, DBLE
      EXTERNAL dprst_comp
! Local Variables
      INTEGER :: i, k, dprst_chk
      REAL :: avail_et, availh2o, srunoff
      DOUBLE PRECISION :: runoff, hru_sroff_down
!***********************************************************************
      dprst_run = 0

      IF ( GSFLOW_flag==1 ) THEN
        IF ( Kkiter==1 ) THEN
! It0 variables used with MODFLOW integration to save iteration states.
          IF ( Dprst_clos_flag==1 ) It0_dprst_vol_clos = Dprst_vol_clos
          IF ( Dprst_open_flag==1 ) It0_dprst_vol_open = Dprst_vol_open
          IF ( Print_debug==1 ) Dprst_stor_ante = Dprst_stor_hru
        ELSE
          IF ( Dprst_clos_flag==1 ) Dprst_vol_clos = It0_dprst_vol_clos
          IF ( Dprst_open_flag==1 ) Dprst_vol_open = It0_dprst_vol_open
        ENDIF
      ELSE
        IF ( Print_debug==1 ) Dprst_stor_ante = Dprst_stor_hru
      ENDIF

      IF ( Cascade_flag>0 ) THEN
        Basin_sroff_upslope = Basin_sroff_upslope*Active_area
        Basin_sroff_down = Basin_sroff_down*Active_area
        Basin_hortonian_lakes = Basin_hortonian_lakes*Active_area
        Basin_dprst_hortonian_lakes = 0.0D0
        Upslope_dprst_hortonian = 0.0D0
      ENDIF
      Basin_dprst_sroff = 0.0D0
      Basin_dprst_evap = 0.0D0
      Basin_dprst_seep = 0.0D0
      Basin_dprst_volop = 0.0D0
      Basin_dprst_volcl = 0.0D0
      Basin_sroff = Basin_sroff*Active_area
      dprst_chk = 0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)
        Hruarea = Hru_area(i)
        Hruarea_dble = Hru_area_dble(i)
        Ihru = i

        IF ( Hru_type(i)==2 ) THEN
! HRU is a lake
!     eventually add code for lake area less than hru_area
!     that includes soil_moist for fraction of hru_area that is dry bank
          IF ( Cascade_flag>0 ) THEN
            Hortonian_lakes(i) = Hortonian_lakes(i) + Upslope_dprst_hortonian(i)
            Basin_dprst_hortonian_lakes = Basin_dprst_hortonian_lakes + Upslope_dprst_hortonian(i)*Hruarea_dble
            Basin_hortonian_lakes = Basin_hortonian_lakes + Hortonian_lakes(i)*Hruarea_dble
          ENDIF
          CYCLE
        ENDIF

        Hruarea_imperv = Hru_imperv(i)
        IF ( Hruarea_imperv>0.0 ) THEN
          Imperv_frac = Hru_percent_imperv(i)
        ENDIF

        avail_et = Potet(i) - Snow_evap(i) - Hru_intcpevap(i) - Hru_impervevap(i)
        availh2o = Intcp_changeover(i) + Net_rain(i)

!******Compute runoff for depression storage area; skip if frozen
        Dprst_in(i) = 0.0D0
        dprst_chk = 0
        runoff = 0.0D0
        IF ( Dprst_area_max(i)>0.0 ) THEN
          Dprst_evap_hru(i) = 0.0
          Dprst_sroff_hru(i) = 0.0D0
          Dprst_seep_hru(i) = 0.0D0
          dprst_chk = 1
!         ******Compute the depression storage component
!         only call if total depression surface area for each HRU is > 0.0
          IF ( Frozen_flag==1 ) THEN
            IF ( Frozen(i)==0 ) THEN
              CALL dprst_comp(Dprst_vol_clos(i), Dprst_area_clos_max(i), Dprst_area_clos(i), &
     &                        Dprst_vol_open_max(i), Dprst_vol_open(i), Dprst_area_open_max(i), Dprst_area_open(i), &
     &                        Dprst_sroff_hru(i), Dprst_seep_hru(i), Sro_to_dprst_perv(i), Sro_to_dprst_imperv(i), &
     &                        Dprst_evap_hru(i), avail_et, availh2o, Dprst_in(i))
              runoff = Dprst_sroff_hru(i)*Hruarea_dble
              srunoff = SNGL( Dprst_sroff_hru(i) )

              !******Compute HRU weighted average (to units of inches/dt)
              IF ( Cascade_flag>0 ) THEN
                hru_sroff_down = 0.0D0
                IF ( Dprst_sroff_hru(i)>0.0 ) THEN
                  IF ( Ncascade_hru(i)>0 ) &
     &              CALL run_cascade_dprst(Ncascade_hru(i), srunoff, hru_sroff_down, Upslope_dprst_hortonian(i))
                  Hru_hortn_cascflow(i) = Hru_hortn_cascflow(i) + hru_sroff_down
                  Basin_sroff_upslope = Basin_sroff_upslope + Upslope_dprst_hortonian(i)*Hruarea_dble
                  Basin_sroff_down = Basin_sroff_down + hru_sroff_down*Hruarea_dble
                ELSE
                  Hru_hortn_cascflow(i) = 0.0D0
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
!       **********************************************************
        !!! call cascades, add Dunnian rsr ???

        Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hruarea_dble
        Sroff(i) = Sroff(i) + srunoff
        Hortonian_flow(i) = Hortonian_flow(i) + srunoff
        Basin_hortonian = Basin_hortonian + DBLE( Hortonian_flow(i)*Hruarea )
        Basin_sroff = Basin_sroff + DBLE( Sroff(i)*Hruarea )
      ENDDO

!******Compute basin weighted averages (to units of inches/dt)
      IF ( Cascade_flag>0 ) THEN
        Basin_hortonian_lakes = Basin_hortonian_lakes*Basin_area_inv
        Basin_sroff_down = Basin_sroff_down*Basin_area_inv
        Basin_sroff_upslope = Basin_sroff_upslope*Basin_area_inv
      ENDIF

      Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
      Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
      Basin_dprst_evap = Basin_dprst_evap*Basin_area_inv
      Basin_dprst_seep = Basin_dprst_seep*Basin_area_inv
      Basin_dprst_sroff = Basin_dprst_sroff*Basin_area_inv
      Basin_sroff = Basin_sroff*Basin_area_inv
      Basin_hortonian = Basin_hortonian*Basin_area_inv

      END FUNCTION dprst_run

!***********************************************************************
! Initialize depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_initialize()
      USE PRMS_DPRST
      USE PRMS_MODULE, ONLY: Init_vars_from_file, Nhru, PRMS4_flag, Inputerror_flag
      USE PRMS_BASIN, ONLY: Dprst_clos_flag, NEARZERO, Dprst_frac, &
     &    Dprst_area_clos_max, Dprst_area_open_max, Basin_area_inv, &
     &    Hru_area_dble, Active_hrus, Hru_route_order, Dprst_open_flag
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open, Dprst_vol_clos
      IMPLICIT NONE
! Functions
      INTRINSIC EXP, LOG, DBLE, SNGL
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: run_cascade_dprst
! Local Variables
      INTEGER :: i, j
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r
!***********************************************************************
      Dprst_evap_hru = 0.0
      Dprst_seep_hru = 0.0D0
      Dprst_sroff_hru = 0.0D0
      Dprst_insroff_hru = 0.0
      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
        IF ( getparam(MODNAME, 'dprst_frac_init', Nhru, 'real', Dprst_frac_init)/=0 ) CALL read_error(2, 'dprst_frac_init')
      ENDIF
      IF ( getparam(MODNAME, 'dprst_flow_coef', Nhru, 'real', Dprst_flow_coef)/=0 ) CALL read_error(2, 'dprst_flow_coef')
      IF ( Dprst_open_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_seep_rate_open', Nhru, 'real', Dprst_seep_rate_open)/=0 )  &
     &       CALL read_error(2, 'dprst_seep_rate_open')
        IF ( getparam(MODNAME, 'va_open_exp', Nhru, 'real', Va_open_exp)/=0 ) CALL read_error(2, 'va_open_exp')
        IF ( getparam(MODNAME, 'op_flow_thres', Nhru, 'real', Op_flow_thres)/=0 ) CALL read_error(2, 'op_flow_thres')
      ELSE
        Dprst_seep_rate_open = 0.0
        Va_open_exp = 0.0
        Op_flow_thres = 0.0
      ENDIF
      IF ( PRMS4_flag==1 ) THEN
        IF ( getparam(MODNAME, 'sro_to_dprst', Nhru, 'real', Sro_to_dprst_perv)/=0 ) CALL read_error(2, 'sro_to_dprst')
      ELSE
        IF ( getparam(MODNAME, 'sro_to_dprst_perv', Nhru, 'real', Sro_to_dprst_perv)/=0 ) CALL read_error(2, 'sro_to_dprst_perv')
      ENDIF
      IF ( getparam(MODNAME, 'sro_to_dprst_imperv', Nhru, 'real', Sro_to_dprst_imperv)/=0 ) &
     &     CALL read_error(2, 'sro_to_dprst_imperv')
      IF ( getparam(MODNAME, 'dprst_depth_avg', Nhru, 'real', Dprst_depth_avg)/=0 ) CALL read_error(2, 'dprst_depth_avg')
      IF ( getparam(MODNAME, 'dprst_et_coef', Nhru, 'real', Dprst_et_coef)/=0 ) CALL read_error(2, 'dprst_et_coef')
      IF ( Dprst_clos_flag==1 ) THEN
        IF ( getparam(MODNAME, 'dprst_seep_rate_clos', Nhru, 'real', Dprst_seep_rate_clos)/=0 ) &
     &       CALL read_error(2, 'dprst_seep_rate_clos')
        IF ( getparam(MODNAME, 'va_clos_exp', Nhru, 'real', Va_clos_exp)/=0 ) CALL read_error(2, 'va_clos_exp')
      ELSE
        Dprst_seep_rate_clos = 0.0
        Va_clos_exp = 0.0
      ENDIF
      Dprst_in = 0.0D0
      Dprst_area_open = 0.0
      Dprst_area_clos = 0.0
      Dprst_stor_hru = 0.0D0
      Upslope_dprst_hortonian = 0.0D0
      Dprst_vol_thres_open = 0.0D0
      Dprst_vol_open_max = 0.0D0
      Dprst_vol_clos_max = 0.0D0
      Dprst_vol_frac = 0.0
      Dprst_vol_open_frac = 0.0
      Dprst_vol_clos_frac = 0.0
      Basin_dprst_volop = 0.0D0
      Basin_dprst_volcl = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)

        IF ( Dprst_frac(i)>0.0 ) THEN
          IF ( Dprst_depth_avg(i)==0.0 ) THEN
            PRINT *, 'ERROR, dprst_frac>0 and dprst_depth_avg==0 for HRU:', i, '; dprst_frac:', Dprst_frac(i)
            Inputerror_flag = 1
            CYCLE
          ENDIF
!         calculate open and closed volumes (acre-inches) of depression storage by HRU
!         Dprst_area_open_max is the maximum open depression area (acres) that can generate surface runoff:
          IF ( Dprst_clos_flag==1 ) Dprst_vol_clos_max(i) = DBLE( Dprst_area_clos_max(i)*Dprst_depth_avg(i) )
          IF ( Dprst_open_flag==1 ) Dprst_vol_open_max(i) = DBLE( Dprst_area_open_max(i)*Dprst_depth_avg(i) )

!         calculate the initial open and closed depression storage volume:
          IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) THEN
            IF ( Dprst_open_flag==1 ) Dprst_vol_open(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_open_max(i)
            IF ( Dprst_clos_flag==1 ) Dprst_vol_clos(i) = DBLE(Dprst_frac_init(i))*Dprst_vol_clos_max(i)
          ENDIF

!         threshold volume is calculated as the % of maximum open
!         depression storage above which flow occurs *  total open depression storage volume
          Dprst_vol_thres_open(i) = DBLE(Op_flow_thres(i))*Dprst_vol_open_max(i)

!         initial open and closed storage volume as fraction of total open and closed storage volume

!         Open depression surface area for each HRU:
          IF ( Dprst_vol_open(i)>0.0D0 ) THEN
            open_vol_r = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
            IF ( open_vol_r<NEARZERO ) THEN
              frac_op_ar = 0.0
            ELSEIF ( open_vol_r>1.0 ) THEN
              frac_op_ar = 1.0
            ELSE
              frac_op_ar = EXP(Va_open_exp(i)*LOG(open_vol_r))
            ENDIF
            Dprst_area_open(i) = Dprst_area_open_max(i)*frac_op_ar
            IF ( Dprst_area_open(i)>Dprst_area_open_max(i) ) Dprst_area_open(i) = Dprst_area_open_max(i)
!            IF ( Dprst_area_open(i)<NEARZERO ) Dprst_area_open(i) = 0.0
          ENDIF

!         Closed depression surface area for each HRU:
          IF ( Dprst_vol_clos(i)>0.0D0 ) THEN
            clos_vol_r = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
            IF ( clos_vol_r<NEARZERO ) THEN
              frac_cl_ar = 0.0
            ELSEIF ( clos_vol_r>1.0 ) THEN
              frac_cl_ar = 1.0
            ELSE
              frac_cl_ar = EXP(Va_clos_exp(i)*LOG(clos_vol_r))
            ENDIF
            Dprst_area_clos(i) = Dprst_area_clos_max(i)*frac_cl_ar
            IF ( Dprst_area_clos(i)>Dprst_area_clos_max(i) ) Dprst_area_clos(i) = Dprst_area_clos_max(i)
!            IF ( Dprst_area_clos(i)<NEARZERO ) Dprst_area_clos(i) = 0.0
          ENDIF

!         calculate the basin open and closed depression storage volumes
          Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open(i)
          Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos(i)
          Dprst_stor_hru(i) = (Dprst_vol_open(i)+Dprst_vol_clos(i))/Hru_area_dble(i)
          IF ( Dprst_vol_open_max(i)>0.0 ) Dprst_vol_open_frac(i) = SNGL( Dprst_vol_open(i)/Dprst_vol_open_max(i) )
          IF ( Dprst_vol_clos_max(i)>0.0 ) Dprst_vol_clos_frac(i) = SNGL( Dprst_vol_clos(i)/Dprst_vol_clos_max(i) )
          Dprst_vol_frac(i) = SNGL( (Dprst_vol_open(i)+Dprst_vol_clos(i))/(Dprst_vol_open_max(i)+Dprst_vol_clos_max(i)) )
        ENDIF
      ENDDO
      Basin_dprst_volop = Basin_dprst_volop*Basin_area_inv
      Basin_dprst_volcl = Basin_dprst_volcl*Basin_area_inv
      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==7 ) DEALLOCATE ( Dprst_frac_init )

      END SUBROUTINE dprst_initialize

!***********************************************************************
!     Compute depression storage area hydrology
!***********************************************************************
      SUBROUTINE dprst_comp(Dprst_vol_clos, Dprst_area_clos_max, Dprst_area_clos, &
     &           Dprst_vol_open_max, Dprst_vol_open, Dprst_area_open_max, Dprst_area_open, &
     &           Dprst_sroff_hru, Dprst_seep_hru, Sro_to_dprst_perv, Sro_to_dprst_imperv, Dprst_evap_hru, &
     &           Avail_et, Net_rain, Dprst_in)
      USE PRMS_DPRST, ONLY: Hruarea, Hruarea_dble, Dprst_et_coef, &
     &    Dprst_seep_rate_open, Dprst_seep_rate_clos, Va_clos_exp, Va_open_exp, Dprst_flow_coef, &
     &    Dprst_vol_thres_open, Dprst_vol_clos_max, Dprst_insroff_hru, &
     &    Basin_dprst_volop, Basin_dprst_volcl, Basin_dprst_evap, Basin_dprst_seep, Basin_dprst_sroff, &
     &    Dprst_vol_open_frac, Dprst_vol_clos_frac, Dprst_vol_frac, Dprst_stor_hru, Hruarea_dble
      USE PRMS_SRUNOFF, ONLY: Srp, Sri, Ihru, Perv_frac, Imperv_frac
      USE PRMS_BASIN, ONLY: NEARZERO, DNEARZERO, Dprst_frac_open, Dprst_frac_clos
      USE PRMS_INTCP, ONLY: Net_snow
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: Pkwater_equiv
      USE PRMS_SNOW, ONLY: Snowmelt, Pptmix_nopack, Snowcov_area
      IMPLICIT NONE
      INTRINSIC EXP, LOG, MAX, DBLE, SNGL
! Arguments
      REAL, INTENT(IN) :: Dprst_area_open_max, Dprst_area_clos_max, Net_rain, Avail_et
      REAL, INTENT(IN) :: Sro_to_dprst_perv, Sro_to_dprst_imperv
      DOUBLE PRECISION, INTENT(IN) :: Dprst_vol_open_max
      DOUBLE PRECISION, INTENT(INOUT) :: Dprst_vol_open, Dprst_vol_clos, Dprst_in
      REAL, INTENT(OUT) :: Dprst_area_open, Dprst_area_clos, Dprst_evap_hru
      DOUBLE PRECISION, INTENT(OUT) :: Dprst_sroff_hru, Dprst_seep_hru
! Local Variables
      REAL :: inflow, dprst_avail_et, dprst_srp, dprst_sri
      REAL :: dprst_srp_open, dprst_srp_clos, dprst_sri_open, dprst_sri_clos
      REAL :: frac_op_ar, frac_cl_ar, open_vol_r, clos_vol_r, unsatisfied_et
      REAL :: tmp, dprst_evap_open, dprst_evap_clos
      DOUBLE PRECISION :: seep_open, seep_clos, tmp1
!***********************************************************************
      inflow = 0.0
      IF ( Pptmix_nopack(Ihru)==1 ) inflow = Net_rain

!******If precipitation on snowpack all water available to the surface is considered to be snowmelt
!******If there is no snowpack and no precip,then check for melt from last of snowpack.
!******If rain/snow mix with no antecedent snowpack, compute snowmelt portion of runoff.

      IF ( Snowmelt(Ihru)>0.0 ) THEN
        inflow = inflow + Snowmelt(Ihru)

!******There was no snowmelt but a snowpack may exist.  If there is
!******no snowpack then check for rain on a snowfree HRU.
      ELSEIF ( Pkwater_equiv(Ihru)<DNEARZERO ) THEN

!      If no snowmelt and no snowpack but there was net snow then
!      snowpack was small and was lost to sublimation.
        IF ( Net_snow(Ihru)<NEARZERO .AND. Net_rain>0.0 ) THEN
          inflow = inflow + Net_rain
        ENDIF
      ENDIF

      Dprst_in = 0.0D0
      IF ( Dprst_area_open_max>0.0 ) THEN
        Dprst_in = DBLE( inflow*Dprst_area_open_max ) ! inch-acres
        Dprst_vol_open = Dprst_vol_open + Dprst_in
      ENDIF
      IF ( Dprst_area_clos_max>0.0 ) THEN
        tmp1 = DBLE( inflow*Dprst_area_clos_max ) ! inch-acres
        Dprst_vol_clos = Dprst_vol_clos + tmp1
        Dprst_in = Dprst_in + tmp1
      ENDIF
      Dprst_in = Dprst_in/Hruarea_dble ! inches over HRU

      ! add any pervious surface runoff fraction to depressions
      dprst_srp = 0.0
      dprst_sri = 0.0
      IF ( Srp>0.0 ) THEN
        tmp = Srp*Perv_frac*Sro_to_dprst_perv*Hruarea
        IF ( Dprst_area_open_max>0.0 ) THEN
          dprst_srp_open = tmp*Dprst_frac_open(Ihru) ! acre-inches
          dprst_srp = dprst_srp_open/Hruarea
          Dprst_vol_open = Dprst_vol_open + DBLE( dprst_srp_open )
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          dprst_srp_clos = tmp*Dprst_frac_clos(Ihru)
          dprst_srp = dprst_srp + dprst_srp_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + DBLE( dprst_srp_clos )
        ENDIF
        Srp = Srp - dprst_srp/Perv_frac
        IF ( Srp<0.0 ) THEN
          IF ( Srp<-NEARZERO ) PRINT *, 'dprst srp<0.0', Srp, dprst_srp
          ! may need to adjust dprst_srp and volumes
          Srp = 0.0
        ENDIF
      ENDIF

      IF ( Sri>0.0 ) THEN
        tmp = Sri*Imperv_frac*Sro_to_dprst_imperv*Hruarea
        IF ( Dprst_area_open_max>0.0 ) THEN
          dprst_sri_open = tmp*Dprst_frac_open(Ihru)
          dprst_sri = dprst_sri_open/Hruarea
          Dprst_vol_open = Dprst_vol_open + DBLE( dprst_sri_open )
        ENDIF
        IF ( Dprst_area_clos_max>0.0 ) THEN
          dprst_sri_clos = tmp*Dprst_frac_clos(Ihru)
          dprst_sri = dprst_sri + dprst_sri_clos/Hruarea
          Dprst_vol_clos = Dprst_vol_clos + DBLE( dprst_sri_clos )
        ENDIF
        Sri = Sri - dprst_sri/Imperv_frac
        IF ( Sri<0.0 ) THEN
          IF ( Sri<-NEARZERO ) PRINT *, 'dprst sri<0.0', Sri, dprst_sri
          ! may need to adjust dprst_sri and volumes
          Sri = 0.0
        ENDIF
      ENDIF

      Dprst_insroff_hru(Ihru) = dprst_srp + dprst_sri

!     Open depression surface area for each HRU:
      Dprst_area_open = 0.0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        open_vol_r = SNGL( Dprst_vol_open/Dprst_vol_open_max )
        IF ( open_vol_r<NEARZERO ) THEN
          frac_op_ar = 0.0
        ELSEIF ( open_vol_r>1.0 ) THEN
          frac_op_ar = 1.0
        ELSE
          frac_op_ar = EXP(Va_open_exp(Ihru)*LOG(open_vol_r))
        ENDIF
        Dprst_area_open = Dprst_area_open_max*frac_op_ar
        IF ( Dprst_area_open>Dprst_area_open_max ) Dprst_area_open = Dprst_area_open_max
!        IF ( Dprst_area_open<NEARZERO ) Dprst_area_open = 0.0
      ENDIF

!     Closed depression surface area for each HRU:
      IF ( Dprst_area_clos_max>0.0 ) THEN
        Dprst_area_clos = 0.0
        IF ( Dprst_vol_clos>0.0D0 ) THEN
          clos_vol_r = SNGL( Dprst_vol_clos/Dprst_vol_clos_max(Ihru) )
          IF ( clos_vol_r<NEARZERO ) THEN
            frac_cl_ar = 0.0
          ELSEIF ( clos_vol_r>1.0 ) THEN
            frac_cl_ar = 1.0
          ELSE
            frac_cl_ar = EXP(Va_clos_exp(Ihru)*LOG(clos_vol_r))
          ENDIF
          Dprst_area_clos = Dprst_area_clos_max*frac_cl_ar
          IF ( Dprst_area_clos>Dprst_area_clos_max ) Dprst_area_clos = Dprst_area_clos_max
!          IF ( Dprst_area_clos<NEARZERO ) Dprst_area_clos = 0.0
        ENDIF
      ENDIF

      ! evaporate water from depressions based on snowcov_area
      ! dprst_evap_open & dprst_evap_clos = inches-acres on the HRU
      unsatisfied_et = Avail_et
      dprst_avail_et = (Potet(Ihru)*(1.0-Snowcov_area(Ihru)))*Dprst_et_coef(Ihru)
      Dprst_evap_hru = 0.0
      IF ( dprst_avail_et>0.0 ) THEN
        dprst_evap_open = 0.0
        dprst_evap_clos = 0.0
        IF ( Dprst_area_open>0.0 ) THEN
          dprst_evap_open = MIN(Dprst_area_open*dprst_avail_et, SNGL(Dprst_vol_open))
          IF ( dprst_evap_open/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
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
          Dprst_vol_open = Dprst_vol_open - DBLE( dprst_evap_open )
        ENDIF
        IF ( Dprst_area_clos>0.0 ) THEN
          dprst_evap_clos = MIN(Dprst_area_clos*dprst_avail_et, SNGL(Dprst_vol_clos))
          IF ( dprst_evap_clos/Hruarea>unsatisfied_et ) THEN
            !IF ( Print_debug>-1 ) THEN
            !  PRINT *, 'Warning, closed dprst evaporation > available ET, HRU:, ', Ihru, &
!      &                 unsatisfied_et, dprst_evap_clos*Dprst_frac_clos(Ihru)
            !  PRINT *, 'Set to available ET, perhaps dprst_et_coef specified too large'
            !  PRINT *, 'Set print_debug to -1 to turn off message'
            !ENDIF
            dprst_evap_clos = unsatisfied_et*Hruarea
          ENDIF
          IF ( dprst_evap_clos>SNGL(Dprst_vol_clos) ) dprst_evap_clos = SNGL( Dprst_vol_clos )
          Dprst_vol_clos = Dprst_vol_clos - DBLE( dprst_evap_clos )
        ENDIF
        Dprst_evap_hru = (dprst_evap_open + dprst_evap_clos)/Hruarea
      ENDIF

      ! compute seepage
      Dprst_seep_hru = 0.0D0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        seep_open = Dprst_vol_open*DBLE( Dprst_seep_rate_open(Ihru) )
        Dprst_vol_open = Dprst_vol_open - seep_open
        IF ( Dprst_vol_open<0.0D0 ) THEN
!          IF ( Dprst_vol_open<-DNEARZERO ) PRINT *, 'negative dprst_vol_open:', Dprst_vol_open, ' HRU:', Ihru
          seep_open = seep_open + Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
        Dprst_seep_hru = seep_open/Hruarea_dble
      ENDIF

      ! compute open surface runoff
      Dprst_sroff_hru = 0.0D0
      IF ( Dprst_vol_open>0.0D0 ) THEN
        Dprst_sroff_hru = MAX( 0.0D0, Dprst_vol_open-Dprst_vol_open_max )
        Dprst_sroff_hru = Dprst_sroff_hru + &
     &                    MAX( 0.0D0, (Dprst_vol_open-Dprst_sroff_hru-Dprst_vol_thres_open(Ihru))*DBLE(Dprst_flow_coef(Ihru)) )
        Dprst_vol_open = Dprst_vol_open - Dprst_sroff_hru
        Dprst_sroff_hru = Dprst_sroff_hru/Hruarea_dble
        ! sanity checks
        IF ( Dprst_vol_open<0.0D0 ) THEN
!          IF ( Dprst_vol_open<-DNEARZERO ) PRINT *, 'issue, dprst_vol_open<0.0', Dprst_vol_open
          Dprst_vol_open = 0.0D0
        ENDIF
      ENDIF

      IF ( Dprst_area_clos_max>0.0 ) THEN
        IF ( Dprst_area_clos>NEARZERO ) THEN
          seep_clos = Dprst_vol_clos*DBLE( Dprst_seep_rate_clos(Ihru) )
          Dprst_vol_clos = Dprst_vol_clos - seep_clos
          IF ( Dprst_vol_clos<0.0D0 ) THEN
!            IF ( Dprst_vol_clos<-DNEARZERO ) PRINT *, 'issue, dprst_vol_clos<0.0', Dprst_vol_clos
            seep_clos = seep_clos + Dprst_vol_clos
            Dprst_vol_clos = 0.0D0
          ENDIF
          Dprst_seep_hru = Dprst_seep_hru + seep_clos/Hruarea_dble
        ENDIF
        IF ( Dprst_vol_clos<0.0D0 ) THEN
!          IF ( Dprst_vol_clos<-DNEARZERO ) PRINT *, 'issue, dprst_vol_clos<0.0', Dprst_vol_clos
          Dprst_vol_clos = 0.0D0
        ENDIF
      ENDIF

      Basin_dprst_volop = Basin_dprst_volop + Dprst_vol_open
      Basin_dprst_volcl = Basin_dprst_volcl + Dprst_vol_clos
      Basin_dprst_evap = Basin_dprst_evap + DBLE( Dprst_evap_hru )*Hruarea_dble
      Basin_dprst_seep = Basin_dprst_seep + Dprst_seep_hru*Hruarea_dble
      Basin_dprst_sroff = Basin_dprst_sroff + Dprst_sroff_hru*Hruarea_dble
      IF ( Dprst_vol_open_max>0.0 ) Dprst_vol_open_frac(Ihru) = SNGL( Dprst_vol_open/Dprst_vol_open_max )
      IF ( Dprst_vol_clos_max(Ihru)>0.0 ) Dprst_vol_clos_frac(Ihru) = SNGL( Dprst_vol_clos/Dprst_vol_clos_max(Ihru) )
      Dprst_vol_frac(Ihru) = SNGL( (Dprst_vol_open+Dprst_vol_clos)/(Dprst_vol_open_max+Dprst_vol_clos_max(Ihru)) )
      Dprst_stor_hru(Ihru) = (Dprst_vol_open+Dprst_vol_clos)/Hruarea_dble

      END SUBROUTINE dprst_comp

!***********************************************************************
!     Compute cascading runoff (runoff in inche*acre/dt)
!***********************************************************************
      SUBROUTINE run_cascade_dprst(Ncascade_hru, Runoff, Hru_sroff_down, Upslope_dprst_hortonian)
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Ihru, Strm_seg_in
      USE PRMS_CASCADE, ONLY: Hru_down, Hru_down_frac, Hru_down_fracwt, Cascade_area
      IMPLICIT NONE
! Functions
      INTRINSIC IABS, ABS, DBLE
! Arguments
      INTEGER, INTENT(IN) :: Ncascade_hru
      REAL, INTENT(INOUT) :: Runoff
      DOUBLE PRECISION, INTENT(INOUT) :: Hru_sroff_down, Upslope_dprst_hortonian
! Local Variables
      INTEGER :: j, k
!***********************************************************************
      DO k = 1, Ncascade_hru
        j = Hru_down(k, Ihru)
! if hru_down(k, Ihru) > 0, cascade contributes to a downslope HRU
        IF ( j>0 ) THEN
          Upslope_dprst_hortonian = Upslope_dprst_hortonian + DBLE( Runoff*Hru_down_fracwt(k, Ihru) )
          Hru_sroff_down = Hru_sroff_down + DBLE( Runoff*Hru_down_frac(k,Ihru) )

! if hru_down(k, Ihru) < 0, cascade contributes to a stream
        ELSEIF ( j<0 ) THEN
          j = IABS( j )
          Strm_seg_in(j) = Strm_seg_in(j) + DBLE( Runoff*Cascade_area(k, Ihru) )*Cfs_conv
        ENDIF
      ENDDO

! reset Sroff as it accumulates flow to streams
      Runoff = Runoff - SNGL( Hru_sroff_down )

    END SUBROUTINE run_cascade_dprst

!***********************************************************************
!     dprst_restart - write or read depresion storage restart file
!***********************************************************************
      SUBROUTINE dprst_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_DPRST
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
     &                            Basin_dprst_volop, Basin_dprst_volcl
        WRITE ( Restart_outunit ) Dprst_area_open
        WRITE ( Restart_outunit ) Dprst_area_clos
        WRITE ( Restart_outunit ) Dprst_stor_hru
        WRITE ( Restart_outunit ) Dprst_vol_thres_open
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_dprst_sroff, Basin_dprst_evap, Basin_dprst_seep, &
     &                          Basin_dprst_volop, Basin_dprst_volcl
        READ ( Restart_inunit ) Dprst_area_open
        READ ( Restart_inunit ) Dprst_area_clos
        READ ( Restart_inunit ) Dprst_stor_hru
        READ ( Restart_inunit ) Dprst_vol_thres_open
      ENDIF
      END SUBROUTINE dprst_restart

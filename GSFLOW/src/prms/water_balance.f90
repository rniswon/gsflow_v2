!***********************************************************************
! Computes water balance for components of a PRMS model
!***********************************************************************
      MODULE PRMS_WATER_BALANCE
        IMPLICIT NONE
!   Local Variables
        character(len=*), parameter :: MODDESC = 'Water Balance Computations'
        character(len=*), parameter :: MODNAME_WB = 'water_balance'
        character(len=*), parameter :: Version_water_balance = '2024-12-15'
        INTEGER, SAVE :: BALUNT, SZUNIT, GWUNIT, INTCPUNT, SROUNIT, SNOWUNIT
        REAL, PARAMETER :: TOOSMALL = 3.1E-05, SMALL = 1.0E-04, BAD = 1.0E-03
        DOUBLE PRECISION, PARAMETER :: DSMALL = 1.0D-04, DTOOSMALL = 1.0D-05
        DOUBLE PRECISION, SAVE :: Last_basin_gwstor, Basin_dprst_wb
        DOUBLE PRECISION, ALLOCATABLE, SAVE :: Hru_storage_ante(:), Gwstor_ante(:)
!   Declared Variables
        DOUBLE PRECISION, SAVE :: Basin_capillary_wb, Basin_gravity_wb, Basin_soilzone_wb
!        DOUBLE PRECISION, ALLOCATABLE, SAVE :: Hru_runoff(:)
      END MODULE PRMS_WATER_BALANCE

!***********************************************************************
!***********************************************************************
      SUBROUTINE water_balance()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, CLEAN
      USE PRMS_MODULE, ONLY: Process_flag
      USE PRMS_WATER_BALANCE
      IMPLICIT NONE
! Functions
      EXTERNAL :: water_balance_decl, water_balance_init, water_balance_run
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        CALL water_balance_run()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL water_balance_decl()
      ELSEIF ( Process_flag==INIT ) THEN
        CALL water_balance_init()
      ELSEIF ( Process_flag==CLEAN ) THEN
        CLOSE ( SZUNIT )
        CLOSE ( BALUNT )
        CLOSE ( INTCPUNT )
        CLOSE ( GWUNIT )
        CLOSE ( SROUNIT )
        CLOSE ( SNOWUNIT )
      ENDIF

      END SUBROUTINE water_balance

!***********************************************************************
!***********************************************************************
      SUBROUTINE water_balance_decl()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, CASCADE_OFF
      use PRMS_MMFAPI, only: declvar_dble
      USE PRMS_MODULE, ONLY: Nhru, Cascade_flag, Dprst_flag
      USE PRMS_WATER_BALANCE
      USE PRMS_SRUNOFF, ONLY: MODNAME
      use prms_utils, only: print_module, PRMS_open_module_file, read_error
      IMPLICIT NONE
!***********************************************************************
      CALL print_module(MODDESC, MODNAME_WB, Version_water_balance)

! Declare Variables
      CALL declvar_dble(MODNAME_WB, 'basin_capillary_wb', 'one', 1, &
     &     'Basin area-weighted average capillary reservoir storage', &
     &     'inches', Basin_capillary_wb)

      CALL declvar_dble(MODNAME_WB, 'basin_gravity_wb', 'one', 1, &
     &     'Basin area-weighted average gravity reservoir storage', &
     &     'inches', Basin_gravity_wb)

      CALL declvar_dble(MODNAME_WB, 'basin_soilzone_wb', 'one', 1, &
     &     'Basin area-weighted average storage in soilzone reservoirs', &
     &     'inches', Basin_soilzone_wb)

!      ALLOCATE ( Hru_runoff(Nhru) )
!      CALL declvar_dble(MODNAME, 'hru_runoff', 'nhru', Nhru, &
!     &     'Total lateral flow leaving each HRU (includes cascading flow)', &
!     &     'inches', Hru_runoff)

      ALLOCATE ( Hru_storage_ante(Nhru) )

      IF ( Dprst_flag==ACTIVE ) THEN
        CALL declvar_dble(MODNAME_WB, 'basin_dprst_wb', 'one', 1, &
     &       'Basin area-weighted average surface-depression storage water balance', &
     &       'inches', Basin_dprst_wb)
      ENDIF

      ALLOCATE ( Gwstor_ante(Nhru) )

      CALL PRMS_open_module_file(INTCPUNT, 'intcp.wbal')
      WRITE ( INTCPUNT, 9003 )

      CALL PRMS_open_module_file(SNOWUNIT, 'snowcomp.wbal')
      WRITE ( SNOWUNIT, 9007 )

      CALL PRMS_open_module_file(SROUNIT, MODNAME//'.wbal')
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        WRITE ( SROUNIT, 9006 )
      ELSE
        WRITE ( SROUNIT, 9005 )
      ENDIF

      CALL PRMS_open_module_file(SZUNIT, 'soilzone.wbal')
      WRITE ( SZUNIT, 9001 )

      CALL PRMS_open_module_file(GWUNIT, 'gwflow.wbal')
      WRITE ( GWUNIT, 9004 )

      CALL PRMS_open_module_file(BALUNT, 'wbal.msgs')

 9001 FORMAT ('    Date     Water Bal     bsmbal    last SM  soilmoist  last stor    SS stor    perv ET      sz2gw  interflow', &
     &        '    soil2gw    dunnian    soil in   lakeinsz   downflow   swale ET  pref flow   pfr dunn   pfr stor', &
     &        '  slow stor dunnian gvr lake evap')
 9003 FORMAT ('    Date     Water Bal     Precip     Netppt  Intcpevap  Intcpstor  last_stor changeover  net apply     apply')
 9004 FORMAT ('    Date     Water Bal last store  GWR store', &
              '   GW input    GW flow    GW sink GW upslope minarea_in   downflow')
 9005 FORMAT ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
     &        '   Perv Sro Imperv Sro  Dprst Sro  Dunnian   Dprst WB   CFGI Sro')
 9006 FORMAT ('    Date     Water Bal     Robal      Sroff      Infil  Impervevap Impervstor Dprst_evap Dprst_seep', &
     &        '   Perv Sro Imperv Sro  Dprst Sro  Dunnian   Dprst WB  Sroffdown  Srofflake   CFGI Sro')
 9007 FORMAT ('    Date     Water Bal  Snowpack    Snowmelt   Snowevap  Snowcover' )

      END SUBROUTINE water_balance_decl

!***********************************************************************
!     szinit - Initialize soilzone module - get parameter values,
!              set initial values and check parameter values
!***********************************************************************
      SUBROUTINE water_balance_init()
      USE PRMS_WATER_BALANCE
      USE PRMS_FLOWVARS, ONLY: Gwres_stor
      USE PRMS_GWFLOW, ONLY: Basin_gwstor
      USE PRMS_BASIN, ONLY: Hru_storage
!***********************************************************************
!      Hru_runoff = 0.0D0
      Last_basin_gwstor = Basin_gwstor
      Gwstor_ante = Gwres_stor
      Hru_storage_ante = Hru_storage

      END SUBROUTINE water_balance_init

!***********************************************************************
!     water_balance_run - Computes balance for each HRU and model domain
!***********************************************************************
      SUBROUTINE water_balance_run()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, LAKE, CASCADE_OFF, CASCADEGW_OFF, OFF
      USE PRMS_MODULE, ONLY: Cascade_flag, Cascadegw_flag, Dprst_flag, Glacier_flag, Nowyear, Nowmonth, Nowday, &
     &    Hru_type, AG_flag
      USE PRMS_WATER_BALANCE
      USE PRMS_BASIN, ONLY: Hru_route_order, Active_hrus, Hru_frac_perv, Hru_area_dble, Hru_perv, &
     &    Basin_area_inv, Dprst_area_max, Hru_frac_imperv, Hru_frac_dprst, Cov_type, Hru_storage, &
     &    Covden_win, Covden_sum, Hru_area, Snowpack_threshold, Ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt, Basin_ppt, Hru_rain, Hru_snow, Pptmix, Potet
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor, Soil_to_gw, Soil_to_ssr, &
     &    Infil, Soil_moist_max, Ssr_to_gw, Ssres_flow, Basin_soil_to_gw, Soil_moist, Ssres_stor, Pref_flow_stor, &
     &    Slow_flow, Basin_perv_et, Basin_ssflow, Basin_swale_et, Slow_stor, Ssres_in, Soil_rechr, &
     &    Basin_lakeevap, Sroff, Hru_actet, Pkwater_equiv, Gwres_stor, Dprst_vol_open, Dprst_vol_clos, Basin_sroff, &
     &    Dprst_stor_hru, Hru_impervstor, Hru_intcpstor, Gw_upslope, &
     &    Intcp_stor, Ag_soil_moist, Basin_ag_soil_moist, Snowmelt, Snow_evap, Snowcov_area, Pptmix_nopack, Basin_pweqv
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_ssres_stor, It0_basin_soil_moist, &
                               It0_ag_soil_moist, It0_basin_ag_soil_moist, It0_hru_intcpstor, &
                               It0_basin_ssstor, It0_dprst_stor_hru, It0_hru_impervstor, It0_pkwater_equiv
      USE PRMS_SET_TIME, ONLY: Nowtime
      USE PRMS_CASCADE, ONLY: Ncascade_hru
      USE PRMS_INTCP, ONLY: Basin_net_ppt, Basin_intcp_evap, Basin_changeover, &
     &    Basin_intcp_stor, Net_rain, Net_snow, Hru_intcpevap, &
     &    Srain_intcp, Wrain_intcp, Snow_intcp, Intcp_evap, &
     &    Canopy_covden, Intcp_changeover, Net_ppt, Last_basin_intcp_stor, &
     &    Net_apply, Gain_inches, Use_transfer_intcp, Basin_hru_apply, Basin_net_apply, Basin_net_snow, Basin_net_rain
      USE PRMS_SNOW, ONLY: Basin_snowmelt, Basin_snowevap, Basin_snowcov, Pk_precip, Glacrb_melt
!      USE PRMS_GLACR, ONLY: Glacr_flow
      USE PRMS_SRUNOFF, ONLY: Basin_infil, Hru_hortn_cascflow, Upslope_hortonian, &
     &    Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, Basin_sroff_down, &
     &    Basin_hortonian_lakes, Basin_imperv_evap, Basin_imperv_stor, Infil_ag, &
     &    Basin_dprst_evap, Basin_dprst_seep, Hru_impervevap, Dprst_seep_hru, &
     &    Dprst_evap_hru, Dprst_sroff_hru, Dprst_insroff_hru, Sro_to_dprst_perv, &
     &    Dprst_area_clos, Hortonian_flow, Dprst_in, Hru_sroffp, Hru_sroffi, Basin_cfgi_sroff
      USE PRMS_SOILZONE, ONLY: Swale_actet, Dunnian_flow, Basin_sz2gw, Pref_flow_den, &
     &    Perv_actet, Cap_infil_tot, Pref_flow_infil, Cap_waterin, Upslope_interflow, &
     &    Upslope_dunnianflow, Pref_flow, Soil_lower, Gvr2pfr, Basin_ssin, &
     &    Basin_lakeinsz, Basin_dunnian, Pref_flow_max, Pref_flow_thrsh, &
     &    Basin_sm2gvr_max, Basin_cap_infil_tot, Basin_slowflow, &
     &    Basin_dunnian_gvr, Basin_pref_flow_infil, Basin_dninterflow, Basin_pref_stor, Basin_dunnian_pfr, &
     &    Basin_dncascadeflow, Basin_capwaterin, Basin_sm2gvr, Basin_prefflow, Basin_slstor, Basin_gvr2pfr, &
     &    Hru_sz_cascadeflow, Pfr_dunnian_flow, Grav_dunnian_flow, Basin_dndunnianflow, &
     &    Soil_moist_tot, Pref_flag
      USE PRMS_SOILZONE_AG, ONLY: Ag_cap_infil_tot, hru_ag_actet, perv_soil_to_gvr, perv_soil_to_gw, &
     &    Basin_perv_to_gw, Basin_ag_actet
      USE PRMS_GWFLOW, ONLY: Basin_dnflow, Basin_gwsink, Basin_gwstor_minarea_wb, Gwres_flow, &
     &    Basin_gwstor, Basin_gwflow, Basin_gw_upslope, Basin_gwin, Gwres_sink, Hru_gw_cascadeflow, &
     &    Gwminarea_flag, Gwstor_minarea_wb, Gwin_dprst, Gwres_in
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS, DBLE, SNGL, DABS
! Local Variables
      INTEGER :: i, k
      REAL :: last_sm, last_ss, soilbal, perv_frac, gvrbal, test, waterin, waterout, hrubal, delta_stor_sngl
      REAL :: delstor, robal, gmelt, harea
      DOUBLE PRECISION :: basin_bal, bsmbal, soil_in, gwbal, gwup, basin_robal, bsnobal
      DOUBLE PRECISION :: hru_out, hru_in, wbal, delta_stor, pptbal, brobal, dprst_hru_wb, harea_dble
      DOUBLE PRECISION :: dprst_stor
      CHARACTER(LEN=*), PARAMETER :: fmt1 = '(A, I5, 2("/",I2.2))'
!***********************************************************************
      Basin_capillary_wb = 0.0D0
      Basin_gravity_wb = 0.0D0
      basin_soilzone_wb = 0.0D0
      basin_bal = 0.0D0
      soil_in = 0.0D0
      basin_robal = 0.0D0
      bsnobal = 0.0D0
      Basin_dprst_wb = 0.0D0
!      Hru_runoff = 0.0D0
      DO k = 1, Active_hrus
        i = Hru_route_order(k)

        IF ( Hru_type(i)==LAKE ) CYCLE ! no water balance for lakes

        harea = Hru_area(i)
        harea_dble = Hru_area_dble(i)
        perv_frac = Hru_frac_perv(i)

        ! intcp
        delstor = Hru_intcpstor(i) - It0_hru_intcpstor(i)
        hrubal = Hru_rain(i) + Hru_snow(i) - Net_rain(i) - Net_snow(i) &
     &           - delstor - Hru_intcpevap(i)
        IF ( Use_transfer_intcp==ACTIVE ) hrubal = hrubal + Gain_inches(i) - Net_apply(i)
        IF ( ABS(hrubal)>TOOSMALL ) THEN
          IF ( ABS(hrubal)>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible HRU interception water balance error'
          ELSE
            WRITE ( BALUNT, * ) 'Interception HRU rounding issue'
          ENDIF
          WRITE ( BALUNT,'(I7,6I5,15F10.5,I5)' ) i, Nowtime, hrubal, &
     &            Net_rain(i), Net_snow(i), Hru_rain(i), Hru_snow(i), &
     &            Intcp_stor(i), It0_hru_intcpstor(i), Intcp_evap(i), Srain_intcp(i), &
     &            Wrain_intcp(i), Snow_intcp(i), Canopy_covden(i), delstor, &
     &            Hru_intcpstor(i), Intcp_changeover(i), Cov_type(i)
          IF ( Use_transfer_intcp==1 ) WRITE ( BALUNT, * ) Gain_inches(i), Net_apply(i)
        ENDIF

        ! Skip the HRU if there is no snowpack and no new snow
        IF ( It0_pkwater_equiv(i)>Snowpack_threshold(i) .OR. Net_snow(i)>0.0 ) THEN
          hrubal = SNGL( It0_pkwater_equiv(i) - Pkwater_equiv(i) ) - Snow_evap(i) - Snowmelt(i)
          IF ( Pptmix_nopack(i)==ACTIVE ) THEN
            hrubal = hrubal + Net_snow(i)
          ELSE
            hrubal = hrubal + Net_ppt(i)
          ENDIF
          IF ( ABS(hrubal)>TOOSMALL ) THEN
            IF ( ABS(hrubal)>SMALL ) THEN
              WRITE ( BALUNT, * ) 'Possible HRU snow water balance error'
            ELSE
              WRITE ( BALUNT, * ) 'Possible HRU snow rounding issue'
            ENDIF
            WRITE ( BALUNT, * ) i, hrubal, Nowyear, Nowmonth, Nowday, &
     &              It0_pkwater_equiv(i), Pkwater_equiv(i), Snow_evap(i), &
     &              Snowmelt(i), Net_ppt(i), Net_snow(i), Net_rain(i), &
     &              Hru_snow(i), Pptmix(i), Pptmix_nopack(i), Intcp_changeover(i)
            WRITE ( BALUNT, '(A,/)' ) '*******************************'
          ENDIF
          bsnobal = bsnobal + DBLE(hrubal*harea)
        ENDIF

! srunoff balance
        delta_stor_sngl = It0_hru_impervstor(i) - Hru_impervstor(i)
        waterout = Infil(i)*perv_frac + Hru_impervevap(i) + Hortonian_flow(i) ! Hortonian_flow includes dprst runoff, if any
        gmelt = 0.0
        IF ( Glacier_flag == 1 ) gmelt = Glacrb_melt(i) ! ?? what about Glacr_flow(i); units inches cubed
        waterin = gmelt + Net_apply(i) !; net_apply = 0 if no application
        IF ( Pptmix_nopack(i) == ACTIVE ) waterin = waterin + Net_rain(i)
        IF ( Snowmelt(i)>0.0 ) THEN
          waterin = waterin + Snowmelt(i)
        ELSEIF ( Pkwater_equiv(i)<Snowpack_threshold(i) ) THEN
          IF ( .not.(Net_snow(i)>0.0) .AND. Net_rain(i)>0.0 ) THEN
            IF ( .not.(Pk_precip(i)>0.0) .AND. Pptmix_nopack(i) == OFF ) waterin = waterin + Net_rain(i)
          ENDIF
        ENDIF
        IF ( Cascade_flag>CASCADE_OFF ) waterin = waterin + SNGL( Upslope_hortonian(i) - Hru_hortn_cascflow(i) )
        IF ( Dprst_flag==ACTIVE ) THEN
          waterout = waterout + Dprst_evap_hru(i) + SNGL( Dprst_seep_hru(i) ) ! ??- Dprst_in(i) - Dprst_insroff_hru(i)
          delta_stor_sngl = delta_stor_sngl + SNGL( It0_dprst_stor_hru(i) - Dprst_stor_hru(i) )
        ENDIF
        robal = waterin + delta_stor_sngl - waterout
        IF ( AG_flag==ACTIVE ) robal = robal - Infil_ag(i)*Ag_frac(i)
        basin_robal = basin_robal + DBLE( robal )
        IF ( ABS(robal)>TOOSMALL ) THEN
          IF ( ABS(robal)>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible HRU surface runoff water balance error'
          ELSE
            WRITE ( BALUNT, * ) 'Possible HRU surface runoff rounding issue'
          ENDIF
          WRITE ( BALUNT, * ) robal, '; HRU: ', i, '; hru_type: ', Hru_type(i)
          IF ( Cascade_flag>CASCADE_OFF ) THEN
            WRITE ( BALUNT, * ) Nowyear, Nowmonth, Nowday, &
     &              It0_hru_impervstor(i), Infil(i), Hortonian_flow(i), Hru_impervstor(i), &
     &              Hru_impervevap(i), Hru_frac_imperv(i), perv_frac, Net_ppt(i), &
     &              Hru_sroffp(i), Hru_sroffi(i), harea, Upslope_hortonian(i), Hru_hortn_cascflow(i)
          ELSE
            WRITE ( BALUNT, * ) Nowyear, Nowmonth, Nowday, &
     &              It0_hru_impervstor(i), Infil(i), Hortonian_flow(i), Hru_impervstor(i), &
     &              Hru_impervevap(i), Hru_frac_imperv(i), perv_frac, Net_ppt(i), &
     &              Hru_sroffp(i), Hru_sroffi(i), Intcp_changeover(i), Potet(i), harea
          ENDIF
          WRITE ( BALUNT,'(A,I3,8(1X,F0.7))' ) 'snow', Pptmix_nopack(i), Snowmelt(i), Snowcov_area(i), Pkwater_equiv(i), &
     &                       Snow_evap(i), It0_pkwater_equiv(i), Net_snow(i), Net_rain(i), gmelt, pk_precip(i)
          IF ( Dprst_flag==ACTIVE ) THEN
            dprst_hru_wb = It0_dprst_stor_hru(i) - Dprst_stor_hru(i) - Dprst_seep_hru(i) - Dprst_sroff_hru(i) + Dprst_in(i) &
     &                     - DBLE( Dprst_evap_hru(i) ) + DBLE( Dprst_insroff_hru(i) )
            Basin_dprst_wb = Basin_dprst_wb + dprst_hru_wb*harea_dble
            WRITE ( BALUNT, * ) 'dprst', dprst_hru_wb, Hru_frac_dprst(i), Dprst_stor_hru(i), &
     &              Dprst_seep_hru(i), Dprst_evap_hru(i), Dprst_sroff_hru(i), Dprst_insroff_hru(i)
            WRITE ( BALUNT, * ) Dprst_vol_open(i), Dprst_vol_clos(i), &
     &              (Dprst_vol_open(i)+Dprst_vol_clos(i))/harea_dble, Dprst_area_max(i), &
     &              Dprst_area_clos(i), Dprst_in(i), Sro_to_dprst_perv(i)
          ENDIF
          IF ( AG_flag==ACTIVE ) WRITE ( BALUNT, * ) 'ag', perv_frac, Ag_frac(i), Infil_ag(i)
          WRITE ( BALUNT, '(A,/)' ) '*******************************'
        ENDIF

        last_sm = It0_soil_moist(i)
        last_ss = It0_ssres_stor(i)

        soilbal = (last_sm - Soil_moist(i) - Perv_actet(i))*perv_frac + Cap_infil_tot(i)
        IF ( AG_flag == ACTIVE ) THEN
          soilbal = soilbal - perv_soil_to_gvr(i) - perv_soil_to_gw(i)
        ELSE  
          soilbal = soilbal - Soil_to_ssr(i) - Soil_to_gw(i)
        ENDIF

        IF ( ABS(soilbal)>TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'HRU capillary problem, HRU:', i
          WRITE ( BALUNT, * ) soilbal, Cap_infil_tot(i), last_sm, Soil_moist(i), Perv_actet(i), Soil_to_ssr(i), &
     &                        Soil_to_gw(i), i, Infil(i), perv_frac, &
     &                        Soil_moist_max(i), Cap_waterin(i), gmelt
          IF ( AG_flag == ACTIVE ) WRITE ( BALUNT, * ) 'pervious for ag', perv_soil_to_gvr(i), perv_soil_to_gw(i)
          IF ( Cascade_flag>CASCADE_OFF ) WRITE ( BALUNT, * ) 'UP cascade', Upslope_interflow(i), Upslope_dunnianflow(i)
        ENDIF
        gvrbal = last_ss - Ssres_stor(i) + Soil_to_ssr(i) - Ssr_to_gw(i) - Swale_actet(i) - Dunnian_flow(i) &
     &           - Ssres_flow(i)
        IF ( Pref_flag == ACTIVE ) gvrbal = gvrbal + Pfr_dunnian_flow(i) + Pref_flow_infil(i)
        IF ( Cascade_flag>CASCADE_OFF ) gvrbal = gvrbal - SNGL( Hru_sz_cascadeflow(i) )
        test = ABS( gvrbal )
        IF ( test>TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'Bad GVR balance, HRU:', i, ' hru_type:', Hru_type(i)
          WRITE ( BALUNT, * ) gvrbal, last_ss, Ssres_stor(i), Ssr_to_gw(i), Swale_actet(i), &
     &            Dunnian_flow(i), Ssres_flow(i), Ssres_in(i), Grav_dunnian_flow(i), Slow_flow(i), Soil_to_ssr(i), &
     &            perv_frac, Slow_stor(i), Infil(i)
          IF ( Pref_flag == ACTIVE ) WRITE ( BALUNT, * ) 'PFR', Pfr_dunnian_flow(i), Pref_flow_thrsh(i), &
     &            Pref_flow_infil(i), Pref_flow(i), Gvr2pfr(i), Pref_flow_stor(i), Pref_flow_max(i), Pref_flow_den(i)
          IF ( Cascade_flag>CASCADE_OFF ) WRITE ( BALUNT, * ) 'sz cascade', Hru_sz_cascadeflow(i)
        ENDIF

        waterin = Cap_infil_tot(i)
        IF ( Pref_flag == ACTIVE ) waterin = waterin + Pref_flow_infil(i) + Pfr_dunnian_flow(i)
        waterout = Ssr_to_gw(i) + Ssres_flow(i) + Soil_to_gw(i) + Swale_actet(i) + Perv_actet(i)*perv_frac &
     &             + Dunnian_flow(i)
        IF ( AG_flag==ACTIVE ) THEN
          waterin = waterin + DBLE( Ag_cap_infil_tot(i) )
          waterout = waterout + DBLE( hru_ag_actet(i) )
        ENDIF
        IF ( Cascade_flag>CASCADE_OFF ) waterout = waterout + Hru_sz_cascadeflow(i)
        soil_in = soil_in + DBLE(Infil(i)*perv_frac)*harea
        soilbal = waterin - waterout + last_ss - Ssres_stor(i) + (last_sm-Soil_moist(i))*perv_frac
        IF ( AG_flag==ACTIVE ) THEN
          soil_in = soil_in + DBLE(Infil_ag(i)*Ag_frac(i))*harea
          soilbal = soilbal + DBLE( (It0_ag_soil_moist(i) - Ag_soil_moist(i))*Ag_frac(i) )
        ENDIF
        basin_bal = basin_bal + DBLE(soilbal*harea)
        test = ABS( soilbal )
        IF ( test>TOOSMALL ) THEN
!          IF ( test>Ssres_stor(i)*TOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'HRU:', i, ' Hru_type:', Hru_type(i)
          IF ( test>BAD ) THEN
            WRITE ( BALUNT, * ) 'HRU soilzone water balance ***ERROR***'
          ELSEIF ( test>SMALL ) THEN
            WRITE ( BALUNT, * ) 'Possible soilzone HRU water balance ERROR'
          ELSE
            WRITE ( BALUNT, * ) 'Possible soilzone HRU water balance rounding issue'
          ENDIF
          WRITE ( BALUNT, 9001 ) Nowyear, Nowmonth, Nowday, i, soilbal, Infil(i), last_sm, last_ss, &
     &            Soil_moist(i), Ssres_stor(i), Perv_actet(i), Ssr_to_gw(i), Slow_flow(i), Ssres_flow(i), &
     &            Soil_to_gw(i), Slow_stor(i), Soil_rechr(i), &
     &            Soil_lower(i), Soil_to_ssr(i), waterin, Swale_actet(i)
          IF ( Pref_flag == ACTIVE ) &
               WRITE ( BALUNT, * ) Pref_flow(i), Pref_flow_infil(i), Pref_flow_stor(i), Pref_flow_den(i)
          IF ( Cascade_flag>CASCADE_OFF ) WRITE ( BALUNT, * ) 'cascade', Upslope_dunnianflow(i), Upslope_interflow(i), &
     &                                                        Hru_sz_cascadeflow(i), Ncascade_hru(i)
          WRITE ( BALUNT, * ) Hru_perv(i), perv_frac, (Infil(i)*perv_frac), Cap_infil_tot(i)
          WRITE ( BALUNT, * ) Dunnian_flow(i)
          IF ( Pref_flag==ACTIVE ) WRITE ( BALUNT, * )  Pfr_dunnian_flow(i)
          IF ( AG_flag==ACTIVE ) WRITE ( BALUNT, * ) &
               'ag', It0_ag_soil_moist(i), Ag_soil_moist(i), Ag_frac(i), Ag_cap_infil_tot(i), hru_ag_actet(i)
!          ENDIF
          WRITE ( BALUNT, '(A,/)' ) '################################'
        ENDIF

        hru_out = DBLE( Sroff(i) + Gwres_flow(i) + Ssres_flow(i) + Hru_actet(i) + Gwres_sink(i) )
        hru_in = DBLE( Hru_ppt(i) ) ! need water use input
        IF ( Cascade_flag>CASCADE_OFF ) THEN
          hru_out = hru_out + Hru_sz_cascadeflow(i) + Hru_hortn_cascflow(i)
          hru_in = hru_in + Upslope_dunnianflow(i) + Upslope_interflow(i) + Upslope_hortonian(i)
        ENDIF
        IF ( Cascadegw_flag>CASCADEGW_OFF ) THEN
          hru_out = hru_out + DBLE( Hru_gw_cascadeflow(i) )
          hru_in = hru_in + Gw_upslope(i)/harea_dble
        ENDIF
!        Hru_runoff(i) = hru_out - DBLE( Hru_actet(i) )
        wbal = hru_in - hru_out + Hru_storage_ante(i) - Hru_storage(i)
        IF ( Gwminarea_flag==ACTIVE ) wbal = wbal + Gwstor_minarea_wb(i)
        IF ( DABS(wbal)>DTOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'Possible HRU water balance issue:', wbal, '; HRU:', i, ' hru_type:', Hru_type(i), '; area:', harea
          WRITE ( BALUNT, * ) 'fluxes', Sroff(i), Gwres_flow(i), Ssres_flow(i), Hru_actet(i), Gwres_sink(i), &
       &                      Intcp_changeover(i), Dunnian_flow(i)
          IF ( Pref_flag == ACTIVE ) WRITE ( BALUNT, * ) 'PFR', Pfr_dunnian_flow(i)
          WRITE ( BALUNT, * ) 'ppt', Hru_snow(i), Hru_rain(i), Hru_ppt(i), Net_rain(i), Net_snow(i), Net_ppt(i), &
       &                      Pptmix(i)
          WRITE(balunt,*) 'snow', It0_pkwater_equiv(i), Pkwater_equiv(i), Snow_evap(i), Snowmelt(i), Pptmix_nopack(i), Pk_precip(i)
          WRITE(balunt,*) 'aet', Hru_intcpevap(i), Hru_impervevap(i), Perv_actet(i), Potet(i)
          !WRITE ( BALUNT, * ) Gwstor_minarea_wb(i)
          dprst_stor = 0.0D0
          IF ( Dprst_flag==ACTIVE ) THEN
            dprst_stor = Dprst_stor_hru(i)
            WRITE ( BALUNT, * ) 'dprst', Dprst_evap_hru(i), It0_dprst_stor_hru(i), Dprst_stor_hru(i), Dprst_seep_hru(i), Dprst_insroff_hru(i)
          ENDIF
          WRITE ( BALUNT, * ) 'availh2o', waterin, Intcp_changeover(i), Snowmelt(i), Net_ppt(i), Pptmix_nopack(i), &
                              Net_rain(i), Snowcov_area(i), It0_pkwater_equiv(i), Snow_evap(i), Net_snow(i), Pkwater_equiv(i)
          WRITE ( BALUNT, * ) 'storage', Soil_moist_tot(i), Hru_intcpstor(i), Gwres_stor(i), &
                              Hru_impervstor(i), Hru_storage_ante(i), Hru_storage(i), dprst_stor
          WRITE ( BALUNT, * ) 'fractions', perv_frac, Hru_frac_dprst(i), Hru_frac_imperv(i), Covden_win(i), Covden_sum(i)
          IF ( AG_flag==ACTIVE ) WRITE ( BALUNT, * ) 'ag', Ag_soil_moist(i), Ag_frac(i), &
      &        perv_frac + Hru_frac_dprst(i) + Hru_frac_imperv(i) + Ag_frac(i)
          IF ( Cascade_flag>CASCADE_OFF ) WRITE ( BALUNT, * ) 'cascade', &
               Hru_sz_cascadeflow(i), Upslope_dunnianflow(i), Upslope_interflow(i), &
               Upslope_hortonian(i), Hru_hortn_cascflow(i)
          IF ( Cascadegw_flag>CASCADEGW_OFF ) WRITE ( BALUNT, * ) 'cascade gw', Gw_upslope(i)/harea_dble, Hru_gw_cascadeflow(i)
          WRITE ( BALUNT, fmt1 ) '   Date:', Nowyear, Nowmonth, Nowday
          WRITE ( BALUNT, '(A,/)' ) '================================'
        ENDIF

        wbal = Gwstor_ante(i) + Gwres_in(i)/harea_dble - Gwres_stor(i) - DBLE( Gwres_sink(i) + Gwres_flow(i) )
        IF ( Cascadegw_flag>CASCADEGW_OFF ) wbal = wbal - DBLE( Hru_gw_cascadeflow(i) )
        IF ( Gwminarea_flag==ACTIVE ) wbal = wbal + Gwstor_minarea_wb(i)
        gwup = 0.0D0
        IF ( Cascadegw_flag>CASCADEGW_OFF ) gwup = Gw_upslope(i)
        IF ( DABS(wbal)>DTOOSMALL ) THEN
          WRITE ( BALUNT, * ) 'Possible GWR water balance issue', &
     &                        i, wbal, Gwstor_ante(i), Gwres_in(i)/harea_dble, Gwres_stor(i), Gwres_flow(i), &
     &                        Gwres_sink(i), Soil_to_gw(i), Ssr_to_gw(i), gwup, harea
          IF ( Cascadegw_flag>CASCADEGW_OFF ) WRITE ( BALUNT, * ) 'gw cascade', Hru_gw_cascadeflow(i)
          IF ( Gwminarea_flag==ACTIVE ) WRITE ( BALUNT, * ) 'gwstor_minarea_wb', Gwstor_minarea_wb(i)
          IF ( Dprst_flag==ACTIVE ) WRITE ( BALUNT, * ) 'gwin_dprst', Gwin_dprst(i)
        ENDIF
        Hru_storage_ante(i) = Hru_storage(i)
        Gwstor_ante(i) = Gwres_stor(i)
      ENDDO
      Basin_dprst_wb = Basin_dprst_wb*Basin_area_inv

! intcp note, changeover added to net_rain
      delta_stor = Basin_intcp_stor - Last_basin_intcp_stor
      pptbal = Basin_ppt - Basin_net_ppt - delta_stor - Basin_intcp_evap !- Basin_changeover
      IF ( Use_transfer_intcp==ACTIVE ) pptbal = pptbal + Basin_net_apply
      IF ( DABS(pptbal)>DSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin interception water balance error', &
     &                         Nowyear, Nowmonth, Nowday, pptbal, Basin_changeover
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ELSEIF ( DABS(pptbal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Interception basin rounding issue'
        WRITE ( BALUNT, 9002 ) Nowyear, Nowmonth, Nowday, pptbal, &
     &                         Basin_ppt, Basin_net_ppt, Basin_intcp_evap, Basin_intcp_stor, &
     &                         Last_basin_intcp_stor, Basin_changeover, Basin_net_apply, Basin_hru_apply
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ENDIF
      WRITE ( INTCPUNT, 9002 ) Nowyear, Nowmonth, Nowday, pptbal, &
     &        Basin_ppt, Basin_net_ppt, Basin_intcp_evap, &
     &        Basin_intcp_stor, Last_basin_intcp_stor, Basin_changeover, Basin_net_apply, Basin_hru_apply

! snowcomp
      bsnobal = bsnobal*Basin_area_inv
      IF ( DABS(bsnobal)>DSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin snow water balance error', &
     &                         Nowyear, Nowmonth, Nowday, bsnobal
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ELSEIF ( DABS(bsnobal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'Possible basin snow rounding issue', &
     &                         Nowyear, Nowmonth, Nowday, bsnobal
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ENDIF
      WRITE ( SNOWUNIT, 9002 ) Nowyear, Nowmonth, Nowday, bsnobal, Basin_pweqv, &
     &                         Basin_snowmelt, Basin_snowevap, Basin_snowcov

! srunoff
      brobal = Basin_sroff - Basin_sroffp - Basin_sroffi - Basin_dprst_sroff - Basin_cfgi_sroff - Basin_dunnian
      IF ( Cascade_flag>CASCADE_OFF ) THEN
        brobal = brobal + Basin_sroff_down
        WRITE ( SROUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_robal, &
     &          brobal, Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, &
     &          Basin_sroff_down, Basin_hortonian_lakes, Basin_cfgi_sroff, Basin_dunnian, Basin_dprst_wb
      ELSE
        WRITE ( SROUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_robal, &
     &          brobal, Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, Basin_cfgi_sroff, Basin_dunnian, Basin_dprst_wb
      ENDIF
      IF ( DABS(basin_robal)>DSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'possible srunoff basin water balance ERROR', &
     &                         Nowyear, Nowmonth, Nowday, basin_robal, brobal, &
     &                         Basin_dprst_wb, Basin_net_ppt, Basin_net_snow, Basin_net_rain
        WRITE ( BALUNT, * ) Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, Basin_dunnian
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ELSEIF ( DABS(basin_robal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, 9003 ) 'possible srunoff basin water balance rounding issue', &
     &                         Nowyear, Nowmonth, Nowday, basin_robal, brobal, Basin_net_ppt, Basin_net_snow, Basin_net_rain
        WRITE ( BALUNT, * ) Basin_sroff, Basin_infil, Basin_imperv_evap, &
     &          Basin_imperv_stor, Basin_dprst_evap, Basin_dprst_seep, &
     &          Basin_sroffp, Basin_sroffi, Basin_dprst_sroff, Basin_dunnian
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ENDIF

! soilzone
      Basin_capillary_wb = It0_basin_soil_moist - Basin_soil_moist - &
     &                     Basin_perv_et - Basin_sm2gvr_max + Basin_cap_infil_tot - Basin_soil_to_gw
      Basin_gravity_wb = It0_basin_ssstor - Basin_ssstor + Basin_sm2gvr - Basin_dncascadeflow - &
     &                   Basin_ssflow - Basin_sz2gw - Basin_dunnian + Basin_dunnian_pfr - &
     &                   Basin_swale_et + Basin_pref_flow_infil
      Basin_soilzone_wb = Basin_infil + It0_basin_ssstor - &
     &                    Basin_ssstor + It0_basin_soil_moist - Basin_soil_moist - &
     &                    Basin_perv_et - Basin_swale_et - Basin_sz2gw - &
     &                    Basin_soil_to_gw - Basin_ssflow - Basin_dunnian - &
     &                    Basin_lakeinsz

      IF ( AG_flag == ACTIVE ) THEN
        Basin_capillary_wb = Basin_capillary_wb - Basin_perv_to_gw
        Basin_soilzone_wb = Basin_soilzone_wb + It0_basin_ag_soil_moist - Basin_ag_soil_moist - Basin_ag_actet
      ENDIF

      IF ( DABS(Basin_gravity_wb)>DTOOSMALL ) WRITE ( BALUNT, * ) 'basin gvrbal issue', Basin_gravity_wb, &
     &     It0_basin_ssstor, Basin_ssstor, Basin_sm2gvr, Basin_ssflow, Basin_sz2gw, Basin_dunnian, &
     &     Basin_swale_et, Basin_pref_flow_infil, Basin_dninterflow, Basin_pref_stor, &
     &     Basin_dunnian_pfr, Basin_lakeinsz, Basin_dncascadeflow, &
     &     Basin_dunnian_gvr, Basin_slowflow, Basin_prefflow, Basin_gvr2pfr, Nowtime
      IF ( DABS(Basin_capillary_wb)>DTOOSMALL ) WRITE( BALUNT, * ) 'possible basin capillary balance issue', &
     &     Basin_capillary_wb, It0_basin_soil_moist, Basin_soil_moist, Basin_perv_et, &
     &     Basin_sm2gvr, Basin_cap_infil_tot, Basin_soil_to_gw, Basin_sm2gvr_max, Basin_capwaterin, Nowtime
      IF ( DABS(Basin_soilzone_wb)>DTOOSMALL ) THEN
        WRITE ( BALUNT, * ) 'possible basin soil zone rounding issue', &
     &     Basin_soilzone_wb, Basin_capwaterin, Basin_pref_flow_infil, Basin_infil, &
     &     It0_basin_ssstor, Basin_ssstor, It0_basin_soil_moist, Basin_soil_moist, Basin_perv_et, Basin_swale_et, &
     &     Basin_sz2gw, Basin_soil_to_gw, Basin_ssflow, Basin_dunnian, Basin_dncascadeflow, &
     &     Basin_sm2gvr, Basin_lakeinsz, Basin_dunnian_pfr
        IF ( AG_flag == ACTIVE ) WRITE ( BALUNT, * ) 'ag', It0_basin_ag_soil_moist, Basin_ag_soil_moist, Basin_ag_actet
        WRITE ( BALUNT, * ) Nowtime
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ENDIF

      soil_in = soil_in*Basin_area_inv
      basin_bal = basin_bal*Basin_area_inv
      bsmbal = It0_basin_soil_moist - Basin_soil_moist + It0_basin_ssstor - Basin_ssstor - Basin_perv_et - Basin_sz2gw + soil_in - &
     &         Basin_ssflow - Basin_soil_to_gw - Basin_dunnian - Basin_swale_et - Basin_lakeinsz
      IF ( AG_flag == ACTIVE ) bsmbal = bsmbal + It0_basin_ag_soil_moist - Basin_ag_soil_moist - Basin_ag_actet

      WRITE ( SZUNIT, 9002 ) Nowyear, Nowmonth, Nowday, basin_bal, &
     &        bsmbal, It0_basin_soil_moist, Basin_soil_moist, It0_basin_ssstor, &
     &        Basin_ssstor, Basin_perv_et, Basin_sz2gw, Basin_ssflow, &
     &        Basin_soil_to_gw, Basin_dunnian, soil_in, Basin_lakeinsz, &
     &        Basin_dncascadeflow, Basin_swale_et, Basin_prefflow, Basin_dunnian_pfr, &
     &        Basin_pref_stor, Basin_slstor, Basin_dunnian_gvr, Basin_lakeevap

      IF ( DABS(bsmbal)>0.05D0 .OR. DABS(basin_bal)>0.001D0 ) THEN
        WRITE ( BALUNT, * ) '*ERROR, soilzone basin water balance', bsmbal, &
     &          basin_bal, It0_basin_soil_moist, Basin_soil_moist, &
     &          It0_basin_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw, &
     &          soil_in, Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
     &          Basin_swale_et, Basin_lakeinsz
        WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ELSEIF ( DABS(bsmbal)>0.005D0 .OR. DABS(basin_bal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, * ) 'Possible soilzone basin water balance ERROR', &
     &          bsmbal, basin_bal, It0_basin_soil_moist, Basin_soil_moist, &
     &          It0_basin_ssstor, Basin_ssstor, Basin_perv_et, Basin_sz2gw, &
     &          soil_in, Basin_ssflow, Basin_soil_to_gw, Basin_dunnian, &
     &          Basin_swale_et, Basin_lakeinsz
        WRITE ( BALUNT, * ) Basin_pref_stor, Basin_slstor
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ELSEIF ( DABS(bsmbal)>0.0005D0 .OR. DABS(basin_bal)>DTOOSMALL ) THEN
        WRITE ( BALUNT, '(A,2F12.7)' ) 'Basin soilzone rounding issue', bsmbal, basin_bal
        WRITE ( BALUNT, * ) Basin_soilzone_wb, Basin_ssin, &
     &          Basin_dninterflow, Basin_sm2gvr, Basin_capwaterin, &
     &          soil_in, Basin_gvr2pfr, Basin_dndunnianflow, (soil_in - Basin_infil)
        WRITE ( BALUNT, '(A,/)' ) '################################'
      ENDIF

! gwflow

      ! not going to balance because gwstor under lakes is computed each time step fix for lakes
      ! basin_gwin includes upslope flow, gwin_dprst, soil_to_gw, ssr_to_gw
      gwbal = Basin_gwin + Last_basin_gwstor - Basin_gwstor - Basin_gwsink &
     &        - Basin_gwflow - Basin_dnflow + Basin_gwstor_minarea_wb
      IF ( DABS(gwbal)>DSMALL ) WRITE ( BALUNT, 9003 ) 'Possible GWR basin water balance issue', &
     &                                                 Nowyear, Nowmonth, Nowday, gwbal
      WRITE ( GWUNIT, 9002 ) Nowyear, Nowmonth, Nowday, &
     &        gwbal, Last_basin_gwstor, Basin_gwstor, Basin_gwin, &
     &        Basin_gwflow, Basin_gwsink, &
     &        Basin_gw_upslope, Basin_gwstor_minarea_wb, Basin_dnflow
      Last_basin_gwstor = Basin_gwstor

 9001 FORMAT (I5, 2('/', I2.2), I7, 26F11.5)
 9002 FORMAT (I5, 2('/', I2.2), 23F11.5)
 9003 FORMAT (A, I5, 2('/', I2.2), 6F12.5)

      END SUBROUTINE water_balance_run

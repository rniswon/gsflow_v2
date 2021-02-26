!**********************************************************************
!     Sums values for daily, monthly, yearly and total flow
!     for daily mode
!***********************************************************************

      MODULE GSFSUM
      USE PRMS_CONSTANTS, ONLY: DEBUG_WB, DEBUG_less, ERROR_open_out, CFS2CMS_CONV, ACTIVE, READ_INIT, OFF
      USE PRMS_MODULE, ONLY: Print_debug
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW Output CSV Summary'
      character(len=10), parameter :: MODNAME = 'gsflow_sum'
      character(len=*), parameter :: Version_gsflow_sum = '2020-12-02'
      INTEGER, SAVE :: BALUNT
      DOUBLE PRECISION, PARAMETER :: ERRCHK = 0.0001D0
      INTEGER, SAVE :: Balance_unt, Vbnm_index(14), Gsf_unt, Rpt_count
      INTEGER, SAVE :: Have_wells
      DOUBLE PRECISION, SAVE :: Cumvol_precip, Cumvol_strmin
      DOUBLE PRECISION, SAVE :: Rate_precip, Cumvol_gwbndin
      DOUBLE PRECISION, SAVE :: Rate_gwbndin, Cumvol_wellin
      DOUBLE PRECISION, SAVE :: Cumvol_et, Rate_et, Cumvol_strmot
      DOUBLE PRECISION, SAVE :: Rate_strmot, Cumvol_wellot
      DOUBLE PRECISION, SAVE :: Cumvol_gwbndot, Rate_gwbndot
      DOUBLE PRECISION, SAVE :: Cum_surfstor, Basin_convert
      DOUBLE PRECISION, SAVE :: Cum_delstore, Rate_delstore
      DOUBLE PRECISION, SAVE :: Last_basin_soil_moist, Last_basin_ssstor
      DOUBLE PRECISION, SAVE :: Rate_strmin, Rate_wellin, Rate_wellot
      DOUBLE PRECISION, SAVE :: Rate_surfstor, Last_Grav_S
      DOUBLE PRECISION, SAVE :: Last_Canopy_S, Last_Imperv_S, Last_SnowPweqv_S, Last_Cap_S
      DOUBLE PRECISION, SAVE :: Basin_gsfstor, Last_Pref_S
      DOUBLE PRECISION, SAVE :: Last_Dprst_S, Rate_Dprst_S
      DOUBLE PRECISION, SAVE :: Lake2Unsat_Q, LakeExchng2Sat_Q, Stream2Unsat_Q
!      DOUBLE PRECISION, SAVE :: Cumvol_lakeppt, Cumvol_lakeevap, Cumvol_uzfet
! Added lake variables
      DOUBLE PRECISION, SAVE :: Rate_lakin, Rate_lakot, Cumvol_lakin
      DOUBLE PRECISION, SAVE :: Rate_lakestor, Cum_lakestor, Cumvol_lakot
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Cum_soilstor, Rate_soilstor
      DOUBLE PRECISION, SAVE :: Cum_uzstor, Rate_uzstor, Basingwstor
      DOUBLE PRECISION, SAVE :: Cum_satstor, Rate_satstor, Basingvr2sm
      DOUBLE PRECISION, SAVE :: Cum_pweqv, Rate_pweqv, Lake_dS
      DOUBLE PRECISION, SAVE :: SnowPweqv_S, Ave_SoilDrainage2Unsat_Q, Infil2Soil_Q
      DOUBLE PRECISION, SAVE :: Basinsoilstor, Cap_S
      DOUBLE PRECISION, SAVE :: CapDrainage2Sat_Q, StreamOut_Q
      DOUBLE PRECISION, SAVE :: Stream_S, Lake_S, SatDisch2Stream_Q
      DOUBLE PRECISION, SAVE :: Precip_Q, CapET_Q, ImpervEvap_Q, DprstEvap_Q
      DOUBLE PRECISION, SAVE :: PotGravDrn2Unsat_Q, Sat2Grav_Q, Lake2Sat_Q
      DOUBLE PRECISION, SAVE :: Canopy_S, Imperv_S, SwaleEvap_Q
      DOUBLE PRECISION, SAVE :: Interflow2Stream_Q, Sroff2Stream_Q
      DOUBLE PRECISION, SAVE :: Obs_strmflow, UnsatDrainageExcess_Q
      DOUBLE PRECISION, SAVE :: UnsatET_Q, SatET_Q, Uzf_et, RechargeUnsat2Sat_Q
      DOUBLE PRECISION, SAVE :: Basinseepout, SoilDrainage2Unsat_Q, Unsat_dS
      DOUBLE PRECISION, SAVE :: Basinrain, Basinsnow, Basinslowflow
      DOUBLE PRECISION, SAVE :: Basingvr2pfr, SnowEvap_Q
      DOUBLE PRECISION, SAVE :: HortSroff2Stream_Q, HortSroff2Lake_Q
      DOUBLE PRECISION, SAVE :: DunnInterflow2Lake_Q, LakeEvap_Q
      DOUBLE PRECISION, SAVE :: LakePrecip_Q, Grav_S
      DOUBLE PRECISION, SAVE :: Basinprefflow, Pref_S, Dprst_S
      DOUBLE PRECISION, SAVE :: Stream2Sat_Q, Basinsm2gvr
      DOUBLE PRECISION, SAVE :: UnsatStream_dS, UnsatStream_S
      DOUBLE PRECISION, SAVE :: SatDisch2Lake_Q, DunnSroff2Stream_Q
      DOUBLE PRECISION, SAVE :: Infil2CapTotal_Q, Infil2Pref_Q
      DOUBLE PRECISION, SAVE :: ActualET_Q, SnowMelt_Q, CanopyEvap_Q
      DOUBLE PRECISION, SAVE :: DunnInterflow2Cap_Q, NetWellFlow_Q, BoundaryStreamFlow_Q
!   Declared Parameters
      INTEGER, SAVE :: Id_obsrunoff
!   Control Parameters
      INTEGER, SAVE :: Rpt_days, Gsf_rpt
      CHARACTER(LEN=256), SAVE :: Csv_output_file, Gsflow_output_file
      END MODULE GSFSUM

!***********************************************************************
!     Main gsflow_sum routine
!***********************************************************************
      INTEGER FUNCTION gsflow_sum()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, SAVE_INIT, RUN, DECL, INIT, CLEAN
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfsumdecl, gsfsuminit, gsfsumrun, gsfsumclean
      EXTERNAL :: gsflow_sum_restart
!***********************************************************************
      gsflow_sum = 0

      IF ( Process_flag==RUN ) THEN
        gsflow_sum = gsfsumrun()
      ELSEIF ( Process_flag==DECL ) THEN
        gsflow_sum = gsfsumdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        gsflow_sum = gsfsuminit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL gsflow_sum_restart(SAVE_INIT)
        gsflow_sum = gsfsumclean()
      ENDIF

      END FUNCTION gsflow_sum

!***********************************************************************
!     gsfsumdecl - set up basin summary parameters
!   Declared Parameters
!     id_obsrunoff, runoff_units
!   Declared Control Parameters
!     rpt_days, csv_output_file, gsflow_output_file, model_output_file
!***********************************************************************
      INTEGER FUNCTION gsfsumdecl()
      USE GSFSUM
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL :: print_module, PRMS_open_module_file
!***********************************************************************
      gsfsumdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_gsflow_sum)

      IF ( Print_debug==DEBUG_WB ) THEN
        CALL PRMS_open_module_file(BALUNT, 'gsflow_sum.wbal')
        WRITE ( BALUNT, 9001 )
      ENDIF

      IF ( declvar(MODNAME, 'CapDrainage2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of direct gravity drainage from excess capillary water to the unsaturated zone', &
     &     'L3/T', CapDrainage2Sat_Q)/=0 ) CALL read_error(3, 'CapDrainage2Sat_Q')

      IF ( declvar(MODNAME, 'Precip_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of precipitation', &
     &     'L3/T', Precip_Q)/=0 ) CALL read_error(3, 'Precip_Q')

      IF ( declvar(MODNAME, 'basinsnow', 'one', 1, 'double', &
     &     'Volumetric flow rate of snow', &
     &     'L3/T', Basinsnow)/=0 ) CALL read_error(3, 'basinsnow')

      IF ( declvar(MODNAME, 'basinrain', 'one', 1, 'double', &
     &     'Volumetric flow rate of rain', &
     &     'L3/T', Basinrain)/=0 ) CALL read_error(3, 'basinrain')

      IF ( declvar(MODNAME, 'CapET_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evapotranspiration from pervious areas ', &
     &     'L3/T', CapET_Q)/=0 ) CALL read_error(3, 'CapET_Q')

      IF ( declvar(MODNAME, 'ImpervEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evaporation from impervious areas', &
     &     'L3/T', ImpervEvap_Q)/=0 ) CALL read_error(3, 'ImpervEvap_Q')

      IF ( declvar(MODNAME, 'DprstEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evaporation from surface depressions', &
     &     'L3/T', DprstEvap_Q)/=0 ) CALL read_error(3, 'DprstEvap_Q')

      IF ( declvar(MODNAME, 'CanopyEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evaporation of intercepted precipitation', &
     &     'L3/T', CanopyEvap_Q)/=0 ) CALL read_error(3, 'CanopyEvap_Q')

      IF ( declvar(MODNAME, 'SnowEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of snowpack sublimation', &
     &     'L3/T', SnowEvap_Q)/=0 ) CALL read_error(3, 'SnowEvap_Q')

      IF ( declvar(MODNAME, 'LakeEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evaporation from lakes', &
     &     'L3/T', LakeEvap_Q)/=0 ) CALL read_error(3, 'LakeEvap_Q')

      IF ( declvar(MODNAME, 'LakePrecip_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of precipitation on lakes', &
     &     'L3/T', LakePrecip_Q)/=0 ) CALL read_error(3, 'LakePrecip_Q')

      IF ( declvar(MODNAME, 'StreamOut_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of streamflow leaving modeled region', &
     &     'L3/T', StreamOut_Q)/=0 ) CALL read_error(3, 'StreamOut_Q')

      IF ( declvar(MODNAME, 'PotGravDrn2Unsat_Q', 'one', 1, 'double', &
     &     'Potential volumetric flow rate of gravity drainage from'// &
     &     ' the soil zone to the unsaturated zone (before conditions of the unsaturated and saturated zones are applied)', &
     &     'L3/T', PotGravDrn2Unsat_Q)/=0 ) CALL read_error(3, 'PotGravDrn2Unsat_Q')

      IF ( declvar(MODNAME, 'Sat2Grav_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of groundwater discharge from the saturated zone to the soil zone', &
     &     'L3/T', Sat2Grav_Q)/=0 ) CALL read_error(3, 'Sat2Grav_Q')

      IF ( declvar(MODNAME, 'RechargeUnsat2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of recharge from the unsaturated zone to the saturated zone', &
     &     'L3/T', RechargeUnsat2Sat_Q)/=0 ) CALL read_error(3, 'RechargeUnsat2Sat_Q')

      IF ( declvar(MODNAME, 'basinseepout', 'one', 1, 'double', &
     &     'Volumetric flow rate of groundwater discharge from the saturated zone to the soil zone', &
     &     'L3/T', Basinseepout)/=0 ) CALL read_error(3, 'basinseepout')

      IF ( declvar(MODNAME, 'Cap_S', 'one', 1, 'double', &
     &     'Volume of water in capillary reservoirs of the soil zone', &
     &     'L3', Cap_S)/=0 ) CALL read_error(3, 'Cap_S')

      IF ( declvar(MODNAME, 'Grav_S', 'one', 1, 'double', &
     &     'Volume of water in gravity reservoirs of the soil zone', &
     &     'L3', Grav_S)/=0 ) CALL read_error(3, 'Grav_S')

      IF ( declvar(MODNAME, 'Canopy_S', 'one', 1, 'double', &
     &     'Volume of intercepted precipitation in plant-canopy reservoirs', &
     &     'L3', Canopy_S)/=0 ) CALL read_error(3, 'Canopy_S')

      IF ( declvar(MODNAME, 'Imperv_S', 'one', 1, 'double', &
     &     'Volume of water in impervious reservoirs', &
     &     'L3', Imperv_S)/=0 ) CALL read_error(3, 'Imperv_S')

      IF ( declvar(MODNAME, 'Interflow2Stream_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of slow plus fast interflow to streams', &
     &     'L3/T', Interflow2Stream_Q)/=0 ) CALL read_error(3, 'Interflow2Stream_Q')

      IF ( declvar(MODNAME, 'Sroff2Stream_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of surface runoff to streams', &
     &     'L3/T', Sroff2Stream_Q)/=0 ) CALL read_error(3, 'Sroff2Stream_Q')

      IF ( declvar(MODNAME, 'HortSroff2Lake_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of Hortonian surface runoff to lakes', &
     &     'L3/T', HortSroff2Lake_Q)/=0 ) CALL read_error(3, 'HortSroff2Lake_Q')

      IF ( declvar(MODNAME, 'DunnInterflow2Lake_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of interflow and Dunnian surface runoff to lakes', &
     &     'L3/T', DunnInterflow2Lake_Q)/=0 ) CALL read_error(3, 'DunnInterflow2Lake_Q')

      IF ( declvar(MODNAME, 'Stream_S', 'one', 1, 'double', &
     &     'Volume of water in streams (non-zero only when transient routing option is used in SFR2)', &
     &     'L3', Stream_S)/=0 ) CALL read_error(3, 'Stream_S')

      IF ( declvar(MODNAME, 'Lake_S', 'one', 1, 'double', &
     &     'Volume of water in lakes', &
     &     'L3', Lake_S)/=0 ) CALL read_error(3, 'Lake_S')

      IF ( declvar(MODNAME, 'obs_strmflow', 'one', 1, 'double', &
     &     'Volumetric flow rate of streamflow measured at a gaging station', &
     &     'L3/T', Obs_strmflow)/=0 ) CALL read_error(3, 'obs_strmflow')

      IF ( declvar(MODNAME, 'UnsatDrainageExcess_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of gravity drainage from the soil'// &
     &     ' zone not accepted due to conditions in the unsaturated and saturated zones', &
     &     'L3/T', UnsatDrainageExcess_Q)/=0 ) CALL read_error(3, 'UnsatDrainageExcess_Q')

      IF ( declvar(MODNAME, 'Pref_S', 'one', 1, 'double', &
     &     'Volume of water stored in preferential-flow reservoirs of the soil zone', &
     &     'L3', Pref_S)/=0 ) CALL read_error(3, 'Pref_S')

      IF ( declvar(MODNAME, 'Dprst_S', 'one', 1, 'double', &
     &     'Volume of water stored in surface-depression storage', &
     &     'L3', Dprst_S)/=0 ) CALL read_error(3, 'Dprst_S')

      IF ( declvar(MODNAME, 'SwaleEvap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evaporation from swale HRUs', &
     &     'L3/T', SwaleEvap_Q)/=0 ) CALL read_error(3, 'SwaleEvap_Q')

      IF ( declvar(MODNAME, 'uzf_et', 'one', 1, 'double', &
     &     'Volumetric flow rate of evapotranspiration from the unsaturated and saturated zones', &
     &     'L3/T', Uzf_et)/=0 ) CALL read_error(3, 'uzf_et')

      IF ( declvar(MODNAME, 'UnsatET_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evapotranspiration from the unsaturated zone', &
     &     'L3/T', UnsatET_Q)/=0 ) CALL read_error(3, 'UnsatET_Q')

      IF ( declvar(MODNAME, 'SatET_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of evapotranspiration from the saturated zone', &
     &     'L3/T', SatET_Q)/=0 ) CALL read_error(3, 'SatET_Q')

      IF ( declvar(MODNAME, 'Unsat_dS', 'one', 1, 'double', &
     &     'Change in unsaturated-zone storage', &
     &     'L3', Unsat_dS)/=0 ) CALL read_error(3, 'Unsat_dS')

      IF ( declvar(MODNAME, 'SoilDrainage2Unsat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of gravity drainage to the unsaturated and saturated zones', &
     &     'L3/T', SoilDrainage2Unsat_Q)/=0 ) CALL read_error(3, 'SoilDrainage2Unsat_Q')

      IF ( declvar(MODNAME, 'Stream2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of stream leakage to the unsaturated and saturated zones', &
     &     'L3/T', Stream2Sat_Q)/=0 ) CALL read_error(3, 'Stream2Sat_Q')

      IF ( declvar(MODNAME, 'UnsatStream_dS', 'one', 1, 'double', &
     &     'Change in unsaturated-zone storage under streams', &
     &     'L3', UnsatStream_dS)/=0 ) CALL read_error(3, 'UnsatStream_dS')

      IF ( declvar(MODNAME, 'SatDisch2Stream_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of groundwater discharge to streams', &
     &     'L3/T', SatDisch2Stream_Q)/=0 ) CALL read_error(3, 'SatDisch2Stream_Q')

      IF ( declvar(MODNAME, 'UnsatStream_S', 'one', 1, 'double', &
     &     'Volume of water in the unsaturated zone under streams', &
     &     'L3', UnsatStream_S)/=0 ) CALL read_error(3, 'UnsatStream_S')

      IF ( declvar(MODNAME, 'Lake2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of lake leakage to the unsaturated and saturated zones', &
     &     'L3/T', Lake2Sat_Q)/=0 ) CALL read_error(3, 'Lake2Sat_Q')

      IF ( declvar(MODNAME, 'Lake_dS', 'one', 1, 'double', &
     &     'Change in lake storage', &
     &     'L3', Lake_dS)/=0 ) CALL read_error(3, 'Lake_dS')

      IF ( declvar(MODNAME, 'SatDisch2Lake_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of groundwater discharge to lakes', &
     &     'L3/T', SatDisch2Lake_Q)/=0 ) CALL read_error(3, 'SatDisch2Lake_Q')

      IF ( declvar(MODNAME, 'Infil2Soil_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of soil infiltration (including precipitation, snowmelt, and cascading Hortonian flow)', &
     &     'L3/T', Infil2Soil_Q)/=0 ) CALL read_error(3, 'Infil2Soil_Q')

      IF ( declvar(MODNAME, 'DunnSroff2Stream_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of Dunnian runoff to streams', &
     &     'L3/T', DunnSroff2Stream_Q)/=0 ) CALL read_error(3, 'DunnSroff2Stream_Q')

      IF ( declvar(MODNAME, 'HortSroff2Stream_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of Hortonian runoff to streams', &
     &     'L3/T', HortSroff2Stream_Q)/=0 ) CALL read_error(3, 'HortSroff2Stream_Q')

      IF ( declvar(MODNAME, 'basinsm2gvr', 'one', 1, 'double', &
     &     'Volumetric flow rate of flow from capillary reservoirs to gravity reservoirs', &
     &     'L3/T', Basinsm2gvr)/=0 ) CALL read_error(3, 'basinsm2gvr')

      IF ( declvar(MODNAME, 'basingvr2sm', 'one', 1, 'double', &
     &     'Volumetric flow rate of flow from gravity reservoirs to capillary reservoirs', &
     &     'L3/T', Basingvr2sm)/=0 ) CALL read_error(3, 'basingvr2sm')

      IF ( declvar(MODNAME, 'Infil2CapTotal_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of soil infiltration into capillary'// &
     &     ' reservoirs including precipitation, snowmelt, and'// &
     &     ' cascading Hortonian and Dunnian runoff and interflow'// &
     &     ' minus infiltration to preferential-flow reservoirs', &
     &     'L3/T', Infil2CapTotal_Q)/=0 ) CALL read_error(3, 'Infil2CapTotal_Q')

      IF ( declvar(MODNAME, 'Infil2Pref_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of soil infiltration into'// &
     &     ' preferential-flow reservoirs including precipitation,'// &
     &     ' snowmelt, and cascading surface runoff', &
     &     'L3/T', Infil2Pref_Q)/=0 ) CALL read_error(3, 'Infil2Pref_Q')

      IF ( declvar(MODNAME, 'DunnInterflow2Cap_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of cascading Dunnian runoff and interflow to HRUs', &
     &     'L3/T', DunnInterflow2Cap_Q)/=0 ) CALL read_error(3, 'DunnInterflow2Cap_Q')

      IF ( declvar(MODNAME, 'ActualET_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of actual evaporation from HRUs', &
     &     'L3/T', ActualET_Q)/=0 ) CALL read_error(3, 'ActualET_Q')

      IF ( declvar(MODNAME, 'SnowMelt_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of snowmelt', &
     &     'L3/T', SnowMelt_Q)/=0 ) CALL read_error(3, 'SnowMelt_Q')

      IF ( declvar(MODNAME, 'Ave_SoilDrainage2Unsat_Q', 'one', 1, 'double', &
     &     'Running average gravity drainage to the unsaturated and saturated zones', &
     &     'L3', Ave_SoilDrainage2Unsat_Q)/=0 ) CALL read_error(3, 'Ave_SoilDrainage2Unsat_Q')

      IF ( declvar(MODNAME, 'cum_pweqv', 'one', 1, 'double', &
     &     'Cumulative change in snowpack storage', &
     &     'L3', Cum_pweqv)/=0 ) CALL read_error(3, 'cum_pweqv')

      IF ( declvar(MODNAME, 'cum_soilstor', 'one', 1, 'double', &
     &     'Cumulative change in soil storage', &
     &     'L3', Cum_soilstor)/=0 ) CALL read_error(3, 'cum_soilstor')

      IF ( declvar(MODNAME, 'cum_uzstor', 'one', 1, 'double', &
     &     'Cumulative change in unsaturated storage', &
     &     'L3', Cum_uzstor)/=0 ) CALL read_error(3, 'cum_uzstor')

      IF ( declvar(MODNAME, 'cum_satstor', 'one', 1, 'double', &
     &     'Cumulative change in saturated storage', &
     &     'L3', Cum_satstor)/=0 ) CALL read_error(3, 'cum_satstor')

      IF ( declvar(MODNAME, 'rate_pweqv', 'one', 1, 'double', &
     &     'Change in snow pack storage', &
     &     'L3', Rate_pweqv)/=0 ) CALL read_error(3, 'rate_pweqv')

      IF ( declvar(MODNAME, 'rate_soilstor', 'one', 1, 'double', &
     &     'Change in soil storage', &
     &     'L3', Rate_soilstor)/=0 ) CALL read_error(3, 'rate_soilstor')

      IF ( declvar(MODNAME, 'rate_uzstor', 'one', 1, 'double', &
     &     'Change in unsaturated storage', &
     &     'L3', Rate_uzstor)/=0 ) CALL read_error(3, 'rate_uzstor')

      IF ( declvar(MODNAME, 'rate_satstor', 'one', 1, 'double', &
     &     'Change in saturated storage', &
     &     'L3', Rate_satstor)/=0 ) CALL read_error(3, 'rate_satstor')

      IF ( declvar(MODNAME, 'SnowPweqv_S', 'one', 1, 'double', &
     &     'Volume of water in snowpack storage', &
     &     'L3', SnowPweqv_S)/=0 ) CALL read_error(3, 'SnowPweqv_S')

      IF ( declvar(MODNAME, 'basinsoilstor', 'one', 1, 'double', &
     &     'Volume of soil moisture storage', &
     &     'L3', Basinsoilstor)/=0 ) CALL read_error(3, 'basinsoilstor')

      IF ( declvar(MODNAME, 'NetWellFlow_Q', 'one', 1, 'double', &
     &     'Net volumetric flow rate of groundwater injection or removal from wells ', &
     &     'L3/T', NetWellFlow_Q)/=0 ) CALL read_error(1, 'NetWellFlow_Q')

      IF ( declvar(MODNAME, 'BoundaryStreamFlow_Q', 'one', 1, 'double', &
     &     'Volumetric specified streamflow into the model domain to SFR', &
     &     'L3/T', BoundaryStreamFlow_Q)/=0 ) CALL read_error(1, 'BoundaryStreamFlow_Q')

      IF ( declparam(MODNAME, 'id_obsrunoff', 'one', 'integer', &
     &     '0', 'bounded', 'nobs', &
     &     'Index of measured streamflow station corresponding to the basin outlet', &
     &     'Index of measured streamflow station corresponding to the basin outlet', &
     &     'none')/=0 ) CALL read_error(1, 'id_obsrunoff')

9001  FORMAT('    Date      Water Bal    capstor   last_cap   gravstor  last_grav   snowstor   lastsnow', &
     &       '  intcpstor  lastintcp   impervst  lastimperv     dprst  lastdprst      gw2sz     precip', &
     &       '  interflow      sroff   lakeinsz  lakesroff   drainage      capET   impervET   canopyET', &
     &       '     snowET    swaleET    dprstET  fluxchnge')
      END FUNCTION gsfsumdecl

!***********************************************************************
!     gsfsuminit - Initialize basinsum module - get parameter values
!                set to zero
!***********************************************************************
      INTEGER FUNCTION gsfsuminit()
      USE GSFSUM
      USE GSFMODFLOW, ONLY: Acre_inches_to_mfl3, Mft_to_days
      USE GWFLAKMODULE, ONLY: TOTSTOR_LAK
      USE GWFSFRMODULE, ONLY: IRTFLG
      USE GLOBAL, ONLY: IUNIT
      USE PRMS_MODULE, ONLY: Init_vars_from_file
      USE PRMS_BASIN, ONLY: Active_area
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor
      USE PRMS_SRUNOFF, ONLY: Basin_dprst_volop, Basin_dprst_volcl
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL GSF_PRINT, gsflow_sum_restart, MODFLOW_SFR_GET_STORAGE
!***********************************************************************
      gsfsuminit = 0

      IF ( getparam(MODNAME, 'id_obsrunoff', 1, 'integer', Id_obsrunoff)/=0 ) CALL read_error(3, 'id_obsrunoff')
      IF ( Id_obsrunoff==0 ) Id_obsrunoff = 1

      Basin_convert = Acre_inches_to_mfl3*Active_area*Mft_to_days       !RGN 7/15/2015 added *Mft_to_days

      Have_wells = 0
      IF ( IUNIT(2)>0 .OR. IUNIT(50)>0 .OR. IUNIT(51)>0 .OR. &
     &     IUNIT(52)>0 ) Have_wells = 1

!  Set the volume budget indicies to -1 anytime "init" is called.
!  This will make "run" figure out the vbnm order.
      Vbnm_index = -1

!  Put a header on the output file when the model starts
      CALL GSF_PRINT()

      Rpt_count = 0
!  Initialize cumulative GSF report variables to 0.0
      Cumvol_precip = 0.0D0
      Cumvol_strmin = 0.0D0
      Cumvol_gwbndin = 0.0D0
      Cumvol_wellin = 0.0D0
      Cumvol_et = 0.0D0
      Cumvol_strmot = 0.0D0
      Cumvol_gwbndot = 0.0D0
      Cumvol_wellot = 0.0D0
      Cum_delstore = 0.0D0
      Cum_surfstor = 0.0D0
      Cum_soilstor = 0.0D0
      Cum_uzstor = 0.0D0
      Cum_satstor = 0.0D0
      Cum_pweqv = 0.0D0
      Cumvol_lakin = 0.0D0
      Cumvol_lakot = 0.0D0
      Cum_lakestor = 0.0D0
!      Cumvol_lakeppt = 0.0D0
!      Cumvol_lakeevap = 0.0D0
!      Cumvol_uzfet = 0.0D0
      IF ( IRTFLG>0 ) CALL MODFLOW_SFR_GET_STORAGE
      Lake_S = 0.0D0
      IF ( IUNIT(22)>0 ) Lake_S = TOTSTOR_LAK
      CALL BASIN_GET_STORAGE
      Last_Cap_S = Cap_S
      Last_Grav_S = Grav_S
      Last_Pref_S = Pref_S
      Last_basin_soil_moist = Basin_soil_moist
      Last_basin_ssstor = Basin_ssstor
      Last_Canopy_S = Canopy_S
      Last_Imperv_S = Imperv_S
      Last_SnowPweqv_S = SnowPweqv_S
      SwaleEvap_Q = 0.0D0
      Dprst_S = (Basin_dprst_volop + Basin_dprst_volcl) * Basin_convert
      Last_Dprst_S = Dprst_S ! need to deal with restart

      IF ( Init_vars_from_file>OFF ) THEN
        CALL gsflow_sum_restart(READ_INIT)
        RETURN
      ENDIF

! Added lake variables
      Rate_lakin = 0.0D0
      Rate_lakot = 0.0D0
      Rate_lakestor = 0.0D0
      Lake2Unsat_Q = 0.0D0
      LakeExchng2Sat_Q = 0.0D0
      Stream2Unsat_Q = 0.0D0

      Rate_Dprst_S = 0.0D0
      Rate_soilstor = 0.0D0
      Rate_uzstor = 0.0D0
      Rate_satstor = 0.0D0
      Rate_pweqv = 0.0D0
      SoilDrainage2Unsat_Q = 0.0D0
      Ave_SoilDrainage2Unsat_Q = 0.0D0
      Lake2Sat_Q = 0.0D0
      Lake_dS = 0.0D0
      Precip_Q = 0.0D0
      CapET_Q = 0.0D0
      ImpervEvap_Q = 0.0D0
      CanopyEvap_Q = 0.0D0
      Interflow2Stream_Q = 0.0D0
      SnowEvap_Q = 0.0D0
      LakeEvap_Q = 0.0D0
      DprstEvap_Q = 0.0D0
      LakePrecip_Q = 0.0D0
      DunnInterflow2Lake_Q = 0.0D0
      StreamOut_Q = 0.0D0
      PotGravDrn2Unsat_Q = 0.0D0
      Sat2Grav_Q = 0.0D0
      RechargeUnsat2Sat_Q = 0.0D0
      Basinseepout = 0.0D0
!     Basingwstor not computed, was the PRMS GWR storage which
!     is not available
      Basingwstor = 0.0D0
      Sroff2Stream_Q = 0.0D0
      HortSroff2Lake_Q = 0.0D0
      HortSroff2Stream_Q = 0.0D0
      Obs_strmflow = 0.0D0
      NetWellFlow_Q = 0.0D0
      BoundaryStreamFlow_Q = 0.0D0
      Basinprefflow = 0.0D0
      UnsatDrainageExcess_Q = 0.0D0
      DunnInterflow2Cap_Q = 0.0D0
      Uzf_et = 0.0D0
      UnsatET_Q = 0.0D0
      SatET_Q = 0.0D0
      Unsat_dS = 0.0D0
      Stream2Sat_Q = 0.0D0
      UnsatStream_dS = 0.0D0
      SatDisch2Stream_Q = 0.0D0
      UnsatStream_S = 0.0D0
      SatDisch2Lake_Q = 0.0D0
      Basinslowflow = 0.0D0
      Infil2Soil_Q = 0.0D0
      Basinrain = 0.0D0
      Basinsnow = 0.0D0
      SnowMelt_Q = 0.0D0
      DunnSroff2Stream_Q = 0.0D0
      Basinsm2gvr = 0.0D0
      Basingvr2sm = 0.0D0
      Basingvr2pfr = 0.0D0
      Infil2CapTotal_Q = 0.0D0
      Infil2Pref_Q = 0.0D0
      CapDrainage2Sat_Q = 0.0D0
      ActualET_Q = 0.0D0
      Stream_S = 0.0D0

      END FUNCTION gsfsuminit

!***********************************************************************
!     gsfsumrun - Computes summary values
!***********************************************************************
      INTEGER FUNCTION gsfsumrun()
      USE GSFSUM
      USE GSFMODFLOW, ONLY: Mfl3t_to_cfs, KKSTP, KKPER, Maxgziter
      USE GSFBUDGET, ONLY: NetBoundaryFlow2Sat_Q, Gw_bnd_in, Gw_bnd_out, Well_in, Basin_szreject, &
     &    Well_out, Stream_inflow, Basin_gw2sm, Sat_dS, StreamExchng2Sat_Q, Unsat_S, Sat_S, Basin_actetgw, Basin_fluxchange
!      USE GSFPRMS2MF, ONLY: Basin_reach_latflow
      USE GWFUZFMODULE, ONLY: UZTSRAT
      USE GWFSFRMODULE, ONLY: SFRUZBD, STRMDELSTOR_RATE, SFRRATIN, SFRRATOUT, IRTFLG, TOTSPFLOW
      USE GWFLAKMODULE, ONLY: TOTGWIN_LAK, TOTGWOT_LAK, TOTDELSTOR_LAK, &
     &    TOTSTOR_LAK, TOTWTHDRW_LAK, TOTRUNF_LAK, TOTSURFIN_LAK, &
     &    TOTSURFOT_LAK, TOTEVAP_LAK, TOTPPT_LAK
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: KKITER, Nobs, Timestep, Dprst_flag, Have_lakes
      USE PRMS_OBS, ONLY: Runoff, Runoff_units
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      USE PRMS_CLIMATEVARS, ONLY: Basin_ppt, Basin_rain, Basin_snow
      USE PRMS_FLOWVARS, ONLY: Basin_perv_et, Basin_swale_et, &
     &    Basin_lakeevap, Basin_soil_to_gw, Basin_ssflow, Basin_actet, &
     &    Basin_sroff, Basin_soil_moist, Basin_ssstor, Basin_cfs
      USE PRMS_SRUNOFF, ONLY: Basin_imperv_evap, &
     &    Basin_hortonian, Basin_hortonian_lakes, &
     &    Basin_infil, Basin_dprst_evap, Basin_dprst_volop, Basin_dprst_volcl
      USE PRMS_SNOW, ONLY: Basin_snowevap, Basin_snowmelt
      USE PRMS_INTCP, ONLY: Basin_intcp_evap
      USE PRMS_SOILZONE, ONLY: Basin_lakeprecip, Basin_dunnian, &
     &    Basin_slowflow, Basin_prefflow, Basin_lakeinsz, &
     &    Basin_cap_infil_tot, Basin_pref_flow_infil, Basin_sz2gw, &
     &    Basin_sm2gvr, Basin_gvr2sm, Basin_gvr2pfr, Basin_dunnian,  Basin_dncascadeflow
      IMPLICIT NONE
      INTRINSIC DBLE
! Local variables
      DOUBLE PRECISION :: obsq_cfs !, obsq_cms
!     REAL :: gw_out, basinreachlatflowm3
      DOUBLE PRECISION :: sz_bal, et, rnf, gvf, szin, szout, szdstor, hru_bal
!***********************************************************************
      gsfsumrun = 0

!*****Evapotranspiration
      ! basin_actet includes land HRU and lake ET

      Uzf_et = UZTSRAT(2) + UZTSRAT(7)
      UnsatET_Q = UZTSRAT(2)
      SatET_Q = UZTSRAT(7)
!      Cumvol_uzfet = Cumvol_uzfet + Uzf_et
!      et = Basin_actetgw*Basin_convert
!      IF (abs(uzf_et-et)>0.0001 ) &
!    &  print *, 'uzfet',uzf_et-et,uzf_et,UnsatET_Q,SatET_Q, &
!    & et,basin_actetgw,basin_actet,basin_perv_et

! convert PRMS variables acre-inches over the basin area (depth)
! to modflow length cubed (total volume)
      CapET_Q = Basin_perv_et*Basin_convert
      SwaleEvap_Q = Basin_swale_et*Basin_convert
      ImpervEvap_Q = Basin_imperv_evap*Basin_convert
      CanopyEvap_Q = Basin_intcp_evap*Basin_convert
      SnowEvap_Q = Basin_snowevap*Basin_convert
      LakeEvap_Q = Basin_lakeevap*Basin_convert
      DprstEvap_Q = Basin_dprst_evap*Basin_convert
!      IF ( Have_lakes==ACTIVE ) LakeEvap_Q = TOTEVAP_LAK
      ! sanity check
!      IF ( Have_lakes==ACTIVE ) THEN
!        IF ( ABS(LakeEvap_Q+TOTEVAP_LAK)>0.0001 ) PRINT *, &
!     &       'LAKE EVAP PROBLEM, MF lake evap not equal PRMS lake evap', &
!     &       LakeEvap_Q+TOTEVAP_LAK, LakeEvap_Q, TOTEVAP_LAK
!      ENDIF
      ActualET_Q = Basin_actet*Basin_convert

! STREAMFLOW

! convert basin_cfs from cfs over to modflow l3/t (volume per time step)
      StreamOut_Q = Basin_cfs/Mfl3t_to_cfs

      IF ( Nobs<1 ) THEN
        obsq_cfs = -1.0D0
        !obsq_cms = -1.0D0
      ELSE
        IF ( Runoff_units==1 ) THEN
          obsq_cfs = DBLE( Runoff(Id_obsrunoff) )/CFS2CMS_CONV
          !obsq_cms = DBLE( Runoff(Id_obsrunoff) )
        ELSE
          obsq_cfs = DBLE( Runoff(Id_obsrunoff) )
          !obsq_cms = obsq_cfs*CFS2CMS_CONV
        ENDIF
      ENDIF
      Obs_strmflow = obsq_cfs/Mfl3t_to_cfs

!rsr  basinreachlatflowm3 = Basin_reach_latflow/Mfl3t_to_cfs

! PRECIPITATION
      !Precip_Q includes precipitation on lakes
      Precip_Q = Basin_ppt*Basin_convert
!rsr, 3/16/2010 do not subtract lake evaporation, not included in lake in below
      Basinrain = Basin_rain*Basin_convert
      Basinsnow = Basin_snow*Basin_convert
      LakePrecip_Q = Basin_lakeprecip*Basin_convert

! SOIL/RUNOFF TOTALS

      !flows to streams
      Sroff2Stream_Q = Basin_sroff*Basin_convert  !Hortonian and Dunnian
      HortSroff2Stream_Q = Basin_hortonian*Basin_convert
      DunnSroff2Stream_Q = Basin_dunnian*Basin_convert
      Interflow2Stream_Q = Basin_ssflow*Basin_convert !slow + pref
      Basinslowflow = Basin_slowflow*Basin_convert
      Basinprefflow = Basin_prefflow*Basin_convert
      !flows to lakes
      HortSroff2Lake_Q = Basin_hortonian_lakes*Basin_convert
      !interflow + Dunnian soilzone
      DunnInterflow2Lake_Q = Basin_lakeinsz*Basin_convert

! SOIL/GW TOTALS
      !flows to soilzone
      !to capillary and preferential
      Infil2Soil_Q = Basin_infil*Basin_convert
      Sat2Grav_Q = Basin_gw2sm*Basin_convert !to gravity
      !infil plus cascading flow to capillary
      Infil2CapTotal_Q = Basin_cap_infil_tot*Basin_convert
      !portion of infil to preferential
      Infil2Pref_Q = Basin_pref_flow_infil*Basin_convert
      SnowMelt_Q = Basin_snowmelt*Basin_convert

      !flows from soilzone
      CapDrainage2Sat_Q = Basin_soil_to_gw*Basin_convert
      PotGravDrn2Unsat_Q = Basin_sz2gw*Basin_convert
      UnsatDrainageExcess_Q = Basin_szreject*Basin_convert

      !internal soilzone flows
      Basinsm2gvr = Basin_sm2gvr*Basin_convert !> field capacity
      Basingvr2sm = Basin_gvr2sm*Basin_convert !replenish soil moist
      Basingvr2pfr = Basin_gvr2pfr*Basin_convert !>pref_flow threshold
      !cascading slow, pref, and Dunnian
      DunnInterflow2Cap_Q = Basin_dncascadeflow*Basin_convert

!  Stuff from MODFLOW
      IF ( Vbnm_index(1)==-1 ) CALL MODFLOW_VB_DECODE(Vbnm_index)

      RechargeUnsat2Sat_Q = UZTSRAT(3)
      !?? doesn't match basin_gw2sm from budget
      Basinseepout = UZTSRAT(5)
      !print *, Sat2Grav_Q, Basinseepout, Sat2Grav_Q - Basinseepout !rsr, not sure why these don't match
      SoilDrainage2Unsat_Q = UZTSRAT(1)
      Unsat_dS = UZTSRAT(4)

      Stream2Sat_Q = SFRRATIN
      UnsatStream_dS = SFRUZBD(5)
      SatDisch2Stream_Q = SFRRATOUT
      UnsatStream_S = SFRUZBD(10)
      BoundaryStreamFlow_Q = TOTSPFLOW

      IF ( Timestep>1 ) THEN ! rsr, this could be incorrect for restart
        Ave_SoilDrainage2Unsat_Q = (Ave_SoilDrainage2Unsat_Q*(Timestep-1)) + PotGravDrn2Unsat_Q - &
     &                             UnsatDrainageExcess_Q + CapDrainage2Sat_Q
      ELSE
        Ave_SoilDrainage2Unsat_Q = PotGravDrn2Unsat_Q - UnsatDrainageExcess_Q + CapDrainage2Sat_Q
      ENDIF
      Ave_SoilDrainage2Unsat_Q = Ave_SoilDrainage2Unsat_Q/Timestep

      IF ( Have_lakes==ACTIVE ) THEN
        Lake_S = TOTSTOR_LAK
        SatDisch2Lake_Q = TOTGWIN_LAK 
        Lake2Sat_Q = TOTGWOT_LAK
        Lake_dS = TOTDELSTOR_LAK
        LakeExchng2Sat_Q = -Lake2Sat_Q - SatDisch2Lake_Q
      ENDIF

      IF ( IRTFLG>0 ) CALL MODFLOW_SFR_GET_STORAGE

      CALL BASIN_GET_STORAGE

! Moved Well calculations up here for printing to CSV file
      Cumvol_wellin = Cumvol_wellin + Well_in
      Rate_wellin = Well_in
      Cumvol_wellot = Cumvol_wellot + Well_out
      Rate_wellot = Well_out
      NetWellFlow_Q = Rate_wellin - Rate_wellot

      IF ( Print_debug==DEBUG_WB ) THEN
        et = Basin_perv_et + Basin_snowevap + Basin_imperv_evap + &
     &       Basin_intcp_evap + Basin_lakeevap + Basin_swale_et + &
     &       Basin_actetgw
        IF ( ABS(Basin_actet-et)>ERRCHK ) THEN
          WRITE (BALUNT, *) 'ET', Basin_actet - et, Basin_actet, et
          WRITE (BALUNT, *) 'ET', Basin_perv_et, Basin_snowevap, &
     &                      Basin_imperv_evap, Basin_intcp_evap, &
     &                      Basin_lakeevap, UnsatET_Q, SatET_Q, &
     &                      Basin_swale_et, Basin_actetgw, Basin_dprst_evap
        ENDIF

        rnf = Basin_hortonian + Basin_dunnian - Basin_sroff
        IF ( ABS(rnf)>ERRCHK ) WRITE (BALUNT, *) 'runoff', rnf, &
     &       Basin_hortonian, Basin_dunnian, Basin_sroff
        gvf = Basin_slowflow + Basin_prefflow - Basin_ssflow
        IF ( ABS(gvf)>ERRCHK ) WRITE (BALUNT, *) 'gravflow', gvf, &
     &       Basin_slowflow, Basin_prefflow, Basin_ssflow

        szin = Basin_infil + Basin_gw2sm + Basin_szreject
        szdstor = Last_basin_soil_moist + Last_basin_ssstor &
     &            - Basin_soil_moist - Basin_ssstor
        szout = Basin_sz2gw + Basin_ssflow + Basin_lakeinsz + &
     &          Basin_dunnian + Basin_perv_et + Basin_soil_to_gw + Basin_swale_et
        IF ( Basin_soil_moist>0.0D0 ) THEN
          IF ( ABS(szin-szout+szdstor)>ERRCHK ) THEN
            WRITE (BALUNT, 9002) Nowyear, Nowmonth, Nowday
            WRITE (BALUNT, *) 'SZ flow', szin-szout+szdstor, szin, &
     &                        szout, szdstor
            WRITE (BALUNT, *) 'SZ flow', Basin_infil, Basin_gw2sm, &
     &                        Basin_szreject, Last_basin_soil_moist, &
     &                        Last_basin_ssstor, Basin_soil_moist, &
     &                        Basin_ssstor, Basin_sz2gw, Basin_ssflow, &
     &                        Basin_lakeinsz, Basin_dunnian, &
     &                        Basin_perv_et, Basin_soil_to_gw, Basin_swale_et, Basin_fluxchange
            WRITE (BALUNT, *) KKITER, Maxgziter
          ENDIF
        ENDIF
        Last_basin_soil_moist = Basin_soil_moist
        Last_basin_ssstor = Basin_ssstor

        sz_bal = Cap_S - Last_Cap_S + Grav_S - Last_Grav_S - Sat2Grav_Q - Infil2Soil_Q &
     &           + Interflow2Stream_Q + DunnSroff2Stream_Q + DunnInterflow2Lake_Q &
     &           + SoilDrainage2Unsat_Q + CapET_Q + SwaleEvap_Q
        IF ( ABS(sz_bal)/Cap_S>ERRCHK ) WRITE (BALUNT, *) 'Possible soil zone water balance problem', sz_bal

        hru_bal = Cap_S - Last_Cap_S + Grav_S - Last_Grav_S  + SnowPweqv_S - Last_SnowPweqv_S &
     &            + Canopy_S - Last_Canopy_S + Imperv_S - Last_Imperv_S + Dprst_S - Last_Dprst_S &
     &            - Sat2Grav_Q - Precip_Q &
     &            + Interflow2Stream_Q + Sroff2Stream_Q + DunnInterflow2Lake_Q + HortSroff2Lake_Q + SoilDrainage2Unsat_Q &
     &            + CapET_Q + ImpervEvap_Q + CanopyEvap_Q + SnowEvap_Q + SwaleEvap_Q + DprstEvap_Q
        IF ( ABS(hru_bal)/Cap_S>ERRCHK ) WRITE (BALUNT, *) 'Possible HRU water balance problem', hru_bal
        WRITE (BALUNT, 9002) Nowyear, Nowmonth, Nowday, hru_bal, Cap_S, Last_Cap_S, Grav_S, Last_Grav_S, SnowPweqv_S, &
     &                       Last_SnowPweqv_S, Canopy_S, Last_Canopy_S, Imperv_S, Last_Imperv_S, Dprst_S, Last_Dprst_S, &
     &                       Sat2Grav_Q, Precip_Q, &
     &                       Interflow2Stream_Q, Sroff2Stream_Q, DunnInterflow2Lake_Q, HortSroff2Lake_Q, SoilDrainage2Unsat_Q, &
     &                       CapET_Q, ImpervEvap_Q, CanopyEvap_Q, SnowEvap_Q, SwaleEvap_Q, DprstEvap_Q, Basin_fluxchange
      ENDIF

      IF ( Gsf_rpt==1 ) THEN
        WRITE ( Balance_unt, 9001 ) Nowmonth, Nowday, Nowyear, StreamOut_Q, &
     &          HortSroff2Stream_Q, DunnSroff2Stream_Q, Interflow2Stream_Q, Stream2Unsat_Q, StreamExchng2Sat_Q, &
     &          Canopy_S, SnowPweqv_S, Imperv_S, Dprst_S, Cap_S, Grav_S, Unsat_S, Sat_S, UnsatStream_S, Lake_S, Stream_s, &
     &          Precip_Q, NetBoundaryFlow2Sat_Q, NetWellFlow_Q, BoundaryStreamFlow_Q, &
     &          CanopyEvap_Q, SnowEvap_Q, ImpervEvap_Q, DprstEvap_Q, CapET_Q, SwaleEvap_Q, UnsatET_Q, SatET_Q, LakeEvap_Q, &
     &          DunnInterflow2Lake_Q, HortSroff2Lake_Q, Lake2Unsat_Q, LakeExchng2Sat_Q, &
     &          SoilDrainage2Unsat_Q, Sat2Grav_Q, RechargeUnsat2Sat_Q, Infil2Soil_Q, KKITER
      ENDIF

!     DANGER strmin set to zero
!  RGN I think I fixed strmin
      Cumvol_precip = Cumvol_precip + Precip_Q*DELT    !9-10-2015. Multiplied all value by DELT to get cumulatives
      Rate_precip = Precip_Q
! RGN change Cumvol_strmin to include specified inflows
      Rate_strmin = Stream_inflow
      Cumvol_strmin = Cumvol_strmin + Rate_strmin*DELT
      Cumvol_gwbndin = Cumvol_gwbndin + Gw_bnd_in*DELT
      Rate_gwbndin = Gw_bnd_in
      Rate_et = ActualET_Q
      Cumvol_et = Cumvol_et + Rate_et*DELT
      Rate_strmot = StreamOut_Q
      Cumvol_strmot = Cumvol_strmot + Rate_strmot*DELT
      Cumvol_gwbndot = Cumvol_gwbndot + Gw_bnd_out*DELT
      Rate_gwbndot = Gw_bnd_out
 ! RGN added specified lake inflow/outflow and storage change
      IF ( Have_lakes==ACTIVE ) THEN
!        IF ( TOTWTHDRW_LAK>0.0 ) THEN
!          Rate_lakin = 0.0D0
!          Rate_lakot = TOTWTHDRW_LAK
!        ELSE
!          Rate_lakot = 0.0D0
!          Rate_lakin = TOTWTHDRW_LAK
!        END IF
        Rate_lakot = (-TOTSURFOT_LAK - TOTGWOT_LAK - TOTEVAP_LAK - TOTWTHDRW_LAK)/DELT   !RGN 7/22/15 these are volumes. added /DELT
!        Cumvol_lakeevap = Cumvol_lakeevap + TOTEVAP_LAK
        Rate_lakin = (TOTRUNF_LAK + TOTSURFIN_LAK + TOTGWIN_LAK + TOTPPT_LAK)/DELT        !RGN 7/22/15 these are volumes. added /DELT
!        Cumvol_lakeppt = Cumvol_lakeppt + TOTPPT_LAK
        Cumvol_lakot = Cumvol_lakot + Rate_lakot*DELT                                   !RGN 7/22/15 must convert to volume. added *DELT
        Cumvol_lakin = Cumvol_lakin + Rate_lakin*DELT                                   !RGN 7/22/15 must convert to volume. added *DELT
        Rate_lakestor = TOTDELSTOR_LAK/DELT     !RGN this is the actual change in lake storage
        Cum_lakestor = Cum_lakestor + Rate_lakestor*DELT
!        Cum_lakestor = TOTSTOR_LAK
!        print *, totsurfot_lak, totevap_lak, totgwot_lak, totrunf_lak
!        print *, totsurfin_lak, totgwin_lak
      END IF
      Rate_pweqv = SnowPweqv_S - Last_SnowPweqv_S
      Cum_pweqv = Cum_pweqv + Rate_pweqv*DELT

      Rate_surfstor = Canopy_S - Last_Canopy_S + &
     &                Imperv_S - Last_Imperv_S + Rate_pweqv
      Cum_surfstor = Cum_surfstor + Rate_surfstor*DELT

      Rate_soilstor = Cap_S - Last_Cap_S + &
     &                Grav_S - Last_Grav_S  !grav + pref
      Cum_soilstor = Cum_soilstor + Rate_soilstor*DELT

      Rate_uzstor = Unsat_dS + UnsatStream_dS
      Cum_uzstor = Cum_uzstor + Rate_uzstor*DELT

      Rate_satstor = Sat_dS/DELT    !RGN 5/6/2019
      Cum_satstor = Cum_satstor + Sat_dS  !RGN 5/6/2019
      Rate_delstore = Rate_surfstor + Rate_soilstor + Rate_satstor + &
     &                Rate_uzstor + Rate_lakestor + STRMDELSTOR_RATE

      IF ( Dprst_flag==1 ) THEN
        Dprst_S = (Basin_dprst_volop + Basin_dprst_volcl) * Basin_convert
        Rate_dprst_S = Dprst_S - Last_Dprst_S
        !Cum_dprststor = Cum_dprststor + Rate_dprst_S*DELT
        Rate_delstore = Rate_delstore + Basin_dprst_volop + Basin_dprst_volcl ! need rate
      ENDIF

      Cum_delstore = Cum_delstore + Rate_delstore*delt   !RGN 5/6/2019

      Rpt_count = Rpt_count + 1
      IF ( Rpt_count==Rpt_days ) THEN  !rpt_days default = 7
        CALL GSFSUMREPORT(Timestep, KKSTP, KKPER)
        Rpt_count = 0
      ENDIF

!  Save old values before computation of new ones
      Last_Canopy_S = Canopy_S
      Last_Imperv_S = Imperv_S
      Last_SnowPweqv_S = SnowPweqv_S
      Last_Cap_S = Cap_S
      Last_Grav_S = Grav_S
      Last_Pref_S = Pref_S
      Last_Dprst_S = Dprst_S

 9001 FORMAT (2(I2.2, '/'), I4, 38(',', E15.7), ',', I5)
 9002 FORMAT (I5, 2('/', I2.2), 1X, F0.3, 27(1X,F0.1))
      END FUNCTION gsfsumrun

!***********************************************************************
!     gsfsumclean - Computes summary values
!***********************************************************************
      INTEGER FUNCTION gsfsumclean()
      USE PRMS_CONSTANTS, ONLY: DEBUG_WB
      USE PRMS_MODULE, ONLY: Print_debug
      USE GSFSUM, ONLY: Balance_unt, Gsf_unt, Gsf_rpt, BALUNT
      IMPLICIT NONE
!***********************************************************************
      gsfsumclean = 0
      IF ( Print_debug==DEBUG_WB ) CLOSE (BALUNT)
      IF ( Gsf_rpt==1 ) CLOSE (Balance_unt)
      CLOSE (Gsf_unt)
      END FUNCTION gsfsumclean

!***********************************************************************
! Print headers for tables
!***********************************************************************
      SUBROUTINE GSF_PRINT()
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, ERROR_open_out
      USE PRMS_MODULE, ONLY: Print_debug
      USE GSFSUM, ONLY: Balance_unt, Gsf_unt, Csv_output_file, Rpt_days, &
     &    Gsflow_output_file, Gsf_rpt
      IMPLICIT NONE
      INTEGER, EXTERNAL :: control_integer, control_string, numchars
      EXTERNAL GSF_HEADERS, read_error, PRMS_open_output_file
! Local Variables
      INTEGER :: nc, ios
!***********************************************************************
      IF ( control_integer(Gsf_rpt, 'gsf_rpt')/=0 ) CALL read_error(5, 'gsf_rpt')
      IF ( Gsf_rpt==1 ) THEN  !gsf_rpt default = 1

        IF ( control_string(Csv_output_file, 'csv_output_file')/=0 ) CALL read_error(5, 'csv_output_file')
        IF ( Csv_output_file(:1)==' ' .OR. &
     &       Csv_output_file(:1)==CHAR(0) ) Csv_output_file = 'gsflow.csv'

        CALL PRMS_open_output_file(Balance_unt, Csv_output_file, 'csv_output_file', 0, ios)
        IF ( ios/=0 ) ERROR STOP ERROR_open_out
      ENDIF
 
! Open the GSF volumetric balance report file

      IF ( control_integer(Rpt_days, 'rpt_days')/=0 ) CALL read_error(5, 'rpt_days')
      IF ( Print_debug>DEBUG_less ) PRINT '(/,A,I4)', 'Water Budget print frequency is:', Rpt_days
      IF ( control_string(Gsflow_output_file, 'gsflow_output_file')/=0 ) CALL read_error(5, 'gsflow_output_file')
      IF ( Gsflow_output_file(:1)==' ' .OR. Gsflow_output_file(:1)==CHAR(0) ) Gsflow_output_file = 'gsflow.out'

      CALL PRMS_open_output_file(Gsf_unt, Gsflow_output_file, 'gsflow_output_file', 0, ios)
      IF ( ios/=0 ) ERROR STOP -3
      nc = numchars(Gsflow_output_file)
      IF ( Print_debug>DEBUG_less ) PRINT 9001, 'Writing GSFLOW Water Budget File: ', Gsflow_output_file(:nc)
      IF ( Gsf_rpt==1 ) THEN
        nc = numchars(Csv_output_file)
        IF ( Print_debug>DEBUG_less ) PRINT 9001, 'Writing GSFLOW CSV File: ', Csv_output_file(:nc)
        CALL GSF_HEADERS()
      ENDIF

 9001 FORMAT (/, 2A)
      END SUBROUTINE GSF_PRINT

!***********************************************************************
! Print headers for reports
!***********************************************************************
      SUBROUTINE GSF_HEADERS()
      USE GSFSUM, ONLY: Balance_unt
      IMPLICIT NONE
!***********************************************************************
      ! uzf_tot_stor = Unsat_S, modflow_tot_stor = Sat_S
      WRITE (Balance_unt, 9001)
 9001 FORMAT ('Date,StreamOut_Q', &
     &       ',HortSroff2Stream_Q,DunnSroff2Stream_Q,Interflow2Stream_Q,Stream2Unsat_Q,StreamExchng2Sat_Q', &
     &       ',Canopy_S,SnowPweqv_S,Imperv_S,Dprst_S,Cap_S,Grav_S,Unsat_S,Sat_S,UnsatStream_S,Lake_S,Stream_S', &
     &       ',Precip_Q,NetBoundaryFlow2Sat_Q,NetWellFlow_Q,BoundaryStreamFlow_Q', &
     &       ',CanopyEvap_Q,SnowEvap_Q,ImpervEvap_Q,DprstEvap_Q,CapET_Q,SwaleEvap_Q,UnsatET_Q,SatET_Q,LakeEvap_Q', &
     &       ',DunnInterflow2Lake_Q,HortSroff2Lake_Q,Lake2Unsat_Q,LakeExchng2Sat_Q', &
     &       ',SoilDrainage2Unsat_Q,Sat2Grav_Q,RechargeUnsat2Sat_Q,Infil2Soil_Q,KKITER')
      END SUBROUTINE GSF_HEADERS

!***********************************************************************
! Figure out the total basin_gsfstor
!***********************************************************************
      SUBROUTINE BASIN_GET_STORAGE()
      USE GSFSUM
      USE PRMS_FLOWVARS, ONLY: Basin_soil_moist, Basin_ssstor
      USE PRMS_SRUNOFF, ONLY: Basin_imperv_stor
      USE PRMS_INTCP, ONLY: Basin_intcp_stor
      USE PRMS_SNOW, ONLY: Basin_pweqv
      USE PRMS_SOILZONE, ONLY: Basin_pref_stor
      USE GSFBUDGET, ONLY: Sat_S, Unsat_S
      IMPLICIT NONE
!***********************************************************************
! LAND SURFACE STORAGE
      Canopy_S = Basin_intcp_stor*Basin_convert
      Imperv_S = Basin_imperv_stor*Basin_convert
      SnowPweqv_S = Basin_pweqv*Basin_convert

! SOIL STORAGE
      Cap_S = Basin_soil_moist*Basin_convert
      Grav_S = Basin_ssstor*Basin_convert
      Basinsoilstor = Grav_S + Cap_S
      Pref_S = Basin_pref_stor*Basin_convert

! MODFLOW STORAGE
!rsr, 4/18/2010 Grav_S includes gravity and preferential
      Basin_gsfstor = Sat_S + Unsat_S + Cap_S + &
     &                Grav_S + Canopy_S + &
     &                SnowPweqv_S + Imperv_S + Lake_S + Stream_S

      END SUBROUTINE BASIN_GET_STORAGE

!-------SUBROUTINE GSFSUMREPORT
      SUBROUTINE GSFSUMREPORT(Nstep, Kkstp, Kkper)
!***********************************************************************
!     PRINTS VOLUMETRIC BUDGET FOR ENTIRE GSFLOW MODEL
!***********************************************************************
      USE GSFSUM
      USE GWFSFRMODULE, ONLY: STRMDELSTOR_RATE, STRMDELSTOR_CUM, IRTFLG
      USE PRMS_MODULE, ONLY: KKITER, Have_lakes
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
      INTRINSIC ABS
      EXTERNAL GSFFMTNUM
! Arguments
      INTEGER :: Kkper, Kkstp, Nstep
! Local Variables
      DOUBLE PRECISION :: cumvol_in, cumvol_out, cumdiff, rate_in
      DOUBLE PRECISION :: ratediff, cum_error, rate_error, cum_percent
      DOUBLE PRECISION :: rate_out, rate_percent, temp
      CHARACTER(LEN=18) :: text1, text2, text3, text4, text5, text6
      CHARACTER(LEN=18) :: text7, text8, text9, text10, text11
      CHARACTER(LEN=18) :: val1, val2
!***********************************************************************
      text1 = '     PRECIPITATION'
      text2 = '        STREAMFLOW'
      text3 = '  GW BOUNDARY FLOW'
      text4 = '             WELLS'
      text5 = 'EVAPOTRANSPIRATION'
      text6 = '      LAND SURFACE'
      text7 = '         SOIL ZONE'
      text8 = '  UNSATURATED ZONE'
      text9 = '    SATURATED ZONE'
      text10 ='             LAKES'
      text11 ='           STREAMS'
      WRITE (Gsf_unt, 9001) Nowmonth, Nowday, Nowyear, Kkstp, Kkper, Nstep, KKITER
!
!1------PRINT CUMULATIVE VOLUMES AND RATES FOR INFLOW.
      WRITE (Gsf_unt, 9002)
!
!1A-----PRECIPITATION.
      CALL GSFFMTNUM(Cumvol_precip, val1)
      CALL GSFFMTNUM(Rate_precip, val2)
      WRITE (Gsf_unt, 9003) text1, val1, text1, val2
!1B-----STREAMFLOW.
      CALL GSFFMTNUM(Cumvol_strmin, val1)
      CALL GSFFMTNUM(Rate_strmin, val2)
      WRITE (Gsf_unt, 9003) text2, val1, text2, val2
!1C-----GROUND WATER FLOW.
      CALL GSFFMTNUM(Cumvol_gwbndin, val1)
      CALL GSFFMTNUM(Rate_gwbndin, val2)
      WRITE (Gsf_unt, 9003) text3, val1, text3, val2
!1D-----ALL WELLS.
      IF ( Have_wells==1 ) THEN
        CALL GSFFMTNUM(Cumvol_wellin, val1)
        CALL GSFFMTNUM(Rate_wellin, val2)
        WRITE (Gsf_unt, 9003) text4, val1, text4, val2
      ENDIF
!1E-----LAKES.
      !IF ( Have_lakes==ACTIVE ) THEN
      !  CALL GSFFMTNUM(Cumvol_lakin, val1)
      !  CALL GSFFMTNUM(Rate_lakin, val2)
      !  WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      !END IF
!
!2------PRINT CUMULATIVE VOLUMES AND RATES FOR OUTFLOW.
      WRITE (Gsf_unt, 9004)
!
!2A-----ALL ET.
      CALL GSFFMTNUM(Cumvol_et, val1)
      CALL GSFFMTNUM(Rate_et, val2)
      WRITE (Gsf_unt, 9003) text5, val1, text5, val2
!2B-----STREAMFLOW.
      CALL GSFFMTNUM(Cumvol_strmot, val1)
      CALL GSFFMTNUM(Rate_strmot, val2)
      WRITE (Gsf_unt, 9003) text2, val1, text2, val2
!2C-----GROUND WATER FLOW.
      CALL GSFFMTNUM(Cumvol_gwbndot, val1)
      CALL GSFFMTNUM(Rate_gwbndot, val2)
      WRITE (Gsf_unt, 9003) text3, val1, text3, val2
!2D-----ALL WELLS.
      IF ( Have_wells==1 ) THEN
        CALL GSFFMTNUM(Cumvol_wellot, val1)
        CALL GSFFMTNUM(Rate_wellot, val2)
        WRITE (Gsf_unt, 9003) text4, val1, text4, val2
      ENDIF
!2E-----LAKES.
      !IF ( Have_lakes==ACTIVE ) THEN
      !  CALL GSFFMTNUM(Cumvol_lakot, val1)
      !  CALL GSFFMTNUM(Rate_lakot, val2)
      !  WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      !END IF
!
!3------CUMULATIVE INFLOW MINUS CUMULATIVE OUTFLOW.
      cumvol_in = Cumvol_precip + Cumvol_strmin + Cumvol_gwbndin + Cumvol_wellin
      ! rsr, need lake pipeline in???
!     &            + Cumvol_lakin - Cumvol_lakeppt
      cumvol_out = Cumvol_et + Cumvol_strmot + Cumvol_gwbndot + &
     &             Cumvol_wellot
      ! rsr, need lake pipeline out???
!     &             + Cumvol_lakot - Cumvol_lakeevap - Cumvol_uzfet
      cumdiff = cumvol_in - cumvol_out
!
!4------INFLOW RATE MINUS OUTFLOW RATE.
!      rate_in = Rate_precip + Rate_strmin + Rate_gwbndin + Rate_wellin + Rate_lakin
!      rate_out = Rate_et + Rate_strmot + Rate_gwbndot + Rate_wellot +
!     &           Rate_lakot

      rate_in = Rate_precip + Rate_strmin + Rate_gwbndin + Rate_wellin
      rate_out = Rate_et + Rate_strmot + Rate_gwbndot + Rate_wellot
      ratediff = rate_in - rate_out
!
!5------PRINT CUMULATIVE AND RATE DIFFERENCES.
      CALL GSFFMTNUM(cumdiff, val1)
      CALL GSFFMTNUM(ratediff, val2)
      WRITE (Gsf_unt, 9005) val1, val2
!
!6-----TOTAL STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_delstore, val1)
      CALL GSFFMTNUM(Rate_delstore, val2)
      WRITE (Gsf_unt, 9006) val1, val2
!
!6A----SURFACE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_surfstor, val1)
      CALL GSFFMTNUM(Rate_surfstor, val2)
      WRITE (Gsf_unt, 9003) text6, val1, text6, val2
!
!6B----SOIL STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_soilstor, val1)
      CALL GSFFMTNUM(Rate_soilstor, val2)
      WRITE (Gsf_unt, 9003) text7, val1, text7, val2
!
!6C----UNSATURATED ZONE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_uzstor, val1)
      CALL GSFFMTNUM(Rate_uzstor, val2)
      WRITE (Gsf_unt, 9003) text8, val1, text8, val2
!
!6D----SATURATED ZONE STORAGE CHANGE.
      CALL GSFFMTNUM(Cum_satstor, val1)
      CALL GSFFMTNUM(Rate_satstor, val2)
      WRITE (Gsf_unt, 9003) text9, val1, text9, val2
!
!6E----LAKE STORAGE CHANGE.
      IF ( Have_lakes==ACTIVE ) THEN
        CALL GSFFMTNUM(Cum_lakestor, val1)
        CALL GSFFMTNUM(Rate_lakestor, val2)
        WRITE (Gsf_unt, 9003) text10, val1, text10, val2
      ENDIF
!
!6F----STREAM STORAGE CHANGE.
      IF ( IRTFLG>0 ) THEN
        temp = DBLE(STRMDELSTOR_CUM)
        CALL GSFFMTNUM(temp, val1)
        temp = DBLE(STRMDELSTOR_RATE)
        CALL GSFFMTNUM(temp, val2)
        WRITE (Gsf_unt, 9003) text11, val1, text11, val2
      END IF
!
!7------PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN IN MINUS
!       OUT AND STORAGE CHANGE.
      cum_error = cumdiff - Cum_delstore
      rate_error = ratediff - Rate_delstore
      CALL GSFFMTNUM(cum_error, val1)
      CALL GSFFMTNUM(rate_error, val2)
      WRITE (Gsf_unt, 9007) val1, val2
      cum_percent = 100.0D0*(cum_error/ &
     &              ((cumvol_in+cumvol_out+ABS(Cum_delstore))*0.5D0))
      rate_percent = 100.0D0*(rate_error/ &
     &               ((rate_in+rate_out+ABS(Rate_delstore))*0.5D0))
      IF ( ABS(cum_percent)>5.0D0 ) WRITE (Gsf_unt, *) &
     &     ' ***WARNING, CUMULATIVE VOLUME OFF > 5%'
      IF ( ABS(rate_percent)>3.0D0 ) THEN
        IF ( ABS(rate_percent)>10.0D0 ) THEN
          WRITE (Gsf_unt, *) ' ***CAUTION, FLUX RATES OFF > 10%'
        ELSE
          WRITE (Gsf_unt, *) ' ***WARNING, FLUX RATES OFF > 3%'
        ENDIF
      ENDIF
      WRITE (Gsf_unt, 9008) cum_percent, rate_percent

 9001 FORMAT ('1', /, ' SUMMARY VOLUMETRIC BUDGET FOR GSFLOW ', /, &
     &        ' DATE:', 2(I3.2), I5.4, 14X, 'CUMULATIVE TIME STEP:', I8, &
     &        /, ' MODFLOW STRESS PERIOD', I7, 5X, 'CURRENT TIME STEP:', &
     &        I8, 5X, 'ITERATIONS:', I8, //, 1X, 83('-'))
 9002 FORMAT (/, '   CUMULATIVE VOLUMES', 15X, 'L**3', 3X, &
     &        'RATES FOR THIS TIME STEP', 11X, 'L**3/T', /, 3X, 18('-'), &
     &        22X, 24('-'), //, 37X, 'IN', 41X, 'IN', /, 37X, '--', 41X, '--')
 9003 FORMAT (3X, A18, ' =', A18, 5X, A18, ' =', A18)
 9004 FORMAT (//, 36X, 'OUT', 40X, 'OUT', /, 36X, '---', 40X, '---') 
 9005 FORMAT (/, 3X, 'INFLOWS - OUTFLOWS =', A18, 5X, &
     &        'INFLOWS - OUTFLOWS =', A18, /, 13X, 8('-'), 35X, 8('-'))
 9006 FORMAT (/, ' TOTAL STORAGE CHANGE =', A18, 9X, 'STORAGE CHANGE =', &
     &        A18, /, 7X, 14('-'), 29X, 14('-'))
 9007 FORMAT (/, ' OVERALL BUDGET ERROR =', A18, 11X, 'BUDGET ERROR =', A18, /)
 9008 FORMAT (/, '  PERCENT DISCREPANCY =', F18.2, 3X, &
     &        ' PERCENT DISCREPANCY =', F18.2, ///)
!
!8------RETURN.
      END SUBROUTINE GSFSUMREPORT

!-------SUBROUTINE GSFFMTNUM
      SUBROUTINE GSFFMTNUM(Val, Strng)
!     ******************************************************************
!     FORMAT VALUE BASED ON VALUE SIZE
!     ******************************************************************
      IMPLICIT NONE
      INTRINSIC ABS, INT
! Arguments
      DOUBLE PRECISION, INTENT(IN) :: Val
      CHARACTER(LEN=*), INTENT(OUT) :: Strng
! Local Variables
      DOUBLE PRECISION, PARAMETER :: BIG = 1.0D07, SMALL = 0.01D0
      DOUBLE PRECISION :: absval
!***********************************************************************
      absval = ABS(Val)
      IF ( absval<1.0D-5 ) THEN
!       WRITE (Strng, '(I18)') INT(Val)
        Strng = ' '
      ELSEIF ( absval>BIG .OR. absval<SMALL ) THEN
        WRITE (Strng, '(1PD18.4)') Val
      ELSE
        WRITE (Strng, '(F18.2)') Val
      ENDIF
      END SUBROUTINE GSFFMTNUM

!***********************************************************************
! Figure out the total storage of the streams
!***********************************************************************
      SUBROUTINE MODFLOW_SFR_GET_STORAGE
      USE GSFSUM, ONLY: Stream_S
      USE GWFSFRMODULE, ONLY: STRM, NSTRM
      IMPLICIT NONE
! Local Variables
      INTEGER :: l
!      REAL :: depth, width, strlen
!***********************************************************************
      Stream_S = 0.0D0

      DO l = 1, NSTRM
!        depth = STRM(7, l)
!        width = STRM(5, l)
!        strlen = STRM(1, l)
        Stream_S = Stream_S + (STRM(7, l)*STRM(5, l)*STRM(1, l))
      ENDDO

      END SUBROUTINE MODFLOW_SFR_GET_STORAGE

!***********************************************************************
!     gsflow_sum_restart - write to or read from restart file
!***********************************************************************
      SUBROUTINE gsflow_sum_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE GSFSUM
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=10) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Rate_soilstor, Rate_uzstor, Basingwstor, &
     &          Rate_satstor, Basingvr2sm, Rate_pweqv, Lake_dS, Rate_lakin, Rate_lakot, Rate_lakestor, &
     &          SnowPweqv_S, Ave_SoilDrainage2Unsat_Q, Infil2Soil_Q, Basinsoilstor, Cap_S, &
     &          CapDrainage2Sat_Q, StreamOut_Q, SatDisch2Stream_Q, Rate_Dprst_S
        WRITE ( Restart_outunit ) Precip_Q, CapET_Q, ImpervEvap_Q, PotGravDrn2Unsat_Q, Sat2Grav_Q, Lake2Sat_Q, &
     &          Canopy_S, Imperv_S, Interflow2Stream_Q, Sroff2Stream_Q, Obs_strmflow, UnsatDrainageExcess_Q, UnsatET_Q, &
     &          SatET_Q, Uzf_et, RechargeUnsat2Sat_Q, Basinseepout, SoilDrainage2Unsat_Q, Unsat_dS, Basinrain, &
     &          Basinsnow, Basinslowflow
        WRITE ( Restart_outunit ) Basingvr2pfr, SnowEvap_Q, HortSroff2Stream_Q, HortSroff2Lake_Q, &
     &          DunnInterflow2Lake_Q, LakeEvap_Q, LakePrecip_Q, Grav_S, Basinprefflow, Pref_S, &
     &          Stream2Sat_Q, Basinsm2gvr, UnsatStream_dS, UnsatStream_S, SatDisch2Lake_Q, DunnSroff2Stream_Q
        WRITE ( Restart_outunit ) Infil2CapTotal_Q, Infil2Pref_Q, ActualET_Q, SnowMelt_Q, &
     &          CanopyEvap_Q, DunnInterflow2Cap_Q, NetWellFlow_Q, Stream2Unsat_Q, &
     &          Dprst_S, DprstEvap_Q, Lake2Unsat_Q, LakeExchng2Sat_Q, Lake_S, Stream_S, &
     &          BoundaryStreamFlow_Q, SwaleEvap_Q
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Rate_soilstor, Rate_uzstor, Basingwstor, &
     &         Rate_satstor, Basingvr2sm, Rate_pweqv, Lake_dS, Rate_lakin, Rate_lakot, Rate_lakestor, &
     &         SnowPweqv_S, Ave_SoilDrainage2Unsat_Q, Infil2Soil_Q, Basinsoilstor, Cap_S, &
     &         CapDrainage2Sat_Q, StreamOut_Q, SatDisch2Stream_Q, Rate_Dprst_S
        READ ( Restart_inunit ) Precip_Q, CapET_Q, ImpervEvap_Q, PotGravDrn2Unsat_Q, Sat2Grav_Q, Lake2Sat_Q, &
     &         Canopy_S, Imperv_S, Interflow2Stream_Q, Sroff2Stream_Q, Obs_strmflow, UnsatDrainageExcess_Q, UnsatET_Q, &
     &         SatET_Q, Uzf_et, RechargeUnsat2Sat_Q, Basinseepout, SoilDrainage2Unsat_Q, Unsat_dS, Basinrain, &
     &         Basinsnow, Basinslowflow
        READ ( Restart_inunit ) Basingvr2pfr, SnowEvap_Q, HortSroff2Stream_Q, HortSroff2Lake_Q, &
     &         DunnInterflow2Lake_Q, LakeEvap_Q, LakePrecip_Q, Grav_S, Basinprefflow, Pref_S, &
     &         Stream2Sat_Q, Basinsm2gvr, UnsatStream_dS, UnsatStream_S, SatDisch2Lake_Q, DunnSroff2Stream_Q
        READ ( Restart_inunit ) Infil2CapTotal_Q, Infil2Pref_Q, ActualET_Q, SnowMelt_Q, &
     &         CanopyEvap_Q, DunnInterflow2Cap_Q, NetWellFlow_Q, Stream2Unsat_Q, &
     &         Dprst_S, DprstEvap_Q, Lake2Unsat_Q, LakeExchng2Sat_Q, Lake_S, Stream_S, &
     &         BoundaryStreamFlow_Q, SwaleEvap_Q
      ENDIF
      END SUBROUTINE gsflow_sum_restart

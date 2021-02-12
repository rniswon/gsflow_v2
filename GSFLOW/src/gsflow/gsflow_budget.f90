!***********************************************************************
!     Perform the MODFLOW budget procedure for PRMS soil zone
!***********************************************************************
      MODULE GSFBUDGET
      USE PRMS_CONSTANTS, ONLY: ERROR_dim, NEARZERO, CLOSEZERO, ACTIVE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW Output Budget Summary'
      character(len=13), parameter :: MODNAME = 'gsflow_budget'
      character(len=*), parameter :: Version_gsflow_budget = '2021-01-12'
      INTEGER, SAVE :: Nreach
      INTEGER, SAVE :: Vbnm_index(14)
      DOUBLE PRECISION, SAVE :: Gw_bnd_in, Gw_bnd_out, Well_in, Well_out, Basin_actetgw, Basin_fluxchange
      REAL, SAVE, ALLOCATABLE :: Fluxchange(:)
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Total_pump, Total_pump_cfs, StreamExchng2Sat_Q, Stream2Unsat_Q, Sat_S
      DOUBLE PRECISION, SAVE :: Stream_inflow, Basin_gw2sm, NetBoundaryFlow2Sat_Q
      DOUBLE PRECISION, SAVE :: Unsat_S, Sat_dS, LakeExchng2Sat_Q, Lake2Unsat_Q, Basin_szreject
      REAL, SAVE, ALLOCATABLE :: Reach_cfs(:), Reach_wse(:), Streamflow_sfr(:), Seepage_reach_sfr(:), Seepage_segment_sfr(:)
      REAL, SAVE, ALLOCATABLE :: Gw2sm(:), Actet_gw(:), Actet_tot_gwsz(:), Gw_rejected(:)
!      REAL, SAVE, ALLOCATABLE :: Uzf_infil_map(:), Sat_recharge(:), Mfoutflow_to_gvr(:)
      END MODULE GSFBUDGET

!     ******************************************************************
!     Budget module to convert PRMS & MODFLOW states for use by GSFLOW
!     ******************************************************************
      INTEGER FUNCTION gsflow_budget()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, SAVE_INIT, READ_INIT, RUN, DECL, INIT, CLEAN
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: gsfbuddecl, gsfbudinit, gsfbudrun
      EXTERNAL :: gsflow_budget_restart
!***********************************************************************
      gsflow_budget = 0

      IF ( Process_flag==RUN ) THEN
        gsflow_budget = gsfbudrun()
      ELSEIF ( Process_flag==DECL ) THEN
        gsflow_budget = gsfbuddecl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL gsflow_budget_restart(READ_INIT)
        gsflow_budget = gsfbudinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL gsflow_budget_restart(SAVE_INIT)
      ENDIF

      END FUNCTION gsflow_budget

!***********************************************************************
!     gsfbuddecl - set up parameters
!   Declared Parameters
!     hru_area, gvr_hru_id, gvr_cell_id, lake_hru_id
!***********************************************************************
      INTEGER FUNCTION gsfbuddecl()
      USE GSFBUDGET
      USE PRMS_MODULE, ONLY: Nhru, Nsegment
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declvar, getdim
      EXTERNAL :: print_module, read_error
!***********************************************************************
      gsfbuddecl = 0

      CALL print_module(MODDESC, MODNAME, Version_gsflow_budget)

      Nreach = getdim('nreach')
      IF ( Nreach==-1 ) CALL read_error(6, 'nreach')

! Declared Variables
      IF ( declvar(MODNAME, 'NetBoundaryFlow2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate to the saturated zone along the external boundary'// &
     &     ' (negative value is flow out of model domain)', &
     &     'L3/T', NetBoundaryFlow2Sat_Q)/=0 ) CALL read_error(3, 'NetBoundaryFlow2Sat_Q')

      IF ( declvar(MODNAME, 'StreamExchng2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of exchange betweeen streams and the saturated'// &
     &     ' zone (value is equal to Strm2UZGW minus SatDisch2Stream_Q, where a'// &
     &     ' negative value indicates a net loss from streams)', &
     &     'L3/T', StreamExchng2Sat_Q)/=0 ) CALL read_error(3, 'StreamExchng2Sat_Q')

      IF ( declvar(MODNAME, 'Stream2Unsat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate betweeen streams and the unsaturated'// &
     &     ' zone (value is equal to Strm2UZGW minus SatDisch2Stream_Q, where a'// &
     &     ' negative value indicates a net loss from streams)', &
     &     'L3/T', Stream2Unsat_Q)/=0 ) CALL read_error(3, 'Stream2Unsat_Q')

      IF ( declvar(MODNAME, 'LakeExchng2Sat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate of exchange betweeen lakes and the saturated'// &
     &     ' zone (value is equal to Strm2UZGW minus SatDisch2Stream_Q, where a'// &
     &     ' negative value indicates a net loss from streams)', &
     &     'L3/T', LakeExchng2Sat_Q)/=0 ) CALL read_error(3, 'LakeExchng2Sat_Q')

      IF ( declvar(MODNAME, 'Lake2Unsat_Q', 'one', 1, 'double', &
     &     'Volumetric flow rate betweeen lakes and the unsaturated'// &
     &     ' zone (value is equal to Strm2UZGW minus SatDisch2Stream_Q, where a'// &
     &     ' negative value indicates a net loss from streams)', &
     &     'L3/T', Lake2Unsat_Q)/=0 ) CALL read_error(3, 'Lake2Unsat_Q')

      IF ( declvar(MODNAME, 'stream_inflow', 'one', 1, 'double', &
     &     'Specified volumetric stream inflow rate into model ', &
     &     'L3/T', Stream_inflow)/=0 ) CALL read_error(3, 'stream_inflow')

      IF ( declvar(MODNAME, 'Unsat_S', 'one', 1, 'double', &
     &     'Volume of water in the unsaturated zone', &
     &     'L3', Unsat_S)/=0 ) CALL read_error(3, 'Unsat_S')

      IF ( declvar(MODNAME, 'Sat_S', 'one', 1, 'double', &
     &     'Volume of water in the saturated zone', &
     &     'L3', Sat_S)/=0 ) CALL read_error(3, 'Sat_S')

      IF ( declvar(MODNAME, 'Sat_dS', 'one', 1, 'double', &
     &     'Change in saturated-zone storage', &
     &     'L3', Sat_dS)/=0 ) CALL read_error(3, 'Sat_dS')

      IF ( declvar(MODNAME, 'total_pump', 'one', 1, 'double', &
     &     'Total pumpage from all cells', &
     &     'L3', Total_pump)/=0 ) CALL read_error(3, 'total_pump')

      IF ( declvar(MODNAME, 'total_pump_cfs', 'one', 1, 'double', &
     &     'Total pumpage from all cells', &
     &     'cfs', Total_pump_cfs)/=0 ) CALL read_error(3, 'total_pump_cfs')

      ALLOCATE (Reach_cfs(Nreach))
      IF ( declvar(MODNAME, 'reach_cfs', 'nreach', Nreach, 'real', &
     &     'Stream flow leaving each stream reach', &
     &     'cfs', Reach_cfs)/=0 ) CALL read_error(3, 'reach_cfs')

      ALLOCATE (Reach_wse(Nreach))
      IF ( declvar(MODNAME, 'reach_wse', 'nreach', Nreach, 'real', &
     &     'Water surface elevation in each stream reach', &
     &     'L', Reach_wse)/=0 ) CALL read_error(3, 'reach_wse')

      IF ( declvar(MODNAME, 'basin_gw2sm', 'one', 1, 'double', &
     &     'Basin average water exfiltrated from unsaturated and saturated zones and added to soil zone', &
     &     'inches', Basin_gw2sm)/=0) CALL read_error(3, 'basin_gw2sm')

      IF ( declvar(MODNAME, 'basin_szreject', 'one', 1, 'double', &
     &     'Basin average recharge from SZ and rejected by UZF', &
     &     'inches', Basin_szreject)/=0) CALL read_error(3, 'basin_szreject')

      ALLOCATE (Gw2sm(Nhru))
      IF ( declvar(MODNAME, 'gw2sm', 'nhru', Nhru, 'real', &
     &     'HRU average water exfiltrated from groundwater model and added back to soil zone', &
     &     'inches', Gw2sm)/=0 ) CALL read_error(3, 'gw2sm')

      ALLOCATE (Actet_gw(Nhru))
      IF ( declvar(MODNAME, 'actet_gw', 'nhru', Nhru, 'real', &
     &     'Actual ET from each GW cell', &
     &     'inches', Actet_gw)/=0 ) CALL read_error(3, 'actet_gw')

      ALLOCATE (Actet_tot_gwsz(Nhru))
      IF ( declvar(MODNAME, 'actet_tot_gwsz', 'nhru', Nhru, 'real', &
     &     'Total actual ET from each GW cell and PRMS soil zone', &
     &     'inches', Actet_tot_gwsz)/=0 ) CALL read_error(3, 'actet_tot_gwsz')

      ALLOCATE (Streamflow_sfr(Nsegment))
      IF ( declvar(MODNAME, 'streamflow_sfr', 'nsegment', Nsegment, 'real', &
     &     'Streamflow as computed by SFR for each segment', &
     &     'cfs', Streamflow_sfr)/=0 ) CALL read_error(3, 'streamflow_sfr')

      ALLOCATE (Seepage_reach_sfr(Nreach))
      IF ( declvar(MODNAME, 'seepage_reach_sfr', 'nreach', Nreach, 'real', &
     &     'Seepage as computed by SFR for each reach', &
     &     'cfs', Seepage_reach_sfr)/=0 ) CALL read_error(3, 'seepage_reach_sfr')

      ALLOCATE (Seepage_segment_sfr(Nreach))
      IF ( declvar(MODNAME, 'seepage_segment_sfr', 'nsegment', Nsegment, 'real', &
     &     'Seepage as computed by SFR for each segment', &
     &     'cfs', Seepage_segment_sfr)/=0 ) CALL read_error(3, 'seepage_segment_sfr')

      ALLOCATE ( Gw_rejected(Nhru) )
      IF ( declvar(MODNAME, 'gw_rejected', 'nhru', Nhru, 'real', &
     &     'HRU average recharge rejected by UZF', 'inches', &
     &     Gw_rejected)/=0 ) CALL read_error(3, 'gw_rejected')

!      ALLOCATE ( Uzf_infil_map(Nhru) )
!      IF ( declvar(MODNAME, 'uzf_infil_map', 'nhru', Nhru, 'real', &
!     &     'HRU total gravity drainage to UZF cells', 'L3', &
!     &     Uzf_infil_map)/=0 ) CALL read_error(3, 'uzf_infil_map')

!      ALLOCATE ( Sat_recharge(Nhru) )
!      IF ( declvar(MODNAME, 'sat_recharge', 'nhru', Nhru, 'real', &
!     &     'HRU total recharge to the saturated zone', 'L3', &
!     &     Sat_recharge)/=0 ) CALL read_error(3, 'sat_recharge')

!      ALLOCATE ( Mfoutflow_to_gvr(Nhru) )
!      IF ( declvar(MODNAME, 'mfoutflow_to_gvr', 'nhru', Nhru, 'real', &
!     &     'MODFLOW total discharge and ET to each HRU', 'L3', &
!     &     Mfoutflow_to_gvr)/=0 ) CALL read_error(3, 'mfoutflow_to_gvr')

      END FUNCTION gsfbuddecl

!***********************************************************************
!     gsfbudinit - Initialize GSFBUDGET module - get parameter values
!***********************************************************************
      INTEGER FUNCTION gsfbudinit()
      USE PRMS_CONSTANTS, ONLY: OFF
      USE GSFBUDGET
      USE PRMS_MODULE, ONLY: Init_vars_from_file, Nhru
      USE GWFSFRMODULE, ONLY: NSTRM
      USE GLOBAL, ONLY: IUNIT
      USE GWFUZFMODULE, ONLY: UZTSRAT
      IMPLICIT NONE
      EXTERNAL :: MODFLOW_GET_STORAGE_BCF, MODFLOW_GET_STORAGE_LPF, MODFLOW_GET_STORAGE_UPW
!***********************************************************************
      gsfbudinit = 0

      IF ( Nreach/=NSTRM ) THEN
        PRINT *, 'ERROR, nreach must equal to NSTRM', Nreach, NSTRM
        ERROR STOP ERROR_dim
      ENDIF

      Reach_cfs = 0.0 ! dimension NSTRM
      Reach_wse = 0.0 ! dimension NSTRM
      IF ( Init_vars_from_file==OFF ) THEN
        Unsat_S = UZTSRAT(6)
        IF ( IUNIT(1)>0 ) CALL MODFLOW_GET_STORAGE_BCF()
        IF ( IUNIT(23)>0 ) CALL MODFLOW_GET_STORAGE_LPF()
        IF ( IUNIT(62)>0 ) CALL MODFLOW_GET_STORAGE_UPW()
        Sat_dS = 0.0D0
        StreamExchng2Sat_Q = 0.0D0
        Stream2Unsat_Q = 0.0D0
        LakeExchng2Sat_Q = 0.0D0
        Lake2Unsat_Q = 0.0D0
        Stream_inflow = 0.0D0
        Basin_gw2sm = 0.0D0
        Total_pump = 0.0D0
        Total_pump_cfs = 0.0D0
      ENDIF
!      Uzf_infil_map = 0.0 ! dimension nhru
!      Sat_recharge = 0.0 ! dimension nhru
!      Mfoutflow_to_gvr = 0.0 ! dimension nhru
      Gw2sm = 0.0 ! dimension nhru
      Actet_gw = 0.0 ! dimension nhru
      Actet_tot_gwsz = 0.0 ! dimension nhru
      Streamflow_sfr = 0.0 ! dimension nsegment
      Seepage_reach_sfr = 0.0 ! dimension nreach
      Seepage_segment_sfr = 0.0 ! dimension nsegment

!  Set the volume budget indicies to -1 anytime "init" is called.
!  This will make "run" figure out the vbnm order.
      Vbnm_index = -1
      ALLOCATE ( Fluxchange(Nhru) )
      Fluxchange = 0.0
      Basin_fluxchange = 0.0D0
      Basin_szreject = 0.0D0
      Gw_rejected = 0.0

      END FUNCTION gsfbudinit

!***********************************************************************
! Compute basin budget for GSFLOW
! adjust gravity flow storage with last gw2sm and gw_rejected
!***********************************************************************
      INTEGER FUNCTION gsfbudrun()
      USE GSFBUDGET
      USE GSFMODFLOW, ONLY: Mfq2inch_conv, Mfl2_to_acre, & !, Cellarea, &
     &    Mfvol2inch_conv, Mfl3t_to_cfs, Mfl_to_inch, Gwc_col, Gwc_row
!      USE GLOBAL, ONLY: IUNIT
!Warning, modifies Gw_rejected_grav
      USE GSFPRMS2MF, ONLY: Excess, Gw_rejected_grav
      USE PRMS_MODULE, ONLY: Nhrucell, Gvr_cell_id, Have_lakes !, Gvr_cell_pct, Print_debug
      USE GWFBASMODULE, ONLY: VBVL, DELT
      USE GWFUZFMODULE, ONLY: SEEPOUT, UZFETOUT, UZTSRAT, REJ_INF, GWET !, UZOLSFLX, UZFLWT
      USE GWFLAKMODULE, ONLY: EVAP, SURFA
!Warning, modifies Basin_gwflow_cfs, Basin_cfs, Basin_cms, Basin_stflow,
!                  Basin_ssflow_cfs, Basin_sroff_cfs
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Active_area, &
     &    Basin_area_inv, Hru_area, Lake_hru_id, Lake_area
      USE PRMS_FLOWVARS, ONLY: Basin_ssflow, Basin_lakeevap, Hru_actet, Basin_sroff, &
     &    Basin_actet, Basin_ssstor, Ssres_stor, Slow_stor, Basin_ssflow_cfs, Basin_sroff_cfs, Basin_gwflow_cfs
      USE PRMS_SET_TIME, ONLY: Cfs_conv
!Warning, modifies Basin_soil_moist, Basin_ssstor, and Gw2sm_grav
      USE PRMS_SOILZONE, ONLY: Pref_flow_stor, Gravity_stor_res, Hrucheck, Gvr_hru_id, &
     &    Basin_slstor, Gw2sm_grav, Gvr_hru_pct_adjusted
      IMPLICIT NONE
! Functions
      INTRINSIC :: ABS, SNGL
!      EXTERNAL MODFLOW_GET_STORAGE_BCF, MODFLOW_GET_STORAGE_LPF
!      EXTERNAL MODFLOW_GET_STORAGE_UPW
      EXTERNAL MODFLOW_VB_DECODE, getStreamFlow, getPump
!     EXTERNAL getHeads, print_date
! Local Variables
      INTEGER :: i, ihru, icell, irow, icol, ii, lake
      REAL :: flux_change, gwdisch, harea, inches_on_lake, pct
      DOUBLE PRECISION :: modflow_in, modflow_out, area_fac
!***********************************************************************
      gsfbudrun = 0

! adjust gravity flow storage using last gw discharge and rejected
      area_fac = Cfs_conv*Active_area
      Basin_ssflow_cfs = Basin_ssflow*area_fac
      Basin_sroff_cfs = Basin_sroff*area_fac
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        Gw2sm(i) = 0.0
        Gw_rejected(i) = 0.0
        Actet_gw(i) = 0.0
        Slow_stor(i) = 0.0 !shouldn't be reset if any cells of HRU inactive and HRU active
!        Uzf_infil_map(i) = 0.0
!        Sat_recharge(i) = 0.0
!        Mfoutflow_to_gvr(i) = 0.0
        Fluxchange(i) = 0.0
      ENDDO
      Streamflow_sfr = 0.0
      Seepage_reach_sfr = 0.0
      Seepage_segment_sfr = 0.0

      DO i = 1, Nhrucell
        ihru = Gvr_hru_id(i)
        icell = Gvr_cell_id(i)
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        pct = SNGL( Gvr_hru_pct_adjusted(i) )
        ! which calculations need gvr_cell_pct ???
!        Uzf_infil_map(ihru) = Uzf_infil_map(ihru) + UZOLSFLX(icol, irow)*Gvr_cell_pct(i)*pct*Cellarea(icell)*DELT
!        Sat_recharge(ihru) = Sat_recharge(ihru) + UZFLWT(icol, irow)*Gvr_hru_pct_adjusted(i)
!        Mfoutflow_to_gvr(ihru) = Mfoutflow_to_gvr(ihru) + (SEEPOUT(icol,irow)+GWET(icol,irow))*pct
        IF ( Hrucheck(ihru)/=1 ) CYCLE ! don't compute the rest if inactive or lake HRU, not sure about 3 variable prior
!-----------------------------------------------------------------------
! Add any excess infiltration to Gw_rejected array
! rejected can be added now as water would not have been used for
! any other purpose, it was water that could have been sent to MODFLOW
! if MODFLOW could accept it, includes (1) binning (Excess) and rejected
! infiltration due to (2) inactive cells and too many waves
! (Gw_rejected_grav(i)), (3) exceeding K and high gw head (gw
! discharging into GVR) (REJ_INF).
!-----------------------------------------------------------------------
        gwdisch = SEEPOUT(icol, irow)*Mfq2inch_conv(i)
! flux equals current minus last GW discharge used with soilzone, usually iteration before convergence
        flux_change = gwdisch - Gw2sm_grav(i) ! gw2sm_grav last set in gsflow_mf2prms with values last used by soilzone
        Fluxchange(Ihru) = Fluxchange(Ihru) + flux_change*pct
        IF ( ABS(flux_change)<CLOSEZERO ) flux_change = 0.0 ! assume round-off error, so set to zero
        !Gw_rejected_grav includes rejected soil_to_gw
        Gw_rejected_grav(i) = Gw_rejected_grav(i) + Excess(icell)*Mfl_to_inch + REJ_INF(icol, irow)*Mfq2inch_conv(i)
        Gw_rejected(ihru) = Gw_rejected(ihru) + Gw_rejected_grav(i)*pct
        Gw2sm_grav(i) = gwdisch ! set in mf2prms
        Gw2sm(ihru) = Gw2sm(ihru) + gwdisch*pct
        Gravity_stor_res(i) = Gravity_stor_res(i) + Gw_rejected_grav(i) + flux_change
        Slow_stor(ihru) = Slow_stor(ihru) + Gravity_stor_res(i)*pct
        Actet_gw(ihru) = Actet_gw(ihru) + (GWET(icol,irow) + UZFETOUT(icol, irow))*Mfvol2inch_conv(i)*pct
      ENDDO

      Basin_ssstor = 0.0D0
      Basin_gw2sm = 0.0D0
      Basin_szreject = 0.0D0
      Basin_lakeevap = 0.0D0
      Basin_actetgw = 0.0D0
      Basin_actet = 0.0D0
      Basin_slstor = 0.0D0
      Basin_fluxchange = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        harea = Hru_area(i)
        IF ( Have_lakes==ACTIVE ) THEN
!-----------------------------------------------------------------------
! Get actual et from lakes
!-----------------------------------------------------------------------
          IF ( Hru_type(i)==2 ) THEN
            lake = Lake_hru_id(i)
            !EVAP in mfl3/dt   SURFA in MFL2/dt
            IF ( SURFA(lake)>NEARZERO ) THEN
              inches_on_lake = SNGL(EVAP(lake))*DELT/SNGL(SURFA(lake)*Mfl_to_inch)                         !RGN 5/23/15 added *DELT for time units other than days.         
              Hru_actet(i) = inches_on_lake*SNGL(SURFA(lake)*Mfl2_to_acre/Lake_area(lake))
            ELSE
              Hru_actet(i) = 0.0
            ENDIF
            ! does not include any ET from UZF, i.e., dry areas in lake
            Actet_tot_gwsz(i) = Hru_actet(i)
            Basin_lakeevap = Basin_lakeevap + Hru_actet(i)*harea
            Basin_actet = Basin_actet + Hru_actet(i)*harea
            CYCLE
          ENDIF
        ENDIF

        Actet_tot_gwsz(i) = Hru_actet(i) + Actet_gw(i)
        !rsr, need to adjust hru_actet for UZF
        Hru_actet(i) = Actet_tot_gwsz(i)
        Basin_actet = Basin_actet + Hru_actet(i)*harea
        Basin_actetgw = Basin_actetgw + Actet_gw(i)*harea
        Basin_gw2sm = Basin_gw2sm + Gw2sm(i)*harea
        Ssres_stor(i) = Slow_stor(i) + Pref_flow_stor(i)
        Basin_ssstor = Basin_ssstor + Ssres_stor(i)*harea
        Basin_szreject = Basin_szreject + Gw_rejected(i)*harea
        Basin_slstor = Basin_slstor + Slow_stor(i)*harea
        Basin_fluxchange = Basin_fluxchange + Fluxchange(i)*harea
      ENDDO

      Basin_actet = Basin_actet*Basin_area_inv
      Basin_actetgw = Basin_actetgw*Basin_area_inv
      Basin_ssstor = Basin_ssstor*Basin_area_inv
      Basin_gw2sm = Basin_gw2sm*Basin_area_inv
      Basin_szreject = Basin_szreject*Basin_area_inv
      Basin_lakeevap = Basin_lakeevap*Basin_area_inv
      Basin_slstor = Basin_slstor*Basin_area_inv
      Basin_fluxchange = Basin_fluxchange*Basin_area_inv

      !IF ( IUNIT(1)>0 ) CALL MODFLOW_GET_STORAGE_BCF()
      !IF ( IUNIT(23)>0 ) CALL MODFLOW_GET_STORAGE_LPF()
      !IF ( IUNIT(62)>0 ) CALL MODFLOW_GET_STORAGE_UPW()

      IF ( Vbnm_index(1)==-1 ) CALL MODFLOW_VB_DECODE(Vbnm_index)
      Sat_dS = (VBVL(4,Vbnm_index(12)) - VBVL(3,Vbnm_index(12)))*DELT
      Sat_S = Sat_S + Sat_dS

      Unsat_S = UZTSRAT(6)

!  Stuff from MODFLOW

      modflow_in = 0.0D0
      Gw_bnd_in = 0.0D0
      Well_in = 0.0D0
      IF ( Vbnm_index(1)/=-1 ) THEN ! constant heads
        modflow_in = modflow_in + VBVL(3, Vbnm_index(1))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(1))
      ENDIF

      IF ( Vbnm_index(3)/=-1 ) THEN ! head dep bounds
        modflow_in = modflow_in + VBVL(3, Vbnm_index(3))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(3))
      ENDIF

      IF ( Vbnm_index(4)/=-1 ) THEN ! specified heads
        modflow_in = modflow_in + VBVL(3, Vbnm_index(4))
        Gw_bnd_in = Gw_bnd_in + VBVL(3, Vbnm_index(4))
      ENDIF

      IF ( Vbnm_index(5)/=-1 ) THEN ! wells
!        modflow_in = modflow_in + VBVL(3, Vbnm_index(5))
        Well_in = Well_in + VBVL(3, Vbnm_index(5))
      ENDIF

      IF ( Vbnm_index(6)/=-1 ) THEN ! multi node wells (MNW1)
!        modflow_in = modflow_in + VBVL(3, Vbnm_index(6))
        Well_in = Well_in + VBVL(3, Vbnm_index(6))
      ENDIF

      IF ( Vbnm_index(14)/=-1 ) THEN ! multi node wells (MNW2)
!        modflow_in = modflow_in + VBVL(3, Vbnm_index(14))
        Well_in = Well_in + VBVL(3, Vbnm_index(14))
      ENDIF

      modflow_out = 0.0D0
      Gw_bnd_out = 0.0D0
      Well_out = 0.0D0
      IF ( Vbnm_index(1)/=-1 ) THEN ! constant heads
        modflow_out = modflow_out + VBVL(4, Vbnm_index(1))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(1))
      ENDIF

      IF ( Vbnm_index(2)/=-1 ) THEN ! drains
        modflow_out = modflow_out + VBVL(4, Vbnm_index(2))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(2))
      ENDIF

      IF ( Vbnm_index(3)/=-1 ) THEN ! head dep bounds
        modflow_out = modflow_out + VBVL(4, Vbnm_index(3))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(3))
      ENDIF

      IF ( Vbnm_index(4)/=-1 ) THEN ! specified heads
        modflow_out = modflow_out + VBVL(4, Vbnm_index(4))
        Gw_bnd_out = Gw_bnd_out + VBVL(4, Vbnm_index(4))
      ENDIF

      IF ( Vbnm_index(5)/=-1 ) THEN ! wells
!        modflow_out = modflow_out + VBVL(4, Vbnm_index(5))
        Well_out = Well_out + VBVL(4, Vbnm_index(5))
      ENDIF

      IF ( Vbnm_index(6)/=-1 ) THEN ! multi node wells (MNW1)
!        modflow_out = modflow_out + VBVL(4, Vbnm_index(6))
        Well_out = Well_out + VBVL(4, Vbnm_index(6))
      ENDIF

       IF ( Vbnm_index(14)/=-1 ) THEN ! multi node wells (MNW2)
!        modflow_out = modflow_out + VBVL(4, Vbnm_index(14))
        Well_out = Well_out + VBVL(4, Vbnm_index(14))
      ENDIF

      NetBoundaryFlow2Sat_Q = modflow_in - modflow_out

      CALL getStreamFlow()

      Basin_gwflow_cfs = StreamExchng2Sat_Q*Mfl3t_to_cfs

!     CALL getHeads()

      CALL getPump()

      END FUNCTION gsfbudrun

!***********************************************************************
! Figure out the total storage of the cells in MODFLOW
! written by markstro but hijacked from MODFLOW subroutine SGWF1BCF6S
! Use Sat_S for display purposes only, don't use in budget.
!***********************************************************************
      SUBROUTINE MODFLOW_GET_STORAGE_BCF()
      USE GSFBUDGET, ONLY: Sat_S
      USE GLOBAL, ONLY: NCOL, NROW, NLAY, IBOUND, BOTM, HNEW, LBOTM
      USE GWFBASMODULE, ONLY: DELT
      USE GWFBCFMODULE, ONLY: LAYCON, SC1, SC2
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, j, k, kt, lc
      DOUBLE PRECISION :: tled, top, bot, rho, storage, head
!***********************************************************************
      tled = 1.0D0/DELT
      Sat_S = 0.0D0

!5------LOOP THROUGH EVERY CELL IN THE GRID.
      kt = 0
      DO k = 1, NLAY
        lc = LAYCON(k)
        IF ( lc==3 .OR. lc==2 ) kt = kt + 1
        DO i = 1, NROW
          DO j = 1, NCOL

!6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
            IF ( IBOUND(j, i, k)>0 ) THEN
              head = HNEW(j, i, k)
              top = BOTM(j, i, LBOTM(k)-1)
              bot = BOTM(j, i, LBOTM(k))

!7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
              IF ( lc==3 .OR. lc==2 ) THEN
                rho = SC2(j, i, kt)
!7A----TWO STORAGE CAPACITIES.
!                IF ( head>top ) THEN
!                  rho = SC1(j, i, k)
!                ELSE
!                  rho = SC2(j, i, kt)
!                ENDIF
              ELSE
!7B----ONE STORAGE CAPACITY.
                rho = SC1(j, i, k)
              ENDIF
              IF ( head>=top ) THEN
                storage = rho*(top-bot)*tled
              ELSE
                storage = rho*(head-bot)*tled
              ENDIF
              Sat_S = Sat_S + storage
            ENDIF

          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE MODFLOW_GET_STORAGE_BCF

!***********************************************************************
! Figure out the total storage of the cells in MODFLOW
! written by markstro but hijacked from MODFLOW subroutine SGWF1LPF1S
! Use Sat_S for display purposes only, don't use in budget.
!***********************************************************************
      SUBROUTINE MODFLOW_GET_STORAGE_LPF()
      USE GSFBUDGET, ONLY: Sat_S
      USE GLOBAL, ONLY: NCOL, NROW, NLAY, IBOUND, BOTM, HNEW, LBOTM
      USE GWFBASMODULE, ONLY: DELT
      USE GWFLPFMODULE, ONLY: LAYTYP, SC1, SC2
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, j, k, kt, lc
      DOUBLE PRECISION :: tled, top, bot, rho, storage, head
!***********************************************************************
      tled = 1.0D0/DELT
      Sat_S = 0.0D0

!5------LOOP THROUGH EVERY CELL IN THE GRID.
      kt = 0
      DO k = 1, NLAY
        lc = LAYTYP(k)
        IF ( lc/=0 ) kt = kt + 1
        DO i = 1, NROW
          DO j = 1, NCOL

!6------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
            IF ( IBOUND(j, i, k)>0 ) THEN
              head = HNEW(j, i, k)
              top = BOTM(j, i, LBOTM(k)-1)
              bot = BOTM(j, i, LBOTM(k))

!7-----CHECK LAYER TYPE TO SEE IF ONE STORAGE CAPACITY OR TWO.
              IF ( lc/=0 ) THEN
!7A----TWO STORAGE CAPACITIES.
!  markstro - always use specific yield
                rho = SC2(j, i, kt)
!               IF ( head>top ) THEN
!                 rho = SC1(j, i, k)
!               ELSE
!                 rho = SC2(j, i, kt)
!               ENDIF
              ELSE
!7A----ONE STORAGE CAPACITY.
                rho = SC1(j, i, k)
              ENDIF
              IF ( head>=top ) THEN
                storage = rho*(top-bot)*tled
              ELSE
                storage = rho*(head-bot)*tled
              ENDIF
              Sat_S = Sat_S + storage
            ENDIF

          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE MODFLOW_GET_STORAGE_LPF

!***********************************************************************
! Figure out the total storage of the cells in MODFLOW
! written by markstro but hijacked from MODFLOW subroutine SGWF1LPF1S
! Use Sat_S for display purposes only, don't use in budget.
!***********************************************************************
      SUBROUTINE MODFLOW_GET_STORAGE_UPW()
      USE GSFBUDGET, ONLY: Sat_S
      USE PRMS_CONSTANTS, ONLY: NEARZERO
      USE GLOBAL, ONLY: NCOL, NROW, NLAY, IBOUND, BOTM, HNEW, LBOTM, HOLD
      USE GWFBASMODULE, ONLY: DELT
      USE GWFUPWMODULE, ONLY: SC1, SC2UPW, Sn
      USE GWFNWTMODULE,ONLY: Icell
      IMPLICIT NONE
! Functions
      INTRINSIC ABS
! Local Variables
      INTEGER :: i, j, k, kt, IJ
      DOUBLE PRECISION :: tled, rho1, rho2
      DOUBLE PRECISION :: ZERO, ONE, HSING, HLD, tp, bt, thick
      DOUBLE PRECISION :: strg
!***********************************************************************
      tled = 1.0D0/DELT
      Sat_S = 0.0D0

      ZERO=0.0D0
      ONE=1.0D0
      TLED=ONE/DBLE(DELT)
!C
!C1------LOOP THROUGH EVERY CELL IN THE GRID.
      KT=0
      DO 300 K=1,NLAY
      DO 300 I=1,NROW
      DO 300 J=1,NCOL
!C
!C2------SKIP NO-FLOW AND CONSTANT-HEAD CELLS.
      IF(IBOUND(J,I,K).LE.0) GO TO 300
      HSING=HNEW(J,I,K)
      HLD = DBLE(HOLD(J,I,K))
!C
!C3----TWO STORAGE CAPACITIES, USE YIELD (SC2) IF SPECIFIED.
      TP=dble(BOTM(J,I,LBOTM(K)-1))
      BT=dble(BOTM(J,I,LBOTM(K)))
      THICK = (TP-BT)
      RHO1 = dble(SC1(J,I,K))
      RHO2 = dble(SC2UPW(J,I,K))
      if ( RHO2 < RHO1 ) RHO2 = RHO1
      ij = Icell(J,I,K)
      STRG = THICK*RHO2*Sn(ij)
      Sat_S = Sat_S + STRG
  300 CONTINUE
      END SUBROUTINE MODFLOW_GET_STORAGE_UPW

!***********************************************************************
! Decode the MODFLOW VBNM array
!***********************************************************************
      SUBROUTINE MODFLOW_VB_DECODE(Vbnm_index)
      USE GWFBASMODULE, ONLY: VBNM, MSUM
      IMPLICIT NONE
! Arguments
      INTEGER :: Vbnm_index(14)
! Local Variables
      INTEGER :: i
!***********************************************************************
!  Stuff from MODFLOW
      DO i = 1, MSUM - 1
        IF ( VBNM(i)=='   CONSTANT HEAD' ) Vbnm_index(1) = i
        IF ( VBNM(i)=='          DRAINS' ) Vbnm_index(2) = i
        IF ( VBNM(i)==' HEAD DEP BOUNDS' ) Vbnm_index(3) = i
        IF ( VBNM(i)==' SPECIFIED FLOWS' ) Vbnm_index(4) = i
        IF ( VBNM(i)=='           WELLS' ) Vbnm_index(5) = i
        IF ( VBNM(i)=='             MNW' ) Vbnm_index(6) = i
        IF ( VBNM(i)=='    UZF RECHARGE' ) Vbnm_index(7) = i
        IF ( VBNM(i)=='           GW ET' ) Vbnm_index(8) = i
        IF ( VBNM(i)==' SURFACE LEAKAGE' ) Vbnm_index(9) = i
        IF ( VBNM(i)=='  STREAM LEAKAGE' ) Vbnm_index(10) = i
        IF ( VBNM(i)=='   LAKE  SEEPAGE' ) Vbnm_index(11) = i
        IF ( VBNM(i)=='         STORAGE' ) Vbnm_index(12) = i
        IF ( VBNM(i)=='INTERBED STORAGE' ) Vbnm_index(13) = i
        IF ( VBNM(i)=='            MNW2' ) Vbnm_index(14) = i
      ENDDO

      END SUBROUTINE MODFLOW_VB_DECODE

!***********************************************************************
!***********************************************************************
      SUBROUTINE getStreamFlow()
      USE GSFBUDGET, ONLY: Reach_cfs, Reach_wse, StreamExchng2Sat_Q, &
     &    Stream_inflow, Streamflow_sfr, Seepage_reach_sfr, Seepage_segment_sfr
      USE GSFMODFLOW, ONLY: Mfl3t_to_cfs
      USE GWFSFRMODULE, ONLY: STRM, IOTSG, NSS, SGOTFLW, SFRRATOUT, &
     &    TOTSPFLOW, NSTRM, SFRRATIN, ISTRM
      USE PRMS_FLOWVARS, ONLY: Basin_cfs, Basin_cms, Basin_stflow_out
      USE PRMS_CONSTANTS, ONLY: CFS2CMS_CONV, ACTIVE
      USE PRMS_MODULE, ONLY: Ag_package_active
      USE PRMS_SET_TIME, ONLY: Cfs2inches
      USE GWFAGMODULE, ONLY:  NUMIRRDIVERSIONSP,IRRSEG
      IMPLICIT NONE
      INTRINSIC :: SNGL
! Local Variables
      INTEGER :: i, itemp, j, first_reach, nrch
      REAL :: Mfl3t_to_cfs_sngl
!***********************************************************************
      Mfl3t_to_cfs_sngl = SNGL(Mfl3t_to_cfs)
      DO i = 1, NSTRM
! Reach_cfs and reach_wse are not used except to be available for output
        Reach_cfs(i) = STRM(9, i)*Mfl3t_to_cfs_sngl
        Reach_wse(i) = STRM(15, i)
      ENDDO

! Total streamflow out of basin for all streams leaving model area.
! Total specified streamflow into model area.
! Ignore segments that are used for irrigation
      Basin_cfs = 0.0D0
      Stream_inflow = 0.0D0
      first_reach = 1
      DO i = 1, NSS
        itemp = 0
        IF ( Ag_package_active==ACTIVE ) THEN
          DO j = 1, NUMIRRDIVERSIONSP
            IF ( i == IRRSEG(J) ) itemp = IRRSEG(J)
          END DO
        END IF
        IF ( IOTSG(i)==0 .and. itemp == 0 ) Basin_cfs = Basin_cfs + SGOTFLW(i)
        Streamflow_sfr(i) = SGOTFLW(i)*Mfl3t_to_cfs_sngl
        nrch = ISTRM(5, i)
        DO j = first_reach, nrch + first_reach - 1
          Seepage_reach_sfr(i) = Seepage_reach_sfr(i) + STRM(11,j)*SNGL( Mfl3t_to_cfs )
        ENDDO
        Seepage_segment_sfr(i) = Seepage_reach_sfr(i)/FLOAT(nrch)
        first_reach = first_reach + nrch
      ENDDO 
      IF ( TOTSPFLOW<0.0 ) THEN
        Basin_cfs = Basin_cfs + TOTSPFLOW
      ELSE
! RGN added specified inflows and outflows from SFR. 
        Stream_inflow = Stream_inflow + TOTSPFLOW
      END IF
! RGN added next line.
      StreamExchng2Sat_Q = SFRRATIN - SFRRATOUT
      Basin_cfs = Basin_cfs*Mfl3t_to_cfs
      Basin_cms = Basin_cfs*CFS2CMS_CONV
      Basin_stflow_out = Basin_cfs*Cfs2inches

      END SUBROUTINE getStreamFlow

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      SUBROUTINE getPump()
      USE GSFBUDGET, ONLY: Total_pump, Total_pump_cfs, Vbnm_index
      USE GSFMODFLOW, ONLY: Mfl3t_to_cfs
      USE GWFBASMODULE, ONLY: VBVL
      IMPLICIT NONE

!***********************************************************************
      Total_pump = 0.0D0

      ! wells
      IF ( Vbnm_index(5)/=-1 ) Total_pump = Total_pump - VBVL(4, Vbnm_index(5))

      ! multi node wells (MNW1)
      IF ( Vbnm_index(6)/=-1 ) Total_pump = Total_pump - VBVL(4, Vbnm_index(6))

      ! multi node wells (MNW2)
      IF ( Vbnm_index(14)/=-1 ) Total_pump = Total_pump - VBVL(4, Vbnm_index(14))

      ! wells
      IF ( Vbnm_index(5)/=-1 ) Total_pump = Total_pump + VBVL(3, Vbnm_index(5))

      ! multi node wells (MNW1)
      IF ( Vbnm_index(6)/=-1 )Total_pump = Total_pump + VBVL(3, Vbnm_index(6))

      ! multi node wells (MNW2)
      IF ( Vbnm_index(14)/=-1 ) Total_pump = Total_pump + VBVL(3, Vbnm_index(14))

      Total_pump_cfs = Total_pump * Mfl3t_to_cfs

      END SUBROUTINE getPump

!***********************************************************************
!     gsflow_budget_restart - write to or read from restart file
!***********************************************************************
      SUBROUTINE gsflow_budget_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE GSFBUDGET
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=13) :: module_name
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Total_pump, Total_pump_cfs, Unsat_S, Sat_S, &
     &          Sat_dS, StreamExchng2Sat_Q, Stream2Unsat_Q, Stream_inflow, &
     &          Basin_gw2sm, LakeExchng2Sat_Q, Lake2Unsat_Q
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Total_pump, Total_pump_cfs, Unsat_S, Sat_S, &
     &         Sat_dS, StreamExchng2Sat_Q, Stream2Unsat_Q, Stream_inflow, &
     &         Basin_gw2sm, LakeExchng2Sat_Q, Lake2Unsat_Q
      ENDIF
      END SUBROUTINE gsflow_budget_restart

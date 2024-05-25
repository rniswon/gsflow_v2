!***********************************************************************
! Sums inflow to and outflow from PRMS ground-water reservoirs for inactive MF cells
! outflow can be routed to downslope ground-water reservoirs and stream
! segments. No PRMS water use or lakes or minimum area or sink or swales
! Can be used for depression storage
!***********************************************************************
      MODULE PRMS_GWFLOW_INACTIVE_CELL
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Groundwater'
      character(len=6), parameter :: MODNAME = 'gwflow_inactive_cell'
      character(len=*), parameter :: Version_gwflow = '2024-02-10'
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gwin_dprst(:)
      DOUBLE PRECISION, SAVE :: Basin_gw_upslope
      DOUBLE PRECISION, SAVE :: Basin_dnflow
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_gwstor, Basin_gwflow, Basin_gwin
      REAL, SAVE, ALLOCATABLE :: Gwres_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_upslope(:), Gwres_in(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_gw_cascadeflow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Gw_in_soil(:), Gw_in_ssr(:), Hru_lateral_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_streamflow_out(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Gwflow_coef(:), Gwstor_init(:), Gwstor_min(:)
      END MODULE PRMS_GWFLOW_INACTIVE_CELL

!***********************************************************************
!     Main gwflow inactive cell routine
!***********************************************************************
      SUBROUTINE gwflow_inactive_cell ()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      EXTERNAL :: gwflow_inactivecell_run, gwflow_inactivecell_decl, gwflow_inactivecell_init
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        CALL gwflow_inactivecell_run()
      ELSEIF ( Process_flag==DECL ) THEN
        CALL gwflow_inactivecell_decl()
      ELSEIF ( Process_flag==INIT ) THEN
        CALL gwflow_inactivecell_init()
      ENDIF

      END SUBROUTINE gwflow_inactive_cell

!***********************************************************************
!     gwflow_inactivecell_decl - set up parameters for groundwater computations
!   Declared Parameters
!     gwstor_init, gwflow_coef, gwsink_coef
!***********************************************************************
      SUBROUTINE gwflow_inactivecell_decl()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, CASCADEGW_OFF
      USE PRMS_MODULE, ONLY: Nhru, Ngw, Init_vars_from_file, Dprst_flag, Cascadegw_flag
      use PRMS_MMFAPI, only: declvar_real, declvar_dble
      use PRMS_READ_PARAM_FILE, only: declparam
      USE PRMS_GWFLOW_INACTIVE_CELL
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
!***********************************************************************
      CALL print_module(MODDESC, MODNAME, Version_gwflow)

! cascading variables and parameters
      IF ( Cascadegw_flag>CASCADEGW_OFF ) THEN
        ALLOCATE ( Gw_upslope(Ngw) )
        CALL declvar_dble(MODNAME, 'gw_upslope', 'ngw', Ngw, &
     &       'Groundwater flow received from upslope GWRs for each GWR', &
     &       'acre-inches', Gw_upslope)

        ALLOCATE ( Hru_gw_cascadeflow(Ngw) )
        CALL declvar_dble(MODNAME, 'hru_gw_cascadeflow', 'ngw', Ngw, &
     &       'Cascading groundwater flow from each GWR', &
     &       'inches', Hru_gw_cascadeflow)
      ENDIF

      ALLOCATE ( Gwres_flow(Ngw) )
      CALL declvar_real(MODNAME, 'gwres_flow', 'ngw', Ngw, &
     &     'Groundwater discharge from each GWR to the stream network', &
     &     'inches', Gwres_flow)

      ALLOCATE ( Gwres_in(Ngw) )
      CALL declvar_dble(MODNAME, 'gwres_in', 'ngw', Ngw, &
     &     'Total inflow to each GWR from associated capillary and gravity reservoirs', &
     &     'acre-inches', Gwres_in)

      ALLOCATE ( Gw_in_soil(Ngw) )
      CALL declvar_dble(MODNAME, 'gw_in_soil', 'ngw', Ngw, &
     &     'Drainage from capillary reservoir excess water for each GWR', &
     &     'acre-inches', Gw_in_soil)

      ALLOCATE ( Gw_in_ssr(Ngw) )
      CALL declvar_dble(MODNAME, 'gw_in_ssr', 'ngw', Ngw, &
     &     'Drainage from gravity reservoir excess water for each GWR', &
     &     'acre-inches', Gw_in_ssr)

      CALL declvar_dble(MODNAME, 'basin_gwstor', 'one', 1, &
     &     'Basin area-weighted average of storage in GWRs', &
     &     'inches', Basin_gwstor)

      CALL declvar_dble(MODNAME, 'basin_gwin', 'one', 1, &
     &     'Basin area-weighted average of inflow to GWRs', &
     &     'inches', Basin_gwin)

      CALL declvar_dble(MODNAME, 'basin_gwflow', 'one', 1, &
     &     'Basin area-weighted average of groundwater flow to the stream network', &
     &     'inches', Basin_gwflow)

      ALLOCATE ( Hru_streamflow_out(Nhru) )
      CALL declvar_dble(MODNAME, 'hru_streamflow_out', 'nhru', Nhru, &
     &     'Total flow to stream network from each HRU', &
     &     'cfs', Hru_streamflow_out)

      ALLOCATE ( Hru_lateral_flow(Nhru) )
      CALL declvar_dble(MODNAME, 'hru_lateral_flow', 'nhru', Nhru, &
     &     'Lateral flow to stream network from each HRU', &
     &     'inches', Hru_lateral_flow)

      IF ( Dprst_flag==1 ) ALLOCATE ( Gwin_dprst(Ngw) )

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==6 ) THEN
        ALLOCATE ( Gwstor_init(Ngw) )
        IF ( declparam(MODNAME, 'gwstor_init', 'ngw', 'real', &
     &       '2.0', '0.0', '50.0', &
     &       'Initial storage in each GWR', &
     &       'Storage in each GWR at the beginning of a simulation', &
     &       'inches')/=0 ) CALL read_error(1, 'gwstor_init')
      ENDIF

      ALLOCATE ( Gwflow_coef(Ngw) )
      IF ( declparam(MODNAME, 'gwflow_coef', 'ngw', 'real', &
     &     '0.015', '0.0', '0.5', &
     &     'Groundwater routing coefficient', &
     &     'Linear coefficient in the equation to compute groundwater discharge for each GWR', &
     &     'fraction/day')/=0 ) CALL read_error(1, 'gwflow_coef')

      END SUBROUTINE gwflow_inactivecell_decl

!***********************************************************************
!     gwflow_inactivecell_init - Initialize gwflow module - get parameter values,
!                                compute initial values.
!***********************************************************************
      SUBROUTINE gwflow_inactivecell_init()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, SWALE, DEBUG_less, CASCADEGW_OFF
      use PRMS_READ_PARAM_FILE, only: getparam_real
      USE PRMS_MODULE, ONLY: Ngw, Init_vars_from_file, Dprst_flag
      USE PRMS_GWFLOW_INACTIVE_CELL
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, Hru_storage
      USE PRMS_FLOWVARS, ONLY: Gwres_stor
      use prms_utils, only: read_error
      IMPLICIT NONE
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: i, j
!***********************************************************************
      IF ( getparam_real(MODNAME, 'gwflow_coef', Ngw, Gwflow_coef)/=0 ) CALL read_error(2, 'gwflow_coef')

      IF ( Init_vars_from_file==0 .OR. Init_vars_from_file==2 .OR. Init_vars_from_file==6 ) THEN
        IF ( getparam_real(MODNAME, 'gwstor_init', Ngw, Gwstor_init)/=0 ) CALL read_error(2, 'gwstor_init')
        Gwres_stor = DBLE( Gwstor_init )
        DEALLOCATE ( Gwstor_init )
      ENDIF

      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        Hru_storage(i) = Hru_storage(i) + Gwres_stor(i)
      ENDDO

      IF ( Dprst_flag==ACTIVE ) Gwin_dprst = 0.0D0

      Basin_gw_upslope = 0.0D0
      Basin_dnflow = 0.0D0
      Gwres_flow = 0.0
      Gwres_in = 0.0
      Gw_in_ssr = 0.0D0
      Gw_in_soil = 0.0D0
      Hru_streamflow_out = 0.0D0
      Hru_lateral_flow = 0.0D0

      END SUBROUTINE gwflow_inactivecell_init

!***********************************************************************
!     gwflow_inactivecell_run - Computes groundwater flow to streamflow
!***********************************************************************
      SUBROUTINE gwflow_inactivecell_run()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, SWALE, DEBUG_less, CASCADEGW_OFF, ERROR_water_use
      USE PRMS_MODULE, ONLY: Print_debug, Dprst_flag, Cascadegw_flag, Gwr_swale_flag, &
     &    Gwr_add_water_use, Gwr_transfer_water_use, Nowyear, Nowmonth, Nowday
      USE PRMS_GWFLOW
      USE PRMS_BASIN, ONLY: Active_gwrs, Gwr_route_order, &
     &    Basin_area_inv, Hru_area, Gwr_type, Hru_area_dble, Hru_storage
      USE PRMS_FLOWVARS, ONLY: Soil_to_gw, Ssr_to_gw, Sroff, Ssres_flow, Gwres_stor
      USE PRMS_CASCADE, ONLY: Ncascade_gwr
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_SRUNOFF, ONLY: Dprst_seep_hru
      USE PRMS_WATER_USE, ONLY: Gwr_transfer, Gwr_gain
      USE GSFPRMS2MF, ONLY: activeHru_inactiveCell
      use prms_utils, only: print_date
      IMPLICIT NONE
! Functions
      EXTERNAL :: rungw_cascade
      INTRINSIC :: DBLE, DABS, SNGL, MIN
! Local Variables
      INTEGER :: i, j
      DOUBLE PRECISION :: dnflow
      DOUBLE PRECISION :: gwin, gwstor, gwsink, gwflow, gwstor_last, gwarea
!***********************************************************************
      IF ( Cascadegw_flag>CASCADEGW_OFF ) THEN
        Gw_upslope = 0.0D0
        Hru_gw_cascadeflow = 0.0D0
        Basin_dnflow = 0.0D0
        Basin_gw_upslope = 0.0D0
      ENDIF

      Basin_gwstor_minarea_wb = 0.0D0
      Basin_gwflow = 0.0D0
      Basin_gwstor = 0.0D0
      Basin_gwsink = 0.0D0
      Basin_gwin = 0.0D0
      DO j = 1, Active_gwrs
        i = Gwr_route_order(j)
        IF ( activeHRU_inactiveCell(i) == 0 ) CYCLE
        gwarea = Hru_area_dble(i)
        gwstor = Gwres_stor(i)*gwarea ! acre-inches
        ! soil_to_gw is for whole HRU, not just perv
        Gw_in_soil(i) = DBLE( Soil_to_gw(i)*Hru_area(i) )
        Gw_in_ssr(i) = DBLE( Ssr_to_gw(i)*Hru_area(i) )
        gwin = Gw_in_soil(i) + Gw_in_ssr(i)
        IF ( Cascadegw_flag>CASCADEGW_OFF ) THEN
          gwin = gwin + Gw_upslope(i)
          Basin_gw_upslope = Basin_gw_upslope + Gw_upslope(i)
        ENDIF
        IF ( Dprst_flag==ACTIVE ) THEN
          !rsr, need basin variable for WB
          Gwin_dprst(i) = Dprst_seep_hru(i)*gwarea
          gwin = gwin + Gwin_dprst(i)
        ENDIF
        IF ( Gwr_add_water_use==ACTIVE ) THEN
          IF ( Gwr_gain(i)>0.0 ) gwin = gwin + DBLE(Gwr_gain(i))/Cfs_conv
        ENDIF
        gwstor = gwstor + gwin
        Basin_gwin = Basin_gwin + gwin
        IF ( Gwminarea_flag==ACTIVE ) THEN
          ! check to be sure gwres_stor >= gwstor_minarea before computing outflows
          IF ( gwstor<Gwstor_minarea(i) ) THEN
            IF ( gwstor<0.0D0 ) THEN
              IF ( Print_debug>DEBUG_less ) PRINT *, 'Warning, groundwater reservoir for HRU:', i, &
     &                                       ' is < 0.0 with gwstor_min active', gwstor
!              ERROR STOP ERROR_var
            ENDIF
            gwstor_last = gwstor
            gwstor = Gwstor_minarea(i)
            !rsr, keep track of change in storage for WB
            Gwstor_minarea_wb(i) = gwstor - gwstor_last
            Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb + Gwstor_minarea_wb(i)
            Gwstor_minarea_wb(i) = Gwstor_minarea_wb(i)/gwarea
            IF ( Print_debug>DEBUG_less ) PRINT *, 'Added to gwres_stor as storage < gwstor_min to GWR:', i, &
     &                                             ' amount:', Gwstor_minarea_wb(i)
          ELSE
            Gwstor_minarea_wb(i) = 0.0D0
          ENDIF
        ENDIF

        IF ( Gwr_transfer_water_use==ACTIVE ) THEN
          IF ( Gwr_transfer(i)>0.0 ) THEN
            IF ( SNGL(gwstor*Cfs_conv)<Gwr_transfer(i) ) THEN
              PRINT *, 'ERROR, not enough storage for transfer from groundwater reservoir storage:', &
     &                  i, ' Date:', Nowyear, Nowmonth, Nowday
              PRINT *, '       storage: ', gwstor, '; transfer: ', Gwr_transfer(i)/Cfs_conv
              ERROR STOP ERROR_water_use
            ENDIF
            gwstor = gwstor - DBLE( Gwr_transfer(i) ) / Cfs_conv
          ENDIF
        ENDIF

        gwsink = 0.0D0
        IF ( gwstor<0.0D0 ) THEN ! could happen with water use
          IF ( Print_debug>DEBUG_less ) PRINT *, 'Warning, groundwater reservoir for HRU:', i, ' is < 0.0, set to 0.0', gwstor
          gwflow = 0.0D0
          Gwres_sink(i) = 0.0
          gwstor = 0.0D0
        ELSE

! Compute groundwater discharge
        gwflow = gwstor*DBLE( Gwflow_coef(i) )

! Reduce storage by outflow
        gwstor = gwstor - gwflow

          IF ( Gwsink_coef(i)>0.0 ) THEN
            gwsink = MIN( gwstor*DBLE( Gwsink_coef(i) ), gwstor ) ! if gwsink_coef > 1, could have had negative gwstor
            gwstor = gwstor - gwsink
          ENDIF
! if gwr_swale_flag = 1 swale GWR flow goes to sink, 2 included in stream network and cascades
! maybe gwr_swale_flag = 3 abs(hru_segment) so hru_segment could be changed from 0 to allow HRU swales
          IF ( Gwr_swale_flag==ACTIVE ) THEN
            IF ( Gwr_type(i)==SWALE ) THEN
              gwsink = gwsink + gwflow
              gwflow = 0.0D0
            ENDIF
          ENDIF
          Gwres_sink(i) = SNGL( gwsink/gwarea )
          Basin_gwsink = Basin_gwsink + gwsink
        ENDIF
        Basin_gwstor = Basin_gwstor + gwstor

        Gwres_flow(i) = SNGL( gwflow/gwarea )
        IF ( Cascadegw_flag>CASCADEGW_OFF ) THEN
          IF ( Ncascade_gwr(i)>0 ) THEN
            CALL rungw_cascade(i, Ncascade_gwr(i), Gwres_flow(i), dnflow)
            Hru_gw_cascadeflow(i) = dnflow
            Basin_dnflow = Basin_dnflow + dnflow*gwarea
          ENDIF
        ENDIF
        Basin_gwflow = Basin_gwflow + DBLE(Gwres_flow(i))*gwarea

        ! leave gwin in inch-acres
        Gwres_in(i) = gwin
        Gwres_stor(i) = gwstor/gwarea
        Hru_lateral_flow(i) = DBLE( Gwres_flow(i) + Sroff(i) + Ssres_flow(i) )
        ! Cfs_conv converts acre-inches per timestep to cfs
        Hru_streamflow_out(i) = gwarea*Cfs_conv*Hru_lateral_flow(i)
        Hru_storage(i) = Hru_storage(i) + Gwres_stor(i)
      ENDDO

      Basin_gwflow = Basin_gwflow*Basin_area_inv
      Basin_gwstor = Basin_gwstor*Basin_area_inv
      Basin_gwsink = Basin_gwsink*Basin_area_inv
      Basin_gwin = Basin_gwin*Basin_area_inv
      Basin_gw_upslope = Basin_gw_upslope*Basin_area_inv
      Basin_gwstor_minarea_wb = Basin_gwstor_minarea_wb*Basin_area_inv
      Basin_dnflow = Basin_dnflow*Basin_area_inv

      END SUBROUTINE gwflow_inactivecell_run

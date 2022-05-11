!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
      MODULE PRMS_INTCP
      IMPLICIT NONE
!   Local Variables
      character(len=*), parameter :: MODDESC = 'Canopy Interception'
      character(len=5), parameter :: MODNAME = 'intcp'
      character(len=*), parameter :: Version_intcp = '2022-05-09'
      INTEGER, SAVE :: Use_transfer_intcp
      INTEGER, PARAMETER :: RAIN = 0, SNOW = 1
!   Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_on(:), Intcp_form(:)
      DOUBLE PRECISION, SAVE :: Basin_net_ppt, Basin_changeover
      DOUBLE PRECISION, SAVE :: Basin_intcp_evap, Basin_net_snow, Basin_net_rain
      REAL, SAVE, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, SAVE, ALLOCATABLE :: Intcp_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_intcpevap(:), Canopy_covden(:)
      REAL, SAVE, ALLOCATABLE :: Net_apply(:), Intcp_changeover(:)
      REAL, SAVE, ALLOCATABLE :: Gain_inches(:), Gain_inches_hru(:)
      DOUBLE PRECISION, SAVE :: Basin_net_apply, Basin_hru_apply
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Irr_type(:)
      REAL, SAVE, ALLOCATABLE :: Snow_intcp(:,:), Srain_intcp(:,:), Wrain_intcp(:,:)
      END MODULE PRMS_INTCP

!***********************************************************************
!     Main intcp routine
!***********************************************************************
      INTEGER FUNCTION intcp()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, RUN, DECL, INIT, CLEAN, READ_INIT, SAVE_INIT
      USE PRMS_MODULE, ONLY: Process_flag, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: intdecl, intinit, intrun
      EXTERNAL :: intcp_restart
!***********************************************************************
      intcp = 0

      IF ( Process_flag==RUN ) THEN
        intcp = intrun()
      ELSEIF ( Process_flag==DECL ) THEN
        intcp = intdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( Init_vars_from_file>OFF ) CALL intcp_restart(READ_INIT)
        intcp = intinit()
      ELSEIF ( Process_flag==CLEAN ) THEN
        IF ( Save_vars_to_file==ACTIVE ) CALL intcp_restart(SAVE_INIT)
      ENDIF

      END FUNCTION intcp

!***********************************************************************
!     intdecl - set up parameters for interception computations
!   Declared Parameters
!     snow_intcp, srain_intcp, wrain_intcp, potet_sublim, cov_type
!     covden_win, covden_sum, epan_coef, hru_area, hru_pansta
!***********************************************************************
      INTEGER FUNCTION intdecl()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, DOCUMENTATION, MONTHS_PER_YEAR
      use PRMS_MMFAPI, only: declvar_dble, declvar_int, declvar_real
      use PRMS_READ_PARAM_FILE, only: declparam
      USE PRMS_MODULE, ONLY: Nhru, Model, Water_use_flag, AG_flag, GSFLOW_flag
      use prms_utils, only: print_module, read_error
      USE PRMS_INTCP
      IMPLICIT NONE
!***********************************************************************
      intdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_intcp)

! NEW VARIABLES and PARAMETERS for APPLICATION RATES
      ALLOCATE ( Net_apply(Nhru) )
      ALLOCATE ( Irr_type(Nhru) )
      Use_transfer_intcp = OFF
      IF ( Water_use_flag==ACTIVE .OR. AG_flag==ACTIVE .OR. GSFLOW_flag==ACTIVE .OR. Model==DOCUMENTATION ) THEN
        ! always declare for GSFLOW as may be needed if the AG Package is active
        ! don't know if AG Package is active during declare, set during init
        IF ( Water_use_flag==ACTIVE ) Use_transfer_intcp = ACTIVE
        ALLOCATE ( Gain_inches(Nhru) )
        CALL declvar_real(MODNAME, 'gain_inches', 'nhru', Nhru, &
     &       'Canopy_gain in as depth in canopy', &
     &       'inches', Gain_inches)
        ALLOCATE ( Gain_inches_hru(Nhru) )
        CALL declvar_real(MODNAME, 'gain_inches_hru', 'nhru', Nhru, &
     &       'Canopy_gain in canopy as depth over the HRU', &
     &       'inches', Gain_inches_hru)
        CALL declvar_dble(MODNAME, 'basin_net_apply', 'one', 1, &
     &       'Basin area-weighted average net application', &
     &       'inches', Basin_net_apply)
        CALL declvar_dble(MODNAME, 'basin_hru_apply', 'one', 1, &
     &       'Basin area-weighted average canopy_gain', &
     &       'inches', Basin_hru_apply)
        CALL declvar_real(MODNAME, 'net_apply', 'nhru', Nhru, &
     &       'canopy_gain minus interception', &
     &       'inches', Net_apply)
        IF ( declparam(MODNAME, 'irr_type', 'nhru', 'integer', &
     &       '0', '0', '2', &
     &       'Application method of irrigation water', &
     &       'Application method of irrigation water for each HRU (0 = sprinkler method with interception only;'// &
     &       ' 1=ditch/drip method with no interception; 2=ignore; 3=sprinkler across whole HRU with interception'// &
     &       ' and throughfall; 4=sprinkler method with amount of water applied on the basis of cover density,'// &
     &       ' such as a living filter), for options 1, 2, and 3 irrigation water is specified as an'// &
     &       ' HRU-area weighted average value', &
     &       'none')/=0 ) CALL read_error(1, 'irr_type')
      ENDIF

      ALLOCATE ( Hru_intcpevap(Nhru) )
      CALL declvar_real(MODNAME, 'hru_intcpevap', 'nhru', Nhru, &
     &     'HRU area-weighted average evaporation from the canopy for each HRU', &
     &     'inches', Hru_intcpevap)

      ALLOCATE ( Net_rain(Nhru) )
      CALL declvar_real(MODNAME, 'net_rain', 'nhru', Nhru, &
     &     'Rain that falls through canopy for each HRU', &
     &     'inches', Net_rain)

      ALLOCATE ( Net_snow(Nhru) )
      CALL declvar_real(MODNAME, 'net_snow', 'nhru', Nhru, &
     &     'Snow that falls through canopy for each HRU', &
     &     'inches', Net_snow)

      ALLOCATE ( Net_ppt(Nhru) )
      CALL declvar_real(MODNAME, 'net_ppt', 'nhru', Nhru, &
     &     'Precipitation (rain and/or snow) that falls through the canopy for each HRU', &
     &     'inches', Net_ppt)

      CALL declvar_dble(MODNAME, 'basin_net_ppt', 'one', 1, &
     &     'Basin area-weighted average net precipitation', &
     &     'inches', Basin_net_ppt)

      CALL declvar_dble(MODNAME, 'basin_net_snow', 'one', 1, &
     &     'Basin area-weighted average snow net precipitation', &
     &     'inches', Basin_net_snow)

      CALL declvar_dble(MODNAME, 'basin_net_rain', 'one', 1, &
     &     'Basin area-weighted average rain net precipitation', &
     &     'inches', Basin_net_rain)

      ALLOCATE ( Intcp_evap(Nhru) )
      CALL declvar_real(MODNAME, 'intcp_evap', 'nhru', Nhru, &
     &     'Evaporation from the canopy for each HRU', &
     &     'inches', Intcp_evap)

      CALL declvar_dble(MODNAME, 'basin_intcp_evap', 'one', 1, &
     &     'Basin area-weighted evaporation from the canopy', &
     &     'inches', Basin_intcp_evap)

      ALLOCATE ( Intcp_form(Nhru) )
      CALL declvar_int(MODNAME, 'intcp_form', 'nhru', Nhru, &
     &     'Form (0=rain; 1=snow) of interception for each HRU', &
     &     'none', Intcp_form)

      ALLOCATE ( Intcp_on(Nhru) )
      CALL declvar_int(MODNAME, 'intcp_on', 'nhru', Nhru, &
     &     'Flag indicating interception storage for each HRU (0=no; 1=yes)', &
     &     'none', Intcp_on)

      ALLOCATE ( Canopy_covden(Nhru) )
      CALL declvar_real(MODNAME, 'canopy_covden', 'nhru', Nhru, &
     &     'Canopy cover density for each HRU', &
     &     'decimal fraction', Canopy_covden)

      ALLOCATE ( Intcp_changeover(Nhru) )
      CALL declvar_real(MODNAME, 'intcp_changeover', 'nhru', Nhru, &
     &     'Water released from a change over of canopy cover type for each HRU', &
     &     'inches', Intcp_changeover)

      CALL declvar_dble(MODNAME, 'basin_changeover', 'one', 1, &
     &     'Basin area-weighted average water released from a change over of canopy cover type', &
     &     'inches', Basin_changeover)

! declare parameters
      ALLOCATE ( Snow_intcp(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'snow_intcp', 'nhru,nmonths', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Snow interception storage capacity', &
     &     'Snow interception storage capacity for the major vegetation type in each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'snow_intcp')

      ALLOCATE ( Srain_intcp(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'srain_intcp', 'nhru,nmonths', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Summer rain interception storage capacity', &
     &     'Summer rain interception storage capacity for the major vegetation type in each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'srain_intcp')

      ALLOCATE ( Wrain_intcp(Nhru,MONTHS_PER_YEAR) )
      IF ( declparam(MODNAME, 'wrain_intcp', 'nhru,nmonths', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Winter rain interception storage capacity', &
     &     'Winter rain interception storage capacity for the major vegetation type in each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'wrain_intcp')

      END FUNCTION intdecl

!***********************************************************************
!     intinit - Initialize intcp module - get parameter values,
!               set initial values.
!***********************************************************************
      INTEGER FUNCTION intinit()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, DEBUG_WB, MONTHS_PER_YEAR, ERROR_control
      use PRMS_READ_PARAM_FILE, only: getparam_int, getparam_real
      USE PRMS_MODULE, ONLY: Nhru, Init_vars_from_file, &
     &    GSFLOW_flag, AG_flag, Ag_package, Agriculture_canopy_flag
      USE PRMS_INTCP
      USE PRMS_CLIMATEVARS, ONLY: Transp_on
      USE PRMS_FLOWVARS, ONLY: Intcp_transp_on, Intcp_stor, Hru_intcpstor, Basin_intcp_stor
      use prms_utils, only: read_error, error_stop
      IMPLICIT NONE
!***********************************************************************
      intinit = 0

      IF ( getparam_real(MODNAME, 'snow_intcp', Nhru*MONTHS_PER_YEAR, Snow_intcp)/=0 ) CALL read_error(2, 'snow_intcp')
      IF ( getparam_real(MODNAME, 'wrain_intcp', Nhru*MONTHS_PER_YEAR, Wrain_intcp)/=0 ) CALL read_error(2, 'wrain_intcp')
      IF ( getparam_real(MODNAME, 'srain_intcp', Nhru*MONTHS_PER_YEAR, Srain_intcp)/=0 ) CALL read_error(2, 'srain_intcp')

      IF ( Use_transfer_intcp==ACTIVE .OR. AG_flag==ACTIVE .OR. GSFLOW_flag==ACTIVE ) THEN
        IF ( getparam_int(MODNAME, 'irr_type', Nhru, Irr_type)/=0 ) CALL read_error(1, 'irr_type')
        Gain_inches = 0.0
        Gain_inches_hru = 0.0
      ELSE
        Irr_type = 0
      ENDIF
      Net_apply = 0.0

      Intcp_changeover = 0.0
      Intcp_form = RAIN
      Intcp_evap = 0.0
      Net_rain = 0.0
      Net_snow = 0.0
      Net_ppt = 0.0
      Hru_intcpevap = 0.0
      Canopy_covden = 0.0
      IF ( Init_vars_from_file==0 ) THEN
        Intcp_transp_on = Transp_on
        Intcp_stor = 0.0
        Intcp_on = OFF
        Hru_intcpstor = 0.0
        Basin_intcp_stor = 0.0D0
      ENDIF
      Basin_net_apply = 0.0D0
      Basin_hru_apply = 0.0D0
      IF ( Ag_package==OFF .AND. Agriculture_canopy_flag==ACTIVE ) &
     &     CALL error_stop('AG Package not active and agriculture_canopy_flag specified = 0', ERROR_control)

      END FUNCTION intinit

!***********************************************************************
!     intrun - Computes and keeps track of intercepted precipitation
!              and evaporation for each HRU
!***********************************************************************
      INTEGER FUNCTION intrun()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, DEBUG_WB, NEARZERO, DNEARZERO, &
     &    DEBUG_less, LAKE, BARESOIL, GRASSES, ERROR_param
      USE PRMS_MODULE, ONLY: Print_debug, PRMS_land_iteration_flag, Kkiter, Nowyear, Nowmonth, Nowday, &
     &    Ag_package, Hru_ag_irr, Agriculture_canopy_flag, Hru_type
      USE PRMS_INTCP
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Covden_win, Covden_sum, &
     &    Hru_route_order, Hru_area, Cov_type, Ag_frac !, Ag_area, Ag_cov_type
      USE PRMS_WATER_USE, ONLY: Canopy_gain ! need to add ag apply ???
! Newsnow and Pptmix can be modfied, WARNING!!!
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Hru_rain, Hru_ppt, &
     &    Hru_snow, Transp_on, Potet, Use_pandata, Hru_pansta, Epan_coef, Potet_sublim
      USE PRMS_FLOWVARS, ONLY: Pkwater_equiv, Intcp_transp_on, Intcp_stor, Hru_intcpstor, Basin_intcp_stor
      USE PRMS_IT0_VARS, ONLY: It0_hru_intcpstor, It0_basin_intcp_stor, It0_intcp_transp_on, It0_intcp_stor
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_OBS, ONLY: Pan_evap
      use prms_utils, only: error_stop
      IMPLICIT NONE
! Functions
      EXTERNAL :: intercept
      INTRINSIC :: DBLE, SNGL
! Local Variables
      INTEGER :: i, j, irrigation_type
      REAL :: last, evrn, evsn, cov, intcpstor, diff, changeover, intcpevap, z, d, harea
      REAL :: netrain, netsnow, extra_water, stor_max_rain, ag_water_maxin
      CHARACTER(LEN=30), PARAMETER :: fmt1 = '(A, I0, ":", I5, 2("/",I2.2))'
!***********************************************************************
      intrun = 0

      ! pkwater_equiv is from last time step
      IF ( PRMS_land_iteration_flag==ACTIVE ) THEN
        IF ( Kkiter>1 ) THEN
          Intcp_stor = It0_intcp_stor
          Hru_intcpstor = It0_hru_intcpstor
          Intcp_transp_on = It0_intcp_transp_on
          Basin_intcp_stor = It0_basin_intcp_stor
        ENDIF
      ENDIF

      Basin_changeover = 0.0D0
      Basin_net_ppt = 0.0D0
      Basin_net_snow = 0.0D0
      Basin_net_rain = 0.0D0
      Basin_intcp_evap = 0.0D0
      Basin_intcp_stor = 0.0D0

! zero application rate variables for today
      IF ( Use_transfer_intcp==ACTIVE .OR. Ag_package==ACTIVE ) THEN
        Basin_net_apply = 0.0D0
        Basin_hru_apply = 0.0D0
        Net_apply = 0.0
        Gain_inches = 0.0
        Gain_inches_hru = 0.0
      ENDIF

      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        harea = Hru_area(i)
        netrain = Hru_rain(i)
        netsnow = Hru_snow(i)

!******Adjust interception amounts for changes in summer/winter cover density

        IF ( Transp_on(i)==ACTIVE ) THEN
          Canopy_covden(i) = Covden_sum(i,Nowmonth)
        ELSE
          Canopy_covden(i) = Covden_win(i,Nowmonth)
        ENDIF
        cov = Canopy_covden(i)
        Intcp_form(i) = RAIN
        IF ( Hru_snow(i)>0.0 ) Intcp_form(i) = SNOW

        intcpstor = Intcp_stor(i)
        intcpevap = 0.0
        changeover = 0.0
        extra_water = 0.0
        ! Lake or bare ground HRUs
        IF ( Hru_type(i)==LAKE .OR. Cov_type(i)==BARESOIL ) THEN ! cov_type = 0
          IF ( Cov_type(i)==BARESOIL .AND. intcpstor>0.0 ) THEN
            ! could happen if cov_type changed from > 0 to 0 with storage using dynamic parameters
            extra_water = Hru_intcpstor(i)
            IF ( Print_debug>DEBUG_less ) THEN
              PRINT *, 'WARNING, cov_type changed to 0 with canopy storage of:', Hru_intcpstor(i)
              PRINT *, '         this storage added to intcp_changeover'
              PRINT FMT1, '          HRU: ', i, Nowyear, Nowmonth, Nowday
            ENDIF
          ENDIF
          intcpstor = 0.0
        ENDIF

!***** go from summer to winter cover density
        IF ( Transp_on(i)==OFF .AND. Intcp_transp_on(i)==ACTIVE ) THEN
          Intcp_transp_on(i) = OFF
          IF ( intcpstor>0.0 ) THEN
            ! assume canopy storage change falls as throughfall
            diff = Covden_sum(i,Nowmonth) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              IF ( changeover<0.0 ) THEN
                ! covden_win > covden_sum, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor*Covden_sum(i,Nowmonth)/cov
                changeover = 0.0
              ENDIF
            ELSE
              IF ( Print_debug>DEBUG_less ) THEN
                PRINT *, 'covden_win=0 at winter change over with canopy storage, HRU:', i, Nowyear, Nowmonth, Nowday
                PRINT *, 'intcp_stor:', intcpstor, ' covden_sum:', Covden_sum(i,Nowmonth)
              ENDIF
              intcpstor = 0.0
            ENDIF
          ENDIF

!****** go from winter to summer cover density, excess = throughfall
        ELSEIF ( Transp_on(i)==ACTIVE .AND. Intcp_transp_on(i)==OFF ) THEN
          Intcp_transp_on(i) = ACTIVE
          IF ( intcpstor>0.0 ) THEN
            diff = Covden_win(i,Nowmonth) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              IF ( changeover<0.0 ) THEN
                ! covden_sum > covden_win, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor*Covden_win(i,Nowmonth)/cov
                changeover = 0.0
              ENDIF
            ELSE
              IF ( Print_debug>DEBUG_less ) THEN
                PRINT *, 'covden_sum=0 at summer change over with canopy storage, HRU:', i, Nowyear, Nowmonth, Nowday
                PRINT *, 'intcp_stor:', intcpstor, ' covden_win:', Covden_win(i,Nowmonth)
              ENDIF
              intcpstor = 0.0
            ENDIF
          ENDIF
        ENDIF

        IF ( Transp_on(i)==ACTIVE ) THEN
          stor_max_rain = Srain_intcp(i,Nowmonth)
        ELSE
          stor_max_rain = Wrain_intcp(i,Nowmonth)
        ENDIF

!*****Determine the amount of interception from rain

        IF ( Hru_type(i)/=LAKE .AND. Cov_type(i)/=BARESOIL ) THEN  ! cov_type /= 0, not a lake or bare ground HRU
          IF ( Hru_rain(i)>0.0 ) THEN
            IF ( cov>0.0 ) THEN
              IF ( Cov_type(i)>GRASSES ) THEN ! cov_type > 1
                CALL intercept(Hru_rain(i), stor_max_rain, cov, intcpstor, netrain)
              ELSEIF ( Cov_type(i)==GRASSES ) THEN ! cov_type = 1
                !rsr, 03/24/2008 intercept rain on snow-free grass,
                !rsr             when not a mixed event
                IF ( Pkwater_equiv(i)<DNEARZERO .AND. netsnow<NEARZERO ) THEN
                  CALL intercept(Hru_rain(i), stor_max_rain, cov, intcpstor, netrain)
                  !rsr 03/24/2008
                  !it was decided to leave the water in intcpstor rather
                  !than put the water in the snowpack, as doing so for a
                  !mixed event on grass with snow-free surface produces a
                  !divide by zero in snowcomp. Storage on grass will
                  !eventually evaporate
                ENDIF
              ENDIF
            ENDIF
          ENDIF

!******Determine amount of interception from snow

          IF ( Hru_snow(i)>0.0 ) THEN
            IF ( cov>0.0 ) THEN
              IF ( Cov_type(i)>GRASSES ) THEN ! cov_type > 1
                CALL intercept(Hru_snow(i), Snow_intcp(i,Nowmonth), cov, intcpstor, netsnow)
                IF ( netsnow<NEARZERO ) THEN   !rsr, added 3/9/2006
                  netrain = netrain + netsnow
                  netsnow = 0.0
                  Newsnow(i) = OFF
                  Pptmix(i) = OFF   ! reset to be sure it is zero
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

! canopy application of irrigation water based on irr_type
! irr_type = 0 (interception, sprinkler), 1 (no interception, furrow),
!            2=ignore, 3 (interception and throughfall over whole HRU),
!            4 (amount of water based on cover density, living filter)
! irr_type = 0 or 3 are the same in terms of application rate
! gain_inches_hru is water applied to whole HRU, gain_inches is water added to canopy

        IF ( Use_transfer_intcp==ACTIVE .OR. Agriculture_canopy_flag==ACTIVE ) THEN
          ag_water_maxin = 0.0  ! inches
          IF ( Agriculture_canopy_flag==ACTIVE ) THEN
            IF ( Hru_ag_irr(i)>0.0 ) THEN
              IF ( .NOT.(Ag_frac(i)>0.0) ) THEN
                PRINT *, 'ag_frac=0.0 for HRU:', i
                CALL error_stop('AG Package irrigation specified and ag_frac=0', ERROR_param)
              ENDIF
              ag_water_maxin = Hru_ag_irr(i) / harea ! Hru_ag_irr is in acre-inches
            ENDIF
          ENDIF
          IF ( Use_transfer_intcp==ACTIVE ) ag_water_maxin = ag_water_maxin + Canopy_gain(i)/SNGL(Cfs_conv)/harea ! Canopy_gain in CFS, convert to inches
          IF ( ag_water_maxin>0.0 ) THEN
            IF ( Hru_type(i)==LAKE ) CALL error_stop('irrigation specified and hru_type is lake', ERROR_param)
            Gain_inches_hru(i) = ag_water_maxin
            Gain_inches(i) = ag_water_maxin
            IF ( cov>0.0 ) Gain_inches(i) = ag_water_maxin/cov
            irrigation_type = Irr_type(i)
            IF ( Cov_type(i)==BARESOIL .AND. irrigation_type/=1 ) THEN
              PRINT *, 'WARNING, cov_type = bare soil and irr_type not equal 1, set to 1'
              irrigation_type = 1
            ENDIF

            IF ( (irrigation_type==0 .OR. irrigation_type==3) .AND. cov>0.0 ) THEN
              CALL intercept(ag_water_maxin, stor_max_rain, cov, intcpstor, Net_apply(i))
            ELSEIF ( irrigation_type==4 ) THEN
              CALL intercept(Gain_inches(i), stor_max_rain, 1.0, intcpstor, Net_apply(i))
            ELSEIF ( irrigation_type==2 ) THEN
              IF ( Use_transfer_intcp==ACTIVE ) PRINT *, 'WARNING, irrigation water > 0, but irr_type = 2 (ignore), HRU:', &
     &                                                   i, ', application water:', Canopy_gain(i)
              IF ( Agriculture_canopy_flag==ACTIVE ) &
     &             CALL error_stop('irrigation specified for HRU and irr_type = 2 (ignore)', ERROR_param)
              Canopy_gain(i) = 0.0 ! remove ignored canopy gain from water budget
            ELSEIF ( irrigation_type==1 .OR. .NOT.(cov>0.0) ) THEN ! irr_type = 1 or (0 or 3 and cov=0) bare ground
              Net_apply(i) = ag_water_maxin
            ELSE
              CALL error_stop('irrigation specified and irr_type and/or cover density invalid', ERROR_param)
            ENDIF

            Basin_hru_apply = Basin_hru_apply + DBLE( ag_water_maxin*harea )
            Basin_net_apply = Basin_net_apply + DBLE( Net_apply(i)*harea )
          ENDIF
        ENDIF

!******compute evaporation or sublimation of interception

        ! if precipitation assume no evaporation or sublimation
        IF ( intcpstor>0.0 ) THEN
          IF ( Hru_ppt(i)<NEARZERO ) THEN

            evrn = Potet(i)/Epan_coef(i, Nowmonth)
            evsn = Potet_sublim(i)*Potet(i)

            IF ( Use_pandata==ACTIVE ) THEN
              evrn = Pan_evap(Hru_pansta(i))
              IF ( evrn<0.0 ) evrn = 0.0
            ENDIF

!******Compute snow interception loss

            IF ( Intcp_form(i)==SNOW ) THEN
              z = intcpstor - evsn
              IF ( z>0.0 ) THEN
                intcpstor = z
                intcpevap = evsn
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
              ENDIF
!           ELSEIF ( Intcp_form(i)==RAIN ) THEN
            ELSE
              d = intcpstor - evrn
              IF ( d>0.0 ) THEN
                intcpstor = d
                intcpevap = evrn
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
              ENDIF
            ENDIF
          ENDIF

        ENDIF

        IF ( intcpevap*cov>Potet(i) ) THEN
          last = intcpevap
          IF ( cov>0.0 ) THEN
            intcpevap = Potet(i)/cov
          ELSE
            intcpevap = 0.0
          ENDIF
          intcpstor = intcpstor + last - intcpevap
        ENDIF
        Intcp_evap(i) = intcpevap
        Hru_intcpevap(i) = intcpevap*cov
        Intcp_stor(i) = intcpstor
        IF ( intcpstor>0.0 ) Intcp_on(i) = ACTIVE
        Hru_intcpstor(i) = intcpstor*cov
        Intcp_changeover(i) = changeover + extra_water
        Net_rain(i) = netrain
        Net_snow(i) = netsnow
        Net_ppt(i) = netrain + netsnow

        !rsr, question about depression storage for basin_net_ppt???
        !     my assumption is that cover density is for the whole HRU
        Basin_net_ppt = Basin_net_ppt + DBLE( Net_ppt(i)*harea )
        Basin_net_snow = Basin_net_snow + DBLE( Net_snow(i)*harea )
        Basin_net_rain = Basin_net_rain + DBLE( Net_rain(i)*harea )
        Basin_intcp_stor = Basin_intcp_stor + DBLE( intcpstor*cov*harea )
        Basin_intcp_evap = Basin_intcp_evap + DBLE( intcpevap*cov*harea )
        IF ( changeover>0.0 ) THEN
          IF ( Print_debug>DEBUG_less ) PRINT '(A,F0.5,A,4(1X,I0))', 'Change over storage:', changeover, '; HRU:', i, &
     &                                                               Nowyear, Nowmonth, Nowday
          Basin_changeover = Basin_changeover + DBLE( Intcp_changeover(i)*harea )
        ENDIF

      ENDDO

      Basin_net_ppt = Basin_net_ppt*Basin_area_inv
      Basin_net_snow = Basin_net_snow*Basin_area_inv
      Basin_net_rain = Basin_net_rain*Basin_area_inv
      Basin_intcp_stor = Basin_intcp_stor*Basin_area_inv
      Basin_intcp_evap = Basin_intcp_evap*Basin_area_inv
      Basin_changeover = Basin_changeover*Basin_area_inv
      IF ( Use_transfer_intcp==ACTIVE ) THEN
        Basin_net_apply = Basin_net_apply*Basin_area_inv
        Basin_hru_apply = Basin_hru_apply*Basin_area_inv
      ENDIF

      END FUNCTION intrun

!***********************************************************************
!      Subroutine to compute interception of rain or snow
!***********************************************************************
      SUBROUTINE intercept(Precip, Stor_max, Cov, Intcp_stor, Net_precip)
      IMPLICIT NONE
! Arguments
      REAL, INTENT(IN) :: Precip, Cov, Stor_max
      REAL, INTENT(INOUT) :: Intcp_stor
      REAL, INTENT(OUT) :: Net_precip
!***********************************************************************
      Net_precip = Precip*(1.0-Cov)

      Intcp_stor = Intcp_stor + Precip

      IF ( Intcp_stor>Stor_max ) THEN
        Net_precip = Net_precip + (Intcp_stor-Stor_max)*Cov
        Intcp_stor = Stor_max
      ENDIF

      END SUBROUTINE intercept

!***********************************************************************
!     intcp_restart - write or read intcp restart file
!***********************************************************************
      SUBROUTINE intcp_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit, text_restart_flag
      USE PRMS_FLOWVARS, ONLY: Intcp_transp_on, Intcp_stor, Hru_intcpstor, Basin_intcp_stor
      USE PRMS_INTCP
      use prms_utils, only: check_restart
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Local Variable
      CHARACTER(LEN=5) :: module_name
!***********************************************************************
    IF ( In_out==SAVE_INIT ) THEN
      IF ( text_restart_flag==OFF ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_intcp_stor
        WRITE ( Restart_outunit ) Intcp_transp_on
        WRITE ( Restart_outunit ) Intcp_on
        WRITE ( Restart_outunit ) Intcp_stor
        WRITE ( Restart_outunit ) Hru_intcpstor
      ELSE
        WRITE ( Restart_outunit, * ) MODNAME
        WRITE ( Restart_outunit, * ) Basin_intcp_stor
        WRITE ( Restart_outunit, * ) Intcp_transp_on
        WRITE ( Restart_outunit, * ) Intcp_on
        WRITE ( Restart_outunit, * ) Intcp_stor
        WRITE ( Restart_outunit, * ) Hru_intcpstor
      ENDIF
    ELSE
      IF ( text_restart_flag==OFF ) THEN
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_intcp_stor
        READ ( Restart_inunit ) Intcp_transp_on
        READ ( Restart_inunit ) Intcp_on
        READ ( Restart_inunit ) Intcp_stor
        READ ( Restart_inunit ) Hru_intcpstor
      ELSE
        READ ( Restart_inunit, * ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit, * ) Basin_intcp_stor
        READ ( Restart_inunit, * ) Intcp_transp_on
        READ ( Restart_inunit, * ) Intcp_on
        READ ( Restart_inunit, * ) Intcp_stor
        READ ( Restart_inunit, * ) Hru_intcpstor
      ENDIF
    ENDIF
    END SUBROUTINE intcp_restart

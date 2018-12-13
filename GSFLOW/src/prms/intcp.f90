!***********************************************************************
! Computes volume of intercepted precipitation, evaporation from
! intercepted precipitation, and throughfall that reaches the soil or
! snowpack
!***********************************************************************
      MODULE PRMS_INTCP
      IMPLICIT NONE
!   Local Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_transp_on(:)
      REAL, SAVE, ALLOCATABLE :: Intcp_changeover(:), Intcp_stor_ante(:)
      DOUBLE PRECISION, SAVE :: Basin_changeover, Last_intcp_stor
      CHARACTER(LEN=5), SAVE :: MODNAME
!   Declared Variables
      INTEGER, SAVE, ALLOCATABLE :: Intcp_on(:), Intcp_form(:)
      DOUBLE PRECISION, SAVE :: Basin_net_ppt, Basin_intcp_stor
      DOUBLE PRECISION, SAVE :: Basin_intcp_evap, Basin_net_snow, Basin_net_rain
      REAL, SAVE, ALLOCATABLE :: Net_rain(:), Net_snow(:), Net_ppt(:)
      REAL, SAVE, ALLOCATABLE :: Intcp_stor(:), Intcp_evap(:)
      REAL, SAVE, ALLOCATABLE :: Hru_intcpstor(:), Hru_intcpevap(:), Canopy_covden(:)
!   Declared Parameters
      REAL, SAVE, ALLOCATABLE :: Snow_intcp(:), Srain_intcp(:), Wrain_intcp(:)
      END MODULE PRMS_INTCP

!***********************************************************************
!     Main intcp routine
!***********************************************************************
      INTEGER FUNCTION intcp()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: intdecl, intinit, intrun
      EXTERNAL :: intcp_restart
!***********************************************************************
      intcp = 0

      IF ( Process(:3)=='run' ) THEN
        intcp = intrun()
      ELSEIF ( Process(:4)=='decl' ) THEN
        intcp = intdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL intcp_restart(1)
        intcp = intinit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL intcp_restart(0)
      ENDIF

      END FUNCTION intcp

!***********************************************************************
!     intdecl - set up parameters for interception computations
!   Declared Parameters
!     snow_intcp, srain_intcp, wrain_intcp, potet_sublim, cov_type
!     covden_win, covden_sum, epan_coef, hru_area, hru_pansta
!***********************************************************************
      INTEGER FUNCTION intdecl()
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
! Local Variables
      CHARACTER(LEN=80), SAVE :: Version_intcp
!***********************************************************************
      intdecl = 0

      Version_intcp = 'intcp.f90 2017-09-27 13:53:00Z'
      CALL print_module(Version_intcp, 'Canopy Interception         ', 90)
      MODNAME = 'intcp'

      ALLOCATE ( Hru_intcpevap(Nhru) )
      IF ( declvar(MODNAME, 'hru_intcpevap', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average evaporation from the canopy for each HRU', &
     &     'inches', Hru_intcpevap)/=0 ) CALL read_error(3, 'hru_intcpevap')

      ALLOCATE ( Net_rain(Nhru) )
      IF ( declvar(MODNAME, 'net_rain', 'nhru', Nhru, 'real', &
     &     'Rain that falls through canopy for each HRU', &
     &     'inches', Net_rain)/=0 ) CALL read_error(3, 'net_rain')

      ALLOCATE ( Net_snow(Nhru) )
      IF ( declvar(MODNAME, 'net_snow', 'nhru', Nhru, 'real', &
     &     'Snow that falls through canopy for each HRU', &
     &     'inches', Net_snow)/=0 ) CALL read_error(3, 'net_snow')

      ALLOCATE ( Net_ppt(Nhru) )
      IF ( declvar(MODNAME, 'net_ppt', 'nhru', Nhru, 'real', &
     &     'Precipitation (rain and/or snow) that falls through the canopy for each HRU', &
     &     'inches', Net_ppt)/=0 ) CALL read_error(3, 'net_ppt')

      IF ( declvar(MODNAME, 'basin_net_ppt', 'one', 1, 'double', &
     &     'Basin area-weighted average throughfall', &
     &     'inches', Basin_net_ppt)/=0 ) CALL read_error(3, 'basin_net_ppt')

      IF ( declvar(MODNAME, 'basin_net_snow', 'one', 1, 'double', &
     &     'Basin area-weighted average snow throughfall', &
     &     'inches', Basin_net_snow)/=0 ) CALL read_error(3, 'basin_net_snow')

      IF ( declvar(MODNAME, 'basin_net_rain', 'one', 1, 'double', &
     &     'Basin area-weighted average rain throughfall', &
     &     'inches', Basin_net_rain)/=0 ) CALL read_error(3, 'basin_net_rain')

      ALLOCATE ( Intcp_stor(Nhru) )
      IF ( declvar(MODNAME, 'intcp_stor', 'nhru', Nhru, 'real', &
     &     'Interception storage in canopy for cover density for each HRU', &
     &     'inches', Intcp_stor)/=0 ) CALL read_error(3, 'intcp_stor')

      IF ( declvar(MODNAME, 'basin_intcp_stor', 'one', 1, 'double', &
     &     'Basin area-weighted average interception storage', &
     &     'inches', Basin_intcp_stor)/=0 ) CALL read_error(3, 'basin_intcp_stor')

      ALLOCATE ( Intcp_evap(Nhru) )
      IF ( declvar(MODNAME, 'intcp_evap', 'nhru', Nhru, 'real', &
     &     'Evaporation from the canopy for each HRU', &
     &     'inches', Intcp_evap)/=0 ) CALL read_error(3, 'intcp_evap')

      IF ( declvar(MODNAME, 'basin_intcp_evap', 'one', 1, 'double', &
     &     'Basin area-weighted evaporation from the canopy', &
     &     'inches', Basin_intcp_evap)/=0 ) CALL read_error(3, 'basin_intcp_evap')

      ALLOCATE ( Hru_intcpstor(Nhru) )
      IF ( declvar(MODNAME, 'hru_intcpstor', 'nhru', Nhru, 'real', &
     &     'HRU area-weighted average Interception storage in the canopy for each HRU', &
     &     'inches', Hru_intcpstor)/=0 ) CALL read_error(3, 'hru_intcpstor')

      ALLOCATE ( Intcp_form(Nhru) )
      IF ( declvar(MODNAME, 'intcp_form', 'nhru', Nhru, 'integer', &
     &     'Form (rain or snow) of interception for each HRU', &
     &     'none', Intcp_form)/=0 ) CALL read_error(3, 'intcp_form')

      ALLOCATE ( Intcp_on(Nhru) )
      IF ( declvar(MODNAME, 'intcp_on', 'nhru', Nhru, 'integer', &
     &     'Flag indicating interception storage for each HRU (0=no; 1=yes)', &
     &     'none', Intcp_on)/=0 ) CALL read_error(3, 'intcp_on')

      ALLOCATE ( Canopy_covden(Nhru) )
      IF ( declvar(MODNAME, 'canopy_covden', 'nhru', Nhru, 'real', &
     &     'Canopy cover density for each HRU', &
     &     'decimal fraction', Canopy_covden)/=0 ) CALL read_error(3, 'canopy_covden')

      ALLOCATE ( Intcp_transp_on(Nhru), Intcp_changeover(Nhru) )

! declare parameters
      ALLOCATE ( Snow_intcp(Nhru) )
      IF ( declparam(MODNAME, 'snow_intcp', 'nhru', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Snow interception storage capacity', &
     &     'Snow interception storage capacity for the major vegetation type in each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'snow_intcp')

      ALLOCATE ( Srain_intcp(Nhru) )
      IF ( declparam(MODNAME, 'srain_intcp', 'nhru', 'real', &
     &     '0.1', '0.0', '1.0', &
     &     'Summer rain interception storage capacity', &
     &     'Summer rain interception storage capacity for the major vegetation type in each HRU', &
     &     'inches')/=0 ) CALL read_error(1, 'srain_intcp')

      ALLOCATE ( Wrain_intcp(Nhru) )
      IF ( declparam(MODNAME, 'wrain_intcp', 'nhru', 'real', &
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
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Nhru, Init_vars_from_file, Print_debug
      USE PRMS_CLIMATEVARS, ONLY: Transp_on
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
!***********************************************************************
      intinit = 0

      IF ( getparam(MODNAME, 'snow_intcp', Nhru, 'real', Snow_intcp)/=0 ) CALL read_error(2, 'snow_intcp')
      IF ( getparam(MODNAME, 'wrain_intcp', Nhru, 'real', Wrain_intcp)/=0 ) CALL read_error(2, 'wrain_intcp')
      IF ( getparam(MODNAME, 'srain_intcp', Nhru, 'real', Srain_intcp)/=0 ) CALL read_error(2, 'srain_intcp')

      Intcp_changeover = 0.0
      IF ( Init_vars_from_file==0 ) THEN
        Intcp_transp_on = Transp_on
        Intcp_stor = 0.0
        Intcp_on = 0
        Intcp_form = 0
        Intcp_evap = 0.0
        Hru_intcpstor = 0.0
        Net_rain = 0.0
        Net_snow = 0.0
        Net_ppt = 0.0
        Hru_intcpevap = 0.0
        Canopy_covden = 0.0
        Basin_changeover = 0.0D0
        Basin_net_ppt = 0.0D0
        Basin_net_snow = 0.0D0
        Basin_net_rain = 0.0D0
        Basin_intcp_evap = 0.0D0
        Basin_intcp_stor = 0.0D0
      ENDIF
      IF ( Print_debug==1 ) THEN
        ALLOCATE ( Intcp_stor_ante(Nhru) )
        Intcp_stor_ante = Hru_intcpstor
        Last_intcp_stor = 0.0D0
      ENDIF

      END FUNCTION intinit

!***********************************************************************
!     intrun - Computes and keeps track of intercepted precipitation
!              and evaporation for each HRU
!***********************************************************************
      INTEGER FUNCTION intrun()
      USE PRMS_INTCP
      USE PRMS_MODULE, ONLY: Print_debug
      USE PRMS_BASIN, ONLY: Basin_area_inv, Active_hrus, Hru_type, Covden_win, Covden_sum, &
     &    Hru_route_order, Hru_area, NEARZERO, DNEARZERO, Cov_type
! Newsnow and Pptmix can be modfied, WARNING!!!
      USE PRMS_CLIMATEVARS, ONLY: Newsnow, Pptmix, Hru_rain, Hru_ppt, &
     &    Hru_snow, Transp_on, Potet, Use_pandata, Hru_pansta, Epan_coef, Potet_sublim
      USE PRMS_FLOWVARS, ONLY: Pkwater_equiv
      USE PRMS_SET_TIME, ONLY: Nowmonth
      USE PRMS_OBS, ONLY: Pan_evap
      IMPLICIT NONE
      EXTERNAL intercept
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j
      REAL :: last, evrn, evsn, cov, intcpstor, diff, changeover, stor, intcpevap, z, d, harea
      REAL :: netrain, netsnow
!***********************************************************************
      intrun = 0

      ! pkwater_equiv is from last time step

      IF ( Print_debug==1 ) THEN
        Intcp_stor_ante = Hru_intcpstor
        Last_intcp_stor = Basin_intcp_stor
      ENDIF
      Basin_changeover = 0.0D0
      Basin_net_ppt = 0.0D0
      Basin_net_snow = 0.0D0
      Basin_net_rain = 0.0D0
      Basin_intcp_evap = 0.0D0
      Basin_intcp_stor = 0.0D0
      DO j = 1, Active_hrus
        i = Hru_route_order(j)
        harea = Hru_area(i)
        Net_ppt(i) = Hru_ppt(i)

        ! Lake or bare ground HRUs
        IF ( Hru_type(i)==2 .OR. Cov_type(i)==0 ) THEN
          Net_rain(i) = Hru_rain(i)
          Net_snow(i) = Hru_snow(i)
          Basin_net_ppt = Basin_net_ppt + DBLE( Net_ppt(i)*harea )
          Basin_net_snow = Basin_net_snow + DBLE( Hru_snow(i)*harea )
          Basin_net_rain = Basin_net_rain + DBLE( Hru_rain(i)*harea )
          CYCLE
        ENDIF

        netrain = Hru_rain(i)
        netsnow = Hru_snow(i)

!******Adjust interception amounts for changes in summer/winter cover density

        IF ( Transp_on(i)==1 ) THEN
          Canopy_covden(i) = Covden_sum(i)
        ELSE
          Canopy_covden(i) = Covden_win(i)
        ENDIF
        cov = Canopy_covden(i)
        Intcp_form(i) = 0

        intcpstor = Intcp_stor(i)
        intcpevap = 0.0
        changeover = 0.0

!*****Determine the amount of interception from rain

!***** go from summer to winter cover density
        IF ( Transp_on(i)==0 .AND. Intcp_transp_on(i)==1 ) THEN
          Intcp_transp_on(i) = 0
          IF ( intcpstor>0.0 ) THEN
            ! assume canopy storage change falls as throughfall
            diff = Covden_sum(i) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              IF ( changeover<0.0 ) THEN
                ! covden_win > covden_sum, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor*Covden_sum(i)/cov
                changeover = 0.0
              ENDIF
            ELSE
              IF ( Print_debug>-1 ) PRINT *, 'covden_win=0 at winter change over with canopy storage, HRU:', i, &
     &                                       'intcp_stor:', intcpstor, ' covden_sum:', Covden_sum(i)
              intcpstor = 0.0
              Intcp_on(i) = 0
            ENDIF
          ENDIF

!****** go from winter to summer cover density, excess = throughfall
        ELSEIF ( Transp_on(i)==1 .AND. Intcp_transp_on(i)==0 ) THEN
          Intcp_transp_on(i) = 1
          IF ( intcpstor>0.0 ) THEN
            diff = Covden_win(i) - cov
            changeover = intcpstor*diff
            IF ( cov>0.0 ) THEN
              IF ( changeover<0.0 ) THEN
                ! covden_sum > covden_win, adjust intcpstor to same volume, and lower depth
                intcpstor = intcpstor*Covden_win(i)/cov
                changeover = 0.0
              ENDIF
            ELSE
              IF ( Print_debug>-1 ) PRINT *, 'covden_sum=0 at summer change over with canopy storage, HRU:', i, &
     &                                       'intcp_stor:', intcpstor, ' covden_win:', Covden_win(i)
              intcpstor = 0.0
              Intcp_on(i) = 0
            ENDIF
          ENDIF
        ENDIF

!*****Determine the amount of interception from rain

        IF ( Transp_on(i)==1 ) THEN
          stor = Srain_intcp(i)
        ELSE
          stor = Wrain_intcp(i)
        ENDIF
        IF ( Hru_rain(i)>0.0 ) THEN
          IF ( cov>0.0 ) THEN
            IF ( Cov_type(i)>1 ) THEN
              CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i), intcpstor, netrain)
            ELSEIF ( Cov_type(i)==1 ) THEN
              !rsr, 03/24/2008 intercept rain on snow-free grass,
              !rsr             when not a mixed event
              IF ( Pkwater_equiv(i)<DNEARZERO .AND. netsnow<NEARZERO ) THEN
                CALL intercept(Hru_rain(i), stor, cov, Intcp_on(i), intcpstor, netrain)
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

        IF ( changeover>0.0) THEN
          IF ( Print_debug>-1 ) PRINT *, 'Change over storage added to rain throughfall:', changeover, '; HRU:', i
          netrain = netrain + changeover
          Basin_changeover = Basin_changeover + DBLE( changeover*harea )
        ENDIF

!******Determine amount of interception from snow

        IF ( Hru_snow(i)>0.0 ) THEN
          IF ( cov>0.0 ) THEN
            Intcp_form(i) = 1
            IF ( Cov_type(i)>1 ) THEN
              stor = Snow_intcp(i)
              CALL intercept(Hru_snow(i), stor, cov, Intcp_on(i), intcpstor, netsnow)
              IF ( netsnow<NEARZERO ) THEN   !rsr, added 3/9/2006
                netrain = netrain + netsnow
                netsnow = 0.0
                Newsnow(i) = 0
                Pptmix(i) = 0   ! reset to be sure it is zero
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        Net_ppt(i) = netrain + netsnow

!******compute evaporation or sublimation of interception

        ! if precipitation assume no evaporation or sublimation
        IF ( Intcp_on(i)==1 ) THEN
          IF ( Hru_ppt(i)<NEARZERO ) THEN

            evrn = Potet(i)/Epan_coef(i, Nowmonth)
            evsn = Potet_sublim(i)*Potet(i)

            IF ( Use_pandata==1 ) THEN
              evrn = Pan_evap(Hru_pansta(i))
              IF ( evrn<0.0 ) evrn = 0.0
            ENDIF

!******Compute snow interception loss

            IF ( Intcp_form(i)==1 ) THEN
              z = intcpstor - evsn
              IF ( z>0.0 ) THEN
                Intcp_on(i) = 1
                intcpstor = z
                intcpevap = evsn
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
                Intcp_on(i) = 0
              ENDIF
!           ELSEIF ( Intcp_form(i)==0 ) THEN
            ELSE
              d = intcpstor - evrn
              IF ( d>0.0 ) THEN
                intcpstor = d
                intcpevap = evrn
                Intcp_on(i) = 1
              ELSE
                intcpevap = intcpstor
                intcpstor = 0.0
                Intcp_on(i) = 0
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
        Hru_intcpstor(i) = intcpstor*cov
        Intcp_changeover(i) = changeover
        Net_rain(i) = netrain
        Net_snow(i) = netsnow

        !rsr, question about depression storage for basin_net_ppt???
        !     my assumption is that cover density is for the whole HRU
        Basin_net_ppt = Basin_net_ppt + DBLE( Net_ppt(i)*harea )
        Basin_net_snow = Basin_net_snow + DBLE( Net_snow(i)*harea )
        Basin_net_rain = Basin_net_rain + DBLE( Net_rain(i)*harea )
        Basin_intcp_stor = Basin_intcp_stor + DBLE( intcpstor*cov*harea )
        Basin_intcp_evap = Basin_intcp_evap + DBLE( intcpevap*cov*harea )

      ENDDO

      Basin_net_ppt = Basin_net_ppt*Basin_area_inv
      Basin_net_snow = Basin_net_snow*Basin_area_inv
      Basin_net_rain = Basin_net_rain*Basin_area_inv
      Basin_intcp_stor = Basin_intcp_stor*Basin_area_inv
      Basin_intcp_evap = Basin_intcp_evap*Basin_area_inv
      Basin_changeover = Basin_changeover*Basin_area_inv

      END FUNCTION intrun

!***********************************************************************
!      Subroutine to compute interception of rain or snow
!***********************************************************************
      SUBROUTINE intercept(Precip, Stor_max, Cov, Intcp_on, Intcp_stor, Net_precip)
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(OUT) :: Intcp_on
      REAL, INTENT(IN) :: Precip, Cov, Stor_max
      REAL, INTENT(INOUT) :: Intcp_stor
      REAL, INTENT(OUT) :: Net_precip
!***********************************************************************
      Intcp_on = 1

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
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_INTCP
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variable
      CHARACTER(LEN=5) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Basin_net_ppt, Basin_intcp_stor, Basin_intcp_evap, Basin_changeover, &
     &                            Basin_net_snow, Basin_net_rain
        WRITE ( Restart_outunit ) Intcp_transp_on
        WRITE ( Restart_outunit ) Intcp_on
        WRITE ( Restart_outunit ) Intcp_form
        WRITE ( Restart_outunit ) Net_rain
        WRITE ( Restart_outunit ) Net_snow
        WRITE ( Restart_outunit ) Net_ppt
        WRITE ( Restart_outunit ) Intcp_stor
        WRITE ( Restart_outunit ) Intcp_evap
        WRITE ( Restart_outunit ) Hru_intcpstor
        WRITE ( Restart_outunit ) Hru_intcpevap
        WRITE ( Restart_outunit ) Canopy_covden
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Basin_net_ppt, Basin_intcp_stor, Basin_intcp_evap, Basin_changeover, &
     &                          Basin_net_snow, Basin_net_rain
        READ ( Restart_inunit ) Intcp_transp_on
        READ ( Restart_inunit ) Intcp_on
        READ ( Restart_inunit ) Intcp_form
        READ ( Restart_inunit ) Net_rain
        READ ( Restart_inunit ) Net_snow
        READ ( Restart_inunit ) Net_ppt
        READ ( Restart_inunit ) Intcp_stor
        READ ( Restart_inunit ) Intcp_evap
        READ ( Restart_inunit ) Hru_intcpstor
        READ ( Restart_inunit ) Hru_intcpevap
        READ ( Restart_inunit ) Canopy_covden
      ENDIF
      END SUBROUTINE intcp_restart

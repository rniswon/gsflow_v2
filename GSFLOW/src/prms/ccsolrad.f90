!***********************************************************************
! Distributes solar radiation to each HRU and estimates missing solar
! radiation data using a relation between solar radiation and cloud cover.
! Declared Parameters
!     ccov_slope, ccov_intcp, radj_sppt, radj_wppt, basin_solsta
!     crad_coef, crad_exp, radmax, ppt_rad_adj, rad_conv, hru_solsta
!RSR: 03/31/2008
!RSR: Warning, summer is based on equinox of Julian days 79 to 265 in
!RSR:          Northern hemisphere and Julian day 265 to 79 in Southern
!***********************************************************************
      MODULE PRMS_CCSOLRAD
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Solar Radiation Distribution'
        character(len=*), parameter :: MODNAME = 'ccsolrad'
        character(len=*), parameter :: Version_ccsolrad = '2022-06-07'
        INTEGER, SAVE :: Observed_flag
        ! Declared Variables
        DOUBLE PRECISION, SAVE :: Basin_radadj
        REAL, SAVE, ALLOCATABLE :: Cloud_radadj(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Crad_coef(:, :), Crad_exp(:, :)
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      use PRMS_MMFAPI, only: declvar_real, declvar_dble
      use PRMS_READ_PARAM_FILE, only: declparam, getparam_real
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, DEBUG_less, MONTHS_PER_YEAR, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Print_debug, Nhru, Nsol, Nowmonth
      USE PRMS_CCSOLRAD
      USE PRMS_CLOUD_COVER, ONLY: Cloud_cover_hru
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Basin_orad, Orad_hru, &
     &    Rad_conv, Hru_solsta, Basin_horad, Basin_potsw, Basin_swrad, Basin_solsta, Orad, Hru_ppt, &
     &    Solsta_flag, Radj_sppt, Radj_wppt, Ppt_rad_adj, Radmax
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl, Soltab_horad_potsw
      USE PRMS_SET_TIME, ONLY: Jday, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      use prms_utils, only: print_date, print_module, read_error
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE, SNGL
! Local Variables
      INTEGER :: j, jj, k
      REAL :: pptadj, radadj
!***********************************************************************
      ccsolrad = 0

      IF ( Process_flag==RUN ) THEN
        !rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)
        Basin_swrad = 0.0D0
        Basin_orad = 0.0D0
        Basin_radadj = 0.0D0
        DO jj = 1, Active_hrus
          j = Hru_route_order(jj)

          ! determine radiation adjustment due to precipitation
          IF ( Hru_ppt(j)>Ppt_rad_adj(j,Nowmonth) ) THEN
            IF ( Summer_flag==ACTIVE ) THEN
              pptadj = Radj_sppt(j)
            ELSE
              pptadj = Radj_wppt(j) ! Winter
            ENDIF
          ELSE
            pptadj = 1.0
          ENDIF

          radadj = Crad_coef(j, Nowmonth) + (1.0-Crad_coef(j,Nowmonth)) * ((1.0-Cloud_cover_hru(j))**Crad_exp(j,Nowmonth))
          IF ( radadj > Radmax(j,Nowmonth) ) radadj = Radmax(j, Nowmonth)
          Cloud_radadj(j) = radadj*pptadj
          Basin_radadj = Basin_radadj + DBLE( Cloud_radadj(j)*Hru_area(j) )

          Orad_hru(j) = Cloud_radadj(j)*SNGL( Soltab_horad_potsw(Jday,j) )
          Basin_orad = Basin_orad + DBLE( Orad_hru(j)*Hru_area(j) )
          IF ( Solsta_flag==ACTIVE ) THEN
            k = Hru_solsta(j)
            IF ( k>0 ) THEN
              IF ( Solrad(k)<0.0 .OR. Solrad(k)>10000.0 ) THEN
                IF ( Print_debug>DEBUG_less ) THEN
                  PRINT *, 'WARNING, measured solar radiation missing, HRU:', j, '; station:', k, '; value computed'
                  CALL print_date(1)
                ENDIF
              ELSE
                Swrad(j) = Solrad(k)*Rad_conv
                Basin_swrad = Basin_swrad + DBLE( Swrad(j)*Hru_area(j) )
                CYCLE
              ENDIF
            ENDIF
          ENDIF
          Swrad(j) = SNGL( Soltab_potsw(Jday, j)*DBLE( Cloud_radadj(j))/Hru_cossl(j) )
          Basin_swrad = Basin_swrad + DBLE( Swrad(j)*Hru_area(j) )
        ENDDO
        Basin_orad = Basin_orad*Basin_area_inv
        Basin_radadj = Basin_radadj*Basin_area_inv
        IF ( Observed_flag==ACTIVE ) THEN
          Orad = Solrad(Basin_solsta)*Rad_conv
        ELSE
          Orad = SNGL( Basin_orad )
        ENDIF
        Basin_swrad = Basin_swrad*Basin_area_inv
        Basin_potsw = Basin_swrad

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_ccsolrad)

        ALLOCATE ( Cloud_radadj(Nhru) )
        CALL declvar_real(MODNAME, 'cloud_radadj', 'nhru', Nhru, &
     &       'Radiation adjustment for cloud cover of each HRU', &
     &       'decimal fraction', Cloud_radadj)

        CALL declvar_dble(MODNAME, 'basin_radadj', 'one', 1, &
     &       'Basin area-weighted average radiation adjustment for cloud cover', &
     &       'decimal fraction', Basin_radadj)

        ! Declare Parameters
        ALLOCATE ( Crad_coef(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'crad_coef', 'nhru,nmonths', 'real', &
     &       '0.4', '0.1', '0.7', &
     &       'Coefficient in cloud cover-solar radiation relationship', &
     &       'Coefficient(B) in Thompson(1976) equation;' // &
     &       ' varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')
        ALLOCATE ( Crad_exp(Nhru,MONTHS_PER_YEAR) )
        IF ( declparam(MODNAME, 'crad_exp', 'nhru,nmonths', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')

      ELSEIF ( Process_flag==INIT ) THEN
        ! Get parameters
        IF ( getparam_real(MODNAME, 'crad_coef', Nhru*MONTHS_PER_YEAR, Crad_coef)/=0) CALL read_error(2, 'crad_coef')
        IF ( getparam_real(MODNAME, 'crad_exp', Nhru*MONTHS_PER_YEAR, Crad_exp)/=0) CALL read_error(2, 'crad_exp')

        Cloud_radadj = 0.0

        Observed_flag = OFF
        IF ( Nsol>0 .AND. Basin_solsta>0 ) Observed_flag = ACTIVE

      ENDIF

      END FUNCTION ccsolrad

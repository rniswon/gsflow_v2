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
        character(len=*), parameter :: Version_ccsolrad = '2024-02-08'
        INTEGER, SAVE :: Observed_flag
        ! Declared Variables
        DOUBLE PRECISION, SAVE :: Basin_radadj, Basin_cloud_cover
        REAL, SAVE, ALLOCATABLE :: Cloud_radadj(:), Cloud_cover_hru(:)
        ! Declared Parameters
        REAL, SAVE, ALLOCATABLE :: Crad_coef(:, :), Crad_exp(:, :)
        REAL, SAVE, ALLOCATABLE :: Ccov_slope(:, :), Ccov_intcp(:, :)
        REAL, SAVE, ALLOCATABLE :: Ppt_rad_adj(:, :)
        REAL, SAVE, ALLOCATABLE :: Radj_sppt(:), Radj_wppt(:), Radmax(:, :)
        REAL, SAVE :: Rad_conv
      END MODULE PRMS_CCSOLRAD
!***********************************************************************
      INTEGER FUNCTION ccsolrad()
      use PRMS_MMFAPI, only: declvar_real, declvar_dble
      use PRMS_READ_PARAM_FILE, only: declparam, getparam_real
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, DEBUG_less, Nmonths, ACTIVE, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Print_debug, Nhru, Nsol, Nowmonth, Nhru_nmonths, Cloud_cover_cbh_flag
      USE PRMS_CCSOLRAD
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Basin_area_inv
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Basin_orad, Orad_hru, &
     &    Hru_solsta, Basin_horad, Basin_potsw, Basin_swrad, Basin_solsta, Orad, Hru_ppt, &
     &    Tmax_hru, Tmin_hru, Solsta_flag
      USE PRMS_CLIMATE_HRU, ONLY: Cloud_cover_cbh
      USE PRMS_SOLTAB, ONLY: Soltab_potsw, Soltab_basinpotsw, Hru_cossl, Soltab_horad_potsw
      USE PRMS_SET_TIME, ONLY: Jday, Summer_flag
      USE PRMS_OBS, ONLY: Solrad
      use prms_utils, only: print_date, print_module, read_error
      IMPLICIT NONE
! Functions
      INTRINSIC :: DBLE, SNGL
! Local Variables
      INTEGER :: j, jj, k
      REAL :: pptadj, radadj, ccov
!***********************************************************************
      ccsolrad = 0

      IF ( Process_flag==RUN ) THEN
!rsr using julian day as the soltab arrays are filled by julian day
        Basin_horad = Soltab_basinpotsw(Jday)
        Basin_swrad = 0.0D0
        Basin_orad = 0.0D0
        Basin_radadj = 0.0D0
        Basin_cloud_cover = 0.0D0
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

          IF ( Cloud_cover_cbh_flag==OFF ) THEN
            ccov = Ccov_slope(j, Nowmonth)*(Tmax_hru(j)-Tmin_hru(j)) + Ccov_intcp(j, Nowmonth)
          ELSE
            ccov = Cloud_cover_cbh(j)
          ENDIF
          IF ( ccov<0.0 ) THEN
            ccov = 0.0
          ELSEIF ( ccov>1.0 ) THEN
            ccov = 1.0
          ENDIF
          Cloud_cover_hru(j) = ccov
          Basin_cloud_cover = Basin_cloud_cover + DBLE( ccov*Hru_area(j) )

          radadj = Crad_coef(j, Nowmonth) + &
     &             (1.0-Crad_coef(j,Nowmonth))*((1.0-Cloud_cover_hru(j))**Crad_exp(j,Nowmonth))
          IF ( radadj>Radmax(j,Nowmonth) ) radadj = Radmax(j, Nowmonth)
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
! in Alaska, there are HRUs on certain days when the sun never rises, so this equation doesn't work
! when soltab_potsw or hru_cossl are 0.0
          if ( Soltab_potsw(jday, j) > 0.0D0 .and. Hru_cossl(j) > 0.0D0 ) then
             Swrad(j) = SNGL( Soltab_potsw(Jday, j)*DBLE( Cloud_radadj(j))/Hru_cossl(j) )
          else
             Swrad(j) = 0.0
          endif
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
        Basin_cloud_cover = Basin_cloud_cover*Basin_area_inv

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_ccsolrad)

        ! Declare Parameters
        ALLOCATE ( Cloud_radadj(Nhru) )
        CALL declvar_real(MODNAME, 'cloud_radadj', 'nhru', Nhru, &
     &       'Radiation adjustment for cloud cover of each HRU', &
     &       'decimal fraction', Cloud_radadj)

        CALL declvar_dble(MODNAME, 'basin_radadj', 'one', 1, &
     &       'Basin area-weighted average radiation adjustment for cloud cover', &
     &       'decimal fraction', Basin_radadj)

        ALLOCATE ( Cloud_cover_hru(Nhru) )
        CALL declvar_real(MODNAME, 'cloud_cover_hru', 'nhru', Nhru, &
     &       'Cloud cover proportion of each HRU', &
     &       'decimal fraction', Cloud_cover_hru)

        CALL declvar_dble(MODNAME, 'basin_cloud_cover', 'one', 1, &
     &       'Basin area-weighted average cloud cover proportion', &
     &       'decimal fraction', Basin_cloud_cover)

        ! Declare Parameters
        ALLOCATE ( Crad_coef(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'crad_coef', 'nhru,nmonths', 'real', &
     &       '0.4', '0.1', '0.7', &
     &       'Coefficient in cloud cover-solar radiation relationship', &
     &       'Coefficient(B) in Thompson(1976) equation;' // &
     &       ' varies by region, contour map of values in reference', &
     &       'none')/=0 ) CALL read_error(1, 'crad_coef')

        ALLOCATE ( Crad_exp(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'crad_exp', 'nhru,nmonths', 'real', &
     &       '0.61', '0.2', '0.8', &
     &       'Exponent in cloud cover-solar radiation relationship', &
     &       'Exponent(P) in Thompson(1976) equation', &
     &       'none')/=0 ) CALL read_error(1, 'crad_exp')

        ALLOCATE ( Ccov_slope(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'ccov_slope', 'nhru,nmonths', 'real', &
     &       '-0.13', '-0.5', '-0.01', &
     &       'Slope in temperature cloud cover relationship', &
     &       'Monthly (January to December) coefficient in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_slope')

        ALLOCATE ( Ccov_intcp(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'ccov_intcp', 'nhru,nmonths', 'real', &
     &       '1.83', '0.0', '5.0', &
     &       'Intercept in temperature cloud cover relationship', &
     &       'Monthly (January to December) intercept in cloud-cover relationship', &
     &       'none')/=0 ) CALL read_error(1, 'ccov_intcp')

        ALLOCATE ( Ppt_rad_adj(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'ppt_rad_adj', 'nhru,nmonths', 'real', &
     &       '0.02', '0.0', '0.5', &
     &       'Radiation reduced if HRU precipitation above this value', &
     &       'Monthly minimum precipitation, if HRU precipitation exceeds this value, radiation is'// &
     &       ' multiplied by radj_sppt or radj_wppt adjustment factor', &
     &       'inches')/=0 ) CALL read_error(1, 'ppt_rad_adj')

      ALLOCATE ( Radj_sppt(Nhru) )
      IF ( declparam(MODNAME, 'radj_sppt', 'nhru', 'real', &
     &     '0.44', '0.0', '1.0', &
     &     'Adjustment to solar radiation on precipitation day - summer', &
     &     'Multiplicative adjustment factor for computed solar radiation for summer day with greater than'// &
     &     ' ppt_rad_adj inches of precipitation for each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'radj_sppt')

      ALLOCATE ( Radj_wppt(Nhru) )
      IF ( declparam(MODNAME, 'radj_wppt', 'nhru', 'real', &
     &     '0.5', '0.0', '1.0', &
     &     'Adjustment to solar radiation on precipitation day - winter', &
     &     'Multiplicative adjustment factor for computed solar radiation for winter day with greater than'// &
     &     ' ppt_rad_adj inches of precipitation for each HRU', &
     &     'decimal fraction')/=0 ) CALL read_error(1, 'radj_wppt')

        ALLOCATE ( Radmax(Nhru,Nmonths) )
        IF ( declparam(MODNAME, 'radmax', 'nhru,nmonths', 'real', &
     &       '0.8', '0.1', '1.0', &
     &       'Maximum fraction of potential solar radiation', &
     &       'Monthly (January to December) maximum fraction of the potential solar radiation'// &
     &       ' that may reach the ground due to haze, dust, smog, and so forth, for each HRU', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'radmax')

        IF ( Nsol > 0 ) THEN
          IF ( declparam(MODNAME, 'rad_conv', 'one', 'real', &
     &         '1.0', '0.1', '100.0', &
     &         'Conversion factor to Langleys for measured radiation', &
     &         'Conversion factor to Langleys for measured solar radiation', &
     &         'Langleys/radiation units')/=0 ) CALL read_error(1, 'rad_conv')
        ENDIF

      ELSEIF ( Process_flag==INIT ) THEN
! Get parameters
        IF ( getparam_real(MODNAME, 'crad_coef', Nhru_nmonths, Crad_coef)/=0 ) CALL read_error(2, 'crad_coef')
        IF ( getparam_real(MODNAME, 'crad_exp', Nhru_nmonths, Crad_exp)/=0 ) CALL read_error(2, 'crad_exp')
        IF ( getparam_real(MODNAME, 'ccov_slope', Nhru_nmonths, Ccov_slope)/=0 ) CALL read_error(2, 'ccov_slope')
        IF ( getparam_real(MODNAME, 'ccov_intcp', Nhru_nmonths, Ccov_intcp)/=0 ) CALL read_error(2, 'ccov_intcp')
        IF ( getparam_real(MODNAME, 'ppt_rad_adj', Nhru_nmonths, Ppt_rad_adj)/=0 ) CALL read_error(2, 'ppt_rad_adj')
        IF ( getparam_real(MODNAME, 'radmax', Nhru_nmonths, Radmax)/=0 ) CALL read_error(2, 'radmax')
        IF ( getparam_real(MODNAME, 'radj_sppt', Nhru, Radj_sppt)/=0 ) CALL read_error(2, 'radj_sppt')
        IF ( getparam_real(MODNAME, 'radj_wppt', Nhru, Radj_wppt)/=0 ) CALL read_error(2, 'radj_wppt')

        Cloud_radadj = 0.0
        Cloud_cover_hru = 0.0

        Observed_flag = OFF
        IF ( Nsol > 0 ) THEN
          IF ( getparam_real(MODNAME, 'rad_conv', 1, Rad_conv)/=0 ) CALL read_error(2, 'rad_conv')
          IF ( Basin_solsta>0 ) Observed_flag = ACTIVE
        ENDIF
      ENDIF

      END FUNCTION ccsolrad

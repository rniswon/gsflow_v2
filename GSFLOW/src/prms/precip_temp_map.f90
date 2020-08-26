!***********************************************************************
! Maximum, minimum, average temperatures, and precipitation are distributed to each HRU
! using temperature and/or precipitation data input as a time series of gridded
! or other spatial units using an area-weighted method and correction factors to
! account for differences in altitude, spatial variation, topography, and 
! measurement gage efficiency
!***********************************************************************
      MODULE PRMS_PRECIP_TEMP_MAP
        USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, SETDIMENS, OFF, MAXDIM, MAXFILE_LENGTH, &
     &      MM, MM2INCH, MONTHS_PER_YEAR, DOCUMENTATION, precip_map_module, temp_map_module
        USE PRMS_MODULE, ONLY: Model, Process_flag, Temp_flag, Precip_flag, Start_year, Start_month, Start_day
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Precipitation Distribution'
        character(len=*), parameter :: MODDESC2 = 'Temperature Distribution'
        character(len=*), parameter :: MODNAME = 'precip_temp_map'
        character(len=*), parameter :: Version_precip_temp_map = '2020-08-26'
        INTEGER, SAVE :: Nmap2hru, Nmap, Precip_unit, Tmax_unit, Tmin_unit
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru2map_id(:), Map2hru_id(:)
        REAL, SAVE, ALLOCATABLE :: Hru2map_pct(:), Tmax_map_values(:), Tmin_map_values(:), Precip_map_values(:)
        REAL, SAVE, ALLOCATABLE :: Tmax_map_adj(:, :), Tmin_map_adj(:, :), Precip_map_adj(:, :)
        ! parameters in basin:
        !    hru_area
        ! Control Parameters
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Tmin_map, Tmax_map, Precip_map
      END MODULE PRMS_PRECIP_TEMP_MAP

      SUBROUTINE precip_temp_map()
      USE PRMS_PRECIP_TEMP_MAP
      USE PRMS_BASIN, ONLY: Hru_area, Basin_area_inv, Active_hrus, Hru_route_order
      USE PRMS_CLIMATEVARS, ONLY: Solrad_tmax, Solrad_tmin, Basin_temp, &
     &    Basin_tmax, Basin_tmin, Tmaxf, Tminf, Tminc, Tmaxc, Tavgf, &
     &    Tavgc, Hru_ppt, Hru_rain, Hru_snow, Prmx, Pptmix, Newsnow, &
     &    Precip_units, Tmax_allrain_f, Adjmix_rain, &
     &    Basin_ppt, Basin_snow, Basin_rain, Basin_obs_ppt, Tmax_allsnow_f
      USE PRMS_SET_TIME, ONLY: Nowmonth
! Functions
      INTEGER, EXTERNAL :: declparam, getparam, getdim, decldim, control_string
      EXTERNAL :: read_error, precip_form, temp_set, find_header_end, find_current_time
      EXTERNAL :: read_cbh_date, print_module, print_date
! Local Variables
      INTEGER :: yr, mo, dy, i, hr, mn, sec, ierr, ios, j, kg, kh, istop
      REAL :: tmax_hru, tmin_hru, ppt, harea
!***********************************************************************
       IF ( Process_flag==RUN ) THEN
        IF ( Temp_flag==temp_map_module ) THEN
          READ ( Tmax_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Tmax_map_values(i), i=1,Nmap)
          READ ( Tmin_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Tmin_map_values(i), i=1,Nmap)
          Basin_tmax = 0.0D0
          Basin_tmin = 0.0D0
          Basin_temp = 0.0D0
          Tmaxf = 0.0
          Tminf = 0.0
        ENDIF

        IF ( Precip_flag==precip_map_module ) THEN
          READ ( Precip_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Precip_map_values(i), i=1,Nmap)
          Basin_ppt = 0.0D0
          Basin_rain = 0.0D0
          Basin_snow = 0.0D0
          Basin_obs_ppt = 0.0D0
          Hru_ppt = 0.0
        ENDIF

        DO j = 1, Nmap2hru
          kg = Map2hru_id(j)
          kh = Hru2map_id(j)
          IF ( Temp_flag==temp_map_module ) THEN
            Tmaxf(kh) = Tmaxf(kh) + Tmax_map_values(kg)*Hru2map_pct(j) + Tmax_map_adj(kg, Nowmonth)
            Tminf(kh) = Tminf(kh) + Tmin_map_values(kg)*Hru2map_pct(j) + Tmin_map_adj(kg, Nowmonth)
          ENDIF
          IF ( Precip_flag==precip_map_module ) &
       &       Hru_ppt(kh) = Hru_ppt(kh) + Precip_map_values(kg)*Hru2map_pct(j)*Precip_map_adj(kg, Nowmonth)
        ENDDO

        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          harea = Hru_area(i)

          IF ( Temp_flag==temp_map_module ) THEN
            tmax_hru = Tmaxf(i)
            tmin_hru = Tminf(i)
            CALL temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), &
     &                    Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i), harea)
          ENDIF

          IF ( Precip_flag==precip_map_module ) THEN
!******Initialize HRU variables
            Pptmix(i) = OFF
            Newsnow(i) = OFF
            Prmx(i) = 0.0
            Hru_rain(i) = 0.0
            Hru_snow(i) = 0.0

            IF ( Hru_ppt(i)>0.0 ) THEN
              IF ( Precip_units==MM ) Hru_ppt(i) = Hru_ppt(i)*MM2INCH
              ppt = Hru_ppt(i)
              CALL precip_form(ppt, Hru_ppt(i), Hru_rain(i), Hru_snow(i), &
     &                         Tmaxf(i), Tminf(i), Pptmix(i), Newsnow(i), &
     &                         Prmx(i), Tmax_allrain_f(i,Nowmonth), 1.0, 1.0, &
     &                         Adjmix_rain(i,Nowmonth), harea, Basin_obs_ppt, Tmax_allsnow_f(i,Nowmonth))
            ELSEIF ( Hru_ppt(i)<0.0 ) THEN
              PRINT *, 'WARNING, negative precipitation value entered in precipitation map file and set to 0.0, HRU:', i
              CALL print_date(0)
              Hru_ppt(i) = 0.0
            ENDIF
          ENDIF

        ENDDO

        IF ( Temp_flag==temp_map_module ) THEN
          Basin_tmax = Basin_tmax*Basin_area_inv
          Basin_tmin = Basin_tmin*Basin_area_inv
          Basin_temp = Basin_temp*Basin_area_inv
          Solrad_tmax = Basin_tmax
          Solrad_tmin = Basin_tmin
        ENDIF

        IF ( Precip_flag==precip_map_module ) THEN
          Basin_ppt = Basin_ppt*Basin_area_inv
          Basin_obs_ppt = Basin_obs_ppt*Basin_area_inv
          Basin_rain = Basin_rain*Basin_area_inv
          Basin_snow = Basin_snow*Basin_area_inv
        ENDIF

      ELSEIF ( Process_flag==SETDIMENS ) THEN
!      declare precip_temp_map module specific dimensions
        IF ( decldim('nmap2hru', 0, MAXDIM, 'Number of intersections between HRUs and input climate map')/=0 ) &
     &       CALL read_error(7, 'nmap2hru')
        IF ( decldim('nmap', 0, MAXDIM, 'Number of mapped values')/=0 ) CALL read_error(7, 'nmap')

      ELSEIF ( Process_flag==DECL ) THEN
        IF ( Temp_flag==temp_map_module .OR. Model==DOCUMENTATION ) CALL print_module(MODDESC2, MODNAME, Version_precip_temp_map)
        IF ( Precip_flag==precip_map_module .OR. Model==DOCUMENTATION ) CALL print_module(MODDESC, MODNAME, Version_precip_temp_map)

        Nmap2hru = getdim('Nmap2hru')
        IF ( Nmap2hru==-1 ) CALL read_error(6, 'Nmap2hru')
        Nmap = getdim('Nmap')
        IF ( Nmap==-1 ) CALL read_error(6, 'Nmap')
        IF ( Model==DOCUMENTATION ) THEN
          IF ( Nmap2hru==0 ) Nmap2hru = 1
          IF ( Nmap==0 ) Nmap = 1
        ENDIF

        IF ( Temp_flag==temp_map_module ) ALLOCATE ( Tmax_map_values(Nmap), Tmin_map_values(Nmap) )
        IF ( Precip_flag==precip_map_module ) ALLOCATE ( Precip_map_values(Nmap) )

! Declare parameters
        IF ( Temp_flag==temp_map_module .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Tmax_map_adj(Nmap,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'tmax_map_adj', 'nmap,nmonths', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'Monthly maximum temperature adjustment factor for each mapped spatial unit', &
     &         'Monthly (January to December) additive adjustment factor to maximum air temperature for each mapped,'// &
     &         ' spatial unit estimated on the basis of slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmax_map_adj')
          ALLOCATE ( Tmin_map_adj(Nmap,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'tmin_map_adj', 'nmap,nmonths', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'Monthly minimum temperature adjustment factor for each mapped spatial unit', &
     &         'Monthly (January to December) additive adjustment factor to minimum air temperature for each'// &
     &         ' mapped spatial unit, estimated on the basis of slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmin_map_adj')
        ENDIF

        IF ( Precip_flag==precip_map_module .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Precip_map_adj(Nmap,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'precip_map_adj', 'nmap,nmonths', 'real', &
     &       '1.0', '0.5', '2.0', &
     &       'Monthly rain adjustment factor for each mapped spatial unit', &
     &       'Monthly (January to December) multiplicative adjustment factor to mapped precipitation to account for'// &
     &       ' differences in elevation, and so forth', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'precip_map_adj')
        ENDIF

        ALLOCATE ( Hru2map_id(Nmap2hru) )
        IF ( declparam(MODNAME, 'hru2map_id', 'Nmap2hru', 'integer', &
     &       '1', 'bounded', 'nhru', &
     &       'HRU identification number for HRU to mapped spatial units intersection', &
     &       'HRU identification number for HRU to mapped spatial units intersection', &
     &       'none')/=0 ) CALL read_error(1, 'hru2map_id')

        !rsr, bounded value could be a problem if number of mapped spatial units > nhru
        ALLOCATE ( Map2hru_id(Nmap2hru) )
        IF ( declparam(MODNAME, 'map2hru_id', 'nmap2hru', 'integer', &
     &       '0', 'bounded', 'Nmap', &
     &       'Mapped spatial unit identification number for HRU to map intersection', &
     &       'Mapped spatial unit identification number for HRU to map intersection', &
     &       'none')/=0 ) CALL read_error(1, 'map2hru_id')

        ALLOCATE ( Hru2map_pct(Nmap2hru) )
        IF ( declparam(MODNAME, 'hru2map_pct', 'nmap2hru', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Portion of HRU associated with each HRU to map intersection', &
     &       'Portion of HRU associated with each HRU to map intersection', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'hru2map_pct')

! Get parameters
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam(MODNAME, 'map2hru_id', Nmap2hru, 'integer', Map2hru_id)/=0 ) CALL read_error(2, 'map2hru_id')
        IF ( getparam(MODNAME, 'hru2map_id', Nmap2hru, 'integer', Hru2map_id)/=0 ) CALL read_error(2, 'hru2map_id')
        IF ( getparam(MODNAME, 'hru2map_pct', Nmap2hru, 'real', Hru2map_pct)/=0 ) CALL read_error(2, 'hru2map_pct')

        istop = 0
        ierr = 0

        IF ( Temp_flag==temp_map_module ) THEN
          IF ( getparam(MODNAME, 'Tmax_map_adj', Nmap*12, 'real', Tmax_map_adj)/=0 ) CALL read_error(2, 'Tmax_map_adj')
          IF ( getparam(MODNAME, 'Tmin_map_adj', Nmap*12, 'real', Tmin_map_adj)/=0 ) CALL read_error(2, 'Tmin_map_adj')
          IF ( control_string(Tmax_map, 'Tmax_map')/=0 ) CALL read_error(5, 'Tmax_map')
          IF ( control_string(Tmin_map, 'Tmin_map')/=0 ) CALL read_error(5, 'Tmin_map')
          CALL find_header_end(Tmax_unit, Tmax_map, 'Tmax_map', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmax_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Tmax Map File: ', Tmax_map
              istop = 1
            ENDIF
          ENDIF
          CALL find_header_end(Tmin_unit, Tmin_map, 'Tmin_map', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmin_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Tmin Map File: ', Tmin_map
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Precip_flag==precip_map_module ) THEN
          IF ( getparam(MODNAME, 'Precip_map_adj', Nmap*12, 'real', Precip_map_adj)/=0 ) CALL read_error(2, 'Precip_map_adj')
          IF ( control_string(Precip_map, 'Precip_map')/=0 ) CALL read_error(5, 'Precip_map')
          CALL find_header_end(Precip_unit, Precip_map, 'Precip_map', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Precip_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Precip Map File: ', Precip_map
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( istop==1 ) STOP 'ERROR in precip_temp_map module'

      ENDIF

 9002 FORMAT (/, 'WARNING: negative precipitation value:', F0.4, 'specified for module ', A, /, &
     &        'precipitation station:', I0, '; Time:', I5, 2('/', I2.2), I3, 2(':', I2.2), '; value set to 0.0')

      END SUBROUTINE precip_temp_map

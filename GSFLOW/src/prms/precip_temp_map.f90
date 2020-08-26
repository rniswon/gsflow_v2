!***********************************************************************
! Distributes maximum, minimum, average temperatures, and precipitation to each HRU
! using temperature and precipitation data input as a time series grid using an
! area-weighted method and correction factors to account for differences
! in altitude, spatial variation, topography, and measurement gage efficiency
!***********************************************************************
      MODULE PRMS_PRECIP_TEMP_GRID
        USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, SETDIMENS, OFF, MAXDIM, MAXFILE_LENGTH, &
     &      MM, MM2INCH, MONTHS_PER_YEAR, DOCUMENTATION, precip_grid_module, temp_grid_module
        USE PRMS_MODULE, ONLY: Model, Process_flag, Temp_flag, Precip_flag, Start_year, Start_month, Start_day
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Precipitation Distribution'
        character(len=*), parameter :: MODDESC2 = 'Temperature Distribution'
        character(len=*), parameter :: MODNAME = 'precip_temp_grid'
        character(len=*), parameter :: Version_precip_temp_grid = '2020-07-30'
        INTEGER, SAVE :: Ngrid2hru, Ngrid, Precip_unit, Tmax_unit, Tmin_unit
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Hru2grid_id(:), Grid2hru_id(:)
        REAL, SAVE, ALLOCATABLE :: Hru2grid_pct(:), Tmax_grid_values(:), Tmin_grid_values(:), Precip_grid_values(:)
        REAL, SAVE, ALLOCATABLE :: Tmax_grid_adj(:, :), Tmin_grid_adj(:, :), Precip_grid_adj(:, :)
        ! parameters in basin:
        !    hru_area
        ! Control Parameters
        CHARACTER(LEN=MAXFILE_LENGTH), SAVE :: Tmin_grid, Tmax_grid, Precip_grid
      END MODULE PRMS_PRECIP_TEMP_GRID

      SUBROUTINE precip_temp_grid()
      USE PRMS_PRECIP_TEMP_GRID
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
        IF ( Temp_flag==temp_grid_module ) THEN
          READ ( Tmax_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Tmax_grid_values(i), i=1,Ngrid)
          READ ( Tmin_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Tmin_grid_values(i), i=1,Ngrid)
          Basin_tmax = 0.0D0
          Basin_tmin = 0.0D0
          Basin_temp = 0.0D0
          Tmaxf = 0.0
          Tminf = 0.0
        ENDIF

        IF ( Precip_flag==precip_grid_module ) THEN
          READ ( Precip_unit, *, IOSTAT=ios ) yr, mo, dy, hr, mn, sec, (Precip_grid_values(i), i=1,Ngrid)
          Basin_ppt = 0.0D0
          Basin_rain = 0.0D0
          Basin_snow = 0.0D0
          Basin_obs_ppt = 0.0D0
          Hru_ppt = 0.0
        ENDIF

        DO j = 1, Ngrid2hru
          kg = Grid2hru_id(j)
          kh = Hru2grid_id(j)
          IF ( Temp_flag==temp_grid_module ) THEN
            Tmaxf(kh) = Tmaxf(kh) + Tmax_grid_values(kg)*Hru2grid_pct(j) + Tmax_grid_adj(kg, Nowmonth)
            Tminf(kh) = Tminf(kh) + Tmin_grid_values(kg)*Hru2grid_pct(j) + Tmin_grid_adj(kg, Nowmonth)
          ENDIF
          IF ( Precip_flag==precip_grid_module ) &
       &       Hru_ppt(kh) = Hru_ppt(kh) + Precip_grid_values(kg)*Hru2grid_pct(j)*Precip_grid_adj(kg, Nowmonth)
        ENDDO

        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          harea = Hru_area(i)

          IF ( Temp_flag==temp_grid_module ) THEN
            tmax_hru = Tmaxf(i)
            tmin_hru = Tminf(i)
            CALL temp_set(i, tmax_hru, tmin_hru, Tmaxf(i), Tminf(i), &
     &                    Tavgf(i), Tmaxc(i), Tminc(i), Tavgc(i), harea)
          ENDIF

          IF ( Precip_flag==precip_grid_module ) THEN
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
              PRINT *, 'WARNING, negative precipitation value entered in precipitation grid file and set to 0.0, HRU:', i
              CALL print_date(0)
              Hru_ppt(i) = 0.0
            ENDIF
          ENDIF

        ENDDO

        IF ( Temp_flag==temp_grid_module ) THEN
          Basin_tmax = Basin_tmax*Basin_area_inv
          Basin_tmin = Basin_tmin*Basin_area_inv
          Basin_temp = Basin_temp*Basin_area_inv
          Solrad_tmax = Basin_tmax
          Solrad_tmin = Basin_tmin
        ENDIF

        IF ( Precip_flag==precip_grid_module ) THEN
          Basin_ppt = Basin_ppt*Basin_area_inv
          Basin_obs_ppt = Basin_obs_ppt*Basin_area_inv
          Basin_rain = Basin_rain*Basin_area_inv
          Basin_snow = Basin_snow*Basin_area_inv
        ENDIF

      ELSEIF ( Process_flag==SETDIMENS ) THEN
!      declare precip_temp_grid module specific dimensions
        IF ( decldim('ngrid2hru', 0, MAXDIM, 'Number of intersections between HRUs and input climate grid')/=0 ) &
     &       CALL read_error(7, 'ngrid2hru')
        IF ( decldim('ngrid', 0, MAXDIM, 'Number of grid values')/=0 ) CALL read_error(7, 'ngrid')

      ELSEIF ( Process_flag==DECL ) THEN
        IF ( Temp_flag==temp_grid_module .OR. Model==DOCUMENTATION ) CALL print_module(MODDESC2, MODNAME, Version_precip_temp_grid)
        IF ( Precip_flag==precip_grid_module .OR. Model==DOCUMENTATION ) CALL print_module(MODDESC, MODNAME, Version_precip_temp_grid)

        Ngrid2hru = getdim('ngrid2hru')
        IF ( Ngrid2hru==-1 ) CALL read_error(6, 'ngrid2hru')
        Ngrid = getdim('ngrid')
        IF ( Ngrid==-1 ) CALL read_error(6, 'ngrid')
        IF ( Model==DOCUMENTATION ) THEN
          IF ( Ngrid2hru==0 ) Ngrid2hru = 1
          IF ( Ngrid==0 ) Ngrid = 1
        ENDIF

        IF ( Temp_flag==temp_grid_module ) ALLOCATE ( Tmax_grid_values(Ngrid), Tmin_grid_values(Ngrid) )
        IF ( Precip_flag==precip_grid_module ) ALLOCATE ( Precip_grid_values(Ngrid) )

! Declare parameters
        IF ( Temp_flag==temp_grid_module .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Tmax_grid_adj(Ngrid,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'tmax_grid_adj', 'ngrid,nmonths', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'Monthly maximum temperature adjustment factor for each grid cell', &
     &         'Monthly (January to December) additive adjustment factor to maximum air temperature for each grid cell,'// &
     &         ' estimated on the basis of slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmax_grid_adj')
          ALLOCATE ( Tmin_grid_adj(Ngrid,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'tmin_grid_adj', 'ngrid,nmonths', 'real', &
     &         '0.0', '-10.0', '10.0', &
     &         'Monthly minimum temperature adjustment factor for each grid cell', &
     &         'Monthly (January to December) additive adjustment factor to minimum air temperature for each grid cell,'// &
     &         ' estimated on the basis of slope and aspect', &
     &         'temp_units')/=0 ) CALL read_error(1, 'tmin_grid_adj')
        ENDIF

        IF ( Precip_flag==precip_grid_module .OR. Model==DOCUMENTATION ) THEN
          ALLOCATE ( Precip_grid_adj(Ngrid,MONTHS_PER_YEAR) )
          IF ( declparam(MODNAME, 'precip_grid_adj', 'ngrid,nmonths', 'real', &
     &       '1.0', '0.5', '2.0', &
     &       'Monthly rain adjustment factor for each grid cell', &
     &       'Monthly (January to December) multiplicative adjustment factor to gridded precipitation to account for'// &
     &       ' differences in elevation, and so forth', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'precip_grid_adj')
        ENDIF

        ALLOCATE ( Hru2grid_id(Ngrid2hru) )
        IF ( declparam(MODNAME, 'hru2grid_id', 'ngrid2hru', 'integer', &
     &       '1', 'bounded', 'nhru', &
     &       'HRU identification number for HRU to grid intersection', &
     &       'HRU identification number for HRU to grid intersection', &
     &       'none')/=0 ) CALL read_error(1, 'hru2grid_id')

        !rsr, bounded value could be a problem if number of grids > nhru
        ALLOCATE ( Grid2hru_id(Ngrid2hru) )
        IF ( declparam(MODNAME, 'grid2hru_id', 'ngrid2hru', 'integer', &
     &       '0', 'bounded', 'ngrid', &
     &       'Grid cell identification number for HRU to grid intersection', &
     &       'Grid cell identification number for HRU to grid intersection', &
     &       'none')/=0 ) CALL read_error(1, 'grid2hru_id')

        ALLOCATE ( Hru2grid_pct(Ngrid2hru) )
        IF ( declparam(MODNAME, 'hru2grid_pct', 'ngrid2hru', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Portion of HRU associated with each HRU to grid intersection', &
     &       'Portion of HRU associated with each HRU to grid intersection', &
     &       'decimal fraction')/=0 ) CALL read_error(1, 'hru2grid_pct')

! Get parameters
      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam(MODNAME, 'grid2hru_id', Ngrid2hru, 'integer', Grid2hru_id)/=0 ) CALL read_error(2, 'grid2hru_id')
        IF ( getparam(MODNAME, 'hru2grid_id', Ngrid2hru, 'integer', Hru2grid_id)/=0 ) CALL read_error(2, 'hru2grid_id')
        IF ( getparam(MODNAME, 'hru2grid_pct', Ngrid2hru, 'real', Hru2grid_pct)/=0 ) CALL read_error(2, 'hru2grid_pct')

        istop = 0
        ierr = 0

        IF ( Temp_flag==temp_grid_module ) THEN
          IF ( getparam(MODNAME, 'tmax_grid_adj', Ngrid*12, 'real', Tmax_grid_adj)/=0 ) CALL read_error(2, 'tmax_grid_adj')
          IF ( getparam(MODNAME, 'tmin_grid_adj', Ngrid*12, 'real', Tmin_grid_adj)/=0 ) CALL read_error(2, 'tmin_grid_adj')
          IF ( control_string(Tmax_grid, 'tmax_grid')/=0 ) CALL read_error(5, 'tmax_grid')
          IF ( control_string(Tmin_grid, 'tmin_grid')/=0 ) CALL read_error(5, 'tmin_grid')
          CALL find_header_end(Tmax_unit, Tmax_grid, 'tmax_grid', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmax_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Tmax Grid File: ', Tmax_grid
              istop = 1
            ENDIF
          ENDIF
          CALL find_header_end(Tmin_unit, Tmin_grid, 'tmin_grid', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Tmin_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Tmin Grid File: ', Tmin_grid
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( Precip_flag==precip_grid_module ) THEN
          IF ( getparam(MODNAME, 'precip_grid_adj', Ngrid*12, 'real', Precip_grid_adj)/=0 ) CALL read_error(2, 'precip_grid_adj')
          IF ( control_string(Precip_grid, 'precip_grid')/=0 ) CALL read_error(5, 'precip_grid')
          CALL find_header_end(Precip_unit, Precip_grid, 'precip_grid', ierr, 1, 0)
          IF ( ierr==1 ) THEN
            istop = 1
          ELSE
            CALL find_current_time(Precip_unit, Start_year, Start_month, Start_day, ierr, 0)
            IF ( ierr==-1 ) THEN
              PRINT *, 'for first time step, Precip Grid File: ', Precip_grid
              istop = 1
            ENDIF
          ENDIF
        ENDIF

        IF ( istop==1 ) STOP 'ERROR in precip_temp_grid module'

      ENDIF

 9002 FORMAT (/, 'WARNING: negative precipitation value:', F0.4, 'specified for module ', A, /, &
     &        'precipitation station:', I0, '; Time:', I5, 2('/', I2.2), I3, 2(':', I2.2), '; value set to 0.0')

      END SUBROUTINE precip_temp_grid

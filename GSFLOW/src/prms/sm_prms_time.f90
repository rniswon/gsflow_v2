submodule(PRMS_SET_TIME) sm_prms_time

contains
!***********************************************************************
!***********************************************************************
  integer module function prms_time()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, YEAR, MONTH, DAY, HOUR, MINUTE, MAX_DAYS_PER_YEAR, DAYS_PER_YEAR, &
                                ACTIVE, OFF, NORTHERN, FT2_PER_ACRE, SECS_PER_HOUR, INCHES_PER_FOOT, SECS_PER_DAY, &
                                ERROR_time, CANOPY
      use PRMS_MMFAPI, only: dattim, deltim
      USE PRMS_MODULE, ONLY: Process_flag, Timestep, Starttime, Nowyear, Nowmonth, Nowday, Dprst_flag, &
                             GSFLOW_flag, PRMS_land_iteration_flag, AG_flag, &
                             Ag_package, Hru_ag_irr, timestep_start_flag, Nlake
      USE PRMS_BASIN, ONLY: Hemisphere, Basin_area_inv
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Pkwater_equiv, Hru_intcpstor, &
                               Ssres_stor, Slow_stor, Pref_flow_stor, Basin_ssstor, &
                               Basin_soil_moist, Intcp_transp_on, Dprst_stor_hru, Intcp_stor, &
                               Gravity_stor_res, Ag_soil_moist, Ag_soil_rechr, Imperv_stor, &
                               Dprst_vol_open, Dprst_vol_clos, Hru_impervstor, &
                               Basin_ag_soil_moist, Basin_ag_soil_rechr
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_IT0_VARS, ONLY: It0_soil_moist, It0_soil_rechr, It0_pkwater_equiv, &
                               It0_hru_intcpstor, It0_ssres_stor, It0_slow_stor, It0_pref_flow_stor, &
                               It0_basin_ssstor, It0_basin_soil_moist, It0_potet, &
                               It0_intcp_transp_on, It0_dprst_stor_hru, It0_intcp_stor, &
                               It0_gravity_stor_res, It0_ag_soil_moist, It0_ag_soil_rechr, It0_imperv_stor, &
                               It0_hru_impervstor, It0_dprst_vol_open, It0_dprst_vol_clos, &
                               It0_basin_ag_soil_moist, It0_basin_ag_soil_rechr
      use PRMS_DATA_FILE, only: read_data_line
      use prms_utils, only: leap_day, julian_day, compute_julday, print_module
      IMPLICIT NONE
! Functions
      INTRINSIC :: SNGL
! Local Variables
      INTEGER :: startday
      DOUBLE PRECISION :: dt
!***********************************************************************
      prms_time = 0

      IF ( Process_flag==RUN .OR. Process_flag==INIT ) THEN

        IF ( Process_flag==RUN ) THEN
          Timestep = Timestep + 1
          timestep_start_flag = ACTIVE

          CALL dattim('now', Nowtime)
          Jday = julian_day('now', 'calendar')
          Jsol = julian_day('now', 'solar')
          Julwater = julian_day('now', 'water')
          Julian_day_absolute = Julian_day_absolute + 1
          CALL read_data_line()
          It0_basin_soil_moist = Basin_soil_moist
          It0_basin_ssstor = Basin_ssstor
          It0_soil_moist = Soil_moist
          It0_soil_rechr = Soil_rechr
          It0_ssres_stor = Ssres_stor
          It0_hru_impervstor = Hru_impervstor
          It0_slow_stor = Slow_stor
          It0_pref_flow_stor = Pref_flow_stor
          It0_pkwater_equiv = Pkwater_equiv
          IF ( Ag_package==ACTIVE ) Hru_ag_irr = 0.0 ! 
          IF ( GSFLOW_flag==ACTIVE ) It0_gravity_stor_res = Gravity_stor_res
          It0_hru_intcpstor = Hru_intcpstor
          IF ( PRMS_land_iteration_flag==CANOPY ) THEN
            It0_intcp_transp_on = Intcp_transp_on
            It0_intcp_stor = Intcp_stor
          ENDIF
          It0_imperv_stor = Imperv_stor
          IF ( Dprst_flag==ACTIVE ) THEN
            It0_dprst_vol_open = Dprst_vol_open
            It0_dprst_vol_clos = Dprst_vol_clos
            It0_dprst_stor_hru = Dprst_stor_hru
          ENDIF
          IF ( AG_flag==ACTIVE ) THEN
            It0_basin_ag_soil_moist = Basin_ag_soil_moist
            It0_basin_ag_soil_rechr = Basin_ag_soil_rechr
            It0_ag_soil_moist = Ag_soil_moist
            It0_ag_soil_rechr = Ag_soil_rechr
          ENDIF
          IF ( Nlake>0 ) It0_potet = Potet

        ELSE ! initialize
          Modays(1) = 31
          Modays(3) = 31
          Modays(4) = 30
          Modays(5) = 31
          Modays(6) = 30
          Modays(7) = 31
          Modays(8) = 31
          Modays(9) = 30
          Modays(10) = 31
          Modays(11) = 30
          Modays(12) = 31

          Nowtime = Starttime
          Jday = julian_day('start', 'calendar')
          Jsol = julian_day('start', 'solar')
          Julwater = julian_day('start', 'water')
          startday = compute_julday(Starttime(YEAR), Starttime(MONTH), Starttime(DAY))
          Julian_day_absolute = startday
        ENDIF

        Nowyear = Nowtime(YEAR)
        Nowmonth = Nowtime(MONTH)
        Nowday = Nowtime(DAY)
        Nowhour = Nowtime(HOUR)
        Nowminute = Nowtime(MINUTE)

        IF ( leap_day(Nowyear)==1 ) THEN
          Yrdays = MAX_DAYS_PER_YEAR
          Modays(2) = 29
        ELSE
          Yrdays = DAYS_PER_YEAR
          Modays(2) = 28
        ENDIF

        ! Summer is based on equinox:
        !   Julian days 79 to 265 for Northern hemisphere
        !   Julian day 265 to 79 in Southern hemisphere
        Summer_flag = ACTIVE ! 1 = summer, 0 = winter
        IF ( Hemisphere==NORTHERN ) THEN
          IF ( Jday<79 .OR. Jday>265 ) Summer_flag = OFF ! Equinox
        ELSE ! Southern Hemisphere
          IF ( Jday>79 .AND. Jday<265 ) Summer_flag = OFF ! Equinox
        ENDIF

        dt = deltim()
        Timestep_hours = SNGL( dt )
        Timestep_days = Timestep_hours/24.0
        Timestep_minutes = Timestep_hours*60.0
        Timestep_seconds = dt*SECS_PER_HOUR
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE

        ! Check to see if in a daily or subdaily time step
        IF ( Timestep_hours>24.0 ) THEN
          PRINT *, 'ERROR, timestep > daily, fix Data File, timestep:', Timestep_hours
          ERROR STOP ERROR_time
        ELSEIF ( Timestep_hours<24.0 ) THEN
          PRINT *, 'ERROR, timestep < daily for daily model, fix Data File', Timestep_hours
          ERROR STOP ERROR_time
        ENDIF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_prms_time)
        Timestep_seconds = SECS_PER_DAY
        Cfs_conv = FT2_PER_ACRE/INCHES_PER_FOOT/Timestep_seconds
        Cfs2inches = Basin_area_inv*INCHES_PER_FOOT*Timestep_seconds/FT2_PER_ACRE
      ENDIF

      END FUNCTION prms_time
end submodule

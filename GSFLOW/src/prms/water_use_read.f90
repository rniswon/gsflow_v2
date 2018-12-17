!***********************************************************************
! Read and makes available water-use data (diversions and gains)
! from files pre-processed Data Files available for other PRMS modules
!***********************************************************************
      MODULE PRMS_WATER_USE
      ! Local Variables
      CHARACTER(LEN=14), SAVE :: MODNAME
      ! Declared Variables
      DOUBLE PRECISION, SAVE :: Total_external_transfer, Total_external_gain
      REAL, ALLOCATABLE, SAVE :: External_transfer(:), External_gain(:), External_transfer_tot(:), External_gain_tot(:)
      DOUBLE PRECISION, SAVE ::  Total_consumed_gain
      REAL, ALLOCATABLE, SAVE :: Consumed_gain(:), Consumed_gain_tot(:)
      DOUBLE PRECISION, SAVE ::  Total_soilzone_gain
      REAL, ALLOCATABLE, SAVE :: Soilzone_gain(:), Soilzone_gain_tot(:)
      DOUBLE PRECISION, SAVE :: Total_dprst_transfer, Total_dprst_gain
      REAL, ALLOCATABLE, SAVE :: Dprst_transfer(:), Dprst_gain(:), Dprst_transfer_tot(:), Dprst_gain_tot(:)
      DOUBLE PRECISION, SAVE :: Total_gwr_transfer, Total_gwr_gain
      REAL, ALLOCATABLE, SAVE :: Gwr_transfer(:), Gwr_gain(:), Gwr_transfer_tot(:), Gwr_gain_tot(:)
      DOUBLE PRECISION, SAVE :: Total_segment_transfer, Total_segment_gain
      REAL, ALLOCATABLE, SAVE :: Segment_transfer(:), Segment_gain(:), Segment_transfer_tot(:), Segment_gain_tot(:)
      DOUBLE PRECISION, SAVE :: Total_lake_transfer, Total_lake_gain
      REAL, ALLOCATABLE, SAVE :: Lake_transfer(:), Lake_gain(:), Lake_transfer_tot(:), Lake_gain_tot(:)
      DOUBLE PRECISION, SAVE ::  Total_canopy_gain
      REAL, SAVE, ALLOCATABLE :: Canopy_gain(:), Canopy_gain_tot(:)
      DOUBLE PRECISION, SAVE ::  Total_transfers
      REAL, ALLOCATABLE, SAVE :: Transfer_rate(:)
      ! Local Variables
      INTEGER, SAVE :: Outunit, Ndiversions
      INTEGER, SAVE :: Consumed_transfers_on, Lake_transfers_on, Segment_transfers_on
      INTEGER, SAVE :: External_transfers_on, Dprst_transfers_on, Gwr_transfers_on
      INTEGER, ALLOCATABLE, SAVE :: Source_id(:), Destination_id(:), Source_type(:), Destination_type(:)
      END MODULE PRMS_WATER_USE

      INTEGER FUNCTION water_use_read()
      USE PRMS_WATER_USE
      USE PRMS_MODULE, ONLY: Process, Nhru, Nsegment, Segment_transferON_OFF, Gwr_transferON_OFF, Lake_transferON_OFF, &
     &    External_transferON_OFF, Dprst_transferON_OFF, Dprst_flag, Nwateruse, Strmflow_flag, &
     &    Model, Starttime, Endtime, Nexternal, Nconsumed, Inputerror_flag, MAXFILE_LENGTH
      USE PRMS_BASIN, ONLY: Hru_perv !, Hru_area_dble
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowday, Nowmonth, Cfs_conv
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Soil_rechr, Soil_rechr_max, Dprst_vol_open !, Gwres_stor
      IMPLICIT NONE
! Functions
      INTRINSIC SNGL, DBLE
      INTEGER, EXTERNAL :: control_string, declvar, decldim, getdim
      EXTERNAL read_error, find_header_end, find_current_file_time, read_event, print_module, PRMS_open_module_file
! Control Parameters
      CHARACTER(LEN=MAXFILE_LENGTH) :: Segment_transfer_file, Gwr_transfer_file, Dprst_transfer_file
      CHARACTER(LEN=MAXFILE_LENGTH) :: External_transfer_file, Lake_transfer_file
! Local Variables
      INTEGER, SAVE :: external_unit, external_next_year, external_next_month, external_next_day
      INTEGER, SAVE :: segment_unit, dprst_unit, gwr_unit, lake_unit
      INTEGER :: year, month, day, ierr, istop, i, id_src, id_dest
      INTEGER, SAVE :: dprst_next_year, dprst_next_month, dprst_next_day
      INTEGER, SAVE :: gwr_next_year, gwr_next_month, gwr_next_day
      INTEGER, SAVE :: segment_next_year, segment_next_month, segment_next_day
      INTEGER, SAVE :: lake_next_year, lake_next_month, lake_next_day
      DOUBLE PRECISION :: cfs_value, diversion_inches, transfer_rate_dble !, factor
      CHARACTER(LEN=80), SAVE :: Version_water_use_read
!***********************************************************************
      ! Types
      ! (1) stream segments; (2) groundwater reservoirs; (3) surface-depression storage;
      ! (4) external locations; (5) lakes; (6) capillary reservoir of the soil zone;
      ! (7) internal consumptive-use locations; and (8) plant canopy. 
!***********************************************************************
      water_use_read = 0

      IF ( Process(:3)=='run' ) THEN
        IF ( External_transferON_OFF==1 ) THEN
          CALL read_event(external_unit, 4, external_next_year, external_next_month, external_next_day)
          Total_external_transfer = 0.0D0
          External_transfer = 0.0
        ENDIF
        IF ( External_transfers_on==1 ) THEN
          Total_external_gain = 0.0D0
          External_gain = 0.0
        ENDIF

        IF ( Gwr_transferON_OFF==1 ) THEN
          CALL read_event(gwr_unit, 2, gwr_next_year, gwr_next_month, gwr_next_day)
          Total_gwr_transfer = 0.0D0
          Gwr_transfer = 0.0
        ENDIF
        IF ( Gwr_transfers_on==1 ) THEN
          Total_gwr_gain = 0.0D0
          Gwr_gain = 0.0
        ENDIF

        IF ( Dprst_transferON_OFF==1 ) THEN
          CALL read_event(dprst_unit, 3, dprst_next_year, dprst_next_month, dprst_next_day)
          Total_dprst_transfer = 0.0D0
          Dprst_transfer = 0.0
        ENDIF
        IF ( Dprst_transfers_on==1 ) THEN
          Total_dprst_gain = 0.0D0
          Dprst_gain = 0.0
        ENDIF

        IF ( Segment_transferON_OFF==1 ) THEN
          CALL read_event(segment_unit, 1, segment_next_year, segment_next_month, segment_next_day)
          Total_segment_transfer = 0.0D0
          Segment_transfer = 0.0
        ENDIF
        IF ( Segment_transfers_on==1 ) THEN
          Total_segment_gain = 0.0D0
          Segment_gain = 0.0
        ENDIF
          
        IF ( Lake_transferON_OFF==1 ) THEN
          CALL read_event(lake_unit, 5, lake_next_year, lake_next_month, lake_next_day)
          Total_lake_transfer = 0.0D0
          Lake_transfer = 0.0
        ENDIF
        IF ( Lake_transfers_on==1 ) THEN
          Total_lake_gain = 0.0D0
          Lake_gain = 0.0
        ENDIF

        IF ( Consumed_transfers_on==1 ) THEN
          Total_consumed_gain = 0.0D0
          Consumed_gain = 0.0
        ENDIF

        Total_soilzone_gain = 0.0D0
        Soilzone_gain = 0.0

        Total_canopy_gain = 0.0D0
        Canopy_gain = 0.0

        Total_transfers = 0.0D0
        DO i = 1, Ndiversions
          id_src = Source_id(i)
          id_dest = Destination_id(i)
          transfer_rate_dble = DBLE( Transfer_rate(i) )
          Total_transfers = Total_transfers + transfer_rate_dble

          IF ( Gwr_transfers_on==1 ) THEN
            IF ( Source_type(i)==2 ) THEN
              Gwr_transfer(id_src) = Gwr_transfer(id_src) + Transfer_rate(i)
              Gwr_transfer_tot(id_src) = Gwr_transfer_tot(id_src) + Transfer_rate(i)
              Total_gwr_transfer = Total_gwr_transfer + transfer_rate_dble
              !!!!!! remove transfer in gwflow so that recharge and gwstor_min are applied 3/20/2017
!              factor = Cfs_conv*Hru_area_dble(id_src)
!              cfs_value = Gwres_stor(id_src)*factor
!              IF ( cfs_value<transfer_rate_dble ) THEN
!                PRINT *, 'ERROR, not enough storage for transfer in GWR:', id_src, ' Date:', Nowyear, Nowmonth, Nowday
!                STOP
!              ENDIF
!              Gwres_stor(id_src) = Gwres_stor(id_src) - transfer_rate_dble/factor
            ENDIF
            IF ( Destination_type(i)==2 ) THEN
              Gwr_gain(id_dest) = Gwr_gain(id_dest) + Transfer_rate(i)
              Gwr_gain_tot(id_dest) = Gwr_gain_tot(id_dest) + Transfer_rate(i)
              Total_gwr_gain = Total_gwr_gain + transfer_rate_dble
              !!!!!! remove transfer in gwflow so that recharge and gwstor_min are applied 3/20/2017
!              Gwres_stor(id_dest) = Gwres_stor(id_dest) + transfer_rate_dble/Cfs_conv/Hru_area_dble(id_dest)
            ENDIF
          ENDIF

          IF ( Dprst_transfers_on==1 ) THEN
            ! WARNING, dprst transfers only apply to open depressions
            IF ( Source_type(i)==3 ) THEN
              Dprst_transfer(id_src) = Dprst_transfer(id_src) + Transfer_rate(i)
              Dprst_transfer_tot(id_src) = Dprst_transfer_tot(id_src) + Transfer_rate(i)
              Total_dprst_transfer = Total_dprst_transfer + transfer_rate_dble
              cfs_value = Dprst_vol_open(id_src)*Cfs_conv
              IF ( cfs_value<transfer_rate_dble ) THEN
                PRINT *, 'ERROR, not enough storage for transfer in surface-depression storage:', &
     &                   id_src, ' Date:', Nowyear, Nowmonth, Nowday
                STOP
              ENDIF
              Dprst_vol_open(id_src) = Dprst_vol_open(id_src) - transfer_rate_dble/Cfs_conv
            ENDIF
            IF ( Destination_type(i)==3 ) THEN
              Dprst_gain(id_dest) = Dprst_gain(id_dest) + Transfer_rate(i)
              Dprst_gain_tot(id_dest) = Dprst_gain_tot(id_dest) + Transfer_rate(i)
              Total_dprst_gain = Total_dprst_gain + transfer_rate_dble
              Dprst_vol_open(id_dest) = Dprst_vol_open(id_dest) + transfer_rate_dble/Cfs_conv
            ENDIF
          ENDIF

          IF ( Segment_transfers_on==1 ) THEN
            IF ( Source_type(i)==1 ) THEN
              Segment_transfer(id_src) = Segment_transfer(id_src) + Transfer_rate(i)
              Segment_transfer_tot(id_src) = Segment_transfer_tot(id_src) + Transfer_rate(i)
              Total_segment_transfer = Total_segment_transfer + transfer_rate_dble
            ENDIF
            IF ( Destination_type(i)==1 ) THEN
              Segment_gain(id_dest) = Segment_gain(id_dest) + Transfer_rate(i)
              Segment_gain_tot(id_dest) = Segment_gain_tot(id_dest) + Transfer_rate(i)
              Total_segment_gain = Total_segment_gain + transfer_rate_dble
            ENDIF
          ENDIF

          IF ( Lake_transfers_on==1 ) THEN
            IF ( Source_type(i)==5 ) THEN
              Lake_transfer(id_src) = Lake_transfer(id_src) + Transfer_rate(i)
              Lake_transfer_tot(id_src) = Lake_transfer_tot(id_src) + Transfer_rate(i)
              Total_lake_transfer = Total_lake_transfer + transfer_rate_dble
!             cfs_value = Lake_vol(id_src)*Cfs_conv ! rsr, need to fix
!             IF ( cfs_value<transfer_rate_dble ) THEN
!               PRINT *, 'ERROR, not enough storage for transfer in lake:', &
!    &                   id_src, ' Date:', Nowyear, Nowmonth, Nowday
!               STOP
!             ENDIF
              !Lake_vol(id_src) = Lake_vol(id_src) - transfer_rate_dble
            ENDIF
            IF ( Destination_type(i)==1 ) THEN
              Lake_gain(id_dest) = Lake_gain(id_dest) + Transfer_rate(i)
              Lake_gain_tot(id_dest) = Lake_gain_tot(id_dest) + Transfer_rate(i)
              Total_lake_gain = Total_lake_gain + transfer_rate_dble
              !lake_stor(id_dest) = lake_stor(id_dest) + transfer_rate_dble
            ENDIF
          ENDIF

          IF ( External_transfers_on==1 ) THEN
            IF ( Source_type(i)==4 ) THEN
              External_transfer(id_src) = External_transfer(id_src) + Transfer_rate(i)
              External_transfer_tot(id_src) = External_transfer_tot(id_src) + Transfer_rate(i)
              Total_external_transfer = Total_external_transfer + transfer_rate_dble
            ENDIF
            IF ( Destination_type(i)==4 ) THEN
              External_gain(id_dest) = External_gain(id_dest) + Transfer_rate(i)
              External_gain_tot(id_dest) = External_gain_tot(id_dest) + Transfer_rate(i)
              Total_external_gain = Total_external_gain + transfer_rate_dble
            ENDIF
          ENDIF

          IF ( Consumed_transfers_on==1 ) THEN
            IF ( Destination_type(i)==7 ) THEN
              Consumed_gain(id_dest) = Consumed_gain(id_dest) + Transfer_rate(i)
              Consumed_gain_tot(id_dest) = Consumed_gain_tot(id_dest) + Transfer_rate(i)
              Total_consumed_gain = Total_consumed_gain + transfer_rate_dble
            ENDIF
          ENDIF

          IF ( Destination_type(i)==6 ) THEN
            Soilzone_gain(id_dest) = Soilzone_gain(id_dest) + Transfer_rate(i)
            Soilzone_gain_tot(id_dest) = Soilzone_gain_tot(id_dest) + Transfer_rate(i)
            Total_soilzone_gain = Total_soilzone_gain + transfer_rate_dble
            diversion_inches = transfer_rate_dble/Cfs_conv/DBLE(Hru_perv(id_dest))
            Soil_moist(id_dest) = Soil_moist(id_dest) + SNGL( diversion_inches )
            Soil_rechr(id_dest) = Soil_rechr(id_dest) + SNGL( diversion_inches )
            IF ( Soil_rechr(id_dest)>Soil_rechr_max(id_dest) ) Soil_rechr(id_dest) = Soil_rechr_max(id_dest)
          ENDIF

          IF ( Destination_type(i)==8 ) THEN
            Canopy_gain(id_dest) = Canopy_gain(id_dest) + Transfer_rate(i)
            Canopy_gain_tot(id_dest) = Canopy_gain_tot(id_dest) + Transfer_rate(i)
            Total_canopy_gain = Total_canopy_gain + transfer_rate_dble
          ENDIF
        ENDDO

      ELSEIF ( Process(:4)=='decl' ) THEN
        Version_water_use_read = 'water_use_read.f90 2017-03-21 10:26:00Z'
        CALL print_module(Version_water_use_read, 'Time Series Data            ', 90)
        MODNAME = 'water_use_read'

        Dprst_transfers_on = 0
        IF ( Dprst_flag==1 .OR. Model==99 ) THEN
          IF ( Dprst_transferON_OFF==1 .OR. Model==99 ) THEN
            Dprst_transfers_on = 1
            ALLOCATE ( Dprst_transfer(Nhru) )
            IF ( declvar(MODNAME, 'dprst_transfer', 'nhru', Nhru, 'real', &
     &           'Transfer flow rate from surface-depression storage for each HRU for each time step', &
     &           'cfs', Dprst_transfer)/=0 ) CALL read_error(1, 'dprst_transfer')
            ALLOCATE ( Dprst_transfer_tot(Nhru) )
            IF ( declvar(MODNAME, 'dprst_transfer_tot', 'nhru', Nhru, 'real', &
     &           'Transfer flow rate from surface-depression storage for each HRU for the simulation', &
     &           'cfs', Dprst_transfer_tot)/=0 ) CALL read_error(1, 'dprst_transfer_tot')
            IF ( declvar(MODNAME, 'total_dprst_transfer', 'one', 1, 'double', &
     &           'Transfer flow rates from all surface-depression storage for each time step', &
     &           'cfs', Total_dprst_transfer)/=0 ) CALL read_error(1, 'total_dprst_transfer')
          ENDIF
          ALLOCATE ( Dprst_gain(Nhru) )
          IF ( declvar(MODNAME, 'dprst_gain', 'nhru', Nhru, 'real', &
     &         'Transfer gain to surface-depression storage for each HRU for each time step', &
     &         'cfs', Dprst_gain)/=0 ) CALL read_error(1, 'dprst_gain')
          ALLOCATE ( Dprst_gain_tot(Nhru) )
          IF ( declvar(MODNAME, 'dprst_gain_tot', 'nhru', Nhru, 'real', &
     &         'Transfer gain to surface-depression storage for each HRU for the simulation', &
     &         'cfs', Dprst_gain_tot)/=0 ) CALL read_error(1, 'dprst_gain_tot')
          IF ( declvar(MODNAME, 'total_dprst_gain', 'one', 1, 'double', &
     &         'Transfer gains to all surface-depression storage for each time step', &
     &         'cfs', Total_dprst_gain)/=0 ) CALL read_error(1, 'total_dprst_gain')
        ELSEIF ( Dprst_transferON_OFF==1 .AND. Model/=99 ) THEN
          PRINT *, 'ERROR, specified to transfer water from surface-depression storage when dprst_flag = 0'
          Inputerror_flag = 1
        ENDIF

        Segment_transfers_on = 0
        IF ( Strmflow_flag>1 .OR. Model==99 ) THEN
          IF ( Segment_transferON_OFF==1 .OR. Model==99 ) THEN
            Segment_transfers_on = 1
            ALLOCATE ( Segment_transfer(Nsegment) )
            IF ( declvar(MODNAME, 'segment_transfer', 'nsegment', Nsegment, 'real', &
     &           'Transfer flow rate from each stream segment for each time step', &
     &           'cfs', Segment_transfer)/=0 ) CALL read_error(1, 'segment_transfer')
            ALLOCATE ( Segment_transfer_tot(Nsegment) )
            IF ( declvar(MODNAME, 'segment_transfer_tot', 'nsegment', Nsegment, 'real', &
     &           'Transfer flow rate from each stream segment for the simulation', &
     &           'cfs', Segment_transfer_tot)/=0 ) CALL read_error(1, 'segment_transfer_tot')
            IF ( declvar(MODNAME, 'total_segment_transfer', 'one', 1, 'double', &
     &           'Transfer flow rates from all stream segments for each time step', &
     &           'cfs', Total_segment_transfer)/=0 ) CALL read_error(1, 'total_segment_transfer')
          ENDIF
          ALLOCATE ( Segment_gain(Nsegment) )
          IF ( declvar(MODNAME, 'segment_gain', 'nsegment', Nsegment, 'real', &
     &         'Transfer gain for each stream segment for each time step', &
     &         'cfs', Segment_gain)/=0 ) CALL read_error(1, 'segment_gain')
          ALLOCATE ( Segment_gain_tot(Nsegment) )
          IF ( declvar(MODNAME, 'segment_gain_tot', 'nsegment', Nsegment, 'real', &
     &         'Transfer gain for each stream segment for the simulation', &
     &         'cfs', Segment_gain_tot)/=0 ) CALL read_error(1, 'segment_gain_tot')
          IF ( declvar(MODNAME, 'total_segment_gain', 'one', 1, 'double', &
     &         'Transfer gains to all stream segments for each time step', &
     &         'cfs', Total_segment_gain)/=0 ) CALL read_error(1, 'total_segment_gain')
        ELSEIF ( Segment_transferON_OFF==1 .AND. Model/=99 ) THEN
          PRINT *, 'ERROR, specified to transfer water from stream segments when they are not present'
          Inputerror_flag = 1
        ENDIF

        Gwr_transfers_on = 0
        IF ( Gwr_transferON_OFF==1 .OR. Model==99 ) THEN
          Gwr_transfers_on = 1
          ALLOCATE ( Gwr_transfer(Nhru) )
          IF ( declvar(MODNAME, 'gwr_transfer', 'nhru', Nhru, 'real', &
     &         'Transfer flow rate from the groundwater reservoir of each HRU for each time step', &
     &         'cfs', Gwr_transfer)/=0 ) CALL read_error(1, 'gwr_transfer')
          ALLOCATE ( Gwr_transfer_tot(Nhru) )
          IF ( declvar(MODNAME, 'gwr_transfer_tot', 'nhru', Nhru, 'real', &
     &         'Transfer flow rate from the groundwater reservoir of each HRU for the simulation', &
     &         'cfs', Gwr_transfer_tot)/=0 ) CALL read_error(1, 'gwr_transfer_tot')
          IF ( declvar(MODNAME, 'total_gwr_transfer', 'one', 1, 'double', &
     &         'Transfer flow rates from all groundwater reservoirs for each time step', &
     &         'cfs', Total_gwr_transfer)/=0 ) CALL read_error(1, 'total_gwr_transfer')
        ENDIF
        ALLOCATE ( Gwr_gain(Nhru) )
        IF ( declvar(MODNAME, 'gwr_gain', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the groundwater reservoir of each HRU for each time step', &
     &       'cfs', Gwr_gain)/=0 ) CALL read_error(1, 'gwr_gain')
        ALLOCATE ( Gwr_gain_tot(Nhru) )
        IF ( declvar(MODNAME, 'gwr_gain_tot', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the groundwater reservoir of each HRU for the simulation', &
     &       'cfs', Gwr_gain_tot)/=0 ) CALL read_error(1, 'gwr_gain_tot')
        IF ( declvar(MODNAME, 'total_gwr_gain', 'one', 1, 'double', &
     &       'Flow to all groundwater reservoirs for each time step', &
     &       'cfs', Total_gwr_gain)/=0 ) CALL read_error(1, 'total_gwr_gain')

        Lake_transfers_on = 0
        IF ( Strmflow_flag==3 .OR. Model==99 ) THEN
          IF ( Lake_transferON_OFF==1 .OR. Model==99 ) THEN
            Lake_transfers_on = 1
            ALLOCATE ( Lake_transfer(Nhru) )
            IF ( declvar(MODNAME, 'lake_transfer', 'nhru', Nhru, 'real', &
     &           'Transfer flow rate from each lake HRU for each time step', &
     &           'cfs', Lake_transfer)/=0 ) CALL read_error(1, 'lake_transfer')
            ALLOCATE ( Lake_transfer_tot(Nhru) )
            IF ( declvar(MODNAME, 'lake_transfer_tot', 'nhru', Nhru, 'real', &
     &           'Transfer flow rate from each lake HRU for the simulation', &
     &           'cfs', Lake_transfer_tot)/=0 ) CALL read_error(1, 'lake_transfer_tot')
            IF ( declvar(MODNAME, 'total_lake_transfer', 'one', 1, 'double', &
     &           'Transfer flow rates from all lake HRUs for each time step', &
     &           'cfs', Total_lake_transfer)/=0 ) CALL read_error(1, 'total_lake_transfer')
          ENDIF
          ALLOCATE ( Lake_gain(Nhru) )
          IF ( declvar(MODNAME, 'lake_gain', 'nhru', Nhru, 'real', &
     &         'Transfer gain to each lake HRU for each time step', &
     &         'cfs', Lake_gain)/=0 ) CALL read_error(1, 'lake_gain')
          ALLOCATE ( Lake_gain_tot(Nhru) )
          IF ( declvar(MODNAME, 'lake_gain_tot', 'nhru', Nhru, 'real', &
     &         'Transfer gain to each lake HRU for the simulation', &
     &         'cfs', Lake_gain_tot)/=0 ) CALL read_error(1, 'lake_gain_tot')
          IF ( declvar(MODNAME, 'total_lake_gain', 'one', 1, 'double', &
     &         'Transfer gains to all lake HRUs for each time step', &
     &         'cfs', Total_lake_gain)/=0 ) CALL read_error(1, 'total_lake_gain')
        ELSEIF ( Lake_transferON_OFF==1 .AND. Model/=99 ) THEN
          PRINT *, 'ERROR, specified to transfer water from lakes when lake module is not active'
          Inputerror_flag = 1
        ENDIF

        External_transfers_on = 0
        IF ( (External_transferON_OFF==1.AND.Nexternal>0) .OR. Model==99 ) THEN
          External_transfers_on = 1
          ALLOCATE ( External_transfer(Nexternal) )
          IF ( declvar(MODNAME, 'external_transfer', 'nexternal', Nexternal, 'real', &
     &         'Transfer flow rate from each external source for each time step', &
     &         'cfs', External_transfer)/=0 ) CALL read_error(1, 'external_transfer')
          ALLOCATE ( External_transfer_tot(Nexternal) )
          IF ( declvar(MODNAME, 'external_transfer_tot', 'nexternal', Nexternal, 'real', &
     &         'Transfer flow rate from each external source for the simulation', &
     &         'cfs', External_transfer_tot)/=0 ) CALL read_error(1, 'external_transfer_tot')
          IF ( declvar(MODNAME, 'total_external_transfer', 'one', 1, 'double', &
     &         'Transfer flow rates from all external sources for each time step', &
     &         'cfs', Total_external_transfer)/=0 ) CALL read_error(1, 'total_external_transfer')
        ENDIF
        IF ( Nexternal>0 ) THEN
          ALLOCATE ( External_gain(Nexternal) )
          IF ( declvar(MODNAME, 'external_gain', 'nexternal', Nexternal, 'real', &
     &         'Transfer gain to each external location for each time step', &
     &         'cfs', External_gain)/=0 ) CALL read_error(1, 'external_gain')
          ALLOCATE ( External_gain_tot(Nexternal) )
          IF ( declvar(MODNAME, 'external_gain_tot', 'nexternal', Nexternal, 'real', &
     &         'Transfer gain to each external location for each time step', &
     &         'cfs', External_gain_tot)/=0 ) CALL read_error(1, 'external_gain_tot')
        ENDIF
        IF ( declvar(MODNAME, 'total_external_gain', 'one', 1, 'double', &
     &       'Transfer gains to all external locations for each time step', &
     &       'cfs', Total_external_gain)/=0 ) CALL read_error(1, 'total_external_gain')

        Consumed_transfers_on = 0
        IF ( Nconsumed>0 ) THEN
          Consumed_transfers_on = 1
          ALLOCATE ( Consumed_gain(Nconsumed) )
          IF ( declvar(MODNAME, 'consumed_gain', 'nconsumed', Nconsumed, 'real', &
     &         'Transfer flow rate to each water-use comsumption destination for each time step', &
     &         'cfs', Consumed_gain)/=0 ) CALL read_error(1, 'consumed_gain')
          ALLOCATE ( Consumed_gain_tot(Nconsumed) )
          IF ( declvar(MODNAME, 'consumed_gain_tot', 'nconsumed', Nconsumed, 'real', &
     &         'Transfer flow rate to each water-use comsumption destination for the simulation', &
     &         'cfs', Consumed_gain_tot)/=0 ) CALL read_error(1, 'consumed_gain_tot')
        ENDIF
        IF ( declvar(MODNAME, 'total_consumed_gain', 'one', 1, 'double', &
     &       'Transfer flow rates to all water-use comsumption destinations for each time step', &
     &       'cfs', Total_consumed_gain)/=0 ) CALL read_error(1, 'total_consumed_gain')

        ALLOCATE ( Soilzone_gain(Nhru) )
        IF ( declvar(MODNAME, 'soilzone_gain', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the capillary reservoir within the soilzone for each HRU for each time step', &
     &       'cfs', Soilzone_gain)/=0 ) CALL read_error(1, 'soilzone_gain')
        ALLOCATE ( Soilzone_gain_tot(Nhru) )
        IF ( declvar(MODNAME, 'soilzone_gain_tot', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the capillary reservoir within the soilzone for each HRU for the simulation', &
     &       'cfs', Soilzone_gain_tot)/=0 ) CALL read_error(1, 'soilzone_gain_tot')
        IF ( declvar(MODNAME, 'total_soilzone_gain', 'one', 1, 'double', &
     &       'Transfer gains to all capillary reservoirs for each time step', &
     &       'cfs', Total_soilzone_gain)/=0 ) CALL read_error(1, 'total_soilzone_gain')

        ALLOCATE ( Canopy_gain(Nhru) )
        IF ( declvar(MODNAME, 'canopy_gain', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the canopy reservoir for each HRU for each time step', &
     &       'cfs', Canopy_gain)/=0 ) CALL read_error(1, 'canopy_gain')
        ALLOCATE ( Canopy_gain_tot(Nhru) )
        IF ( declvar(MODNAME, 'canopy_gain_tot', 'nhru', Nhru, 'real', &
     &       'Transfer gain to the canopy reservoir for each HRU for the simulation', &
     &       'cfs', Canopy_gain_tot)/=0 ) CALL read_error(1, 'canopy_gain_tot')
        IF ( declvar(MODNAME, 'total_canopy_gain', 'one', 1, 'double', &
     &       'Transfer gains to all canopy reservoirs for each time step', &
     &       'cfs', Total_canopy_gain)/=0 ) CALL read_error(1, 'total_canopy_gain')

        ALLOCATE ( Transfer_rate(Nwateruse), Source_id(Nwateruse), Source_type(Nwateruse) )
        ALLOCATE ( Destination_id(Nwateruse), Destination_type(Nwateruse) )
        IF ( declvar(MODNAME, 'total_transfers', 'one', 1, 'double', &
     &           'Transfer of all water-use transfers for each time step', &
     &           'cfs', Total_transfers)/=0 ) CALL read_error(1, 'total_transfers')
        IF ( declvar(MODNAME, 'transfer_rate', 'nwateruse', nwateruse, 'double', &
     &           'Transfer of each water-use transfer for each time step', &
     &           'cfs', Transfer_rate)/=0 ) CALL read_error(1, 'transfer_rate')
        
      ELSEIF ( Process(:4)=='init' ) THEN
        Ndiversions = 0
        year = Starttime(1)
        month = Starttime(2)
        day = Starttime(3)

        CALL PRMS_open_module_file(Outunit, 'water_use.out')
        WRITE ( Outunit, 10 ) 'Simulation Start Date:', year, month, day, '   End Date:', Endtime(1), Endtime(2), Endtime(3)
10      FORMAT ( 'Water Use Summary File', /, 2(A, I5, 2('/',I2.2)), / ) 

        istop = 0
        IF ( Segment_transferON_OFF==1 ) THEN ! type 1
          IF ( control_string(Segment_transfer_file, 'segment_transfer_file')/=0 ) &
     &         CALL read_error(5, 'segment_transfer_file')
          CALL find_header_end(segment_unit, Segment_transfer_file, 'segment_transfer_file', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(segment_unit, year, month, day, segment_next_year, segment_next_month, segment_next_day)
            Total_segment_transfer = 0.0D0
            Segment_transfer = 0.0
            Segment_transfer_tot = 0.0
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Strmflow_flag>1 ) THEN
          Total_segment_gain = 0.0D0
          Segment_gain = 0.0
          Segment_gain_tot = 0.0
        ENDIF

        IF ( Gwr_transferON_OFF==1 ) THEN ! type 2
          IF ( control_string(Gwr_transfer_file, 'gwr_transfer_file')/=0 ) &
     &         CALL read_error(5, 'gwr_transfer_file')
          CALL find_header_end(gwr_unit, Gwr_transfer_file, 'gwr_transfer_file', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(gwr_unit, year, month, day, &
     &                                  gwr_next_year, gwr_next_month, gwr_next_day)
            Total_gwr_transfer = 0.0D0
            Gwr_transfer = 0.0
            Gwr_transfer_tot = 0.0
          ELSE
            istop = 1
          ENDIF
        ENDIF
        Gwr_gain = 0.0
        Gwr_gain_tot = 0.0
        Total_gwr_gain = 0.0D0

        IF ( Dprst_transferON_OFF==1 ) THEN ! type 3
          IF ( control_string(Dprst_transfer_file, 'dprst_transfer_file')/=0 ) CALL read_error(5, 'dprst_transfer_file')
          CALL find_header_end(dprst_unit, Dprst_transfer_file, 'dprst_transfer_file', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(dprst_unit, year, month, day, dprst_next_year, dprst_next_month, dprst_next_day)
            Total_dprst_transfer = 0.0D0
            Dprst_transfer = 0.0
            Dprst_transfer_tot = 0.0
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Dprst_flag==1 ) THEN
          Dprst_gain = 0.0
          Dprst_gain_tot = 0.0
          Total_dprst_gain = 0.0D0
        ENDIF

        IF ( External_transferON_OFF==1 ) THEN ! type 4
          IF ( control_string(External_transfer_file, 'external_transfer_file')/=0 ) &
     &         CALL read_error(5, 'external_transfer_file')
          CALL find_header_end(external_unit, External_transfer_file, 'external_transfer_file', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(external_unit, year, month, day, &
     &                                  external_next_year, external_next_month, external_next_day)
            Total_external_transfer = 0.0D0
            External_transfer = 0.0
            External_transfer_tot = 0.0
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Nexternal>0 ) THEN
          External_gain = 0.0
          External_gain_tot = 0.0
          Total_external_gain = 0.0D0
        ENDIF

        IF ( Lake_transferON_OFF==1 ) THEN ! Type 5
          IF ( control_string(Lake_transfer_file, 'lake_transfer_file')/=0 ) CALL read_error(5, 'lake_transfer_file')
          CALL find_header_end(lake_unit, Lake_transfer_file, 'lake_transfer_file', ierr, 0, 0)
          IF ( ierr==0 ) THEN
            CALL find_current_file_time(lake_unit, year, month, day, lake_next_year, lake_next_month, lake_next_day)
            Total_lake_transfer = 0.0D0
            Lake_transfer = 0.0
            Lake_transfer_tot = 0.0
          ELSE
            istop = 1
          ENDIF
        ENDIF
        IF ( Strmflow_flag==3 ) THEN
          Total_lake_gain = 0.0D0
          Lake_gain = 0.0
          Lake_gain_tot = 0.0
        ENDIF

        IF ( istop==1 ) STOP 'ERROR in water_use_read module'

        ! type 6
        Soilzone_gain = 0.0
        Soilzone_gain_tot = 0.0
        Total_soilzone_gain = 0.0D0

        IF ( Consumed_transfers_on==1 ) THEN ! type 7
          Consumed_gain = 0.0
          Consumed_gain_tot = 0.0
          Total_consumed_gain = 0.0D0
        ENDIF

        ! type 8
        Canopy_gain = 0.0
        Canopy_gain_tot = 0.0
        Total_canopy_gain = 0.0D0

        Total_transfers = 0.0D0
        Source_id = 0
        Source_type = 0
        Destination_id = 0
        Destination_type = 0
        Transfer_rate = 0.0
      ENDIF

      END FUNCTION water_use_read

!*****************************
! Read event for a source type
!*****************************
      SUBROUTINE read_event(Iunit, Src_type, Next_yr, Next_mo, Next_day)
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunit, Src_type
      INTEGER, INTENT (INOUT) :: Next_yr, Next_mo, Next_day
! Funcions
      EXTERNAL check_event, set_transfers, is_eof
! Local Variables
      INTEGER src_id, dest_type, dest_id, keep_reading, ignore
      REAL transfer
!*******************************************************************************
      IF ( Next_mo==0 ) RETURN ! already found end of file
      keep_reading = 1
      DO WHILE ( keep_reading==1 )
        IF ( Next_yr==Nowyear .AND. Next_mo==Nowmonth .AND. Next_day==Nowday ) THEN
          READ ( Iunit, * ) Next_yr, Next_mo, Next_day, src_id, dest_type, dest_id, transfer
          IF ( dest_type>8 ) THEN
            PRINT *, 'ERROR, destination flag>8 for date:', Next_yr, Next_mo, Next_day, ' destination:', dest_type
            STOP
          ENDIF
          CALL check_event(Src_type, dest_type, src_id, dest_id, ignore)
          IF ( ignore==0 ) CALL set_transfers(Src_type, src_id, dest_type, dest_id, transfer)
          CALL is_eof(Iunit, Next_yr, Next_mo, Next_day)
          IF ( Next_mo==0 ) keep_reading = 0
        ELSE
          keep_reading = 0
        ENDIF
      ENDDO
      END SUBROUTINE read_event

! ****************************
! check event for validity
! ****************************
      SUBROUTINE check_event(Src_type, Dest_type, Src_id, Dest_id, Ignore)
      USE PRMS_WATER_USE, ONLY: Outunit, Segment_transfers_on, Dprst_transfers_on, Lake_transfers_on, &
     &    Consumed_transfers_on, External_transfers_on, Gwr_transfers_on
      USE PRMS_MODULE, ONLY: Segment_transferON_OFF, Gwr_transferON_OFF, Lake_transferON_OFF, &
     &    Dprst_transferON_OFF, External_transferON_OFF, Strmflow_flag, Nexternal, Dprst_flag
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Src_type, Dest_type, Src_id, Dest_id
      INTEGER, INTENT(OUT) :: Ignore
!*******************************************************************************
      WRITE ( Outunit, '(/,A,I5,2("/",I2.2))' ) 'Event date:', Nowyear, Nowmonth, Nowday

      Ignore = 0
       IF ( Src_type>5 ) THEN
        PRINT *, 'ERROR, invalid src_type:', Src_type
        STOP 'Valid src_type values are 1 to 5'
      ENDIF
      IF ( Src_type==1 ) THEN
        IF ( Segment_transferON_OFF==0 ) THEN
          PRINT *, 'Warning, specified a stream segment transfer, but segment_transferON_OFF=0, transfer ignored'
          Ignore = 1
        ELSEIF ( Segment_transfers_on==0 ) THEN
          STOP 'ERROR, specified a stream segment transfer, but stream segments not present in model'
        ENDIF
      ENDIF
      IF ( Dest_type==1 ) THEN
        IF ( Strmflow_flag<2 ) THEN
          STOP 'ERROR, specified a transfer to stream segment, but stream segments not present in model'
        ELSE
         Segment_transfers_on = 1
        ENDIF
      ENDIF
      IF ( Src_type==4 ) THEN
        IF ( External_transferON_OFF==0 ) THEN
          PRINT *, 'Warning, specified a external transfer, but external_transferON_OFF=0, transfer ignored'
          Ignore = 1
        ELSEIF ( External_transfers_on==0 ) THEN
          STOP 'ERROR, specified a external transfer, but external locations not present in model'
        ENDIF
      ENDIF
      IF ( Dest_type==4 ) THEN
        IF ( Nexternal==0 ) THEN
          STOP 'ERROR, specified a transfer to external location, but nexternal = 0'
        ELSE
          External_transfers_on = 1
        ENDIF
      ENDIF
      IF ( Dest_type==7 ) THEN
        IF ( Consumed_transfers_on==0 ) &
     &       STOP 'ERROR, specified a consumption-use transfer, but consumption locations not present in model'
      ENDIF
      IF ( Src_type==2 ) THEN
        IF ( Gwr_transferON_OFF==0 ) THEN
          PRINT *, 'Warning, specified a groundwater transfer, but gwr_transferON_OFF=0, transfer ignored'
          Ignore = 1
        ENDIF
      ENDIF
      IF ( Dest_type==2 ) Gwr_transfers_on = 1
      IF ( Src_type==3 ) THEN
        IF ( Dprst_transferON_OFF==0 ) THEN
          PRINT *, 'Warning, specified a external transfer, but dprst_transferON_OFF=0, transfer ignored'
          Ignore = 1
        ELSEIF ( Dprst_transfers_on==0 ) THEN
          STOP 'ERROR, specified a surface-depression transfer, but dprst_flag=0'
        ENDIF
      ENDIF
      IF ( Dest_type==3 ) THEN
        IF ( Dprst_flag==0 ) THEN
          STOP 'ERROR, specified a transfer to depression storage, but dprst_flag = 0'
        ELSE
          Dprst_transfers_on = 1
        ENDIF
      ENDIF
      IF ( Src_type==5 ) THEN
        IF ( Lake_transferON_OFF==0 ) THEN
          PRINT *, 'Warning, specified a lake transfer, but lake_transferON_OFF=0, transfer ignored'
          Ignore = 1
        ELSEIF ( Lake_transfers_on==0 ) THEN
          STOP 'ERROR, specified a lake transfer, but lake module is not active'
        ENDIF
      ENDIF
      IF ( Dest_type==5 ) THEN
        IF ( Strmflow_flag/=3 ) THEN
          STOP 'ERROR, specified a transfer to lake, but lake simulation is inactive'
        ELSE
          Lake_transfers_on = 1
        ENDIF
      ENDIF
      IF ( Src_type==Dest_type .AND. Dest_id==Src_id ) THEN
        PRINT *, 'Warning, specified source and destination as equal, transfer ignored'
        Ignore = 1
      ENDIF
      IF ( Ignore==1 ) PRINT *, 'src_type=', Src_type, '; dest_type=', Dest_type
      END SUBROUTINE check_event

! ****************************
! reset transfers when new event is found
! ****************************
      SUBROUTINE set_transfers(Src_type, Src_id, Dest_type, Dest_id, Diversion)
      USE PRMS_WATER_USE
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Src_type, Src_id, Dest_type, Dest_id
      REAL, INTENT(IN) :: Diversion
! Functions
      EXTERNAL check_transfer
!*******************************************************************************
      IF ( Src_type==4 ) THEN
        CALL check_transfer('external location', Src_type, Src_id, Dest_type, Dest_id, Diversion)
      ELSEIF ( Src_type==2 ) THEN
        CALL check_transfer('groundwater reservior', Src_type, Src_id, Dest_type, Dest_id, Diversion )
      ELSEIF ( Src_type==3 ) THEN
        CALL check_transfer('open surface-depression storage', Src_type, Src_id, Dest_type, Dest_id, Diversion)
      ELSEIF ( Src_type==1 ) THEN
        CALL check_transfer('stream segment', Src_type, Src_id, Dest_type, Dest_id, Diversion)
      ELSEIF ( Src_type==5 ) THEN
        CALL check_transfer('lake storage', Src_type, Src_id, Dest_type, Dest_id, Diversion)
      ELSE
        PRINT *, 'ERROR, invalid src_type:', Src_type
        STOP
      ENDIF
      END SUBROUTINE set_transfers

! ****************************
! check transfer when new event is found
! ****************************
      SUBROUTINE check_transfer(Ctype, Src_type, Src_id, Dest_type, Dest_id, Diversion)
      USE PRMS_WATER_USE
      USE PRMS_MODULE, ONLY: Nwateruse
      IMPLICIT NONE
      ! Functions
      EXTERNAL nwateruse_error
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: Ctype
      INTEGER, INTENT(IN) :: Src_type, Src_id, Dest_type, Dest_id
      REAL, INTENT(IN) :: Diversion
      ! Local Variables
      INTEGER :: i, done
      ! ***********************************
      i = 0 ! index of all transfers
      done = 0
      DO WHILE ( done==0 )
        i = i + 1
        IF ( i>Nwateruse ) CALL nwateruse_error(Ctype)
        ! is this a new diversion
        IF ( Source_type(i)==0 ) THEN ! new
          Source_id(i) = Src_id
          Source_type(i) = Src_type
          Destination_id(i) = Dest_id
          Destination_type(i) = Dest_type
          Transfer_rate(i) = Diversion
          Ndiversions = Ndiversions + 1
          done = 1
        ELSEIF ( Source_type(i)==Src_type .AND. Source_id(i)==Src_id .AND. &
     &           Destination_type(i)==Dest_type .AND. Destination_id(i)==Dest_id ) THEN
          ! replace old value with new value
          Transfer_rate(i) = Diversion
          done = 1
        ENDIF
      ENDDO
      WRITE ( Outunit, '(3A,I6)' ) 'Source: ', Ctype, ':', Src_id
      IF ( Dest_type==1 ) THEN
        WRITE ( Outunit, '(A,I6)' ) 'Destination: stream segment:', Dest_id
      ELSEIF ( Dest_type==2 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Destination: groundwater reservior, HRU:', Dest_id
      ELSEIF ( Dest_type==3 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Destination: open surface-depression storage, HRU:', Dest_id
      ELSEIF ( Dest_type==4 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Destination: external location:', Dest_id
      ELSEIF ( Dest_type==5 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Lake storage, HRU:', Dest_id
      ELSEIF ( Dest_type==6 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Capillary reservoir storage, HRU:', Dest_id
      ELSEIF ( Dest_type==7 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Consumptive-use location:', Dest_id
      ELSEIF ( Dest_type==8 ) THEN
        WRITE ( Outunit, '(2A,I6)' ) 'Canopy storage, HRU:', Dest_id
      ENDIF
      WRITE ( Outunit, '(A,F12.3)' ) 'Transfer flow rate:', Diversion
      END SUBROUTINE check_transfer

      SUBROUTINE nwateruse_error(ctype)
      USE PRMS_MODULE, ONLY: Nwateruse
      USE PRMS_SET_TIME, ONLY: Nowyear, Nowmonth, Nowday
      IMPLICIT NONE
      ! Argument
      CHARACTER(LEN=*), INTENT(IN) :: ctype
      ! ***********************************
      PRINT ('(3A, I5)'), 'ERROR, too many water-use ', ctype, ': specified nwateruse=', Nwateruse
      PRINT *, '       increase nwateruse to number of unique transfers'
      PRINT *, Nowyear, Nowmonth, Nowday
      STOP
      END SUBROUTINE nwateruse_error

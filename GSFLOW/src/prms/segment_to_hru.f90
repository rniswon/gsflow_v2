!***********************************************************************
! Routes stream segment outflow to an HRU capillary reservoir
! used for terminal streams
!***********************************************************************
      MODULE PRMS_SEGMENT_TO_HRU
        IMPLICIT NONE
        ! Local Variables
        character(len=*), parameter :: MODDESC = 'Streamflow'
        character(len=*), parameter :: MODNAME = 'segment_to_hru'
        character(len=*), parameter :: Version_segment_to_hru = '2023-08-10'
        ! Declared Parameters
        INTEGER, SAVE, ALLOCATABLE :: Segment_outflow_id(:)
      END MODULE PRMS_SEGMENT_TO_HRU

      SUBROUTINE segment_to_hru()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT, OFF
      USE PRMS_MODULE, ONLY: Process_flag, Nsegment, AG_flag
      USE PRMS_SEGMENT_TO_HRU
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Basin_area_inv, Hru_perv, Ag_frac
      USE PRMS_FLOWVARS, ONLY: Soil_moist, Basin_soil_moist, Seg_outflow, Soil_rechr, Soil_rechr_max, &
          Ag_soil_moist, Ag_soil_rechr, Ag_soil_rechr_max, Basin_ag_soil_moist, Basin_ag_soil_rechr
      USE PRMS_SOILZONE, ONLY: Basin_soil_rechr
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      use PRMS_READ_PARAM_FILE, only: declparam, getparam_int
      use prms_utils, only: print_module, read_error
      IMPLICIT NONE
! Functions
      INTRINSIC :: SNGL, DBLE
! Local Variables
      INTEGER :: i, ihru, j
      REAL :: flow
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        IF ( AG_flag==OFF ) THEN
          DO i = 1, Nsegment
            ihru = Segment_outflow_id(i)
            IF ( ihru>0 ) THEN
              flow = SNGL(Seg_outflow(i)/Cfs_conv)/Hru_perv(ihru)
              Soil_moist(ihru) = Soil_moist(ihru) + flow
              Soil_rechr(ihru) = Soil_rechr(ihru) + flow
              IF ( Soil_rechr(ihru) > Soil_rechr_max(ihru) ) Soil_rechr(ihru) = Soil_rechr_max(ihru)
            ENDIF
          ENDDO
          Basin_soil_moist = 0.0D0
          Basin_soil_rechr = 0.0D0
          DO j = 1, Active_hrus
            ihru = Hru_route_order(j)
            Basin_soil_moist = Basin_soil_moist + DBLE( Soil_moist(ihru)*Hru_perv(i) )
            Basin_soil_rechr = Basin_soil_rechr + DBLE( Soil_rechr(ihru)*Hru_perv(i) )
          ENDDO
          Basin_soil_moist = Basin_soil_moist*Basin_area_inv
          Basin_soil_rechr = Basin_soil_rechr*Basin_area_inv
        ELSE
          DO i = 1, Nsegment
            ihru = Segment_outflow_id(i)
            IF ( ihru>0 ) THEN
              flow = SNGL(Seg_outflow(i)/Cfs_conv)/Ag_frac(ihru)
              Ag_soil_moist(ihru) = Ag_soil_moist(ihru) + flow
              Ag_soil_rechr(ihru) = Ag_soil_rechr(ihru) + flow
              IF ( Ag_soil_rechr(ihru) > Ag_soil_rechr_max(ihru) ) Ag_soil_rechr(ihru) = Ag_soil_rechr_max(ihru)
            ENDIF
          ENDDO
          Basin_ag_soil_moist = 0.0D0
          Basin_ag_soil_rechr = 0.0D0
          DO j = 1, Active_hrus
            ihru = Hru_route_order(j)
            Basin_ag_soil_moist = Basin_ag_soil_moist + DBLE( Ag_soil_moist(ihru)*Ag_frac(i) )
            Basin_ag_soil_rechr = Basin_ag_soil_rechr + DBLE( Ag_soil_rechr(ihru)*Ag_frac(i) )
          ENDDO
          Basin_ag_soil_moist = Basin_ag_soil_moist*Basin_area_inv
          Basin_ag_soil_rechr = Basin_ag_soil_rechr*Basin_area_inv
        ENDIF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_segment_to_hru)

        ALLOCATE ( Segment_outflow_id(Nsegment) )
        IF ( declparam(MODNAME, 'segment_outflow_id', 'nsegment', 'integer', &
     &       '0', 'bounded', 'nhru', &
     &       'Identification number of HRU that receives outflow from a segment', &
     &       'Identification number of HRU that receives outflow from a segment', &
     &       'none')/=0 ) CALL read_error(1, 'segment_outflow_id')

      ELSEIF ( Process_flag==INIT ) THEN
        IF ( getparam_int(MODNAME, 'segment_outflow_id', Nsegment, &
     &       Segment_outflow_id)/=0 ) CALL read_error(2, 'segment_outflow_id')

      ENDIF

      END SUBROUTINE segment_to_hru

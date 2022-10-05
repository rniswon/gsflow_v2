!***********************************************************************
!     Compute PRMS inflows and outflows for MODSIM lakes and segments
!***********************************************************************
      MODULE GSFPRMS2MODSIM
      IMPLICIT NONE
!   Module Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW PRMS to MODSIM'
      character(len=*), parameter :: MODNAME = 'gsflow_prms2modsim'
      character(len=*), parameter :: Version_gsflow_prms2modsim = '2022-04-13'
      DOUBLE PRECISION, SAVE :: Acre_inches_to_MSl3
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_latflow(:), Lake_In_flow(:), add_irrigation_seg(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_latflow(:), Lake_precip(:), Lake_et(:)
      END MODULE GSFPRMS2MODSIM

!     ******************************************************************
!     Mapping module to convert PRMS states for use by MODSIM
!     ******************************************************************
      SUBROUTINE gsflow_prms2modsim(EXCHANGE, DELTAVOL, LAKEVAP, agDemand, Nsegshold, Nlakeshold)
      USE PRMS_CONSTANTS, ONLY: ACTIVE, RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      USE GSFPRMS2MODSIM
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Nsegshold, Nlakeshold
      DOUBLE PRECISION, INTENT(OUT) :: EXCHANGE(Nsegshold), DELTAVOL(Nlakeshold), LAKEVAP(Nlakeshold), agDemand(Nsegshold)
! Functions
      EXTERNAL :: prms2modsiminit, prms2modsimrun, prms2modsimdecl
!***********************************************************************
      IF ( Process_flag==RUN ) THEN
        CALL prms2modsimrun(EXCHANGE, DELTAVOL, LAKEVAP, agDemand, Nsegshold, Nlakeshold)
      ELSEIF ( Process_flag==DECL ) THEN
        CALL prms2modsimdecl(Nsegshold, Nlakeshold)
      ELSEIF ( Process_flag==INIT ) THEN
        CALL prms2modsiminit(EXCHANGE, agDemand, Nsegshold)
      ENDIF

      END SUBROUTINE gsflow_prms2modsim


!***********************************************************************
!     prms2modsimdecl - Declare variables
!***********************************************************************
      SUBROUTINE prms2modsimdecl(Nsegshold, Nlakeshold)
      USE GSFPRMS2MODSIM
      use PRMS_MMFAPI, only: declvar_dble
      use prms_utils, only: read_error, print_module
      IMPLICIT NONE
      ! Arguments
      INTEGER, INTENT(IN) :: Nsegshold, Nlakeshold
!***********************************************************************
      CALL print_module(MODDESC, MODNAME, Version_gsflow_prms2modsim)

      ALLOCATE ( Segment_latflow(Nsegshold) )
      CALL declvar_dble(MODNAME, 'Segment_latflow', 'nsegment', Nsegshold, &
     &     'Lateral flow to each segment', &
     &     'acre-inches', Segment_latflow)
      ALLOCATE ( Lake_latflow(Nlakeshold) )
      CALL declvar_dble(MODNAME, 'Lake_latflow', 'nlake', Nlakeshold, &
     &     'Total lateral flow into each lake', &
     &     'acre-inches', Lake_latflow)
      ALLOCATE ( Lake_precip(Nlakeshold) )
      CALL declvar_dble(MODNAME, 'Lake_precip', 'nlake', Nlakeshold, &
     &     'Precipitation into each lake', &
     &     'acre-inches', Lake_precip)
      ALLOCATE ( Lake_et(Nlakeshold) )
      CALL declvar_dble(MODNAME, 'Lake_et', 'nlake', Nlakeshold, &
     &     'Evaporation from each lake', &
     &     'acre-inches', Lake_et)
      ALLOCATE ( add_irrigation_seg(Nsegshold) )
      CALL declvar_dble(MODNAME, 'add_irrigation_seg', 'nsegment', Nsegshold, &
     &     'Estimated irrigation demand needed for diversion from each segment', &
     &     'acre-inches', add_irrigation_seg)
      ALLOCATE ( Lake_In_flow(Nlakeshold) )

      END SUBROUTINE prms2modsimdecl

!***********************************************************************
!     prms2mfinit - Initialize PRMS2MODSIM module - get parameter values
!***********************************************************************
      SUBROUTINE prms2modsiminit(EXCHANGE, agDemand, Nsegshold)
      USE PRMS_CONSTANTS, ONLY: FT2_PER_ACRE
      USE GSFPRMS2MODSIM
      USE PRMS_MODULE, ONLY: Hru_type
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Lake_hru_id
      use PRMS_MMFAPI, only: declvar_dble
      use prms_utils, only: read_error
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Nsegshold
      DOUBLE PRECISION, INTENT(OUT) :: EXCHANGE(Nsegshold), agDemand(Nsegshold)
      ! Functions
      INTRINSIC :: ABS, DBLE
      ! Local Variables
      INTEGER :: i, ii, ierr
      DOUBLE PRECISION :: MSl3_to_ft3
!      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: seg_area
!***********************************************************************
      ierr = 0

!      IF ( Nlakeshold/=NLAKES ) THEN ! use MODSIM dimensions
!        PRINT *, 'ERROR, PRMS dimension nlake must equal Lake Package NLAKES'
!        PRINT *, '       nlake=', Nlakeshold, ' NLAKES=', NLAKES
!        ierr = 1
!      ENDIF

!      IF ( Nsegshold/=NSS ) THEN
!        PRINT *, 'ERROR, nsegment must equal NSS', Nsegshold, NSS
!        ierr = 1
!      ENDIF

      Segment_latflow = 0.0D0
      Lake_latflow = 0.0D0
      Lake_precip = 0.0D0
      Lake_et = 0.0D0
      Lake_In_flow = 0.0D0
      EXCHANGE = 0.0D0
      agDemand = 0.0D0
      add_irrigation_seg = 0.0D0

      ! sanity check
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Hru_type(i)==2 ) THEN
          ! Lake package active if Have_lakes=1
          IF ( Lake_hru_id(i)==0 ) THEN
            WRITE (*, 9001) i
            ierr = 1
! must separate condition as lake_hru_id not allocated if have_lakes=0
          ENDIF
        ENDIF
      ENDDO
      IF ( ierr==1 ) STOP

      !DEALLOCATE ( nseg_rch, seg_area ) ! need seg_area

      ! MODSIM in meters
      MSl3_to_ft3 = 3.280839895D0**3.0D0
      Acre_inches_to_MSl3 = FT2_PER_ACRE/(MSl3_to_ft3*12.0D0)

 9001 FORMAT ('ERROR, HRU: ', I0, ' is specified as a lake (hru_type=2) and lake_hru_id is specified as 0', /)

      END SUBROUTINE prms2modsiminit

!***********************************************************************
!     prms2modsimrun - Maps the PRMS results to MODSIM lakes and segments
!***********************************************************************
      SUBROUTINE prms2modsimrun(EXCHANGE, DELTAVOL, LAKEVAP, agDemand, Nsegshold, Nlakeshold)
      USE PRMS_CONSTANTS, ONLY: ACTIVE
      USE GSFPRMS2MODSIM
      USE PRMS_MODULE, ONLY: Hru_type, Iter_aet_flag
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_area, Lake_hru_id, Lake_area
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Hru_actet, Strm_seg_in
      USE PRMS_SET_TIME, ONLY: Cfs_conv
      USE PRMS_MODSIM_DIVERSION_READ, ONLY: Nag_diversions, Hru_destination_id, Seg_source_id
      USE PRMS_SRUNOFF, ONLY: Hortonian_lakes
      USE PRMS_SOILZONE, ONLY: Lakein_sz
      USE PRMS_SOILZONE_AG, ONLY: Ag_irrigation_add_vol ! acre-inches
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Nsegshold, Nlakeshold
      DOUBLE PRECISION, INTENT(OUT) :: EXCHANGE(Nsegshold), DELTAVOL(Nlakeshold), LAKEVAP(Nlakeshold)
      DOUBLE PRECISION, INTENT(OUT) :: agDemand(Nsegshold)
! Functions
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: ii, ilake, j, i, ihru, iseg
!***********************************************************************

!-----------------------------------------------------------------------
! Flow to stream segments in Strm_seg_in, units cfs
!-----------------------------------------------------------------------
      DO i = 1, Nsegshold
        Segment_latflow(i) = Strm_seg_in(i) / Cfs_conv
        EXCHANGE(i) = Segment_latflow(i) * Acre_inches_to_MSl3
      ENDDO

      ! need mapping from HRUs to segments
!----------
! set the ag Demand for each segment based on estimated irrigation
! based on OpenET data
!----------
      IF ( Iter_aet_flag==ACTIVE ) THEN
        agDemand = 0.0D0
        DO j = 1, Nag_diversions
          ihru = hru_destination_id(j)
          iseg = seg_source_id(j)
          IF ( iseg > 0 ) &
               agDemand(iseg) = agDemand(iseg) + DBLE( Ag_irrigation_add_vol(ihru) ) * Acre_inches_to_MSl3 ! m3 / day
        ENDDO
        add_irrigation_seg = agDemand
      ENDIF
        
!-----------------------------------------------------------------------
! Add runoff and precip to lakes
! Pass in hru_actet for the lake
!-----------------------------------------------------------------------
      Lake_latflow = 0.0D0
      Lake_precip = 0.0D0
      Lake_et = 0.0D0
      DO ii = 1, Active_hrus
        j = Hru_route_order(ii)
        IF ( Hru_type(j)==2 ) THEN
          ilake = Lake_hru_id(j)
          ! need gwflow ??
          Lake_latflow(ilake) = Lake_latflow(ilake) + (Lakein_sz(j)+Hortonian_lakes(j))*Hru_area(j)
          Lake_precip(ilake) = Lake_precip(ilake) + Hru_ppt(j)*Hru_area(j) 
          Lake_et(ilake) = Lake_et(ilake) + Hru_actet(j)*Hru_area(j) ! acre-inches per day.
        ENDIF
      ENDDO
! need different array to pass back to MODSIM to avoid circular dependences.
      DO i = 1, Nlakeshold
        Lake_In_flow(i) = Lake_latflow(i) + Lake_precip(i)
        DELTAVOL(i) = Lake_In_flow(i) * Acre_inches_to_MSl3 ! m3 / day
        LAKEVAP(i) = Lake_et(i) / Lake_area(i) ! inches / day
      ENDDO

      END SUBROUTINE prms2modsimrun

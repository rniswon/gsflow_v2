!***********************************************************************
!     Compute PRMS inflows and outflows for MODSIM lakes and segments
!***********************************************************************
      MODULE GSFPRMS2MODSIM
      IMPLICIT NONE
!   Module Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW PRMS to MODSIM'
      character(len=14), SAVE :: MODNAME = 'gsflow_prms2modsim'
      character(len=*), parameter :: Version_gsflow_prms2modsim = '2021-06-11'
      DOUBLE PRECISION, SAVE :: Acre_inches_to_MSl3
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_latflow(:), Lake_In_flow(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Lake_latflow(:), Lake_precip(:), Lake_et(:)
      END MODULE GSFPRMS2MODSIM

!     ******************************************************************
!     Mapping module to convert PRMS states for use by MODSIM
!     ******************************************************************
      INTEGER FUNCTION gsflow_prms2modsim(EXCHANGE,DELTAVOL,LAKEVAP)
	  USE PRMS_CONSTANTS, ONLY: ACTIVE, SAVE_INIT, RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag, Nlake, Nsegment
      USE GSFPRMS2MODSIM
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(OUT) :: EXCHANGE(Nsegment), DELTAVOL(Nlake), LAKEVAP(Nlake)
! Functions
      INTEGER, EXTERNAL :: prms2modsiminit, prms2modsimrun
      EXTERNAL :: print_module
!***********************************************************************
      gsflow_prms2modsim = 0

      IF ( Process_flag==RUN ) THEN
        gsflow_prms2modsim = prms2modsimrun(EXCHANGE, DELTAVOL, LAKEVAP)
      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_gsflow_prms2modsim)
      ELSEIF ( Process_flag==INIT ) THEN
        gsflow_prms2modsim = prms2modsiminit()
      ENDIF

      END FUNCTION gsflow_prms2modsim

!***********************************************************************
!     prms2mfinit - Initialize PRMS2MF module - get parameter values
!***********************************************************************
      INTEGER FUNCTION prms2modsiminit()
      USE PRMS_CONSTANTS, ONLY: FT2_PER_ACRE
      USE GSFPRMS2MODSIM
      USE PRMS_MODULE, ONLY: Nsegment, Nlake
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Lake_hru_id
	  USE PRMS_MODULE, ONLY: declvar_dble_1d
      IMPLICIT NONE
      EXTERNAL read_error
      INTRINSIC ABS, DBLE
! Local Variables
      INTEGER :: i, ii, ierr
      DOUBLE PRECISION :: MSl3_to_ft3
!      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: seg_area
!***********************************************************************
      prms2modsiminit = 0
      ierr = 0

!      IF ( Nlake/=NLAKES ) THEN ! use MODSIM dimensions
!        PRINT *, 'ERROR, PRMS dimension nlake must equal Lake Package NLAKES'
!        PRINT *, '       nlake=', Nlake, ' NLAKES=', NLAKES
!        ierr = 1
!      ENDIF

!      IF ( Nsegment/=NSS ) THEN
!        PRINT *, 'ERROR, nsegment must equal NSS', Nsegment, NSS
!        ierr = 1
!      ENDIF

      ALLOCATE ( Segment_latflow(Nsegment) )
      CALL declvar_dble_1d(MODNAME, 'Segment_latflow', 'nsegment', Nsegment, &
     &     'Lateral flow to each segment', &
     &     'acre-inches', Segment_latflow)
      ALLOCATE ( Lake_latflow(Nlake) )
      CALL declvar_dble_1d(MODNAME, 'Lake_latflow', 'nlake', Nlake, &
     &     'Total lateral flow into each lake', &
     &     'acre-inches', Lake_latflow)
      ALLOCATE ( Lake_precip(Nlake) )
      CALL declvar_dble_1d(MODNAME, 'Lake_precip', 'nlake', Nlake, &
     &     'Precipitation into each lake', &
     &     'acre-inches', Lake_precip)
      ALLOCATE ( Lake_et(Nlake) )
      CALL declvar_dble_1d(MODNAME, 'Lake_et', 'nlake', Nlake, &
     &     'Evaporation from each lake', &
     &     'acre-inches', Lake_et)
      ALLOCATE ( Lake_In_flow(Nlake) )

      Segment_latflow = 0.0D0
      Lake_latflow = 0.0D0
      Lake_precip = 0.0D0
      Lake_et = 0.0D0
      Lake_In_flow = 0.0D0

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

      END FUNCTION prms2modsiminit

!***********************************************************************
!     prms2modsimrun - Maps the PRMS results to MODSIM lakes and segments
!***********************************************************************
      INTEGER FUNCTION prms2modsimrun(EXCHANGE, DELTAVOL, LAKEVAP)
      USE PRMS_CONSTANTS, ONLY: FT2_PER_ACRE
      USE GSFPRMS2MODSIM
      USE PRMS_MODULE, ONLY: Nsegment, Nlake
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_area, Lake_hru_id, Lake_area
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Hru_actet
      USE PRMS_SET_TIME, ONLY: Cfs_conv, Timestep_seconds
      USE PRMS_SRUNOFF, ONLY: Hortonian_lakes, Strm_seg_in
      USE PRMS_SOILZONE, ONLY: Lakein_sz
!      USE PRMS_GWFLOW, ONLY: 
      IMPLICIT NONE
! Arguments
      DOUBLE PRECISION, INTENT(OUT) :: EXCHANGE(Nsegment), DELTAVOL(Nlake), LAKEVAP(Nlake)
! Local Variables
      INTEGER :: ii, ilake, j, i
!***********************************************************************
      prms2modsimrun = 0

!-----------------------------------------------------------------------
! Flow to stream segments in Strm_seg_in, units cfs
!-----------------------------------------------------------------------
      Cfs_conv = FT2_PER_ACRE/Timestep_seconds/12.0D0 ! need segment_latflow in acre-inches
      DO i = 1, Nsegment
        Segment_latflow(i) = Strm_seg_in(i) / Cfs_conv
        EXCHANGE(i) = Segment_latflow(i) * Acre_inches_to_MSl3
      ENDDO

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
      DO i = 1, Nlake
        Lake_In_flow(i) = Lake_latflow(i) + Lake_precip(i)
        DELTAVOL(i) = Lake_In_flow(i) * Acre_inches_to_MSl3 ! m3 / day
        LAKEVAP(i) = Lake_et(i) / Lake_area(i) ! inches / day
      ENDDO
 
      END FUNCTION prms2modsimrun

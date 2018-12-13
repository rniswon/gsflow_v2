!***********************************************************************
! Defines stream and lake routing parameters and variables
!***********************************************************************
      MODULE PRMS_ROUTING
      IMPLICIT NONE
!   Local Variables
      CHARACTER(LEN=7), SAVE :: MODNAME
      DOUBLE PRECISION, SAVE :: Cfs2acft
      DOUBLE PRECISION, SAVE :: Segment_area
      INTEGER, SAVE :: Noarea_flag
      INTEGER, SAVE, ALLOCATABLE :: Segment_order(:), Segment_up(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_hruarea(:)
      CHARACTER(LEN=80), SAVE :: Version_routing
      !CHARACTER(LEN=32), SAVE :: Outfmt
!   Declared Variables
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_ssflow(:), Seginc_sroff(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Seginc_gwflow(:), Seginc_swrad(:), Seginc_potet(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Hru_outflow(:), Seg_ssflow(:), Seg_sroff(:), Seg_gwflow(:)
!   Declared Parameters
      INTEGER, SAVE, ALLOCATABLE :: Tosegment(:), Hru_segment(:), Obsin_segment(:)
      END MODULE PRMS_ROUTING

!***********************************************************************
!     Main routing routine
!***********************************************************************
      INTEGER FUNCTION routing()
      USE PRMS_MODULE, ONLY: Process, Save_vars_to_file, Init_vars_from_file
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: routingdecl, routinginit, route_run
      EXTERNAL :: routing_restart
!***********************************************************************
      routing = 0

      IF ( Process(:3)=='run' ) THEN
        routing = route_run()
      ELSEIF ( Process(:4)=='decl' ) THEN
        routing = routingdecl()
      ELSEIF ( Process(:4)=='init' ) THEN
        IF ( Init_vars_from_file==1 ) CALL routing_restart(1)
        routing = routinginit()
      ELSEIF ( Process(:5)=='clean' ) THEN
        IF ( Save_vars_to_file==1 ) CALL routing_restart(0)
      ENDIF

      END FUNCTION routing

!***********************************************************************
!     routingdecl - set up parameters
!***********************************************************************
      INTEGER FUNCTION routingdecl()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Model, Cascade_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      routingdecl = 0

      Version_routing = 'routing.f90 2017-10-25 15:33:00Z'
      CALL print_module(Version_routing, 'Routing Initialization      ', 90)
      MODNAME = 'routing'

! Declared Variables
      ALLOCATE ( Hru_outflow(Nhru) )
      IF ( declvar(MODNAME, 'hru_outflow', 'nhru', Nhru, 'double', &
     &     'Total flow leaving each HRU', &
     &     'cfs', Hru_outflow)/=0 ) CALL read_error(3, 'hru_outflow')

      ALLOCATE ( Tosegment(Nsegment) )
      IF ( declparam(MODNAME, 'tosegment', 'nsegment', 'integer', &
     &     '0', '-11', '1000000', &
     &     'The index of the downstream segment', &
     &     'Index of downstream segment to which the segment'// &
     &     ' streamflow flows, for segments that do not flow to another segment enter 0', &
     &     'none')/=0 ) CALL read_error(1, 'tosegment')

      IF ( Cascade_flag==0 .OR. Model==99 ) THEN
        ALLOCATE ( Hru_segment(Nhru) )
        IF ( declparam(MODNAME, 'hru_segment', 'nhru', 'integer', &
     &       '0', 'bounded', 'nsegment', &
     &       'Segment index for HRU lateral inflows', &
     &       'Segment index to which an HRU contributes lateral flows'// &
     &       ' (surface runoff, interflow, and groundwater discharge)', &
     &       'none')/=0 ) CALL read_error(1, 'hru_segment')
      ENDIF

      ALLOCATE ( Obsin_segment(Nsegment) )
      IF ( declparam(MODNAME, 'obsin_segment', 'nsegment', 'integer', &
     &     '0', 'bounded', 'nobs', &
     &     'Index of measured streamflow station that replaces inflow to a segment', &
     &     'Index of measured streamflow station that replaces inflow to a segment', &
     &     'none')/=0 ) CALL read_error(1, 'obsin_segment')

      IF ( Cascade_flag==0 .OR. Model==99 ) THEN
        ALLOCATE ( Seginc_potet(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_potet', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average potential ET for each segment'// &
     &       ' from HRUs contributing flow to the segment', &
     &       'inches', Seginc_potet)/=0 ) CALL read_error(3, 'seginc_potet')

        ALLOCATE ( Seginc_swrad(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_swrad', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average solar radiation for each segment'// &
     &       ' from HRUs contributing flow to the segment', &
     &       'Langleys', Seginc_swrad)/=0 ) CALL read_error(3, 'seginc_swrad')

        ALLOCATE ( Seginc_ssflow(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_ssflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average interflow for each segment from'// &
     &       ' HRUs contributing flow to the segment', &
     &       'cfs', Seginc_ssflow)/=0 ) CALL read_error(3, 'seginc_ssflow')

        ALLOCATE ( Seginc_gwflow(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_gwflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average groundwater discharge for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_gwflow)/=0 ) CALL read_error(3, 'seginc_gwflow')

        ALLOCATE ( Seginc_sroff(Nsegment) )
        IF ( declvar(MODNAME, 'seginc_sroff', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average surface runoff for each'// &
     &       ' segment from HRUs contributing flow to the segment', &
     &       'cfs', Seginc_sroff)/=0 ) CALL read_error(3, 'seginc_sroff')

        ALLOCATE ( Seg_ssflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_ssflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average interflow for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_ssflow)/=0 ) CALL read_error(3, 'seg_ssflow')

        ALLOCATE ( Seg_gwflow(Nsegment) )
        IF ( declvar(MODNAME, 'seg_gwflow', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average groundwater discharge for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_gwflow)/=0 ) CALL read_error(3, 'seg_gwflow')

        ALLOCATE ( Seg_sroff(Nsegment) )
        IF ( declvar(MODNAME, 'seg_sroff', 'nsegment', Nsegment, 'double', &
     &       'Area-weighted average surface runoff for each segment from'// &
     &       ' HRUs contributing flow to the segment and upstream HRUs', &
     &       'inches', Seg_sroff)/=0 ) CALL read_error(3, 'seg_sroff')
      ENDIF

      ! local arrays
      ALLOCATE ( Segment_order(Nsegment), Segment_up(Nsegment), Segment_hruarea(Nsegment) )

      END FUNCTION routingdecl

!**********************************************************************
!     routinginit - check for validity of parameters
!**********************************************************************
      INTEGER FUNCTION routinginit()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Init_vars_from_file, Nhru, Cascade_flag, Print_debug, Inputerror_flag
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE PRMS_BASIN, ONLY: FT2_PER_ACRE, DNEARZERO, Active_hrus, Hru_route_order, Hru_area_dble, NEARZERO !, Active_area
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: getparam
      EXTERNAL :: read_error
! Local Variable
      INTEGER :: i, j, test, lval, toseg, iseg, isegerr
      INTEGER, ALLOCATABLE :: x_off(:)
      CHARACTER(LEN=10) :: buffer
!**********************************************************************
      routinginit = 0

      IF ( Init_vars_from_file==0 ) THEN
        IF ( Cascade_flag==0 ) THEN
          Seginc_potet = 0.0D0
          Seginc_gwflow = 0.0D0
          Seginc_ssflow = 0.0D0
          Seginc_sroff = 0.0D0
          Seginc_swrad = 0.0D0
          Seg_gwflow = 0.0D0
          Seg_ssflow = 0.0D0
          Seg_sroff = 0.0D0
        ENDIF
        Hru_outflow = 0.0D0
      ENDIF

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

      IF ( getparam(MODNAME, 'tosegment', Nsegment, 'integer', Tosegment)/=0 ) CALL read_error(2, 'tosegment')
      IF ( getparam(MODNAME, 'obsin_segment', Nsegment, 'integer', Obsin_segment)/=0 ) CALL read_error(2, 'obsin_segment')

      Noarea_flag = 0
      IF ( Cascade_flag==0 ) THEN
        IF ( getparam(MODNAME, 'hru_segment', Nhru, 'integer', Hru_segment)/=0 ) CALL read_error(2, 'hru_segment')
        Segment_hruarea = 0.0D0
        DO j = 1, Active_hrus
          i = Hru_route_order(j)
          iseg = Hru_segment(i)
          IF ( iseg>0 ) Segment_hruarea(iseg) = Segment_hruarea(iseg) + Hru_area_dble(i)
        ENDDO
        Segment_area = 0.0D0
        DO i = 1, Nsegment
          Segment_area = Segment_area + Segment_hruarea(i)
          IF ( Segment_hruarea(i)<DNEARZERO ) THEN
            Noarea_flag = 1
            IF ( Print_debug>-1 ) THEN
              WRITE ( buffer, '(I10)' ) i
              CALL write_outfile('WARNING, No HRUs are associated with segment:'//buffer)
              IF ( Tosegment(i)==0 ) PRINT *, 'WARNING, No HRUs and tosegment=0 for segment:', i
            ENDIF
          ENDIF
        ENDDO
!        IF ( Active_area/=Segment_area ) PRINT *, 'Not all area in model domain included with segments, basin area =', &
!     &                                            Active_area, ' segment area = ', Segment_area
      ENDIF

      isegerr = 0
      Segment_up = 0
      ! Begin the loops for ordering segments
      DO j = 1, Nsegment
        iseg = Obsin_segment(j)
        toseg = Tosegment(j)
        IF ( toseg==j ) THEN
          PRINT *, 'ERROR, tosegment value (', toseg, ') equals itself for segment:', j
          isegerr = 1
        ELSEIF ( toseg>0 ) THEN
          ! load segment_up with last stream segment that flows into a segment
          Segment_up(toseg) = j
        ENDIF
      ENDDO

      IF ( Print_debug>-1 ) THEN
        DO i = 1, Nsegment
          IF ( Segment_up(i)==0 .AND. Tosegment(i)==0 ) &
     &         PRINT *, 'WARNING, no other segment flows into segment:',  i, ' and tosegment=0'
        ENDDO
      ENDIF

      IF ( isegerr==1 ) THEN
        Inputerror_flag = 1
        RETURN
      ENDIF

      ! Begin the loops for ordering segments
      ALLOCATE ( x_off(Nsegment) )
      x_off = 0
      Segment_order = 0
      lval = 0
      DO WHILE ( lval<Nsegment )
        DO i = 1, Nsegment
          ! If segment "i" has not been crossed out consider it, else continue
          IF ( x_off(i)==1 ) CYCLE
          ! Test to see if segment "i" is the to segment from other segments
          test = 1
          DO j = 1, Nsegment
            IF ( Tosegment(j)==i ) THEN
              ! If segment "i" is a to segment, test to see if the originating
              ! segment has been crossed off the list.  if all have been, then
              ! put the segment in as an ordered segment
              IF ( x_off(j)==0 ) THEN
                test = 0
                EXIT
              ENDIF
            ENDIF
          ENDDO
          IF ( test==1 ) THEN
            lval = lval + 1
            Segment_order(lval) = i
            x_off(i) = 1
          ENDIF
        ENDDO
      ENDDO
!      IF ( Print_debug==20 ) THEN
!        PRINT *, 'Stream Network Routing Order:'
!        PRINT '(10I5)', Segment_order
!        PRINT *, 'tosegment:'
!        PRINT '(10I5)', Tosegment
!      ENDIF
      DEALLOCATE ( x_off )

      END FUNCTION routinginit

!***********************************************************************
!     route_run - Computes segment flow states and fluxes
!***********************************************************************
      INTEGER FUNCTION route_run()
      USE PRMS_ROUTING
      USE PRMS_MODULE, ONLY: Nsegment, Cascade_flag
      USE PRMS_BASIN, ONLY: Hru_area, Hru_route_order, Active_hrus, NEARZERO, FT2_PER_ACRE
      USE PRMS_CLIMATEVARS, ONLY: Swrad, Potet
      USE PRMS_SET_TIME, ONLY: Timestep_seconds, Cfs_conv
      USE PRMS_FLOWVARS, ONLY: Ssres_flow, Sroff, Seg_lateral_inflow !, Seg_outflow
      USE PRMS_GWFLOW, ONLY: Gwres_flow
      USE PRMS_SRUNOFF, ONLY: Strm_seg_in
      IMPLICIT NONE
      INTRINSIC DBLE
! Local Variables
      INTEGER :: i, j, jj
      DOUBLE PRECISION :: tocfs
!***********************************************************************
      route_run = 0

      Cfs2acft = Timestep_seconds/FT2_PER_ACRE

      IF ( Cascade_flag==0 ) THEN
        ! add hru_ppt, hru_actet
        Seginc_gwflow = 0.0D0
        Seginc_ssflow = 0.0D0
        Seginc_sroff = 0.0D0
        Seginc_swrad = 0.0D0
        Seginc_potet = 0.0D0
        Seg_gwflow = 0.0D0
        Seg_sroff = 0.0D0
        Seg_ssflow = 0.0D0
        Seg_lateral_inflow = 0.0D0
      ELSE
        Seg_lateral_inflow = Strm_seg_in
      ENDIF

      DO jj = 1, Active_hrus
        j = Hru_route_order(jj)
        tocfs = DBLE( Hru_area(j) )*Cfs_conv
        Hru_outflow(j) = DBLE( (Sroff(j) + Ssres_flow(j) + Gwres_flow(j)) )*tocfs
        IF ( Cascade_flag==0 ) THEN
          i = Hru_segment(j)
          IF ( i>0 ) THEN
            Seg_gwflow(i) = Seg_gwflow(i) + Gwres_flow(j)
            Seg_sroff(i) = Seg_sroff(i) + Sroff(j)
            Seg_ssflow(i) = Seg_ssflow(i) + Ssres_flow(j)
            Seg_lateral_inflow(i) = Seg_lateral_inflow(i) + Hru_outflow(j)
            Seginc_sroff(i) = Seginc_sroff(i) + DBLE( Sroff(j) )*tocfs
            Seginc_ssflow(i) = Seginc_ssflow(i) + DBLE( Ssres_flow(j) )*tocfs
            Seginc_gwflow(i) = Seginc_gwflow(i) + DBLE( Gwres_flow(j) )*tocfs
            Seginc_swrad(i) = Seginc_swrad(i) + DBLE( Swrad(j)*Hru_area(j) )
            Seginc_potet(i) = Seginc_potet(i) + DBLE( Potet(j)*Hru_area(j) )
          ENDIF
        ENDIF
      ENDDO

      IF ( Cascade_flag==1 ) RETURN

! Divide solar radiation and PET by sum of HRU area to get avarage
      IF ( Noarea_flag==0 ) THEN
        DO i = 1, Nsegment
          Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
          Seginc_potet(i) = Seginc_potet(i)/Segment_hruarea(i)
        ENDDO

! If there are no HRUs associated with a segment, then figure out some
! other way to get the solar radiation, the following is not great
      ELSE !     IF ( Noarea_flag==1 ) THEN
        DO i = 1, Nsegment
          IF ( Segment_hruarea(i)>NEARZERO ) THEN
            Seginc_swrad(i) = Seginc_swrad(i)/Segment_hruarea(i)
            Seginc_potet(i) = Seginc_potet(i)/Segment_hruarea(i)
          ELSEIF ( Tosegment(i)>0 ) THEN
            Seginc_swrad(i) = Seginc_swrad(Tosegment(i))
            Seginc_potet(i) = Seginc_potet(Tosegment(i))
          ELSEIF ( i>1 ) THEN ! set to next segment id
            Seginc_swrad(i) = Seginc_swrad(i-1)
            Seginc_potet(i) = Seginc_potet(i-1)
          ELSE ! assume at least 2 segments
            Seginc_swrad(i) = Seginc_swrad(i+1)
            Seginc_potet(i) = Seginc_potet(i+1)
          ENDIF
        ENDDO
      ENDIF

      END FUNCTION route_run

!***********************************************************************
!     routing_restart - write or read restart file
!***********************************************************************
      SUBROUTINE routing_restart(In_out)
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit
      USE PRMS_ROUTING
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      EXTERNAL check_restart
      ! Local Variables
      CHARACTER(LEN=7) :: module_name
!***********************************************************************
      IF ( In_out==0 ) THEN
        WRITE ( Restart_outunit ) MODNAME
        WRITE ( Restart_outunit ) Cfs2acft
        WRITE ( Restart_outunit ) Seginc_ssflow
        WRITE ( Restart_outunit ) Seginc_sroff
        WRITE ( Restart_outunit ) Seginc_gwflow
        WRITE ( Restart_outunit ) Seginc_swrad
        WRITE ( Restart_outunit ) Seginc_potet
        WRITE ( Restart_outunit ) Hru_outflow
      ELSE
        READ ( Restart_inunit ) module_name
        CALL check_restart(MODNAME, module_name)
        READ ( Restart_inunit ) Cfs2acft
        READ ( Restart_inunit ) Seginc_ssflow
        READ ( Restart_inunit ) Seginc_sroff
        READ ( Restart_inunit ) Seginc_gwflow
        READ ( Restart_inunit ) Seginc_swrad
        READ ( Restart_inunit ) Seginc_potet
        READ ( Restart_inunit ) Hru_outflow
      ENDIF
      END SUBROUTINE routing_restart

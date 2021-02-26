!***********************************************************************
!     Route PRMS gravity flow to MODFLOW cells
!***********************************************************************
      MODULE GSFPRMS2MF
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ACTIVE, OFF
      IMPLICIT NONE
!   Module Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW PRMS to MODFLOW'
      character(len=*), parameter :: MODNAME = 'gsflow_prms2mf'
      character(len=*), parameter :: Version_gsflow_prms2mf = '2021-01-08'
      REAL, PARAMETER :: SZ_CHK = 0.00001
      DOUBLE PRECISION, PARAMETER :: PCT_CHK = 0.000005D0
      INTEGER, SAVE :: NTRAIL_CHK, Nlayp1
      ! Number of stream reaches in each stream segment
      INTEGER, SAVE, ALLOCATABLE :: Numreach_segment(:)
      REAL, SAVE, ALLOCATABLE :: Excess(:)
      DOUBLE PRECISION, SAVE :: Totalarea
!   Declared Variables
      DOUBLE PRECISION, SAVE :: Basin_reach_latflow, Net_sz2gw
!     INTEGER, SAVE, ALLOCATABLE :: Reach_id(:,:)
      REAL, SAVE, ALLOCATABLE :: Gw_rejected_grav(:)
!     DOUBLE PRECISION, SAVE, ALLOCATABLE :: Reach_latflow(:)
      REAL, SAVE, ALLOCATABLE :: Cell_drain_rate(:)
      DOUBLE PRECISION, SAVE, ALLOCATABLE :: Segment_pct_area(:)
!   Declared Parameters
!     INTEGER, SAVE, ALLOCATABLE :: Local_reachid(:)
!     INTEGER, SAVE, ALLOCATABLE :: Reach_segment(:)
      REAL, SAVE, ALLOCATABLE :: Gvr_hru_pct(:)
      END MODULE GSFPRMS2MF

!     ******************************************************************
!     Mapping module to convert PRMS & MODFLOW states for use by GSFLOW
!     NOTE: Assumes that the sm2gw_grav variable is in inches.
!           Produces cell_drain in MODFLOW units.
!     ******************************************************************
      INTEGER FUNCTION gsflow_prms2mf()
      USE PRMS_CONSTANTS, ONLY: RUN, DECL, INIT
      USE PRMS_MODULE, ONLY: Process_flag
      IMPLICIT NONE
! Functions
      INTEGER, EXTERNAL :: prms2mfdecl, prms2mfinit, prms2mfrun
!***********************************************************************
      gsflow_prms2mf = 0

      IF ( Process_flag==RUN ) THEN
        gsflow_prms2mf = prms2mfrun()
      ELSEIF ( Process_flag==DECL ) THEN
        gsflow_prms2mf = prms2mfdecl()
      ELSEIF ( Process_flag==INIT ) THEN
        gsflow_prms2mf = prms2mfinit()
      ENDIF

      END FUNCTION gsflow_prms2mf

!***********************************************************************
!     prms2mfdecl - set up parameters
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id, gvr_hru_pct
!     hru_area, mxsziter
!***********************************************************************
      INTEGER FUNCTION prms2mfdecl()
      USE GSFPRMS2MF
      USE PRMS_MODULE, ONLY: Nhrucell, Ngwcell, Nhru, Nsegment, Model
      IMPLICIT NONE
      INTEGER, EXTERNAL :: declparam, declvar
      EXTERNAL read_error, print_module
!***********************************************************************
      prms2mfdecl = 0

      CALL print_module(MODDESC, MODNAME, Version_gsflow_prms2mf)

! Declared Variables
      IF ( declvar(MODNAME, 'net_sz2gw', 'one', 1, 'double', &
     &     'Net volumetric flow rate of gravity drainage from the'// &
     &     ' soil zone to the unsaturated and saturated zones', &
     &     'L3/T', Net_sz2gw)/=0 ) CALL read_error(3, 'net_sz2gw')

!     ALLOCATE (Reach_latflow(Nreach))
!     IF ( decl var(MODNAME, 'reach_latflow', 'nreach', Nreach, 'double', &
!    &     'Lateral flow (surface runoff and interflow) into each stream reach', &
!    &     'cfs', Reach_latflow)/=0 ) CALL read_error(3, 'reach_latflow')

!     ALLOCATE (Reach_id(Nreach, Nsegment))
!     IF ( decl var(MODNAME, 'reach_id', 'nsegment,nreach', &
!    &     Nsegment*Nreach, 'integer', &
!    &     'Mapping of reach id by segment id', &
!    &     'none', Reach_id)/=0 ) CALL read_error(3, 'reach_id')

      ALLOCATE (Cell_drain_rate(Ngwcell))
      IF ( declvar(MODNAME, 'cell_drain_rate', 'ngwcell', Ngwcell, 'real', &
     &     'Recharge rate for each cell', &
     &     'L/T', Cell_drain_rate)/=0 ) CALL read_error(3, 'cell_drain_rate')

      IF ( declvar(MODNAME, 'basin_reach_latflow', 'one', 1, 'double', &
     &     'Lateral flow into all reaches in basin', &
     &     'cfs', Basin_reach_latflow)/=0 ) CALL read_error(3, 'basin_reach_latflow')

      ALLOCATE (Gw_rejected_grav(Nhrucell))
      IF ( declvar(MODNAME, 'gw_rejected_grav', 'nhrucell', Nhrucell, 'real', &
     &   'Recharge rejected by UZF for each gravity-flow reservoir', &
     &   'inches', Gw_rejected_grav)/=0 ) CALL read_error(3, 'gw_rejected_grav')

      !rsr, all reaches receive same precentage of flow to each segment
      ALLOCATE (Segment_pct_area(Nsegment))
!      IF ( declvar(MODNAME, 'segment_pct_area', 'nsegment', Nsegment, 'double', &
!     &     'Proportion of each segment that contributes flow to a stream reach', &
!     &     'decimal fraction', Segment_pct_area)/=0 ) CALL read_error(3, 'segment_pct_area')

      ! Allocate local arrays
      ALLOCATE ( Excess(Ngwcell) )

! Declared Parameters
        ! should be change code so that flow to each reach is computed, either based on a
        ! new parameter segment_reach_fraction or reach_carea or change cascade
        ! procedure to cascade flow to reaches instead of segments
!        ALLOCATE (Segment_reach_fraction(Nreach))
!        IF ( declparam(MODNAME, 'segment_reach_fraction', 'nreach', 'real', &
!      &      '0.0', '0.0', '1.0', &
!      &      'Proportion of each segment that contributes flow to a stream reach', &
!      &      'Proportion of each segment that contributes flow to a stream reach', &
!      &      'decimal fraction')/=0 ) CALL read_error(1, 'segment_reach_fraction')

!     ALLOCATE (Local_reachid(Nreach))
!     IF ( decl param(MODNAME, 'local_reachid', 'nreach', 'integer', &
!    &     '0', 'bounded', 'nreach', &
!    &     'Map of the global reach ids to reach ids of each segment', &
!    &     'Index of stream reach within a stream segment for each stream reach', &
!    &     'none')/=0 ) CALL read_error(1, 'local_reachid')

!     ALLOCATE (Reach_segment(Nreach))
!     IF ( decl param(MODNAME, 'reach_segment', 'nreach', 'integer', &
!    &     '0', 'bounded', 'nsegment', &
!    &     'Map of the stream reaches to the stream segments', &
!    &     'Index of stream segment associate with each stream reach', &
!    &     'none')/=0 ) CALL read_error(1, 'reach_segment')

      IF ( Nhru/=Nhrucell .OR. Model==99 ) THEN
        ALLOCATE ( Gvr_hru_pct(Nhrucell) )
        IF ( declparam('prms2mf', 'gvr_hru_pct', 'nhrucell', 'real', &
     &       '0.0', '0.0', '1.0', &
     &       'Proportion of the HRU associated with each GVR', &
     &       'Proportion of the HRU area associated with each gravity reservoir', &
     &       'decimal fraction').NE.0 ) RETURN
      ENDIF

      END FUNCTION prms2mfdecl

!***********************************************************************
!     prms2mfinit - Initialize PRMS2MF module - get parameter values
!***********************************************************************
      INTEGER FUNCTION prms2mfinit()
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, ERROR_param
      USE GSFPRMS2MF
      USE GWFUZFMODULE, ONLY: NTRAIL, NWAV, IUZFBND
      USE GWFSFRMODULE, ONLY: ISEG, NSS
      USE GWFLAKMODULE, ONLY: NLAKES
      USE GSFMODFLOW, ONLY: Gwc_row, Gwc_col
      USE PRMS_MODULE, ONLY: Nhru, Nsegment, Nlake, Print_debug, &
     &    Nhrucell, Ngwcell, Gvr_cell_id, Have_lakes
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, &
     &    Basin_area_inv, Hru_area
      USE PRMS_SOILZONE, ONLY: Gvr_hru_id, Gvr_hru_pct_adjusted
      USE GLOBAL, ONLY: NLAY, NROW, NCOL
      IMPLICIT NONE
      INTEGER, EXTERNAL :: getparam
      EXTERNAL read_error
      INTRINSIC ABS, DBLE
! Local Variables
      INTEGER :: is, i, ii, ierr, ihru, icell, irow, icol
!     INTEGER :: iseg, max_seg, irch
!     INTEGER, ALLOCATABLE, DIMENSION(:) :: nseg_rch
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: hru_pct, newpct
      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: temp_pct
!     DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: seg_area
      DOUBLE PRECISION :: pct
!***********************************************************************
      prms2mfinit = 0

      Nlayp1 = NLAY + 1

      ierr = 0
      IF ( NROW*NCOL/=Ngwcell ) THEN
        PRINT *, 'ERROR, dimension Ngwcell not equal to NROW*NCOL', Ngwcell, NROW, NCOL
        PRINT *, '       Check for use of correct Parameter File'
        ierr = 1
      ENDIF

      IF ( Have_lakes==ACTIVE ) THEN
        IF ( Nlake/=NLAKES ) THEN
          PRINT *, 'ERROR, PRMS dimension nlake must equal Lake Package NLAKES'
          PRINT *, '       nlake=', Nlake, ' NLAKES=', NLAKES
          ierr = 1
        ENDIF
      ENDIF

      IF ( Nsegment/=NSS ) THEN
        PRINT *, 'ERROR, nsegment must equal NSS', Nsegment, NSS
        ierr = 1
      ENDIF

      IF ( Nhru/=Nhrucell ) THEN
        IF ( getparam('prms2mf', 'gvr_hru_pct', Nhrucell, 'real', Gvr_hru_pct)/=0 ) CALL read_error(2, 'gvr_hru_pct')
      ENDIF

      ALLOCATE ( Numreach_segment(Nsegment) )
      DO i = 1, Nsegment
        Numreach_segment(i) = ISEG(4, i)
        IF ( Numreach_segment(i)<1 ) THEN
          PRINT *, 'ERROR, no reaches associated with segment:', i
          ierr = 1
        ELSE
          Segment_pct_area(i) = 1.0D0 / DBLE( Numreach_segment(i) )
        ENDIF
      ENDDO
      IF ( ierr==1 ) ERROR STOP ERROR_param

!     IF ( get param(MODNAME, 'reach_segment', Nreach, 'integer', Reach_segment)/=0 ) CALL read_error(2, 'reach_segment')

!  DANGER markstro - overriding the parameter Segment_pct_area to test
!                    precision issues
      !rsr, only need number of reaches per segment, divide inflow to
      !     segments by number of reaches
!     DO i = 1, Nreach
!       iseg = Reach_segment(i)
!       Segment_pct_area(i) = 1.0D0 / DBLE( Numreach_segment(iseg) )
!     ENDDO
      ! change so that this is actual contributing area for each reach, not constant value for each reach for each segment
!      IF ( get param(MODNAME, 'segment_reach_fraction', Nreach, 'real', Segment_reach_fraction)/=0 ) &
!     &     CALL read_error(2, 'segment_reach_fraction')
!      IF ( get param(MODNAME, 'segment_pct_area', Nreach, 'real', Segment_pct_area)/=0 ) CALL read_error(2, 'segment_pct_area')
!      IF ( get param(MODNAME, 'local_reachid', Nreach, 'integer', &
!     &     Local_reachid)/=0 ) CALL read_error(2, 'local_reachid')

!      ALLOCATE (nseg_rch(Nsegment), seg_area(Nsegment))
!      DO i = 1, Nsegment
!        nseg_rch(i) = 0
!        seg_area(i) = 0.0D0
!      ENDDO
!      k = 0
!      DO i = 1, Nsegment
!        DO j = 1, Numreach_segment(i)
!          k = k + 1
!          Reach_id(j, i) = k
!          Reach_latflow(k) = 0.0D0
!        ENDDO
!      ENDDO
!      IF ( k/=Nreach ) THEN
!        PRINT *, 'nreach (', nreach, ') should equal sum of number of reaches per segment (', k, ')'
!        RETURN
!      ENDIF
!      max_seg = 0
!      Reach_id = 0
!      DO i = 1, Nreach
!        iseg = Reach_segment(i)
!        IF ( iseg>max_seg ) max_seg = iseg
!        IF ( iseg>Nsegment ) PRINT *, 'Problem with segment number', i, Nsegment, iseg
!        irch = Local_reachid(i)
!        seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
!        IF ( irch>Numreach_segment(iseg) ) PRINT *, &
!    &        'Problem with segment reach id', i, irch, Numreach_segment(iseg)
!        Reach_id(iseg, irch) = i
!        nseg_rch(iseg) = nseg_rch(iseg) + 1
!      ENDDO
!      IF ( max_seg/=Nsegment ) PRINT *, 'Problem with number of segments', Nsegment, max_seg

!      DO i = 1, Nsegment
!        IF ( nseg_rch(i)/=Numreach_segment(i) ) PRINT *, 'Problem with number of reaches in a segment', i,
!    &        nseg_rch(i), Numreach_segment(i)
!       ENDDO

! way to adjust segment_pct_area, rsr
!      WRITE ( 839, 9002 ) 'segment_pct_area 12', 'nreach', Nreach
!      DO i = 1, Nreach
!        iseg = Reach_segment(i)
!         Segment_pct_area(i) = Segment_pct_area(i) + &
!    &            Segment_pct_area(i)*(1.D0-seg_area(iseg))/seg_area(iseg)
!        WRITE (839,'(f15.13)') Segment_pct_area(i)
!      ENDDO
!      seg_area = 0.0
!      DO i = 1, Nreach
!        iseg = Reach_segment(i)
!        seg_area(iseg) = seg_area(iseg) + Segment_pct_area(i)
!      ENDDO
!      WRITE (839,'(f15.13)') seg_area
!      STOP

        Cell_drain_rate = 0.0 ! dimension ngwcell

      ierr = 0
      IF ( Nhru/=Nhrucell ) THEN
        ALLOCATE ( hru_pct(Nhru), newpct(Nhru), temp_pct(Nhrucell) )
        hru_pct = 0.0D0
        newpct = 0.0D0
      ENDIF
      DO i = 1, Nhrucell
        ihru = Gvr_hru_id(i)
        IF ( Nhru/=Nhrucell ) THEN
          temp_pct(i) = DBLE( Gvr_hru_pct(i) )
          is = Gvr_hru_id(i)
          hru_pct(is) = hru_pct(is) + temp_pct(i)
        ENDIF
        icell = Gvr_cell_id(i)
!        IF ( icell==0 ) CYCLE ! don't need as icell must be > 0
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)
        IF ( Print_debug>DEBUG_less ) THEN
          IF ( Hru_type(ihru)==0 ) THEN
            IF ( IUZFBND(icol, irow)/=0 ) &
     &           PRINT *, 'WARNING, HRU inactive & UZF cell active, irow:', irow, 'icell:', icell, ' HRU:', ihru
          ENDIF
          IF ( IUZFBND(icol, irow)==0 ) THEN
            IF ( Hru_type(ihru)/=0 ) &
     &           PRINT *, 'WARNING, UZF cell inactive, irow:', irow, ' icell:', icell, ' HRU is active:', ihru
          ENDIF
        ENDIF
      ENDDO
      IF ( ierr==1 ) ERROR STOP ERROR_param

      IF ( Nhru/=Nhrucell ) THEN
! way to adjust gvr_hru_pct, rsr
!        WRITE ( 840, 9002 ) 'gvr_hru_pct 12', 'nhrucell', Nhrucell
        DO i = 1, Nhrucell
          is = Gvr_hru_id(i)
          temp_pct(i) = temp_pct(i) + temp_pct(i)*(1.0D0-hru_pct(is))/hru_pct(is)
          Gvr_hru_pct_adjusted(i) = temp_pct(i)
          newpct(is) = newpct(is) + temp_pct(i)
!        WRITE ( 840,'(F15.13)' ) temp_pct(i)
        ENDDO
      ELSE
        Gvr_hru_pct_adjusted = 1.0D0
      ENDIF
!      ERROR STOP ERROR_param

      Totalarea = 0.0D0
      DO ii = 1, Active_hrus
        i = Hru_route_order(ii)
        IF ( Nhru/=Nhrucell ) THEN
          pct = newpct(i)
          IF ( ABS(pct-1.0D0)>PCT_CHK ) PRINT *, 'Possible issue with GVR to HRU percentage, HRU:', i, pct
          IF ( pct<0.99D0 ) THEN
            ierr = 1
            PRINT *, 'ERROR, portion of HRU not included in mapping to cells', i, pct
          ELSEIF ( pct>1.00001D0 ) THEN
            IF ( pct>1.0001D0 ) THEN
              ierr = 1
              PRINT *, 'ERROR, extra portion of HRU included in mapping to cells', i, pct
            ENDIF
          ELSEIF ( pct<0.0D0 ) THEN
            PRINT *, 'ERROR, HRU to cell mapping is < 0.0', i, pct
            ierr = 1
          ELSEIF ( pct<PCT_CHK ) THEN
            IF ( Print_debug>DEBUG_less ) PRINT *, 'WARNING, active HRU is not mapped to any cell', i, pct
          ENDIF
          Totalarea = Totalarea + pct*DBLE( Hru_area(i) )
        ELSE
          Totalarea = Totalarea + DBLE( Hru_area(i) ) ! gvr_hru_pct = 1.0
        ENDIF
        IF ( Hru_type(i)==2 ) THEN
          ! Lake package active if Have_lakes=ACTIVE
          IF ( Have_lakes==OFF ) THEN
            WRITE (*, 9001) i
            ierr = 1
! must separate condition as lake_hru_id not allocated if have_lakes=OFF
            ! rsr, need to check that lake_hru_id not 0
          ENDIF
        ENDIF
      ENDDO
      IF ( ierr==1 ) ERROR STOP ERROR_param

      Totalarea = Totalarea*Basin_area_inv
      IF ( Print_debug>DEBUG_less ) PRINT 9003, (Totalarea-1.0D0)*100.0D0

      IF ( Nhru/=Nhrucell ) DEALLOCATE ( hru_pct, newpct, temp_pct )
      !DEALLOCATE ( nseg_rch, seg_area )

      Basin_reach_latflow = 0.0D0
      Net_sz2gw = 0.0D0
      Excess = 0.0 ! dimension ngwcell
      Gw_rejected_grav = 0.0 ! dimension nhrucell
      NTRAIL_CHK = NWAV - 3*NTRAIL + 1

      IF ( Nhru/=Nhrucell ) DEALLOCATE ( Gvr_hru_pct )

 9001 FORMAT ('ERROR, HRU:', I7, ' is specified as a lake (hru_type=2) and lake_hru_id is specified as 0', /, &
     &        'The associated MODFLOW lake must be specified as the value of lake_hru_id for this HRU')
! 9002 FORMAT ('####', /, A, /, '1', /, A, /, I10)
 9003 FORMAT (/, 'Percent difference between GVR mapping and active model domain:', D15.7)

      END FUNCTION prms2mfinit

!***********************************************************************
!     prms2mfrun - Maps the PRMS results to MODFLOW cells
! NOTE:  This module assumes that the Sm2gw_grav variable is in inches.
!        It produces cell_drain in MODFLOW units.
!***********************************************************************
      INTEGER FUNCTION prms2mfrun()
      USE GSFPRMS2MF
      USE GSFMODFLOW, ONLY: Gvr2cell_conv, Acre_inches_to_mfl3, &
     &    Inch_to_mfl_t, Gwc_row, Gwc_col, Mft_to_days
      USE GLOBAL, ONLY: IBOUND
      USE GWFUZFMODULE, ONLY: IUZFBND, NWAVST, PETRATE, IGSFLOW, FINF, IUZFOPT
      USE GWFLAKMODULE, ONLY: RNF, EVAPLK, PRCPLK, NLAKES
      USE PRMS_MODULE, ONLY: Nhrucell, Gvr_cell_id, Have_lakes
      USE PRMS_BASIN, ONLY: Active_hrus, Hru_route_order, Hru_type, Hru_area, Lake_area, Lake_hru_id
      USE PRMS_CLIMATEVARS, ONLY: Hru_ppt
      USE PRMS_FLOWVARS, ONLY: Hru_actet
      USE PRMS_SRUNOFF, ONLY: Hortonian_lakes
      USE PRMS_SOILZONE, ONLY: Sm2gw_grav, Lakein_sz, Hrucheck, Gvr_hru_id, Unused_potet, Gvr_hru_pct_adjusted
      IMPLICIT NONE
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: toStream
      EXTERNAL Bin_percolation
! Local Variables
      INTEGER :: irow, icol, ik, jk, ii, ilake
      INTEGER :: j, icell, ihru, is_draining
!***********************************************************************
      prms2mfrun = 0

!-----------------------------------------------------------------------
! Add runoff to stream reaches
!-----------------------------------------------------------------------
      IF ( toStream()/=0 ) RETURN

!-----------------------------------------------------------------------
! Add runoff and precip to lakes
! Pass in hru_actet for the lake
!-----------------------------------------------------------------------
      IF ( Have_lakes==ACTIVE ) THEN
        RNF = 0.0
        PRCPLK = 0.0D0
        EVAPLK = 0.0D0
        DO ii = 1, Active_hrus
          j = Hru_route_order(ii)
          IF ( Hru_type(j)==2 ) THEN
            ilake = Lake_hru_id(j)
            RNF(ilake) = RNF(ilake) + SNGL( (Lakein_sz(j)+Hortonian_lakes(j)) &
     &                   *DBLE(Hru_area(j))*Acre_inches_to_mfl3*Mft_to_days )   !RGN 7/15/2015 added *Mft_to_days
            PRCPLK(ilake) = PRCPLK(ilake) + Hru_ppt(j)*Inch_to_mfl_t*Hru_area(j)
            EVAPLK(ilake) = EVAPLK(ilake) + Hru_actet(j)*Inch_to_mfl_t*Hru_area(j)
          ENDIF
        ENDDO
        DO ilake = 1, NLAKES
          PRCPLK(ilake) = PRCPLK(ilake)/Lake_area(ilake)
          EVAPLK(ilake) = EVAPLK(ilake)/Lake_area(ilake)
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
      PETRATE = 0.0 ! should just be active cells
      Cell_drain_rate = 0.0 ! should just be active cells
      Gw_rejected_grav = Sm2gw_grav ! assume all is rejected to start with
      is_draining = 0

      DO j = 1, Nhrucell
        ihru = Gvr_hru_id(j)
        IF ( Hrucheck(ihru)==0 ) CYCLE ! make sure to skip lake and inactive HRUs
        icell = Gvr_cell_id(j)
!        IF ( icell==0 ) CYCLE ! don't need as icell must be > 0
        irow = Gwc_row(icell)
        icol = Gwc_col(icell)

        IF ( IUZFBND(icol, irow)==0 ) CYCLE
        jk = 0
        ik = 1
        DO WHILE ( jk==0 .AND. ik<Nlayp1 )
          IF ( IBOUND(icol, irow, ik)>0 ) jk = 1
          ik = ik + 1
        ENDDO
        IF ( jk==0 ) CYCLE

!-----------------------------------------------------------------------
! If UZF cell is inactive OR if too many waves then dump water back into
! the soilzone
!-----------------------------------------------------------------------
        IF ( Sm2gw_grav(j)>0.0 ) THEN

          IF ( IUZFOPT==0 ) THEN !ERIC 20210107: NWAVST is dimensioned (1, 1) if IUZFOPT == 0.
            Cell_drain_rate(icell) = Cell_drain_rate(icell) + Sm2gw_grav(j)*Gvr2cell_conv(j)
            Gw_rejected_grav(j) = 0.0
            is_draining = 1            

          ELSEIF ( NWAVST(icol, irow)<NTRAIL_CHK ) THEN
!-----------------------------------------------------------------------
! Convert drainage from inches to MF Length/Time
!-----------------------------------------------------------------------
            Cell_drain_rate(icell) = Cell_drain_rate(icell) + Sm2gw_grav(j)*Gvr2cell_conv(j)
            Gw_rejected_grav(j) = 0.0
            is_draining = 1
          ENDIF
        ENDIF
!-----------------------------------------------------------------------
! Get the remaining potet from the HRU and put it into the cell
! Unused_potet() is in inches
!-----------------------------------------------------------------------
        IF ( Unused_potet(ihru)>NEARZERO ) THEN
          PETRATE(icol, irow) = PETRATE(icol, irow) + Unused_potet(ihru)*Gvr2cell_conv(j)
          Unused_potet(ihru) = Unused_potet(ihru) - Unused_potet(ihru)*SNGL(Gvr_hru_pct_adjusted(j))
          IF ( Unused_potet(ihru)<0.0 ) Unused_potet(ihru) = 0.0
        ENDIF
      ENDDO
 
!-----------------------------------------------------------------------
! Bin precolation in cell_drain_rate
!-----------------------------------------------------------------------
! Set flag for UZF when PRMS sets FINF
      IGSFLOW = 1
      Net_sz2gw = 0.0D0
      Excess = 0.0
      FINF = 0.0
      IF ( is_draining==1 ) CALL Bin_percolation()

      END FUNCTION prms2mfrun

!***********************************************************************
!***********************************************************************
      INTEGER FUNCTION toStream()
      USE GSFPRMS2MF, ONLY: Numreach_segment, Segment_pct_area, Basin_reach_latflow
!     USE GSFPRMS2MF, ONLY: Reach_latflow, Reach_id
      USE GSFMODFLOW, ONLY: Sfr_conv
      USE GWFSFRMODULE, ONLY: STRM
      USE PRMS_MODULE, ONLY: Nsegment
      USE PRMS_SRUNOFF, ONLY: Strm_seg_in
      IMPLICIT NONE
      INTRINSIC SNGL
! Local Variables
      INTEGER :: j, k, iseg
!     INTEGER :: num_rch
      DOUBLE PRECISION :: latflow
!***********************************************************************
      toStream = 1

      Basin_reach_latflow = 0.0D0
      k = 0
      DO iseg = 1, Nsegment
        latflow = Strm_seg_in(iseg)*Segment_pct_area(iseg)
        DO j = 1, Numreach_segment(iseg)
!         k = Reach_id(j, iseg)
          k = k + 1
!-----------------------------------------------------------------------
! Convert inches over the HRU to CFS
!-----------------------------------------------------------------------
!         if (k<1 ) print *, iseg, j, reach_id(iseg,j)
!         print *, iseg, j, reach_id(iseg,j)
!         Reach_latflow(k) = Strm_seg_in(iseg)*Segment_pct_area(iseg)
!         Reach_latflow(k) = latflow
!-----------------------------------------------------------------------
! latflow is in cfs
! convert the gain to the SFR reach to correct units
!-----------------------------------------------------------------------          
          STRM(12, k) = SNGL( latflow*Sfr_conv )
!         Basin_reach_latflow = Basin_reach_latflow + Reach_latflow(k)
          Basin_reach_latflow = Basin_reach_latflow + latflow
        ENDDO
      ENDDO

      toStream = 0

      END FUNCTION toStream

!***********************************************************************
! Bin percolation to reduce waves
!***********************************************************************
      SUBROUTINE Bin_percolation()
      USE GSFPRMS2MF, ONLY: Excess, Cell_drain_rate, Net_sz2gw
      USE GSFMODFLOW, ONLY: Cellarea, Gwc_row, Gwc_col !, Mft_to_days
      USE GWFUZFMODULE, ONLY: FINF, VKS, FBINS, IUZFBND, NUZTOP, SURFDEP
      USE GLOBAL, ONLY: HNEW, BOTM
      USE PRMS_MODULE, ONLY: Ngwcell
      IMPLICIT NONE
      INTRINSIC ABS
! Local Variables
      INTEGER :: icell, irow, icol, ij1, ij2, land, ij2m1
      REAL :: finfvks, finfprms, finf_temp, celtop
!***********************************************************************
!-----------------------------------------------------------------------
! loop on cells
!-----------------------------------------------------------------------
      DO icell = 1, Ngwcell
        IF ( Cell_drain_rate(icell)>0.0 ) THEN
          irow = Gwc_row(icell)
          icol = Gwc_col(icell)
          land = ABS(IUZFBND(icol, irow))
          IF ( NUZTOP==1 ) THEN
            celtop = BOTM(icol, irow, 0) - 0.5 * SURFDEP
          ELSE
            celtop = BOTM(icol, irow, land-1) - 0.5 * SURFDEP
          END IF
!-----------------------------------------------------------------------
! VKS in L/T, FINF in L/T
!-----------------------------------------------------------------------
          finfvks = VKS(icol, irow)
          finfprms = Cell_drain_rate(icell)
          IF ( finfprms>finfvks ) THEN
! reject part, as infiltration exceeds VKS, don't bin
            finf_temp = finfvks
! bin when water table is below celtop
          ELSE IF ( HNEW( icol, irow, land )<celtop ) THEN
!-----------------------------------------------------------------------
! can accept all infiltration, bin to avoid too many waves.
!-----------------------------------------------------------------------
            finf_temp = finfprms
            IF ( finf_temp<FBINS(1) ) THEN
              finf_temp = 0.0       ! reject small values
            ELSE
              IF ( finf_temp<FBINS(25) ) THEN
                ij1 = 25
                ij2 = 1
              ELSE
                ij1 = 51
                ij2 = 25
              ENDIF
              ij2m1 = ij2 - 1
              DO WHILE ( ij1>ij2m1 )
                IF ( finf_temp>=FBINS(ij1) ) THEN
                  finf_temp = FBINS(ij1)  ! set to bin value
                  EXIT
                ENDIF
                ij1 = ij1 - 1
              ENDDO
            ENDIF
          ELSE
            finf_temp = finfprms
          ENDIF
!-----------------------------------------------------------------------
! Put excess >VKS + bin difference in Excess array
!-----------------------------------------------------------------------
          Excess(icell) = (finfprms - finf_temp)
          FINF(icol, irow) = finf_temp
          Net_sz2gw = Net_sz2gw + finf_temp*Cellarea(icell)
        ENDIF
      ENDDO

      END SUBROUTINE Bin_percolation

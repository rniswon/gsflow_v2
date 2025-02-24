C
      MODULE MF_DLL
C
      CONTAINS
C
!***********************************************************************
!     GSFLOW module that replaces MF_NWT.f
!***********************************************************************
 
C     ******************************************************************
C     MAIN CODE FOR U.S. GEOLOGICAL SURVEY MODULAR MODEL -- MODFLOW-NWT
!rgn------REVISION NUMBER CHANGED TO BE CONSISTENT WITH NWT RELEASE
!rgn------NEW VERSION NUMBER 1.2.0, 03/01/2020
!rsr------MODIFIED for use in GSFLOW and MODSIM-GSFLOW
C     ******************************************************************

!***********************************************************************
!     gsfdecl - set up parameters for GSFLOW computations
!***********************************************************************
      INTEGER FUNCTION gsfdecl()
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, DEBUG_less, ACTIVE,
     +                          MODFLOW, MODSIM_MODFLOW
      USE PRMS_MODULE, ONLY: Nhrucell, Ngwcell, Print_debug, GSFLOW_flag
     +                       , githash, Model
      USE GSFMODFLOW
      IMPLICIT NONE
!***********************************************************************
      gsfdecl = 0
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      IF ( Print_debug>DEBUG_minimum )
     &     WRITE (*,1) MFVNAM,VERSION(:16),VERSION2(:17),VERSION3(:17)
    1 FORMAT (/,28X,'MODFLOW',A,/,
     &        '  U.S. GEOLOGICAL SURVEY MODULAR FINITE-DIFFERENCE',
     &        ' GROUNDWATER-FLOW MODEL',/,25X,'WITH NEWTON FORMULATION',
     &        /,25X,'Version ',A,/,14X,'BASED ON MODFLOW-2005 Version ',
     &        A,/,22X,'SWR1 Version ',A,/)

      IF ( GSFLOW_flag==ACTIVE ) THEN
        IF ( Print_debug>DEBUG_less ) WRITE ( *, 8 )
    8   FORMAT (14X, 'PROCESSES: GWF and OBS', /, 14X,
     &        'PACKAGES:  BAS, BCF, CHD, DE4, FHB, GAG, GHB,',
     &        /, 25X, 'HFB, HUF, LAK LPF, MNW1, MNW2, NWT, PCG,',
     &        /, 25X, 'AG, SFR, SIP, UPW, UZF, WEL, SWI, SWT, LMT', /)

      IF ( Model==MODFLOW .OR. Model==MODSIM_MODFLOW )
     &     PRINT *, githash

        ! Allocate local module variables
        ALLOCATE ( Mfq2inch_conv(Nhrucell), Mfvol2inch_conv(Nhrucell) )
        ALLOCATE ( Gvr2cell_conv(Nhrucell), Cellarea(Ngwcell) )
        ALLOCATE ( Gwc_row(Ngwcell), Gwc_col(Ngwcell) )
      ENDIF

      END FUNCTION gsfdecl

!***********************************************************************
!     MFNWT_INIT - Initialize MODFLOW module - get parameter values
!***********************************************************************
      SUBROUTINE MFNWT_INIT(AFR, Diversions, Idivert,EXCHANGE,DELTAVOL,
     &                      LAKEVOL, Nsegshold, Nlakeshold, agDemand) 
!     ------------------------------------------------------------------
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, MODFLOW, MODSIM_MODFLOW,
     &    DEBUG_minimum, DEBUG_less, ERROR_modflow, READ_INIT
      USE PRMS_MODULE, ONLY: Mxsziter, EQULS, Init_vars_from_file,
     &    Kper_mfo, Have_lakes, NLAKES_MF, Ag_package, Model,
     &    GSFLOW_flag, Print_debug, AG_flag
      use prms_utils, only: error_stop, numchars, print_module
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFLAKMODULE, ONLY: NLAKES
      USE GWFUZFMODULE, ONLY: IGSFLOW
      IMPLICIT NONE
      INTEGER :: I
      INCLUDE 'openspec.inc'
! Arguments
      LOGICAL, INTENT(IN) :: AFR
      INTEGER, INTENT(IN) :: Nsegshold
      INTEGER, INTENT(INOUT) :: Idivert(Nsegshold)
      INTEGER, INTENT(INOUT) :: Nlakeshold
      DOUBLE PRECISION, INTENT(INOUT) :: Diversions(Nsegshold)
      DOUBLE PRECISION, INTENT(INOUT) :: EXCHANGE(Nsegshold), 
     &                                   DELTAVOL(Nlakeshold),
     &                                   LAKEVOL(Nlakeshold),
     &                                   agDemand(Nsegshold)
! Functions
      INTRINSIC :: DBLE
      EXTERNAL :: GWF2BAS7AR, USTOP, GWF2BCF7AR, GWF2LPF7AR
      EXTERNAL :: GWF2HUF7AR, GWF2NWT1AR, GWF2UPW1AR, GWF2WEL7AR
      EXTERNAL :: GWF2DRN7AR, GWF2RIV7AR, GWF2EVT7AR, GWF2GHB7AR
      EXTERNAL :: GWF2RCH7AR, GWF2FHB7AR, GWF2RES7AR, GWF2STR7AR
      EXTERNAL :: GWF2IBS7AR, GWF2CHD7AR, GWF2HFB7AR, GWF2SFR7AR
      EXTERNAL :: GWF2GAG7AR, GWF2DRT7AR, GWF2LAK7AR, GWF2ETS7AR
      EXTERNAL :: GWF2SUB7AR, GWF2UZF1AR, SIP7AR, DE47AR, PCG7AR
      EXTERNAL :: GWF2MNW17AR, GWF2SWR7AR
      EXTERNAL :: GWF2HFB7UPW, GWF2MNW27AR, GWF2MNW2I7AR, GWF2SWT7AR
      EXTERNAL :: GWF2SWI2AR, GWF2AG7AR
      EXTERNAL :: GWF2HYD7BAS7AR, GWF2HYD7IBS7AR, GWF2HYD7SUB7AR
      EXTERNAL :: GWF2HYD7STR7AR, GWF2HYD7SFR7AR, LMT8BAS7AR
      EXTERNAL :: OBS2BAS7AR, OBS2DRN7AR, OBS2RIV7AR, OBS2GHB7AR
      EXTERNAL :: OBS2STR7AR, OBS2CHD7AR
! Local Variables
      INTEGER :: MAXUNIT, NC
C
      CHARACTER*80 HEADNG(2)
      CHARACTER*200 FNAME
      CHARACTER*4 solver
C
      CHARACTER*4 CUNIT(NIUNIT)
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'gfd ', 'GHB ',  !  7
     &           'RCH ', 'SIP ', 'DE4 ', '    ', 'OC  ', 'PCG ', 'lmg ',  ! 14
     &           'gwt ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6',  ! 21
     &           'LAK ', 'LPF ', 'DIS ', '    ', 'PVAL', '    ', 'HOB ',  ! 28
     &           '    ', '    ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB',  ! 35
     &           'STOB', 'HUF2', 'CHOB', 'ETS ', 'DRT ', '    ', 'gmg ',  ! 42
     &           'HYD ', 'SFR ', '    ', 'GAGE', 'LVDA', '    ', 'LMT6',  ! 49
     &           'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'gwm ',  ! 56
     &           'SWT ', 'cfp ', 'pcgn', '    ', 'fmp ', 'UPW ', 'NWT ',  ! 63
     &           'SWR ', 'SWI2', 'AG  ', '    ', 'IWRT', 'IRED', '    ',  ! 70     - SWR - JDH
     &           30*'    '/                                               ! 71-100 - SWR - JDH
C     ------------------------------------------------------------------
C
C2------WRITE BANNER TO SCREEN AND DEFINE CONSTANTS.
      IOUTS = 432
      IGRID=1
      NSOL=1
      Stopcount = 0
      INUNIT = 200
      NCVGERR=0
      ICNVG=1
      AGCONVERGE=1
C
C3------GET THE NAME OF THE NAME FILE
      CALL GETNAMFIL(FNAME)
      MAXUNIT= INUNIT
C
C4------OPEN NAME FILE.
      NC = numchars(FNAME)
      OPEN (UNIT=INUNIT,FILE=FNAME(1:NC),STATUS='OLD',ACTION=ACTION(1))
  490 FORMAT(A,A)
C
C5------Get current date and time, assign to IBDT, and write to screen
      CALL DATE_AND_TIME(VALUES=IBDT)
      IF ( Model==MODFLOW .OR. Model==MODSIM_MODFLOW )
     &     WRITE(*,2) (IBDT(I),I=1,3),(IBDT(I),I=5,7)
    2 FORMAT(' Run start date and time (yyyy/mm/dd hh:mm:ss): ',
     &       I0,'/',I2.2,'/',I2.2,I3,2(':',I2.2),/)
C
C6------ALLOCATE AND READ (AR) PROCEDURE
      CALL GWF2BAS7AR(INUNIT,CUNIT,VERSION,24,31,32,MAXUNIT,IGRID,12,
     1                HEADNG,26,MFVNAM)
      IF(IUNIT(50).GT.0 .AND. IUNIT(52).GT.0) THEN
        WRITE(IOUT,'(1X,/,1X,A)')
     1  'MNW1 and MNW2 cannot both be active in the same simulation'
        CALL USTOP(' ')
      END IF

      ierr = 0
! Packages not available in NWT or GSFLOW, ??? what about MODSIM-GSFLOW ???
      IF ( IUNIT(14)>0 ) THEN
        PRINT *, 'lmg Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(6)>0 ) THEN
        PRINT *, 'gfd Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(15)>0 ) THEN
        PRINT *, 'gwt Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(42)>0 ) THEN
        PRINT *, 'GMG Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(56)>0 ) THEN
        PRINT *, 'gwm Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(58)>0 ) THEN
        PRINT *, 'cfp Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(59)>0 ) THEN
        PRINT *, 'PCGN Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(61)>0 ) THEN
        PRINT *, 'FMP Package not supported'
        ierr = 1
      ENDIF
      IF ( IUNIT(55)==0 .AND. GSFLOW_flag==ACTIVE ) THEN
        PRINT *, 'GSFLOW requires UZF Package'
        ierr = 1
      ENDIF
      IF ( IUNIT(66)==0 .AND. AG_flag == ACTIVE ) THEN
      !  PRINT *, 'GSFLOW with PRMS agriculture requires the AG Package'
         PRINT *, 'WARNING, soilzone_ag active and AG Package inactive'
      !  ierr = 1
      ENDIF

! Packages available in NWT but not in GSFLOW
      IF ( GSFLOW_flag==ACTIVE ) THEN
        IF ( IUNIT(3)>0 ) THEN
          PRINT *, 'DRN Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(4)>0 ) THEN
          PRINT *, 'RIV Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(5)>0 ) THEN
          PRINT *, 'EVT Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(8)>0 ) THEN
          PRINT *, 'RCH Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(17)>0 ) THEN
          PRINT *, 'RES Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(18)>0 ) THEN
          PRINT *, 'STR Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(19)>0 ) THEN
          PRINT *, 'IBS Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(33)>0 ) THEN
          PRINT *, 'OBS DRN Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(34)>0 ) THEN
          PRINT *, 'OBS RIV Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(36)>0 ) THEN
          PRINT *, 'OBS STR Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(39)>0 ) THEN
          PRINT *, 'ETS Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(40)>0 ) THEN
          PRINT *, 'DRT Package not supported'
          ierr = 1
        ENDIF
        IF ( IUNIT(43)>0 ) THEN
          PRINT *, 'HYD Package not supported'
          ierr = 1
        ENDIF
!        IF ( IUNIT(49)>0 ) THEN
!          PRINT *, 'LMT Package not supported'
!          ierr = 1
!        ENDIF
        IF ( IUNIT(54)>0 ) THEN
          PRINT *, 'SUB Package not supported'
          ierr = 1
        ENDIF
!        IF ( IUNIT(57)>0 ) THEN
!          PRINT *, 'SWT Package not supported'
!          ierr = 1
!        ENDIF
        IF ( IUNIT(64)>0 ) THEN
          PRINT *, 'SWR Package not supported'
          ierr = 1
        ENDIF
!        IF ( IUNIT(65)>0 ) THEN
!          PRINT *, 'SWI Package not supported'
!          ierr = 1
!        ENDIF
      ENDIF
      IF ( ierr==1 ) CALL error_stop('INVALID PACKAGE SELECTION',
     &                               ERROR_modflow)

      Have_lakes = OFF
      IF ( IUNIT(22).GT.0 ) Have_lakes = ACTIVE
      IF(IUNIT(1).GT.0) CALL GWF2BCF7AR(IUNIT(1),IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7AR(IUNIT(23),IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7AR(IUNIT(37),IUNIT(47),
     1                                     IUNIT(53),IGRID)
! Allocate arrays for Newton Solver
      IF(IUNIT(63).GT.0) CALL GWF2NWT1AR(IUNIT(63),MXITER,
     1                                   IUNIT(22),IGRID)
      IF(IUNIT(62).GT.0) CALL GWF2UPW1AR(IUNIT(62), Igrid)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7AR(IUNIT(2),IUNIT(63),IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7AR(IUNIT(3),IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7AR(IUNIT(4),IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7AR(IUNIT(5),IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7AR(IUNIT(7),IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7AR(IUNIT(8),IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7AR(IUNIT(16),IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7AR(IUNIT(17),IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7AR(IUNIT(18),IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7AR(IUNIT(19),IUNIT(54),IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7AR(IUNIT(20),IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7AR(IUNIT(21),IGRID)
! Modify conductance for HFB when using UPW.
      IF ( IUNIT(62).GT.0 ) THEN
        IF(IUNIT(21).GT.0) CALL GWF2HFB7UPW(IGRID)
      END IF
      IF(IUNIT(44).GT.0) CALL GWF2SFR7AR(IUNIT(44),IUNIT(1),IUNIT(23),
     1                           IUNIT(37),IUNIT(15),NSOL,IOUTS,
     2                           IUNIT(62),IUNIT(55),IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1AR(IUNIT(55),IUNIT(1),
     1                                   IUNIT(23),IUNIT(37),
     2                                   IUNIT(63),IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0) THEN
          CALL GWF2LAK7AR(
     1             IUNIT(22),IUNIT(44),IUNIT(15),IUNIT(55),NSOL,IGRID)
          Nlakeshold = NLAKES
          NLAKES_MF = NLAKES
      END IF
      IF(IUNIT(46).GT.0) CALL GWF2GAG7AR(IUNIT(46),IUNIT(44),
     1                                     IUNIT(22),IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7AR(IUNIT(39),IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7AR(IUNIT(40),IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7AR(IUNIT(54),IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7AR(IUNIT(9),MXITER,IGRID)
      IF(IUNIT(10).GT.0) CALL DE47AR(IUNIT(10),MXITER,IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7AR(IUNIT(13),MXITER,IGRID)
c      IF(IUNIT(14).GT.0) CALL LMG7AR(IUNIT(14),MXITER,IGRID)
!      IF(IUNIT(42).GT.0) CALL GMG7AR(IUNIT(42),MXITER,IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27AR(IUNIT(50),IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7AR(IUNIT(51),IUNIT(50),IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17AR(IUNIT(52),IUNIT(9),
     1                     IUNIT(10),IUNIT(63),0,IUNIT(13),
     2                     0,IUNIT(42),FNAME,IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7AR(IUNIT(57),IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7AR(IUNIT(64),
     2                        IUNIT(1),IUNIT(23),IUNIT(37),
     3                        IUNIT(62),IUNIT(44),IUNIT(63),IGRID)  !SWR  - JDH
      IF(IUNIT(65).GT.0) CALL GWF2SWI2AR(IUNIT(65),
     2                        IUNIT(1),IUNIT(23),IUNIT(37),IUNIT(62),
     3                        IGRID)   !SWI2  - JDH
!     IF(IUNIT(67).GT.0) CALL GWF2GFB7AR(IUNIT(67),IGRID)
      IF(IUNIT(43).GT.0) THEN
        CALL GWF2HYD7BAS7AR(IUNIT(43),IGRID)
        IF(IUNIT(19).GT.0) CALL GWF2HYD7IBS7AR(IUNIT(43),IGRID)
        IF(IUNIT(54).GT.0) CALL GWF2HYD7SUB7AR(IUNIT(43),IGRID)
        IF(IUNIT(18).GT.0) CALL GWF2HYD7STR7AR(IUNIT(43),IGRID)
        IF(IUNIT(44).GT.0) CALL GWF2HYD7SFR7AR(IUNIT(43),IGRID)
      ENDIF
      IF(IUNIT(49).GT.0) CALL LMT8BAS7AR(INUNIT,CUNIT,IGRID)
      Ag_package = OFF
      IF(IUNIT(66).GT.0) THEN
        IF ( GSFLOW_flag==ACTIVE ) Ag_package = ACTIVE
        CALL GWF2AG7AR(IUNIT(66),IUNIT(44),IUNIT(63))
      ENDIF
!      IF(IUNIT(61).GT.0) THEN
!        CALL FMP2AR(
!     1  IUNIT(61),IUNIT(44),IUNIT(52),IUNIT(55),IGRID)                  !FMP2AR CALL ADDED BY SCHMID
!        CALL FMP2RQ(IUNIT(61),IUNIT(44),IUNIT(52),IGRID)                !FMP2RQ CALL ADDED BY SCHMID
!      ENDIF
C
C7------SIMULATE EACH STRESS PERIOD.
      CALL print_module(MODDESC, MODNAME, Version_gsflow_modflow)
      IF ( Print_debug>DEBUG_minimum )
     &     PRINT '(A,/A,/A)', EQULS(:62), 'MODFLOW Packages', EQULS(:62)
      CALL print_module(MODDESC_UZF, MODNAME_UZF, Version_uzf)
      CALL print_module(MODDESC_SFR, MODNAME_SFR, Version_sfr)
      IF ( Have_lakes==ACTIVE )
     &     CALL print_module(MODDESC_LAK, MODNAME_LAK, Version_lak)
      IF ( Ag_package==ACTIVE )
     &     CALL print_module(MODDESC_AG, MODNAME_AG, Version_ag)
      IF ( Print_debug>DEBUG_minimum ) PRINT '(A,/)', EQULS(:62)

      IF ( IUNIT(63)>0 ) solver = 'NWT'
      IF ( IUNIT(13)>0 ) solver = 'PCG'
      IF ( IUNIT(9)>0 ) solver = 'SIP'
      IF ( IUNIT(10)>0 ) solver = 'DE47'
!      IF ( IUNIT(42)>0 ) solver = 'GMG'
      IF ( Print_debug>DEBUG_less ) THEN
        PRINT '(A,/)', EQULS(:62)
        WRITE(*,490)'Using NAME file: ', FNAME(1:NC)
        PRINT 14, solver
      ENDIF
   14 FORMAT (/, 'Using Solver Package: ', A)
      Sziters = 0
      Convfail_cnt = 0
      Max_iters = 0
      Max_sziters = 0
      Iterations = 0
      Mfiter_cnt = 0
      Iter_cnt = 0
      CALL SETMFTIME()
      IF ( GSFLOW_flag==ACTIVE ) THEN
        IF(IUNIT(55).GT.0) IGSFLOW = 1
        CALL set_cell_values()
        IF ( Init_vars_from_file>0 )
     &       CALL gsflow_modflow_restart(READ_INIT)
        CALL check_gvr_cell_pct()
        ! make the default number of soilzone iterations equal to the
        ! maximum MF iterations, which is a good practice using NWT and cells=nhru
        IF ( Mxsziter<1 ) THEN
          print '(/,A)',
     &          'WARNING, mxsziter not specified, substituting MXITER'
          Mxsziter = MXITER
        ENDIF
      ENDIF
C
C  Observation allocate and read    rgn 5/4/2018 MOVED IN ORDER TO SKIP STEPS FOR OBS PACKAGES
      CALL OBS2BAS7AR(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7AR(IUNIT(33),IUNIT(3),IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7AR(IUNIT(34),IUNIT(4),IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7AR(IUNIT(35),IUNIT(7),IGRID)
      IF(IUNIT(36).GT.0) CALL OBS2STR7AR(IUNIT(36),IUNIT(18),IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7AR(IUNIT(38),IGRID)
! Modify conductance for HFB when using UPW.
      !IF ( IUNIT(62).GT.0 ) THEN
      !  IF(IUNIT(21).GT.0) CALL GWF2HFB7UPW(IGRID)
      !END IF
C
      KSTP = 0
      KPER = 1
      KPERSTART = 1
      ! run SS if needed, read to current stress period, read restart if needed
      CALL SET_STRESS_DATES(AFR, Diversions, Idivert, EXCHANGE,DELTAVOL,
     +                      LAKEVOL,Nsegshold, Nlakeshold, agDemand)
      Delt_save = DELT
      IF ( ISSFLG(1).EQ.0 ) Delt_save = 1.0/Mft_to_days
      CALL SETCONVFACTORS()
C
      KKPER = KPER
      IF ( Model==MODFLOW .OR. Model==MODSIM_MODFLOW ) THEN
        Kkper_new = GET_KPER()
        Kper_mfo = Kkper_new
      ENDIF
C
      END SUBROUTINE MFNWT_INIT

!***********************************************************************
!     MFNWT_RUN = ADVANCE TIME AND RUN THE MODFLOW SOLVER ROUTINE WITH
!                 THE LATEST VALUES OF ISEG UPDATED BY MODSIM.
!***********************************************************************
      SUBROUTINE MFNWT_RUN(AFR, Diversions, Idivert, EXCHANGE, 
     &                     DELTAVOL,LAKEVOL,Nsegshold, Nlakeshold, 
     &                     agDemand)
C
      !DEC$ ATTRIBUTES DLLEXPORT :: MFNWT_RUN
C
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, MODFLOW, ACTIVE, OFF,
     &    ERROR_time, ERROR_modflow, MODSIM_GSFLOW, GSFLOW, CANOPY,
     &    MODSIM_MODFLOW
      USE PRMS_MODULE, ONLY: Kper_mfo, Kkiter, Timestep, snow_flag,
     &    Init_vars_from_file, Mxsziter, Glacier_flag, AG_flag,
     &    PRMS_land_iteration_flag, activeHRU_inactiveCELL_flag,
     &    Model, GSFLOW_flag, Print_debug, MODSIM_flag
      use prms_utils, only: error_stop
C1------USE package modules.
      USE GLOBAL
      USE GWFBASMODULE
!      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE PCGMODULE
c     USE LMGMODULE
      USE SIPMODULE
      USE DE4MODULE
!gsf  USE GMGMODULE
!      USE GWFNWTMODULE, ONLY:ITREAL, ICNVGFLG  !ITREAL removed from NWT module and added to PRMS_MODULE
!      USE GWFNWTMODULE, ONLY:ICNVGFLG
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Nsegshold, Nlakeshold
      LOGICAL, INTENT(IN) :: AFR
      DOUBLE PRECISION, INTENT(INOUT) :: Diversions(Nsegshold),
     &                                   EXCHANGE(Nsegshold)
      DOUBLE PRECISION, INTENT(INOUT) :: DELTAVOL(Nlakeshold),
     &                                   LAKEVOL(Nlakeshold),
     &                                   agDemand(Nsegshold)
      INTEGER, INTENT(INOUT) :: Idivert(Nsegshold)
      INCLUDE 'openspec.inc'
! FUNCTIONS AND SUBROUTINES
      INTEGER, EXTERNAL :: soilzone, soilzone_ag
      INTEGER, EXTERNAL :: srunoff, intcp, snowcomp, glacr
      INTEGER, EXTERNAL :: gsflow_prms2mf, gsflow_mf2prms
      EXTERNAL :: MODSIM2SFR, SFR2MODSIM, LAK2MODSIM
      EXTERNAL :: gwflow_inactive_cell
      EXTERNAL :: GWF2UPWUPDATE, GWF2BAS7AD, GWF2UPW1AD, GWF2CHD7AD
      EXTERNAL :: GWF2BCF7AD, GWF2RES7AD, GWF2LPF7AD, GWF2HUF7AD
      EXTERNAL :: GWF2FHB7AD, GWF2LAK7AD, GWF2UZF1AD, GWF2SWI2AD
      EXTERNAL :: GWF2MNW27BCF, GWF2MNW27LPF, GWF2MNW27HUF, GWF2MNW27UPW
      EXTERNAL :: GWF2MNW27AD, GWF2MNW17AD, USTOP, UMESPR, GWF2SFR7AD
      EXTERNAL :: GWF2SWR7AD, GWF2AG7AD, GWF2ETS7FM, GWF2DRT7FM
      EXTERNAL :: GWF2BAS7FM, GWF2BCF7FM, GWF2UPWFMS, GWF2LPF7FM
      EXTERNAL :: GWF2HUF7FM, GWF2HFB7FM, GWF2WEL7FM, GWF2DRN7FM
      EXTERNAL :: GWF2RIV7FM, GWF2LAK7ST, GWF2EVT7FM, MODSIM2AG
      EXTERNAL :: GWF2GHB7FM, GWF2RCH7FM, SIP7PNT, SIP7AP
      EXTERNAL :: GWF2FHB7FM, GWF2RES7FM, GWF2STR7FM, GWF2IBS7FM
      EXTERNAL :: GWF2LAK7FM, GWF2UZF1FM, GWF2AG7FM, GWF2SFR7FM
      EXTERNAL :: GWF2MNW27FM, GWF2MNW17FM, GWF2SUB7FM, GWF2SWT7FM
      EXTERNAL :: DE47PNT, DE47AP, PCG7PNT, PCG7AP, GWF2LPF7BDADJ
      EXTERNAL :: GWF2SWR7FM, GWF2SWI2FM, GWF2NWT1FM, GWF2SWR7CV
      EXTERNAL :: AG2MODSIM, GWF2BAS7OC, GWF2BCF7BDCH
      EXTERNAL :: GWF2BCF7BDADJ, GWF2LPF7BDS, GWF2LPF7BDCH
      INTRINSIC :: MIN
! Local Variables
      INTEGER :: retval, KITER, iss, iprt, I !, II, IBDRET
!      INTEGER :: IC1, IC2, IR1, IR2, IL1, IL2, IDIR
!      REAL :: BUDPERC
!***********************************************************************
!     Model (0=GSFLOW; 2=MODFLOW; 10=MODSIM-GSFLOW; 11=MODSIM-MODFLOW)
C
C7------SIMULATE EACH STRESS PERIOD.
      IF ( Steady_state.EQ.1 ) THEN
        Kkper_new = 1
        Kper_mfo = 2
      ELSEIF ( GSFLOW_flag==ACTIVE ) THEN
        Kkper_new = GET_KPER()
      ELSE
        Kkper_new = Kper_mfo
      ENDIF

      IF ( Kkper_new.NE.KKPER ) THEN
        KPER = Kkper_new
        KKPER = Kkper_new
        IF ( Init_vars_from_file>0 ) THEN
          IF ( KPER>Modflow_skip_stress+1 ) KSTP = 0
        ELSE
          KSTP = 0
        END IF
        CALL MFNWT_RDSTRESS() ! second time in run, read restart
        IF ( GSFLOW_flag==ACTIVE ) THEN    !RGN added check for MF only mode 2/21/19
          IF ( ISSFLG(KKPER).EQ.1 ) CALL error_stop
     &         ('cannot run steady state after first stress period.',
     &          ERROR_modflow)
!          IF ( ISSFLG(1).EQ.0 ) Delt_save = DELT ! rsr, delt_save set in MFNWT_INIT
          IF ( DELT.NE.Delt_save )
     &         CALL error_stop('cannot change DELT', ERROR_time)
        END IF
      ENDIF
      iss = ISSFLG(KKPER)
      gsflag = OFF
      IF ( GSFLOW_flag==ACTIVE .AND. iss==0 ) gsflag = ACTIVE
C
C7C-----SIMULATE EACH TIME STEP.
!gsf    DO 90 KSTP = 1, NSTP(KPER) ! maybe a problem, need loop for MFNWT and probably MODSIM
          IF(AFR) KSTP = KSTP + 1
          KKSTP = KSTP
          IF ( IUNIT(63).GT.0 )itreal = 0
C
C7C1----CALCULATE TIME STEP LENGTH. SET HOLD=HNEW.
          IF (AFR) THEN
            IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,Igrid)
            CALL GWF2BAS7AD(KKPER,KKSTP,IGRID)
            IF(IUNIT(62).GT.0) CALL GWF2UPW1AD(IGRID)
            IF(IUNIT(20).GT.0) CALL GWF2CHD7AD(KKPER,IGRID)
            IF(IUNIT(1).GT.0) CALL GWF2BCF7AD(KKPER,IGRID)
            IF(IUNIT(17).GT.0) CALL GWF2RES7AD(KKSTP,KKPER,IGRID)
            IF(IUNIT(23).GT.0) CALL GWF2LPF7AD(KKPER,IGRID)
            IF(IUNIT(37).GT.0) CALL GWF2HUF7AD(KKPER,IGRID)
            IF(IUNIT(16).GT.0) CALL GWF2FHB7AD(IGRID)
            IF(IUNIT(22).GT.0) CALL GWF2LAK7AD(KKPER,KKSTP,IUNIT(15),
     1                                             IGRID)
            IF(IUNIT(55).GT.0) CALL GWF2UZF1AD(IUNIT(55), KKPER, KKSTP,
     1                                         Igrid)
            IF(IUNIT(65).GT.0) CALL GWF2SWI2AD(KKSTP,KKPER,IGRID)  !SWI2
            IF( IUNIT(44).GT.0 ) CALL GWF2SFR7AD(IUNIT(44),IUNIT(22),
     1                                           KKSTP,KKPER,IGRID)
          !END IF  !moving this down below all AD routines
          IF(IUNIT(50).GT.0) THEN
            IF (IUNIT(1).GT.0) THEN
              CALL GWF2MNW27BCF(KPER,IGRID)
            ELSE IF (IUNIT(23).GT.0) THEN
              CALL GWF2MNW27LPF(KPER,IGRID)
            ELSE IF(IUNIT(37).GT.0) THEN
              CALL GWF2MNW27HUF(KPER,IGRID)
            ELSE IF(IUNIT(62).GT.0) THEN
              CALL GWF2MNW27UPW(KPER,IGRID)
            ELSE
              WRITE(IOUT,1000)
 1000         FORMAT(/1X,
     &      '***ERROR: MNW2 PACKAGE DOES NOT SUPPORT',/,
     &      ' SELECTED FLOW PACKAGE',/,
     &  ' (MNW2 DOES FULLY SUPPORT BCF, LPF, HUF, AND UPW PACKAGES)',/,
     &      ' -- STOP EXECUTION')
              CALL USTOP('MNW2 error-flow package')
            END IF
            CALL GWF2MNW27AD(KKSTP,KKPER,IUNIT(62),IGRID)
          END IF
          IF(IUNIT(52).GT.0) CALL GWF2MNW17AD(IUNIT(1),IUNIT(23),
     1                                  IUNIT(37),IUNIT(62),IGRID)
!          IF(IUNIT(61).GT.0) THEN                                       !FMP2AD CALL ADDED BY SCHMID
!             IF(IRTFL.EQ.3.OR.ICUFL.EQ.3.OR.IPFL.EQ.3
!     1                           .OR.IEBFL.EQ.1.OR.IEBFL.EQ.3)
!     2       CALL FMP2AD(ISTARTFL,KKPER,IGRID)
!          ENDIF
          IF(IUNIT(64).GT.0) CALL GWF2SWR7AD(KKPER,KKSTP,
     2                                       IGRID,IUNIT(54))  !SWR - JDH
          IF(IUNIT(66).GT.0) CALL GWF2AG7AD(IUNIT(66),KKPER)

          IF ( Model==MODFLOW .or. Model==MODSIM_MODFLOW ) THEN
C
C---------INDICATE IN PRINTOUT THAT SOLUTION IS FOR HEADS
            iprt = 0
            CALL UMESPR('SOLVING FOR HEAD',' ',IOUT)
            IF ( Print_debug>DEBUG_less ) THEN
              IF ( iprt==0 ) THEN
                WRITE(*,25)KPER,KSTP
              ELSE
                WRITE (*, 26) KPER, KSTP
              ENDIF
            ENDIF
   25     FORMAT(' Solving:  Stress period: ',i5,4x,
     &       'Time step:',I6,4x,'Groundwater-Flow Eqn.')
   26     FORMAT('Skipping:  Stress period: ',i5,4x,
     &       'Time step:',I6)
          ENDIF
        END IF   ! moved this AFR check down here 8/30/2022
C
C7C2----ITERATIVELY FORMULATE AND SOLVE THE FLOW EQUATIONS.
          Szcheck = OFF
          IF ( gsflag==ACTIVE ) Szcheck = ACTIVE
!          DO 30 KITER = 1, MXITER
           KITER = 0
           ITREAL2 = 0
!           IF ( IUNIT(63).GT.0 ) ITREAL = 0
           ITREAL = 0
C
C0----Plug in MODSIM values before PRMS-MODFLOW iterations
           IF ( MODSIM_flag==1 ) THEN
             IF(IUNIT(44).GT.0.AND.iss==0) CALL MODSIM2SFR(Diversions)
           ENDIF
C
           DO WHILE (ITREAL2.LT.MXITER)
            KITER = KITER + 1
            KKITER = KITER
            IF ( IUNIT(63).EQ.0 ) THEN
                ITREAL2 = KITER
                ITREAL = KITER
            END IF
            IF(IUNIT(62).GT.0) CALL GWF2UPWUPDATE(2,Igrid)

C
C7C2A---FORMULATE THE FINITE DIFFERENCE EQUATIONS.
            CALL GWF2BAS7FM(IGRID)
            IF(IUNIT(1).GT.0) CALL GWF2BCF7FM(KKITER,KKSTP,
     1                               KKPER,IGRID)
            IF(IUNIT(62).GT.0) CALL GWF2UPWFMS(KKITER,KKSTP,KKPER,IGRID)
            IF(IUNIT(23).GT.0) CALL GWF2LPF7FM(KKITER,KKSTP,KKPER,IGRID)
            IF(IUNIT(37).GT.0) CALL GWF2HUF7FM(KKITER,
     1                             KKSTP,KKPER,IUNIT(47),IGRID)
            IF ( IUNIT(62).EQ.0 ) THEN
              IF(IUNIT(21).GT.0) CALL GWF2HFB7FM(IGRID)
            END IF
            IF(IUNIT(2).GT.0) CALL GWF2WEL7FM(IUNIT(63),IGRID)
            IF(IUNIT(3).GT.0) CALL GWF2DRN7FM(IGRID)
            IF(IUNIT(4).GT.0) CALL GWF2RIV7FM(IGRID)
            IF(IUNIT(5).GT.0) THEN
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
              CALL GWF2EVT7FM(IGRID)
              IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(7).GT.0) CALL GWF2GHB7FM(IGRID)
            IF(IUNIT(8).GT.0) THEN
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    0,IGRID)
               CALL GWF2RCH7FM(IGRID)
               IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                    1,IGRID)
            END IF
            IF(IUNIT(16).GT.0) CALL GWF2FHB7FM(IGRID)
            IF(IUNIT(17).GT.0) CALL GWF2RES7FM(IGRID)
            IF(IUNIT(18).GT.0) CALL GWF2STR7FM(IGRID)
            IF(IUNIT(19).GT.0) CALL GWF2IBS7FM(KKPER,IGRID)
            IF(IUNIT(39).GT.0) CALL GWF2ETS7FM(IGRID)
            IF(IUNIT(40).GT.0) CALL GWF2DRT7FM(IGRID)
!            IF(IUNIT(61).GT.0) CALL FMP2FM(KKITER,KKPER,KKSTP,ISTARTFL, !FMP2FM CALL ADDED BY SCHMID
!     1                              IUNIT(44),IUNIT(52),IUNIT(55),IGRID)
            IF (MODSIM_flag==1 .AND. iss==0) THEN
              IF( IUNIT(66).GT.0 ) CALL MODSIM2AG(Diversions)
            END IF

!  Call the PRMS modules that need to be inside the iteration loop
!            IF ( Szcheck==ACTIVE ) THEN
            IF ( (Szcheck==ACTIVE .AND. Model==GSFLOW) .OR.
     1           (Szcheck==ACTIVE .AND. kkiter==1 .AND.
     2            Model==MODSIM_GSFLOW) ) THEN
              IF ( PRMS_land_iteration_flag==CANOPY ) THEN
                retval = intcp()
                IF ( snow_flag==ACTIVE ) THEN
                  retval = snowcomp()
                  IF ( Glacier_flag==ACTIVE ) THEN
                    retval = glacr()
                  ENDIF
                ENDIF
              ENDIF
              IF ( PRMS_land_iteration_flag>0 ) retval = srunoff()
              IF ( AG_flag==ACTIVE ) THEN
                retval = soilzone_ag()
              ELSE
                retval = soilzone()
              ENDIF
              IF ( activeHRU_inactiveCELL_flag == ACTIVE )
     &             CALL gwflow_inactive_cell()
              retval = gsflow_prms2mf()
              Sziters = Sziters + 1
              Maxgziter = KKITER
              IF ( KKITER==Mxsziter ) Szcheck = OFF ! stop calling PRMS in iteration loop
            ELSEIF ( iss==0 ) THEN
              IF ( KKITER==Mxsziter+1 ) Stopcount = Stopcount + 1
            ENDIF
            IF(IUNIT(22).GT.0) CALL GWF2LAK7FM(KKITER,KKPER,KKSTP,
     1                                     IUNIT(44),IUNIT(55),IGRID) 
            IF(IUNIT(55).GT.0) CALL GWF2UZF1FM(KKPER,KKSTP,KKITER,
     1                           IUNIT(44),IUNIT(22),IUNIT(63),
     2                           IUNIT(64),IGRID)  
            !IF (MODSIM_flag==1 .AND. iss==0) THEN
            !  IF( IUNIT(66).GT.0 ) CALL MODSIM2AG(Diversions)
            !END IF
            IF(IUNIT(66).GT.0 .AND. Model == MODSIM_GSFLOW )
     +         CALL GWF2AG7FM(Kkper, Kkstp, Kkiter,IUNIT(63),AGCONVERGE)
            IF(IUNIT(44).GT.0) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,
     1                              IUNIT(22),IUNIT(63),IUNIT(8),
     2                              IUNIT(55),IGRID)
            IF(IUNIT(66).GT.0 .AND. Model /= MODSIM_GSFLOW )
     +         CALL GWF2AG7FM(Kkper, Kkstp, Kkiter,IUNIT(63),AGCONVERGE)
     !!       IF(IUNIT(44).GT.0) CALL GWF2SFR7FM(KKITER,KKPER,KKSTP,
     !!1                              IUNIT(22),IUNIT(63),IUNIT(8),
     !!2                              IUNIT(55),IGRID)
            IF(IUNIT(50).GT.0) THEN
              IF (IUNIT(1).GT.0) THEN
                CALL GWF2MNW27BCF(KPER,IGRID)
              ELSE IF (IUNIT(23).GT.0) THEN
                CALL GWF2MNW27LPF(KPER,IGRID)
              ELSE IF(IUNIT(37).GT.0) THEN
                CALL GWF2MNW27HUF(KPER,IGRID)
              ELSE IF(IUNIT(62).GT.0) THEN
                CALL GWF2MNW27UPW(KPER,IGRID)
              END IF
              CALL GWF2MNW27FM(KKITER,IUNIT(62),kkstp,kkper,IGRID)
            END IF
            IF(IUNIT(52).GT.0) CALL GWF2MNW17FM(KKITER,IUNIT(1),
     1                               IUNIT(23),IUNIT(37),IUNIT(62),
     2                               IGRID)
            IF(IUNIT(54).GT.0) CALL GWF2SUB7FM(KKPER,KKITER,
     1                                         IUNIT(9),IGRID)
            IF(IUNIT(57).GT.0) CALL GWF2SWT7FM(KKPER,IGRID)
            IF(IUNIT(64).GT.0) CALL GWF2SWR7FM(KKITER,KKPER,KKSTP,IGRID)  !SWR - JDH
!            IF(IUNIT(67).GT.0) CALL GWF2GFB7FM(IGRID)
C-------------SWI2 FORMULATE (GWF2SWI2FM) NEEDS TO BE THE LAST PACKAGE
C             ENTRY SINCE SWI2 SAVES THE RHS (RHSFRESH) PRIOR TO ADDING SWI TERMS
C             RHSFRESH IS USED TO CALCULATE BOUNDARY CONDITION FLUXES
            IF(IUNIT(65).GT.0) CALL GWF2SWI2FM(KKSTP,KKPER,KKITER,IGRID)  !SWI2 - JDH
C
C7C2B---MAKE ONE CUT AT AN APPROXIMATE SOLUTION.
            IERR=0
            IF (IUNIT(9).GT.0) THEN
                   CALL SIP7PNT(IGRID)
                   CALL SIP7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,EL,FL,GL,
     1               V,W,HDCG,LRCH,NPARM,KKITER,HCLOSE,ACCL,ICNVG,
     2               KKSTP,KKPER,IPCALC,IPRSIP,MXITER,NSTP(KKPER),
     3               NCOL,NROW,NLAY,NODES,IOUT,0,IERR)
            END IF
            IF (IUNIT(10).GT.0) THEN
                   CALL DE47PNT(IGRID)
                   CALL DE47AP(HNEW,IBOUND,AU,AL,IUPPNT,IEQPNT,D4B,MXUP,
     1               MXLOW,MXEQ,MXBW,CR,CC,CV,HCOF,RHS,ACCLDE4,KITER,
     2               ITMX,MXITER,NITERDE4,HCLOSEDE4,IPRD4,ICNVG,NCOL,
     3               NROW,NLAY,IOUT,LRCHDE4,HDCGDE4,IFREQ,KKSTP,KKPER,
     4               DELT,NSTP(KKPER),ID4DIR,ID4DIM,MUTD4,
     5               DELTL,NBWL,NUPL,NLOWL,NLOW,NEQ,NUP,NBW,IERR)
            END IF
            IF (IUNIT(13).GT.0) THEN
!rgn increase damping factor for transient solution.
                   IF ( iss.EQ.0 ) DAMPPCG = DAMPPCGT
                   CALL PCG7PNT(IGRID)
                   CALL PCG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,VPCG,SS,
     1               P,CD,HCHG,LHCH,RCHG,LRCHPCG,KKITER,NITER,
     2               HCLOSEPCG,RCLOSEPCG,ICNVG,KKSTP,KKPER,IPRPCG,
     3               MXITER,ITER1,NPCOND,NBPOL,NSTP(KKPER),NCOL,NROW,
     4               NLAY,NODES,RELAXPCG,IOUT,MUTPCG,IT1,DAMPPCG,BUFF,
     5               HCSV,IERR,HPCG,DAMPPCGT,ISSFLG(KKPER),HDRY,
     6               IHCOFADD)
            END IF
c            IF (IUNIT(14).GT.0) THEN
c              CALL LMG7PNT(IGRID)
c              CALL LMG7AP(HNEW,IBOUND,CR,CC,CV,HCOF,RHS,A,IA,JA,U1,
c     1           FRHS,IG,ISIZ1,ISIZ2,ISIZ3,ISIZ4,KKITER,BCLOSE,DAMPLMG,
c     2           ICNVG,KKSTP,KKPER,MXITER,MXCYC,NCOL,NROW,NLAY,NODES,
c     3           HNOFLO,IOUT,IOUTAMG,ICG,IADAMPLMG,DUPLMG,DLOWLMG)
c            END IF
!            IF (IUNIT(42).GT.0) THEN
!                   CALL GMG7PNT(IGRID)
!                   CALL GMG7AP(HNEW,RHS,CR,CC,CV,HCOF,HNOFLO,IBOUND,
!     1                         IITER,MXITER,RCLOSEGMG,HCLOSEGMG,
!     2                         KKITER,KKSTP,KKPER,NCOL,NROW,NLAY,ICNVG,
!     3                         SITER,TSITER,DAMPGMG,IADAMPGMG,IOUTGMG,
!     4                         IOUT,GMGID,
!     5                         IUNITMHC,DUP,DLOW,CHGLIMIT,
!     6                         BIGHEADCHG,HNEWLAST)
!            ENDIF
! Calculate new heads using Newton solver
          IF(IUNIT(63).GT.0 ) THEN
             CALL GWF2NWT1FM(KKITER,ICNVG,KSTP,KPER,Mxiter,
     2                       IUNIT(22),Itreal,AGCONVERGE,IGRID)
             ITREAL2 = ITREAL
          ENDIF
          IF(IERR.EQ.1) CALL USTOP(' ')
C
C-------ENSURE CONVERGENCE OF SWR - BASEFLOW CHANGES LESS THAN TOLF - JDH
            IF(IUNIT(64).GT.0) THEN
              CALL GWF2SWR7CV(KKITER,IGRID,ICNVG,MXITER)
            END IF

            IF ( Szcheck==ACTIVE .AND. Model==MODSIM_GSFLOW ) THEN
              retval = gsflow_mf2prms()
              IF ( PRMS_land_iteration_flag==CANOPY ) THEN
                retval = intcp()
                retval = snowcomp()
                IF ( Glacier_flag==ACTIVE ) THEN
                  retval = glacr()
                ENDIF
              ENDIF
              IF ( PRMS_land_iteration_flag>0 ) THEN
                retval = srunoff()
              ENDIF
              !IF ( Szcheck==ACTIVE ) retval = gsflow_mf2prms()  RGN+RSR 12/14/2022
              IF ( AG_flag==ACTIVE ) THEN
                retval = soilzone_ag()
              ELSE
                retval = soilzone()
              ENDIF
              IF ( activeHRU_inactiveCELL_flag == ACTIVE )
     &             CALL gwflow_inactive_cell()
              retval = gsflow_prms2mf()
            END IF
C
C7C2C---IF CONVERGENCE CRITERION HAS BEEN MET STOP ITERATING.
C
            IF (ICNVG.EQ.1) GOTO 33
            IF ( Szcheck==ACTIVE .AND. GSFLOW_flag==1 )
     1       retval = gsflow_mf2prms()
!  30      CONTINUE
          END DO
          KITER = MXITER
C
   33     CONTINUE
!          kkiter = itreal
      !move above and executed when AFR = TRUE
          IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(2,Igrid)
C
      IF (MODSIM_flag==1 .AND. iss==0) THEN
        IF(IUNIT(44).GT.0) CALL SFR2MODSIM(EXCHANGE, Diversions, 
     1                             Idivert, Nsegshold, Timestep, KITER)
        IF(IUNIT(44).GT.0) CALL LAK2MODSIM(DELTAVOL, LAKEVOL, 
     1                              Diversions, Nsegshold)
        IF( IUNIT(66).GT.0 ) CALL AG2MODSIM(agDemand)
      END IF
      END SUBROUTINE MFNWT_RUN
C
C     ************************************************************************
C     ACCUMULATE ACCRETION/DEPLETIONS FOR ALL STREAM SEGMENETS AND LAKES THAT
C     ARE MAPPED TO MODSIM NODES (OR LINKS)
C     ************************************************************************
      SUBROUTINE COMPUTE_EXCHG(EXCHG, ELAKVol, KPER) 
     &                  BIND(C,NAME="COMPUTE_EXCHG")
C      
        !DEC$ ATTRIBUTES DLLEXPORT :: COMPUTE_EXCHG
C
        USE GWFLAKMODULE, ONLY: NLAKES, VOL 
        IMPLICIT NONE
        INTEGER I
C
C        REAL(8), DIMENSION(3,13), INTENT(inout) :: EXCHG  !From before switching to a 1D array 
        REAL(8), DIMENSION(100), INTENT(inout) :: EXCHG    !Now a 1D array (5-16-14)
        REAL(8), DIMENSION(1), INTENT(inout) :: ELAKVol
        INTEGER, DIMENSION(1), INTENT(inout) :: KPER
C        
C      CALL RAD1_ACC_DEP(EXCHG)
      !CALL RAD1_ACC_DEP2(EXCHG,KPER) ! rsr this routine is missing
C
      DO I=1,NLAKES    ! " - 1" Will need to adjust this between scenarios (Need to have it for 'dry up' scenario)
          ELAKVol(I)=VOL(I)
      ENDDO
C
      END SUBROUTINE COMPUTE_EXCHG
C
C
C
C     ************************************************************************
C     Upon MODSIM-MODFLOW conversion,
C     write the budget terms
C     ************************************************************************
      SUBROUTINE MFNWT_OCBUDGET(agDemand, Diversions, Nsegshold)
     &                  BIND(C,NAME="MFNWT_OCBUDGET")
C      
      !DEC$ ATTRIBUTES DLLEXPORT :: MFNWT_OCBUDGET
C
      USE PRMS_CONSTANTS, ONLY: ACTIVE, OFF, DEBUG_less
      USE GLOBAL
      USE GWFBASMODULE
      USE GWFHUFMODULE, ONLY:IOHUFHDS,IOHUFFLWS
      USE GWFEVTMODULE, ONLY:NEVTOP
      USE GWFRCHMODULE, ONLY:NRCHOP
      USE GWFNWTMODULE, ONLY:ICNVGFLG
      USE GSFMODFLOW
      USE PRMS_MODULE, ONLY: Print_debug, Timestep, Kkiter,
     +    Nowyear, Nowmonth, Nowday, MODSIM_flag
      IMPLICIT NONE
      EXTERNAL :: GWF2BAS7OC, GWF2BCF7BDS, GWF2BCF7BDCH, GWF2UPWBDADJ
      EXTERNAL :: GWF2BCF7BDADJ, GWF2LPF7BDS, GWF2LPF7BDCH, USTOP
      EXTERNAL :: GWF2LPF7BDADJ, GWF2UPWBDS, GWF2UPWBDCH, GWF2HUF7BDS
      EXTERNAL :: GWF2HUF7BDCH, GWF2HUF7BDADJ, GWF2WEL7BD, GWF2DRN7BD
      EXTERNAL :: GWF2RIV7BD, GWF2EVT7BD, GWF2FHB7BD, GWF2RES7BD
      EXTERNAL :: GWF2STR7BD, GWF2IBS7BD, GWF2ETS7BD, GWF2DRT7BD
      EXTERNAL :: GWF2GHB7BD, GWF2RCH7BD, GWF2LAK7ST, MODSIM2AG
      EXTERNAL :: GWF2SFR7BD, GWF2LAK7BD, GWF2UZF1BD, GWF2MNW27BD
      EXTERNAL :: GWF2MNW17BD, GWF2SUB7BD, GWF2SWT7BD
      EXTERNAL :: GWF2SWI2BD, GWF2AG7BD, LMT8BD, GWF2NWT1BD
      EXTERNAL :: OBS2BAS7SE, OBS2RIV7SE, OBS2GHB7SE, OBS2STR7SE
      EXTERNAL :: OBS2CHD7SE, GWF2HYD7BAS7SE, GWF2HYD7IBS7SE
      EXTERNAL :: GWF2HYD7SFR7SE, GWF2HYD7SUB7SE, GWF2HYD7STR7SE
      EXTERNAL :: GWF2SWR7BD, OBS2DRN7SE, GWF2BAS7OT, GWF2IBS7OT
      EXTERNAL :: GWF2HUF7OT, GWF2MNW2I7OT, GWF2SUB7OT, GWF2SWT7OT
      EXTERNAL :: GWF2HYD7BAS7OT, GWF2MNW17OT
      INTRINSIC :: MIN
      INTEGER :: IBDRET, IC1, IC2, IR1, IR2, IL1, IL2, IDIR, ii, 
     +           Nsegshold, ISS
      REAL :: BUDPERC
      DOUBLE PRECISION, INTENT(INOUT) :: agDemand(Nsegshold), 
     +                                   Diversions(Nsegshold)
C     ************************************************************************
      KKSTP = KSTP
      KKPER = KPER
      ISS  = ISSFLG(KKPER)

          CALL GWF2BAS7OC(KKSTP,KKPER,ICNVG,IUNIT(12),IGRID)
C
C7C4----CALCULATE BUDGET TERMS. SAVE CELL-BY-CELL FLOW TERMS.
          MSUM = 1
          IF (IUNIT(1).GT.0) THEN
            CALL GWF2BCF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2BCF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 37 IDIR = 1, 3
              CALL GWF2BCF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     1                          IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
   37       CONTINUE
          ENDIF
          IF(IUNIT(23).GT.0) THEN
            CALL GWF2LPF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2LPF7BDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 157 IDIR=1,3
              CALL GWF2LPF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
157         CONTINUE
          ENDIF
! ADDED CALLS FOR FLOW BUDGETS TO UPW PACKAGE.
          IF(IUNIT(62).GT.0) THEN
            CALL GWF2UPWBDS(KKSTP,KKPER,IGRID)
            CALL GWF2UPWBDCH(KKSTP,KKPER,IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 158 IDIR=1,3
              CALL GWF2UPWBDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IGRID)
158         CONTINUE
          ENDIF
          IF(IUNIT(37).GT.0) THEN
            CALL GWF2HUF7BDS(KKSTP,KKPER,IGRID)
            CALL GWF2HUF7BDCH(KKSTP,KKPER,IUNIT(47),IGRID)
            IBDRET=0
            IC1=1
            IC2=NCOL
            IR1=1
            IR2=NROW
            IL1=1
            IL2=NLAY
            DO 159 IDIR=1,3
              CALL GWF2HUF7BDADJ(KKSTP,KKPER,IDIR,IBDRET,
     &                        IC1,IC2,IR1,IR2,IL1,IL2,IUNIT(47),IGRID)
159         CONTINUE
          ENDIF
          IF(IUNIT(2).GT.0) CALL GWF2WEL7BD(KKSTP,KKPER,IUNIT(63),IGRID)
          IF(IUNIT(3).GT.0) CALL GWF2DRN7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(4).GT.0) CALL GWF2RIV7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(5).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
             CALL GWF2EVT7BD(KKSTP,KKPER,IGRID)
             IF(IUNIT(22).GT.0.AND.NEVTOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(7).GT.0) CALL GWF2GHB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(8).GT.0) THEN
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     0,IGRID)
             CALL GWF2RCH7BD(KKSTP,KKPER,IGRID)
             IF(IUNIT(22).GT.0.AND.NRCHOP.EQ.3) CALL GWF2LAK7ST(
     1                                                     1,IGRID)
          END IF
          IF(IUNIT(16).GT.0) CALL GWF2FHB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(17).GT.0) CALL GWF2RES7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(18).GT.0) CALL GWF2STR7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(19).GT.0) CALL GWF2IBS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(39).GT.0) CALL GWF2ETS7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(40).GT.0) CALL GWF2DRT7BD(KKSTP,KKPER,IGRID)
          IF (MODSIM_flag==1 .AND. iss==0) THEN
            IF( IUNIT(66).GT.0 ) CALL MODSIM2AG(Diversions)
          END IF
          IF(IUNIT(44).GT.0) CALL GWF2SFR7BD(KKSTP,KKPER,IUNIT(15),
     1                        IUNIT(22),IUNIT(46),IUNIT(55),NSOL,
     2                        IUNIT(8),IGRID)
          IF(IUNIT(22).GT.0) CALL GWF2LAK7BD(KKSTP,KKPER,IUNIT(15),
     1                       IUNIT(46),IUNIT(44),IUNIT(55),NSOL,IGRID)
! Moved call to UZF1BD to follow SFR7BD for printing net recharge in UZF.
          IF(IUNIT(55).GT.0) CALL GWF2UZF1BD(KKSTP,KKPER,IUNIT(22),
     1                             IUNIT(44),IGRID)
          IF(IUNIT(50).GT.0) CALL GWF2MNW27BD(KKSTP,KKPER,IUNIT(62),
     1                                        IGRID)
          IF(IUNIT(52).GT.0) CALL GWF2MNW17BD(NSTP(KPER),KKSTP,KKPER,
     1                      IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(57).GT.0) CALL GWF2SWT7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(64).GT.0) CALL GWF2SWR7BD(KKSTP,KKPER,IGRID)  !SWR - JDH
!         IF(IUNIT(67).GT.0) CALL GWF2GFB7BD(KKSTP,KKPER,IGRID)
          IF(IUNIT(65).GT.0) CALL GWF2SWI2BD(KKSTP,KKPER,IGRID)  !SWI2 - JDH
          IF(IUNIT(66).GT.0) CALL GWF2AG7BD(KKSTP,KKPER,IUNIT(63),
     +                                      agDemand, Nsegshold)
CLMT
CLMT----CALL LINK-MT3DMS SUBROUTINES TO SAVE FLOW-TRANSPORT LINK FILE
CLMT----FOR USE BY MT3DMS FOR TRANSPORT SIMULATION
CLMT
          IF(IUNIT(49).GT.0) CALL LMT8BD(KKSTP,KKPER,IGRID)
CLMT
C
C
!  Set NWT heads to Hdry when head is below bottom.
          IF(IUNIT(63).GT.0)CALL GWF2NWT1BD()
C  Observation simulated equivalents
          CALL OBS2BAS7SE(IUNIT(28),IGRID)
          IF(IUNIT(33).GT.0) CALL OBS2DRN7SE(IGRID)
          IF(IUNIT(34).GT.0) CALL OBS2RIV7SE(IGRID)
          IF(IUNIT(35).GT.0) CALL OBS2GHB7SE(IGRID)
          IF(IUNIT(36).GT.0) CALL OBS2STR7SE(IGRID)
          IF(IUNIT(38).GT.0) CALL OBS2CHD7SE(KKPER,IUNIT(62),IGRID)
          IF(IUNIT(43).GT.0) THEN
            CALL GWF2HYD7BAS7SE(1,IGRID)
            IF(IUNIT(19).GT.0) CALL GWF2HYD7IBS7SE(1,IGRID)
            IF(IUNIT(54).GT.0) CALL GWF2HYD7SUB7SE(1,IGRID)
            IF(IUNIT(18).GT.0) CALL GWF2HYD7STR7SE(1,IGRID)
            IF(IUNIT(44).GT.0) CALL GWF2HYD7SFR7SE(1,IGRID)
          ENDIF
C
C7C5---PRINT AND/OR SAVE DATA.
          CALL GWF2BAS7OT(KKSTP,KKPER,ICNVG,1,IGRID,BUDPERC)
          IF(IUNIT(19).GT.0) CALL GWF2IBS7OT(KKSTP,KKPER,IUNIT(19),
     1                                       IGRID)
          IF(IUNIT(37).GT.0)THEN
            IF(IOHUFHDS .NE.0 .OR.IOHUFFLWS .NE.0)
     1         CALL GWF2HUF7OT(KKSTP,KKPER,ICNVG,1,IGRID)
          ENDIF
          IF(IUNIT(51).NE.0) CALL GWF2MNW2I7OT(NSTP(KKPER),KKSTP,
     1                       KKPER,IGRID)
          IF(IUNIT(54).GT.0) CALL GWF2SUB7OT(KKSTP,KKPER,IUNIT(54),
     1                                       IGRID)
          IF(IUNIT(57).GT.0) CALL GWF2SWT7OT(KKSTP,KKPER,IGRID)
          IF(IUNIT(43).GT.0) CALL GWF2HYD7BAS7OT(KKSTP,KKPER,IGRID)
C
C7C6---JUMP TO END OF PROGRAM IF CONVERGENCE WAS NOT ACHIEVED.
          IF(ICNVG.EQ.0) THEN
            NCVGERR=NCVGERR+1
            WRITE(IOUT,87) BUDPERC
   87       FORMAT(1X,'FAILURE TO MEET SOLVER CONVERGENCE CRITERIA',/
     1             1X,'BUDGET PERCENT DISCREPANCY IS',F10.4)
            IF ( gsflag==OFF ) THEN
              IF ( IUNIT(63).GT.0 ) THEN
                IF ( ICNVGFLG.EQ.0 ) THEN
                  WRITE(IOUT,*) 'STOPPING SIMULATION'
                  CALL MFNWT_CLEAN()
                  CALL USTOP(' ')
                END IF
              ELSE
                IF(ABS(BUDPERC).GT.STOPER) THEN
                  WRITE(IOUT,*) 'STOPPING SIMULATION'
                  CALL MFNWT_CLEAN()
                  CALL USTOP(' ')
                END IF
              END IF
            END IF
            WRITE(IOUT,*) 'CONTINUING EXECUTION'
            Convfail_cnt = Convfail_cnt + 1
            IF ( Print_debug>DEBUG_less )
     &           PRINT 9004, Nowyear, Nowmonth, Nowday, Convfail_cnt
          ENDIF
C
C-----END OF TIME STEP (KSTP) AND STRESS PERIOD (KPER) LOOPS
!gsf 90   CONTINUE
!gsf100 CONTINUE
C
C
      IF(IUNIT(52).NE.0) CALL GWF2MNW17OT(IGRID)

      IF ( gsflag==ACTIVE ) THEN
        II = MIN(ITDIM, Maxgziter)
        Iter_cnt(II) = Iter_cnt(II) + 1
        IF ( Maxgziter.GT.Max_sziters ) Max_sziters = Maxgziter
        II = MIN(ITDIM, ITREAL2)
        Mfiter_cnt(II) = Mfiter_cnt(II) + 1
        Iterations = Iterations + ITREAL2
        IF ( KKITER.GT.Max_iters ) Max_iters = ITREAL2
        IF ( Print_debug>DEBUG_less ) THEN
          IF ( Nowday.EQ.1 ) THEN
            PRINT 9002, Nowyear, Nowmonth, Nowday, KKPER, KKSTP,
     &                  Timestep, KKITER, Maxgziter
          ELSEIF ( KKITER.GT.75 ) THEN
            PRINT 9002, Nowyear, Nowmonth, Nowday, KKPER, KKSTP,
     &                  Timestep, KKITER, Maxgziter
          ENDIF
        ENDIF
      ENDIF

 9002 FORMAT('Date: ', I0, 2('/',I2.2), '; Stress: ', I0, '; Step: ',I0,
     &       '; Simulation step: ', I0, /, 18X, 'MF iterations: ', I0,
     &       '; SZ iterations: ', I0, /)
 9004 FORMAT('***TIME STEP FAILED TO CONVERGE - Date:', I5, 2('/',I2.2),
     &       ' number: ', I0, /)

      END SUBROUTINE MFNWT_OCBUDGET
!
!***********************************************************************
!     MFNWT_CLEAN - After GSFLOW is done
!***********************************************************************
C
C     ************************************************************************
C     Close out MODFLOW
C     ************************************************************************
      SUBROUTINE MFNWT_CLEAN() BIND(C,NAME="MFNWT_CLEAN")
C
      !DEC$ ATTRIBUTES DLLEXPORT :: MFNWT_CLEAN
C
!     ------------------------------------------------------------------
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GSFMODFLOW
      USE PRMS_CONSTANTS, ONLY: SAVE_INIT, ACTIVE, DEBUG_less, MODFLOW
      USE PRMS_MODULE, ONLY: Timestep, Save_vars_to_file, GSFLOW_flag,
     &    Print_debug, Model, Gsf_unt
      USE GLOBAL, ONLY: IOUT, IUNIT, NIUNIT
      USE GWFNWTMODULE, ONLY:LINMETH
      IMPLICIT NONE
      EXTERNAL :: RESTART1WRITE
      EXTERNAL :: GWF2SUB7SV, OBS2BAS7OT, OBS2RIV7OT, OBS2GHB7OT
      EXTERNAL :: OBS2STR7OT, OBS2CHD7OT, GWF2SWR7OT
      EXTERNAL :: SGWF2BAS7PNT, GWF2BCF7DA, GWF2WEL7DA, GWF2DRN7DA
      EXTERNAL :: GWF2RIV7DA, GWF2EVT7DA, GWF2GHB7DA, GWF2RCH7DA
      EXTERNAL :: SIP7DA, DE47DA, PCG7DA, GMRES7DA, XMD7DA
      EXTERNAL :: GWF2UPW1DA, GWF2FHB7DA, GWF2RES7DA, GWF2STR7DA
      EXTERNAL :: GWF2IBS7DA, OBS2DRN7OT, GWF2HYD7DA, LMT8DA, GWF2AG7DA
      EXTERNAL :: GWF2CHD7DA, GWF2HFB7DA, GWF2LAK7DA
      EXTERNAL :: GWF2LPF7DA, GWF2HUF7DA, GWF2ETS7DA, GWF2DRT7DA
      EXTERNAL :: GWF2SFR7DA, GWF2GAG7DA, GWF2MNW27DA, GWF2MNW2I7DA
      EXTERNAL :: GWF2MNW17DA, GWF2SUB7DA, GWF2UZF1DA, GWF2SWT7DA
      EXTERNAL :: GWF2SWR7DA, GWF2SWI2DA, OBS2BAS7DA, OBS2DRN7DA
      EXTERNAL :: OBS2RIV7DA, OBS2GHB7DA, OBS2STR7DA, OBS2CHD7DA
      EXTERNAL :: GWF2NWT1DA, GWF2BAS7DA, USTOP
!***********************************************************************
C
C8------END OF SIMULATION
C-------SAVE RESTART RECORDS FOR SUB PACKAGE
C 110 IF(IUNIT(54).GT.0) CALL GWF2SUB7SV(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7SV(IGRID)
C-------WRITE RESTART INFORMATION FOR HEADS, SFR, AND UZF
      IF ( Save_vars_to_file==ACTIVE ) THEN
        CALL RESTART1WRITE()
        IF ( GSFLOW_flag==ACTIVE )CALL gsflow_modflow_restart(SAVE_INIT)
      ENDIF
C
C  Observation output
      IF(IUNIT(28).GT.0) CALL OBS2BAS7OT(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7OT(IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7OT(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7OT(IGRID)
      IF(IUNIT(36).GT.0) CALL OBS2STR7OT(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7OT(IGRID)
      CALL GLO1BAS6ET(IOUT,IBDT,1)
C
C-------OUTPUT RESULTS OF SWR TIMER
      IF(IUNIT(64).GT.0) CALL GWF2SWR7OT(IGRID)
C
C9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7DA MUST BE CALLED
C9------LAST BECAUSE IT DEALLOCATES IUNIT.
      CALL SGWF2BAS7PNT(IGRID)
      IF(IUNIT(1).GT.0) CALL GWF2BCF7DA(IGRID)
      IF(IUNIT(2).GT.0) CALL GWF2WEL7DA(IGRID)
      IF(IUNIT(3).GT.0) CALL GWF2DRN7DA(IGRID)
      IF(IUNIT(4).GT.0) CALL GWF2RIV7DA(IGRID)
      IF(IUNIT(5).GT.0) CALL GWF2EVT7DA(IGRID)
      IF(IUNIT(7).GT.0) CALL GWF2GHB7DA(IGRID)
      IF(IUNIT(8).GT.0) CALL GWF2RCH7DA(IGRID)
      IF(IUNIT(9).GT.0) CALL SIP7DA(IGRID)
      IF(IUNIT(10).GT.0) CALL DE47DA(IGRID)
      IF(IUNIT(13).GT.0) CALL PCG7DA(IGRID)
c      IF(IUNIT(14).GT.0) CALL LMG7DA(IGRID)
      IF(IUNIT(63).GT.0) THEN
        IF(LINMETH.EQ.1) THEN
          CALL GMRES7DA(IGRID)
        ELSEIF(LINMETH.EQ.2) THEN
          CALL XMD7DA(IGRID)
!        ELSEIF(LINMETH.EQ.3) THEN
!          CALL SAMG7DA(IGRID)
        END IF
        CALL GWF2NWT1DA(IGRID)
      END IF
      IF(IUNIT(62).GT.0) CALL GWF2UPW1DA(IGRID)
      IF(IUNIT(16).GT.0) CALL GWF2FHB7DA(IGRID)
      IF(IUNIT(17).GT.0) CALL GWF2RES7DA(IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7DA(IGRID)
      IF(IUNIT(19).GT.0) CALL GWF2IBS7DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7DA(IGRID)
!      IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(21).GT.0) CALL GWF2HFB7DA(IGRID)
      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7DA(IUNIT(22),
     1                                              IGRID)
      IF(IUNIT(23).GT.0) CALL GWF2LPF7DA(IGRID)
      IF(IUNIT(37).GT.0) CALL GWF2HUF7DA(IGRID)
      IF(IUNIT(39).GT.0) CALL GWF2ETS7DA(IGRID)
      IF(IUNIT(40).GT.0) CALL GWF2DRT7DA(IGRID)
!      IF(IUNIT(42).GT.0) CALL GMG7DA(IGRID)
      IF(IUNIT(44).GT.0) CALL GWF2SFR7DA(IGRID)
      IF(IUNIT(46).GT.0) CALL GWF2GAG7DA(IGRID)
      IF(IUNIT(50).GT.0) CALL GWF2MNW27DA(IGRID)
      IF(IUNIT(51).GT.0) CALL GWF2MNW2I7DA(IGRID)
      IF(IUNIT(52).GT.0) CALL GWF2MNW17DA(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7DA(IGRID)
      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
      IF(IUNIT(57).GT.0) CALL GWF2SWT7DA(IGRID)
      IF(IUNIT(64).GT.0) CALL GWF2SWR7DA(IGRID)  !SWR - JDH
      IF(IUNIT(65).GT.0) CALL GWF2SWI2DA(IGRID)  !SW12 - JDH
!      IF(IUNIT(67).GT.0) CALL GWF2GFB7DA(IGRID)
      CALL OBS2BAS7DA(IUNIT(28),IGRID)
      IF(IUNIT(33).GT.0) CALL OBS2DRN7DA(IGRID)
      IF(IUNIT(34).GT.0) CALL OBS2RIV7DA(IGRID)
      IF(IUNIT(35).GT.0) CALL OBS2GHB7DA(IGRID)
      IF(IUNIT(36).GT.0) CALL OBS2STR7DA(IGRID)
      IF(IUNIT(38).GT.0) CALL OBS2CHD7DA(IGRID)
      IF(IUNIT(43).GT.0) CALL GWF2HYD7DA(IGRID)
      IF(IUNIT(49).GT.0) CALL LMT8DA(IGRID)
      IF(IUNIT(66).GT.0) CALL GWF2AG7DA()
!      IF(IUNIT(61).GT.0) CALL FMP2DA(IGRID)
      CALL GWF2BAS7DA(IGRID)
C
      IF ( GSFLOW_flag==ACTIVE .and. Timestep > 0 ) THEN
        PRINT 9001, Timestep, Convfail_cnt, Iterations, Sziters,
     &            FLOAT(Iterations)/FLOAT(Timestep),
     &            FLOAT(Sziters)/FLOAT(Timestep), Max_iters, Max_sziters
        IF ( Stopcount>0 ) PRINT 9005, Stopcount
        IF ( Print_debug>DEBUG_less ) THEN
          PRINT 9003, 'MF iteration distribution:', Mfiter_cnt
          PRINT 9007, 'SZ computation distribution:', Iter_cnt
        ENDIF
        WRITE(Gsf_unt,9001) Timestep, Convfail_cnt, Iterations, Sziters,
     &            FLOAT(Iterations)/FLOAT(Timestep),
     &            FLOAT(Sziters)/FLOAT(Timestep), Max_iters, Max_sziters
        IF ( Stopcount>0 ) WRITE(Gsf_unt,9005) Stopcount
        IF ( Print_debug>DEBUG_less ) THEN
          WRITE(Gsf_unt,9003) 'MF iteration distribution:', Mfiter_cnt
          WRITE(Gsf_unt,9007) 'SZ computation distribution:', Iter_cnt
        ENDIF
      ENDIF

C10-----END OF PROGRAM.
      IF(NCVGERR.GT.0) THEN
        WRITE(*,*) 'FAILED TO MEET SOLVER CONVERGENCE CRITERIA ',
     1          NCVGERR,' TIME(S)'
      ELSE
        PRINT '(A,/)', ' Normal termination of simulation'
      END IF

!gsf  CALL USTOP(' ')
      IF ( Model==MODFLOW ) CALL USTOP(' ')

 9001 FORMAT (' Number of time steps:', I7,
     &        ';  Number of non-convergence:', I4, /, ' MF iterations:'
     &        , I14, ';  SZ iterations:', I16, /,
     &        ' Average MF iterations:', F6.2,
     &        ';  Average SZ iterations:', F8.2, /,
     &        ' Maximum MF iterations:', I6,
     &        ';  Maximum SZ iterations:', I8, /)
 9003 FORMAT (A, 2X, 10I5, /, 10(28X, 10I5, /), /)
 9005 FORMAT ('mxsziter reached: ', I0, /)
 9007 FORMAT (A, 10I5, /, 10(28X, 10I5, /))

C
      END SUBROUTINE MFNWT_CLEAN
!
      SUBROUTINE GETNAMFIL(FNAME)
C     ******************************************************************
C     GET THE NAME OF THE NAME FILE
C     ******************************************************************
      use PRMS_CONTROL_FILE, only: control_string
      use prms_utils, only: numchars, read_error
      USE GSFMODFLOW, ONLY: Modflow_name
C        SPECIFICATIONS:
C
C     ------------------------------------------------------------------
      EXTERNAL :: USTOP
      CHARACTER*(*) FNAME
!      CHARACTER*200 COMLIN
      LOGICAL EXISTS
      INTEGER nc
C     ------------------------------------------------------------------
C
C Get name file from command line or user interaction.
        FNAME=' '
!        COMLIN=' '
        IF ( control_string(Modflow_name, 'modflow_name')/=0 )
     &       CALL read_error(5, 'modflow_name')
        FNAME = Modflow_name
C *** Subroutines GETARG and GETCL are extensions to Fortran 90/95 that
C *** allow a program to retrieve command-line arguments.  To enable
C *** Modflow-2000 to read the name of a Name file from the command
C *** line, either GETARG or GETCL must be called, but not both.  As
C *** distributed, the call to GETARG is uncommented.  For compilers
C *** that support GETCL but not GETARG, comment out the call to GETARG
C *** and uncomment the call to GETCL.  The calls to both GETARG and
C *** GETCL may be commented out for compilers that do not support
C *** either extension.
!        CALL GETARG(1,COMLIN)
C        CALL GETCL(COMLIN)
!        IF(COMLIN.NE.' ') FNAME=COMLIN
!      ENDIF
!      IF ( FNAME(:1).EQ.' ' ) THEN
!  15    WRITE (*,*) ' Enter the path of the NAME FILE or "quit": '
!        READ (*,'(A)') FNAME
!        IF ( FNAME(:4)=='quit' .OR. FNAME(:4)=='QUIT' )
!     &       CALL USTOP(' ')
!        IF (FNAME(:1).EQ.' ') GOTO 15
!      ENDIF
      nc = numchars(FNAME)
        INQUIRE (FILE=FNAME(:nc),EXIST=EXISTS)
        IF(.NOT.EXISTS) THEN
   15   PRINT '(/,A,A,/)', 'Name File does not exist: ', FNAME(:nc)
        PRINT *, ' Enter a different Name File path or "quit": '
        READ (*,'(A)') FNAME
        IF ( FNAME(:4)=='quit' .OR. FNAME(:4)=='QUIT'
     &       .OR. FNAME(:1)==' ' ) CALL USTOP(' ')
        nc = numchars(FNAME)
        INQUIRE ( FILE=FNAME(:nc),EXIST=EXISTS )
        IF (.NOT.EXISTS) GOTO 15
        PRINT *, ' '
        ENDIF
C
      RETURN
      END SUBROUTINE GETNAMFIL
C
      SUBROUTINE GLO1BAS6ET(IOUT,IBDT,IPRTIM)
C     ******************************************************************
C     Get end time and calculate elapsed time
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTRINSIC :: INT, NINT, MOD
      REAL :: RSECS, ELSEC
      INTEGER :: NSPD, I, IPRTIM, IOUT, NDAYS, LEAP, IBD, IED
      INTEGER :: M, MB, MC, ME, NM, NHOURS, NMINS, NSECS, MSECS, NRSECS
      INTEGER IBDT(8), IEDT(8), IDPM(12)
      DATA IDPM/31,28,31,30,31,30,31,31,30,31,30,31/ ! Days per month
      DATA NSPD/86400/  ! Seconds per day
C     ------------------------------------------------------------------
C
C     Get current date and time, assign to IEDT, and write.
      CALL DATE_AND_TIME(VALUES=IEDT)
      WRITE(*,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
 1000 FORMAT(/,1X,'Run end date and time (yyyy/mm/dd hh:mm:ss): ',
     &       I4,2('/',I2.2),I3,2(':',I2.2))
      IF(IPRTIM.GT.0) THEN
        WRITE(IOUT,'(1X)')
        WRITE(IOUT,1000) (IEDT(I),I=1,3),(IEDT(I),I=5,7)
      END IF
C
C     Calculate elapsed time in days and seconds
      NDAYS=0
      LEAP=0
      IF (MOD(IEDT(1),4).EQ.0) LEAP = 1
      IBD = IBDT(3)            ! BEGIN DAY
      IED = IEDT(3)            ! END DAY
C     FIND DAYS
      IF (IBDT(2).NE.IEDT(2)) THEN
C       MONTHS DIFFER
        MB = IBDT(2)             ! BEGIN MONTH
        ME = IEDT(2)             ! END MONTH
        NM = ME-MB+1             ! NUMBER OF MONTHS TO LOOK AT
        IF (MB.GT.ME) NM = NM+12
        MC=MB-1
        DO 10 M=1,NM
          MC=MC+1                ! MC IS CURRENT MONTH
          IF (MC.EQ.13) MC = 1
          IF (MC.EQ.MB) THEN
            NDAYS = NDAYS+IDPM(MC)-IBD
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ELSEIF (MC.EQ.ME) THEN
            NDAYS = NDAYS+IED
          ELSE
            NDAYS = NDAYS+IDPM(MC)
            IF (MC.EQ.2) NDAYS = NDAYS + LEAP
          ENDIF
   10   CONTINUE
      ELSEIF (IBD.LT.IED) THEN
C       START AND END IN SAME MONTH, ONLY ACCOUNT FOR DAYS
        NDAYS = IED-IBD
      ENDIF
      ELSEC=NDAYS*NSPD
C
C     ADD OR SUBTRACT SECONDS
      ELSEC = ELSEC+(IEDT(5)-IBDT(5))*3600.0
      ELSEC = ELSEC+(IEDT(6)-IBDT(6))*60.0
      ELSEC = ELSEC+(IEDT(7)-IBDT(7))
      ELSEC = ELSEC+(IEDT(8)-IBDT(8))*0.001
C
C     CONVERT SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
      NDAYS = INT(ELSEC/86400.0)
      RSECS = MOD(ELSEC,86400.0)
      NHOURS = INT(RSECS/3600.0)
      RSECS = MOD(RSECS,3600.0)
      NMINS = INT(RSECS/60.0)
      RSECS = MOD(RSECS,60.0)
      NSECS = INT(RSECS)
      RSECS = MOD(RSECS,1.0)
      MSECS = NINT(RSECS*1000.0)
      NRSECS = NSECS
      IF (RSECS.GE.0.5) NRSECS=NRSECS+1
C
C     Write elapsed time to screen
        IF (NDAYS.GT.0) THEN
          WRITE(*,1010) NDAYS,NHOURS,NMINS,NRSECS
 1010     FORMAT(' Elapsed run time: ',I0,' Days, ',I0,' Hours, ',I0,
     &           ' Minutes, ',I0,' Seconds',/)
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(*,1020) NHOURS,NMINS,NRSECS
 1020     FORMAT(' Elapsed run time: ',I0,' Hours, ',I0,
     &           ' Minutes, ',I0,' Seconds',/)
        ELSEIF (NMINS.GT.0) THEN
          WRITE(*,1030) NMINS,NSECS,MSECS
 1030     FORMAT(' Elapsed run time: ',I0,' Minutes, ',
     &      I0,'.',I3.3,' Seconds',/)
        ELSE
          WRITE(*,1040) NSECS,MSECS
 1040     FORMAT(' Elapsed run time: ',I0,'.',I3.3,' Seconds',/)
        ENDIF
C
C     Write times to file if requested
      IF(IPRTIM.GT.0) THEN
        IF (NDAYS.GT.0) THEN
          WRITE(IOUT,1010) NDAYS,NHOURS,NMINS,NRSECS
        ELSEIF (NHOURS.GT.0) THEN
          WRITE(IOUT,1020) NHOURS,NMINS,NRSECS
        ELSEIF (NMINS.GT.0) THEN
          WRITE(IOUT,1030) NMINS,NSECS,MSECS
        ELSE
          WRITE(IOUT,1040) NSECS,MSECS
        ENDIF
      ENDIF
C
      RETURN
      END SUBROUTINE GLO1BAS6ET

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C     READ CURRENT STRESS PERIOD'S BOUNDARY PACKAGE INFORMATION
C     REMEMBER THAT THE MODSIM-MODFLOW COUPLING IS GOING TO 
C     REQUIRE THAT A MODSIM TIME STEP EQUAL A MODFLOW TIME STEP
C     AND THAT MODFLOW WILL NEED TO BE RESTRICTED TO ONE TIME STEP
C     PER MODFLOW STRESS PERIOD.
!***********************************************************************
      SUBROUTINE MFNWT_RDSTRESS() BIND(C,NAME="MFNWT_RDSTRESS")
C
      !DEC$ ATTRIBUTES DLLEXPORT :: MFNWT_RDSTRESS
C
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ACTIVE, ERROR_time
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      USE GSFMODFLOW, ONLY: IGRID, KKPER, KPER, NSOL, IOUTS, KKSTP,
     &                      Mft_to_sec, KSTP
      USE GLOBAL, ONLY: IUNIT, ISSFLG, IOUT
      USE PRMS_SET_TIME, ONLY: Timestep_seconds
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
      INTRINSIC :: ABS
!***********************************************************************
C7------SIMULATE EACH STRESS PERIOD.
        IF ( KSTP == 0 ) KKSTP = 1
        KKPER = KPER
        IF(IUNIT(62).GT.0 ) CALL GWF2UPWUPDATE(1,IGRID)
        CALL GWF2BAS7ST(KKPER,IGRID)
        IF(IUNIT(19).GT.0) CALL GWF2IBS7ST(KKPER,IGRID)
        IF(IUNIT(54).GT.0) CALL GWF2SUB7ST(KKPER,IGRID)
        IF(IUNIT(57).GT.0) CALL GWF2SWT7ST(KKPER,IGRID)
        IF ( GSFLOW_flag==ACTIVE ) THEN
          IF ( ABS(Timestep_seconds-DELT*Mft_to_sec)>NEARZERO ) THEN
            WRITE (IOUT, 9003) Timestep_seconds, DELT, Mft_to_sec
            PRINT 9003, Timestep_seconds, DELT, Mft_to_sec
            ERROR STOP ERROR_time
          ENDIF
        ENDIF
C
C7B-----READ AND PREPARE INFORMATION FOR STRESS PERIOD.
C----------READ USING PACKAGE READ AND PREPARE MODULES.
        IF(IUNIT(2).GT.0) CALL GWF2WEL7RP(IUNIT(2),KKPER,IGRID)
        IF(IUNIT(3).GT.0) CALL GWF2DRN7RP(IUNIT(3),IGRID)
        IF(IUNIT(4).GT.0) CALL GWF2RIV7RP(IUNIT(4),IGRID)
        IF(IUNIT(5).GT.0) CALL GWF2EVT7RP(IUNIT(5),IGRID)
        IF(IUNIT(7).GT.0) CALL GWF2GHB7RP(IUNIT(7),IGRID)
!        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IUNIT(44),IGRID)
        IF(IUNIT(8).GT.0) CALL GWF2RCH7RP(IUNIT(8),IGRID)
        IF(IUNIT(17).GT.0) CALL GWF2RES7RP(IUNIT(17),IGRID)
        IF(IUNIT(18).GT.0) CALL GWF2STR7RP(IUNIT(18),IGRID)
        IF(IUNIT(43).GT.0 .AND. IUNIT(18).GT.0)
     1                     CALL GWF2HYD7STR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(20).GT.0) CALL GWF2CHD7RP(IUNIT(20),IGRID)
        IF(IUNIT(44).GT.0) CALL GWF2SFR7RP(IUNIT(44),IUNIT(15),
     1                                     IUNIT(22),KKPER,KKSTP,
     2                                     NSOL,IOUTS,IUNIT(55),IGRID)
        IF(IUNIT(43).GT.0 .AND. IUNIT(44).GT.0)
     1                     CALL GWF2HYD7SFR7RP(IUNIT(43),KKPER,IGRID)
        IF(IUNIT(55).GT.0) CALL GWF2UZF1RP(IUNIT(55),KKPER,KKSTP,
     1                                     IUNIT(44),IGRID)
        IF(IUNIT(22).GT.0) CALL GWF2LAK7RP(IUNIT(22),IUNIT(1),
     1               IUNIT(15),IUNIT(23),IUNIT(37),IUNIT(44),IUNIT(55),
     2               IUNIT(62),KKPER,NSOL,IOUTS,IGRID)
        IF(IUNIT(46).GT.0.AND.KKPER.EQ.1) CALL GWF2GAG7RP(IUNIT(15),
     1             IUNIT(22),IUNIT(55),NSOL,IGRID)
        IF(IUNIT(39).GT.0) CALL GWF2ETS7RP(IUNIT(39),IGRID)
        IF(IUNIT(40).GT.0) CALL GWF2DRT7RP(IUNIT(40),IGRID)
        IF(IUNIT(50).GT.0) CALL GWF2MNW27RP(IUNIT(50),KKPER,IUNIT(9),
     +                                      IUNIT(10),0,IUNIT(13),
     +                                      IUNIT(15),IUNIT(63),IGRID)
        IF(IUNIT(51).GT.0.AND.KKPER.EQ.1) CALL GWF2MNW2I7RP(IUNIT(51),
     1                     0,IGRID)
        IF(IUNIT(52).GT.0) CALL GWF2MNW17RP(IUNIT(52),IUNIT(1),
     1                            IUNIT(23),IUNIT(37),IUNIT(62),KKPER,
     2                            IGRID)
!        IF(IUNIT(61).GT.0) CALL FMP2RP(IUNIT(61),ISTARTFL,KKPER,        !FMP2AR CALL ADDED BY SCHMID
!     1                          IUNIT(44),IUNIT(52),IGRID)
        IF(IUNIT(64).GT.0) CALL GWF2SWR7RP(IUNIT(64),KKPER,IGRID)  !SWR - JDH
        IF ( IUNIT(66).GT.0 ) CALL GWF2AG7RP(IUNIT(66),IUNIT(44),KKPER)
C
        IF ( GSFLOW_flag==ACTIVE .AND. ISSFLG(KPER).EQ.0 )
     1                   CALL ZERO_SPECIFIED_FLOWS(IUNIT(22),IUNIT(44))
 9003 FORMAT (' Time steps must be equal: PRMS dtsec = ', F0.4,
     1        ' MODFLOW delt =', F12.4, ' Mft_to_sec = ', F0.4)
      END SUBROUTINE MFNWT_RDSTRESS

C     ************************************************************************
C     WRITE RASTERS OF GROUNDWATER/SURFACE-WATER INTERACTION
C     
C     ************************************************************************
C      SUBROUTINE MFNWT_WRITERAS(KPER) BIND(C,NAME="MFNWT_WRITERAS")
C
C      !DEC$ ATTRIBUTES DLLEXPORT :: MFNWT_WRITERAS
C
C      USE GLOBAL,       ONLY: NROW,NCOL
C      USE GWFSFRMODULE, ONLY: NSTRM,ISTRM,STRM
C      IMPLICIT NONE
C      INTEGER, INTENT(in)  :: KPER
C      INTEGER I,N,IRCH,JRCH,I1
C      CHARACTER*256 str1,str2
C      INTEGER*4 Y,ls1,ls2
C      REAL    FLOBOT
C     REAL, DIMENSION(NROW*NCOL) :: GWSWI
C      CHARACTER OUTFILE*255,FMT*18
C      CHARACTER X1*20
C
C--INITIALIZE ARRAY THAT WILL STORE GROUNDWATER SURFACE WATER INTERACTION
C  USE NODATA_VALUE TO INITIALIZE THE ARRAY
C      DO I=1,NROW*NCOL
C          GWSWI(I)=2446527.9909387
C      ENDDO
C
C      DO I=1,NSTRM
C          IRCH=ISTRM(2,I)
C          JRCH=ISTRM(3,I)
C          FLOBOT=STRM(11,I)
C          N=(IRCH-1)*NCOL+JRCH
C          IF (GWSWI(N).NE.2446527.9909387) THEN
C            GWSWI(N) = GWSWI(N) + FLOBOT
C          ELSE
C            GWSWI(N)=FLOBOT
C          ENDIF
C      ENDDO
C
C--WRITE THE ARRAY TO A TEXT FILE BASED FOR THE CURRENT TIME STEP
!      FMT='(I4.4)'
!      I1=KPER
!      WRITE(X1,FMT) I1
!      OUTFILE='H:\\MODSIM_MODFLOW_New
!     &\\ET_Computation_For_MODSIM-MODFLOW_MIF_FullIrr_SAcc_NewHD
!     &\\Source_Code_Coupling\\MODSIMStreamAquifer\\GWSWI_Rasters
!     &\\GWSWI_'//TRIM(X1)//'.TXT'
!C--REMOVE SPACES FROM THE LONG STRING
!      ls1 = len_trim(OUTFILE)
!      ls2 = 0
!      DO Y = 1,ls1
!        IF(OUTFILE(Y:Y).ne.' ') THEN
!           ls2 = ls2 + 1
!           str2(ls2:ls2) = OUTFILE(Y:Y)
!        ENDIF
!      ENDDO
      
C      OPEN(201,FILE=OUTFILE)
C      WRITE(201,*) "ncols         133"
C      WRITE(201,*) "nrows         64"
C      WRITE(201,*) "xllcorner     278826.60355005"
C      WRITE(201,*) "yllcorner     4343615.4021448"
C     WRITE(201,*) "cellsize      400"
C     WRITE(201,*) "NODATA_value  -999.99"
C     DO I=1,NROW
C       WRITE(201,'(1000F11.2)') ((-1*GWSWI(N)*35.315/86400),
C    &                            N=(I-1)*NCOL+1,I*NCOL)
C     ENDDO
C     CLOSE(201)
C
C      END SUBROUTINE MFNWT_WRITERAS
C     
C
!     ******************************************************************
!     DETERMINE THE STRESS PERIOD FOR THE CURRENT TIMESTEP
!     ******************************************************************
      INTEGER FUNCTION GET_KPER()
      USE PRMS_CONSTANTS, ONLY: MODFLOW, MODSIM_MODFLOW
      USE GLOBAL, ONLY: NPER
      USE GSFMODFLOW, ONLY: Stress_dates, KPER
      USE PRMS_MODULE, ONLY: Start_year, Start_month, Start_day,
     1    Nowyear, Nowmonth, Nowday, Model, mf_nowtime
      use prms_utils, only: compute_julday
      IMPLICIT NONE
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: KPERTEST, now
!     ------------------------------------------------------------------
      GET_KPER = -1
      IF ( Model==MODFLOW .or. Model==MODSIM_MODFLOW ) THEN
        now = mf_nowtime
      ELSE
        now = compute_julday(Nowyear, Nowmonth, Nowday)
      ENDIF
      KPERTEST = 1
      IF ( KPER > KPERTEST ) KPERTEST = KPER
!
!     If called from init, then "now" isn't set yet.
!     Set "now" to model start date.
      IF ( now<2 )
     &     now = compute_julday(Start_year, Start_month, Start_day)
      IF ( now<Stress_dates(KPERTEST) )
     &     STOP 'ERROR, now<stress period time'
      IF ( now>Stress_dates(NPER) ) THEN
        GET_KPER = NPER
      ELSEIF ( now<Stress_dates(KPER+1) ) THEN
        GET_KPER = KPER
      ELSE
        GET_KPER = KPER + 1
      ENDIF

      END FUNCTION GET_KPER

!***********************************************************************
!     READ AND PREPARE INFORMATION FOR STRESS PERIOD.
!***********************************************************************
      SUBROUTINE SET_STRESS_DATES(AFR, Diversions, Idivert, 
     &    EXCHANGE, DELTAVOL, LAKEVOL,Nsegshold, Nlakeshold, agDemand)
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, MODFLOW,
     &    ERROR_restart, ERROR_time, MODSIM_MODFLOW
      USE PRMS_MODULE, ONLY: Init_vars_from_file, Kkiter, Model,
     &    Start_year, Start_month, Start_day, Print_debug
      use prms_utils, only: compute_julday, error_stop
      USE GLOBAL, ONLY: NPER, ISSFLG, PERLEN, IUNIT, NSTP
      USE GSFMODFLOW, ONLY: Modflow_skip_time, Modflow_skip_stress,
     &    Modflow_time_in_stress, Stress_dates, Modflow_time_zero,
     &    Steady_state, ICNVG, KPER, KSTP, Mft_to_days, KPERSTART,
     &    Modflow_skip_time_step, IGRID
      USE GWFBASMODULE, ONLY: TOTIM
      USE OBSBASMODULE, ONLY: OBSTART,ITS
      IMPLICIT NONE
      ! Arguments
      LOGICAL, INTENT(IN) :: AFR
      INTEGER, INTENT(IN) :: Nsegshold, Nlakeshold
      INTEGER, INTENT(INOUT) :: Idivert(Nsegshold)
      DOUBLE PRECISION, INTENT(INOUT) :: Diversions(Nsegshold)
      DOUBLE PRECISION, INTENT(INOUT) :: EXCHANGE(Nsegshold), 
     &                                   DELTAVOL(Nlakeshold),
     &                                   LAKEVOL(Nlakeshold),
     &                                   agDemand(Nsegshold)
      ! Functions
      EXTERNAL :: RESTART1READ, GWF2BAS7OC
      INTRINSIC :: INT, DBLE
! Local Variables
      INTEGER :: i, n, nstress, start_jul, mfstrt_jul
      DOUBLE PRECISION :: kstpskip, plen, time
!***********************************************************************
      IF ( Print_debug>DEBUG_less )
     &     PRINT ( '(/, A, I5,2("/",I2.2))' ), 'modflow_time_zero:',
     &  Modflow_time_zero(1), Modflow_time_zero(2), Modflow_time_zero(3)
      ALLOCATE ( Stress_dates(NPER+1) )
      Stress_dates = 0
      Stress_dates(1) = compute_julday(Modflow_time_zero(1),
     &                  Modflow_time_zero(2), Modflow_time_zero(3))
      mfstrt_jul = Stress_dates(1)

      ! determine julian day
      start_jul = compute_julday(Start_year, Start_month, Start_day)

      IF ( mfstrt_jul>start_jul ) THEN
        PRINT *, 'ERROR, modflow_time_zero > start_time',
     &           mfstrt_jul, start_jul
        ERROR STOP ERROR_time
      ENDIF

      IF ( mfstrt_jul==start_jul .AND. Init_vars_from_file==1 .AND.
     &     ISSFLG(1)==1 ) STOP
     &    'ERROR, modflow_time_zero = start_time for restart'//
     &    ' simulation with steady state as first stress period'

      IF ( Mft_to_days>1.0 ) PRINT *, 'CAUTION, MF time step /= 1 day'

      IF ( Model==MODFLOW .OR. Model==MODSIM_MODFLOW ) PRINT *, ' '
      TOTIM = 0.0
      KPER = 0
      KSTP = 0
      Steady_state = 0
      ! run steady state and load Stress_dates array (Julian days)
      DO i = 1, NPER
        plen = PERLEN(i)*Mft_to_days
        IF ( ISSFLG(i)==1 ) THEN
          IF ( i/=1 )
     &         CALL error_stop('only first time step can be SS',
     &                         ERROR_time)
          Stress_dates(i) = Stress_dates(i) - INT( plen )
          KPER = 1
          CALL MFNWT_RDSTRESS()
          IF ( Init_vars_from_file==0 ) THEN
            Steady_state = 1
            CALL MFNWT_RUN(AFR, Diversions, Idivert, EXCHANGE, DELTAVOL,
     +                     LAKEVOL,Nsegshold,Nlakeshold,agDemand)    ! ITERATE TO SOLVE GW-SW SOLUTION FOR SS
            CALL MFNWT_OCBUDGET(agDemand, Diversions, Nsegshold)          ! CALCULATE BUDGET
            Steady_state = 0
 !           TOTIM = plen !RGN 9/4/2018 TOTIM needs to stay in MF time units
            TOTIM = PERLEN(i)  !RGN 9/4/2018 TOTIM needs to stay in MF time units
            IF ( ICNVG==0 ) THEN
              PRINT 222, KKITER
            ELSE
              PRINT 223, KKITER
            ENDIF
          ELSE ! call OC as SS is skipped
            CALL GWF2BAS7OC(1,1,1,IUNIT(12),IGRID)  !assumes only SP1 can be SS
          ENDIF
        ENDIF
        Stress_dates(i+1) = Stress_dates(i) + INT( plen )
      ENDDO
 222  FORMAT ( /, 'Steady state simulation did not converge ', I0)
 223  FORMAT ( /, 'Steady state simulation successful, used ', I0,
     &         ' iterations')

      Modflow_skip_stress = 0
      Modflow_skip_time_step = 0
      Modflow_skip_time = DBLE( start_jul - mfstrt_jul )
      Modflow_time_in_stress = Modflow_skip_time
      IF ( Modflow_skip_time>0.0D0 ) THEN
        kstpskip = 0.0D0
        time = 0.0D0
        DO i = 1, NPER
          IF ( ISSFLG(i)/=1 ) time = time + DBLE(PERLEN(i)*Mft_to_days)
          IF ( time<=Modflow_skip_time ) THEN     !RGN
!          IF ( time<Modflow_skip_time ) THEN   !RGN
            Modflow_skip_stress = i
            kstpskip = kstpskip + DBLE( PERLEN(i)*Mft_to_days )
            Modflow_skip_time_step = Modflow_skip_time_step + NSTP(i)
          ELSE
            EXIT
          ENDIF
        ENDDO
!        Modflow_time_in_stress = Modflow_time_in_stress - time   !RGN
        Modflow_time_in_stress = Modflow_skip_time - kstpskip
        IF ( Modflow_time_in_stress<0.0D0 ) Modflow_time_in_stress=0.0D0
        ! skip stress periods from modflow_time_zero to start_time
        IF ( Modflow_skip_stress - ISSFLG(1) == 0 ) THEN
          KPER = 1
          IF ( ISSFLG(1)==0 ) CALL MFNWT_RDSTRESS()
        ELSE
          nstress = INT( Modflow_skip_stress ) - ISSFLG(1)
          DO i = 1, nstress   !RGN because SP1 already read if SS during first period.
            KPER = KPER + 1 ! set to next stress period
            IF ( ISSFLG(KPER) == 0 ) CALL MFNWT_RDSTRESS()
            n = NSTP(KPER)
            IF ( i==nstress ) n = INT( Modflow_time_in_stress )
            DO KSTP = 1, n
              CALL GWF2BAS7OC(KSTP,KPER,1,IUNIT(12),IGRID)  !RGN 4/4/2018 skip through OC file
            END DO
          ENDDO
        ENDIF
        KPERSTART = KPER
        TOTIM = TOTIM + INT( Modflow_skip_time/Mft_to_days ) ! TOTIM includes SS time as set above, rsr
      ELSEIF ( Init_vars_from_file==0 .AND. ISSFLG(1)/=1) THEN
        !start with TR and no restart and no skip time
        KPER = KPER + 1 ! set to next stress period
        CALL MFNWT_RDSTRESS()           !RGN need to read first SP or
      ENDIF
      KSTP = INT( Modflow_time_in_stress ) ! caution, in days
      Modflow_skip_time_step = Modflow_skip_time_step + KSTP ! caution, in days

      ! read restart files to Modflow_time_in_stress
      IF ( Init_vars_from_file>0 ) THEN
        IF ( Iunit(69)==0 ) THEN
          PRINT 111
          CALL error_stop('Restart option active and no restart file'//
     +                    'listed in Name File. Model stopping.',
     +                    ERROR_restart)
        ENDIF
        CALL RESTART1READ()
      END IF
      OBSTART = Modflow_skip_time_step
      ITS = OBSTART
  111 FORMAT('Restart option active and no restart file listed in Name',
     +       ' file, simulation stopping ')
      END SUBROUTINE SET_STRESS_DATES
!
!***********************************************************************
!     Zero SFR and LAK arrays for specified precipitation, ET and runoff.
!     For GSFLOW simulations only.
!***********************************************************************
      SUBROUTINE ZERO_SPECIFIED_FLOWS(Iunitlak,Iunitsfr)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum
      USE PRMS_MODULE, ONLY: Print_debug
      USE GWFSFRMODULE, ONLY: NSTRM, STRM
      USE GWFLAKMODULE, ONLY: PRCPLK, EVAPLK, RNF, WTHDRW, NLAKES
      IMPLICIT NONE
! Arguments
      INTEGER, INTENT(IN) :: Iunitlak, Iunitsfr
      INTRINSIC :: ABS, SNGL
! Local Variables
      INTEGER :: i, j
      REAL :: TESTSFR, TESTLAK
!***********************************************************************
! Zero SFR flows (RUNOFF, ETSW, and PPTSW)
      TESTSFR = 0.0
      TESTLAK = 0.0
      IF ( Iunitsfr>0 ) THEN
        DO i = 1, NSTRM
          DO j = 12,14
            TESTSFR = TESTSFR + ABS(STRM(j,i))
            STRM(j,i) = 0.0
          END DO
        END DO
        IF ( TESTSFR>1.0 ) THEN
          IF ( Print_debug>DEBUG_minimum ) PRINT 10
        END IF
      END IF
! Zero LAK flows (PPT, EVAP, RUNOFF, SP.WITHDRAWL).
      IF ( Iunitlak>0 ) THEN
        DO i = 1, NLAKES
          TESTLAK = TESTLAK + SNGL(ABS(PRCPLK(i)) + ABS(EVAPLK(i))) +
     +              ABS(RNF(i)) + ABS(WTHDRW(i))
          IF ( TESTLAK>1.0 ) EXIT
        END DO
        PRCPLK = 0.0D0
        EVAPLK = 0.0D0
        RNF = 0.0
        WTHDRW = 0.0
        IF ( TESTLAK>1.0 ) THEN
          IF ( Print_debug>DEBUG_minimum ) PRINT 11
        END IF
      END IF

   10 FORMAT(/, '***WARNING***', /,
     +       'Non-zero values were specified for precipitation,',/,
     +       'streamflow, and ET for streams in MODFLOW input files.',/,
     +       'These values are set to zero for GSFLOW ',
     +       'simulations', /)
   11 FORMAT(/, '***WARNING***', /,
     +       'Non-zero values were specified for precipitation,',/,
     +       'streamflow, ET, and Sp.Flow for lakes in MODFLOW',/,
     +       'input files. These values are set to zero',/,
     +       'for GSFLOW simulations.', /)

      END SUBROUTINE ZERO_SPECIFIED_FLOWS

!***********************************************************************
! Set cell values
!***********************************************************************
      SUBROUTINE set_cell_values()
      USE PRMS_CONSTANTS, ONLY: NEARZERO, ERROR_dim
      USE GLOBAL, ONLY: NROW, NCOL, DELR, DELC
      USE GSFMODFLOW, ONLY: Cellarea, Gwc_col, Gwc_row
      USE PRMS_MODULE, ONLY: Ngwcell
      use prms_utils, only: error_stop
      IMPLICIT NONE
! Local Variables
      INTEGER :: i, irow, icell, icol, ierr
!***********************************************************************
      IF ( NROW*NCOL/=Ngwcell ) THEN
        PRINT *, '       dimension ngwcell not equal to NROW*NCOL',
     &           Ngwcell, NROW, NCOL
        CALL error_stop('Check for use of correct Parameter File',
     &                  ERROR_dim)
      ENDIF

      ierr = 0
      irow = 1
      DO i = 1, Ngwcell, NCOL
        icell = i
        DO icol = 1, NCOL
          Gwc_col(icell) = icol
          Gwc_row(icell) = irow
          Cellarea(icell) = DELR(icol)*DELC(irow)
          IF ( Cellarea(i)<NEARZERO ) THEN
            PRINT *, 'Cellarea = 0.0, irow, icol', irow, icol
            ierr = 1
          ENDIF
          icell = icell + 1
        ENDDO
        irow = irow + 1
      ENDDO
      IF ( ierr==1 )
     &     STOP 'ERROR, with DELR and DELC for computing cellarea'

      END SUBROUTINE set_cell_values

!***********************************************************************
! Check gvr_cell_pct when read from Parameter File
!***********************************************************************
      SUBROUTINE check_gvr_cell_pct()
      USE PRMS_CONSTANTS, ONLY: DEBUG_less, ERROR_param
      USE PRMS_MODULE, ONLY: Nhrucell, Ngwcell, Print_debug,
     &    Gvr_cell_id, Gvr_cell_pct
      USE GSFMODFLOW, ONLY: Gwc_row, Gwc_col, Ncells,
     &    Totalarea_mf
      USE GWFUZFMODULE, ONLY: IUZFBND
      IMPLICIT NONE
      INTRINSIC :: DBLE
! Local Variables
      INTEGER :: icell, ierr, i, irow, icol
      DOUBLE PRECISION :: pctdiff
      DOUBLE PRECISION, ALLOCATABLE :: cell_pct(:)
!***********************************************************************
      ALLOCATE ( cell_pct(Ngwcell) )
      cell_pct = 0.0D0
      ierr = 0
      DO i = 1, Nhrucell
        icell = Gvr_cell_id(i)
        IF ( icell==0 ) THEN
          PRINT *, 'ERROR, gvr_cell_id = 0 for gvr:', i
          PRINT *, 'Be sure gvr_cell_id is in the Parameter File'
          ierr = 1
          CYCLE
        ENDIF
        IF ( icell>Ngwcell ) THEN
          PRINT *, 'ERROR, gvr_cell_id > ngwcell for gvr:', i
          ierr = 1
          CYCLE
        ENDIF
        IF ( Gvr_cell_pct(i)<=0.0D0 ) THEN
          ierr = 1
          PRINT *, 'ERROR, gvr_cell_pct <= 0, cell:', i
          CYCLE
        ENDIF
!        cell_temp_pct(i) = Gvr_cell_pct(i)
        cell_pct(icell) = cell_pct(icell) + DBLE( Gvr_cell_pct(i) )
      ENDDO

      IF ( ierr==1 ) ERROR STOP ERROR_param

      Ncells = 0
      Totalarea_mf = 0.0D0
      DO i = 1, Ngwcell
        irow = Gwc_row(i)
        icol = Gwc_col(i)
        IF ( IUZFBND(icol,irow)==0 ) CYCLE
        IF ( cell_pct(i)>0.0D0 ) THEN
          pctdiff = cell_pct(i) - 1.0D0
          Totalarea_mf = Totalarea_mf + pctdiff
          Ncells = Ncells + 1
          IF ( Print_debug>DEBUG_less ) THEN
            IF ( pctdiff<-0.0000001D0 ) THEN
              PRINT *, 'WARNING, portion of cell in gvr_cell_pct',
     &                 ' mapping < 1.0 ', cell_pct(i), pctdiff
              PRINT *, 'Will lose some water in MF cell:', i
            ENDIF
            IF ( pctdiff>1.00001D0 ) THEN
              PRINT *, 'WARNING, portion of cell in gvr_cell_pct',
     &                 ' > 1.0 mapping', cell_pct(i), pctdiff
              PRINT *, 'Will make some water in MF cell:', i
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      DEALLOCATE ( cell_pct )

      END SUBROUTINE check_gvr_cell_pct

!***********************************************************************
! Set MODFLOW time factors
!***********************************************************************
      SUBROUTINE SETMFTIME()
      USE PRMS_CONSTANTS, ONLY: ERROR_control, ERROR_modflow
      use PRMS_CONTROL_FILE, only: control_integer_array
      USE PRMS_MODULE, ONLY: Starttime
      use prms_utils, only: error_stop
      USE GSFMODFLOW, ONLY: Mft_to_sec, Mft_to_days, Modflow_time_zero
      USE GLOBAL, ONLY: ITMUNI
      IMPLICIT NONE
      INTRINSIC :: SNGL
! Local Variables
      INTEGER :: j
!***********************************************************************
      ! get modflow_time_zero and determine julian day
      DO j = 1, 6
        IF ( control_integer_array(Modflow_time_zero(j),
     &       j, 'modflow_time_zero')/=0 ) THEN
          PRINT *, 'ERROR, modflow_time_zero, index:', j,
     &             'value: ', Modflow_time_zero(j)
          ERROR STOP ERROR_control
        ENDIF
        IF ( j==1 ) THEN
          IF ( Modflow_time_zero(1)<0 ) THEN
            Modflow_time_zero = Starttime
            PRINT '(/, A)',
     &     'WARNING, modflow_time_zero not specified, set to start_time'
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF ( ITMUNI==1 ) THEN
! Modflow in seconds
        Mft_to_sec = 1.0D0
      ELSEIF ( ITMUNI==2 ) THEN
! Modflow in minutes
        Mft_to_sec = 60.0D0
      ELSEIF ( ITMUNI==3 ) THEN
! Modflow in hours
        Mft_to_sec = 3600.0D0
      ELSEIF ( ITMUNI==4 ) THEN
! Modflow in days
        Mft_to_sec = 86400.0D0
      ELSEIF ( ITMUNI==5 ) THEN
! Modflow in years
!DANGER, not all years have 365 days
        Mft_to_sec = 86400.0D0*365.0D0
      ELSE
        CALL error_stop('invalid MODFLOW Time Unit', ERROR_modflow)
      ENDIF
      Mft_to_days = SNGL( Mft_to_sec/86400.0D0 )
      END SUBROUTINE SETMFTIME

!***********************************************************************
! Set conversion factors to go to and from PRMS and MF units
!***********************************************************************
      SUBROUTINE SETCONVFACTORS()
      USE PRMS_CONSTANTS, ONLY: FT2_PER_ACRE, MODFLOW, ERROR_modflow,
     &                          OFF,MODSIM_MODFLOW
      USE PRMS_MODULE, ONLY: Nhrucell, Gvr_cell_id, Model, Gvr_cell_pct,
     &    GSFLOW_flag
      use prms_utils, only: error_stop
      USE GLOBAL, ONLY: ITMUNI, LENUNI, IOUT
      USE GWFBASMODULE, ONLY: DELT
      USE GSFMODFLOW, ONLY: Mft_to_sec, Cellarea, MFQ_to_inch_acres,
     &    Mfl2_to_acre, Mfl3_to_ft3, Mfl_to_inch, Sfr_conv,
     &    Acre_inches_to_mfl3, Inch_to_mfl_t, Mfl3t_to_cfs,
     &    Mfvol2inch_conv, Gvr2cell_conv, Mfq2inch_conv,
     &    Acre_inches_to_mfl3_sngl, MFQ_to_inch_acres_dble
      IMPLICIT NONE
! Local Variables
      REAL :: inch_to_mfl
      INTEGER :: i
!***********************************************************************
      IF ( LENUNI<1 .OR. ITMUNI<1 .OR. LENUNI>3 .OR. ITMUNI>6 ) THEN
        IF ( (Model==MODFLOW .or. Model==MODSIM_MODFLOW) .AND. 
     &       (LENUNI==0.OR.ITMUNI==0) ) RETURN
        WRITE ( IOUT, 9001 ) LENUNI, ITMUNI
        PRINT 9001, LENUNI, ITMUNI
        ERROR STOP ERROR_modflow
      ENDIF

      IF ( LENUNI==1 ) THEN
! Modflow in feet
        inch_to_mfl = 1.0/12.0
        Mfl2_to_acre = 1.0D0
        Mfl3_to_ft3 = 1.0D0

      ELSEIF ( LENUNI==2 ) THEN
! Modflow in meters
        inch_to_mfl = 0.0254
        Mfl2_to_acre = 3.280839895D0*3.280839895D0
        Mfl3_to_ft3 = 3.280839895D0**3.0D0

      ELSEIF ( LENUNI==3 ) THEN
! Modflow in centimeters
        inch_to_mfl = 2.54
        Mfl2_to_acre = 328.0839895D0 * 328.0839895D0
        Mfl3_to_ft3 = 328.0839895D0**3.0D0
      ELSE
        CALL error_stop('invalid MODFLOW Length unit', ERROR_modflow)
      ENDIF
      Mfl_to_inch = 1.0/inch_to_mfl
      Mfl2_to_acre = Mfl2_to_acre/FT2_PER_ACRE
      Inch_to_mfl_t = inch_to_mfl/DELT  ! will need to move if DELT allowed to change
      MFQ_to_inch_acres = SNGL( Mfl2_to_acre) * DELT * Mfl_to_inch
      MFQ_to_inch_acres_dble = DBLE( MFQ_to_inch_acres )

      Sfr_conv = Mft_to_sec/Mfl3_to_ft3
      Mfl3t_to_cfs = Mfl3_to_ft3/Mft_to_sec
! inch over basin (acres) conversion to modflow length cubed
      Acre_inches_to_mfl3 = FT2_PER_ACRE/(Mfl3_to_ft3*12.0D0)
      Acre_inches_to_mfl3_sngl = SNGL( Acre_inches_to_mfl3 )

      IF ( GSFLOW_flag==OFF ) RETURN

      DO i = 1, Nhrucell
        ! MF volume to PRMS inches
        Mfvol2inch_conv(i) = Mfl_to_inch/Cellarea(Gvr_cell_id(i))
        ! MF discharge to PRMS inches
        ! note DELT may change during simulation at some point, so this will need to go in MFNWT_RDSTRESS
        Mfq2inch_conv(i) = Mfvol2inch_conv(i)*DELT
        ! PRMS inches in a gravity-flow reservoir to MF rate
        Gvr2cell_conv(i) = Gvr_cell_pct(i)*Inch_to_mfl_t
      ENDDO

 9001 FORMAT (' Units are undefined. LENUNI and ITMUNI must be > 0', /,
     +        ' Lenuni = ', I0, '; Itmuni = ', I0)

      END SUBROUTINE SETCONVFACTORS

!***********************************************************************
!     gsflow_modflow_restart - write or read restart file
!***********************************************************************
      SUBROUTINE gsflow_modflow_restart(In_out)
      USE PRMS_CONSTANTS, ONLY: DEBUG_minimum, SAVE_INIT, OFF
      USE PRMS_MODULE, ONLY: Restart_outunit, Restart_inunit,
     &                       Print_debug, text_restart_flag
      use prms_utils, only: check_restart
      USE GSFMODFLOW, ONLY: MODNAME, Modflow_time_zero
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
      ! Argument
      INTEGER, INTENT(IN) :: In_out
      ! Local Variables
      CHARACTER(LEN=14) :: module_name
      INTEGER :: MF_time_zero(6)
!***********************************************************************
      IF ( In_out==SAVE_INIT ) THEN
        IF ( text_restart_flag==OFF ) THEN
          WRITE ( Restart_outunit ) MODNAME
          WRITE ( Restart_outunit ) DELT, Modflow_time_zero
        ELSE
          WRITE ( Restart_outunit, * ) MODNAME
          WRITE ( Restart_outunit, * ) DELT, Modflow_time_zero
        ENDIF
      ELSE
        IF ( text_restart_flag==OFF ) THEN
          READ ( Restart_inunit ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit ) DELT, MF_time_zero
        ELSE
          READ ( Restart_inunit, * ) module_name
          CALL check_restart(MODNAME, module_name)
          READ ( Restart_inunit, * ) DELT, MF_time_zero
        ENDIF
        IF ( Print_debug>DEBUG_minimum ) PRINT 4,
     &       'modflow_time_zero of Restart File:', MF_time_zero(1),
     &       MF_time_zero(2), MF_time_zero(3), MF_time_zero(4),
     &       MF_time_zero(5), MF_time_zero(6)
    4   FORMAT (/, A, I5, 2('/',I2.2), I3.2, 2(':', I2.2) )
      ENDIF
      END SUBROUTINE gsflow_modflow_restart

      END MODULE MF_DLL

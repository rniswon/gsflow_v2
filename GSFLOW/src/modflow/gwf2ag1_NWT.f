      MODULE GWFAGMODULE
        DOUBLE PRECISION, PARAMETER :: zerod30 = 1.0d-30
        DOUBLE PRECISION, PARAMETER :: NEARZERO = 1.0D-17
        DOUBLE PRECISION, PARAMETER :: zerod7 = 1.0d-7
        DOUBLE PRECISION, PARAMETER :: done = 1.0d0
        DOUBLE PRECISION, PARAMETER :: DONEP = 1.00001d0
        REAL, PARAMETER :: sone = 1.0e0
        REAL, PARAMETER :: szero = 0.0e0
        DOUBLE PRECISION, PARAMETER :: dtwo = 2.0d0
        DOUBLE PRECISION, PARAMETER :: doneneg = -1.0d0
        DOUBLE PRECISION, PARAMETER :: zerod1 = 1.0d-1
        DOUBLE PRECISION, PARAMETER :: zerod2 = 1.0d-2
        DOUBLE PRECISION, PARAMETER :: zerod3 = 1.0d-3
        DOUBLE PRECISION, PARAMETER :: dzero = 0.0d0
        INTEGER, SAVE, POINTER :: NWELLS, MXWELL, NWELVL, NPWEL, IPRWEL
        INTEGER, SAVE, POINTER :: IWELLCB, IRDPSI, NNPWEL, NAUXWELL
        INTEGER, SAVE, POINTER :: IWELLCBU, ISFRCB, ICBSUP
        INTEGER, SAVE, POINTER :: IPONDCB, IPONDCBU  !DS
        INTEGER, SAVE, POINTER :: IRRWELLCB, IRRSFRCB, IRRPONDCB
        LOGICAL, SAVE, POINTER :: TSACTIVEGW, TSACTIVESW
        LOGICAL, SAVE, POINTER :: TSACTIVEGWET, TSACTIVESWET
        LOGICAL, SAVE, POINTER :: TSACTIVEPOND, TSACTIVEPONDET  !DS
        LOGICAL, SAVE, POINTER :: TSACTIVEALLPOND, TSACTIVEALLPONDET  !DS
        INTEGER, SAVE, POINTER :: NUMSW, NUMGW, NUMSWET, NUMGWET
        INTEGER, SAVE, POINTER :: NUMPOND, NUMPONDET
        INTEGER, SAVE, POINTER :: TSGWETALLUNIT, TSGWALLUNIT
        INTEGER, SAVE, POINTER :: TSSWETALLUNIT, TSSWALLUNIT
        INTEGER, SAVE, POINTER :: TSPONDETALLUNIT, TSPONDALLUNIT
        INTEGER, SAVE, POINTER :: NSEGDIMTEMP
        CHARACTER(LEN=16), SAVE, DIMENSION(:), POINTER :: WELAUX
        CHARACTER(LEN=16), SAVE, DIMENSION(:), POINTER :: PONDAUX
!        CHARACTER(LEN=16), SAVE, DIMENSION(:), POINTER :: SFRAUX
        REAL, SAVE, DIMENSION(:, :), POINTER :: WELL
        REAL, SAVE, DIMENSION(:, :), POINTER :: POND
        REAL, SAVE, DIMENSION(:), POINTER :: PONDSTORNEW
        REAL, SAVE, DIMENSION(:), POINTER :: PONDSTOROLD
        REAL, SAVE, DIMENSION(:, :), POINTER :: TABTIMEWELL
        REAL, SAVE, DIMENSION(:, :), POINTER :: TABRATEWELL
        REAL, SAVE, DIMENSION(:, :), POINTER :: TABTIMEPOND
        REAL, SAVE, DIMENSION(:, :), POINTER :: TABRATEPOND
        REAL, SAVE, DIMENSION(:), POINTER :: QONLY
        REAL, SAVE, DIMENSION(:), POINTER :: QONLYOLD
        REAL, SAVE, DIMENSION(:), POINTER :: PONDFLOW
        REAL, SAVE, DIMENSION(:), POINTER :: PONDSEGFLOW
        REAL, SAVE, DIMENSION(:), POINTER :: PONDFLOWOLD
        REAL, SAVE, DIMENSION(:), POINTER :: PONDFLOWMAX
        REAL, SAVE, DIMENSION(:), POINTER :: PONDSEGFRAC
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABLAYWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABROWWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABCOLWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABVALWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABIDWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABUNITWELL
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABPONDHRU
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABVALPOND
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABIDPOND
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABUNITPOND
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSSWUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSSWNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSGWUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSGWNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSSWETUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSGWETUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSSWETNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSGWETNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSPONDUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSPONDNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSPONDETUNIT
        INTEGER, SAVE, DIMENSION(:), POINTER :: TSPONDETNUM
        INTEGER, SAVE, DIMENSION(:), POINTER :: LASTREACH
        INTEGER, SAVE, DIMENSION(:), POINTER :: SEGLIST
        INTEGER, SAVE, POINTER :: NUMSEGLIST
        REAL, SAVE, POINTER :: PSIRAMP
        REAL, SAVE, POINTER :: ACCEL
        REAL, SAVE, POINTER :: AGTOL
        INTEGER, SAVE, POINTER :: MAXAGITER
        INTEGER, SAVE, POINTER :: IUNITRAMP
        INTEGER, SAVE, POINTER :: NUMTABWELL
        INTEGER, SAVE, POINTER :: NUMTABPOND
        INTEGER, SAVE, POINTER :: MAXVALWELL
        INTEGER, SAVE, POINTER :: MAXVALPOND
        REAL, SAVE, DIMENSION(:, :), POINTER :: VBVLAG
        CHARACTER(LEN=22), SAVE, DIMENSION(:), POINTER :: VBNMAG
        INTEGER, SAVE, POINTER :: MSUMAG
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: DIVERSIONSEG
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: IRRROW_GW
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: IRRCOL_GW
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: IRRHRU_POND   !DS
        INTEGER, SAVE, DIMENSION(:), POINTER :: FLOWTHROUGH_POND   !DS
        REAL, SAVE, DIMENSION(:), POINTER :: IRRPERIODWELL
        REAL, SAVE, DIMENSION(:), POINTER :: IRRPERIODPOND   !DS
        REAL, SAVE, DIMENSION(:), POINTER :: IRRPERIODSEG
        REAL, SAVE, DIMENSION(:), POINTER :: TRIGGERPERIODWELL
        REAL, SAVE, DIMENSION(:), POINTER :: TRIGGERPERIODPOND  !DS
        REAL, SAVE, DIMENSION(:), POINTER :: TRIGGERPERIODSEG
        REAL, SAVE, DIMENSION(:), POINTER :: TIMEINPERIODSEG
        REAL, SAVE, DIMENSION(:), POINTER :: TIMEINPERIODWELL
        REAL, SAVE, DIMENSION(:), POINTER :: TIMEINPERIODPOND  !DS
        REAL, SAVE, DIMENSION(:), POINTER :: AETITERSW, RMSESW
        REAL, SAVE, DIMENSION(:), POINTER :: AETITERGW, RMSEGW
        REAL, SAVE, DIMENSION(:), POINTER :: AETITERPOND, PETPOND
        REAL, SAVE, DIMENSION(:), POINTER :: RMSEPOND
        REAL, SAVE, DIMENSION(:, :), POINTER :: WELLIRRUZF
        REAL, SAVE, DIMENSION(:, :), POINTER :: WELLIRRPRMS
        REAL, SAVE, DIMENSION(:, :), POINTER :: IRRFACT
        REAL, SAVE, DIMENSION(:, :), POINTER :: IRRFACTPOND    !DS
        REAL, SAVE, DIMENSION(:, :), POINTER :: IRRFIELDFACT
        REAL, SAVE, DIMENSION(:, :), POINTER :: IRRFIELDFACTPOND  !DS
        INTEGER, SAVE, DIMENSION(:), POINTER :: SUPWELVAR
        REAL, SAVE, DIMENSION(:), POINTER :: SUPFLOW
        REAL, SAVE, DIMENSION(:), POINTER :: SUPSEG
        INTEGER, SAVE, DIMENSION(:), POINTER :: IRRWELVAR
        INTEGER, SAVE, DIMENSION(:), POINTER :: IRRPONDVAR   !DS
        INTEGER, SAVE, DIMENSION(:), POINTER :: TABPONDSEG   !DS
        REAL, SAVE, DIMENSION(:), POINTER :: TABPONDFRAC !DS
        REAL, SAVE, DIMENSION(:, :), POINTER :: FRACSUP
        REAL, SAVE, DIMENSION(:, :), POINTER :: FRACSUPMAX
        INTEGER, SAVE, POINTER :: NUMSUP
        INTEGER, SAVE, POINTER :: NUMSUPSP
        INTEGER, SAVE, POINTER :: UNITSUP
        INTEGER, SAVE, POINTER :: NUMIRRWEL
        INTEGER, SAVE, POINTER :: NUMIRRPOND
        INTEGER, SAVE, POINTER :: UNITIRRWEL
        INTEGER, SAVE, DIMENSION(:), POINTER :: NUMSUPWELLSEG
        INTEGER, SAVE, POINTER :: MAXSEGS
        INTEGER, SAVE, POINTER :: MAXCELLSWEL
        INTEGER, SAVE, POINTER :: MAXCELLSPOND  !DS
        INTEGER, SAVE, POINTER :: NUMIRRWELSP
        INTEGER, SAVE, POINTER :: NUMIRRPONDSP   !DS
        INTEGER, SAVE, DIMENSION(:), POINTER :: NUMCELLS
        INTEGER, SAVE, DIMENSION(:), POINTER :: NUMCELLSPOND  !DS
        INTEGER, SAVE, DIMENSION(:), POINTER :: NUMSEGS
        INTEGER, SAVE, POINTER :: ETDEMANDFLAG
        INTEGER, SAVE, POINTER :: TRIGGERFLAG
        INTEGER, SAVE, POINTER :: NUMIRRDIVERSION, UNITIRRDIVERSION
        INTEGER, SAVE, POINTER :: MAXCELLSDIVERSION, NUMIRRDIVERSIONSP
        INTEGER, SAVE, POINTER :: IDVFLG
        INTEGER, SAVE, DIMENSION(:), POINTER :: DVRCH
        INTEGER, SAVE, DIMENSION(:), POINTER :: IRRSEG
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: IRRROW_SW
        INTEGER, SAVE, DIMENSION(:, :), POINTER :: IRRCOL_SW
        REAL, SAVE, DIMENSION(:, :), POINTER :: DIVERSIONIRRUZF
        REAL, SAVE, DIMENSION(:, :), POINTER :: DIVERSIONIRRPRMS
        REAL, SAVE, DIMENSION(:, :), POINTER :: PONDIRRPRMS
        REAL, SAVE, DIMENSION(:, :), POINTER :: DVRPERC
        REAL, SAVE, DIMENSION(:, :), POINTER :: DVEFF
        REAL, SAVE, DIMENSION(:, :), POINTER :: KCROPDIVERSION
        REAL, SAVE, DIMENSION(:, :), POINTER :: KCROPWELL
        REAL, SAVE, DIMENSION(:), POINTER :: DEMAND, SUPACT, SUPACTOLD
        REAL, SAVE, DIMENSION(:), POINTER :: ACTUAL
        REAL, SAVE, DIMENSION(:), POINTER :: ACTUALOLD
        INTEGER, SAVE, POINTER :: KPEROLD
      END MODULE GWFAGMODULE

      SUBROUTINE GWF2AG7AR(IN, IUNITSFR, IUNITNWT)
      !******************************************************************
      ! ALLOCATE ARRAY STORAGE FOR AG PACKAGE
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT, NCOL, NROW
      USE GWFAGMODULE
      USE GWFSFRMODULE, ONLY: NSEGDIM
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS
      INTEGER, INTENT(IN) :: IN, IUNITSFR, IUNITNWT
      ! - -----------------------------------------------------------------
      ! VARIABLES
      !CHARACTER(len=200) :: LINE
      INTEGER :: MXACTWSUP, MXACTWIRR, NUMSUPHOLD, NUMIRRHOLD
      INTEGER :: MAXSEGSHOLD, NUMCOLS, NUMROWS, MAXCELLSHOLD
      INTEGER :: NUMCELLSHOLD
      INTEGER :: NUMTABHOLD, max, maxirr, maxcellpond
      ! - -----------------------------------------------------------------
      !
      !1 - --- ALLOCATE ALLOCATE CONSTANTS AND FLAGS
      ALLOCATE (VBVLAG(4, 10), VBNMAG(10), MSUMAG)
      ALLOCATE (NWELLS, MXWELL, NWELVL, IWELLCB, ISFRCB, NAUXWELL)
      ALLOCATE (WELAUX(20))
      ALLOCATE (IRRWELLCB, IRRSFRCB, IWELLCBU, ICBSUP)
      ALLOCATE (IPONDCB, IPONDCBU, IRRPONDCB)
      ALLOCATE (PSIRAMP, IUNITRAMP, ACCEL, AGTOL, MAXAGITER)
      ALLOCATE (NUMTABWELL, MAXVALWELL, NPWEL, NNPWEL, IPRWEL)
      ALLOCATE (TSACTIVEGW, TSACTIVESW, NUMSW, NUMGW)
      ALLOCATE (TSACTIVEGWET, TSACTIVESWET, NUMSWET, NUMGWET)
      ALLOCATE (TSACTIVEPOND, TSACTIVEPONDET)
      ALLOCATE (TSACTIVEALLPOND, TSACTIVEALLPONDET)
      ALLOCATE (TSGWALLUNIT, TSGWETALLUNIT, NSEGDIMTEMP)
      ALLOCATE (TSSWALLUNIT, TSSWETALLUNIT)
      ALLOCATE (TSPONDALLUNIT, TSPONDETALLUNIT)
      ALLOCATE (NUMPOND, NUMPONDET, NUMTABPOND, MAXVALPOND)
      ALLOCATE (KPEROLD)
      VBVLAG = szero
      MSUMAG = 0
      PSIRAMP = 0.10
      ACCEL = sone
      AGTOL = 0.10
      MAXAGITER = 20
      NUMTABWELL = 0
      NUMTABPOND = 0
      MAXVALWELL = 1
      MAXVALPOND = 1
      IPRWEL = 1
      NAUXWELL = 0
      NSEGDIMTEMP = 1
      TSACTIVEGW = .FALSE.
      TSACTIVESW = .FALSE.
      TSACTIVEGWET = .FALSE.
      TSACTIVESWET = .FALSE.
      TSACTIVEPOND = .FALSE.
      TSACTIVEPONDET = .FALSE.
      TSACTIVEALLPOND = .FALSE.
      TSACTIVEALLPONDET = .FALSE.
      NUMSW = 0
      NUMGW = 0
      NUMSWET = 0
      NUMGWET = 0
      NUMPOND = 0
      NUMPONDET = 0
      TSGWETALLUNIT = 0
      TSGWALLUNIT = 0
      TSSWETALLUNIT = 0
      TSSWALLUNIT = 0
      TSPONDALLUNIT = 0
      TSPONDETALLUNIT = 0
      WELAUX = ' '
      KPEROLD = 0
      ALLOCATE (NUMSUP, NUMIRRWEL, UNITSUP, MAXCELLSWEL)
      ALLOCATE (NUMSUPSP, MAXSEGS, NUMIRRWELSP)
      ALLOCATE (ETDEMANDFLAG, NUMIRRDIVERSION, NUMIRRDIVERSIONSP)
      ALLOCATE (MAXCELLSDIVERSION, TRIGGERFLAG)
      ALLOCATE (NUMIRRPOND, MAXCELLSPOND)
      NWELLS = 0
      NNPWEL = 0
      MXWELL = 0
      NWELVL = 0
      IWELLCB = 0
      IWELLCBU = 0
      ICBSUP = 0
      IPONDCB = 0
      IPONDCBU = 0
      IRRPONDCB = 0
      ISFRCB = 0
      IRRWELLCB = 0
      IRRSFRCB = 0
      IUNITRAMP = IOUT
      NUMSUP = 0
      NUMSUPSP = 0
      NUMIRRWEL = 0
      UNITSUP = 0
      MAXSEGS = 0
      MAXCELLSWEL = 0
      MAXCELLSDIVERSION = 0
      NUMIRRWELSP = 0
      ETDEMANDFLAG = 0
      NUMIRRDIVERSION = 0
      NUMIRRDIVERSIONSP = 0
      TRIGGERFLAG = 0
      NUMIRRPOND = 0
      MAXCELLSPOND = 0
      !
      !2 - --- IDENTIFY PACKAGE AND INITIALIZE AG OPTIONS.
      WRITE (IOUT, 1) IN
1     FORMAT(1X, /1X, 'AG -- AG PACKAGE FOR NWT VERSION 1.3.0, ',
     +     ' 3/03/2020 INPUT READ FROM UNIT ', I4)
      !
      !3 - --- CHECK FOR KEYWORDS.
      CALL PARSEAG7OPTIONS(In, Iout, Iunitnwt)
      NNPWEL = MXWELL
      !
      !4 - --- ALLOCATE ARRAYS FOR TIME SERIES OUTPUT
      IF (IUNITSFR > 0) NSEGDIMTEMP = NSEGDIM
      ALLOCATE (TSSWUNIT(NSEGDIMTEMP), TSGWUNIT(MXWELL), QONLY(MXWELL))
      ALLOCATE (QONLYOLD(MXWELL))
      ALLOCATE (TSGWNUM(MXWELL), TSSWNUM(NSEGDIMTEMP))
      ALLOCATE (TSSWETUNIT(NSEGDIMTEMP), TSGWETUNIT(MXWELL))
      ALLOCATE (TSSWETNUM(NSEGDIMTEMP), TSGWETNUM(MXWELL))
      ALLOCATE (SUPSEG(NSEGDIMTEMP))
      ALLOCATE (LASTREACH(NSEGDIMTEMP))
      ALLOCATE (IRRPERIODWELL(MXWELL), IRRPERIODSEG(NSEGDIMTEMP))
      ALLOCATE (TRIGGERPERIODWELL(MXWELL))
      ALLOCATE (TRIGGERPERIODSEG(NSEGDIMTEMP))
      ALLOCATE (TIMEINPERIODWELL(MXWELL), TIMEINPERIODSEG(NSEGDIMTEMP))
      ALLOCATE (SEGLIST(NSEGDIMTEMP), NUMSEGLIST)
      maxirr = NUMIRRPOND
      IF ( maxirr<1 ) maxirr = 1
      maxcellpond = MAXCELLSPOND
      IF ( maxcellpond<1 ) maxcellpond = 1
      ALLOCATE (TIMEINPERIODPOND(maxirr))
      ALLOCATE (AETITERPOND(maxirr),PETPOND(maxirr))
      ALLOCATE (PONDFLOW(maxirr),PONDFLOWOLD(maxirr))
      ALLOCATE (PONDFLOWMAX(maxirr), PONDSEGFLOW(maxirr))
      ALLOCATE (PONDIRRPRMS(maxcellpond,maxirr))
      ALLOCATE (TSPONDUNIT(maxirr),TSPONDNUM(maxirr))
      ALLOCATE (TSPONDETUNIT(maxirr),TSPONDETNUM(maxirr))
      ALLOCATE (IRRPONDVAR(maxirr),TABPONDSEG(maxirr))
      ALLOCATE (TABPONDFRAC(maxirr))
      ALLOCATE (NUMCELLSPOND(maxirr),IRRPERIODPOND(maxirr))
      ALLOCATE (TRIGGERPERIODPOND(maxirr))
      ALLOCATE (FLOWTHROUGH_POND(maxirr))
      ALLOCATE (NUMIRRPONDSP,IRRFIELDFACTPOND(maxcellpond,maxirr)) 
      ALLOCATE (IRRFACTPOND(maxcellpond,maxirr))
      ALLOCATE (IRRHRU_POND(maxcellpond,maxirr))
      ALLOCATE (RMSEPOND(maxirr))
      ALLOCATE (PONDSTORNEW(maxirr),PONDSTOROLD(maxirr))
      IF ( NUMIRRPOND > 0 ) THEN
        ALLOCATE(PONDSEGFRAC(NSEGDIMTEMP))
      ELSE
        ALLOCATE(PONDSEGFRAC(1))
      ENDIF
      PONDSEGFRAC = 0.0
      NUMIRRPONDSP = 0
      IRRFIELDFACTPOND = szero
      IRRFACTPOND = szero
      IRRHRU_POND = 0
      TSSWNUM = 0
      TSGWNUM = 0
      QONLY = szero
      QONLYOLD = szero
      TSSWETUNIT = 0
      TSGWETUNIT = 0
      TSSWETNUM = 0
      TSGWETNUM = 0
      TSPONDUNIT = 0
      TSPONDNUM = 0
      TSPONDETUNIT = 0
      TSPONDETNUM = 0
      SUPSEG = szero
      LASTREACH = 0
      IRRPERIODWELL = szero
      IRRPERIODSEG = szero
      TRIGGERPERIODWELL = szero
      TRIGGERPERIODSEG = szero
      TIMEINPERIODWELL = 1E30
      TIMEINPERIODSEG = 1E30
      TIMEINPERIODPOND = 1E30
      TRIGGERPERIODPOND = szero
      FLOWTHROUGH_POND = 0
      IRRPERIODPOND = szero
      AETITERPOND = szero
      PETPOND = szero
      PONDFLOW = szero
      PONDSEGFLOW = szero
      PONDFLOWOLD = szero
      PONDFLOWMAX = szero
      PONDIRRPRMS = szero
      SEGLIST = 0
      NUMSEGLIST = 0
      IRRPONDVAR = 0
      TABPONDSEG = 0
      TABPONDFRAC = szero
      NUMCELLSPOND = 0
      IRRPERIODPOND = 0
      TRIGGERPERIODPOND = 0
      RMSEPOND = szero
      PONDSTORNEW = szero
      PONDSTOROLD = szero
      !
      !5 - --- ALLOCATE TIME SERIES VARIABLES
      IF (TSACTIVEGW .OR. TSACTIVESW .OR. TSACTIVEGWET .OR.
     +    TSACTIVESWET .OR. TSACTIVEPONDET .OR. TSACTIVEPOND ) 
     +    CALL TSREAD(IN, IOUT)
      !
      !6 - --- ALLOCATE VARIABLES FOR TIME SERIES WELL INPUT RATES
      NUMTABHOLD = NUMTABWELL
      IF (NUMTABHOLD .EQ. 0) NUMTABHOLD = 1
      max = MAXVALWELL
      IF ( max<1 ) max = 1
      ALLOCATE (TABTIMEWELL(max, NUMTABHOLD))
      ALLOCATE (TABRATEWELL(max, NUMTABHOLD))
      IF ( max<1 ) max = 1
      ALLOCATE (TABLAYWELL(max), TABROWWELL(max))
      ALLOCATE (TABVALWELL(max), TABIDWELL(max))
      ALLOCATE (TABCOLWELL(max),TABUNITWELL(max))
      TABTIMEWELL = szero
      TABRATEWELL = szero
      TABLAYWELL = 0
      TABROWWELL = 0
      TABCOLWELL = 0
      TABVALWELL = 0
      TABIDWELL = 0
      TABUNITWELL = 0
      !
      !6B - --- ALLOCATE VARIABLES FOR TIME SERIES POND INPUT RATES
      NUMTABHOLD = NUMTABPOND
      IF (NUMTABHOLD .EQ. 0) NUMTABHOLD = 1
      max = MAXVALPOND
      IF ( max<1 ) max = 1
      ALLOCATE (TABTIMEPOND(max, NUMTABHOLD))
      ALLOCATE (TABRATEPOND(max, NUMTABHOLD))
      ALLOCATE (TABVALPOND(maxirr),TABIDPOND(maxirr))
      ALLOCATE (TABPONDHRU(maxirr),TABUNITPOND(maxirr))
      TABTIMEPOND = szero
      TABRATEPOND = szero
      TABPONDHRU = 0
      TABVALPOND = 0
      TABIDPOND = 0
      TABUNITPOND = 0
      !
      !7 - --- THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
      !7 - --- CELL - BY - CELL FLOW.
      NWELVL = 5 + NAUXWELL
      !
      !8 - ---ALLOCATE SPACE FOR THE WELL POND DATA
      IF (MXWELL .LT. 1) THEN
         WRITE (IOUT, 17)
17       FORMAT(1X,
     +        'No wells active in the AG Package')
         MXWELL = 1
      END IF
      ALLOCATE (WELL(NWELVL, MXWELL))
      WELL = 0.0
      IF (NUMIRRPOND .LT. 1) THEN
         WRITE (IOUT, 18)
18       FORMAT(1X,
     +        'No ponds active in the AG Package')
        ! NUMIRRPOND = 1
      END IF
      ALLOCATE (POND(4, maxirr))
      !
      !9 - --- ALLOCATE SUPPLEMENTAL AND IRRIGATION WELL ARRAYS
      NUMSUPHOLD = NUMSUP
      MXACTWSUP = MXWELL
      MXACTWIRR = MXWELL
      NUMSUPHOLD = NUMSUP
      NUMIRRHOLD = NUMIRRWEL
      MAXSEGSHOLD = MAXSEGS
      NUMCOLS = NCOL
      NUMROWS = NROW
      MAXCELLSHOLD = MAXCELLSWEL
      IF (NUMSUPHOLD .EQ. 0) THEN
         NUMSUPHOLD = 1
         MXACTWSUP = 1
         MAXSEGSHOLD = 1
      END IF
      ALLOCATE (DEMAND(NSEGDIMTEMP), ACTUAL(NSEGDIMTEMP))
      ALLOCATE (ACTUALOLD(NSEGDIMTEMP))
      ALLOCATE (SUPACT(NSEGDIMTEMP), SUPACTOLD(NSEGDIMTEMP))
      IF (NUMIRRHOLD .EQ. 0) THEN
         MAXCELLSHOLD = 1
         NUMIRRHOLD = 1
         MXACTWIRR = 1
         NUMCELLSHOLD = 1
         NUMCOLS = 1
         NUMROWS = 1
         MAXCELLSWEL = 1
      END IF
      ALLOCATE (DIVERSIONSEG(MAXSEGSHOLD, MXACTWSUP))
      ALLOCATE (SUPWELVAR(NUMSUPHOLD), NUMSEGS(MXWELL))
      ALLOCATE (FRACSUP(MAXSEGSHOLD, MXACTWSUP))
      ALLOCATE (FRACSUPMAX(MAXSEGSHOLD, MXACTWSUP))
      ALLOCATE (KCROPWELL(MAXSEGSHOLD, MXACTWSUP))
      ALLOCATE (IRRROW_GW(MAXCELLSHOLD, MXACTWIRR))
      ALLOCATE (IRRCOL_GW(MAXCELLSHOLD, MXACTWIRR))
      ALLOCATE (IRRWELVAR(NUMIRRHOLD))
      ALLOCATE (WELLIRRUZF(NUMCOLS, NUMROWS), NUMCELLS(MXACTWIRR))
      ALLOCATE (WELLIRRPRMS(MAXCELLSHOLD, MXACTWIRR))
      ALLOCATE (AETITERGW(MXWELL),RMSEGW(MXWELL))
      ALLOCATE (IRRFACT(MAXCELLSHOLD, MXACTWIRR))
      ALLOCATE (IRRFIELDFACT(MAXCELLSHOLD, MXACTWIRR))
      ALLOCATE (SUPFLOW(MXACTWSUP))
      ALLOCATE (NUMSUPWELLSEG(MXACTWSUP))
      DIVERSIONSEG = 0
      IRRROW_GW = 0
      IRRCOL_GW = 0
      SUPWELVAR = 0
      IRRWELVAR = 0
      WELLIRRUZF = szero
      WELLIRRPRMS = szero
      NUMCELLS = 0
      IRRFACT = szero
      IRRFIELDFACT = szero
      NUMSEGS = 0
      FRACSUP = szero
      FRACSUPMAX = szero
      SUPFLOW = szero
      AETITERGW = szero
      RMSEGW = szero
      KCROPWELL = szero
      NUMSUPWELLSEG = 0
      !
      !10 - --- ALLOCATE VARIABLES FOR DIVERSIONS
      IF (NUMIRRDIVERSION > 0) THEN
         ALLOCATE (DVRCH(NSEGDIMTEMP))
         ALLOCATE (DVEFF(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (KCROPDIVERSION(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (IRRROW_SW(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (IRRCOL_SW(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (AETITERSW(NSEGDIMTEMP), RMSESW(NSEGDIMTEMP))
         ALLOCATE (DVRPERC(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (DIVERSIONIRRUZF(NCOL, NROW))
         ALLOCATE (DIVERSIONIRRPRMS(MAXCELLSDIVERSION, NSEGDIMTEMP))
         ALLOCATE (IRRSEG(NSEGDIMTEMP))
      ELSE
         ALLOCATE (DVRCH(1), DVEFF(1, 1), KCROPDIVERSION(1, 1))
         ALLOCATE (IRRROW_SW(1, 1), IRRCOL_SW(1, 1))
         ALLOCATE (DVRPERC(1, 1))
         ALLOCATE (DIVERSIONIRRUZF(1, 1), DIVERSIONIRRPRMS(1, 1))
         ALLOCATE (IRRSEG(1), AETITERSW(1),RMSESW(1))
      END IF
      DVRCH = 0
      DVEFF = szero
      KCROPDIVERSION = szero
      IRRROW_SW = 0
      IRRCOL_SW = 0
      AETITERSW = szero
      RMSESW = szero
      DIVERSIONIRRUZF = szero
      DIVERSIONIRRPRMS = szero
      DVRPERC = szero
      ALLOCATE (IDVFLG)
      IDVFLG = 0
      DEMAND = szero
      SUPACT = szero
      SUPACTOLD = szero
      ACTUAL = szero
      ACTUALOLD = szero
      RETURN
      END SUBROUTINE
      !
      SUBROUTINE PARSEAG7OPTIONS(IN, IOUT, IUNITNWT)
      !******************************************************************
      ! READ AG OPTIONS
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IUNIT, NPER, ISSFLG
      USE GWFAGMODULE
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      USE GWFUZFMODULE, ONLY: IETFLG
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS
      INTEGER, INTENT(IN) :: IN, IOUT, IUNITNWT
      ! - -----------------------------------------------------------------
      ! VARIABLES
      ! - -----------------------------------------------------------------
      INTEGER intchk, Iostat, LLOC, ISTART, ISTOP, I
      logical :: found, option, found1, found2, found3
      real :: R
      character(len=16)  :: text = 'AG'
      character(len=200) :: line
      ! - -----------------------------------------------------------------
      !
      LLOC = 1
      found = .false.
      found1 = .false.
      option = .false.
      found3 = .false.
      CALL URDCOM(In, IOUT, line)
      DO
         LLOC = 1
         found2 = .false.
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
         select case (LINE(ISTART:ISTOP))
         case ('OPTIONS')
            write (iout, '(/1x,a)') 'PROCESSING '//
     +             trim(adjustl(text))//' OPTIONS'
            found = .true.
            option = .true.
            !
            !1 - ---CREATE WELL FOR SUPPLEMENTAL PUMPING
            !
         case ('SUPPLEMENTAL_WELL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, 
     +                  NUMSUP, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, 
     +                  MAXSEGS, R, IOUT, IN)
            IF (NUMSUP .LT. 0) NUMSUP = 0
            IF (GSFLOW_flag == 1 .or. IUNIT(44) > 0) found2 = .true.
            IF (.not. found2) THEN
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +               //' Option: '//LINE(ISTART:ISTOP)
     +               //' PRMS or SFR2 must be active for this option'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +              //' Option: '//LINE(ISTART:ISTOP)
     +              //' PRMS or SFR2 must be active for this option')
            END IF
            WRITE (IOUT, *)
            WRITE (IOUT, 33) NUMSUP
            WRITE (IOUT, *)
            found1 = .true.
            found = .true.
                !
                !2 - ---PUMPED WATER WILL BE APPLIED AS IRRIGATION
         case ('IRRIGATION_WELL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NUMIRRWEL, 
     +                  R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXCELLSWEL, 
     +                  R, IOUT, IN)
            IF (NUMIRRWEL .LT. 0) NUMIRRWEL = 0
            IF (MAXCELLSWEL < 1) MAXCELLSWEL = 1
            WRITE (IOUT, *)
            WRITE (IOUT, 34) NUMIRRWEL
            WRITE (IOUT, *)
            found = .true.
            found1 = .true.
            !3 - --- MAX NUMBER OF PRMS DETENTION RESERVOIRS
         case ('IRRIGATION_POND')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NUMIRRPOND, 
     +                  R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXCELLSPOND, 
     +                  R, IOUT, IN)
            IF (NUMIRRPOND .LT. 0) NUMIRRPOND = 0
            IF (MAXCELLSPOND < 1) MAXCELLSPOND = 1
            WRITE (IOUT, *)
            WRITE (IOUT, 43) NUMIRRPOND
            WRITE (IOUT, *)
            found = .true.
            found3 = .true.
            !
            !3 - --- MAX NUMBER OF SUP OR IRR WELLS
         case ('MAXWELLS')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MXWELL, 
     +                  R, IOUT, IN)
            IF (MXWELL .LT. 0) MXWELL = 0
            WRITE (IOUT, *)
            WRITE (IOUT, 36) MXWELL
            WRITE (IOUT, *)
         case ('CHECKCONVERGE')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, AGTOL, 
     +                     IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXAGITER, 
     +                  R, IOUT, IN)
            IF (AGTOL .LT. zerod3) AGTOL = zerod3
            IF (MAXAGITER .LT. 1) MAXAGITER = 1
            WRITE (IOUT, *)
            WRITE (IOUT, 48) AGTOL, MAXAGITER
            WRITE (IOUT, *)
            !
            !4 - --- Option to output list for wells
         case ('WELLLIST')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IWELLCB, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 37) IWELLCB
            WRITE (IOUT, *)
            !
            !5 - --- Option to output list for wells
         case ('WELLCBC')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IWELLCBU, 
     +                  R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ICBSUP, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 37) IWELLCBU, ICBSUP
            WRITE (IOUT, *)
            !
            !5 - --- Option to output list for ponds
         case ('PONDCBC')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IPONDCBU, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 42) IPONDCBU
            WRITE (IOUT, *)
            !6 - --- Option to output list for segments
         case ('DIVERSIONLIST')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ISFRCB, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 37) ISFRCB
            WRITE (IOUT, *)
            !
            !7 - --- Option to output list for irrigation segments
         case ('DIVERSIONIRRLIST')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IRRSFRCB, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 37) IRRSFRCB
            WRITE (IOUT, *)
            !
            !8 - --- Option to output list for irrigation ponds
         case ('PONDIRRLIST')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IRRWELLCB, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 44) IRRPONDCB
            WRITE (IOUT, *)
            !9 - --- Option to output list for irrigation wells
         case ('WELLIRRLIST')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IRRWELLCB, 
     +                  R, IOUT, IN)
            WRITE (IOUT, *)
            WRITE (IOUT, 37) IRRWELLCB
            WRITE (IOUT, *)
            !
            !10 - --- Option to output time series by SW right
         case ('TIMESERIES_DIVERSION')
            TSACTIVESW = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 39)
            WRITE (IOUT, *)
            !
            !11 - --- Option to output time series by GW right
         case ('TIMESERIES_WELL')
            TSACTIVEGW = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 40)
            WRITE (IOUT, *)
            !10 - --- Option to output time series by IRR pond
         case ('TIMESERIES_POND')
            TSACTIVEPOND = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 40)
            WRITE (IOUT, *)
            !
            !11 - --- Option to output time series by SW right
         case ('TIMESERIES_DIVERSIONET')
            TSACTIVESWET = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 39)
            WRITE (IOUT, *)
            !
            !12 - --- Option to output time series by GW right
         case ('TIMESERIES_WELLET')
            TSACTIVEGWET = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 40)
            WRITE (IOUT, *)
           !12 - --- Option to output time series by IRR pond
         case ('TIMESERIES_PONDET')
            TSACTIVEPONDET = .TRUE.
            WRITE (IOUT, *)
            WRITE (IOUT, 45)
            WRITE (IOUT, *)
            !
            !13 - --- Option to turn off writing to LST file
         case ('NOPRINT')
            IPRWEL = 0
            WRITE (IOUT, *)
            WRITE (IOUT, 38)
            WRITE (IOUT, *)
            !
            !14 - --- REDUCING PUMPING FOR DRY CELLS
         case ('PHIRAMP')
         !
         !15 - --- CHECK THAT THERE ARE SUP OR IRR WELLS FIRST
            if (found1) then
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, PSIRAMP, 
     +                     IOUT, IN)
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IUNITRAMP, R, 
     +                     IOUT, IN)
               IF (IUNITNWT .EQ. 0) THEN
                  WRITE (IOUT, *)
                  write (IOUT, 32)
                  WRITE (IOUT, *)
               ELSE
                  IF (PSIRAMP .LT. 1.0E-5) PSIRAMP = 1.0E-5
                  IF (IUNITRAMP .EQ. 0) IUNITRAMP = IOUT
                  WRITE (IOUT, *)
                  WRITE (IOUT, 29) PSIRAMP, IUNITRAMP
                  WRITE (IOUT, *)
               END IF
            else
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required')
            end if
            found = .true.
            !
            !16 - --- SPEICYING PUMPING RATES AS TIMES SERIES INPUT FILE FOR EACH WELL
         case ('TABFILESWELL')
            if (found1) then
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NUMTABWELL, R, 
     +                     IOUT, IN)
               IF (NUMTABWELL .LT. 0) NUMTABWELL = 0
               WRITE (IOUT, *)
               WRITE (IOUT, 30) NUMTABWELL
               WRITE (IOUT, *)
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXVALWELL, R, 
     +                     IOUT, IN)
               IF (MAXVALWELL .LT. 0) THEN
                  MAXVALWELL = 1
                  NUMTABWELL = 0
               END IF
               WRITE (IOUT, *)
               WRITE (IOUT, 31) MAXVALWELL
               WRITE (IOUT, *)
            else
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required')
            end if
            found = .true.
                 !
            !16 - --- SUPPORT PREVIOUS AG INPUT OPTIONS
         case ('TABFILES')
            if (found1) then
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NUMTABWELL, R, 
     +                     IOUT, IN)
               IF (NUMTABWELL .LT. 0) NUMTABWELL = 0
               WRITE (IOUT, *)
               WRITE (IOUT, 30) NUMTABWELL
               WRITE (IOUT, *)
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXVALWELL, R, 
     +                     IOUT, IN)
               IF (MAXVALWELL .LT. 0) THEN
                  MAXVALWELL = 1
                  NUMTABWELL = 0
               END IF
               WRITE (IOUT, *)
               WRITE (IOUT, 31) MAXVALWELL
               WRITE (IOUT, *)
            else
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' SUP or IRR wells required')
            end if
            found = .true.
      !
      !16b - --- SPEICYING POND DIVERSION RATES AS TIMES SERIES INPUT FILE FOR EACH POND
         case ('TABFILESPOND')
            if (found3) then
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NUMTABPOND, R, 
     +                     IOUT, IN)
               IF (NUMTABPOND .LT. 0) NUMTABPOND = 0
               WRITE (IOUT, *)
               WRITE (IOUT, 46) NUMTABPOND
               WRITE (IOUT, *)
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, MAXVALPOND, R, 
     +                     IOUT, IN)
               IF (MAXVALPOND .LT. 0) THEN
                  MAXVALPOND = 1
                  NUMTABPOND = 0
               END IF
               WRITE (IOUT, *)
               WRITE (IOUT, 47) MAXVALPOND
               WRITE (IOUT, *)
            else
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' IRR ponds required'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                       //' Option: '//LINE(ISTART:ISTOP)
     +                       //' IRR ponds required')
            end if
            found = .true.
         !
         !17 - --- Surface water will be added as irrigation
         case ('IRRIGATION_DIVERSION')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, 
     +                  NUMIRRDIVERSION, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, 
     +                  MAXCELLSDIVERSION, R, IOUT, IN)   
            IF (NUMIRRDIVERSION .LT. 0) NUMIRRDIVERSION = 0
            IF (MAXCELLSDIVERSION < 1) MAXCELLSDIVERSION = 1
            IF (GSFLOW_flag == 1 .or. IUNIT(44) > 0) found2 = .true.
            IF (.not. found2) THEN
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                //' Option: '//LINE(ISTART:ISTOP)
     +                //' SFR2 or PRMS must be active for this option'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                //' Option: '//LINE(ISTART:ISTOP)
     +                //' SFR2 or PRMS must be active for this option')
            END IF
            WRITE (IOUT, *)
            WRITE (IOUT, 35) NUMIRRDIVERSION
            WRITE (IOUT, *)
            found = .true.
         case ('ETDEMAND')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, R, 
     +                     IOUT, IN)
            if ( R > 0.0 ) ACCEL = R
            ETDEMANDFLAG = 1
            WRITE (iout, *)
            WRITE (IOUT, '(A)') ' AGRICULTURAL DEMANDS WILL BE '//
     +                          'CALCULATED USING ET DEFICIT'
            WRITE (iout, *)
            IF (GSFLOW_flag == 1 .or. IUNIT(55) > 0) found2 = .true.
            IF (.not. found2) THEN
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                 //' Option: '//LINE(ISTART:ISTOP)
     +                 //' PRMS or UZF1 must be active for this option'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +                 //' Option: '//LINE(ISTART:ISTOP)
     +                 //' PRMS or UZF1 must be active for this option')
            END IF
            IF (IUNIT(55) > 0 .AND. GSFLOW_flag == 0) THEN
               IF (IETFLG == 0) THEN
                  WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +            //' Option: '//LINE(ISTART:ISTOP)
     +            //' UZF1 IETFLG must not be zero for this option'
                  CALL USTOP('Invalid '//trim(adjustl(text))
     +            //' Option: '//LINE(ISTART:ISTOP)
     +            //' UZF1 IETFLG must not be zero for this option')
               END IF
            END IF
            IF (NPER == 1 .AND. ISSFLG(1) == 1) THEN
              WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP)
     +        //' cannot be used for SS only models'
              CALL USTOP('Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP)
     +        //' cannot be used for SS only models')
            END IF
         case ('TRIGGER')
            TRIGGERFLAG = 1
            WRITE (iout, *)
            WRITE (IOUT, '(A)') ' IRRIGATION WILL OCCUR FOR A SET '//
     +                  'PERIOD WHEN ET DEFICIT DROPS BELOW THRESHOLD'
            WRITE (iout, *)
            IF (GSFLOW_flag == 1 .or. IUNIT(55) > 0) found2 = .true.
            IF (.not. found2) THEN
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +             //' Option: '//LINE(ISTART:ISTOP)
     +             //' PRMS or UZF1 must be active for this option'
               CALL USTOP('Invalid '//trim(adjustl(text))
     +             //' Option: '//LINE(ISTART:ISTOP)
     +             //' PRMS or UZF1 must be active for this option')
            END IF
            IF (IUNIT(55) > 0 .AND. GSFLOW_flag == 0) THEN
               IF (IETFLG == 0) THEN
                  WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +              //' Option: '//LINE(ISTART:ISTOP)
     +              //' UZF1 IETFLG must not be zero for this option'
                 CALL USTOP('Invalid '//trim(adjustl(text))
     +              //' Option: '//LINE(ISTART:ISTOP)
     +              //' UZF1 IETFLG must not be zero for this option')
               END IF
            END IF
            IF (NPER == 1 .AND. ISSFLG(1) == 1) THEN
              WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP)
     +        //' cannot be used for SS only models'
              CALL USTOP('Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP)
     +        //' cannot be used for SS only models')
            END IF
         case ('END')
            write (iout, '(/1x,a)') 'END PROCESSING '//
     +             trim(adjustl(text))//' OPTIONS'
            CALL URDCOM(In, IOUT, line)
            found = .true.
            exit
         case default
            read (line(istart:istop), *, IOSTAT=Iostat) intchk
            if (option) then
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP)
               CALL USTOP('Invalid '//trim(adjustl(text))
     +        //' Option: '//LINE(ISTART:ISTOP))

            elseif (Iostat .ne. 0) then
               !
               !18 - --- Not an integer.  Likely misspelled or unsupported
               !18 - --- so terminate here.
               WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +              //' Option: '//LINE(ISTART:ISTOP)
               CALL USTOP('Invalid '//trim(adjustl(text))
     +              //' Option: '//LINE(ISTART:ISTOP))
            else
               exit
            endif
         end select
         if (found) CALL URDCOM(In, IOUT, line)
      ENDDO
      if (found) backspace (in)
      IF (NUMIRRWEL > MXWELL) THEN
         WRITE (IOUT, *) 'THE VALUE SPECIFIED FOR NUMIRRWEL: ', 
     +   NUMIRRWEL,'IS GREATER THAN THE MAXIMUM NUMBER OF AG WELLS: ',
     +   MXWELL
         CALL USTOP('NUMIRRWEL IS GREATER THAN MAX AG WELLS')
      END IF
      IF (NUMSUP > MXWELL) THEN
         WRITE (IOUT, *) 'THE VALUE SPECIFIED FOR NUMSUP: ', NUMSUP,
     +   'IS GREATER THAN THE MAXIMUM NUMBER OF AG WELLS: ', MXWELL
         CALL USTOP(' NUMSUP IS GREATER THAN MAX AG WELLS')
      END IF
      IF (ETDEMANDFLAG > 0 .AND. TRIGGERFLAG > 0 ) THEN
          WRITE (IOUT, *) 'ETDEMAND AND ETTRIGGER: ',
     +   'ARE BOTH ACTIVE. ONLY ONE CAN BE USED AT A TIME '
         CALL USTOP('BOTH ETDEMAND AND ETTRIGGER ARE ACTIVE')
      END IF
29    FORMAT(1X, 'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
     +  ' FALLS WITHIN THE INTERVAL PHIRAMP TIMES THE CELL '/
     +  ' THICKNESS. THE VALUE SPECIFIED FOR PHIRAMP IS ', E12.5, /
     +  ' WELLS WITH REDUCED PUMPING WILL BE '
     +  'REPORTED TO FILE UNIT NUMBER', I5)
30    FORMAT(1X, ' PUMPING RATES WILL BE READ FROM TIME ',
     +  'SERIES INPUT FILE. ', I10, ' FILES WILL BE READ')
31    FORMAT(1X, ' PUMPING RATES WILL BE READ FROM TIME ',
     + 'SERIES INPUT FILE. A MAXIMUM OF ', I10,
     + ' ROW ENTRIES WILL BE READ FROM EACH FILE')
32    FORMAT(1X, ' OPTION TO REDUCE PUMPING DURING CELL ',
     + 'DEWATERING IS ACTIVATED AND NWT SOLVER ', I10,
     + ' IS NOT BEING USED. OPTION DEACTIVATED')
33    FORMAT(1X, 'OPTION TO PUMP SUPPLEMENTARY WATER ',
     + 'FOR SURFACE DIVERSION SHORTFALL IS ACTIVATED. '
     + ' A TOTAL OF    ', I10, ' SUPPLEMENTAL WELLS ARE ACTIVE')
34    FORMAT(1X, 'OPTION TO APPLY PUMPED WATER AS IRRIGATION IS ',
     +      'ACTIVE. ','PUMPED IRRIGATION WATER WILL BE APPLIED TO ', 
     +      I10,' CELLS/RHUS.')
35    FORMAT(1X, 'OPTION TO APPLY SURFACE WATER AS IRRIGATION IS ',
     +      'ACTIVE.'
     +  , ' DIVERTED SURFACE WATER WILL BE APPLIED TO ', I10, 
     +    ' CELLS/RHUS.')
36    FORMAT(1X, 'THE MAXIMUM NUMBER OF WELLS FOR SUPPLEMENTING'
     +  , ' DIVERSIONS OR APPLYING IRRIGATION IS ', I10, ' WELLS.')
37    FORMAT(1X, ' UNFORMATTED CELL BY CELL RATES FOR SUP AND IRR WELLS'
     +  , ' WILL BE SAVED TO FILE UNIT NUMBER ', I10)
38    FORMAT(1X, ' PRINTING OF WELL LISTS IS SUPPRESSED')
39    FORMAT(1X, ' SURFACE WATER IRRIGATION, POTENTIAL AND ACTUAL ET'
     +  , ' WILL BE SAVED TO TIMES SERIES OUTPUT FILES.')
40    FORMAT(1X, ' GROUND WATER IRRIGATION, POTENTIAL AND ACTUAL ET'
     +  , ' WILL BE SAVED TO TIMES SERIES OUTPUT FILES.')
41    FORMAT(1X, 'OPTION TO APPLY POND WATER AS IRRIGATION IS ',
     +      'ACTIVE. ','POND IRRIGATION WATER WILL BE APPLIED TO ', 
     +      I10,' CELLS/RHUS.')
42    FORMAT(1X, ' UNFORMATTED CELL BY CELL RATES FOR IRR PONDS'
     +  , ' WILL BE SAVED TO FILE UNIT NUMBER ', I10)
43    FORMAT(1X, 'OPTION TO APPLY POND WATER AS IRRIGATION IS ',
     +      'ACTIVE. ','POND IRRIGATION WATER WILL BE APPLIED TO ', 
     +      I10,' CELLS/RHUS.')
44    FORMAT(1X, ' UNFORMATTED CELL BY CELL RATES FOR IRRIGATION PONDS'
     +  , ' WILL BE SAVED TO FILE UNIT NUMBER ', I10)
45    FORMAT(1X, ' POND WATER IRRIGATION, POTENTIAL AND ACTUAL ET'
     +  , ' WILL BE SAVED TO TIMES SERIES OUTPUT FILES.')
46    FORMAT(1X, ' POND DIVERSION RATES WILL BE READ FROM TIME ',
     +  'SERIES INPUT FILE. ', I10, ' FILES WILL BE READ')
47    FORMAT(1X, ' POND DIVERSION RATES WILL BE READ FROM TIME ',
     + 'SERIES INPUT FILE. A MAXIMUM OF ', I10,
     + ' ROW ENTRIES WILL BE READ FROM EACH FILE')
48    FORMAT(1X, ' Convergence on AET will be checked ',
     + 'For convergence. A fraction of PET equal to ', e12.5,
     + ' will be used to determine convergence, and a',
     + ' maximum of ',i6,'iterations')
      END SUBROUTINE
      !
      SUBROUTINE GWF2AG7RP(IN, IUNITSFR, KPER)
      !******************************************************************
      ! READ AG DATA FOR A STRESS PERIOD
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT, NCOL, NROW, NLAY, IFREFM
      USE GWFAGMODULE
      USE GWFSFRMODULE, ONLY: ISTRM, NSTRM, NSS
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS:
      ! - -----------------------------------------------------------------
      INTEGER, INTENT(IN):: IN, KPER, IUNITSFR
      ! - -----------------------------------------------------------------
      ! VARIABLES:
      ! - -----------------------------------------------------------------
      CHARACTER(LEN=200)::LINE
      INTEGER I, ITMP
      character(len=22)  :: text = 'AG STRESS PERIOD DATA'
      character(len=18)  :: text1 = 'IRRIGATION SEGMENT'
      character(len=16)  :: text2 = 'IRRIGATION WELL'
      character(len=17)  :: text3 = 'SUPPLEMENTAL WELL'
      character(len=16)  :: text4 = 'IRRDIVERSION'
      character(len=16)  :: text5 = 'IRRWEL'
      character(len=16)  :: text6 = 'SUPWEL'
      character(len=16)  :: text7 = 'STRESS PERIOD'
      character(len=16)  :: text8 = 'END'
      character(len=16)  :: text9 ='IRRIGATION POND'
      character(len=16)  :: text10 = 'IRRPOND'
      character(len=16)  :: char1 = 'WELL LIST'
      character(len=16)  :: char2 = 'SEGMENT LIST'
      character(len=16)  :: char3 = 'POND LIST'
      character(len=16)  :: char = 'SOURCE LIST'
      INTEGER LLOC, ISTART, ISTOP, ISTARTSAVE
      INTEGER J, II, KPER2, L, MATCH, NUMTABS, is, ip, nseg
      INTEGER istsg, istsgold, ISEG, IPOND, NUMTABPOND2
      logical :: FOUND, ierror
      logical :: found1, found2, found3, found4, found7
      REAL :: R, TTIME, TRATE, QPOND, QFRAC, TABVAL, TABFRAC
      INTEGER :: TABUNIT, TABHRU, TABSEG
      CHARACTER*6 CWELL
      ! - -----------------------------------------------------------------
      found4 = .false.
      found7 = .false.
      ierror = .false.
      is = 0
      ISEG = 0
      ip = 0
      QPOND = szero
      TTIME = szero
      TRATE = szero
      QFRAC = szero
      NUMTABPOND2 = 0
      !
      !1 - ---READ SEGMENT, POND, AND WELL LIST DATA
      IF (KPER .EQ. 1) THEN
        do
          CALL URDCOM(In, IOUT, line)
          LLOC = 1
          CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
          ISTARTSAVE = ISTART
          CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
          select case (LINE(ISTARTSAVE:ISTOP))
          case ('SEGMENT LIST')
            CHAR = CHAR2
            write (iout, '(/1x,a)') 'PROCESSING '//
     +             trim(adjustl(CHAR2))//''
            IF ( IUNITSFR == 0 ) THEN
               WRITE (IOUT, *)
                    WRITE (IOUT, *) 'SFR PACKAGE IS NOT ACTIVE',
     +                               ' AND MUST BE ACTIVE FOR ',
     +                               ' SEGMENT LIST. MODEL STOPPING'
                    WRITE (IOUT, *)
                    CALL USTOP('ERROR IN AG:SFR PACKAGE MUST BE '//
     +                         'ACTIVE MODEL STOPPING')  
            END IF
            nseg = 0
            do
               nseg = nseg + 1
               CALL URDCOM(In, IOUT, line)
               LLOC = 1
               CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
               select case (LINE(ISTART:ISTOP))
               case ('END')
                  write (iout, '(/1x,a)') 'FINISHED READING '//
     +                                     trim(adjustl(char2))
                  exit
               case default
                 if ( nseg > NUMIRRDIVERSION ) then
                    WRITE (IOUT, *)
                    WRITE (IOUT, *) 'ERROR IN AG: TOO MANY SEGMENTS IN',
     +                               ' LIST, OR LIST DOES NOT INCLUDE ',
     +                               ' KEY WORD "END". MODEL STOPPING'
                    WRITE (IOUT, *)
                    CALL USTOP('ERROR IN AG: TOO MANY SEGMENTS IN LIST,
     +                     OR "END" NOT SPECIFIED. MODEL STOPPING') 
                  end if
                  LLOC = 1
                  CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ISEG, R, 
     +                        IOUT, IN)
                  if (iseg < 1) then
                     WRITE (IOUT, *)
                     WRITE (IOUT, *) 'ERROR: INVALID SEGMENT VALUE',
     +                               ' CHECK AG SEGMENT LIST INPUT.',
     +                               ' MODEL STOPPING'
                     WRITE (IOUT, *)
                     CALL USTOP('ERROR: INVALID SEGMENT VALUE
     +                     CHECK AG SEGMENT LIST INPUT.MODEL STOPPING')
                  else if (iseg > NSS) then
                     WRITE (IOUT, *)
                     WRITE (IOUT, *) 'ERROR: INVALID SEGMENT VALUE',
     +                               ' CHECK AG SEGMENT LIST INPUT.',
     +                               ' MODEL STOPPING'
                     WRITE (IOUT, *)
                     CALL USTOP('ERROR: INVALID SEGMENT VALUE
     +                     CHECK AG SEGMENT LIST INPUT.MODEL STOPPING')
                  else
                     is = is + 1
                     seglist(is) = iseg
                     numseglist = is
                  end if
               end select
            end do
         !2 - ---READ POND LIST
          case ('POND LIST')
            CHAR = CHAR3
            write (iout, '(/1x,a)') 'PROCESSING '//
     +                         trim(adjustl(CHAR3))//''
              L = 0
              DO
                L = L + 1
                CALL URDCOM(In, IOUT, line)
                LLOC = 1
              CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
                select case (LINE(ISTART:ISTOP))
                case ('END')
                  write (iout, '(/1x,a)') 'FINISHED READING '//
     +                                     trim(adjustl(char3))
                  exit
                case default
                  IF (L > Numirrpond) THEN
                    WRITE (IOUT, *)
                    WRITE (IOUT, *) 'ERROR: Number of ponds in POND '
     +                    ,'LIST is greater than Numirrpond',
     +                      ' MODEL STOPPING.'
                    WRITE (IOUT, *)
                    CALL USTOP('ERROR: Too many ponds in list')
                  END IF
                  IF (NUMTABPOND .EQ. 0) THEN
                    LLOC = 1 
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IPOND, R,
     +                           IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                           QPOND, IOUT,IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ISEG, R,
     +                           IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                           QFRAC, IOUT,IN)
                    POND(1,L) = IPOND   !check that this is less than NRHU
                    IRRPONDVAR(L) = IPOND
                    POND(2,L) = QPOND   
                    POND(3,L) = ISEG    !check that this is less than NSEG
                    PONDSEGFRAC(ISEG) = PONDSEGFRAC(ISEG) + QFRAC
                    POND(4,L) = QFRAC   !check that sum of QFRAC for a SEG sums to 1.
                    IF (POND(2, L) < 0.0) THEN
                      WRITE (IOUT, *)
                      WRITE (IOUT, *) 'ERROR: MAX AG POND IRRIGATION '
     +                        ,'IN LIST',
     +                        ' IS NEGATIVE AND SHOULD BE POSITIVE.',
     +                        ' MODEL STOPPING.'
                      WRITE (IOUT, *)
                CALL USTOP('ERROR: MAX AG IRRIGATION RATE IS NEGATIVE')
                    END IF
                    IF (INT(POND(1,L)) > Nhru) THEN
                      WRITE (IOUT, *)
                      WRITE (IOUT, *) 'ERROR: HRUID for Pond > NHRU ',
     +                        ' Check HRU ID set for Pond.',
     +                        ' MODEL STOPPING.'
                      WRITE (IOUT, *)
                          CALL USTOP('ERROR: HRUID for Pond > NHRU')
                    END IF
                    IF (INT(POND(3,L)) > NSS) THEN
                     WRITE (IOUT, *)
                     WRITE (IOUT, *) 'ERROR: SEGID for Pond > NSEG.',
     +                        ' Check SEGID set for Pond.',
     +                        ' MODEL STOPPING.'
                     WRITE (IOUT, *)
                     CALL USTOP('ERROR: SEGID for Pond > NSEG')
                    END IF               
                  ELSE
                    MATCH = 0  
                    J = L
                    LLOC = 1
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, TABUNIT, 
     +                           R, IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                           TABVAL, IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, TABHRU, R,
     +                           IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, TABSEG, R,
     +                           IOUT, IN)
                    CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                           TABFRAC, IOUT,IN)
                    TABUNITPOND(J) = TABUNIT
                    TABVALPOND(J) = TABVAL
                    TABPONDHRU(J) = TABHRU
                    TABPONDSEG(J) = TABSEG
                    TABPONDFRAC(J) = TABFRAC
                    IRRPONDVAR(J) = TABPONDHRU(J)
                    DO I = 1, J - 1
                      IF (TABUNITPOND(I) == TABUNITPOND(J)) THEN
                        MATCH = 1
                        TABIDPOND(J) = TABIDPOND(I)
                      END IF
                    END DO
                    IF (MATCH == 0) THEN
                      NUMTABPOND2 = NUMTABPOND2 + 1
                      TABIDPOND(J) = NUMTABPOND2
                    END IF
                    IF (TABUNITPOND(J) .LE. 0) THEN
                      WRITE (IOUT, 100)
                      CALL USTOP('')
                    END IF
                    REWIND (TABUNITPOND(J))   
                    DO II = 1, TABVALPOND(J)
                      LLOC = 1
                      CALL URDCOM(TABUNITPOND(J), IOUT, LINE)
                      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                          TTIME, IOUT,TABUNITPOND(J))
                      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                            TRATE, IOUT,TABUNITPOND(J))
                      IF (TRATE < 0.0) THEN
                        WRITE (IOUT, *)
                        WRITE (IOUT, *) 'ERROR: MAX AG POND',
     +                          ' DIVERSION IN LIST IS NEGATIVE AND ',
     +                          ' SHOULD BE POSITIVE. MODEL STOPPING'
                        WRITE (IOUT, *)
                           CALL USTOP('ERROR: MAX POND DIVERSION RATE IN
     +                     LIST IS NEGATIVE AND SHOULD BE POSITIVE.
     +                     MODEL STOPPING')
                        END IF
                        TABTIMEPOND(II, TABIDPOND(J)) = TTIME
                        TABRATEPOND(II, TABIDPOND(J)) = TRATE
                     END DO
                   END IF
                 DO I = 1, NSEGDIMTEMP
                   IF ( PONDSEGFRAC(I) > DONEP ) THEN
                     IF ( .NOT. ierror)
     +                       WRITE (IOUT, *) 'ERROR: SEGMENT INFLOW ',
     +                          ' FRACTION FOR ALL IRR PONDS SUMS TO',
     +                          ' GREATER THAN ONE. MODEL STOPPING'
                           WRITE (IOUT, *)'SEGMENT= ',I,'SUM= ',
     +                                     PONDSEGFRAC(I)
                    ierror = .true.
                   END IF
                 END DO
                 IF ( ierror )  CALL USTOP('ERROR: SEGMENT INFLOW'//
     +              ' FRACTION TO PONDS IS GREATER THAN ONE.'//
     +              ' MODEL STOPPING')
              end select
            end do
          case ('WELL LIST')
            found4 = .true.
            CHAR = CHAR1
            write (iout, '(/1x,a)') 'PROCESSING '//
     +                trim(adjustl(CHAR1))//''
            DO
              CALL URDCOM(In, IOUT, line)
              LLOC = 1
              CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
              select case (LINE(ISTART:ISTOP))
                case ('END')
                  write (iout, '(/1x,a)') 'FINISHED READING '//
     +                                     trim(adjustl(char1))
                  exit
                case default
                 backspace(in)
                 IF (NUMTABWELL .EQ. 0) THEN
                   CALL ULSTRD(NNPWEL, WELL, 1, NWELVL, MXWELL, 1, IN, 
     +                 IOUT,'LAYER   ROW   COL   MAX STRESS RATE',
     +                 WELAUX, 20, NAUXWELL, IFREFM, NCOL, NROW, NLAY, 
     +                 4, 4, IPRWEL)
                   DO L = 1, NNPWEL
                     IF (WELL(4, L) > 0.0) THEN
                        WRITE (IOUT, *)
                        WRITE (IOUT, *) 'ERROR: MAX AG PUMPING RATE IN '
     +                        ,'LIST',
     +                        ' IS POSITIVE AND SHOULD BE NEGATIVE.',
     +                        ' MODEL STOPPING'
                        WRITE (IOUT, *)
                  CALL USTOP('ERROR: MAX AG PUMPING RATE IS POSITIVE')
                     END IF
                  END DO
                 ELSE
                   NUMTABS = 0
                   MATCH = 0
                   DO J = 1, MXWELL    
                     READ (IN, *) TABUNITWELL(J), TABVALWELL(J), 
     +                    TABLAYWELL(J),TABROWWELL(J), TABCOLWELL(J)
                     DO I = 1, J - 1
                        IF (TABUNITWELL(I) == TABUNITWELL(J)) THEN
                           MATCH = 1
                           TABIDWELL(J) = TABIDWELL(I)
                        END IF
                     END DO
                     IF (MATCH == 0) THEN
                        NUMTABS = NUMTABS + 1
                        TABIDWELL(J) = NUMTABS
                     END IF
                     IF (TABUNITWELL(J) .LE. 0) THEN
                        WRITE (IOUT, 100)
                        CALL USTOP('UNIT NUMBER FOR TABULAR INPUT FILE 
     +                              SPECIFIED AS ZERO.')
                     END IF
                     REWIND (TABUNITWELL(J))   
                     DO II = 1, TABVALWELL(J)
                        LLOC = 1
                        CALL URDCOM(TABUNITWELL(J), IOUT, LINE)
                        CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                              TTIME, IOUT,TABUNITWELL(J))
                        CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, I, 
     +                              TRATE, IOUT,TABUNITWELL(J))
                        IF (TRATE > 0.0) THEN
                           WRITE (IOUT, *)
                           WRITE (IOUT, *) 'ERROR: MAX AG PUMPING RATE',
     +                          ' IN LIST IS POSITIVE AND SHOULD BE',
     +                          ' NEGATIVE. MODEL STOPPING'
                           WRITE (IOUT, *)
                           CALL USTOP('ERROR: MAX AG PUMPING RATE IN 
     +                     LIST IS POSITIVE AND SHOULD BE NEGATIVE.
     +                     MODEL STOPPING')
                        END IF
                        TABTIMEWELL(II, TABIDWELL(J)) = TTIME
                        TABRATEWELL(II, TABIDWELL(J)) = TRATE
                     END DO
                   END DO
                 END IF
                end select
              END DO
            case ('END')
               !found4 = .false.
               write (iout, '(/1x,a)') 'FINISHED READING '//
     +          trim(adjustl(char))
               exit
            case ('STRESS PERIOD')
              backspace(in)
              exit
            case default
               WRITE (IOUT, *) 'Invalid AG Input: '//LINE(ISTART:ISTOP)
     +           //' Should be: '//trim(adjustl(CHAR))
               CALL USTOP('Invalid AG Input: '//LINE(ISTART:ISTOP)
     +          //' Should be: '//trim(adjustl(CHAR)))
            end select
      end do
         !
         !3 - ---PRINT NUMBER OF WELLS USED FOR SUP OR IRR.
         IF ( found4 ) THEN
           NWELLS = MXWELL
           CWELL = ' WELLS'
           IF (NWELLS .EQ. 1) CWELL = ' WELL '
           WRITE (IOUT, 101) NWELLS, CWELL
         END IF
101      FORMAT(1X, /1X, I6, A)
100      FORMAT(1X, /1X, '****MODEL STOPPING**** ',
     +      'UNIT NUMBER FOR TABULAR INPUT FILE SPECIFIED AS ZERO.')
         !
         !4 - ---DETERMINE MASTER REACH NUMBER FOR LAST REACH IN EACH SEGMENT
         IF (IUNITSFR > 0) THEN
            istsg = 1
            DO l = 1, NSTRM
               istsgold = istsg
               istsg = ISTRM(4, l)
               if (istsgold /= istsg) lastreach(istsgold) = l - 1
            END DO
            lastreach(NSS) = NSTRM
         END IF
      END IF
      !
      !5 - ---READ AG OPTIONS DATA FOR STRESS PERIOD(OR FLAG SAYING REUSE AG DATA) .
      FOUND = .FALSE.
      found1 = .FALSE.
      found2 = .FALSE.
      found3 = .FALSE.
      found4 = .FALSE.
      found7 = .FALSE.
      if (NUMIRRDIVERSION == 0) found1 = .true.
      if (NUMIRRWEL == 0) found2 = .true.
      if (NUMSUP == 0) found3 = .true.
      write (iout, '(/1x,a)') 'PROCESSING '//
     +       trim(adjustl(text))
      CALL URDCOM(In, IOUT, line)
      LLOC = 1
      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
      ISTARTSAVE = ISTART
      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
      DO
         select case (LINE(ISTARTSAVE:ISTOP))
         case ('STRESS PERIOD')
            found4 = .true.
            write (iout, '(/1x,a)') 'READING '//trim(adjustl(text))//''
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, KPER2, R, 
     +                  IOUT, IN)
            IF (KPER /= KPER2) THEN
               WRITE (IOUT, *) 'INCORRECT PERIOD FOR '//
     +                          trim(adjustl(text))
     +                          //' SPECIFIED. CHECK INPUT.'
               CALL USTOP('INCORRECT PERIOD FOR  '//trim(adjustl(text))
     +                  //'  SPECIFIED. CHECK INPUT.')
            END IF
            found = .true.
         case ('IRRDIVERSION')
            found1 = .true.
            write (iout, '(/1x,a)') 'READING '//
     +      trim(adjustl(text1))//''
            CALL URDCOM(In, IOUT, line)
            LLOC = 1
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ITMP, R, IOUT, IN)
            CALL IRRDIVERSION(IN, IOUT, ITMP)
            IF (KPER == 1 .AND. ITMP < 0) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text4))
     +         //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text4))
     +         //'  specified with no additional input.')
            END IF
            IF (.NOT. FOUND) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text4))
     +         //' found without key word '//trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text4))
     +         //'  found without key word '//trim(adjustl(text7)))
            END IF
         case ('IRRWELL')
            found2 = .true.
            write (iout, '(/1x,a)') 'READING '//
     +      trim(adjustl(text2))//''
            CALL URDCOM(In, IOUT, line)
            LLOC = 1
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ITMP, R, IOUT, IN)
            CALL IRRWEL(IN, ITMP)
            IF (KPER == 1 .AND. ITMP < 0) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text5))
     +     //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text5))
     +     //'  specified with no additional input.')
            END IF
            IF (.NOT. FOUND) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text5))
     +     //' found without key word '//trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text5))
     +     //'  found without key word '//trim(adjustl(text7)))
            END IF
         case ('IRRPOND')
            found7 = .true.
            write (iout, '(/1x,a)') 'READING '//
     +      trim(adjustl(text9))//''
            CALL URDCOM(In, IOUT, line)
            LLOC = 1
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ITMP, R, IOUT, IN)
            CALL IRRPOND(IN, ITMP)
            IF (KPER == 1 .AND. ITMP < 0) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text10))
     +     //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text10))
     +     //'  specified with no additional input.')
            END IF
         case ('SUPWELL')
            found3 = .true.
            write (iout, '(/1x,a)') 'READING '//
     +     trim(adjustl(text3))//''
            CALL URDCOM(In, IOUT, line)
            LLOC = 1
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ITMP, R, IOUT, IN)
            CALL SUPWEL(IN, ITMP)
            IF (KPER == 1 .AND. ITMP < 0) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text6))
     +          //' specified with no additional input.'
               CALL USTOP('Keyvword '//trim(adjustl(text6))
     +          //'  specified with no additional input.')
            END IF
            IF (.NOT. FOUND) THEN
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text6))
     +        //' found without key word '//trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text6))
     +         //'  found without key word '//trim(adjustl(text7)))
            END IF
         case default
            !
            !6 - ---NO KEYWORDS FOUND DURING FIRST STRESS PERIOD SO TERMINATE
            WRITE (IOUT, *) 'Invalid '//trim(adjustl(text))
     +                  //' Option: '//LINE(ISTART:ISTOP)
            CALL USTOP('Invalid '//trim(adjustl(text))
     +                //' Option: '//LINE(ISTART:ISTOP))
         case ('END')
            found4 = .false.
            IF (.NOT. FOUND) THEN
               WRITE (IOUT, *)
               WRITE (IOUT, *) 'Key word '//trim(adjustl(text8))
     +       //' found without key word '//trim(adjustl(text7))
               CALL USTOP('Key word '//trim(adjustl(text8))
     +       //'  found without key word '//trim(adjustl(text7)))
            END IF
! rgn don't need the following code because farms may not be active during first period.
!           if ( kper == 1 ) then
!             if(.not. found1 .or. .not. found2 .or. .not. found3) then
!C
      !2 - ---NO KEYWORDS FOUND DURING FIRST STRESS PERIOD SO TERMINATE
!                WRITE(IOUT,*)
!                WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
!     +                   //' Option: '//LINE(ISTART:ISTOP)
!                CALL USTOP('Invalid '//trim(adjustl(text))
!     +                   //' Option: '//LINE(ISTART:ISTOP))
!             end if
!           else
            if (.not. found1) then
                 WRITE (IOUT, 6)
              end if
              if (.not. found2) then
                 WRITE (IOUT, 7)
              end if
              if (.not. found3) then
                 WRITE (IOUT, 8)
              end if
              if (.not. found7) then
                 WRITE (IOUT, 9)
              end if
!           end if
            write (iout, '(/1x,a)') 'END PROCESSING '//
     +      trim(adjustl(text))//' OPTIONS'
            exit
         end select
         if (found4) then
            CALL URDCOM(In, IOUT, line)
            LLOC = 1
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
            ISTARTSAVE = ISTART
         end if
      end do
!
6     FORMAT(1X, /
     +       1X, 'NO IRRDIVERSION DATA OR REUSING IRRDIVERSION DATA ',
     +       'FROM LAST STRESS PERIOD ')
7     FORMAT(1X, /
     +       1X, 'NO IRRWEL DATA OR REUSING IRRWEL DATA ',
     +       'FROM LAST STRESS PERIOD')
8     FORMAT(1X, /
     +       1X, 'NO SUPWEL OR REUSING SUPWEL DATA ',
     +       'FROM LAST STRESS PERIOD')
9     FORMAT(1X, /
     +       1X, 'NO IRRPOND OR REUSING IRRPOND DATA ',
     +       'FROM LAST STRESS PERIOD')
      RETURN
      END
!
      SUBROUTINE GWF2AG7AD(IN, KPER)
      !******************************************************************
      ! UPDATE DEMANDS FOR NEW TIME STEP
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GWFAGMODULE
      USE GWFSFRMODULE, ONLY: SEG, NUMTAB_SFR, ISFRLIST, SEGINFLOWSAVE
      USE GLOBAL, ONLY: IUNIT, ISSFLG 
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open
      USE PRMS_IT0_VARS, ONLY: It0_dprst_vol_open
      USE GSFMODFLOW, ONLY: MFQ_to_inch_acres
      USE GWFBASMODULE, ONLY: TOTIM
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS:
      INTEGER, INTENT(IN)::IN, KPER
      !
      INTRINSIC :: SNGL
      INTEGER ISEG, i, ii, tabseg, istab, L, ID, ipond
      EXTERNAL :: RATETERPQ
      REAL :: RATETERPQ, TIME
      ! - -----------------------------------------------------------------
      !
      !1 - ------RESET DEMAND IF IT CHANGES
      TIME = TOTIM
      if(IUNIT(44).GT.0) then
        if (NUMTAB_SFR.ne.0) then
            DO ii = 1, NUMTAB_SFR
               tabseg = ISFRLIST(1, ii)
               DEMAND(tabseg) = 0.0
            END DO
        endif
      endif
      ! RESET ALL DEMAND if new stress period 
      if (KPEROLD.ne.KPER) then
	   DEMAND = 0.0
      endif
      !
      !3 - -----SET MAXIMUM POND OUTFLOW DIVERSION RATES WHEN TABFILES ARE USED
      IF ( NUMIRRPOND > 0 ) THEN
          IF (NUMTABPOND > 0) THEN
            DO L = 1, NUMIRRPOND
              ID = TABIDPOND(L)
              POND(1,L) = TABPONDHRU(L)
              POND(3,L) = TABPONDSEG(L)
              POND(4,L) = TABPONDFRAC(L)
              POND(2,L) = RATETERPQ(TIME, TABTIMEPOND(:,ID), 
     +                              TABRATEPOND(:,ID), TABVALPOND(L))
              IF ( FLOWTHROUGH_POND(L) == 1 ) THEN
                iseg = int(POND(3,L))
                SEG(2,iseg) = 0.0
              END IF                
            END DO
          END IF
      END IF
!
!
!5 - -----SET MAXIMUM POND DIVERSION RATES WHEN TABFILES ARE USED
      DO L = 1, NWELLS
         IF (NUMTABWELL > 0) THEN
            WELL(1, L) = TABLAYWELL(L)
            WELL(2, L) = TABROWWELL(L)
            WELL(3, L) = TABCOLWELL(L)
            ID = TABIDWELL(l)
            WELL(4, L) = RATETERPQ(TIME, TABTIMEWELL(:,ID), 
     +                    TABRATEWELL(:,ID), TABVALWELL(L))
         END IF
      END DO
      DO i = 1, NUMIRRDIVERSIONSP
         iseg = IRRSEG(i)
         if (iseg > 0) then
            if (IUNIT(44) > 0) then
               ! Because SFR7AD has just been called (prior to AG7AD) and MODSIM
               ! has not yet overwritten values in SEG(2,x), SEG(2,x) still 
               ! contains the TABFILE values at this point.
               if (NUMTAB_SFR.ne.0) then
			    ! check if this segment has a tabfile associated with it
                  istab = 0
                  DO ii = 1, NUMTAB_SFR
                     tabseg = ISFRLIST(1, ii)
                     if (iseg.eq.tabseg) then
                         istab = 1
                     endif
                  END DO
                  ! update demand if there is a tabfile or if it is a new
                  ! stress period
                  if ((istab.eq.1) .OR. (KPEROLD.ne.KPER)) then
			       DEMAND(ISEG) = SEG(2, ISEG)
                  else
                     DEMAND(ISEG) = SEGINFLOWSAVE(ISEG) 
                  endif
                 ! update demand if this is a new stress period
               else
			    DEMAND(ISEG) = SEGINFLOWSAVE(ISEG)
			 endif
               IF (ETDEMANDFLAG > 0) SEG(2, ISEG) = 0.0
            end if
            SUPACT(ISEG) = 0.0
            ACTUAL(ISEG) = 0.0
         END IF
      END DO
      ! update kperold to track new stress periods and set data accordingly
      if (KPEROLD.ne.KPER) then
          KPEROLD = KPEROLD + 1
      endif
      !2 - ------SET ALL SPECIFIED DIVERSIONS TO ZERO FOR ETDEMAND AND TRIGGER
      IF (ETDEMANDFLAG > 0 .OR. TRIGGERFLAG > 0) THEN
         DO i = 1, NUMSEGLIST
            SEG(2, SEGLIST(i)) = 0.0
         END DO
      END IF
!
!6 --------SET POND STORAGE FOR PREVIOUS AND CURRENT TIME STEPS
      IF ( ISSFLG(kper) == 0 ) THEN
        do i = 1, NUMIRRPOND
          ipond = IRRPONDVAR(i)  !these are hru ids for ponds
          PONDSTOROLD(i) = SNGL(It0_dprst_vol_open(ipond))
     +                       /MFQ_to_inch_acres
          PONDSTORNEW(i) = SNGL(Dprst_vol_open(ipond))
     +                       /MFQ_to_inch_acres
        end do
      END IF
!
!7 - -----RESET SAVED IRR AND AET FROM LAST TIME STEP
      DIVERSIONIRRUZF = szero
      DIVERSIONIRRPRMS = szero
      WELLIRRUZF = szero
      WELLIRRPRMS = szero
      QONLY = szero
      PONDFLOW = szero
      RETURN
      END
!
      SUBROUTINE SUPWEL(IN, ITMP)
      !******************************************************************
      ! READ SUP WELL DATA FOR EACH STRESS PERIOD
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE GWFAGMODULE
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS:
      INTEGER, INTENT(IN)::IN, ITMP
      ! - -----------------------------------------------------------------
      ! VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, ISPWL
      INTEGER :: NMSG, K, L, LL
      REAL :: R
      ! - -----------------------------------------------------------------
      !
      !1 - ---REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0) RETURN
      !
      !2 - ---INACTIVATE ALL IRRIGATION WELLS.
      IF (ITMP == 0) THEN
         NUMSUPSP = 0
         NUMSEGS = 0
         SUPWELVAR = 0
         NUMSEGS = 0
         DIVERSIONSEG = 0
         FRACSUP = szero
         FRACSUPMAX = szero
         RETURN
      END IF
      !
      !3 - ---READ LIST OF DIVERSION SEGEMENTS FOR CALCALATING SUPPLEMENTAL PUMPING
      IERR = 0
      NUMSUPSP = ITMP
      IF (NUMSUPSP > NUMSUP) THEN
         WRITE (IOUT, *)
         WRITE (IOUT, 102) NUMSUP, NUMSUPSP
         CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR '//
     +                 'SUP WELLS')
      END IF
      IERR = 0
      DO J = 1, NUMSUPSP
         LLOC = 1
         CALL URDCOM(IN, IOUT, LINE)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, ISPWL, R, IOUT, IN)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NMSG, R, IOUT, IN)
         IF (NMSG > MAXSEGS) THEN
            WRITE (IOUT, *)
            WRITE (IOUT, 103) MAXSEGS, NMSG
            CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR '//
     +                 'SUP WELLS')
         END IF
         SUPWELVAR(J) = ISPWL
         NUMSEGS(ISPWL) = NMSG
         DO K = 1, NMSG
            READ (IN, *) DIVERSIONSEG(K, ISPWL), FRACSUP(K, ISPWL),
     +                   FRACSUPMAX(K, ISPWL)
         END DO
         DO K = 1, NUMSEGS(SUPWELVAR(J))
            IF (DIVERSIONSEG(K, SUPWELVAR(J)) == 0) IERR = 1
         END DO
      END DO
      IF (IERR == 1) THEN
         WRITE (IOUT, *) 'SEGMENT NUMBER FOR SUPPLEMENTAL WELL ',
     +                   'SPECIFIED AS ZERO. MODEL STOPPING'
         CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR '//
     +                 'SUP WELLS')
      END IF
      !
!99    FORMAT(1X, /1X, '****MODEL STOPPING**** ',
!     + 'WELL PACKAGE MUST BE ACTIVE TO SIMULATE SUPPLEMENTAL ',
!     + 'WELLS')
102   FORMAT('***Error in WELL*** maximum number of supplimentary ',
     + 'wells is less than the number specified in ',
     + 'stress period. ', /
     + 'Maximum wells and the number specified for stress ',
     + 'period are: ', 2i6)
103   FORMAT('***Error in WELL*** maximum number of segments ',
     + 'for a supplementary well is less than the number  ',
     + 'specified in stress period. ', /
     + 'Maximum segments and the number specified for stress '
     + 'period are: ', 2i6)
!106   FORMAT('***Error in SUP WEL*** cell row or column number for '
!     + , 'supplemental well specified as zero. Model stopping')
      !
      !4 - ---CALCULATE THE NUMBER OF SUPWELLS ASSOCIATED WITH A DIVERSION SEGEMENT
      NUMSUPWELLSEG = 1
      DO L = 1, MXWELL
         DO LL = L + 1, MXWELL
            IF (DIVERSIONSEG(1, LL) == DIVERSIONSEG(1, L))
     +         NUMSUPWELLSEG(L) = NUMSUPWELLSEG(L) + 1
         END DO
         DO LL = 1, L - 1
            IF (DIVERSIONSEG(1, LL) == DIVERSIONSEG(1, L))
     +         NUMSUPWELLSEG(L) = NUMSUPWELLSEG(L) + 1
         END DO
      END DO
      RETURN
      END
!
      SUBROUTINE IRRWEL(IN, ITMP)
      !******************************************************************
      ! READ WELL IRRIGATION DATA FOR EACH STRESS PERIOD
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE GWFAGMODULE
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS:
      INTEGER, INTENT(IN)::IN, ITMP
      ! - -----------------------------------------------------------------
      ! VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, IDUM
      INTEGER :: K, IRWL, NMCL, I
      REAL :: R, IPRW, TRPW
      ! - -----------------------------------------------------------------
      !
      !
      !1 - --REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0) RETURN
      !
      !2 - --INITIALIZE AG VARIABLES TO ZERO.
      IRRWELVAR = 0
      NUMCELLS = 0
      IRRFACT = szero
      IRRFIELDFACT = szero
      IRRROW_GW = 0
      IRRCOL_GW = 0
      !
      !3 - --INACTIVATE ALL IRRIGATION WELLS.
      IF (ITMP == 0) THEN
         NUMIRRWELSP = 0
         RETURN
      END IF
      !
      !4 - --READ NEW IRRIGATION WELL DATA
      IERR = 0
      NUMIRRWELSP = ITMP
      !
      !5 - --READ LIST OF IRRIGATION CELLS FOR EACH WELL
      IF (NUMIRRWELSP > NUMIRRWEL) THEN
         WRITE (IOUT, *)
         WRITE (IOUT, 104) NUMIRRWEL, NUMIRRWELSP
         CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR WELLS')
      END IF
      DO J = 1, NUMIRRWELSP
         CALL URDCOM(IN, IOUT, LINE)
         LLOC = 1
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IRWL, R, IOUT, IN)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NMCL, R, IOUT, IN)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, i, IPRW, IOUT, In)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, i, TRPW, IOUT, In)
         IF (NMCL > MAXCELLSWEL) THEN
            WRITE (IOUT, *)
            WRITE (IOUT, 105) MAXCELLSWEL, NMCL
            CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'WELLS')
         END IF
         IRRWELVAR(J) = IRWL
         NUMCELLS(IRWL) = NMCL
         IRRPERIODWELL(IRWL) = IPRW
         TRIGGERPERIODWELL(IRWL) = TRPW
         IF (GSFLOW_flag == 1) then   
            DO K = 1, NMCL
               READ (IN, *) IRRROW_GW(K, IRWL), IDUM, IRRFACT(K, IRWL),
     +                      IRRFIELDFACT(K, IRWL)
            END DO
            DO K = 1, NUMCELLS(IRRWELVAR(J))
               IF (IRRROW_GW(K, IRRWELVAR(J)) == 0) THEN
                  WRITE (IOUT, 107)
                  CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR '//
     +                 'IRR WELLS')
               END IF
            END DO
         ELSE
            DO K = 1, NMCL
               READ (IN, *) IRRROW_GW(K, IRWL), IRRCOL_GW(K, IRWL), 
     +                      IRRFACT(K, IRWL), IRRFIELDFACT(K, IRWL)
            END DO
            DO K = 1, NUMCELLS(IRRWELVAR(J))
               IF (IRRROW_GW(K, IRRWELVAR(J)) == 0 .OR.
     +             IRRCOL_GW(K, IRRWELVAR(J)) == 0) THEN
                  WRITE (IOUT, 106)
                  CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR '//
     +                 'IRR WELLS')
               END IF
            END DO
         END IF
      END DO
!99    FORMAT(1X, /1X, '****MODEL STOPPING**** ',
!     +  'WELL PACKAGE MUST BE ACTIVE TO SIMULATE IRRIGATION ',
!     +  'WELLS')
104   FORMAT('***Error in IRR WEL*** maximum number of irrigation ',
     +      'wells is less than the number specified in stress ',
     +      'period. ', /
     +      'Maximum wells and the number specified for stress '
     +      'period are: ', 2i6)
105   FORMAT('***Error in IRR WEL*** maximum number of cells ',
     +       'irrigated by a well is less than the number ',
     +       'specified in stress period. ', /
     +       'Maximum cells and the number specified for stress '
     +       'period are: ', 2i6)
106   FORMAT('***ERROR IN AG PACKAGE*** cell row or column for ',
     +       'irrigation well specified as zero. Model stopping.')
107   FORMAT('***ERROR IN AG PACKAGE*** HRU ID for ',
     +       'irrigation well specified as zero. Model stopping.')
      RETURN
      END
      !
      SUBROUTINE IRRPOND(IN, ITMP)
      !******************************************************************
      ! READ POND IRRIGATION DATA FOR EACH STRESS PERIOD
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT
      USE GWFAGMODULE
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS:
      INTEGER, INTENT(IN)::IN, ITMP
      ! - -----------------------------------------------------------------
      ! VARIABLES:
      CHARACTER(LEN=200)::LINE
      INTEGER :: IERR, LLOC, ISTART, ISTOP, J, IDUM
      INTEGER :: K, IRWL, NMCL, IP, I, IPOND, IFLTHRU ! , IRSG
      REAL :: R, IPRW, TRPW !, SGFC
      logical :: TEST
      ! - -----------------------------------------------------------------
      !
      !
      !1 - --REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0) RETURN
      !
      !2 - --INITIALIZE AG VARIABLES TO ZERO.
      !IRRPONDVAR = 0  This is read at beginning of simulation and is constant
      NUMCELLSPOND = 0
      IRRFACTPOND = szero
      IRRFIELDFACTPOND = szero
      IRRHRU_POND = 0
      IPOND = 1
      !
      !3 - --INACTIVATE ALL IRRIGATION DEPRESSION STORAGE RESERRVOIRS.
      IF (ITMP == 0) THEN
         RETURN
      ELSEIF (ITMP > 0) THEN
         FLOWTHROUGH_POND = 0
      END IF
      !
      !4 - --READ NEW IRRIGATION DEPRESSION STORAGE DATA
      IERR = 0
      NUMIRRPONDSP = ITMP
      !
      !5 - --READ LIST OF IRRIGATION HRUS FOR EACH DEPRESSION STORAGE RESERVOIR
      IF (NUMIRRPONDSP > NUMIRRPOND) THEN
         WRITE (IOUT, *)
         WRITE (IOUT, 104) NUMIRRPOND, NUMIRRPONDSP
         CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR PONDS')
      END IF
      DO J = 1, NUMIRRPONDSP
         CALL URDCOM(IN, IOUT, LINE)
         LLOC = 1
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IRWL, R, IOUT, IN)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NMCL, R, IOUT, IN)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, i, IPRW, IOUT, In)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, i, TRPW, IOUT, In)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, IFLTHRU, R, IOUT, IN)
         IF (NMCL > MAXCELLSPOND) THEN
            WRITE (IOUT, *)
            WRITE (IOUT, 105) MAXCELLSPOND, NMCL
            STOP
         END IF
         TEST = .TRUE.
         DO 200 IP = 1, NUMIRRPOND
           IF ( IRRPONDVAR(IP) == IRWL ) THEN
              TEST = .FALSE.
              IPOND = IP
              EXIT
           END IF
 200     END DO
         NUMCELLSPOND(IPOND) = NMCL
         IRRPERIODPOND(IPOND) = IPRW
         TRIGGERPERIODPOND(IPOND) = TRPW 
         IF ( IFLTHRU < 0 ) IFLTHRU = 0
         IF ( IFLTHRU > 1 ) IFLTHRU = 1
         FLOWTHROUGH_POND(IPOND) = IFLTHRU
         IF ( ETDEMANDFLAG < 1 .AND. IFLTHRU > 0 ) THEN
          WRITE(IOUT,*)'**ERROR** IFLOWTHROUGH_POND > 0, ETDEMAND = 0.',
     +     ' MODEL STOPPING.'
           CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'PONDS')  
         END IF
         IF (TEST) THEN
           WRITE(IOUT,*)IRWL
           WRITE(IOUT,107)
           CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'PONDS')
         END IF
         DO K = 1, NMCL
           READ (IN, *)IRRHRU_POND(K, IPOND), IDUM, 
     +                 IRRFACTPOND(K, IPOND),IRRFIELDFACTPOND(K, IPOND)
         END DO
         DO K = 1, NUMCELLSPOND(IPOND)
           IF (IRRHRU_POND(K, IPOND) <= 0 .OR. 
     +         IRRHRU_POND(K, IPOND) > NHRU) THEN
             WRITE (IOUT, 106)
             WRITE (IOUT, *)
             CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'PONDS')
           END IF
        END DO
      END DO
104   FORMAT('***Error in IRR Ponds*** maximum number ',
     +      'of irrigation HRUs is less than the number specified in ',
     +      'stress period. ', /
     +      'Maximum Pond storage HRUs and the number specified '
     +      'for stress period are: ', 2i6)
105   FORMAT('***Error in IRR Ponds*** maximum number of ',
     +       'HRUs irrigated by a depression storage is less than ',
     +       'the number specified in stress period. ', /
     +       'Maximum HRUs and the number specified for stress '
     +       'period are: ', 2i6)
106   FORMAT('***Error in IRR Ponds*** HRU ID ',
     +       'irrigated by pond is zero. Model stopping.')
107   FORMAT('***Error in IRR Ponds*** HRU ID for ',
     +    'irrigation pond is not listed in POND LIST. Model stopping.')
      RETURN
      END
!
! ----------------------------------------------------------------------
!
      SUBROUTINE IRRDIVERSION(IN, IOUT, ITMP)
      !******************************************************************
      ! READ DIVERSION SEGMENT DATA FOR EACH STRESS PERIOD
      !******************************************************************
      USE GWFAGMODULE
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS
      INTEGER, INTENT(IN)::IN, IOUT, ITMP
      ! - -----------------------------------------------------------------
      ! VARIABLES
      ! - -----------------------------------------------------------------
      INTEGER LLOC, ISTART, ISTOP, J, SGNM, NMCL, K, IDUM
      REAL R, IRRTRGR, IRRPER
      DOUBLE PRECISION :: totdum
      CHARACTER(LEN=200)::LINE
      ! - -----------------------------------------------------------------
      !
      !1 - ---REUSE VALUES FROM PREVIOUS STRESS PERIOD.
      IF (ITMP < 0) RETURN
      !
      !2 - ---INITIALIZE AG VARIABLES TO ZERO.
      DVRPERC = szero
      DVEFF = szero
      IRRSEG = 0
      IRRROW_SW = 0
      IRRCOL_SW = 0
      DVRCH = 0
      !
      !3 - ---INACTIVATE ALL IRRIGATION SEGMENTS.
      IF (ITMP == 0) THEN
         NUMIRRDIVERSIONSP = 0
         RETURN
      END IF
      !
      !4 - ---READ IRRIGATION SEGEMENT INFORMATION.
      NUMIRRDIVERSIONSP = ITMP
      IF (NUMIRRDIVERSIONSP > NUMIRRDIVERSION) THEN
         WRITE (IOUT, *)
         WRITE (IOUT, 9008) NUMIRRDIVERSION, NUMIRRDIVERSIONSP
         CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'DIVERSION')
      END IF
      DO J = 1, NUMIRRDIVERSIONSP
         LLOC = 1
         CALL URDCOM(IN, IOUT, LINE)
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, SGNM, R, IOUT, IN) 
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, NMCL, R, IOUT, IN) 
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, IDUM, IRRPER, IOUT, 
     +               IN)  
         CALL URWORD(LINE, LLOC, ISTART, ISTOP, 3, IDUM, IRRTRGR, IOUT, 
     +               IN) 
         IF (NMCL > MAXCELLSDIVERSION) THEN
            WRITE (IOUT, *)
            WRITE (IOUT, 9009) MAXCELLSDIVERSION, NMCL
            CALL USTOP('ERROR IN STRESS PERIOD INFORMATION FOR IRR '//
     +                 'DIVERSION')
         END IF
         IF (SGNM > 0) THEN    !need to make sure SGNM is in the segment list
            IRRSEG(J) = SGNM
            DVRCH(SGNM) = NMCL
            IRRPERIODSEG(SGNM) = IRRPER
            TRIGGERPERIODSEG(SGNM) = IRRTRGR
            IF (GSFLOW_flag == 1) then
               DO K = 1, NMCL
                  READ (IN, *) IRRROW_SW(K, SGNM), idum, DVEFF(K, SGNM),
     +                         DVRPERC(K, SGNM)           
               END DO
               totdum = dzero
               DO K = 1, NMCL
                  IF (IRRROW_SW(K, SGNM) == 0) THEN
                     totdum = totdum + DVRPERC(NMCL, SGNM)
                     WRITE (IOUT, 9010)
                     CALL USTOP('ERROR IN STRESS PERIOD INFORMATION '//
     +                 'FOR IRR DIVERSION')
                     IF (totdum .GT. 1.000001 .OR. totdum .LT. 0.999)
     +                   WRITE (Iout, 9006) totdum
                  END IF
               END DO
            ELSE
               DO K = 1, NMCL
                  READ (IN, *) IRRROW_SW(K, SGNM), IRRCOL_SW(K, SGNM),
     +                        DVEFF(K, SGNM), DVRPERC(K, SGNM)
               END DO
               totdum = dzero
               DO K = 1, NMCL
                  IF (IRRROW_SW(K, SGNM) == 0 .OR. 
     +                IRRCOL_SW(K, SGNM) == 0) THEN
                     totdum = totdum + DVRPERC(NMCL, SGNM)
                     WRITE (IOUT, 9007)
                     CALL USTOP('ERROR IN STRESS PERIOD INFORMATION '//
     +                 'FOR IRR DIVERSION')
                     IF (totdum .GT. 1.000001d0 .OR. totdum < 0.999d0)
     +                  WRITE (Iout, 9006) totdum
                  END IF
               END DO
            END IF
         END IF
      END DO
9006  FORMAT(' ***Warning in AG*** ', /
     +        'Fraction of diversion for each cell in group does '/,
     +        'does not sum to one. Sum = ', E15.5)
9007  FORMAT('***Error in AG*** cell row or column for irrigation',
     +       'cell specified as zero. Model stopping.')
9008  FORMAT('***Error in AG*** maximum number of irrigation ',
     +       'segments is less than the number specified in ',
     +       'stress period. ', /
     +       'Maximum segments and the number specified are: ', 2i6)
9009  FORMAT('***Error in AG*** maximum number of irrigation ',
     +       'cells is less than the number specified in ',
     +       'stress period. ', /
     +       'Maximum cells and the number specified are: ', 2i6)
9010  FORMAT('***Error in AG*** HRU_ID for irrigation',
     +       'cell specified as zero. Model stopping.')
      RETURN
      END
!
!
! ----------------------------------------------------------------------
      !
      ! - ------SUBROUTINE TSREAD
      SUBROUTINE TSREAD(IN, IOUT)
      ! READ SEGMENTS, PONDS, AND WELLS WITH TIME SERIES OUTPUT
      USE GWFAGMODULE
      USE PRMS_MODULE, ONLY: Nhru
      IMPLICIT NONE
      ! - -----------------------------------------------------------------
      ! ARGUMENTS
      INTEGER, INTENT(IN)::IN, IOUT
      ! - -----------------------------------------------------------------
      ! VARIABLES
      ! - -----------------------------------------------------------------
      INTEGER LLOC, ISTART, ISTOP, I, SGNM, UNIT, WLNM
      INTEGER PDNM
      INTEGER ISTARTSAVE, ITEST, NUMGWETALL, NUMGWALL, NUMPONDETALL
      INTEGER NUMPONDALL, NUMSWALL, NUMSWETALL
      real :: R
      !character(len=16)  :: text = 'AG'
      character(len=17)  :: char1 = 'TIME SERIES'
      character(len=200) :: line
      logical :: TEST
      ! - -----------------------------------------------------------------
      !
      NUMGWETALL = 0
      NUMGWALL = 0
      NUMPONDETALL = 0
      NUMPONDALL = 0
      NUMPONDET = 0
      NUMSWALL = 0
      NUMSWETALL = 0
      WLNM = 0
      TEST = .FALSE.
      CALL URDCOM(In, IOUT, line)
      LLOC = 1
      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
      ISTARTSAVE = ISTART
      CALL URWORD(LINE, LLOC, ISTART, ISTOP, 1, I, R, IOUT, IN)
      do
         select case (LINE(ISTARTSAVE:ISTOP))
         case ('TIME SERIES')
            TEST = .TRUE.
            write (iout, '(/1x,a)') 'PROCESSING '//
     +                     trim(adjustl(CHAR1))//''
         case ('DIVERSION')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, SGNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMSW = NUMSW + 1
            TSSWUNIT(NUMSW) = UNIT
            TSSWNUM(NUMSW) = SGNM
            ITEST = 0
            DO I = 1, NUMSW - 1
               IF (UNIT == TSSWUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('DIV', NUMSW)
            IF (SGNM > NSEGDIMTEMP) THEN
               WRITE (IOUT, *) 'Bad segment number for AG time series. '
               CALL USTOP('Bad segment number for AG time series.')
            END IF
         case ('WELL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, WLNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMGW = NUMGW + 1
            TSGWUNIT(NUMGW) = UNIT
            TSGWNUM(NUMGW) = WLNM
            ITEST = 0
            DO I = 1, NUMGW - 1
               IF (UNIT == TSGWUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('WEL', NUMGW)
            IF (WLNM > MXWELL) THEN
               WRITE (IOUT, *) 'Bad well number for AG time series. '
               CALL USTOP('Bad well number for AG time series.')
            END IF
         case ('POND')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, PDNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMPOND = NUMPOND + 1
            TSPONDUNIT(NUMPOND) = UNIT
            TSPONDNUM(NUMPOND) = PDNM
            ITEST = 0
            DO I = 1, NUMPOND - 1
               IF (UNIT == TSPONDUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('PND', NUMPOND)
            IF (PDNM > Nhru) THEN
              WRITE (IOUT, *) 'Invalid HRU ID for depression storage ',
     +                         'reservoir for AG time series. '
              CALL USTOP('Invalid HRU ID for AG reservoir time series.')
            END IF
         case ('DIVERSIONET')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, SGNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMSWET = NUMSWET + 1
            TSSWETUNIT(NUMSWET) = UNIT
            TSSWETNUM(NUMSWET) = SGNM
            ITEST = 0
            DO I = 1, NUMSWET - 1
               IF (UNIT == TSSWUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('SET', NUMSWET)
            IF (SGNM > NSEGDIMTEMP) THEN
               WRITE (IOUT, *) 'Bad segment number for AG time series. '
               CALL USTOP('Bad segment number for AG time series.')
            END IF
         case ('WELLET')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, WLNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMGWET = NUMGWET + 1
            TSGWETUNIT(NUMGWET) = UNIT
            TSGWETNUM(NUMGWET) = WLNM
            ITEST = 0
            DO I = 1, NUMGWET - 1
               IF (UNIT == TSGWETUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('GET', NUMGWET)
            IF (WLNM > MXWELL) THEN
               WRITE (IOUT, *) 'Bad well number for AG ET time series. '
               CALL USTOP('Bad well number for AG ET time series.')
            END IF
         case ('PONDET')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, PDNM, R, IOUT, IN)
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            NUMPONDET = NUMPONDET + 1
            TSPONDETUNIT(NUMPONDET) = UNIT
            TSPONDETNUM(NUMPONDET) = PDNM
            ITEST = 0
            DO I = 1, NUMPONDET - 1
               IF (UNIT == TSPONDETUNIT(I)) ITEST = 1
            END DO
            IF (ITEST /= 1) CALL WRITE_HEADER_AG('PIR', NUMPONDET)
            IF (PDNM > NHRU) THEN
               WRITE (IOUT, *) 'Bad pond number for AG ET time series. '
               CALL USTOP('Bad pond number for AG ET time series.')
            END IF
         case ('WELLETALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSGWETALLUNIT = UNIT
            NUMGWETALL = 1
            CALL WRITE_HEADER_AG('AL1', NUMGWET)
         case ('WELLALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSGWALLUNIT = UNIT
            NUMGWALL = 1
            CALL WRITE_HEADER_AG('AL2', NUMGWALL)
         case ('DIVERSIONETALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSSWETALLUNIT = UNIT
            NUMSWETALL = 1
            CALL WRITE_HEADER_AG('SL1', NUMSWETALL)
         case ('DIVERSIONALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSSWALLUNIT = UNIT
            NUMSWALL = 1
            CALL WRITE_HEADER_AG('SL2', NUMSWALL)
         case ('PONDETALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSPONDETALLUNIT = UNIT
            TSACTIVEALLPONDET = .TRUE.
            NUMPONDETALL = 1
            CALL WRITE_HEADER_AG('PA1', NUMPONDETALL)
         case ('PONDALL')
            CALL URWORD(LINE, LLOC, ISTART, ISTOP, 2, UNIT, R, IOUT, IN)
            TSPONDALLUNIT = UNIT
            TSACTIVEALLPOND = .TRUE.
            NUMPONDALL = 1
            CALL WRITE_HEADER_AG('PA2', NUMPONDALL)
         case ('END')
            write (iout, '(/1x,a)') 'FINISHED READING '//
     +             trim(adjustl(char1))
            exit
         case default
            WRITE (IOUT, *) 'Invalid AG Input: '//LINE(ISTART:ISTOP)
     +                        //' Should be: '//trim(adjustl(CHAR1))
            CALL USTOP('Invalid AG Input: '//LINE(ISTART:ISTOP)
     +                        //' Should be: '//trim(adjustl(CHAR1)))
         end select
         CALL URDCOM(In, IOUT, line)
         LLOC = 1
         CALL URWORD(LINE, LLOC, ISTARTSAVE, ISTOP, 1, I, R, IOUT, IN)
      end do
      !11 - ----output number of files activated for time series output.
      IF ( TEST ) THEN
        write (iout, 6) NUMSW + NUMSWALL
        write (iout, 7) NUMGW + NUMGWALL
        write (iout, 8) NUMSWET + NUMSWETALL
        write (iout, 9) NUMGWET + NUMGWETALL        
        write (iout, 10) NUMPONDET + NUMPONDETALL
        write (iout, 11) NUMPOND + NUMPONDALL
      ELSE
        BACKSPACE(IN)
      END IF
6     FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for SURFACE WATER ')
7     FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for GROUNDWATER ')
8     FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for SURFACE WATER ET')
9     FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for GROUNDWATER ET')
10    FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for POND ET')
11    FORMAT(' A total number of ', i10, ' AG output time series files '
     +       'were activated for POND IRRIGATION')

      !11 - ----RETURN.
      RETURN
      END
      !
      ! - ------SUBROUTINE WRITE_HEADER_AG
        SUBROUTINE WRITE_HEADER_AG(TSTYPE, NUM)
        ! READ SEGMENTS AND WELLS WITH TIME SERIES OUTPUT
        USE GWFAGMODULE
        IMPLICIT NONE
        ! - -----------------------------------------------------------------
        ! ARGUMENTS
        CHARACTER(len=3), INTENT(IN)::TSTYPE
        INTEGER, INTENT(IN)::NUM
        ! - -----------------------------------------------------------------
        ! VARIABLES
        ! - -----------------------------------------------------------------
        integer :: UNIT, SGNM, WLNM
        ! character(len=30)  :: text1 = 'AG DIVERSION Time Series'
        ! character(len=30)  :: text2 = 'AG WELL Time Series'
        ! - -----------------------------------------------------------------
        select case (TSTYPE)
        case ('DIV')
           UNIT = TSSWUNIT(NUM)
           SGNM = TSSWNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP SEGMENT SW-RIGHT ',
     +                     'SW-DIVERSION SUP-PUMPING'
        case ('WEL')
           UNIT = TSGWUNIT(NUM)
           WLNM = TSGWNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP WELL GW-DEMAND GW-PUMPED ',
     +                     'NULL'
        case ('SET')
           UNIT = TSSWETUNIT(NUM)
           SGNM = TSSWETNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP SEGMENT ETww ETa NULL'
        case ('GET')
           UNIT = TSGWETUNIT(NUM)
           WLNM = TSGWETNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP WELL ETww ETa NULL'
        case ('PND')
           UNIT = TSPONDUNIT(NUM)
           WLNM = TSPONDNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP POND-HRU SEGMENT-INFLOWS ',
     +                     'POND-IRRIGATION POND-STORAGE'
        case ('PET')
           UNIT = TSPONDETUNIT(NUM)
           WLNM = TSPONDETNUM(NUM)
           WRITE (UNIT, *) 'TIME KPER KSTP POND-HRU ETww ETa NULL'
        case ('AL1')
           UNIT = TSGWETALLUNIT
           WRITE (UNIT, *) 'TIME KPER KSTP NULL ETww ETa NULL'
        case ('AL2')
           UNIT = TSGWALLUNIT
           WRITE (UNIT, *) 'TIME KPER KSTP NULL GW-DEMAND GW-PUMPED ',
     +                     'NULL'
        case ('PA1')
           UNIT = TSPONDETALLUNIT
           WRITE (UNIT, *) 'TIME KPER KSTP NULL ETww ETa NULL'
        case ('SL2')
           UNIT = TSSWALLUNIT
        WRITE (UNIT, *) 'TIME KPER KSTP SW-DEMAND SW-DIVERTED ',
     +                     'NULL'
        case ('SL1')
           UNIT = TSSWETALLUNIT
        WRITE (UNIT, *) 'TIME KPER KSTP NULL ETww ETa NULL'
        end select
      END SUBROUTINE WRITE_HEADER_AG

      !
      SUBROUTINE GWF2AG7FM(Kkper, Kkstp, Kkiter, Iunitnwt, agconverge)
      !******************************************************************
      ! CALCULATE APPLIED IRRIGATION, DIVERISONS, AND PUMPING
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: DELR, DELC, IBOUND, HNEW, LBOTM, BOTM,
     +                  RHS !, IUNIT
      USE GWFBASMODULE, ONLY: TOTIM
      USE GWFAGMODULE
      USE GWFSFRMODULE, ONLY: SEG, DVRSFLW, SGOTFLW !, NUMTAB_SFR
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      IMPLICIT NONE
      !
      ! ARGUMENTS:
      ! - -----------------------------------------------------------------
      INTEGER, INTENT(IN) :: KKPER, KKSTP, KKITER, Iunitnwt
      INTEGER, INTENT(INOUT) :: agconverge
      !
      ! VARIABLES:
      ! - -----------------------------------------------------------------
      INTEGER :: L, I, J, ISTSG, ICOUNT, IRR, ICC, IC, IR, IL, IJ !, ID
      DOUBLE PRECISION :: SUP, FMIN, Q, SUBVOL, SUBRATE, DVT
      EXTERNAL :: SMOOTHQ, demandgw_uzf, demandgw_prms
      EXTERNAL :: demandtrigger_gw
      REAL :: TIME
      DOUBLE PRECISION :: Qp, Hh, Ttop, Bbot, dQp, SMOOTHQ
      DOUBLE PRECISION :: QSW, QQ, demandtrigger_gw
      DOUBLE PRECISION :: demandgw_uzf, demandgw_prms
      INTEGER :: k, ipc, iseg !, PONDID
      !
      ! - -----------------------------------------------------------------
      !
      !1 - ----INITIALIZE LOCAL VARIABLES
      
      TIME = TOTIM
      SUP = DZERO
      DIVERSIONIRRUZF = szero
      DIVERSIONIRRPRMS = szero
      WELLIRRUZF = szero
      WELLIRRPRMS = szero
      PONDIRRPRMS = szero
      SUPFLOW = szero
      Qp = DZERO
!      print *, 'qp', qp, totim, zero
      Q = DZERO
      QQ = DZERO
      QSW = DZERO
      TIME = TOTIM
      RMSESW = szero
      RMSEGW = szero
      RMSEPOND = szero
!1 - ------RESET DEMAND IF IT CHANGES for MODSIM simulations
      !IF ( kkiter == 1 .and. IUNIT(44) > 0 ) Then
      !  if ( NUMTAB_SFR > 0 ) DEMAND = 0.0
      !  DO i = 1, NUMIRRDIVERSIONSP
      !    iseg = IRRSEG(i)
      !    if (iseg > 0 ) then
      !         ! Because SFR7AD has just been called (prior to AG7AD) and MODSIM
      !         ! has not yet overwritten values in SEG(2,x), SEG(2,x) still 
      !         ! contains the TABFILE values at this point.
      !      IF ( NUMTAB_SFR > 0 )  DEMAND(ISEG) = SEG(2, ISEG)
      !      SUPACT(ISEG) = 0.0
      !      ACTUAL(ISEG) = 0.0
      !    end if
      !  END DO
      !!2 - ------SET ALL SPECIFIED DIVERSIONS TO ZERO FOR ETDEMAND AND TRIGGER
      !  IF (ETDEMANDFLAG > 0 .OR. TRIGGERFLAG > 0) THEN
      !     DO i = 1, NUMSEGLIST
      !        SEG(2, SEGLIST(i)) = szero
      !     END DO
      !  END IF
      !END IF
      agconverge = 0
      IF ( kkiter > 2 ) agconverge = 1    
      DO L = 1, NUMIRRPOND
        !IF ( FLOWTHROUGH_POND(L) == 1 ) THEN
        !  PONDSEGFLOW(L) = szero
        !ELSE
!          IF ( POND(3,L) > 0 .and. NUMCELLSPOND(L) > 0 ) THEN
           iseg = int(POND(3,L))
           IF ( iseg > 0 ) THEN
            PONDSEGFLOW(L) = POND(4,L)*SGOTFLW(iseg)
          END IF
        !END IF
      END DO
      !
      !2 - -----IF DEMAND BASED ON ET DEFICIT THEN CALCULATE VALUES
      IF (ETDEMANDFLAG > 0) THEN
         IF (GSFLOW_flag == 0) THEN
           CALL demandconjunctive_uzf(kkper, kkstp, kkiter, agconverge)
         ELSE
           CALL demandconjunctive_prms(kkper, kkstp, kkiter, agconverge)
           CALL demandpond_prms(kkper, kkstp, kkiter, agconverge)
         END IF
      END IF
      IF (TRIGGERFLAG > 0) THEN
         CALL demandtrigger_sw(kkper, kkstp, kkiter)
      END IF
      !
      !2B - ----SAVE OLD SUPPLEMENTAL PUMPING
      DO L = 1, NWELLS
         DO I = 1, NUMSEGS(L)
            J = DIVERSIONSEG(I, L)
            ACTUALOLD(J) = ACTUAL(J)
         END DO
      END DO
      !
      !2C - ---SET CUMULATIVE SUP PUMPING TO ZERO EACH ITERATION
      ACTUAL = szero
      !
      !3 - -----SET MAX PUMPING RATE OR IRR DEMAND FOR GW.
      DO L = 1, NWELLS
         IR = INT( WELL(2, L) )
         IC = INT( WELL(3, L) )
         IL = INT( WELL(1, L) )
         Q = WELL(4, L)
         IF (NUMIRRDIVERSIONSP + NUMIRRWELSP == 0) Q = 0.0
         QQ=Q
         !
         !4 - -----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
         IF (IBOUND(IC, IR, IL) > 0) THEN
            !
            !5 - -----CALCULATE SUPPLEMENTAL PUMPING IF SUP WELL.
            SUP = DZERO
            IF (NUMSEGS(L) > 0) THEN
               DO I = 1, NUMSEGS(L)
                  J = DIVERSIONSEG(I, L)
                  QSW = SEG(2, J)
                  IF ( kkiter > 1 ) QSW = DVRSFLW(J)
                  IF (ETDEMANDFLAG > 0) THEN
                     FMIN = SUPACT(J)
                  ELSE IF (TRIGGERFLAG > 0) then
                     IF (TIMEINPERIODSEG(J) < IRRPERIODSEG(J)) THEN  
                        FMIN = SEG(2, J)
                     ELSE
                       QSW = DZERO
                       FMIN = DZERO
                     END IF
                  ELSE
                     FMIN = DEMAND(J)
                  END IF
                  FMIN = FRACSUP(I, L)*(FRACSUPMAX(I, L)*FMIN - QSW)
                  IF (FMIN < DZERO) FMIN = DZERO
                  SUP = SUP + FMIN
               END DO
               SUPFLOW(L) = SUPFLOW(L)-sngl(SUP/dble(NUMSUPWELLSEG(L)))
               !
               !6 - -----CHECK IF SUPPLEMENTARY PUMPING RATE EXCEEDS MAX ALLOWABLE RATE IN TABFILE
               !
               IF (SUPFLOW(L) < Q) SUPFLOW(L) = SNGL(Q)
               Q = SUPFLOW(L)
               QONLY(L) = SNGL(DONENEG*Q)
            ELSEIF (Q < DZERO) THEN
               !
               !7 - -----CALCULATE ETDEMAND IF NOT SUPPLEMENTAL WELL.
               IF (ETDEMANDFLAG > 0) THEN
                  IF (GSFLOW_flag == 0) THEN
                     QQ = demandgw_uzf(l, kkper, kkstp, kkiter, time, 
     +                                 agconverge)
                  ELSE
                     QQ = demandgw_prms(l, kkper, kkstp, kkiter, 
     +                                  agconverge)
                  END IF
               ELSEIF (TRIGGERFLAG > 0) then
                  QQ = demandtrigger_gw(Q, kkper, kkstp, kkiter, l)   
               END IF
               IF (QQ < Q) QQ = Q
               Q = QQ
               QONLY(L) = SNGL( DONENEG*Q )
            END IF
            !
            !8 - -----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
            ! - ------THE RHS ACCUMULATOR.
            IF (IUNITNWT .NE. 0) THEN
               IF (LAYTYPUPW(il) .GT. 0) THEN
                  Hh = HNEW(ic, ir, il)
                  bbot = Botm(IC, IR, Lbotm(IL))
                  ttop = Botm(IC, IR, Lbotm(IL) - 1)
                  Qp = Q*smoothQ(Hh, Ttop, Bbot, dQp)
                  RHS(IC, IR, IL) = RHS(IC, IR, IL) - Qp
                  !
                  !8 - -----Derivative for RHS
                  !
                  ij = Icell(IC, IR, IL)
                  A(IA(ij)) = A(IA(ij)) + dQp*Q
               ELSE
                  RHS(IC, IR, IL) = RHS(IC, IR, IL) - Q
                  Qp = Q
               END IF
            ELSE
               RHS(IC, IR, IL) = RHS(IC, IR, IL) - Q
               Qp = Q
            END IF
            !
            !9 - -----SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
            !
            SUP = DZERO
            DO I = 1, NUMSEGS(L)  ! need to test when multiple segs supported by single well
               J = DIVERSIONSEG(I, L)
               SUP = SUP - Qp
               ACTUAL(J) = ACTUAL(J) + SNGL( SUP )
            END DO
            !
            !9b - -----SET ACTUAL PUMPING FOR NON SUP IRRIGATION WELLS.
            !
            Qonly(L) = SNGL( DONENEG*Q )
            !
            !10------APPLY IRRIGATION FROM WELLS
            !
            IF (GSFLOW_flag == 0) THEN
               DO I = 1, NUMCELLS(L)
                  SUBVOL = -(DONE - IRRFACT(I, L))*Qp*IRRFIELDFACT(I, L)
                  SUBRATE = SUBVOL/(DELR(IRRCOL_GW(I, L))*
     +                      DELC(IRRROW_GW(I, L)))
                  WELLIRRUZF(IRRCOL_GW(I, L), IRRROW_GW(I, L)) =
     +            WELLIRRUZF(IRRCOL_GW(I, L), IRRROW_GW(I, L)) +
     +            SNGL( SUBRATE )
               END DO
            ELSE
               DO I = 1, NUMCELLS(L)
                  SUBVOL = -(DONE - IRRFACT(I, L))*Qp*IRRFIELDFACT(I, L)
                  ! Keep irrigation for PRMS as volumetric rate
                  WELLIRRPRMS(I, L) = WELLIRRPRMS(I, L) + SNGL( SUBVOL )
               END DO
            END IF
         END IF
      END DO
      ! APPLY IRRIGATION FROM SW DIVERSIONS
      DO L = 1, NUMIRRDIVERSIONSP
         istsg = IRRSEG(L)
         IF (GSFLOW_flag == 0) THEN
            DO icount = 1, DVRCH(istsg)   !THESE VARS COULD BE DIMENSIONED NUMIRRDIVERSION TO SAVE MEMORY
               irr = IRRROW_SW(icount, istsg)
               icc = IRRCOL_SW(icount, istsg)
               dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT,istsg)
               if (dvt < dzero) dvt = dzero
               dvt = dvt/(DELR(icc)*DELC(irr))
               DIVERSIONIRRUZF(icc, irr) = DIVERSIONIRRUZF(icc, irr) +
     +                        SNGL( dvt*(done - DVEFF(ICOUNT, istsg)) )
            END DO
         ELSE
            DO icount = 1, DVRCH(istsg)
               dvt = SGOTFLW(istsg)*DVRPERC(ICOUNT, istsg)
               dvt = (done - DVEFF(ICOUNT, istsg))*dvt
         ! Keep irrigation for PRMS as volume
               DIVERSIONIRRPRMS(icount, istsg) = 
     +         DIVERSIONIRRPRMS(icount, istsg) + SNGL( dvt )
            END DO
         END IF
      END DO
      !
      ! divide pond water into irrigated HRUs
      ! divide segment diversion into pond inflows
      DO L = 1, NUMIRRPOND
        K = NUMCELLSPOND(L)
        SUBVOL = DBLE(PONDFLOW(L))
        DO IPC = 1, K
          dvt = IRRFIELDFACTPOND(IPC, L)*SUBVOL
          dvt = (done-IRRFACTPOND(IPC, L))*dvt
          PONDIRRPRMS(IPC, L) = SNGL( dvt )
        END DO
      !
      ! Set diversions rates to zero before summing up demands from
      ! flow through ponds.
      !
        iseg = int(POND(3,L))
        IF ( iseg > 0 .AND. FLOWTHROUGH_POND(L) == 1 ) THEN
          SEG(2,iseg) = dzero
        END IF
      END DO
      !
      ! Set diversion for filling pond
      DO L = 1, NUMIRRPOND
        IF ( FLOWTHROUGH_POND(L) == 1 ) THEN
          IF ( POND(3,L) > 0 ) THEN
            SEG(2,int(POND(3,L))) = SEG(2,int(POND(3,L))) + 
     +                              PONDSEGFLOW(L)
          END IF
        END IF
      END DO
      !
      ! Check that diversion does not exceed max constraint
      DO L = 1, NUMIRRPOND
        IF ( FLOWTHROUGH_POND(L) == 1 ) THEN
          IF ( POND(3,L) > 0 ) THEN
             Q = POND(2, L)
             IF ( SEG(2,int(POND(3,L))) > Q ) SEG(2,int(POND(3,L))) = Q
             PONDSEGFLOW(L) = POND(4,L)*SGOTFLW(int(POND(3,L)))        
          END IF
        END IF
      END DO
      !
      !3 - -----RETURN
      RETURN
      END
!
      SUBROUTINE GWF2AG7BD(kkstp, kkper, Iunitnwt, agDemand, 
     +                     Nsegshold)
      !******************************************************************
      ! CALCULATE FLOWS FOR AG OPTIONS(DIVERSIONS, PONDS, AND PUMPING)
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      USE GLOBAL, ONLY: IOUT, DELR, DELC, NCOL, NROW, NLAY,
     +     IBOUND, HNEW, BUFF, BOTM, LBOTM, ISSFLG
      USE GWFBASMODULE, ONLY: ICBCFL, IAUXSV, DELT, PERTIM, TOTIM,
     +     VBNM, VBVL, MSUM, IBUDFL
      USE GWFAGMODULE
      USE GWFSFRMODULE, ONLY: SGOTFLW, DVRSFLW
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE PRMS_MODULE, ONLY: GSFLOW_flag
      USE PRMS_FLOWVARS, ONLY: Dprst_vol_open, Dprst_total_open_in,
     +    Dprst_total_open_out
      USE GSFMODFLOW, ONLY: MFQ_to_inch_acres_dble, MFQ_to_inch_acres
      IMPLICIT NONE
      ! ARGUMENTS:
      ! - -----------------------------------------------------------------
      INTEGER, INTENT(IN):: KKSTP, KKPER, Iunitnwt, Nsegshold
      DOUBLE PRECISION, INTENT(INOUT) :: agDemand(Nsegshold)
      ! VARIABLES:
      ! - -----------------------------------------------------------------
!      CHARACTER*22 TEXT2, TEXT6, TEXT7, TEXT8, TEXT1, TEXT3, TEXT4, 
!     +             TEXT5, TEXT13, TEXT14, TEXT15, TEXT16
      CHARACTER*22 TEXT2, TEXT7, TEXT8, TEXT3, TEXT4, TEXT13
!      CHARACTER*22 TEXT14
      CHARACTER*16 TEXT1, TEXT9
!      CHARACTER*21 TEXT10, TEXT11, TEXT12
      DOUBLE PRECISION :: RATIN, RATOUT, DVT, RIN, ROUT, STOR
      DOUBLE PRECISION :: SUP, SUBVOL, RATINAG, RATOUTAG, AREA
      DOUBLE PRECISION :: QSW, QSWIRR, QWELL, QWELLIRR, QWELLET
      DOUBLE PRECISION :: QSWGL, QPOND, QPONDIRR
      REAL :: Q, TIME, QIRR, BUDPERC, RIN_SNGL, ROUT_SNGL
      INTEGER :: NWELLSTEMP, L, I, J, ISTSG, ICOUNT, IL !, ID
      INTEGER :: IC, IR, IBDLBL, IW1, IHRU, K, IPC, ipond
      INTEGER :: IBD1, IBD2, IBD3, IBD4, IBD5, IBD6, IBD7
!      INTEGER :: TOTWELLCELLS, TOTDIVERSIONCELLS
!      INTEGER :: TOTPONDCELLS
      EXTERNAL :: SMOOTHQ
      INTRINSIC :: INT, SNGL
      DOUBLE PRECISION :: SMOOTHQ, bbot, ttop, hh
      DOUBLE PRECISION :: Qp, QQ, Qsave, dQp, DELIN, DELOUT, DELSTOR
      DATA TEXT1/'        AG WELLS'/
      DATA TEXT2/'  DIVERSION SEGMENTS'/
      DATA TEXT13/'   IRRIGATION PONDS'/
!      DATA TEXT14/'       POND STORAGE'/
      DATA TEXT3/'       SW IRRIGATION'/
      DATA TEXT4/'       GW IRRIGATION'/
!      DATA TEXT14/'    POND IRRIGATION'/
!      DATA TEXT16/'     POND SW INFLOW'/
!      DATA TEXT5/'       SW RETURN FLOW'/
!      DATA TEXT6/'       GW RETURN FLOW'/
!      DATA TEXT15/'    POND RETURN FLOW'/
      DATA TEXT7/'    SYSTEM LOSSES SW'/
      DATA TEXT8/'    SYSTEM LOSSES GW'/
      DATA TEXT9/'       SUP WELLS'/
!      DATA TEXT10/'CROP CONSUMPTION SW'/
!      DATA TEXT11/'CROP CONSUMPTION GW'/
!      DATA TEXT12/'CROP CONSUMPTION POND'/
      ! - -----------------------------------------------------------------
      RATIN = DZERO
      RATOUT = DZERO
      RATINAG = DZERO
      RATOUTAG = DZERO
      STOR = DZERO
      QSW = DZERO
      QSWIRR = DZERO
      QSWGL = DZERO
      QWELLIRR = DZERO
      QWELLET = DZERO
      QWELL = DZERO
      QIRR = szero
      QPOND = DZERO
      QPONDIRR = DZERO
      SUPSEG = DZERO
      TIME = TOTIM
      ACTUAL = szero
      ACTUALOLD = szero
      SUP = DZERO
      MSUMAG = 1
      IBD1 = 0
      IBD2 = 0
      IBD3 = 0
      IBD4 = 0
      IBD5 = 0
      IBD6 = 0
      IBD7 = 0
      Qp = DONE
      NWELLSTEMP = NWELLS
      IBDLBL = 0
      iw1 = 1
      ! Budget output for wells
      IF (IWELLCB .LT. 0 .AND. ICBCFL .NE. 0) IBD1 = IOUT
      IF (IWELLCB .GT. 0 .AND. ICBCFL .NE. 0) IBD1 = IWELLCB
      ! Unformatted cbc budget output for SUP wells
      IF (IWELLCBU .GT. 0) IBD5 = ICBCFL
      IF (IWELLCBU .LT. 0) IBD5 = ICBCFL
      ! Budget output for segments
      IF (ISFRCB .LT. 0 .AND. ICBCFL .NE. 0) IBD2 = IOUT
      IF (ISFRCB .GT. 0 .AND. ICBCFL .NE. 0) IBD2 = ISFRCB
      ! Budget output for irrigation segments
      IF (IRRSFRCB .LT. 0 .AND. ICBCFL .NE. 0) IBD3 = IOUT
      IF (IRRSFRCB .GT. 0 .AND. ICBCFL .NE. 0) IBD3 = IRRSFRCB
      ! Budget output for irrigation wells
      IF (IRRWELLCB .LT. 0 .AND. ICBCFL .NE. 0) IBD4 = IOUT
      IF (IRRWELLCB .GT. 0 .AND. ICBCFL .NE. 0) IBD4 = IRRWELLCB
      ! Budget output for ponds
      IF (IPONDCB .LT. 0 .AND. ICBCFL .NE. 0) IBD6 = IOUT
      IF (IPONDCB .GT. 0 .AND. ICBCFL .NE. 0) IBD6 = IPONDCB
      ! Unformatted cbc budget output for ponds
      IF (IPONDCBU .GT. 0) IBD7= ICBCFL
      !
      !1 - -----ADD UP TOTAL NUMBER OF WELL IRRIGATION CELLS DURING STRESS PERIOD.
      !DO L=1,NWELLSTEMP
      !  TOTWELLCELLS = TOTWELLCELLS + NUMCELLS(L)
      !END DO
      !
      !1 - -----ADD UP TOTAL NUMBER OF DIVERSION IRRIGATION CELLS DURING STRESS PERIOD.
      !DO L = 1, NUMIRRDIVERSIONSP
      !  J = IRRSEG(L)
      !  TOTDIVERSIONCELLS = TOTDIVERSIONCELLS + DVRCH(J)
      !END DO
      !
      !1 - -----CLEAR THE BUFFER.
      DO 50 IL = 1, NLAY
      DO 50 IR = 1, NROW
      DO 50 IC = 1, NCOL
      BUFF(IC, IR, IL) = SZERO
50    CONTINUE
      !
      !
      !4 - -----SET MAX NUMBER OF POSSIBLE SUPPLEMENTARY WELLS.
      NWELLSTEMP = NWELLS
      !      IF ( NUMTABWELL.GT.0 ) NWELLSTEMP = NUMTABWELL
      !
      !2 - ----IF CELL - BY - CELL PUMPING WILL BE SAVED AS A LIST(COMPACT BUDGET),
      ! WRITE HEADER.
      IF (IAUXSV .EQ. 0) NAUXWELL = 0
      !
      !2 - ----IF CELL - BY - CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF (IBD5 .EQ. 2) THEN
         CALL UBDSV4(KKSTP, KKPER, TEXT1, NAUXWELL, WELAUX, IWELLCBU,  
     +    NCOL,NROW, NLAY, NWELLS, IOUT, DELT, PERTIM, TOTIM, IBOUND)
      END IF
      !
      !5 - -----CALCULATE DIVERSION SHORTFALL TO SET SUPPLEMENTAL PUMPING DEMAND
      DO L = 1, NWELLSTEMP
        IL = INT( WELL(1, L) )
        IR = INT( WELL(2, L) )
        IC = INT( WELL(3, L) )
        Q = WELL(4, L)
         !
         !6 - -----IF TRIGGER ACTIVE THEN IMCREMENT IRRIGATION PERIOD FOR WELL
         if (TRIGGERFLAG > 0) then
            TIMEINPERIODWELL(L) = TIMEINPERIODWELL(L) + DELT
         end if
         !
         !7 - -----IF THE CELL IS NO - FLOW OR CONSTANT HEAD, IGNORE IT.
         !
         !
         IF (IBOUND(IC, IR, IL) > 0) THEN
            !
            !6a - -----SUPPLEMENTAL WELL SET DEMAND.
            IF (NUMSEGS(L) > 0) THEN
               Q = SUPFLOW(L)
            ELSE
               !
               !6b - -----NOT SUPPLEMENTAL WELL SET DEMAND
               Q = SNGL(DONENEG*QONLY(L))
            END IF
            QSAVE = Q
            !
            bbot = Botm(IC, IR, Lbotm(IL))
            ttop = Botm(IC, IR, Lbotm(IL) - 1)
            Hh = HNEW(ic, ir, il)
            IF (Iunitnwt .NE. 0) THEN
               IF (LAYTYPUPW(il) .GT. 0) THEN
                  Qp = smoothQ(Hh, Ttop, Bbot, dQp)
                  Q = Q*SNGL(Qp)
               ELSE
                  Q = SNGL( Qsave )
               END IF
            ELSE
               Q = SNGL( Qsave )
            END IF
            QQ = Q
            !8 - -----SET ACTUAL SUPPLEMENTAL PUMPING BY DIVERSION FOR IRRIGATION.
            SUP = DZERO
            DO I = 1, NUMSEGS(L)
               J = DIVERSIONSEG(I, L)
               SUP = SUP - Q
               ACTUAL(J) = ACTUAL(J) + SNGL( SUP )
               SUPSEG(J) = SUPSEG(J) - SNGL( Q/NUMSEGS(L) )
            END DO
            !
            !9 - -----CALCULATE IRRIGATION FROM WELLS
            IF (NUMCELLS(L) > 0) QWELL = QWELL + QQ
            DO I = 1, NUMCELLS(L)
               SUBVOL = -(DONE - IRRFACT(I, L))*QQ*IRRFIELDFACT(I, L)
               QWELLIRR = QWELLIRR + SUBVOL
               QWELLET = QWELLET - IRRFACT(I, L)*QQ*IRRFIELDFACT(I, L)
            END DO
            !
            !10 - -----WRITE WELLS WITH REDUCED PUMPING
            IF (Qp .LT. 0.9999D0 .AND. Iunitnwt .NE. 0 .AND.
     +              IPRWEL .NE. 0 .and. Qsave < DZERO) THEN
            IF (iw1 .EQ. 1) THEN
               WRITE (IUNITRAMP, *)
               WRITE (IUNITRAMP, 300) KKPER, KKSTP
               WRITE (IUNITRAMP, 400)
            END IF
            WRITE (IUNITRAMP, 500) IL, IR, IC, QSAVE, Q, hh, bbot
            iw1 = iw1 + 1
         END IF
         !
         !11D-----FLOW RATE IS ALWAYS NEGATIVE(DISCHARGE) ADD IT TO RATOUT.
         RATOUT = RATOUT - QQ
         !
         ! - -------COPY FLOW TO WELL LIST.
         WELL(NWELVL, L) = SNGL( QQ )
         END IF
      END DO
      DO L = 1, NUMIRRPOND
        IF ( FLOWTHROUGH_POND(L) == 0 ) THEN
!          IF ( POND(3,L) > 0 .and. NUMCELLSPOND(L) > 0 ) THEN
           IF ( POND(3,L) > 0 ) THEN
            PONDSEGFLOW(L) = POND(4,L)*SGOTFLW(int(POND(3,L)))
          END IF
        END IF
      END DO
      !
      ! - -------WRITE REQUESTED TIME SERIES OUTPUT.
      call TIMESERIESOUT(KKPER, KKSTP, TOTIM, agDemand, Nsegshold)
      !
      !12 - ------APPLY IRRIGATION FROM DIVERSIONS
      ! IF SAVING CELL - BY - CELL FLOWS IN A LIST(COMPACT BUDGET), WRITE SW IRRIGATION
      ! AND DIVERTED SW.
      DO L = 1, NUMIRRDIVERSIONSP
         istsg = IRRSEG(L)
         !
         !12A - -----IF TRIGGER ACTIVE THEN IMCREMENT IRRIGATION PERIOD
         if (TRIGGERFLAG > 0) then
            TIMEINPERIODSEG(istsg) = TIMEINPERIODSEG(istsg) + DELT
         end if
         !
         !12A - -----ADD UP TOTAL IRRIGATION DIVERSIONS
         QSW = QSW + DVRSFLW(istsg)

         !
         !12B - ----ADD UP SPECIFIED LOSSES
         DO icount = 1, DVRCH(istsg)
            DVT = SGOTFLW(istsg)*DVRPERC(ICOUNT, istsg)
            QSWGL = QSWGL + DVT*(DVEFF(ICOUNT, istsg))
            QSWIRR = QSWIRR + DVT*(DONE - DVEFF(ICOUNT, istsg))
         END DO
         !
         !12B - ----ADD SIMULATED CANAL GAINS/LOSSES
         QSWGL = QSWGL + DVRSFLW(istsg) - SGOTFLW(istsg)
      END DO
!
      !13 - ----PRINT PUMPING RATE IF REQUESTED.
      IF (IBD1 .GT. 0) THEN
         WRITE (IBD1, *)
         WRITE (IBD1, 61) TEXT1, KKPER, KKSTP
         DO L = 1, NWELLSTEMP
           IR = INT( WELL(2, L) )
           IC = INT( WELL(3, L) )
           IL = INT( WELL(1, L) )
           WRITE (IBD1, 62) L, IL, IR, IC, WELL(NWELVL, L)
         END DO
         WRITE (IBD1, *)
      END IF
!
      !13 - ----PRINT IRRIGATION DIVERSION RATE IF REQUESTED.
      IF (IBD2 .GT. 0) THEN
         WRITE (IBD2, *)
         WRITE (IBD2, 61) TEXT2, KKPER, KKSTP
         DO L = 1, NUMIRRDIVERSIONSP
            istsg = IRRSEG(L)
            WRITE (IBD2, 63) istsg, DVRSFLW(istsg)
         END DO
         WRITE (IBD2, *)
      END IF
!
      !13 - ----PRINT APPLIED SW IRRIGATION FOR EACH CELL
      IF (IBD3 .GT. 0) THEN
         WRITE (IBD3, *)
         WRITE (IBD3, 61) TEXT3, KKPER, KKSTP
         DO L = 1, NUMIRRDIVERSIONSP
            ISTSG = IRRSEG(L)
            IF (GSFLOW_flag == 0) THEN
               DO icount = 1, DVRCH(istsg)
                  ir = IRRROW_SW(icount, istsg)
                  ic = IRRCOL_SW(icount, istsg)
                  AREA = DELR(ic)*DELC(ir)
                  WRITE (IBD3, 65) ISTSG, IR, IC, 
     +                             DIVERSIONIRRUZF(IC, IR)*AREA
               END DO
            ELSE
               DO icount = 1, DVRCH(istsg)
                  ihru = IRRROW_SW(icount, istsg)
                  WRITE (IBD3, 66) ISTSG, IHRU, 
     +                             DIVERSIONIRRPRMS(icount, istsg)
               END DO
            END IF
         END DO
         WRITE (IBD3, *)
      END IF
      !
      !13 - ----PRINT APPLIED GW IRRIGATION FOR EACH CELL
      IF (IBD4 .GT. 0) THEN
         WRITE (IBD4, *)
         WRITE (IBD4, 61) TEXT4, KKPER, KKSTP
         DO L = 1, NWELLSTEMP
            IF (GSFLOW_flag == 0) THEN
               DO I = 1, NUMCELLS(L)
                  IC = IRRCOL_GW(I, L)
                  IR = IRRROW_GW(I, L)
                  AREA = DELR(ic)*DELC(ir)
                  WRITE (IBD4, 64) L, IR, IC, WELLIRRUZF(IC, IR)*AREA
               END DO
            ELSE
               DO I = 1, NUMCELLS(L)
                  IHRU = IRRROW_GW(I, L)
                  WRITE (IBD4, 67) L, IHRU, WELLIRRPRMS(I, L)
               END DO
            END IF
         END DO
         WRITE (IBD4, *)
      END IF
      !
      IF (iw1 .GT. 1) WRITE (IUNITRAMP, *)
      !
C
C------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C------CALL UBUDSV TO SAVE THEM.
      IF (IBD5.EQ.1) THEN
        DO L = 1, NWELLSTEMP
          IL = INT( WELL(1, L) )
          IR = INT( WELL(2, L) )
          IC = INT( WELL(3, L) )
          IF ( IBOUND(ic,ir,il) > 0 ) THEN
              BUFF(IC, IR, IL) = BUFF(IC, IR, IL) + WELL(NWELVL, L)
          END IF
        END DO
        CALL UBUDSV(KKSTP,KKPER,TEXT1,IWELLCBU,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
      END IF
      !
      !14 - -----IF SUP PUMPING WILL BE SAVED AS COMPACT FORMAT,
      ! - ------CALL UBUDSVB TO SAVE THEM.
      
      IF (IBD5.EQ.2) THEN
        DO L = 1, NWELLSTEMP
          IR = INT( WELL(2, L) )
          IC = INT( WELL(3, L) )
          IL = INT( WELL(1, L) )
          IF ( ICBSUP == 0 ) THEN
            Q = WELL(NWELVL, L)
          ELSE 
            Q = szero
            DO I = 1, NUMSEGS(L)
              J = DIVERSIONSEG(I, L)
              Q = Q + SUPSEG(J)
            END DO
          END IF
          CALL UBDSVB(IWELLCBU,NCOL,NROW,IC,IR,IL,Q,
     1                WELL(:,L),NWELVL,NAUXWELL,5,IBOUND,NLAY)
        END DO
      END IF
      !
      !15 - -----MOVE RATES, VOLUMES&LABELS INTO ARRAYS FOR PRINTING.
      RIN = RATIN
      ROUT = RATOUT
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVL(3, MSUM) = RIN_SNGL
      VBVL(4, MSUM) = ROUT_SNGL
      VBVL(1, MSUM) = VBVL(1, MSUM) + RIN_SNGL*DELT
      VBVL(2, MSUM) = VBVL(2, MSUM) + ROUT_SNGL*DELT
      VBNM(MSUM) = TEXT1
      !
      !16 - -----INCREMENT BUDGET TERM COUNTER(MSUM) .
      MSUM = MSUM + 1
      !
      !18 - -----MOVE RATES, VOLUMES&LABELS INTO ARRAYS FOR PRINTING
      ! GW PUMPING (NEGATIVE OUT OF GW)
      RIN = -QWELL
      ROUT = DZERO
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT1
      MSUMAG = MSUMAG + 1
      !
      !18 - ------SW DIVERSIONS
      RIN = QSW
      ROUT = DZERO
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + SNGL(RIN)*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + SNGL(ROUT)*DELT
      VBNMAG(MSUMAG) = TEXT2
      MSUMAG = MSUMAG + 1
      !19 - ------POND RELEASES
      
      QPOND = DZERO
      DO L = 1, NUMIRRPOND
        QPOND = QPOND + DBLE(PONDFLOW(L))
      END DO
      RIN = QPOND
      QPOND = DZERO
      !19b ------POND IRRIGATION
      STOR = DZERO
      DELSTOR = DZERO
      DO L = 1, NUMIRRPOND
        K = NUMCELLSPOND(L)
        SUBVOL = DBLE(PONDFLOW(L))
        DO IPC = 1, K
          QPOND = QPOND + PONDIRRPRMS(IPC, L)
        END DO
      END DO
      ROUT = QPOND
      DELIN = DZERO
      DELOUT = DZERO
      IF ( ISSFLG(kkper) == 0 ) THEN
        DO L = 1, NUMIRRPOND
          ipond = IRRPONDVAR(L)
          DELIN = DELIN + Dprst_total_open_in(ipond)/
     +                    MFQ_to_inch_acres_dble
          DELOUT = DELOUT + Dprst_total_open_out(ipond)/
     +                      MFQ_to_inch_acres_dble 
          PONDSTORNEW(L) = SNGL( Dprst_vol_open(ipond))/
     +                           MFQ_to_inch_acres
          DELSTOR = DELSTOR + DBLE(PONDSTORNEW(L) - PONDSTOROLD(L))
        END DO  
      END IF
      DELIN = DELIN - RIN
      DELOUT = DELOUT - ROUT
      DELSTOR = (DELSTOR - DELIN + DELOUT)/DELT
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      IF ( DELSTOR >= DZERO ) THEN
          ROUT_SNGL = ROUT_SNGL + SNGL(DELSTOR)
      ELSE
          RIN_SNGL = RIN_SNGL + SNGL(DELSTOR)
      END IF
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT13
      MSUMAG = MSUMAG + 1
      !
      !18 - ------GW IRRIGATION
      RIN = DZERO
      ROUT = QWELLIRR
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT4
      MSUMAG = MSUMAG + 1
      !
      !18 - ------SW IRRIGATION
      RIN = DZERO
      ROUT = QSWIRR
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT3
      MSUMAG = MSUMAG + 1
      !
      !18 - ------GW EFFICIENCY ET
      RIN = DZERO
      ROUT = QWELLET
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT8
      MSUMAG = MSUMAG + 1
      !
      !18 - ------SW EFFICIENCY(GAINS/LOSSES)
      IF (QSWGL > DZERO) THEN
         RIN = DZERO
         ROUT = QSWGL
      ELSE
         RIN = DONENEG*QSWGL
         ROUT = DZERO
      END IF
      RIN_SNGL = SNGL(RIN)
      ROUT_SNGL = SNGL(ROUT)
      VBVLAG(3, MSUMAG) = RIN_SNGL
      VBVLAG(4, MSUMAG) = ROUT_SNGL
      VBVLAG(1, MSUMAG) = VBVLAG(1, MSUMAG) + RIN_SNGL*DELT
      VBVLAG(2, MSUMAG) = VBVLAG(2, MSUMAG) + ROUT_SNGL*DELT
      VBNMAG(MSUMAG) = TEXT7
      MSUMAG = MSUMAG + 1
      IF (IBUDFL .NE. 0)
     +     CALL SGWF2AG7V(MSUMAG, VBNMAG, VBVLAG, KKSTP, KKPER, IOUT, 
     +                    BUDPERC)
      !
61    FORMAT(1X, /1X, A, '   PERIOD ', I4, '   STEP ', I3)
62    FORMAT(1X, 'AG WELL ', I6, '   LAYER ', I3, '   ROW ', I5, 
     +     '   COL ', I5,'   RATE ', 1PG15.6)
63    FORMAT(1X, 'SEGMENT ', I6, '   RATE ', 1PG15.6)
64    FORMAT(1X, 'AG WELL ', I6, '   ROW ', I5, '   COL ', I5,
     +     '   RATE ', 1PG15.6)
65    FORMAT(1X, 'SEGMENT ', I6, '   ROW ', I5, '   COL ', I5,
     +     '   RATE ', 1PG15.6)
66    FORMAT(1X, 'SEGMENT ', I6, '   HRU ', I5, '   RATE ', 1PG15.6)
67    FORMAT(1X, 'AG WELL ', I6, '   HRU ', I5, '   RATE ', 1PG15.6)
300   FORMAT(' AG WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ', I5,
     +     ' TIME STEP ', I5)
400   FORMAT('   LAY   ROW   COL         APPL.Q          ACT.Q',
     +     '        GW-HEAD       CELL-BOT')
500   FORMAT(3I6, 4E15.6)
      !19 - -----RETURN
      RETURN
      END
! ----------------------------------------------------------------------
!
      SUBROUTINE DEMANDCONJUNCTIVE_UZF(kper, kstp, kiter, agconverge)
!     ******************************************************************
!     demandconjunctive---- sums up irrigation demand using ET deficit
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT, PETRATE, VKS
      USE GWFSFRMODULE, ONLY: SEG, DVRSFLW !, STRM
      USE GWFAGMODULE
      USE GLOBAL, ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! ---------------------------------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: kper, kstp, kiter
      integer, intent(inout) :: agconverge
      !dummy
      DOUBLE PRECISION :: factor, area, uzet, aet, pet
      double precision :: pettotal, aettotal, aetold, aetnew, 
     +                    etdif, ettest, supold, sup, sumvks
!      real :: fmaxflow
      integer :: k, iseg, ic, ir, i
      external :: set_factor
      double precision :: set_factor
! ---------------------------------------------------------------------
!
      etdif = dzero
      ettest = dzero
      supold = dzero
      sup = dzero
      !
      !1 - -----loop over diversion segments that supply irrigation
      !
      do 300 i = 1, NUMIRRDIVERSIONSP
        iseg = IRRSEG(i)
        factor = dzero
        !
        !1b - ------Update demand as max flow in segment
        !
!        IF (DEMAND(iseg) < zerod7) goto 300
        aetold = dzero
        aetnew = dzero
        pettotal = dzero
        aettotal = dzero
        sumvks = dzero
        !
        !1 - -----loop over cells irrigated diversion
        !
        do k = 1, DVRCH(iseg)
           ic = IRRCOL_SW(k, iseg)
           ir = IRRROW_SW(k, iseg)
           area = delr(ic)*delc(ir)
           sumvks = sumvks + vks(ic, ir)*area
           pet = area*PETRATE(ic, ir)
           uzet = uzfetout(ic, ir)
           aet = (gwet(ic, ir) + uzet/delt)
           pettotal = pettotal + pet
           aettotal = aettotal + aet
        end do
        aetold = AETITERSW(ISEG)
        etdif = abs(aettotal - aetold)
        sup = DVRSFLW(iseg) + ACTUAL(ISEG)
        supold = SUPACTOLD(ISEG) + ACTUALOLD(ISEG)
        factor = set_factor(iseg, aetold, pettotal, aettotal, sup,
     +           supold, kper, kstp, kiter)
        AETITERSW(ISEG) = SNGL(aettotal)
        if ( kiter == 2 ) then
          SUPACT(iseg) = SNGL(factor)
          SUPACTOLD(ISEG) = dzero
        else
          SUPACTOLD(ISEG) = DVRSFLW(iseg)
          SUPACT(iseg) = SUPACT(iseg) + SNGL(factor)
        end if
        !
        !1 - -----set diversion to demand
        !
        SEG(2, iseg) = SUPACT(iseg)
        !
        !1 - -----limit diversion to water right and flow in river
        !
  !    if(iseg==9.and.kper==8.and.kstp==1)then
  !    etdif = pettotal - aettotal
  !        write(999,33)kper,kstp,kiter,SEG(2, iseg),
  !   +                 SUPACT(iseg),pettotal,aettotal,demand(ISEG),etdif
  !      endif
  !33  format(3i5,6e20.10)
        IF (SEG(2, iseg) > demand(ISEG)) SEG(2, iseg) = demand(ISEG)
300   CONTINUE
      RETURN
      END SUBROUTINE DEMANDCONJUNCTIVE_UZF
! ----------------------------------------------------------------------
!
!
      subroutine demandconjunctive_prms(kper, kstp, kiter, agconverge)
!     ******************************************************************
!     demandconjunctive---- sums up irrigation demand using ET deficit
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFSFRMODULE, ONLY: SEG, DVRSFLW !, STRM
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: Nhru, Nhrucell, Gvr_cell_id
      USE PRMS_BASIN, ONLY: gsflow_ag_area, gsflow_ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet !, ag_soil_moist_max, 
!     +                        ag_soil_moist, soil_to_ssr, soil_to_gw
!      USE PRMS_SOILZONE, ONLY: dunnian_flow
!      USE PRMS_SOILZONE_AG, ONLY: ag_water_in
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch, Gwc_col, Gwc_row
      USE GWFUZFMODULE, ONLY: UZFETOUT, GWET
      USE GLOBAL, ONLY: ISSFLG
      IMPLICIT NONE
! --------------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: kper, kstp, kiter
      integer, intent(inout) :: agconverge
      !dummy
      DOUBLE PRECISION :: factor, area, aet, pet, uzet
      double precision :: pettotal,aettotal, prms_inch2mf_q,
     +                    aetold, supold, sup
!      double precision :: tot1,tot2,tot3,tot4,tot5
      integer :: k, iseg, hru_id, i, icell, irow, icol
      external :: set_factor
      double precision :: set_factor !, etdif
      INTRINSIC :: ABS
! --------------------------------------------------
!
      prms_inch2mf_q = DZERO
      if ( ISSFLG(kper) == 0 ) 
     +     prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
      !
      !1 - -----loop over diversion segments that supply irrigation
      !
      do 300 i = 1, NUMIRRDIVERSIONSP
        pettotal = DZERO
        aettotal = DZERO
        factor = DZERO
!        tot1= DZERO
!        tot2= DZERO
!        tot3= DZERO
!        tot4= DZERO
!        tot5=DZERO
        iseg = IRRSEG(i)
!        IF (DEMAND(iseg) < zerod7) goto 300
        !
        !1 - -----loop over hrus irrigated by diversion
        !
        do k = 1, DVRCH(iseg)
           hru_id = IRRROW_SW(k, iseg)
           area = gsflow_ag_area(hru_id)
           pet = Potet(hru_id)*area*prms_inch2mf_q
           aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
           pettotal = pettotal + pet
           aettotal = aettotal + aet
           if ( Nhru==Nhrucell ) then
             icell = Gvr_cell_id(hru_id)
             irow = Gwc_row(icell)
             icol = Gwc_col(icell)
             uzet = gsflow_ag_frac(hru_id)*UZFETOUT(icol, irow)/DELT  !multiply by ag_frac
             aet = uzet + gsflow_ag_frac(hru_id)*GWET(icol, irow)   !multiply by ag_frac
             aettotal = aettotal + aet
           end if
!           tot1 = tot1 + 
!     +area*prms_inch2mf_q*(ag_soil_moist_max(hru_id) - 
!     +ag_soil_moist(hru_id))
!           tot2 = tot2 + area*prms_inch2mf_q*soil_to_ssr(hru_id)
!           tot3 = tot3 + area*prms_inch2mf_q*ag_water_in(hru_id)
!           tot4 = tot4 + area*prms_inch2mf_q*dunnian_flow(hru_id)   
!           tot5=tot5+soil_to_gw(hru_id)
!           soil_moist_max(hru_id) - soil_moist(hru_id), 
!     +                  soil_to_ssr(hru_id), infil(hru_id), 
!     +                  dunnian_flow(hru_id)
        end do
        ! convert PRMS ET deficit to MODFLOW flow
        aetold = AETITERSW(ISEG)
        sup = DVRSFLW(iseg) + ACTUAL(ISEG)
        supold = SUPACTOLD(ISEG) + ACTUALOLD(ISEG)
        factor = set_factor(iseg, aetold, pettotal, aettotal, sup,
     +           supold, kper, kstp, kiter)
        RMSESW(ISEG) = SQRT((aetold - aettotal)**dtwo)
        IF ( RMSESW(ISEG) > AGTOL*pettotal .AND. KITER > 2 ) then
            IF ( KITER < MAXAGITER ) AGCONVERGE = 0
        END IF
        AETITERSW(ISEG) = SNGL(aettotal)
        if ( kiter <= 2 ) then
          SUPACT(iseg) = SNGL(factor)
          SUPACTOLD(ISEG) = dzero
        else
          SUPACTOLD(ISEG) = DVRSFLW(iseg)
          SUPACT(iseg) = SUPACT(iseg) + 
     +                 (sone - REAL(AGCONVERGE))*SNGL(factor)
        end if
!        if (SUPACT(iseg) < 0.0) SUPACT(iseg) = 0.0
        !
        !1 - -----set diversion to demand
        !
        SEG(2, iseg) = SUPACT(iseg)
        !
        !1 - -----limit diversion to water right
        !
        
        IF (SEG(2, iseg) > demand(ISEG)) SEG(2, iseg) = demand(ISEG)  !need to check this, unintensional comment?
! NEED to check IPRIOR value here
!        k = IDIVAR(1, ISEG)

!        if(iseg==24)then
!        etdif = pettotal - aettotal
!          write(999,33)kper,kstp,kiter,iseg,SEG(2, iseg),demand(iseg),
!     +                 SUPACT(iseg),etdif,RMSESW(ISEG),pettotal,
!     +                 AGCONVERGE,tot1,tot2,tot3,tot4,tot5
!        endif
!  33  format(4i5,6e20.10,i5,5e20.10)
300   continue
      return
      end subroutine demandconjunctive_prms
!
!
      subroutine demandpond_prms(kper, kstp, kiter, agconverge)
!     ******************************************************************
!     demandpond----sums up pond irrigation demand using ET deficit
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_BASIN, ONLY: gsflow_ag_area
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet !,hru_actet
      USE PRMS_IT0_VARS, ONLY: It0_dprst_vol_open
!     +    , Dprst_total_open_in, Dprst_total_open_out
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch,
     +                      MFQ_to_inch_acres
      USE GLOBAL, ONLY: ISSFLG
!      USE GWFSFRMODULE, ONLY: DVRSFLW
      IMPLICIT NONE
! --------------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: kper, kstp, kiter
      integer, intent(inout) :: agconverge
      !dummy
      DOUBLE PRECISION :: factor, area, aet, pet
      double precision :: pettotal,aettotal, prms_inch2mf_q,
     +                    aetold, supold, sup !, etdif
      real :: saveflow, pondstor, totstor !, Q
      integer :: k, ipond, hru_id, i
      external :: set_factor
      double precision :: set_factor
! --------------------------------------------------
!     check if there are active irrigation ponds for this stress period.
!
      if ( NUMIRRPONDSP == 0 ) THEN
          PONDFLOW = SZERO
          PETPOND = SZERO
          PONDFLOWOLD = SZERO
          AETITERPOND = SZERO
          return
      end if
      !
      !1 - -----loop over HRUs with ponds that supply irrigation
      !
      prms_inch2mf_q = DZERO
      if ( ISSFLG(kper) == 0 ) 
     +     prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
      do 300 i = 1, NUMIRRPOND
        pettotal = DZERO
        aettotal = DZERO
        factor = DZERO
        ipond = IRRPONDVAR(i)  !these are hru ids for ponds
        IF ( ipond == 0 ) goto 300
        !
        !1 - -----loop over HRUs irrigated by pond
        !
        do k = 1, NUMCELLSPOND(i)
           hru_id = IRRHRU_POND(K, i)    !these are HRUs irrigated by ponds
           area = gsflow_ag_area(hru_id)
           pet = Potet(hru_id)*area*prms_inch2mf_q
           aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
           pettotal = pettotal + pet
           aettotal = aettotal + aet
        end do
        ! convert PRMS ET deficit to MODFLOW flow
        aetold = AETITERPOND(i)
        PETPOND(i) = SNGL(pettotal)
        sup = DBLE(PONDFLOW(i))
        supold = DBLE(PONDFLOWOLD(i))
        PONDFLOWOLD(i) = PONDFLOW(i)
        factor = set_factor(ipond, aetold, pettotal, aettotal, sup,
     +                      supold, kper, kstp, kiter)
        if ( factor < dzero ) factor = dzero
        RMSEPOND(I) = SQRT((aetold - aettotal)**dtwo)
        IF ( NUMCELLSPOND(i) > 0 ) THEN
          IF ( RMSEPOND(I) > AGTOL*pettotal .and. kiter > 2 ) then
            IF ( KITER < MAXAGITER ) AGCONVERGE = 0
          END IF
        END IF
        AETITERPOND(i) = SNGL(aettotal)
        saveflow = PONDFLOW(i)
        if ( kiter == 1 ) then
          PONDFLOWOLD(i) = szero
          PONDFLOW(i) = PONDFLOW(i) + SNGL(factor)
          PONDSEGFLOW(i) = PONDFLOW(i)
        else
          PONDFLOWOLD(i) = PONDFLOW(i)
          PONDFLOW(i) = PONDFLOW(i) + 
     +                (sone - REAL(AGCONVERGE))*SNGL(factor)
        end if
        !
        !set max pond irrigation rate
        !
        !1 limit pond outflow to pond storage
        pondstor = SNGL(It0_dprst_vol_open(ipond))/MFQ_to_inch_acres
        if ( pondstor < szero ) pondstor = szero
        totstor = pondstor/DELT + PONDSEGFLOW(i)
        !
        !set pond inflow using demand.
        IF ( FLOWTHROUGH_POND(i) == 1 .and. NUMCELLSPOND(i) > 0 ) THEN
          PONDSEGFLOW(i) = PONDFLOW(i)
        END IF 
        IF ( PONDFLOW(i) > totstor ) PONDFLOW(i) = totstor
     !!   IF ( kiter > 1) THEN
     !!       IF ( PONDFLOW(i) < saveflow ) 
     !!+       PONDFLOW(i) = saveflow
     !!   END IF
  !      if(ipond==27131)then
  !    etdif = pettotal - aettotal
  !        write(999,33)i,kper,kstp,kiter,PONDFLOW(I),
  !   +                 PONDSEGFLOW(I),pettotal,aettotal,
  !   +    It0_dprst_vol_open(ipond)/MFQ_to_inch_acres,factor,
  !   +    hru_actet(5389),potet(5389)
  !      endif
  !33  format(4i5,8e20.10)
300   continue
      return
      end subroutine demandpond_prms

      subroutine demandtrigger_sw(kper, kstp, kiter)
!     ******************************************************************
!     demandtrigger_sw---- triggers and sets sw irrigation demand
!     ******************************************************************
!     SPECIFICATIONS:
      USE GLOBAL, ONLY: DELR, DELC, ISSFLG
      USE GWFSFRMODULE, ONLY: SEG, NSS !, STRM, DVRSFLW
      USE GWFAGMODULE
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT, PETRATE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: Nhru, Nhrucell, Gvr_cell_id, GSFLOW_flag
      USE PRMS_BASIN, ONLY: gsflow_ag_area, gsflow_ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch, Gwc_col, Gwc_row
      IMPLICIT NONE
! --------------------------------------------
      !modules
      !arguments
      integer, intent(in) :: kper, kstp, kiter
      !dummy
      DOUBLE PRECISION :: factor, area, aet, pet, uzet
      double precision :: prms_inch2mf_q
      double precision, allocatable, dimension(:) :: petseg, aetseg
!      real :: fmaxflow
      integer :: k, iseg, hru_id, i, ic, ir, icell, irow, icol
! --------------------------------------------
!
      if ( NUMIRRDIVERSIONSP == 0 ) return
      allocate (petseg(NSS), aetseg(NSS))
      aetseg = dzero
      petseg = dzero
      prms_inch2mf_q = DZERO
      if ( ISSFLG(kper) == 0 ) 
     +     prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
      !
      !1 - -----loop over diversion segments that supply irrigation
      !
      do 300 i = 1, NUMIRRDIVERSIONSP
         iseg = IRRSEG(i)
!         IF (DEMAND(iseg) < zerod30) goto 300
         aet = dzero
         pet = dzero
         !
         !1 - -----loop over hrus irrigated by diversion
         !
         do k = 1, DVRCH(iseg)
            if (GSFLOW_flag == 0) THEN
               ic = IRRCOL_SW(k, iseg)
               ir = IRRROW_SW(k, iseg)
               area = delr(ic)*delc(ir)
               pet = pet + PETRATE(ic, ir)*area
               uzet = uzfetout(ic, ir)/DELT
               aet = aet + gwet(ic, ir) + uzet
            else
               hru_id = IRRROW_SW(k, iseg)
               area = gsflow_ag_area(hru_id)
               pet = Potet(hru_id)*area*prms_inch2mf_q
               aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
               if ( Nhru==Nhrucell ) then
                 icell = Gvr_cell_id(hru_id)
                 irow = Gwc_row(icell)
                 icol = Gwc_col(icell)
                 uzet = gsflow_ag_frac(hru_id)*UZFETOUT(icol, irow)/DELT
                 aet = aet + uzet + gsflow_ag_frac(hru_id)
     +                              *gwet(icol, irow)
               end if
            end if
         end do
         petseg(iseg) = petseg(iseg) + pet
         aetseg(iseg) = aetseg(iseg) + aet
         factor = done
         if (petseg(iseg) > zerod30) factor = aetseg(iseg)/petseg(iseg)
         !
         !1 - -----set diversion to demand if in period or triggered
         !
         SEG(2, iseg) = 0.0
         if (TIMEINPERIODSEG(ISEG) > IRRPERIODSEG(ISEG)) then
            if (factor <= TRIGGERPERIODSEG(ISEG)) then
                SEG(2, iseg) = DEMAND(iseg)
                TIMEINPERIODSEG(ISEG) = 0.0
            end if
         end if
         if (TIMEINPERIODSEG(ISEG) - DELT < IRRPERIODSEG(ISEG))
     +                              SEG(2, iseg) = DEMAND(iseg)
300    continue
       deallocate (petseg, aetseg)
       return
      end subroutine demandtrigger_sw
!
      double precision function demandtrigger_gw(Q, kper, kstp, kiter,l)
!     ******************************************************************
!     demandtrigger_gw---- triggers and sets gw irrigation demand
!     ******************************************************************
!     SPECIFICATIONS:
      !modules
      USE GLOBAL, ONLY: DELR, DELC, ISSFLG
      USE GWFAGMODULE
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT, PETRATE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: GSFLOW_flag, Nhru, Nhrucell, Gvr_cell_id
      USE PRMS_BASIN, ONLY: gsflow_ag_area, gsflow_ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch, Gwc_col, Gwc_row
      IMPLICIT NONE
! --------------------------------------------
      !arguments
      integer, intent(in) :: kper, kstp, kiter, l
      DOUBLE PRECISION, INTENT(INOUT) :: Q
      !dummy
      DOUBLE PRECISION :: factor, area, prms_inch2mf_q
      DOUBLE PRECISION :: pettotal
      DOUBLE PRECISION :: aettotal, uzet, aet, pet
      integer :: hru_id, i, ic, ir, icell, irow, icol
! --------------------------------------------
!
      demandtrigger_gw = dzero
      if ( NUMCELLS(L) == 0 ) return
      pettotal = dzero
      aettotal = dzero
      aet = dzero
      prms_inch2mf_q = DZERO
      if ( ISSFLG(kper) == 0 ) 
     +     prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
      DO i = 1, NUMCELLS(L)
         if (GSFLOW_flag == 0) THEN
            ic = IRRCOL_GW(i, l)
            ir = IRRROW_GW(i, l)
            uzet = uzfetout(ic, ir)/DELT
            area = delr(ic)*delc(ir)
            pettotal = pettotal + PETRATE(ic, ir)*area
            aettotal = aettotal + (gwet(ic, ir) + uzet)
          else
            hru_id = IRRROW_GW(i, l)
            area = gsflow_ag_area(hru_id)
            pet = Potet(hru_id)*area*prms_inch2mf_q
            aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
            if ( Nhru==Nhrucell ) then
              icell = Gvr_cell_id(hru_id)
              irow = Gwc_row(icell)
              icol = Gwc_col(icell)
              uzet = gsflow_ag_frac(hru_id)*UZFETOUT(icol, irow)/DELT
              aet = aet + uzet + gsflow_ag_frac(hru_id)*gwet(icol, irow)
            end if
            pettotal = pettotal + pet
            aettotal = aettotal + aet
          end if
      end do
      factor = done
      if (pettotal > zerod30) factor = aettotal/pettotal
 !     write(999,*)l,kper,kstp,aettotal,pettotal,factor
      !
      !1 - -----set diversion to demand if in period or triggered
      !
      if (TIMEINPERIODWELL(L) > IRRPERIODWELL(L)) then
          if (factor <= TRIGGERPERIODWELL(L)) then
              demandtrigger_gw = Q
              TIMEINPERIODWELL(L) = SZERO
          end if
      end if
      if (TIMEINPERIODWELL(L) < IRRPERIODWELL(L))
     +                                 demandtrigger_gw = Q
      end function demandtrigger_gw
      
      double precision function demandgw_uzf(l, kper, kstp, kiter, time,
     +                                       agconverge)
!     ******************************************************************
!     demandgw---- sums up irrigation demand using ET deficit for gw
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT, PETRATE, VKS
      USE GWFAGMODULE
      USE GLOBAL, ONLY: DELR, DELC
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
! --------------------------------------
      !modules
      !arguments
      integer, intent(in) :: l, kiter, kper, kstp
      integer, intent(inout) :: agconverge
      real, intent(in) :: time
      !dummy
      DOUBLE PRECISION :: factor, area, uzet, aet, pet
      double precision :: aetold, supold, sup, pettotal, 
     +                    aettotal, sumvks
      integer :: ic, ir, i
      external :: set_factor
      double precision :: set_factor
! --------------------------------------
!
      pettotal = DZERO
      aettotal = DZERO
      demandgw_uzf = DZERO
      sumvks = DZERO
      DO I = 1, NUMCELLS(L)
         IC = IRRCOL_GW(I, L)
         IR = IRRROW_GW(I, L)
         area = delr(ic)*delc(ir)
         pet = PETRATE(ic, ir)*area
         uzet = uzfetout(ic, ir)/DELT
         aet = (gwet(ic, ir) + uzet)
         sumvks = sumvks + vks(ic, ir)*area
         if (sumvks < zerod30) aet = pet
         pettotal = pettotal + pet
         aettotal = aettotal + aet
      END DO
!
      aetold = AETITERGW(l)
      sup = QONLY(l)
      supold = QONLYOLD(l)
      factor = set_factor(l, aetold, pettotal, aettotal, sup, supold,
     +                    kper, kstp, kiter)
      QONLYOLD(l) = QONLY(l)
      RMSEGW(L) = SQRT((aetold - aettotal)**dtwo)
      IF ( NUMCELLS(L) > 0 ) THEN
        IF ( RMSEGW(L) > zerod1*pettotal ) AGCONVERGE = 0
      END IF
      AETITERGW(l) = sngl(aettotal)
      QONLY(L) = QONLY(L) + (sone - REAL(AGCONVERGE))*SNGL(factor)
      if (QONLY(L) > sumvks) then
         QONLY(L) = sngl(sumvks)
      end if
      demandgw_uzf = doneneg*QONLY(L)
      end function demandgw_uzf
!
!
      double precision function demandgw_prms(l, kper, kstp, kiter, 
     +                                        agconverge)
!     ******************************************************************
!     demandgw---- sums up irrigation demand using ET deficit for gw
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: Nhru, Nhrucell, Gvr_cell_id
      USE PRMS_BASIN, ONLY: gsflow_ag_area, gsflow_ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch, Gwc_col, Gwc_row
      USE GLOBAL, ONLY: ISSFLG
      IMPLICIT NONE
! --------------------------------------
      !modules
      !arguments
      integer, intent(in) :: l, kper, kstp, kiter
      integer, intent(inout) :: agconverge
      !dummy
      DOUBLE PRECISION :: factor, area, aet, pet, prms_inch2mf_q,
     +                    aetold, supold, sup, aettotal, pettotal
      integer :: i, hru_id, icell, irow, icol 
      external :: set_factor
      double precision :: set_factor, uzet
! --------------------------------------
!
      demandgw_prms = DZERO
      pettotal = DZERO
      aettotal = DZERO
      prms_inch2mf_q = DZERO
      if ( ISSFLG(kper) == 0 ) 
     +     prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
      DO I = 1, NUMCELLS(L)
         hru_id = IRRROW_GW(I, L)
         area = gsflow_ag_area(hru_id)
         pet = Potet(hru_id)*area*prms_inch2mf_q
         aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
         pettotal = pettotal + pet
         aettotal = aettotal + aet
         if ( Nhru==Nhrucell ) then
           icell = Gvr_cell_id(hru_id)
           irow = Gwc_row(icell)
           icol = Gwc_col(icell)
           uzet = gsflow_ag_frac(hru_id)*UZFETOUT(icol, irow)/DELT
           aet = uzet + gsflow_ag_frac(hru_id)*gwet(icol, irow)
           aettotal = aettotal + aet
         end if
      end do
      ! convert PRMS ET deficit to MODFLOW flow
      aetold = AETITERGW(l)
      sup = QONLY(l)
      supold = QONLYOLD(l)
      factor = set_factor(l, aetold, pettotal, aettotal, sup, supold,
     +                    kper, kstp, kiter)
      QONLYOLD(l) = QONLY(L)
      RMSEGW(L) = SQRT((aetold - aettotal)**dtwo)
      IF ( NUMCELLS(L) > 0 ) THEN
        IF ( RMSEGW(L) > AGTOL*pettotal ) AGCONVERGE = 0
      END IF
      AETITERGW(l) = sngl(aettotal)
      QONLY(L) = QONLY(L) + (sone - REAL(AGCONVERGE))*SNGL(factor)
      if (QONLY(L) < 0.0) QONLY(L) = 0.0
      demandgw_prms = doneneg*QONLY(L)
      end function demandgw_prms
!
      double precision function set_factor(l,aetold, pettotal, 
     +                                     aettotal, sup, supold,
     +                                     kper, kstp, kiter)
!     ******************************************************************
!     updates diversion or pumping rate based on ET deficit
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFAGMODULE
      IMPLICIT NONE
! -----------------------------------------
      !modules
      !arguments
      double precision, intent(in) :: aetold, pettotal, aettotal,
     +                                sup, supold
      integer, intent(in) :: l, kper, kstp, kiter
      !dummy
      DOUBLE PRECISION :: factor, etdif, det, dq
! -----------------------------------------
!
      set_factor = dzero
      factor = dzero
      etdif = pettotal - aettotal
      det = (aettotal - aetold)
      factor = etdif
      dq = sup - supold
      
      if (kiter > 2) then
        if (abs(det) > dzero) then
          factor = dq*etdif/det
        end if
      end if
      if( factor > accel*etdif ) factor = accel*etdif
      if( factor < etdif ) factor = etdif
      if( factor < dzero ) factor = dzero
!      open(222,file='debug.out')
!      if(l==18)write(222,333)kper, kstp, kiter,pettotal,aettotal,dq,det,
!     +aettotal,aetold,factor,sup
!333   format(3i5,8e20.10)
      set_factor = factor
      end function set_factor
!
      subroutine timeseriesout(KKPER, KKSTP, TOTIM, agDemand, 
     +                         Nsegshold)
!     ******************************************************************
!     timeseriesout---- output to time series file each time step
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFUZFMODULE, ONLY: GWET, UZFETOUT, PETRATE
      USE GWFSFRMODULE, ONLY: DVRSFLW, SGOTFLW
      USE GWFAGMODULE
      USE GLOBAL, ONLY: DELR, DELC, ISSFLG
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_CONSTANTS, ONLY: MODSIM_GSFLOW
      USE PRMS_MODULE, ONLY: GSFLOW_flag, Nhru, Nhrucell, Gvr_cell_id,
     +    dprst_flag, Model
      USE PRMS_BASIN, ONLY: gsflow_ag_area, gsflow_ag_frac
      USE PRMS_CLIMATEVARS, ONLY: Potet
      USE PRMS_FLOWVARS, ONLY: gsflow_ag_actet, Dprst_vol_open
      USE GSFMODFLOW, ONLY: Mfl2_to_acre, Mfl_to_inch, Gwc_col, Gwc_row,
     +                      Mfq_to_inch_acres, MFQ_to_inch_acres_dble
      IMPLICIT NONE
! --------------------------------------
      !arguments
      INTEGER, INTENT(IN) :: KKPER, KKSTP, Nsegshold
      REAL, INTENT(IN) :: TOTIM
      DOUBLE PRECISION, INTENT(INOUT) :: agDemand(Nsegshold)
      !dummy
      DOUBLE PRECISION :: area, uzet, aet, pet, aettot, pettot, sub
      DOUBLE PRECISION :: Q, QQ, QQQ, DVT, prms_inch2mf_q
      integer :: k, iseg, ic, ir, i, l, UNIT, J, hru_id
      integer :: icell, irow, icol
! --------------------------------------
!
      !
      if ( ISSFLG(kkper) >= 1 ) return
      aettot = DZERO
      pettot = DZERO
      prms_inch2mf_q = DZERO
      sub = DZERO
                      
      !
      ! - -------OUTPUT TIME SERIES FOR SEGMENTS DIVERSIONS
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      IF (TSACTIVESW) THEN
         DO I = 1, NUMSW
            UNIT = TSSWUNIT(I)
            L = TSSWNUM(I)
            IF ( Model == MODSIM_GSFLOW ) THEN
              Q = agDemand(L)  !for MODSIM-GSFLOW
            ELSE
              Q = demand(L)
            ENDIF
            QQ = DVRSFLW(L)   !consider making this SGOTFLOW
            QQQ = SUPSEG(L)
            CALL timeseries(unit, Kkper, Kkstp, TOTIM, L,
     +                      Q, QQ, QQQ)
         END DO
      END IF
      !
      ! - -------OUTPUT TIME SERIES FOR ALL SEGMENTS DIVERSIONS
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      I = 0
      IF (TSSWALLUNIT > 0) THEN
         DO L = 1, NUMIRRDIVERSION
            UNIT = TSSWALLUNIT
            iseg = IRRSEG(L)
            IF ( iseg > 0 ) THEN
              IF ( Model == MODSIM_GSFLOW ) THEN
                Q = Q + agDemand(iseg)  !for MODSIM-GSFLOW
              ELSE
                Q = Q + demand(iseg)
              ENDIF
              QQ = QQ + DVRSFLW(iseg)   !consider making this SGOTFLOW
              QQQ = QQQ + SUPSEG(iseg)
            END IF
         END DO
         CALL timeseries(unit, Kkper, Kkstp, TOTIM, I,
     +                      Q, QQ, QQQ)
      END IF
            !
      ! - ----Output time series of ET irrigated by all diversions
      ! - ----Total ET for all cells irrigated by segments for all diversions
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      i = 0
      if ( TSSWETALLUNIT > 0 ) then
         prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
         do L = 1, NUMIRRDIVERSION
            aettot = DZERO
            pettot = DZERO
            UNIT = TSSWETALLUNIT
            iseg = IRRSEG(L)
            if ( iseg > 0 ) then
              do k = 1, DVRCH(iseg)  !cells per segement
               IF (ETDEMANDFLAG > 0 .OR. TRIGGERFLAG > 0) THEN
                  IF (GSFLOW_flag == 0) THEN
                     ic = IRRCOL_SW(k, iseg)
                     ir = IRRROW_SW(k, iseg)
                     area = delr(ic)*delc(ir)
                     pet = PETRATE(ic, ir)*area
                     uzet = uzfetout(ic, ir)/DELT
                     aet = gwet(ic, ir) + uzet  !vol rate
                  ELSE
                     hru_id = IRRROW_SW(k, iseg)
                     area = gsflow_ag_area(hru_id)
                     pet = Potet(hru_id)*area*prms_inch2mf_q
                     aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
                     if ( Nhru==Nhrucell ) then
                       icell = Gvr_cell_id(hru_id)
                       irow = Gwc_row(icell)
                       icol = Gwc_col(icell)
                       uzet = gsflow_ag_frac(hru_id)*
     +                        UZFETOUT(icol, irow)/DELT
                       aet = aet + uzet + gsflow_ag_frac(hru_id)*
     +                                    gwet(icol, irow)
                     end if
                  end if
                  aettot = aettot + aet
                  pettot = pettot + pet
               ELSE
                  dvt = SGOTFLW(iseg)*DVRPERC(k, iseg)
                  aettot = aettot + dvt*DVEFF(k, iseg)
                  pettot = aettot
               END IF
              end do
            end if
            Q = pettot
            QQ = aettot
            QQQ = DZERO
         end do
         CALL timeseries(unit, Kkper, Kkstp, TOTIM, i,
     +                      Q, QQ, QQQ)
      end if
      !
      ! - -------OUTPUT TIME SERIES FOR INFLOWS AND OUTFLOWS TO PONDS
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      IF (TSACTIVEPOND) THEN
         DO j = 1, NUMPOND
            k = TSPONDNUM(j)
            DO i = 1, NUMIRRPOND
              hru_id = IRRPONDVAR(i)  !these are hru ids for ponds
              IF ( hru_id == k ) THEN
                UNIT = TSPONDUNIT(j)
                Q = PONDSEGFLOW(I)
                QQ = PONDFLOW(I)
                QQQ = DZERO
                if ( dprst_flag == 1 )   !uncommment this and next line
     +               QQQ = Dprst_vol_open(hru_id)/MFQ_to_inch_acres_dble
                CALL timeseries(unit, Kkper, Kkstp, TOTIM, hru_id,
     +                          Q, QQ, QQQ)
              END IF
           END DO
         END DO
      END IF
            !
      ! - ----Output time series of ET irrigated by ponds
      ! - ----Total ET for all cells irrigated by each pond
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      if (TSACTIVEPONDET) then
        do j = 1, NUMPONDET
          k = TSPONDETNUM(j)
          do i = 1, NUMIRRPOND
             hru_id = IRRPONDVAR(i)  !these are hru ids for ponds
             if ( hru_id == k ) then
               Q = DBLE(PETPOND(i))
               QQ = DBLE(AETITERPOND(i))
               QQQ = DZERO
               unit = TSPONDETUNIT(j)
               CALL timeseries(unit, Kkper, Kkstp, TOTIM, hru_id,
     +                         Q, QQ, QQQ)
             end if
          end do
        end do
      end if
            !
      ! - -------OUTPUT TIME SERIES FOR INFLOWS AND OUTFLOWS TO PONDS
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      IF (TSACTIVEALLPOND) THEN
         UNIT = TSPONDALLUNIT
         DO i = 1, NUMIRRPONDSP
           k = TSPONDNUM(I)
           Q = Q + PONDSEGFLOW(I)
           QQ = QQ + PONDFLOW(I)
           hru_id = IRRPONDVAR(I)
           sub = DZERO
           if ( dprst_flag == 1 ) then    !uncomment this and next 4 lines
             if ( ISSFLG(kkper) == 0 ) sub = 
     +            Dprst_vol_open(hru_id)/MFQ_to_inch_acres
             if ( sub < DZERO ) sub = DZERO
             QQQ = QQQ + sub
           end if
         END DO
         hru_id = 0
         CALL timeseries(unit, Kkper, Kkstp, TOTIM, hru_id,
     +                          Q, QQ, QQQ)
       END IF
            !
      ! - ----Output time series of ET irrigated by ponds
      ! - ----Total ET for all cells irrigated by each pond
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      if (TSACTIVEALLPONDET) then
        unit = TSPONDETALLUNIT
        hru_id = 0  !dummy value for all ponds
        do i = 1, NUMIRRPONDSP
          Q = Q + DBLE(PETPOND(i))
          QQ = QQ + DBLE(AETITERPOND(i))
          QQQ = DZERO
        end do
        CALL timeseries(unit, Kkper, Kkstp, TOTIM, hru_id,
     +                         Q, QQ, QQQ)
      end if
      
      !
      ! - ----Output time series of ET irrigated by diversions
      ! - ----Total ET for all cells irrigated by each diversion
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      if (TSACTIVESWET) then
         prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
         do I = 1, NUMSWET
            aettot = DZERO
            pettot = DZERO
            UNIT = TSSWETUNIT(I)
            iseg = TSSWETNUM(I)
            do k = 1, DVRCH(iseg)  !cells per segement
               IF (ETDEMANDFLAG > 0 .OR. TRIGGERFLAG > 0) THEN
                  IF (GSFLOW_flag == 0) THEN
                     ic = IRRCOL_SW(k, iseg)
                     ir = IRRROW_SW(k, iseg)
                     area = delr(ic)*delc(ir)
                     pet = PETRATE(ic, ir)*area
                     uzet = uzfetout(ic, ir)/DELT
                     aet = gwet(ic, ir) + uzet  !vol rate
                  ELSE
                     hru_id = IRRROW_SW(k, iseg)
                     area = gsflow_ag_area(hru_id)
                     pet = Potet(hru_id)*area*prms_inch2mf_q
                     aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
                     if ( Nhru==Nhrucell ) then
                       icell = Gvr_cell_id(hru_id)
                       irow = Gwc_row(icell)
                       icol = Gwc_col(icell)
                       uzet = gsflow_ag_frac(hru_id)*
     +                        UZFETOUT(icol, irow)/DELT
                       aet = aet + uzet + gsflow_ag_frac(hru_id)*
     +                                    gwet(icol, irow)
                     end if
                  end if
                  aettot = aettot + aet
                  pettot = pettot + pet
               ELSE
                  dvt = SGOTFLW(iseg)*DVRPERC(k, iseg)
                  aettot = aettot + dvt*DVEFF(k, iseg)
                  pettot = aettot
               END IF
            end do
            Q = pettot
            QQ = aettot
            QQQ = DZERO
            CALL timeseries(unit, Kkper, Kkstp, TOTIM, iseg,
     +                      Q, QQ, QQQ)
         end do
      end if
      !
      ! - -------OUTPUT TIME SERIES FOR WELL
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      DO L = 1, NWELLS
         IF (TSACTIVEGW) THEN
            DO I = 1, NUMGW
               IF (TSGWNUM(I) == L) THEN
                  UNIT = TSGWUNIT(I)
                  Q = QONLY(L)
                  QQ = -1.0*WELL(NWELVL, L)
                  QQQ = 0.0
                  CALL timeseries(unit, Kkper, Kkstp, TOTIM, L,
     +                            Q, QQ, QQQ)
               END IF
            END DO
         END IF
         !
         ! - ----Total ET for all cells irrigated by each well
         !
         Q = DZERO
         QQ = DZERO
         QQQ = DZERO
         aettot = DZERO
         pettot = DZERO
         IF (TSACTIVEGWET) THEN
            prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
            DO I = 1, NUMGWET
               pettot = DZERO
               aettot = DZERO
               IF (TSGWETNUM(I) == L) THEN
                  UNIT = TSGWETUNIT(I)
                  do J = 1, NUMCELLS(L)
                     IF (ETDEMANDFLAG > 0) THEN
                        IF (GSFLOW_flag == 0) THEN
                           IC = IRRCOL_GW(J, L)
                           IR = IRRROW_GW(J, L)
                           area = delr(ic)*delc(ir)
                           pet = PETRATE(ic, ir)*area
                           uzet = uzfetout(ic, ir)/DELT
                           aet = gwet(ic, ir) + uzet
                        ELSE
                           hru_id = IRRROW_GW(J, L)
                           area = gsflow_ag_area(hru_id)
                           pet = Potet(hru_id)*area*prms_inch2mf_q
                           aet = gsflow_ag_actet(hru_id)*area*
     +                           prms_inch2mf_q
                           if ( Nhru==Nhrucell ) then
                             icell = Gvr_cell_id(hru_id)
                             irow = Gwc_row(icell)
                             icol = Gwc_col(icell)
                             uzet = gsflow_ag_frac(hru_id)*
     +                              UZFETOUT(icol, irow)/DELT
                             aet = aet + uzet + gsflow_ag_frac(hru_id)*
     +                                          gwet(icol, irow)
                          end if
                        END IF
                        pettot = pettot + pet
                        aettot = aettot + aet
                     ELSE
                        QQ = WELL(NWELVL, L)
                        aettot = aettot - IRRFACT(J, L)*
     +                           QQ*IRRFIELDFACT(J, L)
                        pettot = pettot - IRRFACT(J, L)*
     +                           QQ*IRRFIELDFACT(J, L)
                     END IF
                  end do
                  QQQ = DZERO
                  QQ = aettot
                  Q = pettot
                  CALL timeseries(unit, Kkper, Kkstp, TOTIM, L,
     +                            Q, QQ, QQQ)
               end if
            END DO
         END IF
      END DO
      !
      ! - ------TOTAL ET FOR ALL WELLS USED FOR IRRIGATION
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      aettot = DZERO
      pettot = DZERO
      IF (TSGWETALLUNIT > 0) THEN 
         prms_inch2mf_q = done/(DELT*Mfl2_to_acre*Mfl_to_inch)
         DO L = 1, NWELLS
            UNIT = TSGWETALLUNIT
            do J = 1, NUMCELLS(L)
               IF (ETDEMANDFLAG > 0 .OR. TRIGGERFLAG > 0) THEN
                  IF (GSFLOW_flag == 0) THEN
                     IC = IRRCOL_GW(J, L)
                     IR = IRRROW_GW(J, L)
                     area = delr(ic)*delc(ir)
                     pet = PETRATE(ic, ir)*area
                     uzet = uzfetout(ic, ir)/DELT
                     aet = gwet(ic, ir) + uzet
                  ELSE
                     hru_id = IRRROW_GW(J, L)
                     area = gsflow_ag_area(hru_id)
                     pet = Potet(hru_id)*area*prms_inch2mf_q
                     aet = gsflow_ag_actet(hru_id)*area*prms_inch2mf_q
                     if ( Nhru==Nhrucell ) then
                       icell = Gvr_cell_id(hru_id)
                       irow = Gwc_row(icell)
                       icol = Gwc_col(icell)
                       uzet = gsflow_ag_frac(hru_id)*
     +                        UZFETOUT(icol, irow)/DELT
                       aet = aet + uzet + gsflow_ag_frac(hru_id)*
     +                                    gwet(icol, irow)
                     end if
                  END IF
                  pettot = pettot + pet
                  aettot = aettot + aet
               ELSE
                  QQ = WELL(NWELVL, L)
                  aettot = aettot - IRRFACT(J, L)*QQ*IRRFIELDFACT(J, L)
                  pettot = pettot - IRRFACT(J, L)*QQ*IRRFIELDFACT(J, L)
               END IF
            end do
         END DO
         QQ = aettot
         Q = pettot
         QQQ = DZERO
         J = 0
         CALL timeseries(unit, Kkper, Kkstp, TOTIM, J,
     +                   Q, QQ, QQQ)
      END IF
      !
      !----TOTAL GW USED BY ALL WELLS FOR IRRIGATION
      !
      Q = DZERO
      QQ = DZERO
      QQQ = DZERO
      IF (TSGWALLUNIT > 0) THEN
         Q = DZERO
         QQ = DZERO
         J = 0
         DO L = 1, NWELLS
            UNIT = TSGWALLUNIT
            Q = Q + QONLY(L)
            QQ = DONENEG*WELL(NWELVL, L) + QQ
            QQQ = DZERO
         END DO
         CALL timeseries(unit, Kkper, Kkstp, TOTIM, J,
     +                   Q, QQ, QQQ)
      END IF
      RETURN
      END SUBROUTINE TIMESERIESOUT
!
!
      subroutine timeseries(unit, Kkper, Kkstp, time, id, qd, q, qq)
!     ******************************************************************
!     timeseries---- write AG water use time series for SW and GW use
!     ******************************************************************
!     SPECIFICATIONS:
      USE GWFAGMODULE
      IMPLICIT NONE
! --------------------------------------
      !arguments
      integer, intent(in) :: kkper, kkstp, id, unit
      real, intent(in) :: time
      double precision, intent(in) :: qd, q, qq
!
! --------------------------------------
!
      write (unit, 2) time, kkper, kkstp, id, qd, q, qq
2     format(E20.10, 3I10, 3E20.10)
      return
      end subroutine timeseries
!
!
      DOUBLE PRECISION FUNCTION smoothQ(H, T, B, dQ)
!******************************************************************
! SMOOTHLY REDUCES PUMPING TO ZERO FOR DEWATERED CONDITIONS
!******************************************************************
! h is the depth
      USE GWFAGMODULE
      IMPLICIT NONE
      DOUBLE PRECISION s, aa, bb, x
      DOUBLE PRECISION cof1, cof2, cof3, Qp
      DOUBLE PRECISION, INTENT(IN) :: H
      DOUBLE PRECISION, INTENT(IN) :: T
      DOUBLE PRECISION, INTENT(IN) :: B
      DOUBLE PRECISION, INTENT(OUT) :: dQ
      smoothQ = DZERO
      s = PSIRAMP
      s = s*(T - B)   ! puming rate begins to be ramped down.
      x = (H - B)
      aa = -6.0d0/(s**3.0d0)
      bb = -6.0d0/(s**2.0d0)
      cof1 = x**2.0D0
      cof2 = -(2.0D0*x)/(s**3.0D0)
      cof3 = 3.0D0/(s**2.0D0)
      Qp = cof1*(cof2 + cof3)
      dQ = (aa*x**2.0D0 - bb*x)
      IF (x .LT. DZERO) THEN
         Qp = DZERO
         dQ = DZERO
      ELSEIF (x - s .GT. -1.0e-14) THEN
         Qp = DONE
         dQ = DZERO
      END IF
      smoothQ = Qp
      END FUNCTION smoothQ
      !
      REAL FUNCTION RATETERPQ(TIME, TABTIME, TABRATE, NVAL)
      !******************************************************************
      ! LINEARLY INTERPOLATE PUMPING RATE FROM TABFILE
      !******************************************************************
      ! FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
      ! OF TIME TO CACULATE SPECIFIED PUMPING RATES.
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
      !ARGUMENTS
      REAL, INTENT(IN):: TIME
      REAL CLOSEZERO
      REAL FLOW, TIMEBEG, TIMEND, TIMESTART, SUMFLOW, TOLF2
      INTEGER IEND, ISTM1, ISTART, iflg, NVAL, I
      REAL :: TABTIME(NVAL), TABRATE(NVAL)
      TOLF2 = 1.0E-4
      CLOSEZERO = 1.0E-15
      FLOW = SZERO
      IFLG = 0
      SUMFLOW = SZERO
      I = 1
      TIMEBEG = TIME - DELT
      IF (TIMEBEG - TABTIME(1) .LT. SZERO) THEN
         RATETERPQ = TABRATE(1)
      ELSEIF (TIMEBEG - TABTIME(NVAL) .GE. SZERO) THEN
         RATETERPQ = TABRATE(NVAL)
      ELSE
         ! Find table value before beginning of time step.
         DO WHILE (I .LE. NVAL - 1)
            IF (TIMEBEG - TABTIME(I) .LE. CLOSEZERO) THEN
               EXIT
            ELSEIF (TIMEBEG - TABTIME(I + 1) .LE. 
     +              CLOSEZERO) THEN
               EXIT
            ELSE
               I = I + 1
            END IF
         END DO
         ISTART = I
         ISTM1 = I
         IF (I .GT. 1) ISTM1 = ISTM1 - 1
         ! Find table value after end of time step
         DO WHILE (I .LE. NVAL)
            IF (TIME - TABTIME(I) .LE. SZERO) THEN
               EXIT
            ELSE
               I = I + 1
            END IF
         END DO
         IEND = I
         IF (IEND .GT. NVAL) IEND = NVAL
         DO I = ISTART, IEND - 1
            TIMESTART = TABTIME(I)
            TIMEND = TABTIME(I + 1)
            IF (TIMEBEG - TIMESTART .GT. SZERO) TIMESTART = TIMEBEG
            IF (TIME - TIMEND .LT. SZERO) TIMEND = TIME
            SUMFLOW = SUMFLOW + (TIMEND - TIMESTART)*
     +                TABRATE(I)
         END DO
         RATETERPQ = SUMFLOW/DELT
      END IF
      RETURN
      END FUNCTION RATETERPQ
!
      SUBROUTINE SGWF2AG7V(MSUM, VBNMAG, VBVLAG, KSTP, KPER, IOUT, 
     +                    BUDPERC)
      !******************************************************************
      ! PRINT VOLUMETRIC BUDGET
      !******************************************************************
      !
      ! SPECIFICATIONS:
      ! - -----------------------------------------------------------------
      INTEGER, INTENT(IN) :: MSUM, IOUT, KSTP, KPER
      CHARACTER*22 VBNMAG(MSUM)
      DIMENSION VBVLAG(4, MSUM)
      CHARACTER*17 VAL1, VAL2
      INTEGER MSUM1, L
      REAL BUDPERC, TOTRIN, TOTROT, TOTVIN, TOTVOT, VBVLAG
      REAL DIFFR, ADIFFR, PDIFFR, AVGRAT, DIFFV, ADIFFV, PDIFFV, AVGVOL
      REAL, PARAMETER :: ZERO = 0.0, TWO = 2.0, SMALL = 0.1, HUND = 100.
      REAL, PARAMETER :: BIGVL1 = 9.99999E11, BIGVL2 = 9.99999E10
      ! - -----------------------------------------------------------------
      !
      !1 - -----DETERMINE NUMBER OF INDIVIDUAL BUDGET ENTRIES.
      BUDPERC = 0.
      MSUM1 = MSUM - 1
      IF (MSUM1 < 1) RETURN
      !
      !2 - -----CLEAR RATE AND VOLUME ACCUMULATORS.
      TOTRIN = ZERO
      TOTROT = ZERO
      TOTVIN = ZERO
      TOTVOT = ZERO
      !
      !3 - -----ADD RATES AND VOLUMES(IN AND OUT) TO ACCUMULATORS.
      DO 100 L = 1, MSUM1
      TOTRIN = TOTRIN + VBVLAG(3, L)
      TOTROT = TOTROT + VBVLAG(4, L)
      TOTVIN = TOTVIN + VBVLAG(1, L)
      TOTVOT = TOTVOT + VBVLAG(2, L)
100   CONTINUE
      !
      !4 - -----PRINT TIME STEP NUMBER AND STRESS PERIOD NUMBER.
      WRITE (IOUT, 260) KSTP, KPER
      WRITE (IOUT, 265)
      !
      !5 - -----PRINT INDIVIDUAL INFLOW RATES AND VOLUMES AND THEIR TOTALS.
      DO 200 L = 1, MSUM1
         IF (VBVLAG(1, L) .NE. ZERO .AND.
     +        (VBVLAG(1, L) .GE. BIGVL1 .OR. VBVLAG(1, L) 
     +         .LT. SMALL)) THEN
         WRITE (VAL1, '(1PE17.4)') VBVLAG(1, L)
         ELSE
         WRITE (VAL1, '(F17.4)') VBVLAG(1, L)
         END IF
         IF (VBVLAG(3, L) .NE. ZERO .AND.
     +        (VBVLAG(3, L) .GE. BIGVL1 .OR. VBVLAG(3, L) .LT. 
     +         SMALL)) THEN
         WRITE (VAL2, '(1PE17.4)') VBVLAG(3, L)
         ELSE
         WRITE (VAL2, '(F17.4)') VBVLAG(3, L)
         END IF
         WRITE (IOUT, 275) VBNMAG(L), VAL1, VBNMAG(L), VAL2
200   CONTINUE
      IF (TOTVIN .NE. ZERO .AND.
     +     (TOTVIN .GE. BIGVL1 .OR. TOTVIN .LT. SMALL)) THEN
         WRITE (VAL1, '(1PE17.4)') TOTVIN
      ELSE
         WRITE (VAL1, '(F17.4)') TOTVIN
      END IF
      IF (TOTRIN .NE. ZERO .AND.
     +     (TOTRIN .GE. BIGVL1 .OR. TOTRIN .LT. SMALL)) THEN
         WRITE (VAL2, '(1PE17.4)') TOTRIN
      ELSE
         WRITE (VAL2, '(F17.4)') TOTRIN
      END IF
      WRITE (IOUT, 286) VAL1, VAL2
      !
      !6 - -----PRINT INDIVIDUAL OUTFLOW RATES AND VOLUMES AND THEIR TOTALS.
      WRITE (IOUT, 287)
      DO 250 L = 1, MSUM1
         IF (VBVLAG(2, L) .NE. ZERO .AND.
     +        (VBVLAG(2, L) .GE. BIGVL1 .OR. VBVLAG(2, L) 
     +         .LT. SMALL)) THEN
         WRITE (VAL1, '(1PE17.4)') VBVLAG(2, L)
         ELSE
         WRITE (VAL1, '(F17.4)') VBVLAG(2, L)
         END IF
         IF (VBVLAG(4, L) .NE. ZERO .AND.
     +        (VBVLAG(4, L) .GE. BIGVL1 .OR. VBVLAG(4, L) 
     +        .LT. SMALL)) THEN
         WRITE (VAL2, '(1PE17.4)') VBVLAG(4, L)
         ELSE
         WRITE (VAL2, '(F17.4)') VBVLAG(4, L)
         END IF
         WRITE (IOUT, 275) VBNMAG(L), VAL1, VBNMAG(L), VAL2
250   CONTINUE
      IF (TOTVOT .NE. ZERO .AND.
     +     (TOTVOT .GE. BIGVL1 .OR. TOTVOT .LT. SMALL)) THEN
      WRITE (VAL1, '(1PE17.4)') TOTVOT
      ELSE
      WRITE (VAL1, '(F17.4)') TOTVOT
      END IF
      IF (TOTROT .NE. ZERO .AND.
     +     (TOTROT .GE. BIGVL1 .OR. TOTROT .LT. SMALL)) THEN
        WRITE (VAL2, '(1PE17.4)') TOTROT
      ELSE
        WRITE (VAL2, '(F17.4)') TOTROT
      END IF
      WRITE (IOUT, 298) VAL1, VAL2
      !
      !7 - -----CALCULATE THE DIFFERENCE BETWEEN INFLOW AND OUTFLOW.
      !
      !7A - ----CALCULATE DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      DIFFR = TOTRIN - TOTROT
      ADIFFR = ABS(DIFFR)
      !
      !7B - ----CALCULATE PERCENT DIFFERENCE BETWEEN RATE IN AND RATE OUT.
      PDIFFR = ZERO
      AVGRAT = (TOTRIN + TOTROT)/TWO
      IF (AVGRAT .NE. ZERO) PDIFFR = HUND*DIFFR/AVGRAT
      BUDPERC = PDIFFR
      !
      !7C - ----CALCULATE DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      DIFFV = TOTVIN - TOTVOT
      ADIFFV = ABS(DIFFV)
      !
      !7D-----GET PERCENT DIFFERENCE BETWEEN VOLUME IN AND VOLUME OUT.
      PDIFFV = ZERO
      AVGVOL = (TOTVIN + TOTVOT)/TWO
      IF (AVGVOL .NE. ZERO) PDIFFV = HUND*DIFFV/AVGVOL
      !
      !8 - -----PRINT DIFFERENCES AND PERCENT DIFFERENCES BETWEEN INPUT
      !8 - -----AND OUTPUT RATES AND VOLUMES.
      IF (ADIFFV .NE. ZERO .AND.
     +     (ADIFFV .GE. BIGVL2 .OR. ADIFFV .LT. SMALL)) THEN
        WRITE (VAL1, '(1PE17.4)') DIFFV
      ELSE
        WRITE (VAL1, '(F17.4)') DIFFV
      END IF
      IF (ADIFFR .NE. ZERO .AND.
     +     (ADIFFR .GE. BIGVL2 .OR. ADIFFR .LT. SMALL)) THEN
        WRITE (VAL2, '(1PE17.4)') DIFFR
      ELSE
        WRITE (VAL2, '(F17.4)') DIFFR
      END IF
      WRITE (IOUT, 299) VAL1, VAL2
      WRITE (IOUT, 300) PDIFFV, PDIFFR
      !
      !9 - -----RETURN.
      RETURN
      !
      ! - --FORMATS
      !
260   FORMAT('1', /2X, 'VOLUMETRIC BUDGET FOR AGRICULTURAL FIELDS AT '
     +     , ' END OF TIME STEP', I5, ', STRESS PERIOD', I4/2X, 78('-'))
265   FORMAT(1X, /5X, 'CUMULATIVE VOLUMES', 6X, 'L**3', 7X
     +     , 'RATES FOR THIS TIME STEP', 6X, 'L**3/T'/5X, 18('-'), 17X, 
     +     24('-')//11X, 'IN:', 38X, 'IN:'/11X, '---', 38X, '---')
275   FORMAT(1X, 3X, A20, ' =', A17, 6X, A20, ' =', A17)
286   FORMAT(1X, /16X, 'TOTAL IN =', A, 18X, 'TOTAL IN =', A)
287   FORMAT(1X, /10X, 'OUT:', 37X, 'OUT:'/10X, 4('-'), 37X, 4('-'))
298   FORMAT(1X, /15X, 'TOTAL OUT =', A, 17X, 'TOTAL OUT =', A)
299   FORMAT(1X, /16X, 'IN - OUT =', A, 18X, 'IN - OUT =', A)
300   FORMAT(1X, /5X, 'PERCENT DISCREPANCY =', F15.2
     +     , 9X, 'PERCENT DISCREPANCY =', F15.2, ///)
!
      END

C
C-------SUBROUTINE SFR2MODSIM
C
      SUBROUTINE AG2MODSIM(agDemand)
C     *******************************************************************
C     Pass Irrigation demand to MODSIM 
C     
!--------January 17, 2022
C     *******************************************************************
      USE GWFSFRMODULE, ONLY: SEG, NSS
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
      DOUBLE PRECISION, INTENT(INOUT) :: agDemand(NSS)
C     -------------------------------------------------------------------
!      INTEGER 
!      DOUBLE PRECISION 
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER :: ISEG, i
!      double precision :: total !delete this
C     -------------------------------------------------------------------
C
C1------LOOP OVER SEGMETS
C
C
C2------Set diversion demand from that calculated from AG.
C 
        agDemand = 0.0
        do i = 1, NUMIRRDIVERSIONSP
          iseg = IRRSEG(i)        
          !IF ( ABS(IDIVAR(1, ISEG)) > 0 ) THEN
            agDemand(ISEG) = SEG(2,iseg)*DELT
          !END IF
        END DO
C
C8------RETURN.
      RETURN
      END SUBROUTINE AG2MODSIM
C
C-------SUBROUTINE MODSIM2AG
C
      SUBROUTINE MODSIM2AG(Diversions)
C     *******************************************************************
C     Update AF diversion to MODSIM diversion
C     
!-------October 5, 2022
C     *******************************************************************
      USE GWFSFRMODULE, ONLY: SEG, NSS
      USE GWFAGMODULE
      USE GWFBASMODULE, ONLY: DELT
      USE PRMS_MODULE, ONLY: Kkiter
      IMPLICIT NONE
C     -------------------------------------------------------------------
C     SPECIFICATIONS:
C     -------------------------------------------------------------------
C     ARGUMENTS
      DOUBLE PRECISION, INTENT(INOUT) :: Diversions(NSS)
C     -------------------------------------------------------------------
!      INTEGER 
!      DOUBLE PRECISION 
C     -------------------------------------------------------------------
C     LOCAL VARIABLES
C     -------------------------------------------------------------------
      INTEGER :: ISEG, i
!      double precision :: total !delete this
C     -------------------------------------------------------------------
C
C1------LOOP OVER SEGMETS
C
C
C2------Set diversion demand from that calculated from AG.
C 
        if ( kkiter == 1 ) DIVERSIONIRRPRMS = dzero
        do i = 1, NUMIRRDIVERSIONSP
          iseg = IRRSEG(i)        
          !IF ( ABS(IDIVAR(1, ISEG)) > 0 ) THEN
           if ( SEG(2,iseg) > Diversions(ISEG)/DELT ) 
     +          SEG(2,iseg) = Diversions(ISEG)/DELT
          !END IF
        END DO
C
C8------RETURN.
      RETURN
      END SUBROUTINE MODSIM2AG
      !
      SUBROUTINE GWF2AG7DA()
      ! Deallocate AG MEMORY
      USE GWFAGMODULE
      DEALLOCATE(NUMSUP)
      DEALLOCATE(NUMSUPSP)
      DEALLOCATE(NUMSUPWELLSEG)
      DEALLOCATE(NUMIRRWEL)
      DEALLOCATE(DIVERSIONSEG)
      DEALLOCATE(IRRROW_GW)
      DEALLOCATE(IRRCOL_GW)
      DEALLOCATE(SUPWELVAR)
      DEALLOCATE(IRRWELVAR)
      DEALLOCATE(NUMSEGS)
      DEALLOCATE(MAXSEGS)
      DEALLOCATE(MAXCELLSWEL)
      DEALLOCATE(NUMCELLS)
      DEALLOCATE(WELLIRRUZF)
      DEALLOCATE(WELLIRRPRMS)
      DEALLOCATE(IRRFACT)
      DEALLOCATE(IRRFIELDFACT)
      DEALLOCATE(PSIRAMP)
      DEALLOCATE(IUNITRAMP)
      DEALLOCATE(NWELLS)
      DEALLOCATE(MXWELL)
      DEALLOCATE(NWELVL)
      DEALLOCATE(IWELLCB)
      DEALLOCATE(IWELLCBU)
      DEALLOCATE(ISFRCB)
      DEALLOCATE(IRRWELLCB)
      DEALLOCATE(IRRSFRCB)
      DEALLOCATE(IPRWEL)
      DEALLOCATE(NPWEL)
      DEALLOCATE(NNPWEL)
!      DEALLOCATE(WELL)
      DEALLOCATE(NUMTABWELL)
      DEALLOCATE(NUMTABPOND)
      DEALLOCATE(MAXVALWELL)
      DEALLOCATE(MAXVALPOND)
      DEALLOCATE(TABTIMEWELL)
      DEALLOCATE(TABRATEWELL)
      DEALLOCATE(TABVALWELL)
      DEALLOCATE(TABIDWELL)
      DEALLOCATE(TABUNITWELL)
      DEALLOCATE(KCROPDIVERSION)
      DEALLOCATE(DVRCH)
      DEALLOCATE(DVEFF)
      DEALLOCATE(IRRROW_SW)
      DEALLOCATE(IRRCOL_SW)
      DEALLOCATE(DIVERSIONIRRUZF)
      DEALLOCATE(DIVERSIONIRRPRMS)
      DEALLOCATE(DVRPERC)
      DEALLOCATE(IDVFLG)
      DEALLOCATE(NUMIRRDIVERSION)
      DEALLOCATE(NUMIRRDIVERSIONSP)
      DEALLOCATE(MAXCELLSDIVERSION)
      DEALLOCATE(DEMAND)
      DEALLOCATE(ACTUAL)
      DEALLOCATE(ACTUALOLD)
      DEALLOCATE(SUPACT)
      DEALLOCATE(NUMIRRWELSP)
      DEALLOCATE(TSSWUNIT)
      DEALLOCATE(TSGWUNIT)
      DEALLOCATE(TSSWNUM)
      DEALLOCATE(TSGWNUM)
      DEALLOCATE(NUMSW)
      DEALLOCATE(NUMGW)
      DEALLOCATE(TSACTIVESW)
      DEALLOCATE(TSACTIVEGW)
      DEALLOCATE(QONLY)
      DEALLOCATE(TSSWETUNIT)
      DEALLOCATE(TSGWETUNIT)
      DEALLOCATE(TSSWETNUM)
      DEALLOCATE(TSGWETNUM)
      DEALLOCATE(SUPSEG)
      DEALLOCATE(TSGWETALLUNIT)
      DEALLOCATE(TSSWETALLUNIT)
      DEALLOCATE(TSSWALLUNIT)
      DEALLOCATE(NSEGDIMTEMP)
      DEALLOCATE(LASTREACH)
      DEALLOCATE(KCROPWELL)
      DEALLOCATE(FRACSUP)
      DEALLOCATE(FRACSUPMAX)
      DEALLOCATE(IRRPERIODWELL)
      DEALLOCATE(IRRPERIODSEG)
      DEALLOCATE(TRIGGERPERIODWELL)
      DEALLOCATE(TRIGGERPERIODSEG)
      DEALLOCATE(TIMEINPERIODWELL)
      DEALLOCATE(TIMEINPERIODSEG)
      DEALLOCATE(SEGLIST)
      DEALLOCATE(NUMSEGLIST)
      DEALLOCATE(ACCEL)
      DEALLOCATE(AGTOL)
      DEALLOCATE(MAXAGITER)
      DEALLOCATE(MAXCELLSPOND)
      DEALLOCATE(IPONDCB)
      DEALLOCATE(IPONDCBU)
      DEALLOCATE(IRRPONDCB)
      DEALLOCATE(TSACTIVEPOND)
      DEALLOCATE(TSACTIVEPONDET)
      DEALLOCATE(IRRHRU_POND)
      DEALLOCATE(IRRPERIODPOND)
      DEALLOCATE(TRIGGERPERIODPOND)
      DEALLOCATE(TIMEINPERIODPOND)
      DEALLOCATE(NUMIRRPONDSP)
      DEALLOCATE(AETITERPOND)
      DEALLOCATE(PONDFLOW)
      DEALLOCATE(PONDFLOWOLD)
      DEALLOCATE(PONDFLOWMAX)
      DEALLOCATE(TSPONDUNIT)
      DEALLOCATE(TSPONDNUM)
      DEALLOCATE(TSPONDETUNIT)
      DEALLOCATE(TSPONDETNUM)
      DEALLOCATE(NUMPOND)
      DEALLOCATE(NUMPONDET)
      DEALLOCATE(TABPONDSEG)
      DEALLOCATE(TABPONDFRAC)
      DEALLOCATE(AETITERGW)
      DEALLOCATE(AETITERSW)
      DEALLOCATE(RMSESW)
      DEALLOCATE(RMSEGW)
      DEALLOCATE(RMSEPOND)
      DEALLOCATE(PONDSEGFRAC)
      DEALLOCATE(FLOWTHROUGH_POND)
      DEALLOCATE(PONDSTOROLD)
      DEALLOCATE(PONDSTORNEW)
      RETURN
      END

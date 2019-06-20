      MODULE GWFWELMODULE
        INTEGER,SAVE,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,SAVE,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   POINTER     ::WELAUX
        REAL,             SAVE, DIMENSION(:,:), POINTER     ::WELL
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,             SAVE, DIMENSION(:,:),   POINTER     ::TABRATE
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABROW
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,          SAVE, DIMENSION(:),   POINTER     ::TABVAL
        REAL,             SAVE,                 POINTER     ::PSIRAMP
        INTEGER,          SAVE,                 POINTER     ::IUNITRAMP
        INTEGER,          SAVE,                 POINTER     ::NUMTAB
        INTEGER,          SAVE,                 POINTER     ::MAXVAL
      TYPE GWFWELTYPE
        INTEGER,POINTER  ::NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL
        INTEGER,POINTER  ::NPWEL,IWELPB,NNPWEL,IRDPSI
        CHARACTER(LEN=16), DIMENSION(:),   POINTER     ::WELAUX
        REAL,              DIMENSION(:,:), POINTER     ::WELL
        REAL,              DIMENSION(:,:),   POINTER     ::TABTIME
        REAL,              DIMENSION(:,:),   POINTER     ::TABRATE
        INTEGER,           DIMENSION(:),   POINTER     ::TABLAY
        INTEGER,           DIMENSION(:),   POINTER     ::TABROW
        INTEGER,           DIMENSION(:),   POINTER     ::TABCOL
        INTEGER,           DIMENSION(:),   POINTER     ::TABVAL
        REAL,                              POINTER     ::PSIRAMP
        INTEGER,                           POINTER     ::IUNITRAMP
        INTEGER,                           POINTER     ::NUMTAB
        INTEGER,                           POINTER     ::MAXVAL
      END TYPE
      TYPE(GWFWELTYPE), SAVE:: GWFWELDAT(10)
      END MODULE GWFWELMODULE


      SUBROUTINE GWF2WEL7AR(IN,IUNITNWT,IGRID)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR WELL PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNIT
      USE GWFWELMODULE
      integer,intent(in) :: IUNITNWT,IGRID
      integer,intent(inout) :: IN
C
      CHARACTER(len=200) :: LINE
      CHARACTER(len=16) :: text        = ' WELL PACKAGE '
      LOGICAL :: found
      INTEGER intchk, Iostat
      INTEGER NUMTABHOLD
      DOUBLE PRECISION :: CLOSEZERO
C     ------------------------------------------------------------------
      ALLOCATE(NWELLS,MXWELL,NWELVL,IWELCB,IPRWEL)
      ALLOCATE(NPWEL,IWELPB,NNPWEL,PSIRAMP,IUNITRAMP)
      ALLOCATE(NUMTAB,MAXVAL)
      PSIRAMP = 1.0E-1
      NUMTAB = 0
      MAXVAL = 1
      CLOSEZERO = 1.0E-7
C
C1------IDENTIFY PACKAGE AND INITIALIZE NWELLS.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'WEL -- WELL PACKAGE FOR NWT VERSION 1.1.4, ',
     1' 4/01/2018 INPUT READ FROM UNIT ',I4)
      NWELLS=0
      NNPWEL=0
      IUNITRAMP=IOUT
C
C2------READ MAXIMUM NUMBER OF WELLS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(In, IOUT, line)
      CALL UPARLSTAL(IN,IOUT,LINE,NPWEL,MXPW)
C
C
C2A------CHECK FOR KEYWORDS.  
C
      CALL PARSEWELLOPTIONS(In, Iout, LINE, IUNITNWT)
C
! ALLOCATE VARS FOR TIME SERIES WELL RATES
      NUMTABHOLD = NUMTAB
      IF ( NUMTABHOLD.EQ.0 ) NUMTABHOLD = 1
      ALLOCATE(TABTIME(MAXVAL,NUMTABHOLD),TABRATE(MAXVAL,NUMTABHOLD))
      ALLOCATE(TABLAY(NUMTABHOLD),TABROW(NUMTABHOLD),TABCOL(NUMTABHOLD))
      ALLOCATE(TABVAL(NUMTABHOLD))
      TABTIME = 0.0
      TABRATE = 0.0
      TABLAY = 0
      TABROW = 0
      TABCOL = 0
      TABVAL = 0
!        
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTW,IWELCB
         LLOC=21
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTW,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IWELCB,R,IOUT,IN)
      END IF
      i = 0
      WRITE(IOUT,3) MXACTW
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE WELLS AT ONE TIME')
      IF(IWELCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(IWELCB.GT.0) WRITE(IOUT,8) IWELCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
C
C3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(WELAUX(20))
      NAUX=0
      IPRWEL=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            WELAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) WELAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY WELL VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF WELL CELLS WILL NOT BE PRINTED')
         IPRWEL = 0
         GO TO 10
      END IF
! Check keyword for specifying PSI (NWT). This is to support previous versions of NWT
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPP,MXVL)
      LLOC=1
      CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'SPECIFY') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
         IF ( Iunitnwt.EQ.0 ) THEN
             write(IOUT,32)
         ELSE
           IF(PSIRAMP.LT.1.0E-1) PSIRAMP=1.0E-1
           IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
           WRITE(IOUT,*)
           WRITE(IOUT,9) PSIRAMP,IUNITRAMP
         END IF
      ELSE
         BACKSPACE IN
         IF ( IUNITNWT.GT.0 .AND. PSIRAMP.LT.CLOSEZERO )THEN
           IUNITRAMP = IOUT
      WRITE(IOUT,*)' PHIRAMP WILL BE SET TO A DEFAULT VALUE OF 1.0E-5'
      WRITE(IOUT,*) ' WELLS WITH REDUCED PUMPING WILL BE '
     +                      ,'REPORTED TO THE MAIN LISTING FILE'
         END IF
      END IF
    9 FORMAT(1X,'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
     +       ' FALLS WITHIN THE INTERVAL PHIRAMP TIMES THE CELL '/
     +       ' THICKNESS. THE VALUE SPECIFIED FOR PHIRAMP IS ',E12.5,/
     +       ' WELLS WITH REDUCED PUMPING WILL BE '
     +       'REPORTED TO FILE UNIT NUMBER',I5)
   32 FORMAT(1X,' Option to reduce pumping during cell ',
     +          'dewatering is activated and NWT solver ',I10,
     +          ' is not being used. Option deactivated')
C
C3A-----THERE ARE FOUR INPUT VALUES PLUS ONE LOCATION FOR
C3A-----CELL-BY-CELL FLOW.
      NWELVL=5+NAUX
C
C4------ALLOCATE SPACE FOR THE WELL DATA.
      IWELPB=MXACTW+1
      MXWELL=MXACTW+MXPW
      IF(MXACTW.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'Deactivating the Well Package because MXACTW=0')
         IN=0
      END IF
      ALLOCATE (WELL(NWELVL,MXWELL))
C
C5------READ NAMED PARAMETERS.
      WRITE(IOUT,18) NPWEL
   18 FORMAT(1X,//1X,I5,' Well parameters')
      IF(NPWEL.GT.0) THEN
        LSTSUM=IWELPB
        DO 120 K=1,NPWEL
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'WEL','Q',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
C5A-----READ PARAMETER WITHOUT INSTANCES.
            CALL ULSTRD(NLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
          ELSE
C5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRWEL)
            CALL ULSTRD(NINLST,WELL,LSTBEG,NWELVL,MXWELL,1,IN,
     &        IOUT,'WELL NO.  LAYER   ROW   COL   STRESS FACTOR',
     &        WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
          END IF
C
C6------RETURN
      CALL SGWF2WEL7PSV(IGRID)
      RETURN
      END SUBROUTINE
C
      SUBROUTINE PARSEWELLOPTIONS(IN,IOUT,line,IUNITNWT)
C     ******************************************************************
C     READ WELL DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IUNIT
      USE GWFWELMODULE
      USE GWFSFRMODULE, ONLY: NSS
C
      INTEGER, INTENT(IN) :: IN,IOUT,IUNITNWT
      character(len=200), intent(INOUT) :: line
C     ------------------------------------------------------------------
C     LOCAL VARIABLES
C     ------------------------------------------------------------------
      INTEGER intchk, Iostat, LLOC,ISTART,ISTOP,I,IHEADER
      logical :: found,option
      real :: R
      character(len=16)  :: text
C     ------------------------------------------------------------------
C
      LLOC=1
      found = .false.
      option = .false.
      text = 'WELL '
        DO
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,I,R,IOUT,IN)
        select case (LINE(ISTART:ISTOP))
        case('OPTIONS')
            write(iout,'(/1x,a)') 'PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
            found = .true.
            option = .true.
! REDUCING PUMPING FOR DRY CELLS OLD STYLE
        case('SPECIFY')
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
          IF ( IUNITNWT.EQ.0 ) THEN
            write(IOUT,32)
          ELSE
            IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
            IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
            WRITE(IOUT,*)
            WRITE(IOUT,9) PSIRAMP,IUNITRAMP
          END IF
          found = .true.
! REDUCING PUMPING FOR DRY CELLS
        case('PHIRAMP')
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,PSIRAMP,IOUT,IN)
          CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IUNITRAMP,R,IOUT,IN)
          IF ( IUNITNWT.EQ.0 ) THEN
            write(IOUT,32)
          ELSE
            IF(PSIRAMP.LT.1.0E-5) PSIRAMP=1.0E-5
            IF ( IUNITRAMP.EQ.0 ) IUNITRAMP = IOUT
            WRITE(IOUT,*)
            WRITE(IOUT,9) PSIRAMP,IUNITRAMP
          END IF
          found = .true.
! SPEICYING PUMPING RATES AS TIMES SERIES INPUT FILE FOR EACH WELL
        case('TABFILES')
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,NUMTAB,R,IOUT,IN)
            IF(NUMTAB.LT.0) NUMTAB=0
            WRITE(IOUT,30) NUMTAB
            CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MAXVAL,R,IOUT,IN)
            IF(MAXVAL.LT.0) THEN
                MAXVAL=1
                NUMTAB=0
            END IF
            WRITE(IOUT,31) MAXVAL
            found = .true.
        case ('END')
         write(iout,'(/1x,a)') 'END PROCESSING '//
     +            trim(adjustl(text)) //' OPTIONS'
            CALL URDCOM(In, IOUT, line)
            found = .true.
            exit
          case default
            read(line(istart:istop),*,IOSTAT=Iostat) intchk
            if ( option ) then
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))

            elseif( Iostat .ne. 0 ) then
              ! Not an integer.  Likely misspelled or unsupported 
              ! so terminate here.
              WRITE(IOUT,*) 'Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP)
              CALL USTOP('Invalid '//trim(adjustl(text))
     +                   //' Option: '//LINE(ISTART:ISTOP))
            else
              exit
            endif
        end select
        if ( found ) CALL URDCOM(In, IOUT, line)
      ENDDO
    9 FORMAT(1X,'NEGATIVE PUMPING RATES WILL BE REDUCED IF HEAD '/
     +       ' FALLS WITHIN THE INTERVAL PHIRAMP TIMES THE CELL '/
     +       ' THICKNESS. THE VALUE SPECIFIED FOR PHIRAMP IS ',E12.5,/
     +       ' WELLS WITH REDUCED PUMPING WILL BE '
     +       'REPORTED TO FILE UNIT NUMBER',I5)
   30 FORMAT(1X,' Pumping rates will be read from time ',
     +                 'series input files. ',I10,' files will be read')
   31 FORMAT(1X,' Pumping rates will be read from time ',
     +                 'series input files. A maximum of ',I10,
     +                 ' row entries will be read per file')
   32 FORMAT(1X,' Option to reduce pumping during cell ',
     +                 'dewatering is activated and NWT solver ',I10,
     +                 ' is not being used. Option deactivated')
      END SUBROUTINE     
C
C
      SUBROUTINE GWF2WEL7RP(IN,KPER,IGRID)
C     ******************************************************************
C     READ WELL DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,IUNIT
      USE GWFWELMODULE, ONLY:NWELLS,MXWELL,NWELVL,IPRWEL,NPWEL,IWELPB,
     1                       NNPWEL,WELAUX,WELL,NUMTAB,MAXVAL,TABTIME,
     2                       TABRATE,TABVAL,TABLAY,TABROW,TABCOL
C
      CHARACTER*6 CWELL
      CHARACTER(LEN=200)::LINE
      INTEGER TABUNIT, I, NUMSUPSP
      REAL TTIME,TRATE
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
      TABUNIT = 0
      TTIME = 0.0
      TRATE = 0.0
C
C1----READ NUMBER OF WELLS (OR FLAG SAYING REUSE WELL DATA).
C1----AND NUMBER OF PARAMETERS
      IF ( KPER.EQ.1 .OR. NUMTAB.EQ.0 ) THEN
        IF(NPWEL.GT.0) THEN
          IF(IFREFM.EQ.0) THEN
             READ(IN,'(2I10)') ITMP,NP
          ELSE
            READ(IN,*) ITMP,NP
          END IF
        ELSE
           NP=0
           IF(IFREFM.EQ.0) THEN
              READ(IN,'(I10)') ITMP
           ELSE
              READ(IN,*) ITMP
           END IF
        END IF
      ELSE
        ITMP = -1
        NP = 0
      END IF
C
C------Calculate some constants.
      NAUX=NWELVL-5
      IOUTU = IOUT
      IF (IPRWEL.EQ.0) IOUTU=-IOUTU
C
C1A-----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C1A-----IF ITMP=>0, SET NUMBER OF NON-PARAMETER WELLS EQUAL TO ITMP.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING NON-PARAMETER WELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPWEL=ITMP
      END IF
C
C1B-----IF THERE ARE NEW NON-PARAMETER WELLS, READ THEM.
      MXACTW=IWELPB-1
      IF(ITMP.GT.0) THEN
         IF(NNPWEL.GT.MXACTW) THEN
            WRITE(IOUT,99) NNPWEL,MXACTW
   99       FORMAT(1X,/1X,'THE NUMBER OF ACTIVE WELLS (',I6,
     1                     ') IS GREATER THAN MXACTW(',I6,')')
            CALL USTOP(' ')
         END IF
         IF ( NUMTAB.EQ.0 ) THEN
           CALL ULSTRD(NNPWEL,WELL,1,NWELVL,MXWELL,1,IN,IOUT,
     1            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     2             WELAUX,20,NAUX,IFREFM,NCOL,NROW,NLAY,4,4,IPRWEL)
         ELSEIF ( KPER.EQ.1 ) THEN
           DO J = 1, NUMTAB
             READ(IN,*)TABUNIT,TABVAL(J),TABLAY(J),TABROW(J),TABCOL(J)
             IF ( TABUNIT.LE.0 ) THEN
                 WRITE(IOUT,100)
                 CALL USTOP('')
             END IF
             REWIND(TABUNIT)   !IN CASE FILES ARE REUSED FOR MULTIPLE WELLS
             DO II = 1, TABVAL(J)
              LLOC = 1
              CALL URDCOM(TABUNIT,IOUT,LINE)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TTIME,IOUT,TABUNIT)
              CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TRATE,IOUT,TABUNIT)
              TABTIME(II,J) = TTIME
              TABRATE(II,J) = TRATE
             END DO
           END DO
         END IF
      END IF
      NWELLS=NNPWEL        
C
C1C-----IF THERE ARE ACTIVE WELL PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('Q')
      NREAD=NWELVL-1
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'WEL',IOUTU,'Q',WELL,NWELVL,MXWELL,NREAD,
     1                MXACTW,NWELLS,4,4,
     2            'WELL NO.  LAYER   ROW   COL   STRESS RATE',
     3            WELAUX,20,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF WELLS IN CURRENT STRESS PERIOD.
      CWELL=' WELLS'
      IF(NWELLS.EQ.1) CWELL=' WELL '
      WRITE(IOUT,101) NWELLS,CWELL
  101 FORMAT(1X,/1X,I6,A)
  100 FORMAT(1X,/1X,'****MODEL STOPPING**** ',
     +       'UNIT NUMBER FOR TABULAR INPUT FILE SPECIFIED AS ZERO.')
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GWF2WEL7FM(Iunitnwt, IGRID)
C     ******************************************************************
C     SUBTRACT Q FROM RHS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,       ONLY:IBOUND,RHS,HCOF,LBOTM,BOTM,HNEW,IOUT,DELR,
     1                       DELC
      USE GWFWELMODULE, ONLY:NWELLS,WELL,PSIRAMP,TABROW,TABCOL,TABLAY, 
     1                       NUMTAB,NWELVL
      USE GWFNWTMODULE, ONLY: A, IA, Heps, Icell
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
      USE GWFBASMODULE, ONLY: TOTIM
!External function interface
      INTERFACE 
      FUNCTION SMOOTH3(H,T,B,dQ)
      DOUBLE PRECISION SMOOTH3
      DOUBLE PRECISION, INTENT(IN) :: H
      DOUBLE PRECISION, INTENT(IN) :: T
      DOUBLE PRECISION, INTENT(IN) :: B
      DOUBLE PRECISION, INTENT(OUT) :: dQ
      END FUNCTION SMOOTH3
      END INTERFACE
!External function interface
      INTERFACE 
      FUNCTION RATETERP(TIME,L)
      REAL RATETERP
      REAL, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: L
      END FUNCTION RATETERP
      END INTERFACE
!
      DOUBLE PRECISION Qp,Hh,Ttop,Bbot,dQp,FMIN
      INTEGER Iunitnwt, NWELLSTEMP
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
      ZERO=0.0D0
      Qp = 0.0
      NWELLSTEMP = NWELLS
      TIME = TOTIM
      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
C
C1------IF NUMBER OF WELLS <= 0 THEN RETURN.
      IF(NWELLS.LE.0) RETURN
C
C2------PROCESS EACH WELL IN THE WELL LIST.
      DO 100 L=1,NWELLSTEMP
      IF ( NUMTAB.LE.0 ) THEN
        IR=WELL(2,L)
        IC=WELL(3,L)
        IL=WELL(1,L)
        Q=WELL(4,L)
      ELSE
        IR = TABROW(L)
        IC = TABCOL(L)
        IL = TABLAY(L)
        Q = RATETERP(TIME,L)
      END IF
C
C2A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 100
C
C2B-----IF THE CELL IS VARIABLE HEAD THEN SUBTRACT Q FROM
C       THE RHS ACCUMULATOR.
      IF ( Q .LT. ZERO .AND. IUNITNWT.NE.0 ) THEN
        IF ( LAYTYPUPW(il).GT.0 ) THEN
          Hh = HNEW(ic,ir,il)
          bbot = Botm(IC, IR, Lbotm(IL))
          ttop = Botm(IC, IR, Lbotm(IL)-1)
          Qp = Q*smooth3(Hh,Ttop,Bbot,dQp)
          RHS(IC,IR,IL)=RHS(IC,IR,IL)-Qp
! Derivative for RHS
          ij = Icell(IC,IR,IL)
          A(IA(ij)) = A(IA(ij)) + dQp*Q
        ELSE
          RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
          Qp = Q
        END IF
      ELSE
        RHS(IC,IR,IL)=RHS(IC,IR,IL)-Q
        Qp = Q
      END IF
  100 CONTINUE
C
C3------RETURN
      RETURN
      END
      SUBROUTINE GWF2WEL7BD(KSTP,KPER,Iunitnwt,IGRID)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR WELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,BOTM,LBOTM,
     1                      HNEW,DELR,DELC
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GWFWELMODULE,ONLY:NWELLS,IWELCB,WELL,NWELVL,WELAUX,PSIRAMP,
     1                      IUNITRAMP,IPRWEL,TABROW,TABCOL,TABLAY, 
     2                      NUMTAB
      USE GWFUPWMODULE, ONLY: LAYTYPUPW
!External function interface
      INTERFACE 
        FUNCTION SMOOTH3(H,T,B,dQ)
        DOUBLE PRECISION SMOOTH3
        DOUBLE PRECISION, INTENT(IN) :: H
        DOUBLE PRECISION, INTENT(IN) :: T
        DOUBLE PRECISION, INTENT(IN) :: B
        DOUBLE PRECISION, INTENT(OUT) :: dQ
        END FUNCTION SMOOTH3
      END INTERFACE
!External function interface
      INTERFACE 
      FUNCTION RATETERP(TIME,L)
      REAL RATETERP
      REAL, INTENT(IN) :: TIME
      INTEGER, INTENT(IN) :: L
      END FUNCTION RATETERP
      END INTERFACE
      CHARACTER*16 TEXT
      DOUBLE PRECISION RATIN,RATOUT,QQ,QSAVE,FMIN
      double precision Qp,Hh,Ttop,Bbot,dQp
      real Q
      INTEGER Iunitnwt, iw1, NWELLSTEMP
      DATA TEXT /'           WELLS'/
C     ------------------------------------------------------------------
      CALL SGWF2WEL7PNT(IGRID)
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      TIME = TOTIM
      Qp = 1.0
      NWELLSTEMP = NWELLS
      IF ( NUMTAB.GT.0 ) NWELLSTEMP = NUMTAB
      
      IF(IWELCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(IWELCB.GT.0) IBD=ICBCFL
      IBDLBL=0
      iw1 = 1
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NWELVL-5
         IF(IAUXSV.EQ.0) NAUX=0
         CALL UBDSV4(KSTP,KPER,TEXT,NAUX,WELAUX,IWELCB,NCOL,NROW,NLAY,
     1          NWELLS,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 IL=1,NLAY
      DO 50 IR=1,NROW
      DO 50 IC=1,NCOL
      BUFF(IC,IR,IL)=ZERO
50    CONTINUE
C
C4------IF THERE ARE NO WELLS, DO NOT ACCUMULATE FLOW.
      IF(NWELLS.EQ.0) GO TO 200
C
C5------LOOP THROUGH EACH WELL CALCULATING FLOW.
      DO 100 L=1,NWELLSTEMP
        Q=ZERO
        QQ=ZERO
        IF ( NUMTAB.LE.0 ) THEN
C
C5A-----GET LAYER, ROW & COLUMN OF CELL CONTAINING WELL.
          IR=WELL(2,L)
          IC=WELL(3,L)
          IL=WELL(1,L)
C
C5C-----GET FLOW RATE FROM WELL LIST.
          QSAVE=WELL(4,L)
        ELSE
          IR = TABROW(L)
          IC = TABCOL(L)
          IL = TABLAY(L)
          QSAVE = RATETERP(TIME,L)
        END IF
C
      bbot = Botm(IC, IR, Lbotm(IL))
      ttop = Botm(IC, IR, Lbotm(IL)-1)
      Hh = HNEW(ic,ir,il)
C
C5B-----IF THE CELL IS NO-FLOW OR CONSTANT HEAD, IGNORE IT.
C-------CHECK IF PUMPING IS NEGATIVE AND REDUCE FOR DRYING CONDITIONS.
C
      IF(IBOUND(IC,IR,IL).LE.0)GO TO 99
      IF ( Qsave.LT.zero  .AND. Iunitnwt.NE.0) THEN
        IF ( LAYTYPUPW(il).GT.0 ) THEN
          Qp = smooth3(Hh,Ttop,Bbot,dQp)
          Q = Qsave*Qp
        ELSE
          Q = Qsave
        END IF
      ELSE
        Q = Qsave
      END IF
      QQ=Q
C
! write wells with reduced pumping
      IF ( Qp.LT.0.9999D0 .AND. Iunitnwt.NE.0 .AND. 
     +     IPRWEL.NE.0 .and. Qsave < ZERO ) THEN
          IF ( iw1.EQ.1 ) THEN
            WRITE(IUNITRAMP,*)
            WRITE(IUNITRAMP,300)KPER,KSTP
            WRITE(IUNITRAMP,400)
          END IF
          WRITE(IUNITRAMP,500)IL,IR,IC,QSAVE,Q,hh,bbot
          iw1 = iw1 + 1 
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+QQ
C
C5F-----SEE IF FLOW IS POSITIVE OR NEGATIVE.
      IF(QQ.GE.ZERO) THEN
C
C5G-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
        RATIN=RATIN+QQ
      ELSE
C
C5H-----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
        RATOUT=RATOUT-QQ
      END IF
C
C5I-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5I-----COPY FLOW TO WELL LIST.
   99 IF(IBD.EQ.2) CALL UBDSVB(IWELCB,NCOL,NROW,IC,IR,IL,Q,
     1                  WELL(:,L),NWELVL,NAUX,5,IBOUND,NLAY)
      WELL(NWELVL,L)=QQ
!
      
   61 FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
   62 FORMAT(1X,'WELL ',I6,'   LAYER ',I3,'   ROW ',I5,'   COL ',I5,
     1      '   RATE ',1PG15.6)
  300 FORMAT(' WELLS WITH REDUCED PUMPING FOR STRESS PERIOD ',I5,
     1      ' TIME STEP ',I5)
  400 FORMAT('   LAY   ROW   COL         APPL.Q          ACT.Q',
     1       '        GW-HEAD       CELL-BOT')
  500 FORMAT(3I6,4E15.6)

  100       CONTINUE
!
C5D-----PRINT FLOW RATE IF REQUESTED.
       IF(IBDLBL.EQ.0.AND.IBD.LT.0) WRITE(IOUT,61) TEXT,KPER,KSTP
       DO L=1,NWELLSTEMP
         IF ( NUMTAB.LE.0 ) THEN
           IR=WELL(2,L)
           IC=WELL(3,L)
           IL=WELL(1,L)
         ELSE
           IR = TABROW(L)
           IC = TABCOL(L)
           IL = TABLAY(L)  
         END IF
         IF(IBD.LT.0) THEN
              WRITE(IOUT,62) L,IL,IR,IC,WELL(NWELVL,L)
         END IF
       END DO
C
      IF (iw1.GT.1 )WRITE(IUNITRAMP,*)
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,IWELCB,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C9------RETURN
      RETURN
      END
C
C     ------------------------------------------------------------------
C
      REAL FUNCTION RATETERP (TIME,INUM)
C     FUNCTION LINEARLY INTERPOLATES BETWEEN TWO VALUES
C     OF TIME TO CACULATE SPECIFIED PUMPING RATES.
      USE GWFWELMODULE, ONLY: TABRATE, TABTIME, TABVAL
      USE GWFBASMODULE, ONLY: DELT
      IMPLICIT NONE
!ARGUMENTS
      INTEGER, INTENT(IN):: INUM
      REAL, INTENT(IN):: TIME
!
      REAL CLOSEZERO
      REAL FLOW, TIMEBEG, TIMEND, TIMESTART, SUMFLOW, TOLF2
      INTEGER IEND, ISTM1, ISTART, iflg, NVAL, I
      TOLF2=1.0E-4
      CLOSEZERO=1.0E-15
      FLOW = 0.0
      NVAL = TABVAL(INUM)
      IFLG = 0
      SUMFLOW = 0.0
      I = 1
      TIMEBEG = TIME - DELT
      IF ( TIMEBEG-TABTIME(1,INUM).LT.0.0 ) THEN
        RATETERP = TABRATE(1,INUM)
      ELSEIF ( TIMEBEG-TABTIME(NVAL,INUM).GE.0.0 ) THEN
        RATETERP = TABRATE(NVAL,INUM)
      ELSE
! Find table value before beginning of time step.
        DO WHILE ( I.LE.NVAL-1 )
          IF ( TIMEBEG-TABTIME(I,INUM).LE.CLOSEZERO ) THEN
            EXIT
          ELSEIF ( TIMEBEG-TABTIME(I+1,INUM).LE.CLOSEZERO ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        ISTART = I
        ISTM1 = I
        IF ( I.GT.1 ) ISTM1 = ISTM1 - 1
! Find table value after end of time step
        DO WHILE ( I.LE.NVAL ) 
          IF ( TIME-TABTIME(I,INUM).LE.0.0 ) THEN
            EXIT
          ELSE
            I = I + 1
          END IF
        END DO
        IEND = I
        IF ( IEND.GT.NVAL ) IEND = NVAL
        DO I = ISTART, IEND - 1
          TIMESTART = TABTIME(I,INUM)
          TIMEND = TABTIME(I+1,INUM)
          IF ( TIMEBEG-TIMESTART.GT.0.0 ) TIMESTART = TIMEBEG
          IF ( TIME-TIMEND.LT.0.0 ) TIMEND = TIME
          SUMFLOW = SUMFLOW + (TIMEND-TIMESTART)*TABRATE(I,INUM)
        END DO
        RATETERP = SUMFLOW/DELT
      END IF
      RETURN
      END FUNCTION RATETERP
C
      DOUBLE PRECISION FUNCTION smooth3(H,T,B,dQ)
C     ******************************************************************
C     SMOOTHLY REDUCES PUMPING TO ZERO FOR DEWATERED CONDITIONS
C     ******************************************************************
! h is the depth 
      USE GWFWELMODULE,ONLY:PSIRAMP
      IMPLICIT NONE
      DOUBLE PRECISION s, aa, bb, x
      DOUBLE PRECISION cof1, cof2, cof3, Qp
      DOUBLE PRECISION, INTENT(IN) :: H
      DOUBLE PRECISION, INTENT(IN) :: T
      DOUBLE PRECISION, INTENT(IN) :: B
      DOUBLE PRECISION, INTENT(OUT) :: dQ
      smooth3 = 0.0D0
      s = PSIRAMP
      s = s*(T-B)   ! puming rate begins to be ramped down.
      x = (H-B)
      aa = -6.0d0/(s**3.0d0)
      bb = -6.0d0/(s**2.0d0)
      cof1 = x**2.0D0
      cof2 = -(2.0D0*x)/(s**3.0D0)
      cof3 = 3.0D0/(s**2.0D0)
      Qp = cof1*(cof2+cof3)
      dQ = (aa*x**2.0D0-bb*x)
      IF ( x.LT.0.0D0 ) THEN
        Qp = 0.0D0
        dQ = 0.0D0
      ELSEIF ( x-s.GT.-1.0e-14 ) THEN
        Qp = 1.0D0
        dQ = 0.0D0
      END IF
      smooth3 = Qp
      END FUNCTION smooth3
C
      SUBROUTINE GWF2WEL7DA(IGRID)
C  Deallocate WEL MEMORY
      USE GWFWELMODULE
C
        CALL SGWF2WEL7PNT(IGRID)
        DEALLOCATE(PSIRAMP) 
        DEALLOCATE(IUNITRAMP) 
        DEALLOCATE(NWELLS)
        DEALLOCATE(MXWELL)
        DEALLOCATE(NWELVL)
        DEALLOCATE(IWELCB)
        DEALLOCATE(IPRWEL)
        DEALLOCATE(NPWEL)
        DEALLOCATE(IWELPB)
        DEALLOCATE(NNPWEL)
        DEALLOCATE(WELAUX)
        DEALLOCATE(WELL)
        DEALLOCATE(NUMTAB)
        DEALLOCATE(MAXVAL)
        DEALLOCATE(TABTIME)
        DEALLOCATE(TABRATE)
        DEALLOCATE(TABVAL)
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PNT(IGRID)
C  Change WEL data to a different grid.
      USE GWFWELMODULE
C
        PSIRAMP=>GWFWELDAT(IGRID)%PSIRAMP
        IUNITRAMP=>GWFWELDAT(IGRID)%IUNITRAMP 
        NWELLS=>GWFWELDAT(IGRID)%NWELLS
        MXWELL=>GWFWELDAT(IGRID)%MXWELL
        NWELVL=>GWFWELDAT(IGRID)%NWELVL
        IWELCB=>GWFWELDAT(IGRID)%IWELCB
        IPRWEL=>GWFWELDAT(IGRID)%IPRWEL
        NPWEL=>GWFWELDAT(IGRID)%NPWEL
        IWELPB=>GWFWELDAT(IGRID)%IWELPB
        NNPWEL=>GWFWELDAT(IGRID)%NNPWEL
        WELAUX=>GWFWELDAT(IGRID)%WELAUX
        WELL=>GWFWELDAT(IGRID)%WELL
        NUMTAB=>GWFWELDAT(IGRID)%NUMTAB
        MAXVAL=>GWFWELDAT(IGRID)%MAXVAL
        TABTIME=>GWFWELDAT(IGRID)%TABTIME
        TABRATE=>GWFWELDAT(IGRID)%TABRATE
        TABVAL=>GWFWELDAT(IGRID)%TABVAL
C
      RETURN
      END
      SUBROUTINE SGWF2WEL7PSV(IGRID)
C  Save WEL data for a grid.
      USE GWFWELMODULE
C 
        GWFWELDAT(IGRID)%PSIRAMP=>PSIRAMP
        GWFWELDAT(IGRID)%IUNITRAMP=>IUNITRAMP
        GWFWELDAT(IGRID)%NWELLS=>NWELLS
        GWFWELDAT(IGRID)%MXWELL=>MXWELL
        GWFWELDAT(IGRID)%NWELVL=>NWELVL
        GWFWELDAT(IGRID)%IWELCB=>IWELCB
        GWFWELDAT(IGRID)%IPRWEL=>IPRWEL
        GWFWELDAT(IGRID)%NPWEL=>NPWEL
        GWFWELDAT(IGRID)%IWELPB=>IWELPB
        GWFWELDAT(IGRID)%NNPWEL=>NNPWEL
        GWFWELDAT(IGRID)%WELAUX=>WELAUX
        GWFWELDAT(IGRID)%WELL=>WELL
        GWFWELDAT(IGRID)%NUMTAB=>NUMTAB
        GWFWELDAT(IGRID)%MAXVAL=>MAXVAL
        GWFWELDAT(IGRID)%TABTIME=>TABTIME
        GWFWELDAT(IGRID)%TABRATE=>TABRATE
        GWFWELDAT(IGRID)%TABVAL=>TABVAL
C
      RETURN
      END

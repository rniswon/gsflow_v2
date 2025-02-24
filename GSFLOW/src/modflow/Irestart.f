
      SUBROUTINE RESTART1WRITE()
C     ******************************************************************
C      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IOUT, IUNIT
      EXTERNAL :: WRITE_HEADS, WRITE_UZF, WRITE_SFR, WRITE_LAK, 
     &            WRITE_MNW2
C
!     CHARACTER*200 LINE
C     ------------------------------------------------------------------
C
C
C2------WRITE DATA FOR RESTART OPTION
        IF ( IUNIT(68)>0 ) THEN
          CALL WRITE_HEADS(IUNIT(68))
          IF ( IUNIT(55)>0 ) CALL WRITE_UZF(IOUT,IUNIT(68))
          IF ( IUNIT(44)>0 ) CALL WRITE_SFR(IOUT,IUNIT(68))
          IF ( IUNIT(22)>0 ) CALL WRITE_LAK(IOUT,IUNIT(68))
          IF ( IUNIT(50)>0 ) CALL WRITE_MNW2(IOUT,IUNIT(68))
        END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE RESTART1READ()
C     ******************************************************************
C     
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IOUT, IUNIT
      EXTERNAL :: READ_HEADS, READ_UZF, READ_SFR, READ_LAK, READ_MNW2
C     ------------------------------------------------------------------
C
C7C2----CHECK FOR RESTART
      IF(IUNIT(69).GT.0) THEN
        CALL READ_HEADS(IOUT,IUNIT(69))
        IF ( IUNIT(55)>0 ) CALL READ_UZF(IOUT,IUNIT(69))
        IF ( IUNIT(44)>0 ) CALL READ_SFR(IOUT,IUNIT(69))
        IF ( IUNIT(22)>0 ) CALL READ_LAK(IOUT,IUNIT(69))
        IF ( IUNIT(50)>0 ) CALL READ_MNW2(IOUT,IUNIT(69))
      ENDIF
      END
C
      SUBROUTINE WRITE_UZF(IOUT,IURESTARTUZF)
      USE GWFUZFMODULE, ONLY: UZDPST, UZFLST, UZSPST, UZTHST, UZSTOR,
     +                        UZOLSFLX, NWAVST, ITRLST, LTRLST, HLDUZF,
     +                        IUZHOLD, HLDUZF, PETRATE, SEEPOUT, FINF, 
     +                        LAYNUM
      IMPLICIT NONE
      INTEGER IOUT, IURESTARTUZF
      CHARACTER*30 ANAME
         DATA ANAME  /'  WRITING DATA FOR RESTART UZF'/
         WRITE(IOUT,15) IURESTARTUZF,ANAME
   15    FORMAT(1X,/1X,'WRITING FILE ON UNIT ',I4,':',/1X,A)
         WRITE(IURESTARTUZF) UZSTOR
         WRITE(IURESTARTUZF) UZOLSFLX
         WRITE(IURESTARTUZF) NWAVST
         WRITE(IURESTARTUZF) IUZHOLD
         WRITE(IURESTARTUZF) UZDPST
         WRITE(IURESTARTUZF) UZFLST
         WRITE(IURESTARTUZF) UZSPST
         WRITE(IURESTARTUZF) UZTHST
         WRITE(IURESTARTUZF) ITRLST
         WRITE(IURESTARTUZF) LTRLST
         WRITE(IURESTARTUZF) HLDUZF
         WRITE(IURESTARTUZF) PETRATE
         WRITE(IURESTARTUZF) SEEPOUT
         WRITE(IURESTARTUZF) FINF
         WRITE(IURESTARTUZF) LAYNUM
      END SUBROUTINE WRITE_UZF
C
C
      SUBROUTINE READ_UZF(IOUT,IURESTARTUZF)
      USE GWFUZFMODULE, ONLY: UZDPST, UZFLST, UZSPST, UZTHST, UZSTOR,
     +                        UZOLSFLX, NWAVST, ITRLST, LTRLST, HLDUZF,
     +                        IUZHOLD, HLDUZF, PETRATE, SEEPOUT, FINF, 
     +                        LAYNUM, UZFRESTART
      IMPLICIT NONE
      INTEGER IOUT, IURESTARTUZF
      CHARACTER*30 ANAME
         DATA ANAME /'  READING DATA FOR RESTART UZF'/
         UZFRESTART = 1
         WRITE(IOUT,15) IURESTARTUZF,ANAME
   15    FORMAT(1X,/1X,'READING FILE ON UNIT ',I4,':',/1X,A)
         READ(IURESTARTUZF) UZSTOR
         READ(IURESTARTUZF) UZOLSFLX
         READ(IURESTARTUZF) NWAVST
         READ(IURESTARTUZF) IUZHOLD
         READ(IURESTARTUZF) UZDPST
         READ(IURESTARTUZF) UZFLST
         READ(IURESTARTUZF) UZSPST
         READ(IURESTARTUZF) UZTHST
         READ(IURESTARTUZF) ITRLST
         READ(IURESTARTUZF) LTRLST
         READ(IURESTARTUZF) HLDUZF
         READ(IURESTARTUZF) PETRATE
         READ(IURESTARTUZF) SEEPOUT
         READ(IURESTARTUZF) FINF
         READ(IURESTARTUZF) LAYNUM
      END SUBROUTINE READ_UZF
C
       SUBROUTINE WRITE_SFR(IOUT,IURESTARTSFR)
      USE GWFSFRMODULE, ONLY: UZDPST, UZFLST, UZSPST, UZTHST, UZSTOR,
     +                        UZOLSFLX, NWAVST, ITRLST, LTRLST,
     +                        UZSEEP, HLDSFR, STRM, WETPER, ISTRM, SEG,
     +                        ISEG, DLKOTFLW, DLKSTAGE
!      USE GWFGAGMODULE, ONLY: IGGLST
      USE GLOBAL,     ONLY: IUNIT
      IMPLICIT NONE
      INTEGER IOUT,IURESTARTSFR
      CHARACTER*30 ANAME
      DATA ANAME /'  WRITING DATA FOR RESTART SFR'/
         WRITE(IOUT,15) IURESTARTSFR, ANAME
   15    FORMAT(1X,/1X,'WRITING FILE ON UNIT ',I4,':',/1X,A)
         WRITE(IURESTARTSFR) UZSTOR
         WRITE(IURESTARTSFR) NWAVST
         WRITE(IURESTARTSFR) UZDPST
         WRITE(IURESTARTSFR) UZFLST
         WRITE(IURESTARTSFR) UZSPST
         WRITE(IURESTARTSFR) UZTHST
         WRITE(IURESTARTSFR) ITRLST
         WRITE(IURESTARTSFR) LTRLST
         WRITE(IURESTARTSFR) UZOLSFLX
         WRITE(IURESTARTSFR) UZSEEP
         WRITE(IURESTARTSFR) HLDSFR
         WRITE(IURESTARTSFR) STRM
         WRITE(IURESTARTSFR) WETPER
         WRITE(IURESTARTSFR) ISTRM
         WRITE(IURESTARTSFR) SEG
         WRITE(IURESTARTSFR) ISEG
!         IF ( IUNIT(46)>0 ) WRITE(IURESTARTSFR) IGGLST
         IF ( IUNIT(22)>0 ) THEN
           WRITE(IURESTARTSFR) DLKOTFLW
           WRITE(IURESTARTSFR) DLKSTAGE
         ENDIF
       END SUBROUTINE WRITE_SFR
C
      SUBROUTINE READ_SFR(IOUT,IURESTARTSFR)
      USE GWFSFRMODULE, ONLY: UZDPST, UZFLST, UZSPST, UZTHST, UZSTOR,
     +                        UZOLSFLX, NWAVST, ITRLST, LTRLST,
     +                        UZSEEP, HLDSFR, STRM, WETPER, ISTRM, SEG,
     +                        ISEG, DLKOTFLW, DLKSTAGE
!      USE GWFGAGMODULE, ONLY: IGGLST
      USE GLOBAL,     ONLY: IUNIT
      IMPLICIT NONE
      INTEGER IOUT, IURESTARTSFR
      CHARACTER*30 ANAME
      DATA ANAME /'  READING DATA FOR RESTART SFR'/
         WRITE(IOUT,15) IURESTARTSFR,ANAME
   15    FORMAT(1X,/1X,'READING FILE ON UNIT ',I4,':',/1X,A)
         READ(IURESTARTSFR) UZSTOR
         READ(IURESTARTSFR) NWAVST
         READ(IURESTARTSFR) UZDPST
         READ(IURESTARTSFR) UZFLST
         READ(IURESTARTSFR) UZSPST
         READ(IURESTARTSFR) UZTHST
         READ(IURESTARTSFR) ITRLST
         READ(IURESTARTSFR) LTRLST
         READ(IURESTARTSFR) UZOLSFLX
         READ(IURESTARTSFR) UZSEEP
         READ(IURESTARTSFR) HLDSFR
         READ(IURESTARTSFR) STRM
         READ(IURESTARTSFR) WETPER
         READ(IURESTARTSFR) ISTRM
         READ(IURESTARTSFR) SEG
         READ(IURESTARTSFR) ISEG
!         IF ( IUNIT(46)>0 ) READ(IURESTARTSFR) IGGLST
         IF ( IUNIT(22)>0 ) THEN
           READ(IURESTARTSFR) DLKOTFLW
           READ(IURESTARTSFR) DLKSTAGE
         ENDIF
      END SUBROUTINE READ_SFR
C
       SUBROUTINE WRITE_LAK(IOUT,IURESTARTLAK)
      USE GWFLAKMODULE, ONLY: VOL, STGOLD, STGNEW, STGOLD2, VOLOLDD, 
     +                        VOLOLD, VOLINIT, STGITER, LAKSEEP, STAGES,
     +                        RNF, PRCPLK, BGAREA, OVRLNDRNF, SURFIN, 
     +                        THETA, NSSITR, EVAPLK, WTHDRW, 
     +                        FLWIN, ILAKE, CNDFCT, SURFA, SSMN, SSMX,
     +                        IDIV, VOLUMETABLE, AREATABLE, DEPTHTABLE,
     +                        NSSITR, ISTARTLAK
      IMPLICIT NONE
      INTEGER IOUT,IURESTARTLAK
      CHARACTER*30 ANAME
      DATA ANAME /'  WRITING DATA FOR RESTART LAK'/
         WRITE(IOUT,15) IURESTARTLAK, ANAME
   15    FORMAT(1X,/1X,'WRITING FILE ON UNIT ',I4,':',/1X,A)
         WRITE(IURESTARTLAK)VOL
         WRITE(IURESTARTLAK)STGOLD
         WRITE(IURESTARTLAK)STGNEW
         WRITE(IURESTARTLAK)STGOLD2
         WRITE(IURESTARTLAK)VOLOLDD
         WRITE(IURESTARTLAK)VOLOLD
         WRITE(IURESTARTLAK)VOLINIT
         WRITE(IURESTARTLAK)STGITER
         WRITE(IURESTARTLAK)LAKSEEP
         WRITE(IURESTARTLAK)STAGES
         WRITE(IURESTARTLAK)RNF
         WRITE(IURESTARTLAK)PRCPLK
         WRITE(IURESTARTLAK)BGAREA
         WRITE(IURESTARTLAK)OVRLNDRNF
         WRITE(IURESTARTLAK)SURFIN
         WRITE(IURESTARTLAK)THETA
         WRITE(IURESTARTLAK)NSSITR
         WRITE(IURESTARTLAK)EVAPLK
         WRITE(IURESTARTLAK)WTHDRW
         WRITE(IURESTARTLAK)FLWIN
         WRITE(IURESTARTLAK)ILAKE
         WRITE(IURESTARTLAK)CNDFCT
         WRITE(IURESTARTLAK)SURFA
         WRITE(IURESTARTLAK)SSMN
         WRITE(IURESTARTLAK)SSMX
         WRITE(IURESTARTLAK)IDIV
         WRITE(IURESTARTLAK)VOLUMETABLE
         WRITE(IURESTARTLAK)AREATABLE
         WRITE(IURESTARTLAK)DEPTHTABLE
         WRITE(IURESTARTLAK)ISTARTLAK
      END SUBROUTINE WRITE_LAK
C
C
      SUBROUTINE READ_LAK(IOUT,IURESTARTLAK)
      USE GWFLAKMODULE, ONLY: VOL, STGOLD, STGNEW, STGOLD2, VOLOLDD, 
     +                        VOLOLD, VOLINIT, STGITER, LAKSEEP, STAGES,
     +                        RNF, PRCPLK, BGAREA, OVRLNDRNF, SURFIN, 
     +                        THETA, NSSITR, EVAPLK, WTHDRW, 
     +                        FLWIN, ILAKE, CNDFCT, SURFA, SSMN, SSMX,
     +                        IDIV, VOLUMETABLE, AREATABLE, DEPTHTABLE,
     +                        ISTARTLAK
      IMPLICIT NONE
      INTEGER IOUT,IURESTARTLAK
      CHARACTER*30 ANAME
      DATA ANAME /'  READING DATA FOR RESTART LAK'/
         WRITE(IOUT,15) IURESTARTLAK, ANAME
   15    FORMAT(1X,/1X,'READING FILE ON UNIT ',I4,':',/1X,A)
         READ(IURESTARTLAK)VOL
         READ(IURESTARTLAK)STGOLD
         READ(IURESTARTLAK)STGNEW
         READ(IURESTARTLAK)STGOLD2
         READ(IURESTARTLAK)VOLOLDD
         READ(IURESTARTLAK)VOLOLD
         READ(IURESTARTLAK)VOLINIT
         READ(IURESTARTLAK)STGITER
         READ(IURESTARTLAK)LAKSEEP
         READ(IURESTARTLAK)STAGES
         READ(IURESTARTLAK)RNF
         READ(IURESTARTLAK)PRCPLK
         READ(IURESTARTLAK)BGAREA
         READ(IURESTARTLAK)OVRLNDRNF
         READ(IURESTARTLAK)SURFIN
         READ(IURESTARTLAK)THETA
         READ(IURESTARTLAK)NSSITR
         READ(IURESTARTLAK)EVAPLK
         READ(IURESTARTLAK)WTHDRW
         READ(IURESTARTLAK)FLWIN
         READ(IURESTARTLAK)ILAKE
         READ(IURESTARTLAK)CNDFCT
         READ(IURESTARTLAK)SURFA
         READ(IURESTARTLAK)SSMN
         READ(IURESTARTLAK)SSMX
         READ(IURESTARTLAK)IDIV
         READ(IURESTARTLAK)VOLUMETABLE
         READ(IURESTARTLAK)AREATABLE
         READ(IURESTARTLAK)DEPTHTABLE
         READ(IURESTARTLAK)ISTARTLAK
      END SUBROUTINE READ_LAK
C
      SUBROUTINE WRITE_MNW2(IOUT,IURESTARTMNW2)
      USE GWFMNW2MODULE, ONLY: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                        NODTOT,INTTOT,NTOTNOD,LIMQ,
     2                        SMALL,MNW2,MNWNOD,MNWINT,CapTable
     3                        !,MNWAUX,WELLID
      IMPLICIT NONE
      INTEGER IOUT,IURESTARTMNW2
      CHARACTER*30 ANAME
      DATA ANAME /' WRITING DATA FOR RESTART MNW2'/
         WRITE(IOUT,15) IURESTARTMNW2, ANAME
   15    FORMAT(1X,/1X,'WRITING FILE ON UNIT ',I4,':',/1X,A)
         WRITE(IURESTARTMNW2)NMNW2
         WRITE(IURESTARTMNW2)MNWMAX
         WRITE(IURESTARTMNW2)NMNWVL
         WRITE(IURESTARTMNW2)IWL2CB
         WRITE(IURESTARTMNW2)MNWPRNT
         WRITE(IURESTARTMNW2)NODTOT
         WRITE(IURESTARTMNW2)INTTOT
         WRITE(IURESTARTMNW2)NTOTNOD
         WRITE(IURESTARTMNW2)LIMQ
         WRITE(IURESTARTMNW2)SMALL
!         WRITE(IURESTARTMNW2)WELLID
!         WRITE(IURESTARTMNW2)MNWAUX
         WRITE(IURESTARTMNW2)MNW2
         WRITE(IURESTARTMNW2)MNWNOD
         WRITE(IURESTARTMNW2)MNWINT
         WRITE(IURESTARTMNW2)CapTable
      END SUBROUTINE WRITE_MNW2
C
      SUBROUTINE READ_MNW2(IOUT,IURESTARTMNW2)
      USE GWFMNW2MODULE, ONLY: NMNW2,MNWMAX,NMNWVL,IWL2CB,MNWPRNT,
     1                        NODTOT,INTTOT,NTOTNOD,LIMQ,
     2                        SMALL,MNW2,MNWNOD,MNWINT,CapTable
     4                        !,MNWAUX,WELLID
      IMPLICIT NONE
      INTEGER IOUT,IURESTARTMNW2
      CHARACTER*30 ANAME
      DATA ANAME /' READING DATA FOR RESTART MNW2'/
         WRITE(IOUT,15) IURESTARTMNW2, ANAME
   15    FORMAT(1X,/1X,'READING FILE ON UNIT ',I4,':',/1X,A)
         READ(IURESTARTMNW2)NMNW2
         READ(IURESTARTMNW2)MNWMAX
         READ(IURESTARTMNW2)NMNWVL
         READ(IURESTARTMNW2)IWL2CB
         READ(IURESTARTMNW2)MNWPRNT
         READ(IURESTARTMNW2)NODTOT
         READ(IURESTARTMNW2)INTTOT
         READ(IURESTARTMNW2)NTOTNOD
         READ(IURESTARTMNW2)LIMQ
         READ(IURESTARTMNW2)SMALL
!         READ(IURESTARTMNW2)WELLID
!         READ(IURESTARTMNW2)MNWAUX
         READ(IURESTARTMNW2)MNW2
         READ(IURESTARTMNW2)MNWNOD
         READ(IURESTARTMNW2)MNWINT
         READ(IURESTARTMNW2)CapTable
      END SUBROUTINE READ_MNW2
C
      SUBROUTINE READ_HEADS(IOUT,IURESTARTHEAD)
C     ******************************************************************
C     READ HEADS FOR RESTART AND COPY INTO HNEW
C     ******************************************************************
C
      USE GLOBAL,      ONLY:STRT,NCOL,NROW,NLAY,HNEW,IBOUND,
     +                      IXSEC,IUNIT
      USE GWFBASMODULE,ONLY:HNOFLO
      USE GWFNWTMODULE, ONLY:HITER
      EXTERNAL :: U2DREL_RST
      INTEGER, INTENT(IN) :: IOUT, IURESTARTHEAD
      DOUBLE PRECISION HNF
      INTEGER K, KK, I, J
      CHARACTER*30 ANAME
      DATA ANAME /'           RESTART HEADS READ '/
C        SPECIFICATIONS:
C     ------------------------------------------------------------------      
C
C8G-----READ INITIAL HEADS FOR RESTART.
      IF(IXSEC.EQ.0) THEN
         DO 300 K=1,NLAY
         KK=K
         CALL U2DREL_RST(STRT(:,:,KK),ANAME,NROW,NCOL,KK,IURESTARTHEAD,
     +                   IOUT)
  300    CONTINUE
      ELSE
         CALL U2DREL_RST(STRT(:,:,1),ANAME,NLAY,NCOL,-1,IURESTARTHEAD,
     +                   IOUT)
      END IF      
C
C9------COPY INITIAL HEADS FROM STRT TO HNEW.
      HNF = HNOFLO
      DO 400 K=1,NLAY
      DO 400 I=1,NROW
      DO 400 J=1,NCOL
      IF ( IUNIT(63).GT.0 ) THEN
        HITER(J,I,K)=STRT(J,I,K)
      END IF
      HNEW(J,I,K)=STRT(J,I,K)
      IF(IBOUND(J,I,K).EQ.0) HNEW(J,I,K)=HNF
  400 CONTINUE
      RETURN 
      END SUBROUTINE
C
       SUBROUTINE U2DREL_RST(A,ANAME,II,JJ,K,IN,IOUT)
C     ******************************************************************
C     ROUTINE TO INPUT 2-D REAL DATA MATRICES
C       A IS ARRAY TO INPUT
C       ANAME IS 24 CHARACTER DESCRIPTION OF A
C       II IS NO. OF ROWS
C       JJ IS NO. OF COLS
C       K IS LAYER NO. (USED WITH NAME TO TITLE PRINTOUT --)
C              IF K=0, NO LAYER IS PRINTED
C              IF K<0, CROSS SECTION IS PRINTED)
C       IN IS INPUT UNIT
C       IOUT IS OUTPUT UNIT
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      INTEGER, INTENT(IN) :: II, JJ, K, IN, IOUT
      CHARACTER*24 ANAME
      REAL :: A(JJ,II)
      CHARACTER*20 FMTIN
!     CHARACTER*200 CNTRL
      CHARACTER*16 TEXT
!     CHARACTER*200 FNAME
!      INCLUDE 'openspec.inc'
      INTEGER LOCAT, I, J, KSTP, KPER, NCOL, NROW, ILAY
      REAL CNSTNT, PERTIM, TOTIM
C     ------------------------------------------------------------------
C
C1------SET LOCAT TO NEGATIVE UNIT NUMBER FOR UNFORMATTED FILE
       LOCAT = -IN
C2------SET FMTIN to read binary
!       FMTIN = FORM
       FMTIN = 'UNFORMATTED'
       CNSTNT = 1.0
C
C4------TEST LOCAT TO SEE HOW TO DEFINE ARRAY VALUES.
      IF(LOCAT.EQ.0) THEN
C
C4A-----LOCAT=0; SET ALL ARRAY VALUES EQUAL TO CNSTNT. RETURN.
        DO 80 I=1,II
        DO 80 J=1,JJ
   80   A(J,I)=CNSTNT
        IF(K.GT.0) WRITE(IOUT,2) ANAME,CNSTNT,K
    2   FORMAT(1X,/1X,A,' =',1P,G14.6,' FOR LAYER',I4)
        IF(K.LE.0) WRITE(IOUT,3) ANAME,CNSTNT
    3   FORMAT(1X,/1X,A,' =',1P,G14.6)
        RETURN
      ELSE IF(LOCAT.GT.0) THEN
C
C4B-----LOCAT>0; READ FORMATTED RECORDS USING FORMAT FMTIN.
        IF(K.GT.0) THEN
           WRITE(IOUT,94) ANAME,K,LOCAT,FMTIN
   94      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,95) ANAME,LOCAT,FMTIN
   95      FORMAT(1X,///11X,A,/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        ELSE
           WRITE(IOUT,96) ANAME,LOCAT,FMTIN
   96      FORMAT(1X,///11X,A,' FOR CROSS SECTION',/
     1      1X,'READING ON UNIT ',I4,' WITH FORMAT: ',A)
        END IF
        DO 100 I=1,II
!        IF(FMTIN.EQ.'(FREE)') THEN
!           READ(LOCAT,*) (A(J,I),J=1,JJ)
!        ELSE
           READ(LOCAT,FMTIN) (A(J,I),J=1,JJ)
!        END IF
  100   CONTINUE
      ELSE
C
C4C-----LOCAT<0; READ UNFORMATTED ARRAY VALUES.
        LOCAT=-LOCAT
        IF(K.GT.0) THEN
           WRITE(IOUT,201) ANAME,K,LOCAT
  201      FORMAT(1X,///11X,A,' FOR LAYER',I4,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE IF(K.EQ.0) THEN
           WRITE(IOUT,202) ANAME,LOCAT
  202      FORMAT(1X,///1X,A,/
     1      1X,'READING BINARY ON UNIT ',I4)
        ELSE
           WRITE(IOUT,203) ANAME,LOCAT
  203      FORMAT(1X,///1X,A,' FOR CROSS SECTION',/
     1      1X,'READING BINARY ON UNIT ',I4)
        END IF
        READ(LOCAT) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
        READ(LOCAT) A
      END IF
C
C6------IF PRINT CODE (IPRN) >0 OR =0 THEN PRINT ARRAY VALUES.
!  320 IF(IPRN.GE.0) CALL ULAPRW(A,ANAME,0,0,JJ,II,0,IPRN,IOUT)
C
C7------RETURN
      RETURN
      END
C
      SUBROUTINE WRITE_HEADS(IHEDFM)   
C     ******************************************************************
C     PRINT AND RECORD HEADS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:NCOL,NROW,NLAY,IXSEC,HNEW,BUFF
      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,CHEDFM,IOFLG
!!      USE GWFBASMODULE,ONLY:PERTIM,TOTIM,LBHDSV,CHEDFM,IOFLG
      USE GSFMODFLOW, ONLY: KKPER, KKSTP
C
      EXTERNAL :: ULASAV
      INTEGER, INTENT(IN) :: IHEDFM
      INTEGER K, I, J, IFIRST, KK, IHEDUN !, KL
      CHARACTER*16 TEXT
      DATA TEXT /'            HEAD'/
C     ------------------------------------------------------------------
C
      IHEDUN = IHEDFM
C1------FOR EACH LAYER MOVE HNEW TO BUFF IF PRINT OR SAVE IS REQUESTED.
!      DO 59 K=1,NLAY
C
C2------IS HEAD NEEDED FOR THIS LAYER?
!      KL=K
!      IF(IXSEC.NE.0) KL=1
!      IF(IOFLG(KL,1).EQ.0 .AND. IOFLG(KL,3).EQ.0) GO TO 59  !RGN
C
C3------MOVE HNEW TO BUFF FOR THE LAYER.
       DO 59 K=1,NLAY
       DO 58 I=1,NROW
       DO 58 J=1,NCOL
      BUFF(J,I,K)=SNGL( HNEW(J,I,K) )
   58 CONTINUE
   59 CONTINUE
!       BUFF = SNGL( HNEW )
C
C4------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE PRINTED.
C4------IF SO THEN CALL ULAPRS OR ULAPRW TO PRINT HEAD.
!      IF(ISA.NE.0) THEN   !RGN DON'T NEED TO PRINT HEAD
!         IF(IXSEC.EQ.0) THEN
!           DO 69 K=1,NLAY
!           KK=K
!           IF(IOFLG(K,1).EQ.0) GO TO 69
!           IF(IHEDFM.LT.0) CALL ULAPRS(BUFF(:,:,K),TEXT,KKSTP,KKPER,
!     1               NCOL,NROW,KK,-IHEDFM,IOUT)
!           IF(IHEDFM.GE.0) CALL ULAPRW(BUFF(:,:,K),TEXT,KKSTP,KKPER,
!     1               NCOL,NROW,KK,IHEDFM,IOUT)
!           IPFLG=1
!   69      CONTINUE
C
C4A-----PRINT HEAD FOR CROSS SECTION.
!         ELSE
!           IF(IOFLG(1,1).NE.0) THEN
!             IF(IHEDFM.LT.0) CALL ULAPRS(BUFF,TEXT,KKSTP,KKPER,
!     1                 NCOL,NLAY,-1,-IHEDFM,IOUT)
!             IF(IHEDFM.GE.0) CALL ULAPRW(BUFF,TEXT,KSKTP,KKPER,
!     1                 NCOL,NLAY,-1,IHEDFM,IOUT)
!             IPFLG=1
!           END IF
!         END IF
!      END IF
C
C5------FOR EACH LAYER: DETERMINE IF HEAD SHOULD BE SAVED ON DISK.
C5------IF SO THEN CALL ULASAV OR ULASV2 TO SAVE HEAD.
      IFIRST=1
      IF(IHEDUN.LE.0) GO TO 80
      IF(IXSEC.EQ.0) THEN
        DO 79 K=1,NLAY
          KK=K
!        IF(IOFLG(K,3).EQ.0) GO TO 79
!        IF(IFIRST.EQ.1) WRITE(IOUT,74) IHEDUN,KKSTP,KKPER
!   74   FORMAT(1X,/1X,'HEAD WILL BE SAVED ON UNIT ',I4,
!     1      ' AT END OF TIME STEP',I6,', STRESS PERIOD ',I4) !gsf
!        IFIRST=0
!        IF(CHEDFM.EQ.' ') THEN   !RGN UNFORMATTED ONLY
          CALL ULASAV(BUFF(:,:,K),TEXT,KKSTP,KKPER,PERTIM,TOTIM,NCOL,
     1                NROW,KK,IHEDUN)
!        ELSE
!           CALL ULASV2(BUFF(:,:,K),TEXT,KKSTP,KKPER,PERTIM,TOTIM,NCOL,
!     1                NROW,KK,IHEDUN,CHEDFM,LBHDSV,IBOUND(:,:,K))
!        END IF
   79   CONTINUE
C
C5A-----SAVE HEAD FOR CROSS SECTION.
      ELSE
        IF(IOFLG(1,3).NE.0) THEN
!          WRITE(IOUT,74) IHEDUN,KKSTP,KKPER
          IF(CHEDFM.EQ.' ') THEN
             CALL ULASAV(BUFF,TEXT,KKSTP,KKPER,PERTIM,TOTIM,NCOL,
     1                NLAY,-1,IHEDUN)
!          ELSE
!             CALL ULASV2(BUFF,TEXT,KKSTP,KKPER,PERTIM,TOTIM,NCOL,
!     1                  NLAY,-1,IHEDUN,CHEDFM,LBHDSV,IBOUND)
          END IF
        END IF
      END IF
C
C6------RETURN.
   80 RETURN
      END

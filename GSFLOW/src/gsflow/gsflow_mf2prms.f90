!***********************************************************************
! Transfer MODFLOW data to PRMS
!***********************************************************************
!     ******************************************************************
!     Mapping module to convert MODFLOW to PRMS states for use by GSFLOW
!   Declared Parameters
!     gvr_hru_id, gvr_cell_id
!     ******************************************************************
      INTEGER FUNCTION gsflow_mf2prms()
      USE PRMS_CONSTANTS, ONLY: ACTIVE, RUN, DECL
      USE PRMS_MODULE, ONLY: Process_flag, Nhrucell, Gvr_cell_id, Ag_package, Dprst_flag, Dprst_ag_gain, Hru_ag_irr
      use prms_utils, only: print_module
      USE GSFMODFLOW, ONLY: Mfq2inch_conv, Gwc_col, Gwc_row, MFQ_to_inch_acres
      USE PRMS_SOILZONE, ONLY: Hrucheck, Gvr_hru_id, Gw2sm_grav
      USE GWFUZFMODULE, ONLY: SEEPOUT
      USE GWFAGMODULE, ONLY: NUMIRRWELSP, IRRWELVAR, NUMCELLS, WELLIRRPRMS, IRRROW_SW, &
     &                       NUMIRRDIVERSIONSP, IRRSEG, DVRCH, DIVERSIONIRRPRMS, IRRROW_GW, &
     &                       NUMCELLSPOND, IRRHRU_POND, PONDIRRPRMS, PONDSEGFLOW, &
     &                       IRRPONDVAR, NUMIRRPOND
      IMPLICIT NONE
! Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW MODFLOW to PRMS'
      character(len=*), parameter :: MODNAME = 'gsflow_mf2prms'
      character(len=*), parameter :: Version_gsflow_mf2prms = '2022-02-18'
      integer :: i, j, k, ihru, IRWL, NMCL, SGNM
!***********************************************************************
      gsflow_mf2prms = 0

      IF ( Process_flag==RUN ) THEN
        DO i = 1, Nhrucell
          IF ( Hrucheck(Gvr_hru_id(i))==1 ) &
     &         Gw2sm_grav(i) = SEEPOUT(Gwc_col(Gvr_cell_id(i)), Gwc_row(Gvr_cell_id(i)))*Mfq2inch_conv(i)
        ENDDO
!
! Add irrigation to HRU from AG Package
!
! From irrigation wells
!
        IF ( Ag_package==ACTIVE ) THEN
          Hru_ag_irr = 0.0
          DO J = 1, NUMIRRWELSP
            IRWL = IRRWELVAR(J)
            NMCL = 0
            IF ( IRWL > 0 ) NMCL = NUMCELLS(IRWL)
            DO K = 1, NMCL
              ihru = IRRROW_GW(K,IRWL)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + WELLIRRPRMS(k,IRWL)*MFQ_to_inch_acres
            END DO
          END DO
!
! From segment diversions
!
          DO J = 1, NUMIRRDIVERSIONSP
            SGNM = IRRSEG(J)
            NMCL = 0
            IF ( SGNM>0 ) NMCL = DVRCH(SGNM)
            DO K=1,NMCL
              ihru = IRRROW_SW(K,SGNM)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + DIVERSIONIRRPRMS(k,SGNM)*MFQ_to_inch_acres
            END DO
          END DO
!
! From open depression storage reservoirs and streams to storage reservoirs
!
          IF ( Dprst_flag==ACTIVE ) THEN
            Dprst_ag_gain = 0.0
            DO i = 1, NUMIRRPOND
!              print *, irrpondvar
              ihru = IRRPONDVAR(i)
              IF ( ihru>0 ) THEN
                IF ( PONDSEGFLOW(i)>0.0 ) Dprst_ag_gain(ihru) = Dprst_ag_gain(ihru) + PONDSEGFLOW(i)*MFQ_to_inch_acres
              ENDIF
            ENDDO
            DO i = 1, NUMIRRPOND
              DO k = 1, NUMCELLSPOND(i)
                ihru = IRRHRU_POND(k, i)
                Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + PONDIRRPRMS(k, i)*MFQ_to_inch_acres
              END DO
            END DO
          END IF
        END IF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_gsflow_mf2prms)
      ENDIF

      END FUNCTION gsflow_mf2prms

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
      USE GSFMODFLOW, ONLY: Mfq2inch_conv, Gwc_col, Gwc_row, &
                            Mfl2_to_acre, Mfl_to_inch
      USE PRMS_SOILZONE, ONLY: Hrucheck, Gvr_hru_id, Gw2sm_grav, Hru_ag_irr
      USE GWFUZFMODULE, ONLY: SEEPOUT
      USE PRMS_MODULE, ONLY: Process_flag, Nhrucell, Gvr_cell_id, Ag_package_active
      USE GWFBASMODULE, ONLY: DELT
      USE GWFAGMODULE, ONLY: NUMIRRWELSP, IRRWELVAR, NUMCELLS, WELLIRRPRMS, IRRROW_SW, &
     &                       NUMIRRDIVERSIONSP, IRRSEG, DVRCH, DIVERSIONIRRPRMS, IRRROW_GW
      IMPLICIT NONE
! Functions
      EXTERNAL print_module
! Local Variables
      character(len=*), parameter :: MODDESC = 'GSFLOW MODFLOW to PRMS'
      character(len=*), parameter :: MODNAME = 'gsflow_mf2prms'
      character(len=*), parameter :: Version_gsflow_mf2prms = '2021-01-14'
      INTEGER :: i, j, k, ihru
      integer :: IRWL,NMCL,SGNM
      DOUBLE PRECISION :: mf_q2prms_inchacres
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
        IF ( Ag_package_active==ACTIVE ) THEN
          mf_q2prms_inchacres = DELT*Mfl2_to_acre*Mfl_to_inch
          Hru_ag_irr = 0.0
          DO J = 1, NUMIRRWELSP
            IRWL = IRRWELVAR(J)
            NMCL = 0
            IF ( IRWL > 0 ) NMCL = NUMCELLS(IRWL)
            DO K = 1, NMCL
              ihru = IRRROW_GW(K,IRWL)
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + WELLIRRPRMS(k,IRWL)*SNGL(mf_q2prms_inchacres)
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
              Hru_ag_irr(ihru) = Hru_ag_irr(ihru) + DIVERSIONIRRPRMS(k,SGNM)*SNGL(mf_q2prms_inchacres)
            END DO
          END DO
        END IF

      ELSEIF ( Process_flag==DECL ) THEN
        CALL print_module(MODDESC, MODNAME, Version_gsflow_mf2prms)
      ENDIF

      END FUNCTION gsflow_mf2prms
